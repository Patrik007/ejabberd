-module(mod_elenty_offlineretrieval).

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         depends/2,
         mod_opt_type/1,
         mod_options/1,
         clear_messages/2,
         decode_iq_subel/1,
         process_local_iq/1]).

%% -include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("../deps/xmpp/include/xmpp.hrl").

-include("mod_offline.hrl").

-define(NS_ELENTY_BROADCAST, <<"elenty:offlineretrieval">>).
-define(MODULE_MOD_OFFLINE, mod_offline).

start(Host, Opts) ->
    Mod = gen_mod:db_mod(Opts, ?MODULE_MOD_OFFLINE),
    Mod:init(Host, Opts),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_ELENTY_BROADCAST, ?MODULE, process_local_iq),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
                                     ?NS_ELENTY_BROADCAST).

depends(_Host, _Opts) ->
[].

mod_opt_type(db_type) ->
    econf:db_type(?MODULE_MOD_OFFLINE).

mod_options(Host) ->
    [{db_type, ejabberd_config:default_db(Host, ?MODULE_MOD_OFFLINE)}].

-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
%% Tell gen_iq_handler not to auto-decode IQ payload
decode_iq_subel(El) ->
    El.

%%% Yoink a whole bunch of code from
%%% mod_offline_riak so we can have a custom pop_messages function
offline_msg_schema() ->
    {record_info(fields, offline_msg), #offline_msg{}}.

offline_msg_to_route(LServer, #offline_msg{} = R) ->
    ?DEBUG("Offline message messageID=~p", [R#offline_msg.messageid]),
    %% ?DEBUG("Offline message timestamp=~p", [R#offline_msg.timestamp]),
    X = case R#offline_msg.messageid of
        undefined ->
            ?DEBUG("Packet corrupted= ~p", [R#offline_msg.packet]),
            ?DEBUG("From= ~p ", [R#offline_msg.from]),
            ?DEBUG("To= ~p ", [R#offline_msg.to]),
            ?DEBUG("TimeStamp= ~p ", [R#offline_msg.timestamp]),
            ?DEBUG("US= ~p ", [R#offline_msg.us]),
            undefined;
        _ ->
            ok
    end,
    El = case R#offline_msg.timestamp of
	     undefined ->
                 R#offline_msg.packet;
             TS ->
                 CodecOpts = ejabberd_config:codec_options(),
                 Pkt = xmpp:decode(R#offline_msg.packet, ?NS_CLIENT, CodecOpts),
                 Packet0 = xmpp:put_meta(Pkt, from_offline, true),
                 Packet1 = misc:add_delay_info(Packet0, R#offline_msg.from, TS,
                                               <<"Offline Storage">>),
                 ?DEBUG("Packet1: ~p", [Packet1]),
                 {TS1, TS2, TS3} = TS,
                 TSString = io_lib:format("~p:~p:~p", [TS1, TS2, TS3]),
                 TimestampTag = #xmlel{name = <<"offlineid">>,
                                       attrs = [{<<"id">>, TSString}]},
                 xmpp:encode(xmpp:append_subtags(Packet1, [TimestampTag]))
         end,
    ?DEBUG("El: ~p", [El]),
    El.

pop_offline_messages(LUser, LServer, MsgLimit) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE_MOD_OFFLINE),
    OfflineMessages = case Mod:pop_messages(LUser, LServer) of
        {ok, OffMsgs} ->
            OffMsgs;
        _ ->
        []
    end,

    ?DEBUG("Count messages=~p, user=~p", [length(OfflineMessages), {LUser, LServer}]),

    case catch OfflineMessages of
        [] ->
            {ok, []};
        _ ->
            SortedRs = lists:keysort(#offline_msg.timestamp, OfflineMessages),
            Rs = case MsgLimit of
                -1 ->
                    OfflineMessages;
                Limit ->
                    lists:sublist(SortedRs, Limit)
    end,
    ?DEBUG("Limited Rs: ~p", [Rs]),
    {ok, lists:reverse(Rs)}
end.

process_local_iq(#iq{from = #jid{luser=FromUser, lserver=Server},
                     type = Type, sub_els = [SubEl]} = IQ) ->
    case Type of
        set ->
            MessageIdsCSV = fxml:get_subtag_cdata(SubEl, <<"offlineids">>),
            MessageIds = string:tokens(erlang:binary_to_list(MessageIdsCSV), ","),
            lists:foreach(
              fun(MsgTimestampString) ->
                      case string:words(MsgTimestampString, $:) of
                          3 ->
                              ?DEBUG("String: ~p", [MsgTimestampString]),
                              ?DEBUG("Tokens: ~p", [string:tokens(MsgTimestampString, ":")]),
                              [T1, T2, T3] =
                                  string:tokens(MsgTimestampString, ":"),
                              Timestamp = {erlang:list_to_integer(T1),
                                           erlang:list_to_integer(T2),
                                           erlang:list_to_integer(T3)},
                              ObjDel = #offline_msg{timestamp=Timestamp},
                              %% ?DEBUG("delete=~p", [ObjDel]),
                              ?DEBUG("FromUser=~p Server=~p", [FromUser, Server]),
                              case mnesia:dirty_match_object(offline_msg, #offline_msg{us={FromUser, Server}, timestamp = Timestamp, _ = '_'}) of
                                [] ->
                                    ?DEBUG("DB= Not found message in DB", []),
                                    ok;
                                Msgs ->
                                    ?DEBUG("DB= Found messages is DB", []),
                                    lists:foreach(
                                        fun(Msg) ->
                                        mnesia:dirty_delete_object(Msg)
                                    end, Msgs)
                                end,
                              %% ok = mnesia:dirty_delete(offline_msg, ObjDel),
                              %% ?DEBUG("ok=~p", [ok]),
                              ok;
                              %% ok = ejabberd_riak:delete(offline_msg, Timestamp);
                          _ ->
                              ?DEBUG("Invalid OfflineID: ~p", [MsgTimestampString])
                      end
              end, MessageIds),
            Response = #xmlel{name= <<"offlinemsgs">>,
                              attrs= [{<<"xmlns">>, ?NS_ELENTY_BROADCAST}],
                              children= []},
            ?DEBUG("Packets End: ~p", [Response]),
            IQ#iq{type = result, sub_els = Response};
        get ->
            MsgLimit = case fxml:get_tag_attr_s(<<"limit">>, SubEl) of
                           <<"">> ->
                               -1;
                           Limit ->
                               list_to_integer(binary_to_list(Limit))
                       end,
            {ok, OfflineMsgs} = pop_offline_messages(FromUser, Server, MsgLimit),

            Packets = lists:map(fun (R) ->
                                   offline_msg_to_route(Server, R)
                           end, OfflineMsgs),
            Response = #xmlel{name= <<"offlinemsgs">>,
                              attrs= [{<<"xmlns">>, ?NS_ELENTY_BROADCAST}],
                              children= Packets},
            ?DEBUG("Packets End: ~p", [Response]),
            xmpp:make_iq_result(IQ, Response)
    end.
