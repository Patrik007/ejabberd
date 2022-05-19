-module(mod_elenty_offlineretrieval).

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         depends/2,
         mod_options/1,
         clear_messages/2,
         decode_iq_subel/1,
         process_local_iq/1]).

%% -include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("../deps/xmpp/include/xmpp.hrl").

-include("mod_offline.hrl").

-define(NS_ELENTY_BROADCAST, <<"elenty:offlineretrieval">>).

start(Host, Opts) ->
    %% IQDisc = gen_mod:get_opt(iqdisc, Opts,
        %%                     fun gen_iq_handler:check_type/1,
        %%                     one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_ELENTY_BROADCAST, ?MODULE, process_local_iq),
    ok.

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
                                     ?NS_ELENTY_BROADCAST).

depends(_Host, _Opts) ->
[].

mod_options(_Host) ->
[].

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
    %% Result = mnesia:dirty_index_read(offline_msg, {LUser, LServer}, <<"us">>),
    Result = mnesia:dirty_read(offline_msg, {LUser, LServer}),

    FilterOkMessages = lists:filter(fun(#offline_msg{} = R) ->
        case R#offline_msg.messageid of
            undefined ->
                false;
            _ ->
                true
        end
    end, Result),

    FilterNotOkMessages = lists:filter(fun(#offline_msg{} = R) ->
        case R#offline_msg.messageid of
            undefined ->
                true;
            _ ->
                false
        end
    end, Result),

    ?DEBUG("Count messages=~p, user=~p", [length(FilterOkMessages), {LUser, LServer}]),
    case catch FilterOkMessages of
        [] ->
            {ok, []};
        _ ->
        ?DEBUG("Pop Msgs: ~p, ~p", [MsgLimit, FilterOkMessages]),
        SortedRs = lists:keysort(#offline_msg.timestamp, FilterOkMessages),
        Rs = case MsgLimit of
                        -1 ->
                            FilterOkMessages;
                        Limit ->
                            lists:sublist(SortedRs, Limit)
             end,
        ?DEBUG("Limited Rs: ~p", [Rs]),
        {ok, lists:reverse(Rs)}
end.

    %% case ejabberd_riak:get_by_index(offline_msg, offline_msg_schema(), <<"us">>, {LUser, LServer}) of
        %%{ok, AllRs} ->
        %%?DEBUG("Pop Msgs: ~p, ~p", [MsgLimit, AllRs]),
            %%SortedRs = lists:keysort(#offline_msg.timestamp, AllRs),
            %%Rs = case MsgLimit of
        %%                    -1 ->
            %%                    AllRs;
            %%                Limit ->
            %%                    lists:sublist(SortedRs, Limit)
            %%     end,
            %%?DEBUG("Limited Rs: ~p", [Rs]),
            %%try
                %% We used to delete the messages here, that is now
                %% moved to the "set" iq in this module
            %%    {ok, lists:reverse(Rs)}
            %%catch _:{badmatch, Err} ->
            %%        Err
            %%end;
        %%Err ->
            %%Err
    %%end.

clear_messages(User, Server) ->
    {ok, OfflineMsgs} = pop_offline_messages(User, Server, -1),
    ?DEBUG("Clearing ~p messages for ~p", [length(OfflineMsgs), User]),
    lists:foreach(
      fun(Msg) ->
              Ts = Msg#offline_msg.timestamp,
              ok = mnesia:dirty_delete(offline_msg, Ts)
              %% ok = ejabberd_riak:delete(offline_msg, Ts)
      end, OfflineMsgs),
    ok.


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
