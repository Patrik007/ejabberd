%%%----------------------------------------------------------------------
%%% File    : mod_elenty_servertimestamp.erl
%%% Purpose : Elenty Server Timestamp -- add timestamps to certain messages
%%% Created : 1 June 2014 by Zach Goldberg <zach@sticksandbrains.com>
%%%
%%%
%%% Copyright (C) 2014 Sticks and Brains
%%%
%%%----------------------------------------------------------------------

-module(mod_elenty_broadcastmsg).

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         depends/2,
         mod_options/1,
         decode_iq_subel/1,
         process_local_iq/1]).

-import('mod_elenty_servertimestamp', [get_servertimestamp_elm/0]).

-include("logger.hrl").
-include_lib("../deps/xmpp/include/xmpp.hrl").

-define(STANZA_ERROR(Code, Type, Condition),
        {xmlelement, "error",
         [{"code", Code}, {"type", Type}],
         [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}).

-define(NS_ELENTY_BROADCAST, <<"elenty:broadcastmessage">>).

start(Host, Opts) ->
    %% IQDisc = gen_mod:get_opt(iqdisc, Opts,
        %%                     fun gen_iq_handler:check_type/1,
        %%                     one_queue),
    xmpp:register_codec(elenty_broadcast),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_ELENTY_BROADCAST, ?MODULE, process_local_iq),
    ok.

stop(Host) ->
    xmpp:unregister_codec(elenty_broadcast),
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

process_local_iq(#iq{from = _FromJID, to = _ToJID, id = _ID, type = Type, sub_els = [SubEl]} = IQ) ->
    ?DEBUG("broadcastmessage", []),
    ?DEBUG("~p", [SubEl]),
    case Type of
        get ->
            IQ#iq{type = error, sub_els = [SubEl, ?STANZA_ERROR("400", "modify", "bad-request")]};
        set ->
            Body = fxml:get_subtag(SubEl, <<"body">>),
            Data = fxml:get_subtag(SubEl, <<"data">>),
            Data1 = fxml:get_subtag(SubEl, <<"data1">>),
            MsgId = fxml:get_subtag(SubEl, <<"msgid">>),
            Children = lists:filter(fun (F) ->
                                            case F of
                                                false ->
                                                    false;
                                                _ ->
                                                    true
                                            end
                                    end,
                                    [Body, Data, Data1, MsgId]),
            MsgType = case fxml:get_subtag_cdata(SubEl, <<"type">>) of
                          false ->
                              <<"chat">>;
                          <<"">> ->
                              <<"chat">>;
                          Typ ->
                              Typ
                   end,
            RecipientCSV = fxml:get_subtag_cdata(SubEl, <<"recipients">>),
            Recipients = string:tokens(erlang:binary_to_list(RecipientCSV), ","),
            MessageID = fxml:get_subtag_cdata(SubEl, <<"msgid">>),
            Message = #xmlel{name= <<"message">>,
                             attrs= [{<<"type">>, MsgType}, {<<"id">>, MessageID}],
                             children= Children},
            ?DEBUG("Broadcast message=~p", [Message]),
            lists:foreach(fun (RecipientName) ->
                                  RecipientJID = lists:concat([RecipientName, "@xmpp.elenty.com"]),
                                  ToJID = jid:decode(erlang:list_to_binary(RecipientJID)),
                                  DecodedMsg = xmpp:decode(Message),
                                  ejabberd_router:route(
                                    _FromJID,
                                    ToJID,
                                    DecodedMsg)
                          end,
                          Recipients),
            xmpp:make_iq_result(IQ, mod_elenty_servertimestamp:get_servertimestamp_elm())
    end.
