%%%----------------------------------------------------------------------
%%% File    : mod_elenty_delete_messages.erl
%%% Purpose : Elenty Delete Messages -- remove messages from offline inbox and iOS notification on device
%%% Created : 7 October 2021 by Zach Goldberg <zach@sticksandbrains.com>
%%%
%%%
%%% Copyright (C) 2021 Sticks and Brains
%%%
%%%----------------------------------------------------------------------

-module(mod_elenty_delete_messages).

-behaviour(gen_mod).

-include("logger.hrl").
-include_lib("../deps/xmpp/include/xmpp.hrl").

-export([start/2, stop/1, mod_options/1, depends/2, decode_iq_subel/1, process_local_iq/1]).

-import('mod_fcm', [send/0]).

-record(gcm_users, {user, gcm_token, last_seen, gcm_type}).
-define(STANZA_ERROR(Code, Type, Condition),
        {xmlelement, "error",
         [{"code", Code}, {"type", Type}],
         [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []}]}).

-define(NS_ELENTY_DELETE_MESSAGES, <<"elenty:deletemessages">>).

gcm_users_schema() ->
    {record_info(fields, gcm_users), #gcm_users{}}.

get_gcm_token(User, Server, mnesia) ->
    Result = mnesia:dirty_read(gcm_users, {User, Server}),
    case catch Result of
        [] ->
            {not_found, unknown};
        [#gcm_users{gcm_token = Token, gcm_type=Type}] ->
            {Token, Type}
    end.

start(Host, Opts) ->
    xmpp:register_codec(elenty_delete_messages),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                  ?NS_ELENTY_DELETE_MESSAGES, ?MODULE, process_local_iq),
    ok.

stop(Host) ->
    xmpp:unregister_codec(elenty_delete_messages),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
                                     ?NS_ELENTY_DELETE_MESSAGES).

depends(_Host, _Opts) ->
[].

mod_options(_Host) ->
[].

-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
decode_iq_subel(El) ->
    El.

process_local_iq(#iq{from = _FromJID, to = _ToJID, id = _ID, type = Type, sub_els = [SubEl]} = IQ) ->
    %% ?DEBUG("mod_elenty_delete_messages, Incomming iq", []),
    %% ?DEBUG("~p", [IQ]),
    %% ?DEBUG("~p", [SubEl]),
    case Type of
        get ->
            IQ#iq{type = error, sub_els = [SubEl, ?STANZA_ERROR("400", "modify", "bad-request")]};
        set ->
            RecipientCSV = fxml:get_subtag_cdata(SubEl, <<"recipients">>),
            Recipients = string:tokens(erlang:binary_to_list(RecipientCSV), ","),
            ?DEBUG("Recipients: ~p", [Recipients]),

            IDsCSV = fxml:get_subtag_cdata(SubEl, <<"ids">>),
            ?DEBUG("~p", [IDsCSV]),

            ListOfIOSTokens = lists:filtermap(fun(RecipientName) ->
                RecipientJID = lists:concat([RecipientName, "@xmpp.elenty.com"]),
                ToJID = jid:decode(erlang:list_to_binary(RecipientJID)),
                ToUser = ToJID#jid.user,
                ToServer = ToJID#jid.server,
                {Token, GCMType} = get_gcm_token(ToUser, ToServer, mnesia),

                case Token of
                    not_found -> false;
                    _ ->
                        case GCMType of
                            apn -> {true, Token};
                            _ ->
                                false
                        end
                end
            end, Recipients),

            ?DEBUG("End of recipients filtermap ~p", [ListOfIOSTokens]),

            Len = length(ListOfIOSTokens),
            %% ?DEBUG("Tokens: ~p", [ListOfIOSTokens]),

            CanSend = if
                Len > 0 -> 1;
                true -> 0
            end,

            case CanSend of
                1 ->
                    ?DEBUG("CanSend 1 before", []),
                    Payload = {[
                        {tokens, ListOfIOSTokens},
                        {data, {[
                            {del_ids, IDsCSV}
                        ]}}
                    ]},
                    ?DEBUG("CanSend 1 payload: ~p", [Payload]),
                    mod_fcm:send(<<"xmpp.elenty.com">>, Payload),
                    ?DEBUG("Can send", []);
                _ ->
                    ?DEBUG("No send", [])
            end,
            ?DEBUG("End of process_local_iq", []),
            xmpp:make_iq_result(IQ)
    end.
