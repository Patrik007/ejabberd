%%%----------------------------------------------------------------------
%%% File    : mod_elenty_image_upload.erl
%%% Purpose : Handle HTTP API Registration calls
%%%
%%%
%%%----------------------------------------------------------------------

-module(mod_elenty_send_message_api).

-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    depends/2,
    mod_options/1,
    process/2
    ]).

-include("logger.hrl").
%% -include("xmpp.hrl").
-include("ejabberd_http.hrl").

-define(ELENTY_HTTP_TOKEN, <<"fc7c3f73-ab56-4db6-b0cd-3093fe2b252a">>).
-define(HOST_ELENTY, <<"xmpp.elenty.com">>).
-define(HOST_ELENTY_WITH_AT, "@xmpp.elenty.com").

gen_response(Message) ->
    jiffy:encode({[{status, erlang:list_to_binary(Message)}]}).

process(_LocalPath, Request) ->
    % Do some sort of authentication of the user here.
    #request{data=Data, method=Method, host=Host, q=Query,
             path=Path} = Request,
    ?DEBUG("Request: ~p", [Request]),
    ?DEBUG("Post DataValues: ~p", [Data]),
    ?DEBUG("Method: ~p", [Method]),
    {Token, Message, FromJID, ToJIDs } =
        case Method of
            'POST' ->
                {JSON} = jiffy:decode(Data),
                ?DEBUG("JSON: ~p", [JSON]),
                {proplists:get_value(<<"token">>, JSON),
                 proplists:get_value(<<"message">>, JSON),
                 proplists:get_value(<<"fromJid">>, JSON),
                 proplists:get_value(<<"toJids">>, JSON)};
            'DELETE' ->
                {lists:nth(3, Path), <<>>, proplists:get_value(<<"token">>, Query)}
        end,
    %% ?DEBUG("Token: ~p ~p", [Token, ?ELENTY_HTTP_TOKEN]),
    ?DEBUG("message= ~p", [Message]),
    ?DEBUG("fromJid= ~p", [FromJID]),
    ?DEBUG("toJids= ~p", [ToJIDs]),
    case Token of
        ?ELENTY_HTTP_TOKEN ->
            case Method of
                'POST' ->
                    ?DEBUG("Got POST", []),
                    if (FromJID == undefined) ->
                            gen_response("Invalid Arguments");
                       true ->
                            case ejabberd_auth:user_exists(FromJID, ?HOST_ELENTY) of
                                true ->
                                    ToJIDsArr = string:tokens(erlang:binary_to_list(ToJIDs), ","),
                                    MessageXML = fxml_stream:parse_element(Message),
                                    __FromJIDStr = lists:concat([erlang:binary_to_list(FromJID), ?HOST_ELENTY_WITH_AT]),
                                    __FromJID = jid:decode(erlang:list_to_binary(__FromJIDStr)),

                                    lists:foreach(fun (Jid) ->
                                                          RecipientJID = lists:concat([Jid, ?HOST_ELENTY_WITH_AT]),
                                                          ToJID = jid:decode(erlang:list_to_binary(RecipientJID)),
                                                          DecodedMsg = xmpp:decode(MessageXML),
                                                          ejabberd_router:route(
                                                            __FromJID,
                                                            ToJID,
                                                            DecodedMsg)
                                                  end,
                                                  ToJIDsArr),
                                    gen_response("OK");
                                _ ->
                                    gen_response("User not found")
                            end
                    end;
                _ ->
                    ?DEBUG("Unknown Method", []),
                    gen_response("Invalid Method")
            end;
        _ ->
            ?DEBUG("Invalid Token", []),
            gen_response("Invalid Token")
    end.

auth_modules(Server) ->
    LServer = jid:nameprep(Server),
    Default = external,
    Methods = ejabberd_config:get_option(
                {auth_method, LServer},
                fun(V) when is_list(V) ->
                        true = lists:all(fun is_atom/1, V),
                        V;
                   (V) when is_atom(V) ->
                        [V]
                end, [Default]),
    [jlib:binary_to_atom(<<"ejabberd_auth_",
                           (jlib:atom_to_binary(M))/binary>>)
     || M <- Methods].

remove_user(User, Server) ->
    lists:foreach(fun (M) -> M:remove_user(User, Server)
                  end,
                  auth_modules(Server)),
    ok.

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

depends(_Host, _Opts) ->
[].

mod_options(_Host) ->
[].
