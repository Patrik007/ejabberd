%%%----------------------------------------------------------------------
%%% File    : mod_elenty_image_upload.erl
%%% Purpose : Handle HTTP API Registration calls
%%%
%%%
%%%----------------------------------------------------------------------

-module(mod_elenty_registration_api).

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
-include_lib("../deps/xmpp/include/xmpp.hrl").

-define(ELENTY_HTTP_TOKEN, <<"fc7c3f73-ab56-4db6-b0cd-3093fe2b252a">>).

gen_response(Message) ->
    jiffy:encode({[{status, erlang:list_to_binary(Message)}]}).

process(_LocalPath, Request) ->
    % Do some sort of authentication of the user here.
    #request{data=Data, method=Method, host=Host, q=Query,
             path=Path} = Request,
    ?DEBUG("Request: ~p", [Request]),
    ?DEBUG("Post DataValues: ~p", [Data]),
    ?DEBUG("Method: ~p", [Method]),
    {Username, Password, Token} =
        case Method of
            'POST' ->
                {JSON} = jiffy:decode(Data),
                ?DEBUG("JSON: ~p", [JSON]),
                {proplists:get_value(<<"username">>, JSON),
                 proplists:get_value(<<"password">>, JSON),
                 proplists:get_value(<<"token">>, JSON)};
            'DELETE' ->
                {lists:nth(3, Path), <<>>, proplists:get_value(<<"token">>, Query)}
        end,
    ?DEBUG("Token: ~p ~p", [Token, ?ELENTY_HTTP_TOKEN]),
    case Token of
        ?ELENTY_HTTP_TOKEN ->
            case Method of
                'POST' ->
                    ?DEBUG("Got POST", []),
                    if (Username == undefined) or (Password == undefined) ->
                            gen_response("Invalid Arguments");
                       true ->
                            case ejabberd_auth:user_exists(Username, Host) of
                                true ->
                                    gen_response("User already exists");
                                _ ->
                                    case ejabberd_auth:try_register(
                                           Username, Host, Password) of
                                        ok ->
                                            gen_response("Registration OK");
                                        Error ->
                                            case Error of
                                                {atomic, exists} ->
                                                    gen_response("User already exists");
                                                {error, invalid_jid} ->
                                                    gen_response("Invalid Username");
                                                {error, not_allowed} ->
                                                    gen_response("Not Allowed");
                                                {error, _Reason} ->
                                                    gen_response("Server Error")
                                            end
                                    end
                            end
                    end;
                'DELETE' ->
                    ?DEBUG("Delete ~p ~p!", [Username, Host]),
                    case ejabberd_auth:user_exists(Username, Host) of
                        false ->
                            gen_response("User doesn't exists");
                        _ ->
                            case remove_user(Username, Host) of
                                ok ->
                                    %% User deleted, now clear their offline messages
                                    ?DEBUG("Deleting offline messages for ~p", [Username]),
                                    ok = mod_elenty_offlineretrieval:clear_messages(Username, Host),
                                    gen_response("Removal OK");
                                not_allowed ->
                                    gen_response("Not Allowed");
                                not_exists ->
                                    gen_response("User Does Not Exist");
                                _ ->
                                    gen_response("Server Error")
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
