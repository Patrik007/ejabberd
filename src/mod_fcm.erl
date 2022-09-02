-module(mod_fcm).
-author("mrDoctorWho").

-include("logger.hrl").
-include("xmpp.hrl").

-behaviour(gen_mod).

-export([start/2, stop/1, mod_options/1, depends/2, send/2, mod_opt_type/1]).

-define(CONTENT_TYPE, "application/json").

start(Host, Opts) ->
    ?DEBUG("mod_fcm started", []),
    ?DEBUG("Host: ~p, URL : ~p", [Host, gen_mod:get_module_opt(Host, ?MODULE, post_url)]),
    ?DEBUG("Host: ~p, Token : ~p", [Host, gen_mod:get_module_opt(Host, ?MODULE, auth_token)]),
    %% send(Host, {[{tokens, [<<"token1">>, <<"token2">>]},{data, {[{del_ids, [<<"id1">>, <<"id2">>]}]}}]}),
  ok.

stop(_Host) ->
    ?DEBUG("mod_fcm stopped", []).

depends(_Host, _Opts) ->
[].

mod_options(_Host) ->
[{ post_url, <<"http://xmpp.elenty.com:3456">> },
    { auth_token, <<"">>}].

mod_opt_type(post_url) ->
    fun iolist_to_binary/1;
mod_opt_type(auth_token) ->
    fun iolist_to_binary/1.

send(Host, Payload) ->

    URL = erlang:binary_to_list(gen_mod:get_module_opt(Host, ?MODULE, post_url)),

    API_KEY = "key=" ++ binary_to_list(gen_mod:get_module_opt(Host, ?MODULE, auth_token)),
    Header = [{"Authorization", API_KEY}, {"Content-Type", ?CONTENT_TYPE}],
    Body = try jiffy:encode(Payload) of
           BodyDec ->
               BodyDec
        catch
           Aborted:Reason ->
               ?DEBUG("Error encoding payload... ~p ~p", [Aborted, Reason]),
               ?ERROR_MSG("Error encoding payload... ~p ~p", [Aborted, Reason]),
               {}
        end,
    ?DEBUG("Body: ~p", [Body]),
    ssl:start(),
   	application:start(inets),
   	{ok, RawResponse} = httpc:request(post, {URL, Header, ?CONTENT_TYPE, Body}, [{timeout, 5000}], []),
    ?DEBUG("RawResp ~p", [RawResponse]),
   	{{_, SCode, Status}, ResponseBody} = {element(1, RawResponse), element(3, RawResponse)},
   	%% TODO: Errors 5xx
   	case catch SCode of
   		200 -> ?DEBUG("mod_fcm: A message was sent, body: ~p", [ResponseBody]);
   		401 -> ?ERROR_MSG("mod_fcm: ~s ~p", [Status, ResponseBody]);
   		_ -> ?ERROR_MSG("mod_fcm: ~s", [ResponseBody])
   	end.
