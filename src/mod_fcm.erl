-module(mod_fcm).
-author("mrDoctorWho").

-include("logger.hrl").
%% -include("xmpp.hrl").

-behaviour(gen_mod).

-export([start/2, stop/1, mod_options/1, depends/2, send/2]).

-define(URL_FCM_BRIDGE, "http://172.17.0.1:3456").
-define(CONTENT_TYPE, "application/json").

start(Host, Opts) ->
    ?DEBUG("mod_fcm started", []),
    %% ?DEBUG("Host: ~p, URL : ~p", [Host, gen_mod:get_module_opt(Host, ?MODULE, url_send, fun(V) -> V end, undefined)]),
    %% send(Host, {[{tokens, [<<"token1">>, <<"token2">>]},{data, {[{del_ids, [<<"id1">>, <<"id2">>]}]}}]}),
  ok.

stop(_Host) ->
    ?DEBUG("mod_fcm stopped", [])
    .

depends(_Host, _Opts) ->
[].

mod_options(_Host) ->
[].

send(Host, Payload) ->
    ?DEBUG("Send - payload: ~p, host: ~p", [Payload, Host]),
    %% URL_FCM_BRIDGE = erlang:binary_to_list(gen_mod:get_module_opt(Host, ?MODULE, url_send, fun(V) -> V end, undefined)),
    ?DEBUG("url: ~p", [?URL_FCM_BRIDGE]),
    API_KEY = "key=dsjajdsadoi01002-3-ksdkaskk2",
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
   	{ok, RawResponse} = httpc:request(post, {?URL_FCM_BRIDGE, Header, ?CONTENT_TYPE, Body}, [{timeout, 5000}], []),
    ?DEBUG("RawResp ~p", [RawResponse]),
   	{{_, SCode, Status}, ResponseBody} = {element(1, RawResponse), element(3, RawResponse)},
   	%% TODO: Errors 5xx
   	case catch SCode of
   		200 -> ?DEBUG("mod_fcm: A message was sent, body: ~p", [ResponseBody]);
   		401 -> ?ERROR_MSG("mod_fcm: ~s ~p", [Status, ResponseBody]);
   		_ -> ?ERROR_MSG("mod_fcm: ~s", [ResponseBody])
   	end.
