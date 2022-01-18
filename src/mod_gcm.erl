%% Google Cloud Messaging for Ejabberd
%% Created: 02/08/2015 by mrDoctorWho
%% License: MIT/X11
%% https://github.com/mrDoctorWho/ejabberd_mod_gcm/blob/master/src/mod_gcm.erl

-module(mod_gcm).
-author("mrDoctorWho").

-include("logger.hrl").
-include_lib("../deps/xmpp/include/xmpp.hrl").

-behaviour(gen_mod).

-record(gcm_users, {user, gcm_token, last_seen, gcm_type}).

-define(NS_GCM, "https://fcm.googleapis.com/fcm").
-define(GCM_URL, ?NS_GCM ++ "/send").
-define(CONTENT_TYPE, "application/json").


-export([start/2, stop/1, message/1, iq/1, decode_iq_subel/1, mod_options/1, depends/2]).

-import('mod_fcm', [send/0]).

%% 114196@stackoverflow
-spec(url_encode(string()) -> string()).


-spec decode_iq_subel(xmpp_element() | xmlel()) -> xmpp_element() | xmlel().
%% Tell gen_iq_handler not to auto-decode IQ payload
decode_iq_subel(El) ->
    El.

escape_uri(S) when is_list(S) ->
    escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $. ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $- ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C == $_ ->
    [C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
    "".

escape_byte(C) ->
    "%" ++ hex_octet(C).

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].


url_encode(Data) ->
    url_encode(Data,"").

url_encode([],Acc) ->
    Acc;
url_encode([{Key,Value}|R],"") ->
    url_encode(R, escape_uri(Key) ++ "=" ++ escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ escape_uri(Key) ++ "=" ++ escape_uri(Value)).

%% because API_KEY is <<"api_key">> string and is not "api_key" string. "key=" ++ <<"api_key">> make error.
fix_api_key(S) when is_list(S) ->
	fix_api_key(unicode:characters_to_binary(S));
fix_api_key(<<C:8, Cs/binary>>) ->
	[C] ++ fix_api_key(Cs);
fix_api_key(<<>>) ->
    "".


%% Send an HTTP request to Google APIs and handle the response
send(Payload, API_KEY) ->
    ?DEBUG("Send with Key: ~p", [API_KEY]),
    Header = [{"Authorization", "key=" ++ fix_api_key(API_KEY)}, {"Content-Type", ?CONTENT_TYPE}],
    ?DEBUG("Pre Jiffy ~p", [Payload]),
	Body = try jiffy:encode(Payload) of
               BodyDec ->
                   BodyDec
           catch
               Aborted:Reason ->
                   ?DEBUG("Error encoding payload... ~p ~p", [Aborted, Reason]),
                   ?ERROR_MSG("Error encoding payload... ~p ~p", [Aborted, Reason]),
                   {}
           end,
    ?DEBUG("Sending payload: ~p", [Body]),
	ssl:start(),
	application:start(inets),
    ?DEBUG("Headers ~p", [Header]),
	{ok, RawResponse} = httpc:request(post, {?GCM_URL, Header, ?CONTENT_TYPE, Body}, [{timeout, 5000}], []),
	%% {{"HTTP/1.1",200,"OK"} ..}
	{{_, SCode, Status}, ResponseBody} = {element(1, RawResponse), element(3, RawResponse)},
	%% TODO: Errors 5xx
	case catch SCode of
		200 -> ?DEBUG("mod_gcm: A message was sent, body: ~p", [ResponseBody]);
		401 -> ?ERROR_MSG("mod_gcm: ~s ~p", [Status, ResponseBody]);
		_ -> ?ERROR_MSG("mod_gcm: ~s", [ResponseBody])
	end.


gcm_users_schema() ->
    {record_info(fields, gcm_users), #gcm_users{}}.

get_gcm_token(User, Server, riak) ->
  case ejabberd_riak:get(gcm_users, gcm_users_schema(),
                         {User, Server}) of
        {ok, #gcm_users{gcm_token = Token, gcm_type=Type}} ->
          {Token, Type};
      {error, notfound} ->
          {not_found, unknown};
      Err ->
          ?DEBUG("Err: ~p", [Err]),
          {not_found, unknown}
  end;
get_gcm_token(User, Server, mnesia) ->
    Result = mnesia:dirty_read(gcm_users, {User, Server}),
    case catch Result of
        [] ->
            {not_found, unknown};
        [#gcm_users{gcm_token = Token, gcm_type=Type}] ->
            {Token, Type}
    end.


%% TODO: Define some kind of a shaper to prevent floods and the GCM API to burn out :/
%% Or this could be the limits, like 10 messages/user, 10 messages/hour, etc
message({_Action, #message{from = From, to = To} = Pkt} = Acc) ->
    Packet = xmpp:encode(Pkt),
	Type = fxml:get_tag_attr_s(<<"type">>, Packet),
	?INFO_MSG("Offline message from=~p, type=~p, packet=~p", [From, Type, Packet]),
	case catch Type of
		<<"headline">> -> ok;
		_ ->
			%% Strings
            MessageTimestamp = fxml:get_path_s(Packet, [{elem, <<"servertimestamp">>}, cdata]),
            MessageId = fxml:get_path_s(Packet, [{elem, <<"msgid">>}, cdata]),
			JFrom = jid:encode(From#jid{user = From#jid.user, server = From#jid.server, resource = <<"">>}),
			JTo = jid:encode(To#jid{user = To#jid.user, server = To#jid.server, resource = <<"">>}),
			ToUser = To#jid.user,
			ToServer = To#jid.server,
            %% ServerKey = gen_mod:get_module_opt(ToServer, ?MODULE, gcm_api_key, fun(V) -> V end, undefined),
			BodyFull = fxml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
			Data = fxml:get_path_s(Packet, [{elem, <<"data">>}, cdata]),
            BodyLength = string:len(erlang:binary_to_list(BodyFull)),
            ?DEBUG("BodyFull: ~p ~p", [BodyLength, BodyFull]),
            Body = if
                       BodyLength > 2000 ->
                           <<"*Message too long to be displayed here.*">>;
                      true ->
                           BodyFull
                   end,
            ?DEBUG("Body: ~p", [Body]),
            case catch Body of
                <<>> -> ok; %% There is no body
                _ ->
                    {Token, GCMType} = get_gcm_token(ToUser, ToServer, mnesia), %%  {<<"123123">>, apns}, %%
                    ?DEBUG("Found FCM for user= ~p", [ToUser]),
                    ?DEBUG("Found Key: ~p", [Token]),
                    ?DEBUG("Found GCMType: ~p", [GCMType]),
                    case Token of
                        not_found ->
                            ok;
                        _ ->
                            case GCMType of
                                apns ->
                                    JToStringList = string:tokens(erlang:binary_to_list(JTo), "-"),
                                    JFromStringList = string:tokens(erlang:binary_to_list(JFrom), "-"),

                                    UserIdTo = lists:nth(1, JToStringList),
                                    UserIdFrom = lists:nth(1, JFromStringList),

                                    if
                                        UserIdTo == UserIdFrom ->
                                            %% prevent messages from being sent to my devices
                                            ok;
                                        true ->
                                        Payload = get_payload(Token, apns, Body, Data, JTo, JFrom, MessageId, MessageTimestamp),
                                        mod_fcm:send(ToServer, Payload)
                                    end,
                                    ok;
                                gcm ->
                                    Payload = get_payload(Token, gcm, Body, Data, JTo, JFrom, MessageId, MessageTimestamp),
                                    mod_fcm:send(ToServer, Payload),
                                    ok;
                                _ ->
                                    ok
                            end
                    end
            end
    end,
    Acc.

get_payload(Token, Type, Body, Data, JTo, JFrom, MessageId, MessageTimestamp) ->
    case Type of
        gcm ->
            {[
              {token, Token},
              {data,
               {[
                 {json, Data},
                 {messageTimestamp, MessageTimestamp},
                 {messageId, MessageId},
                 {fromJid, JFrom},
                 {toJid, JTo}
                ]}
              }
             ]};
        apns ->
            {[
              {token, Token},
              {apns, {[
                {headers, {[
                    {<<"apns-collapse-id">>, MessageId}
                ]}},
                {payload, {[
                    {aps, {[
                        {<<"mutable-content">>, true}
                    ]}}
                ]}}
              ]}},
              {notification, {[
                {title, <<"title defined by messaging service">>},
                {body, <<"body defined by messaging service">>}
               ]}},
              {data,
               {[
                 {mutable_content, <<"1">>},
                 {json, Data},
                 {messageTimestamp, MessageTimestamp},
                 {messageId, MessageId},
                 {fromJid, JFrom},
                 {toJid, JTo}
                ]}
              }
             ]};
        _ ->
            {}

end.

store_gcm_token(User, Server, Token, Timestamp, Type, mnesia) ->
    mnesia:dirty_write(#gcm_users{user={User, Server}, gcm_token=Token, last_seen=Timestamp,
                            gcm_type=Type
                           });

store_gcm_token(User, Server, Token, Timestamp, Type, riak) ->
    Obj = #gcm_users{
             user={User, Server}, gcm_token=Token, last_seen=Timestamp,
             gcm_type=Type},
    ejabberd_riak:put(Obj, gcm_users_schema()).



iq(#iq{from = #jid{luser = LUser, lserver = LServer, resource = FromRes}, type = _Type, sub_els = [SubEl]} = IQ) ->
	{MegaSecs, Secs, _MicroSecs} = now(),
	Timestamp = MegaSecs * 1000000 + Secs,
	Token = fxml:get_tag_cdata(fxml:get_subtag(SubEl, <<"key">>)),
  ?DEBUG("Processing gcm key...4", []),
  ResStr = erlang:binary_to_list(FromRes),
  Type = case string:str(ResStr, "android") of
               0 ->
                   apns;
               _ ->
                   gcm
           end,
  ?DEBUG("Processing gcm key... 5... user=~p, token=~p, Timestamp=~p, Type=~p", [LUser, Token, Timestamp, Type]),
  store_gcm_token(LUser, LServer, Token, Timestamp, Type, mnesia),
  xmpp:make_iq_result(IQ).


start(Host, Opts) ->
	mnesia:create_table(gcm_users, [{disc_copies, [node()]}, {attributes, record_info(fields, gcm_users)}]),
	case catch gen_mod:get_opt(gcm_api_key, Opts, fun(V) -> V end) of
		undefined -> ?ERROR_MSG("There is no API_KEY set! The GCM module won't work without the KEY!", []);
		_ ->
            %% IQDisc = gen_mod:get_opt(iqdisc, Opts,
                                     %% fun gen_iq_handler:check_type/1,
                                     %% one_queue),
            gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
                                          <<?NS_GCM>>, ?MODULE, iq),
            ssl:start(),
            application:start(inets),
            xmpp:register_codec(elenty_gcm),
            elenty_cache:create_cache(apn_name_cache, ets),
			ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, message, 49),
			?INFO_MSG("mod_gcm Has started successfully!", []),
			ok
  end.

stop(_Host) ->
    xmpp:unregister_codec(elenty_gcm).

depends(_Host, _Opts) ->
[].

mod_options(_Host) ->
[].
