%%%----------------------------------------------------------------------
%%% File    : mod_last.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:last support (XEP-0012)
%%% Created : 24 Oct 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_elenty_last).

-author('alexey@process-one.net').

-protocol({xep, 12, '2.0'}).

-behaviour(gen_mod).


-export([start/2, stop/1, reload/3, process_local_iq/1, export/1,
	 process_sm_iq/1, on_presence_update/4, import_info/0,
	 import/5, import_start/2, store_last_info/4, get_last_info/2,
	 remove_user/2, mod_opt_type/1, mod_options/1,
	 register_user/2, depends/2]).

-include("logger.hrl").

-include_lib("../deps/xmpp/include/xmpp.hrl").
%% -include("xmpp.hrl").

-include("mod_privacy.hrl").
-include("mod_last.hrl").

-define(LAST_CACHE, last_activity_cache).

-callback init(binary(), gen_mod:opts()) -> any().
-callback import(binary(), #last_activity{}) -> ok | pass.
-callback get_last(binary(), binary()) ->
    {ok, non_neg_integer(), binary()} | not_found | {error, any()}.
-callback store_last_info(binary(), binary(), non_neg_integer(), binary()) -> ok | {error, any()}.
-callback remove_user(binary(), binary()) -> {atomic, any()}.
-callback use_cache(binary()) -> boolean().
-callback cache_nodes(binary()) -> [node()].

-define(STANZA_ERRORT(Code, Type, Condition, Lang, Text),
        {xmlelement, "error",
         [{"code", Code}, {"type", Type}],
         [{xmlelement, Condition, [{"xmlns", ?NS_STANZAS}], []},
          {xmlelement, "text", [{"xmlns", ?NS_STANZAS}],
           [{xmlcdata, translate:translate(Lang, Text)}]}]}).

-define(ERRT_SERVICE_UNAVAILABLE(Lang, Text),
        ?STANZA_ERRORT("503", "cancel", "service-unavailable", Lang, Text)).
-define(ERRT_NOT_ACCEPTABLE(Lang, Text),
        ?STANZA_ERRORT("406", "modify", "not-acceptable", Lang, Text)).
-define(ERRT_NOT_ALLOWED(Lang, Text),
        ?STANZA_ERRORT("405", "cancel", "not-allowed", Lang, Text)).
-define(ERR_INTERNAL_SERVER_ERROR,
        ?STANZA_ERROR("500", "wait",   "internal-server-error")).
-define(ERRT_INTERNAL_SERVER_ERROR(Lang, Text),
        ?STANZA_ERRORT("500", "wait",   "internal-server-error", Lang, Text)).

start(Host, Opts) ->
    %% Mod = gen_mod:db_mod(Host, Opts, ?MODULE), // from 18.06 to 21.04
	Mod = gen_mod:db_mod(Host, ?MODULE),
	?DEBUG("~p", [Mod]),
    Mod:init(Host, Opts),
    ?WARNING_MSG("Mod_elenty_last starting!", []),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_LAST, ?MODULE, process_local_iq),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host,
				  ?NS_LAST, ?MODULE, process_sm_iq),
    ejabberd_hooks:add(register_user, Host, ?MODULE,
		       register_user, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
		       on_presence_update, 50).

stop(Host) ->
    ejabberd_hooks:delete(register_user, Host, ?MODULE,
			  register_user, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user, 50),
    ejabberd_hooks:delete(unset_presence_hook, Host,
			  ?MODULE, on_presence_update, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_LAST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host,
				     ?NS_LAST).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
	    NewMod:init(Host, NewOpts);
       true ->
	    ok
    end,
    init_cache(NewMod, Host, NewOpts).

%%%
%%% Uptime of ejabberd node
%%%

-spec process_local_iq(iq()) -> iq().
process_local_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_local_iq(#iq{type = get} = IQ) ->
    xmpp:make_iq_result(IQ, #last{seconds = get_node_uptime()}).

%% @spec () -> integer()
%% @doc Get the uptime of the ejabberd node, expressed in seconds.
%% When ejabberd is starting, ejabberd_config:start/0 stores the datetime.
get_node_uptime() ->
    case ejabberd_config:get_option(node_start) of
        undefined ->
            trunc(element(1, erlang:statistics(wall_clock)) / 1000);
        Now ->
            p1_time_compat:system_time(seconds) - Now
    end.

%%%
%%% Serve queries about user last online
%%%

-spec process_sm_iq(iq()) -> iq().
process_sm_iq(#iq{type = set, lang = Lang} = IQ) ->
    Txt = <<"Value 'set' of 'type' attribute is not allowed">>,
    xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_sm_iq(#iq{from = _From, to = To, lang = _Lang} = IQ) ->
    User = To#jid.luser,
    Server = To#jid.lserver,
    get_last_iq(IQ, User, Server).

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found | {error, Reason}
-spec get_last(binary(), binary()) -> {ok, non_neg_integer(), binary()} |
				      not_found | {error, any()}.
get_last(LUser, LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Res = case use_cache(Mod, LServer) of
              true ->
                  ets_cache:lookup(
                    ?LAST_CACHE, {LUser, LServer},
                    fun() -> Mod:get_last(LUser, LServer) end);
              false ->
                  Mod:get_last(LUser, LServer);
              undefined ->
                  error
          end,
    case Res of
        {ok, {TimeStamp, Status}} -> {ok, TimeStamp, Status};
        error -> not_found;
        Err -> Err
    end.

timestamp_to_str(TimeStamp) ->
    DateTime = calendar:gregorian_seconds_to_datetime(TimeStamp + 62167219200),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",[Year,Month,Day,Hour,Minute,Second])).

build_response_attrs(Sec, TimeStamp, DateTime) ->
    [{<<"xmlns">>, ?NS_LAST},
     {<<"seconds">>,
      iolist_to_binary(integer_to_list(Sec))
     },
     {<<"epochtimestamp">>,
      iolist_to_binary(integer_to_list(TimeStamp))
     },
     {<<"timestamp">>, iolist_to_binary(DateTime)}
    ].

generate_last_iq(#iq{lang = Lang} = IQ, LUser, LServer) ->
    case get_last(LUser, LServer) of
        {error, _Reason} ->
            Txt = <<"Database failure">>,
            xmpp:make_error(IQ, xmpp:err_internal_server_error(Txt, Lang));
        not_found ->
            Txt = <<"No info about last activity found">>,
            xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang));
        {ok, TimeStamp, Status} ->
            DateTime = timestamp_to_str(TimeStamp),
            TimeStamp2 = p1_time_compat:system_time(seconds),
            Sec = TimeStamp2 - TimeStamp,
            xmpp:make_iq_result(IQ,
                                #xmlel{name = <<"query">>,
                                       attrs = build_response_attrs(Sec, TimeStamp, DateTime),
                                       children = [{xmlcdata, Status}]}
                               )
    end.

get_last_iq(IQ, LUser, LServer) ->
    case ejabberd_sm:get_user_resources(LUser, LServer) of
        [] ->
            generate_last_iq(IQ, LUser, LServer);
        [R] ->
            %% Before immediately returning 0 because the user has a session
            %% first confirm if the other user's session is active or not!
            Pid = ejabberd_sm:get_session_pid(LUser, LServer, R),
            Pres = ejabberd_c2s:get_presence(Pid),
            #presence{type=PType} = Pres,
            case PType of
                available ->
                    xmpp:make_iq_result(IQ, #last{seconds = 0});
                _ ->
                    generate_last_iq(IQ, LUser, LServer)
            end
    end.

register_user(User, Server) ->
    on_presence_update(
       User,
       Server,
       <<"RegisterResource">>,
       <<"Registered but didn't login">>).

on_presence_update(User, Server, _Resource, Status) ->
    %%% Only actually store new status time
    %%% if the user was actually online before
    TimeStamp = p1_time_compat:system_time(seconds),
    store_last_info(User, Server, TimeStamp, Status).
    %% The below was an attempt to only store an updated presence time
    %% if the user was previously active.  However the hook is run
    %% AFTER ejabberd_c2s cuts off the session, so there is no PID,
    %% so this always does nothing, will need a different way to lookup
    %% the last presence
    %% case ejabberd_sm:get_session_pid(User, Server, Resource) of
    %%     none ->
    %%         ok.
    %%     Pid ->
    %%         {_User2, _Resource2, Status, _} = ejabberd_c2s:get_last_presence(Pid),
    %%         ?DEBUG("Storing presnece if stauts is available: ~p", [Status]),
    %%         case Status of
    %%             <<"available">> ->
    %%                 TimeStamp = p1_time_compat:system_time(seconds),
    %%                 store_last_info(User, Server, TimeStamp, Status);
    %%             _ ->
    %%                 ok
    %%         end
    %% end.

-spec store_last_info(binary(), binary(), non_neg_integer(), binary()) -> any().
store_last_info(User, Server, TimeStamp, Status) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    case use_cache(Mod, LServer) of
	true ->
	    ets_cache:update(
	      ?LAST_CACHE, {LUser, LServer}, {ok, {TimeStamp, Status}},
	      fun() ->
		      Mod:store_last_info(LUser, LServer, TimeStamp, Status)
	      end, cache_nodes(Mod, LServer));
	false ->
	    Mod:store_last_info(LUser, LServer, TimeStamp, Status)
    end.

%% @spec (LUser::string(), LServer::string()) ->
%%      {ok, TimeStamp::integer(), Status::string()} | not_found
get_last_info(LUser, LServer) ->
    case get_last(LUser, LServer) of
      {error, _Reason} -> not_found;
      Res -> Res
    end.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:remove_user(LUser, LServer),
    ets_cache:delete(?LAST_CACHE, {LUser, LServer}, cache_nodes(Mod, LServer)).


-spec init_cache(module(), binary(), gen_mod:opts()) -> ok.
init_cache(Mod, Host, Opts) ->
    case use_cache(Mod, Host) of
	true ->
	    CacheOpts = cache_opts(Opts),
	    ets_cache:new(?LAST_CACHE, CacheOpts);
	false ->
	    ets_cache:delete(?LAST_CACHE)
    end.

-spec cache_opts(gen_mod:opts()) -> [proplists:property()].
cache_opts(Opts) ->
    MaxSize = gen_mod:get_opt(cache_size, Opts),
    CacheMissed = gen_mod:get_opt(cache_missed, Opts),
    LifeTime = case gen_mod:get_opt(cache_life_time, Opts) of
		   infinity -> infinity;
		   I -> timer:seconds(I)
	       end,
    [{max_size, MaxSize}, {cache_missed, CacheMissed}, {life_time, LifeTime}].

-spec use_cache(module(), binary()) -> boolean().
use_cache(Mod, Host) ->
    case erlang:function_exported(Mod, use_cache, 1) of
	true -> Mod:use_cache(Host);
	false -> gen_mod:get_module_opt(Host, ?MODULE, use_cache)
    end.

-spec cache_nodes(module(), binary()) -> [node()].
cache_nodes(Mod, Host) ->
    case erlang:function_exported(Mod, cache_nodes, 1) of
	true -> Mod:cache_nodes(Host);
	false -> ejabberd_cluster:get_nodes()
    end.

import_info() ->
    [{<<"last">>, 3}].

import_start(LServer, DBType) ->
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:init(LServer, []).

import(LServer, {sql, _}, DBType, <<"last">>, [LUser, TimeStamp, State]) ->
    TS = case TimeStamp of
             <<"">> -> 0;
             _ -> binary_to_integer(TimeStamp)
         end,
    LA = #last_activity{us = {LUser, LServer},
                        timestamp = TS,
                        status = State},
    Mod = gen_mod:db_mod(DBType, ?MODULE),
    Mod:import(LServer, LA).

export(LServer) ->
    Mod = gen_mod:db_mod(LServer, ?MODULE),
    Mod:export(LServer).

depends(_Host, _Opts) ->
    [].

mod_opt_type(db_type) -> fun(T) -> ejabberd_config:v_db(mod_last, T) end;
mod_opt_type(O) when O == cache_life_time; O == cache_size ->
    fun (I) when is_integer(I), I > 0 -> I;
        (infinity) -> infinity
    end;
mod_opt_type(O) when O == use_cache; O == cache_missed ->
    fun (B) when is_boolean(B) -> B end.

mod_options(Host) ->
    %% [{db_type, ejabberd_config:default_db(Host, mod_last)},
    %% {use_cache, ejabberd_config:use_cache(Host)},
    %% {cache_size, ejabberd_config:cache_size(Host)},
    %% {cache_missed, ejabberd_config:cache_missed(Host)},
    %% {cache_life_time, ejabberd_config:cache_life_time(Host)}].

	[{db_type, ejabberd_config:default_db(Host, mod_last)},
	{use_cache, ejabberd_config:get_option({use_cache, Host})},
    {cache_size, ejabberd_config:get_option({cache_size, Host})},
    {cache_missed, ejabberd_config:get_option({cache_missed, Host})},
    {cache_life_time, ejabberd_config:get_option({cache_life_time, Host})}].
