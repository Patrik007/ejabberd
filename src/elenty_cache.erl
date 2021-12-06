-module(elenty_cache).

-include("logger.hrl").
%% -include("xmpp.hrl").


-export([
         create_cache/2,
         cache_add/4,
         cache_get/3,
         cache_remove/3
        ]).

-record(cacheentry, {key,
                     value,
                     last_updated}).

create_cache(CacheName, mnesia) ->
    mnesia:create_table(CacheName,
			      [{ram_copies, [node()]},
                   {type, set},
			       {attributes,
                    record_info(fields, cacheentry)}]);

create_cache(CacheName, ets) ->
    ets:new(CacheName, [set, named_table, public, {read_concurrency, true}, {keypos, 1}]).

cache_add(CacheName, Key, Value, mnesia) ->
    %%?DEBUG("Cache Write for ~p", [Key]),
    mnesia:dirty_write(CacheName, {CacheName, Key, Value,
                                   calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                                  });

cache_add(CacheName, Key, Value, ets) ->
    %%?DEBUG("Cache Write for ~p", [Key]),
    true = ets:insert(CacheName, {Key, Value,
                                 calendar:datetime_to_gregorian_seconds(calendar:universal_time())}).

is_too_old(Time) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Delta = Now - Time,
    Delta > 60 * 60.


cache_get(CacheName, Key, ets) ->
    %%?DEBUG("Cache Get for ~p ~p ets", [CacheName, Key]),
    case catch ets:lookup(CacheName, Key) of
        {'EXIT', _} ->
            false;
        [] ->
            false;
        [{Key, Val, LastUpdated}] ->
            TooOld = is_too_old(LastUpdated),
            case TooOld of
                true ->
                    false;
                false ->
                    Val
            end
    end;

cache_get(CacheName, Key, mnesia) ->
    %%?DEBUG("Cache Get for ~p ~p mnesia", [CacheName, Key]),
    case catch mnesia:dirty_read(CacheName, Key) of
        {CacheName, Key, Value, LastUpdated} ->
            TooOld = is_too_old(LastUpdated),
            case TooOld of
                false ->
                    false;
               true ->
                    Value
            end;
        _ ->
            false
    end.

cache_remove(CacheName, Key, mnesia) ->
    %%?DEBUG("Cache Remove for ~p", [Key]),
    mnesia:dirty_delete(CacheName, Key);

cache_remove(CacheName, Key, ets) ->
    %%?DEBUG("Cache Remove for ~p", [Key]),
    true = ets:delete(CacheName, Key).
