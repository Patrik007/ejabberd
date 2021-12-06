%%%----------------------------------------------------------------------
%%% File    : mod_elenty_servertimestamp.erl
%%% Purpose : Elenty Server Timestamp -- add timestamps to certain messages
%%% Created : 1 June 2014 by Zach Goldberg <zach@sticksandbrains.com>
%%%
%%%
%%% Copyright (C) 2014 Sticks and Brains
%%%
%%%----------------------------------------------------------------------

-module(mod_elenty_servertimestamp).

-export([start/2, stop/1, local_filter_packet/1, mod_options/1, mod_opt_type/1, depends/2, get_servertimestamp_elm/0]).

-export([]).

-include("logger.hrl").
%% -include("xmpp.hrl").
-include_lib("../deps/xmpp/include/xmpp.hrl").

start(_, _) ->
    ejabberd_hooks:add(filter_packet, global,
                       ?MODULE, local_filter_packet, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(filter_packet, Host,
                          ?MODULE, local_filter_packet, 10).

depends(_Host, _Opts) ->
[].

add_timestamp(El) ->
    %% If there's already a timestamp don't add another one
    Els = xmpp:get_els(El),
    case fxml:get_subtag(#xmlel{children=Els}, <<"servertimestamp">>) of
        false ->
            %% {Mega, Sec, Micro} = now(),
            %% Timestamp = Mega * 1000000 + Sec + (Micro / 1000000),
            %% CData = {xmlcdata, binary:list_to_bin(io_lib:format("~p", [Timestamp]))},
            %% TimeTag = #xmlel{name= <<"servertimestamp">>,
            %%                  children= [CData]},
            xmpp:append_subtags(El, [get_servertimestamp_elm()]);
        _ ->
            El
    end.

get_servertimestamp_elm() ->
    {Mega, Sec, Micro} = now(),
    Timestamp = Mega * 1000000 + Sec + (Micro / 1000000),
    CData = {xmlcdata, binary:list_to_bin(io_lib:format("~p", [Timestamp]))},
    #xmlel{name= <<"servertimestamp">>,
                 children= [CData]}.

local_filter_packet(#message{} = Msg) ->
    add_timestamp(Msg);

local_filter_packet(#iq{} = IQ) ->
    ?DEBUG("~p", [IQ]),
%%    add_timestamp(IQ);
    IQ;
%%    Els = xmpp:get_els(IQ),
%%    case fxml:get_subtag(#xmlel{children=Els}, <<"query">>) of
%%        false ->
%%            add_timestamp(IQ);
%%        _Query ->
%%            IQ
%%    end;

local_filter_packet(#presence{} = Pres) ->
    add_timestamp(Pres);

local_filter_packet(Pkt) ->
    Pkt.

mod_opt_type(_) ->
    none.

mod_options(_Host) ->
    [].
