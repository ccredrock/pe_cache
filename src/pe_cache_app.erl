%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <free>
%%% @doc
%%%
%%% @end
%%% Created : 2017年08月29日14:17:22
%%%-------------------------------------------------------------------
-module(pe_cache_app).

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
-behaviour(application).

%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    pe_cache:start(),
    pe_cache_sup:start_link().

stop(_State) ->
    ok.
