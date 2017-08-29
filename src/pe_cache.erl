%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright (C) 2017, <free>
%%% @doc
%%%
%%% @end
%%% Created : 2017年08月29日14:17:22
%%%-------------------------------------------------------------------
-module(pe_cache).

-export([start/0, start/1,
         read/2, write/3]).

%%------------------------------------------------------------------------------
%% @doc process dict
%%------------------------------------------------------------------------------
-define(GET_TABLE(Table),      get({pe_cache, Table})).
-define(SET_TABLE(Table, Val), put({pe_cache, Table}, Val)).
-define(GET_SIZE(Table),       get({pe_size, Table})).
-define(SET_SIZE(Table, Val),  put({pe_size, Table}, Val)).

%%------------------------------------------------------------------------------
start() ->
    Tables = application:get_env(pe_cache, tables, []),
    start(Tables).

start(Tables) ->
    start_cache(),
    ets:insert(?MODULE, [{Table, {PSize, ESize}}|| {Table, PSize, ESize} <- Tables]),
    [ets:new(X, [named_table, public,
                 {write_concurrency, true},
                 {read_concurrency, true}])
     || {X, _, _} <- Tables].

start_cache() ->
    case ets:info(?MODULE, size) of
        undefined -> ets:new(?MODULE, [named_table, public, {read_concurrency, true}]);
        _ -> skip
    end.

read(Table, Key) ->
    case ?GET_TABLE(Table) of
        #{Key := Val} -> {ok, Val};
        Map ->
            case ets:lookup(Table, Key) of
                [] -> error;
                [{_, Val}] -> set_map(Map, Table, Key, Val), {ok, Val}
            end
    end.

write(Table, Key, Val) ->
    {Size, _} = get_size(Table),
    case ets:info(Table, size) > Size of
        false ->
            ets:insert(Table, {Key, Val});
        true ->
            ets:delete_all_objects(Table),
            ets:insert(Table, {Key, Val})
    end.

%%------------------------------------------------------------------------------
set_map(Map, Table, Key, Val) ->
    {Size, _} = get_size(Table),
    case Map of
        #{} when map_size(Map) < Size ->
            ?SET_TABLE(Table, Map#{Key => Val});
        _ ->
            ?SET_TABLE(Table, #{Key => Val})
    end.

get_size(Table) ->
    case ?GET_SIZE(Table) of
        undefined ->
            [Size] = ets:lookup(?MODULE, Table),
            ?SET_SIZE(Table, Size), Size;
        Size -> Size
    end.

