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

-export([qin/3, qfun/2, qfun/4, lstop/2]).

%%------------------------------------------------------------------------------
%% @doc process dict
%%------------------------------------------------------------------------------
-define(GET_TABLE(T),      get({?MODULE, cache, T})).
-define(SET_TABLE(T, V),   put({?MODULE, cache, T}, V)).
-define(GET_SIZE(T),       get({?MODULE, size, T})).
-define(SET_SIZE(T, V),    put({?MODULE, size, T}, V)).
-define(GET_QUEUE(Q),      get({?MODULE, queue, Q})).
-define(SET_QUEUE(Q, V),   put({?MODULE, queue, Q}, V)).

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

%%------------------------------------------------------------------------------
qin(QName, 0, _VList) -> ?SET_QUEUE(QName, queue:new());
qin(_QName, _, []) -> skip;
qin(QName, Len, VList) ->
    case ?GET_QUEUE(QName) of
        undefined ->
            Queue1 = qtrunc(qappend(VList, queue:new()), Len),
            ?SET_QUEUE(QName, Queue1),
            VList;
        Queue ->
            Queue1 = qtrunc(qappend(VList, Queue), Len),
            ?SET_QUEUE(QName, Queue1),
            VList
    end.

qappend([V | T], Queue) -> qappend(T, queue:in(V, Queue));
qappend([], Queue) -> Queue.

qtrunc(Queue, Len) ->
    case queue:len(Queue) > Len of
        true -> qtrunc(queue:drop(Queue), Len);
        false -> Queue
    end.

qfun(QName, Fun) ->
    case ?GET_QUEUE(QName) of
        undefined -> ok;
        {[],[]} -> ok;
        Queue ->
            List = queue:to_list(Queue),
            case lstop(Fun, List) of
                ok ->
                    ?SET_QUEUE(QName, queue:new()),
                    ok;
                {error, Left, Reason} ->
                    ?SET_QUEUE(QName, queue:from_list(Left)),
                    {error, Reason}
            end
    end.

qfun(QName, Fun, Len, List) ->
    case qfun(QName, Fun) of
        ok ->
            case lstop(Fun, List) of
                ok ->
                    ok;
                {error, Left, Reason} ->
                    qin(QName, Len, Left),
                    {error, Reason}
            end;
        {error, Reason} ->
            qin(QName, Len, List),
            {error, Reason}
    end.

lstop(Fun, [H | T] = List) ->
    case catch Fun(H) of
        {'EXIT', Reason} -> {error, List, Reason};
        _ -> lstop(Fun, T)
    end;
lstop(_Fun, []) -> ok.

