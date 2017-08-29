-module(pe_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Setup, fun() -> application:start(pe_cache) end).
-define(Clearnup, fun(_) -> application:stop(pe_cache) end).

basic_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [{"val",
         fun() ->
                 [{_, _}] = ets:lookup(pe_cache, test1),
                 pe_cache:write(test1, a, b),
                 {ok, b} = pe_cache:read(test1, a),
                 #{a := b} = get({pe_cache, test1})
         end}
      ]}
    }.

