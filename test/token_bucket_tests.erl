-module(token_bucket_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {inorder,
     {setup,
      fun() -> token_bucket:start() end,
      fun(_) -> skip end,
      [{"add_bucket",
        fun() ->
                Props = [{time,1000},{count, 50}, {gap, 10}],
                ?assertEqual(ok, token_bucket:add_buckets(depot1, [{<<"tb1">>, Props}])),
                ?assertEqual(ok, token_bucket:add_buckets(depot1, [{<<"tb2">>, Props}])),
                ?assertEqual(ok, token_bucket:add_buckets(depot2, [{<<"tb1">>, Props}])),
                ?assertEqual(1, length(token_bucket:list_buckets(depot2))),
                Props1 = [{time,1000},{count, 30}, {gap, 10}],
                ?assertEqual(ok, token_bucket:add_buckets(depot1, [{<<"tb1">>, Props1}])),
                ?assertEqual(1, length(token_bucket:list_buckets(depot1)))
        end}
      ]}
    }.

