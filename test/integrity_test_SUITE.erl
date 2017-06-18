-module(integrity_test_SUITE).
-author("silviu.caragea").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
    {group, erlpool_group}
].

groups() -> [
    {erlpool_group, [sequence], [
        test_map,
        test_pid_round_robin,
        test_group
    ]}
].

init_per_suite(Config) ->
    ok = erlpool:start(),
    Args = [{start_mfa, {dummy_worker, start_link, [[]]}}],
    ok = erlpool:start_pool(pool1, [{size, 10} |Args]),
    ok = erlpool:start_pool(pool2, [{size, 5} |Args]),
    Config.

end_per_suite(_Config) ->
    ok = erlpool:stop_pool(pool1),
    %let pool2 to be destroyed by the app supervisor
    erlpool:stop().

test_map(_Config) ->
    L1 = erlpool:map(pool1, fun(X) -> X end),
    L2 = erlpool:map(pool2, fun(X) -> X end),
    Length1 = length(L1),
    Length2 = length(L2),
    Length1 = erlpool:pool_size(pool1),
    Length2 = erlpool:pool_size(pool2),
    true.

test_pid_round_robin(_Config) ->
    L1 = erlpool:map(pool1, fun(X) -> X end),
    Size = erlpool:pool_size(pool1),

    Fun = fun(_X, Acc) ->
        Pid = erlpool:pid(pool1),
        lists:delete(Pid, Acc)
    end,
    [] = lists:foldl(Fun, L1, lists:seq(1, Size)),
    true.


test_group(_Config) ->

    Args = [{start_mfa, {dummy_worker, start_link, [[]]}}],
    ok = erlpool:start_pool(gpool1, [{size, 2}, {group, g1} |Args]),
    ok = erlpool:start_pool(gpool2, [{size, 3}, {group, g1} |Args]),
    ok = erlpool:start_pool(gpool3, [{size, 4}, {group, g2} |Args]),
    ok = erlpool:start_pool(gpool4, [{size, 5}, {group, g2} |Args]),

    PG1 = whereis(erlpool_pool_sup:name(gpool1)),
    PG2 = whereis(erlpool_pool_sup:name(gpool2)),
    PG3 = whereis(erlpool_pool_sup:name(gpool3)),
    PG4 = whereis(erlpool_pool_sup:name(gpool4)),

    true = erlpool:restart_pool(gpool1),
    false = erlpool:restart_pool(gpool5),

    timer:sleep(100),

    PG11 = whereis(erlpool_pool_sup:name(gpool1)),

    ?assert(PG11 =/= undefined),
    ?assert(PG11 =/= PG1),

    ok = erlpool:restart_group(g2),

    timer:sleep(100),

    NPG1 = whereis(erlpool_pool_sup:name(gpool1)),
    NPG2 = whereis(erlpool_pool_sup:name(gpool2)),
    NPG3 = whereis(erlpool_pool_sup:name(gpool3)),
    NPG4 = whereis(erlpool_pool_sup:name(gpool4)),

    ?assert(NPG1 =:= PG11),
    ?assert(NPG2 =:= PG2),
    ?assert(NPG3 =/= PG3),
    ?assert(NPG4 =/= PG4),

    ok = erlpool:stop_group(g2),

    timer:sleep(100),

    NPG1 = whereis(erlpool_pool_sup:name(gpool1)),
    NPG2 = whereis(erlpool_pool_sup:name(gpool2)),
    undefined = whereis(erlpool_pool_sup:name(gpool3)),
    undefined = whereis(erlpool_pool_sup:name(gpool4)),
    true.

