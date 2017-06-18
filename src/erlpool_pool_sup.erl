-module(erlpool_pool_sup).

-author("silviu.caragea").

-define(DEFAULT_MAX_PERIOD_SEC, 1).
-define(DEFAULT_MAX_INTENSITY, 100).
-define(DEFAULT_SUP_RESTART_STRATEGY, permanent).

-behaviour(supervisor).

-export([start_link/2,
         init/1,
         start_worker/3,
         name/1]).

start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, name(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

name(PoolName) -> list_to_atom(lists:concat([?MODULE_STRING "_", PoolName, "_sup"])).

%internals

init([PoolName, PoolArgs]) ->
    SupRestartStrategy = proplists:get_value(supervisor_restart, PoolArgs, ?DEFAULT_SUP_RESTART_STRATEGY),
    MFA = proplists:get_value(start_mfa, PoolArgs),
    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(PoolTable, [{sq, 0}]),
    {ok,
     {{one_for_one,
       proplists:get_value(supervisor_intensity, PoolArgs, ?DEFAULT_MAX_INTENSITY),
       proplists:get_value(supervisor_period, PoolArgs, ?DEFAULT_MAX_PERIOD_SEC)},
      [children_specs(Id, SupRestartStrategy, [Id, PoolTable, MFA]) || Id <- lists:seq(1, proplists:get_value(size, PoolArgs))]}}.

start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = R = apply(M, F, A),
    true = ets:insert(PoolTable, {Id, Pid}),
    R.

children_specs(Name, SupRestartStrategy, Args) ->
    {Name, {?MODULE, start_worker, Args}, SupRestartStrategy, 2000, worker, [?MODULE]}.

-compile({inline, [children_specs/3]}).
