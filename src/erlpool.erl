-module(erlpool).

-author("silviu.caragea").

-define(POOL_SIZE(PoolName), erlpool_globals:PoolName()).

-export([start/0, start/1,
         stop/0,
         start_pool/2,
         stop_pool/1,
         restart_pool/1,
         stop_group/1,
         restart_group/1,
         pid/1,
         map/2,
         pool_size/1]).

-type pool_option() :: {start_mfa, {module(), atom(), [term()]}} |
                       {size, non_neg_integer()} |
                       {supervisor_period, non_neg_integer()} |
                       {supervisor_intensity, non_neg_integer()} |
                       {supervisor_restart, supervisor:restart()}.

-export_type([pool_option/0]).

-spec start() -> ok  | {error, any()}.
start() -> start(temporary).

-spec start(permanent | transient | temporary) -> ok | {error, any()}.
start(Type) ->
    case application:ensure_all_started(erlpool, Type) of
        {ok, _} -> ok;
        Other -> Other
    end.

-spec stop() -> ok.
stop() -> application:stop(erlpool).

-spec start_pool(atom(), [pool_option()]) -> ok | {error, any()}.
start_pool(PoolName, PoolArgs) -> erlpool_manager:new_pool(PoolName, PoolArgs).

-spec stop_pool(atom()) -> ok | {error, any()}.
stop_pool(PoolName) -> erlpool_manager:rem_pool(PoolName).

-spec restart_pool(atom()) -> boolean() | {error, any()}.
restart_pool(PoolName) ->
    case whereis(erlpool_pool_sup:name(PoolName)) of
        undefined -> false;
        Pid -> exit(Pid, kill)
    end.

-spec stop_group(term()) -> ok | {error, any()}.
stop_group(GroupName) -> erlpool_manager:rem_group(GroupName).

-spec restart_group(term()) -> ok | {error, any()}.
restart_group(GroupName) ->
    case erlpool_manager:get_pools(GroupName) of
        {ok, Pools} -> lists:foreach(fun restart_pool/1, Pools);
        Error -> Error
    end.

-spec pid(atom()) -> pid() | {error, any()}.
pid(PoolName) ->
    try
        N = ets:update_counter(PoolName, sq, {2, 1, ?POOL_SIZE(PoolName), 1}),
        [{N, Worker}] = ets:lookup(PoolName, N),
        Worker
    catch
        _:Error -> {error, Error}
    end.

-spec map(atom(), fun()) -> [term()] | {error, any()}.
map(PoolName, Fun) ->
    try
        lists:map(Fun, ets:foldl(fun({Id, Pid}, Acc) ->
                                     if
                                         is_integer(Id) -> [Pid|Acc];
                                         true -> Acc
                                     end
                                 end, [], PoolName))
    catch
        _:Error -> {error, Error}
    end.

-spec pool_size(atom()) -> non_neg_integer() | {error, any()}.
pool_size(PoolName) ->
    try
        ?POOL_SIZE(PoolName)
    catch
        _:Error -> {error, Error}
    end.
