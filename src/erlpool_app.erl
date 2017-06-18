-module(erlpool_app).
-author("silviu.caragea").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = erlpool_manager:init(),
    {ok, _Pid} = R = erlpool_sup:start_link(),
    start_pools(),
    R.

stop(_State) -> ok.

start_pools() ->
    case application:get_env(erlpool, pools, []) of
        undefined -> ok;
        Pools -> lists:foreach(fun({Name, Args}) -> ok = erlpool:start_pool(Name, Args) end, Pools)
    end.

-compile({inline, [start_pools/0]}).
