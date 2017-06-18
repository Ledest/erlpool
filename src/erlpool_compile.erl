-module(erlpool_compile).
-author("silviu.caragea").

-define(GLOBALS_MODULE, erlpool_globals).
-define(STRINGIFY(A), ??A).

-export([compile_settings/1]).

compile_settings(SettingsList) ->
    case compile:noenv_forms(get_settings_code(SettingsList), []) of
        {ok, ?GLOBALS_MODULE, B} ->
            code:purge(?GLOBALS_MODULE),
            case code:load_binary(?GLOBALS_MODULE, ?STRINGIFY(?GLOBALS_MODULE) ".erl", B) of
                {module, ?GLOBALS_MODULE} -> ok;
                Error -> Error
            end;
        Error -> Error
    end.

get_settings_code(SettingsList) ->
    Last = length(SettingsList) * 2,
    {Exports, Body, _} = lists:foldr(fun({PoolName, PoolSize, _PoolGroup}, {EA, BA, L}) ->
                                         {[{PoolName, 0}|EA],
                                          [{function, L, PoolName, 0,
                                            [{clause, L, [], [], [erl_parse:abstract(PoolSize, L)]}]}|BA],
                                          L - 2}
                                  end, {[], [{eof, Last + 4}], Last + 3}, SettingsList),
    [{attribute, 1, file, {?STRINGIFY(?GLOBALS_MODULE) ".erl", 1}},
     {attribute, 1, module, ?GLOBALS_MODULE},
     {attribute, 3, export, Exports}|Body].

-compile({inline, [get_settings_code/1]}).
