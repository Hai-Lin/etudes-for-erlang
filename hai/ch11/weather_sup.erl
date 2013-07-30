
-module(weather_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).
-include_lib("xmerl/include/xmerl.hrl").


start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  Weather = {weather, {weather, start_link, []}, Restart, Shutdown, Type, [weather]},
  {ok, {SupFlags, [Weather]}}.

