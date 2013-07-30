
-module(weather).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, handle_call/3]).
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

handle_call(_Request, _From, State) ->
  case State =/= [] of
    true ->
      io:format("Most recent requests: ~p~n", [State]);
    false ->
      {reply, {ok, handle_request(_Request)}, State ++ _Request}
  end.

handle_request(StationCode) ->


