
-module(weather).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-include_lib("xmerl/include/xmerl.hrl").


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  inets:start(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, {ok, handle_request(_Request)}, State ++ _Request}.

handle_cast(_Message, State) ->
  io:format("Most recent requests: ~p~n", [State]),
  {nonreply, State}.

handle_info(_Info, State) ->
  {nonreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

handle_request(StationCode) ->
  URL = "http://w1.weather.gov/xml/current_obs/" ++ StationCode ++ ".xml",
  {Result, Info} = httpc:request(URL).

