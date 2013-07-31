
-module(weather).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, report/1, recent/0, connect/1]).
-define(SERVER, ?MODULE).
-include_lib("xmerl/include/xmerl.hrl").


start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

init([]) ->
  inets:start(),
  {ok, []}.

handle_call(_Request, _From, State) ->
  Result = handle_request(_Request),
  {reply, Result, [_Request | State]}.

handle_cast(_Message, State) ->
  io:format("Most recent requests: ~p~n", [lists:reverse(State)]),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

handle_request(StationCode) ->
  URL = "http://w1.weather.gov/xml/current_obs/" ++ StationCode ++ ".xml",
  {Result, Info} = httpc:request(URL),
  case Result of 
    error -> {error, Info};
    ok ->
      handle_success_request(Info)
  end.

handle_success_request({{_, Code, _}, _, Data}) ->
  case Code of 
    200 ->
      {ok, analyze_info(Data)};
    _ ->
      {error, Code}
  end.

%% Take raw XML data and return a set of {key, value} tuples

analyze_info(WebData) ->
  %% list of fields that you want to extract
  ToFind = [location, observation_time_rfc822, weather, temperature_string],

  %% get just the parsed data from the XML parse result
  Parsed = element(1, xmerl_scan:string(WebData)),

  %% This is the list of all children under <current_observation>
  Children = Parsed#xmlElement.content,

  %% Find only XML elements and extract their names and their text content.
  %% You need the guard so that you don't process the newlines in the
  %% data (they are XML text descendants of the root element).
  ElementList = [{El#xmlElement.name, extract_text(El#xmlElement.content)}
    || El <- Children, element(1, El) == xmlElement],

  %% ElementList is now a keymap; get the data you want from it.
  lists:map(fun(Item) -> lists:keyfind(Item, 1, ElementList) end, ToFind).


%% Given the parsed content of an XML element, return its first node value
%% (if it's a text node); otherwise return the empty string.

extract_text(Content) ->
  Item = hd(Content),
  case element(1, Item) of
    xmlText -> Item#xmlText.value;
    _ -> ""
  end.

report(Station) ->
  gen_server:call({global, weather}, Station).

recent() ->
  gen_server:cast({global, weather}, "").

connect(NodeName) ->
  Result = net_adm:ping(NodeName),
  case Result of
    ping ->
      io:format("Successfully connect server ~p~n", [NodeName]);
    pang ->
      io:format("Fail to connect server ~p~n", [NodeName])
  end.

