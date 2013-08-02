-module(person).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

start_link(Server) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Server, []).

init(Server) ->
  {ok, [Server, []]}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

handle_call(get_chat_node,_From, State) ->
  {Node, _} = State,
  {reply, {ok, Node}, State};

handle_call(get_profile, _From, State) ->
  {_, Profile} = State,
  {reply, {ok, Profile}, State};

handle_call({set_profile, Key, Value}, _From, State) ->
  {Node, Profile} = State, 
  NewProfile = set_profile(Key, Value, Profile),
  {reply, {ok, "Set " ++ Key ++ " to " ++ Value}, {Node, NewProfile}}.

set_profile(Key, Value, Profile) ->
  case lists:keymember(Key, 1, Profile) of
    false ->
      [{Key, Value} | Profile ];
    true ->
      lists:keyreplace(Key, 1, Profile, {Key, Value})
  end.

