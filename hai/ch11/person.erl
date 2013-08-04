-module(person).
-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         get_chat_node/0,
         login/1,
         logout/0,
         say/1,
         users/0,
         who/2,
         set_profile/2]).
-define(CLIENT, ?MODULE).
-record(state, {chat_node, user_name, profile}).

start_link(Server) ->
  gen_server:start_link({local, ?CLIENT}, ?MODULE, Server, []).

init(Server) ->
  io:format("Person start in server ~p~n", [Server]),
  {ok, #state{chat_node = Server, user_name = [], profile = []}}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

handle_call(get_chat_node,_From, State) ->
  {reply, {ok, State#state.chat_node}, State};

handle_call(get_profile, _From, State) ->
  {reply, {ok, State#state.profile}, State};

handle_call(get_user, _From, State) ->
  {reply, {ok, State#state.user_name}, State};

handle_call({login, UserName}, _From, State) ->
  {Reply, NewState} = handle_login(UserName, State),
  {reply, Reply, NewState};

handle_call({set_profile, Key, Value}, _From, State) ->
  NewProfile = set_profile(Key, Value, State#state.profile),
  NewState = #state{chat_node = State#state.chat_node, user_name = State#state.user_name, profile = NewProfile},
  {reply, {ok, "Set " ++ parse_atom(Key) ++ " to " ++ Value}, NewState}.

set_profile(Key, Value, Profile) ->
  case lists:keymember(Key, 1, Profile) of
    false ->
      [{Key, Value} | Profile ];
    true ->
      lists:keyreplace(Key, 1, Profile, {Key, Value})
  end.

handle_login(UserName, State) ->
  io:format("User ~p login server ~p~n", [UserName, State#state.chat_node]),
  Reply = gen_server:call({chatroom, State#state.chat_node}, {login, UserName, node()}),
  NewState = #state{chat_node = State#state.chat_node, user_name = UserName, profile = State#state.profile},
  io:format("reply: ~p NewState: ~p~n",[Reply, NewState]),
  {Reply, NewState}.

get_chat_node() ->
  {ok, ChatNode} = gen_server:call(?CLIENT, get_chat_node),
  io:format("Chat node ~p~n", [ChatNode]),
  ChatNode.

login(UserName) ->
  ParsedUserName = parse_atom(UserName),
  case ParsedUserName of 
    {ok, StringUserName} ->
      gen_server:call(?CLIENT, {login, StringUserName});
    {error, Error} ->
      io:format("~p~n", Error)
  end.

parse_atom(UserName)  when is_atom(UserName) ->
  {ok, atom_to_list(UserName)};

parse_atom(UserName)  when is_list(UserName) ->
  {ok, UserName};

parse_atom(_) ->
  {error, "User name should be a string or atom."}.

logout() ->
  gen_server:call({chatroom, get_chat_node()}, logout).

say(Text) ->
  gen_server:call({chatroom, get_chat_node()}, {say, Text}).

users() ->
  gen_server:call({chatroom, get_chat_node()}, users).

who(UserName, UserNode) ->
  gen_server:call({chatroom, get_chat_node()}, {who, UserName, UserNode}).

set_profile(Key, Value) ->
  gen_server:call(?CLIENT, {set_profile, Key, Value}).
