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

start_link(Server) ->
  gen_server:start_link({local, ?CLIENT}, ?MODULE, Server, []).

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

handle_call(get_user, _From, State) ->
  {_, {User, _}} = State,
  {reply, {ok, User}, State};

handle_call({set_profile, Key, Value}, _From, State) ->
  {Node, {_, Profile}} = State,
  NewProfile = set_profile(Key, Value, Profile),
  {reply, {ok, "Set " ++ Key ++ " to " ++ Value}, {Node, NewProfile}}.

set_profile(Key, Value, Profile) ->
  case lists:keymember(Key, 1, Profile) of
    false ->
      [{Key, Value} | Profile ];
    true ->
      lists:keyreplace(Key, 1, Profile, {Key, Value})
  end.

get_chat_node() ->
  gen_server:call(person, get_chat_node).

login(UserName) ->
  UserNameAfterParse = parse_user_name(UserName),
  case UserNameAfterParse of 
    {ok, StringUserName} ->
      gen_server:call(chatroom, {login, StringUserName});
    {error, Error} ->
      io:format("~p~n", Error)
  end.

parse_user_name(UserName)  when is_atom(UserName) ->
  {ok, atom_to_list(UserName)};

parse_user_name(UserName)  when is_list(UserName) ->
  {ok, UserName};

parse_user_name(_) ->
  {error, "User name should be a string or atom."}.

logout() ->
  gen_server:call(chatroom, logout).

say(Text) ->
  gen_server:call(chatroom, {say, Text}).

users() ->
  gen_server:call(chatroom, users).

who(UserName, UserNode) ->
  gen_server:call(chatroom, {who, UserName, UserNode}).

set_profile(Key, Value) ->
  gen_server:call(?CLIENT, {set_profile, Key, Value}).
