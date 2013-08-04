
-module(chatroom).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call({login, UserName, ServerNode}, {Pid, _Tag}, State) -> 
  {Reply, NewState} = handle_login(UserName, ServerNode, Pid, State),
  {reply, Reply, NewState};

handle_call(logout, {Pid, _Tag}, State) ->
  {Reply, NewState} = handle_logout(Pid, State),
  {reply, Reply, NewState};

handle_call({say, Text}, {Pid, _Tag}, State) ->
  {Reply, NewState} = handle_say(Pid, State, Text),
  {reply, Reply, NewState};

handle_call({who, Person, ServerNode}, _From, State) ->
  {Reply, NewState} = handle_get_profile(Person, ServerNode, State),
  {reply, Reply, NewState};

handle_call(users, {_, _Tag}, State) ->
  {Reply, NewState} = handle_users(State),
  {reply, Reply, NewState}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

handle_login(UserName, ServerNode, Pid, State) ->
  io:format("User ~p in ~p try to login~n", [UserName, ServerNode]),
  case lists:keymember({UserName, ServerNode}, 1, State) of
    true ->
      io:format("User ~p in ~p~p succefully login",[UserName, ServerNode, Pid]),
      {{error, "User " ++ UserName ++ " is already in " ++ atom_to_list(ServerNode)}, State};
    false ->
      io:format("User ~p cannot login because he is already login with ~p~p~p",[UserName, UserName, ServerNode, Pid]),
      {{ok, "User " ++ UserName ++ " successfully log in " ++ atom_to_list(ServerNode)}, [{{UserName, ServerNode}, Pid} | State]}
  end.

handle_logout(Pid, State) ->
  case lists:keymember(Pid, 2, State) of 
    true ->
      {{ok, "Successfully log out"}, lists:keydelete(Pid, 2, State)};
    false ->
      {{error, "User not log in"}, State}
  end.

handle_say(Pid, State, Text) ->
  ServerAndUser = find_user_and_server(Pid, State),
  case ServerAndUser of
    false ->
      {{error, "User " ++ Pid ++ " not exist"}, State};
    _ ->
      {value, {User, Server}} = ServerAndUser,
      populate_text(User, Server, State, Text),
      {{ok, "Successfully populate text"}, State}
  end.

find_user_and_server(Pid, State) ->
  {{User, Server}, _} = lists:keysearch(Pid, 2, State),
  {User, Server}.

populate_text(FromUser, FromServer, State, Text) ->
  [gen_server:cast(Pid, {message, {FromUser, FromServer}, Text}) || {{User, Server}, Pid}  <- State, Server =:= FromServer, User /= FromUser].

handle_users(State) ->
  Users = [{User, Server, Pid} || {{User, Server}, Pid} <- State ],
  {{ok, Users}, State}.

handle_get_profile(Person, Server, State) ->
  Pid = find_pid(Person, Server, State),
  Reply = gen_server:call(Pid, get_profile),
  {Reply, State}.

find_pid(Person, Server, State) ->
  {{Person, Server}, Pid} = lists:keysearch({Person, Server}, 1, State),
  Pid.

