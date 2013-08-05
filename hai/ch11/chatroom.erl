
-module(chatroom).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call({login, UserName, ServerNode}, {Pid, _Tag}, State) -> 
  io:format("Sign in ~p~n", [Pid]),
  {Reply, NewState} = handle_login(UserName, ServerNode, Pid, State),
  {reply, Reply, NewState};

handle_call(logout, {Pid, _Tag}, State) ->
  {Reply, NewState} = handle_logout(Pid, State),
  {reply, Reply, NewState};

handle_call({say, Text}, {Pid, _Tag}, State) ->
  io:format("User say ~p~n", [Pid]),
  {Reply, NewState} = handle_say(Pid, State, Text),
  {reply, Reply, NewState};

handle_call({who, Person, ServerNode}, _From, State) ->
  {Reply, NewState} = handle_get_profile(Person, ServerNode, State),
  {reply, Reply, NewState};

handle_call(users, {Pid, _Tag}, State) ->
  io:format("Users ~p~n", [Pid]),
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

handle_login(UserName, ServerNode, Pid, State) -> io:format("User ~p in ~p try to login~n", [UserName, ServerNode]),
  case lists:keymember({UserName, ServerNode}, 1, State) of
    true ->
      io:format("User ~p cannot login because he is already login with ~p~p~p~n",[UserName, UserName, ServerNode, Pid]),
      {{error, "User " ++ UserName ++ " is already in " ++ atom_to_list(ServerNode)}, State};
    false ->
      io:format("User ~p in ~p~p succefully login~n",[UserName, ServerNode, Pid]),
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
  io:format("id ~p say ~p~n", [Pid, Text]),
  ServerAndUser = lists:keysearch(Pid, 2, State),
  case ServerAndUser of
    false ->
      {{error, "User not exist"}, State};
    _ ->
      {value, {{User, Server}, _}} = ServerAndUser,
      populate_text(User, Server, State, Text),
      {{ok, "Successfully populate text"}, State}
  end.

populate_text(FromUser, FromServer, State, Text) ->
  [gen_server:cast({person, Server}, {message, {FromUser, FromServer}, Text}) || {{User, Server}, _}  <- State, {User, Server} /= {FromUser, FromServer}].

handle_users(State) ->
  Users = [{User, Server, Pid} || {{User, Server}, Pid} <- State ],
  {{ok, Users}, State}.

handle_get_profile(_Person, Server, State) ->
  Reply = gen_server:call({person, Server}, get_profile),
  {Reply, State}.

