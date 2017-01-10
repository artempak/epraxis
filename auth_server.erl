-module(auth_server).
%% gen_server_mini_template
-behaviour(gen_server).

-export([start/0, stop/0, restart/0, new_user/2, authenticate/2, check_token/1, drop_user/1, list_tokens/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() -> 
  gen_server:start_link({local, auth}, ?MODULE, [], []),
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()).

stop() -> gen_server:cast(auth, stop).

restart() ->
  stop(),
  start().

new_user(Login, Password) -> gen_server:call(auth, {new, Login, Password}).

authenticate(Login, Password) -> gen_server:call(auth, {authenticate, Login, Password}).

check_token(Token) -> gen_server:call(auth, {check_token, Token}).

drop_user(Login) -> gen_server:call(auth, {drop_user, Login}).

list_tokens() -> gen_server:call(auth, {list_tokens}).

init([]) -> 
  dets:open_file(users, [{type, set}]),
  {ok, dict:new()}.

handle_call({new, Login, Password}, _From, State) -> 
  Reply = case dets:insert_new(users, {Login, Password}) of
    true -> {Login, "Successfully created"};
    false -> {Login, "User exists"}
  end,
  {reply, Reply, State};

handle_call({authenticate, Login, Password}, _From, State) -> 
  {NewState, Reply} = case dets:lookup(users, Login) of
    [] -> {State, {Login, "No such user exists"}};
    [{Login, Password}] -> 
      Token = generate_token(),
      {save_token(Token, Login, State), {Login, Token}};
    [{Login, _}] ->
      {State, {Login, "Wrong password"}}
  end,
  {reply, Reply, NewState};

handle_call({check_token, Token}, _From, State) -> 
  {reply, dict:is_key(Token, State), State};

handle_call({drop_user, Login}, _From, State) -> 
  {NewState, Reply} = case dets:lookup(users, Login) of
    [] -> {State, {Login, "No such user exists"}};
    [_] -> 
      dets:delete(users, Login),
      {try_remove_token(Login, State), {Login, "Done"}}
  end,
  {reply, Reply, NewState};

handle_call({list_tokens}, _From, State) -> 
  {reply, dict:to_list(State), State}.

handle_cast(stop, State) ->
  io:format("Stopping normally~n"),
  {stop, normal, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, Extra) -> {ok, State}.

save_token(Token, Login, State) ->
  io:format("Saving token~n"),
  {_, Secs, _} = os:timestamp(),
  dict:store(Token, {Login, Secs}, State).

try_remove_token(Login, State) ->
  dict:filter(fun(Key, Value) -> Value /= Login end, State).

generate_token() ->
  rand_string(10).

rand_string(Length) ->
  L = lists:seq(1, Length),
  lists:flatten([rand_char() || _Index <- L]).

rand_char() ->
  Chars = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!@#$%^&*()",
  Index = random:uniform(length(Chars)),
  lists:nth(Index, Chars).


