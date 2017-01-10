-module(fserver).
-author(artemp).

-export([start/0, init/0, stop/0, call/1]).


start() ->
  register(fserver, spawn(?MODULE, init, [])).

init() ->
  loop(idle).

 stop() when ->
  fserver ! {stop, self()},
  receive {reply, Reply} -> Reply end.

call(Msg) ->
  fserver ! {request, self(), Msg},
  receive 
    {reply, Reply} -> Reply 
  end.

reply(To, Msg) ->
  To ! {reply, Msg}.

loop(State) ->
  receive
    {request, From, Msg} ->
      {Reply, NewState} = handle_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    {stop, From} ->
      reply(From, ok)
  end.

handle_msg(Msg, State) ->
  io:format("~p ~p ~n", [Msg, State]),
  {ok, ok}.
