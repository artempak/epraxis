-module(mserver).
-author(artemp).

-behavior(gen_server).

%% -export([start_link/0]).

-include_lib("kernel/include/file.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

start_link() ->
  gen_server:start_link({local, mserver}, ?MODULE, [], []).

init([]) ->
  io:format("init~n"),
  {ok, {0, 0, []}}.

handle_call(Req, From, State) ->
  {reply, State}.

handle_cast(Req, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  ok.

code_change(OldVsn, State, Extra) ->
  {ok, State}.

process(Path, [File | Rest], RegExp) ->
  AbsPath = filename:absname(File, Path),

  case file:read_file_info(AbsPath) of
    {ok, Info} ->

      case Info#file_info.type of
        regular ->
          msearch ! ev_file_added,
          spawn(?MODULE, spawn_single, [AbsPath, RegExp]);
        directory ->
          case file:list_dir(AbsPath) of
            {ok, Clist} -> process(AbsPath, Clist, RegExp);
            {error, Reason} -> io:format("~p~n", [Reason])
          end
      end,
      process(Path, Rest, RegExp);
    {error, _} -> error
  end.

spawn_single(Fpath, RegExp) ->
  case file:open(Fpath, read) of
    {ok, Dev} ->
      case check_lines(Dev, RegExp, 0) of
        {ok, Count} -> msearch ! {ev_file_done, {ok, {Fpath, Count}}};
        error -> msearch ! {error, {Fpath}}
      end;
    {error, Reason} ->
      io:format("~p~n", [Reason]),
      msearch ! {error, Fpath}
  end.

check_lines(Dev, RegExp, Count) ->
  case io:get_line(Dev, '') of
    {error, _} -> error;
    eof ->
      file:close(Dev),
      {ok, Count};
    Data ->
      case re:run(Data, RegExp, [global]) of
        {match, Res} -> check_lines(Dev, RegExp, Count + length(Res));
        _ -> check_lines(Dev, RegExp, Count)
      end
  end.

print_report(Result) ->
  io:format("===============================================~n~n"),
  Succeed = lists:filter(fun({Res, _Fdata}) -> Res =:= ok end, Result),
  Failed = lists:filter(fun({Res, _Fdata}) -> Res =:= error end, Result),

  io:format("Files processed: ~p~n", [length(Result)]),
  io:format("Succeed: ~p~n", [length(Succeed)]),
  io:format("Failed: ~p~n", [length(Failed)]),

  Total = lists:foldl(fun({ok, {_Fpath, Num}}, Acc) -> Num + Acc end, 0, Succeed),

  io:format("Number of occurences: ~p~n", [Total]).
