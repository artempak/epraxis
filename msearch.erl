%%%-------------------------------------------------------------------
%%% @author artemp
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2017 2:32 PM
%%%-------------------------------------------------------------------
-module(msearch).
-author("artemp").

-include_lib("kernel/include/file.hrl").

-export([find_usages/2, loop/2, spawn_single/2]).

find_usages(Dir, Name) ->
  io:format("Directory: ~p ~nFunction: ~p ~n~n", [Dir, Name]),
  RegExp = Name ++ "(...)|" ++ Name ++ "()",

  register(msearch, spawn(?MODULE, loop, [process, {0, 0, []}])),

  case file:list_dir(Dir) of
    {ok, Flist} ->
      process(Dir, Flist, RegExp),
      msearch ! ev_summarize,
      io:format("Waiting for result...~n~n");
    {error, Error} ->
      io:format("App error: ~p~n", [Error]),
      error
  end.

process(_, [], _) ->
  ok;

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


loop(State, {FilesIn, FilesOut, Result}) ->
  receive
    ev_file_added ->
      loop(State, {FilesIn + 1, FilesOut, Result});
    {ev_file_done, Fdata} when (State == summarize) and (FilesIn == FilesOut + 1) ->
      print_report([Fdata|Result]);
    {ev_file_done, Fdata} ->
      loop(State, {FilesIn, FilesOut + 1, [Fdata | Result]});
    ev_summarize when FilesIn == FilesOut ->
      print_report(Result);
    ev_summarize ->
      loop(summarize, {FilesIn, FilesOut, Result});
    Any -> io:format("Unknown message received: ~p~n", [Any])
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

