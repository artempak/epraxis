-module(fservice).
-author(artemp).

-include_lib("kernel/include/file.hrl").

-export([find_usage/2, loop/2]).



find_usage(Dir, Name) ->
  io:format("Directory: ~p ~nFunction: ~p ~n~n", [Dir, Name]),
  RegExp = list_to_binary(Name) + <<"(...)">> + <<"|">> + list_to_binary(Name) + <<"()">>,
  case file:list_dir(Dir) of
    {ok, Flist} -> process(Dir, Flist, RegExp);
    {error, Error} ->
      io:format("App error: ~p~n", [Error]),
      error
  end.

process(_, [], _) ->
%%  io:format("Directory is empty~n"),
  ok;

process(Path, [File | Rest], RegExp) ->
  %% io:format("processing ~p~n", [Path]),
  
  AbsPath = filename:absname(File, Path),

  case file:read_file_info(AbsPath) of
    {ok, Info} ->

      %% io:format("File: ~p info: ~p~n", [AbsPath, Info]),
      case Info#file_info.type of
        regular ->
          io:format("File: ~p~n", [AbsPath]);
          
        directory ->
          case file:list_dir(AbsPath) of
            {ok, Clist} -> process(AbsPath, Clist, RegExp);
            {error, Reason} -> io:format("~p~n", [Reason])
          end
      end,
      process(Path, Rest, RegExp);
    {error, _} -> error
  end.


%% init() -> loop({0, 0, []}).

loop(State, {FilesIn, FilesOut, Result}) ->
  receive
    {ev_file_added, _Fdata} ->
      loop(State, {FilesIn + 1, FilesOut, Result});
    {ev_file_done, Fdata} ->
      loop(State, {FilesIn, FilesOut + 1, [Fdata | Result]});
    %%{ev_summarize}
    _ -> io:format("Unknown message received!!!~n")
  end.

spawn_single(From, Fpath, RegExp) ->
  case file:open(Fpath, read) of
    {ok, Dev} ->
      case check_lines(Dev, RegExp, 0) of
        {ok, Count} -> {ok, Fpath, Count};
        error -> {error, Fpath}
      end
  end.

check_lines(Dev, RegExp, Count) ->
  case io:get_line(Dev) of
    oef -> {ok, Count};
    {error, _} -> error;
    Data ->
      case re:run(Data, RegExp, [global]) of
        {match, Res} -> check_lines(Dev, RegExp, Count + length(Res));
        _ -> check_lines(Dev, RegExp, Count)
      end
  end.
