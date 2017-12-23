-module(erlcount_lib).

-export([find_erl_files/1, dequeue_and_run/1, find_all/1]).
-include_lib("kernel/include/file.hrl").


find_erl_files(DirectoryName) ->
  find_erl_files(DirectoryName, queue:new()).


find_erl_files(FileOrDirName, Queue) ->
  {ok, FileOrDir = #file_info{}} = file:read_file_info(FileOrDirName),

  case FileOrDir#file_info.type of
    regular ->
      handle_file(FileOrDirName, Queue);
    directory ->
      handle_dir(FileOrDirName, Queue);
    _Other ->
      dequeue_and_run(Queue)
  end.


handle_file(FileName, Queue) ->
  case filename:extension(FileName) of
    ".erl" ->
      {continue, FileName, fun() -> dequeue_and_run(Queue) end};
    _NonErl ->
      dequeue_and_run(Queue)
  end.


handle_dir(DirName, Queue) ->
  case file:list_dir(DirName) of
    {ok, []} ->
      dequeue_and_run(Queue);
    {ok, Files} ->
      dequeue_and_run(enqueue_many(DirName, Files, Queue))
  end.


dequeue_and_run(Queue) ->
  case queue:out(Queue) of
    {empty, _} ->
      done;
    {{value, File}, NewQueue} ->
      find_erl_files(File, NewQueue)
  end.


enqueue_many(DirName, Files, Queue) ->
  AddToQueue = fun (File, Q) -> queue:in(filename:join(DirName, File), Q) end,
  lists:foldl(AddToQueue, Queue, Files).
