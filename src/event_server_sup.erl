-module(event_server_sup).

%% API
-export([loop/1, init/1, start_link/2, start/2]).


start(Module, Args) ->
  spawn(?MODULE, init, [{Module, Args}]).


start_link(Module, Args) ->
  spawn_link(?MODULE, init, [{Module, Args}]).


init({Module, Args}) ->
  process_flag(trap_exit, true),
  loop({Module, start_link, Args}).


loop(L = {Module, FunName, Args}) ->
  Pid = apply(Module, FunName, Args),
  receive
    %% Manual shutdown.
    {'EXIT', _Pid, shutdown} ->
      exit(shutdown);  %% Will kill linked processes too.
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
      loop(L)
  end.
