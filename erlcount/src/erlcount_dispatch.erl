-module(erlcount_dispatch).
-behaviour(gen_fsm).

-export([start_link/0, complete/4]).
-export([init/1, dispatching/2, listening/2, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-define(POOL, erlcount).

-record(data, {regex = [],
               refs = []}).

%%%%%%%%%%%%%%%%
%% Public API %%
%%%%%%%%%%%%%%%%
start_link() ->
  gen_fsm:start_link(?MODULE, [], []).


complete(Pid, Regex, Ref, Count) ->
  gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).


%%%%%%%%%%%%%%%
%% Callbacks %%
%%%%%%%%%%%%%%%
init([]) ->
  {ok, Re} = application:get_env(regex),
  {ok, Dir} = application:get_env(directory),
  {ok, MaxFiles} = application:get_env(max_files),

  otp_pool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),

  case lists:all(fun valid_regex/1, Re) of
    true ->
      self() ! {start, Dir},
      {ok, dispatching, #data{regex = [{R, 0} || R <- Re]}};
    false ->
      {stop, invalid_regex}
  end.


handle_event(Event, StateName, StateData) ->
  erlang:error(not_implemented).


handle_sync_event(Event, From, StateName, StateData) ->
  erlang:error(not_implemented).


handle_info(Info, StateName, StateData) ->
  erlang:error(not_implemented).


terminate(Reason, StateName, StateData) ->
  erlang:error(not_implemented).


code_change(OldVsn, StateName, StateData, Extra) ->
  erlang:error(not_implemented).


dispatching(_Arg0, _Arg1) ->
  erlang:error(not_implemented).


listening(_Arg0, _Arg1) ->
  erlang:error(not_implemented).


%%%%%%%%%%%
%% Utils %%
%%%%%%%%%%%
valid_regex(Re) ->
  try re:run("", Re) of
    _ -> true
  catch
    error:badarg -> false
  end.