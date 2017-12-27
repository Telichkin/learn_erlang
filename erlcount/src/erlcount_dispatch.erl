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
      InitState = dispatching,
      {ok, InitState, #data{regex = [{R, 0} || R <- Re]}};
    false ->
      {stop, invalid_regex}
  end.


%% The result messages are global, because they can be
%% received in either 'dispatching' or 'listening' states.
handle_event({complete, Regex, Ref, Count}, StateName, StateData=#data{regex = Re, refs = Refs}) ->
  {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
  NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
  NewData = StateData#data{regex = NewRe, refs = Refs -- [Ref]},

  case state of
    listening ->
      listening(done, NewData);
    _ ->
      {next_state, StateName, NewData}
  end.


handle_sync_event(Event, _From, StateName, StateData) ->
  io:format("Unexpected event ~p~n", [Event]),
  {next_state, StateName, StateData}.


handle_info({start, Dir}, StateName, StateData) ->
  gen_fsm:send_event(self(), erlcount_lib:find_erl_files(Dir)),
  {next_state, StateName, StateData}.


terminate(_Reason, _StateName, _StateData) ->
  ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.


dispatching({continue, FileName, FunNext}, StateData = #data{regex = Re, refs = Refs}) ->
  StartRegexWorker = fun ({Regex, _Count}, NewRefs) ->
    Ref = make_ref(),
    otp_pool:run_task_async(?POOL, [self(), Ref, FileName, Regex]),
    [Ref|NewRefs]
  end,

  NewRefs = lists:foldl(StartRegexWorker, Refs, Re),
  gen_fsm:send_event(self(), FunNext()),
  {next_state, dispatching, StateData#data{refs = NewRefs}};

dispatching(done, StateData) ->
  %% This is a special case. We can not assume that all messages have NOT
  %% been received by the time we hit 'done'. As such, we directly move to
  %% listening/2 without waiting for an external event.
  listening(done, StateData).


%% All 'done' messages received
listening(done, #data{regex = Re, refs = []}) ->
  [io:format("Regex ~s has ~p results~n", [R, F]) || {R, F} <- Re],
  {stop, normal, done};

listening(done, StateData) ->
  %%  Still listening
  {next_state, listening, StateData}.


%%%%%%%%%%%
%% Utils %%
%%%%%%%%%%%
valid_regex(Re) ->
  try re:run("", Re) of
    _ -> true
  catch
    error:badarg -> false
  end.