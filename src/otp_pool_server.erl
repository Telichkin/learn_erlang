-module(otp_pool_server).
-behaviour(gen_server).

-export([start/4, start_link/4, run_task_strict/2, run_task_sync/2, run_task_async/2, stop_task/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(WORKER_SPEC(ModuleFuncArg), {
  worker_sup,
  {otp_pool_worker_sup, start_link, [ModuleFuncArg]},
  temporary, 10000, supervisor, [otp_pool_worker_sup]
}).

-record(state, {
  limit=0,
  supervisor,
  refs,  %% <- To know when a worker's done running and to fetch one from the queue to start it,
         %%    we will need to track each worker from the server. The sane way to do this is with
         %%    monitors, so we also keep all the monitor references in memory.
  queue = queue:new()
}).


%%%%%%%%%%%%%%%%
%% Public API %%
%%%%%%%%%%%%%%%%
start(Name, Limit, OtpPoolSupervisor, ModuleFuncArg) when is_atom(Name), is_integer(Limit) ->
  gen_server:start({local, Name}, ?MODULE, {Limit, ModuleFuncArg, OtpPoolSupervisor}, []).


start_link(Name, Limit, OtpPoolSupervisor, ModuleFuncArg) when is_atom(Name), is_integer(Limit) ->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, ModuleFuncArg, OtpPoolSupervisor}, []).


run_task_strict(Name, Args) ->
  gen_server:call(Name, {run_task_strict, Args}).


run_task_sync(Name, Args) ->
  gen_server:call(Name, {run_task_sync, Args}, infinity).


run_task_async(Name, Args) ->
  gen_server:cast(Name, {run_task_async, Args}).


stop_task(Name) ->
  gen_server:call(Name, stop).


%%%%%%%%%%%%%%%
%% Callbacks %%
%%%%%%%%%%%%%%%
init({Limit, ModuleFuncArg, OtpPoolSupervisor}) ->
  %% We need to send initial message to self because we want to avoid deadlock
  %% that can be created when we call supervisor:start_child on init. Because
  %% this init called by supervisor and supervisor is blocking by waiting answer
  %% from our init function.
  self() ! {start_worker_supervisor, OtpPoolSupervisor, ModuleFuncArg},
  {ok, #state{limit = Limit, refs = gb_sets:empty()}}.


handle_call({run_task_strict, Args}, _From,
            S=#state{limit = Limit, supervisor = WorkersSup, refs = Refs}) when Limit > 0 ->
  {ok, Pid} = supervisor:start_child(WorkersSup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit = Limit - 1, refs = gb_sets:add(Ref, Refs)}};

handle_call({run_task_strict, _Args}, _From, S=#state{limit=Limit}) when Limit =< 0 ->
  {reply, noalloc, S};

handle_call({run_task_sync, Args}, _From,
            S=#state{limit = Limit, supervisor = WorkerSup, refs = Refs}) when Limit > 0 ->
  {ok, Pid} = supervisor:start_child(WorkerSup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok, Pid}, S#state{limit = Limit - 1, refs = gb_sets:add(Ref, Refs)}};

handle_call({run_task_sync, Args}, From, S=#state{queue = Queue, limit = Limit}) when Limit =< 0 ->
  {noreply, S#state{queue = queue:in({From, Args}, Queue)}};

handle_call(stop, _From, State=#state{}) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State=#state{}) ->
  {noreply, State}.


handle_cast({run_task_async, Args},
            S=#state{limit = Limit, supervisor = WorkerSup, refs = Refs}) when Limit > 0 ->
  {ok, Pid} = supervisor:start_child(WorkerSup, Args),
  Ref = erlang:monitor(process, Pid),
  {noreply, S#state{limit = Limit - 1, refs = gb_sets:add(Ref, Refs)}};

handle_cast({run_task_async, Args}, S=#state{limit = Limit, queue = Queue}) when Limit =< 0 ->
  {noreply, S#state{queue = queue:in(Args, Queue)}};

handle_cast(_Msg, State=#state{}) ->
  {noreply, State}.


handle_info({start_worker_supervisor, OtpPoolSupervisor, ModuleFuncArg}, S=#state{}) ->
  {ok, Pid} = supervisor:start_child(OtpPoolSupervisor, ?WORKER_SPEC(ModuleFuncArg)),
  link(Pid),
  {noreply, S#state{supervisor = Pid}};

%% Whenever a worker goes down, we're notified of it. And it's time to dequeue something.
handle_info({'DOWN', Ref, process, _Pid, _}, S=#state{refs = Refs}) ->
  io:format("Worker complete a task~n"),
  case gb_sets:is_element(Ref, Refs) of
    true ->
      run_next_task(S#state{refs = gb_sets:delete(Ref, Refs)});
    false ->
      {noreply, S}
  end;

handle_info(Message, State) ->
  io:format("Unknown msg: ~p~n", [Message]),
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%%%%%%%%%
%% Utils %%
%%%%%%%%%%%
run_next_task(S=#state{limit = Limit, supervisor = WorkerSup, refs = Refs}) ->
  case queue:out(S#state.queue) of
    {{value, {From, Args}}, Queue} ->
      {ok, Pid} = supervisor:start_child(WorkerSup, Args),
      Ref = erlang:monitor(process, Pid),
      gen_server:reply(From, {ok, Pid}),
      {noreply, S#state{refs = gb_sets:add(Ref, Refs), queue = Queue}};

    {{value, Args}, Queue} ->
      {ok, Pid} = supervisor:start_child(WorkerSup, Args),
      Ref = erlang:monitor(process, Pid),
      {noreply, S#state{refs = gb_sets:add(Ref, Refs), queue = Queue}};

    {empty, _} ->
      {noreply, S#state{limit = Limit + 1}}
  end.