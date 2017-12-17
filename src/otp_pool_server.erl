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


handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info({start_worker_supervisor, OtpPoolSupervisor, ModuleFuncArg}, S=#state{}) ->
  {ok, Pid} = supervisor:start_child(OtpPoolSupervisor, ?WORKER_SPEC(ModuleFuncArg)),
  link(Pid),
  {noreply, S#state{supervisor = Pid}};

handle_info(Message, State) ->
  io:format("Unknown msg: ~p~n", [Message]),
  {ok, State}.

terminate(Reason, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).