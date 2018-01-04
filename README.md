# Learn Erlang

Learn Erlang with ["Learn you some Erlang from great good!"](http://learnyousomeerlang.com)


## Useful chapters

- [records explain](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#records)
- [spawn_link explain](http://learnyousomeerlang.com/errors-and-processes#links)
- [trap_exit explain](http://learnyousomeerlang.com/errors-and-processes#its-a-trap)
- [monitors explain](http://learnyousomeerlang.com/errors-and-processes#monitors)


## OTP CheatSheet

### Supervisor
![Supervisor CheatSheet](https://raw.githubusercontent.com/Telichkin/learn_erlang/master/cheatsheet/supervisor_cheatsheet.png)

```erlang
-module(supervisor_cheatsheet).
-behaviour(supervisor).

-export([start_link/1, start_child/1]).
-export([init/1]).

%%%%%%%%%%%%
%% Client %%
%%%%%%%%%%%%
% The supervisor is not registered and can be accessed only by Pid
start_link(not_registered) ->
  %% Supervisor process calls Module:init/1. To ensure a synchronized
  %% startup procedure, start_link/2,3 does not return until Module:init/1 has
  %% returned and all child processes have been started
  Module = ?MODULE,

  %% Argument is any term that is passed as the argument to Module:init/1
  Argument = static,
  supervisor:start_link(Module, Argument);
  %% Returns {ok, Pid} If the supervisor and its child processes are successfully created

% The supervisor is registered and can be accessed by Name or Pid
start_link(registered) ->
  Module = ?MODULE,

  N = random:uniform(3),
  Argument = dynamic,
  supervisor:start_link(supervisor_name(N), Module, Argument).

supervisor_name(1) ->
  %% Name should be an atom
  Name = cheatsheet,
  %% The supervisor is registered locally as Name using register/2
  {local, Name};

supervisor_name(2) ->
  %% Name should be an atom
  Name = cheatsheet,
  %% The supervisor is registered globally as Name using global:register_name/2
  {global, Name};

supervisor_name(3) ->
  Name = "any",
  Module = register_server,

  %% The supervisor is registered as Name using the registry represented by Module.
  %% The Module callback must export the functions register_name/2, unregister_name/1, and send/2
  {via, Module, Name}.

%% Starts the child process defined in simple_one_for_one RestartStrategy
start_child(ListOfArguments) ->
  %% SupervisorName can be one of supervisor_name/1 above or Pid
  SupervisorName = {local, cheatsheet},

  %% The child process is started by appending ListOfArguments to the existing start function
  %% arguments by calling apply(Module, Function, InitialArguments ++ ListOfArguments)
  supervisor:start_child(SupervisorName, ListOfArguments).

%%%%%%%%%%%%
%% Server %%
%%%%%%%%%%%%
init(static) ->
  {ok, {supervisor_flags(static), child_specs(static)}};

init(dynamic) ->
  {ok, {supervisor_flags(dynamic), child_specs(dynamic)}}.

supervisor_flags(static) ->
  N = random:uniform(3),
  supervisor_flags(N);

supervisor_flags(1) ->
  %% If one child fails only that child will be restarted
  supervisor_flags(one_for_one);

supervisor_flags(2) ->
  %% If one child fails all child processes will be restarted
  supervisor_flags(one_for_all);

supervisor_flags(3) ->
  %% If one child fails all child processes that started after that child will be restarted
  supervisor_flags(rest_for_one);

supervisor_flags(dynamic) ->
  %% A simplified one_for_one supervisor, where all child processes are dynamically added
  %% instances of the same process type, that is, running the same code
  supervisor_flags(simple_one_for_one);

supervisor_flags(RestartStrategy) ->
  %% If more than MaxRestarts occurs within MaxTime seconds
  %% the supervisor terminates all child processes and then itself
  MaxRestarts = 10,
  MaxTime = 1,
  {RestartStrategy, MaxRestarts, MaxTime}.

child_specs(static) ->
  [
    child_spec(1),
    child_spec(2),
    child_spec(3)
  ];

child_specs(dynamic) ->
  N = random:uniform(3),
  %% If start child processes in a dynamic manner the list of child
  %% specifications should contains only one child specification
  [child_spec(N)].

child_spec(1) ->
  %% A permanent child process is always restarted
  Restart = permanent,

  %% Terminate child with exit(ChildPid, shutdown) and then wait infinity time
  %% for an exit signal with reason 'shutdown' back from the child process
  Shutdown = infinity,
  child_spec(Restart, Shutdown);

child_spec(2) ->
  %% A temporary child process is never restarted (even when the supervisor's
  %% restart strategy is rest_for_one or one_for_all and a sibling's death
  %% causes the temporary process to be terminated)
  Restart = temporary,

  %% Terminate child with exit(ChildPid, kill)
  Shutdown = brutal_kill,
  child_spec(Restart, Shutdown);

child_spec(3) ->
  %% A transient child process is restarted only if it terminates abnormally,
  %% that is, with another exit reason than normal, shutdown, or {shutdown,Term}
  Restart = transient,

  %% Terminate child with exit(ChildPid, shutdown) and then wait for an exit signal
  %% with reason 'shutdown' back from the child process.
  %% Terminate child with exit(ChildPid, kill) if wait more than 1000 ms
  Shutdown = 1000,
  child_spec(Restart, Shutdown).

child_spec(Restart, Shutdown) ->
  %% Used to identify the child specification internally by the supervisor
  ChildId = some_atom,

  ChildModule = some_child,
  StartLinkFuncInChildModule = start_link,
  Arguments = ["foo", "bar", 3],
  %% Supervisor will start this child calling apply(some_child, start_link, ["foo", "bar", 3]
  %% that transforms to some_child:start_link("foo", "bar", 3).
  %% StartLinkFuncInChildModule must create and link to the child process and must return
  %% {ok, ChildPid} | {ok, ChildPid, Info} | ignore | {error, Error}
  Start = {ChildModule, StartLinkFuncInChildModule, Arguments},

  %% Specifies if the child process is a 'supervisor' or a 'worker'.
  ChildType = worker,

  %% Is used by the release handler during code replacement to determine which processes
  %% are using a certain module. If the child process is a supervisor, gen_server or, gen_statem,
  %% this is to be a list with one element [ChildModule]. If the child process is gen_event
  %% value 'dynamic' must be used.
  Modules = [ChildModule],

  {ChildId, Start, Restart, Shutdown, ChildType, Modules}.
```  
