-module(generic_server).

%% API
-export([call/2, loop/2, cast/2, reply/2, start_link/2, start/2]).


start(Module, InitialState) ->
  spawn(fun() -> init(Module, InitialState) end).


start_link(Module, InitialState) ->
  spawn_link(fun() -> init(Module, InitialState) end).


call(Pid, Message) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Message},

  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
    after 5000 ->
      erlang:error(timeout)
  end.


cast(Pid, Message) ->
  Pid ! {async, Message},
  ok.


reply(_From = {Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.


init(Module, InitialState) ->
  loop(Module, Module:init(InitialState)).


loop(Module, State) ->
  receive
    {sync, Pid, Ref, Message} ->
      loop(Module, Module:handle_call(Message, _From = {Pid, Ref}, State));
    {async, Message} ->
      loop(Module, Module:handle_cast(Message, State))
  end.
