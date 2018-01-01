# Learn Erlang

Learn Erlang with ["Learn you some Erlang from great good!"](http://learnyousomeerlang.com)


## Simple event server without OTP

- [Single Event](https://github.com/Telichkin/learn_erlang/blob/master/src/es_event.erl)
    - using spawn_link that explains in the chapter [Links](http://learnyousomeerlang.com/errors-and-processes#links)
    - work with records from the chapter [Records](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#records)
    - cancel event using erlang:monitor/2. Monitors explains in the chapter [Monitors](http://learnyousomeerlang.com/errors-and-processes#monitors)
    - work with calendar module
    - workaround for 50 days limit in 'receive after'
- [Event Server](https://github.com/Telichkin/learn_erlang/blob/master/src/es_event_server.erl)
    - work with records
    - work with orddict
    - simple hot code reloading. [Hot Code Loving](http://learnyousomeerlang.com/designing-a-concurrent-application#hot-code-loving)
    - save events using Reference from erlang:monitor/2
    - work with calendar module
- [Supervisor](https://github.com/Telichkin/learn_erlang/blob/master/src/es_supervisor.erl)
    - use process_flag(trap_exit, true) for receiving 'EXIT' message from the supervised process.
    Explains in the chapter [It's a trap!](http://learnyousomeerlang.com/errors-and-processes#its-a-trap)
    

## Dummy kitty server with gen_server

- [Handmade gen_server](https://github.com/Telichkin/learn_erlang/blob/master/src/generic_server.erl)
    - encapsulate generic server parts such as sync/async calls (call/cast), loop, init, reply, start and start_link

- [Use build-in gen_server](https://github.com/Telichkin/learn_erlang/blob/master/src/kitty_gen_server.erl)
    - implement callbacks for [behaviour](http://learnyousomeerlang.com/clients-and-servers#beam-me-up-scotty)
    - available handle_call answers [code](https://github.com/Telichkin/learn_erlang/blob/master/src/kitty_gen_server.erl#L30)
    - available handle_cast answers [code](https://github.com/Telichkin/learn_erlang/blob/master/src/kitty_gen_server.erl#L50)
    

## Trade system with gen_fsm

- [Trade Finite State Machine](https://github.com/Telichkin/learn_erlang/blob/master/src/trade_fsm.erl)
    - implement callbacks for [gen_fms behaviour](http://learnyousomeerlang.com/finite-state-machines#generic-finite-state-machines)
    - use `send_event`, `sync_send_event` and `sync_send_all_state_event` 
    - available sync callbacks answers [code](https://github.com/Telichkin/learn_erlang/blob/master/src/trade_fsm.erl#L111)
    - tested with [this code](http://learnyousomeerlang.com/static/erlang/trade_calls.erl)


## OTP CheatSheet

```erlang
-module(module_name).
-behaviour(supervisor).

-export([init/1]).


init(_) ->
  {ok, {supervisor_flags(1), child_specs()}}.

supervisor_flags(1) ->
  supervisor_flags(one_for_one);  %% <- if one child fails only that child will be restarted

supervisor_flags(2) ->
  supervisor_flags(one_for_all);  %% <- if one child fails all child processes will be restarted

supervisor_flags(3) ->
  supervisor_flags(rest_for_one); %% <- if one child fails all child processes that started after that
                                  %%    child will be restarted

supervisor_flags(RestartStrategy) ->
  MaxRestarts = 10,               %% <- if more than MaxRestarts occurs within MaxTime seconds
  MaxTime = 1,                    %%    the supervisor terminates all child processes and then itself
  {RestartStrategy, MaxRestarts, MaxTime}.


child_specs() -> 
  [].
```  