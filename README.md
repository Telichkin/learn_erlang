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