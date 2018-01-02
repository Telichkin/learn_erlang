-module(otp_pool_test).
-include_lib("eunit/include/eunit.hrl").


start_stop_test_() ->
  {"It should be possible to start and stop named workers pool",
    {foreach, fun set_up/0, fun tear_down/1, [
    fun (Name) ->
      ?_assertNotEqual(undefined, whereis(Name))
    end,

    fun (Name) ->
      otp_pool:stop_pool(Name),
      ?_assertEqual(undefined, whereis(Name))
    end
    ]}
  }.


run_worker_test_() ->
  {"Can start worker process with list of arguments",
    {setup, fun set_up/0, fun tear_down/1,
    fun (Name) ->
      {ok, Pid} = otp_pool:run_task_strict(Name, [i_am_running, 1, 1, self()]),
      Msg = receive
              {Pid, i_am_running} ->
                ok
              after 1000 ->
                timeout
            end,
      [?_assertEqual(ok, Msg),
       ?_assert(is_pid(Pid))]
    end}
  }.


run_too_many_workers_test_() ->
  {"Sync task should return noalloc when try to start in strict mode "
   "more workers than specified in otp_pool limit",
    {setup, fun set_up/0, fun tear_down/1,
    fun (Name) ->
      otp_pool:run_task_strict(Name, [i_am_running, 300, 1, self()]),
      otp_pool:run_task_strict(Name, [i_am_running, 300, 1, self()]),
      Msg = otp_pool:run_task_strict(Name, [i_am_running, 1, 1, self()]),

      ?_assertEqual(Msg, noalloc)
    end}
  }.


workers_should_dealloc_test_() ->
  {"Can add more workers after others die",
    {foreach, fun set_up/0, fun tear_down/1, [
     fun (Name) ->
       {ok, First} = otp_pool:run_task_strict(Name, [first_task, 200, 1, self()]),
       {ok, Second} = otp_pool:run_task_strict(Name, [second_task, 200, 1, self()]),
       timer:sleep(300),
       {ok, Third} = otp_pool:run_task_strict(Name, [third_task, 1, 1, self()]),
       timer:sleep(100),

       AllMessages = flush(),
       [?_assert(is_pid(Third)),
        ?_assertEqual([{First, first_task}, {Second, second_task}, {Third, third_task}],
                      AllMessages)]
     end,

     fun (Name) ->
       ok = otp_pool:run_task_async(Name, [first_task, 1000, 1, self()]),
       ok = otp_pool:run_task_async(Name, [second_task, 1000, 1, self()]),
       noalloc = otp_pool:run_task_strict(Name, [noalloc_task, 1, 1, self()]),
       ok = otp_pool:run_task_async(Name, [third_task, 1000, 1, self()]),
       timer:sleep(2100),

       AllMessages = flush(),
       ?_assertMatch([{_, first_task}, {_, second_task}, {_, third_task}], AllMessages)
     end,

     fun (Name) ->
       {ok, First} = otp_pool:run_task_sync(Name, [first_task, 200, 1, self()]),
       {ok, Second} = otp_pool:run_task_sync(Name, [second_task, 200, 1, self()]),
       {ok, Third} = otp_pool:run_task_sync(Name, [third_task, 100, 1, self()]),
       timer:sleep(400),

       AllMessages = flush(),
       [?_assert(is_pid(First)), ?_assert(is_pid(Second)), ?_assert(is_pid(Third)),
        ?_assertEqual([{First, first_task}, {Second, second_task}, {Third, third_task}],
                      AllMessages)
       ]
     end]}
  }.


otp_pool_never_restarts_children_test_() ->
  {"otp_pool will never restart a dead child. All pool's children will be "
   "shut down when closing the pool",
    {setup, fun set_up/0, fun tear_down/1,
     fun (Name) ->
       {ok, Pid} = otp_pool:run_task_strict(Name, [will_die, 1000, 1, self()]),
       otp_pool:stop_pool(Name),
       timer:sleep(100),
       ?_assertEqual(undefined, process_info(Pid))
     end}
   }.


dequeue_only_on_down_from_worker_test_() ->
  {"otp_pool should dequeue tasks after receiving a 'DOWN' signal from a worker",
    {setup, fun set_up/0, fun tear_down/1,
     fun (Name) ->
       {ok, _} = otp_pool:run_task_strict(Name, [first_task, 300, 1, self()]),
       ok = otp_pool:run_task_async(Name, [second_task, 5000, 1, self()]),
       ok = otp_pool:run_task_async(Name, [third_task, 5000, 1, self()]),
       ok = otp_pool:run_task_async(Name, [fourth_task, 1, 1, self()]),
       timer:sleep(400),

       Name ! {'DOWN', make_ref(), process, self(), normal},
       timer:sleep(100),

       AllMessages = flush(),
       ?_assertMatch([{_, first_task}], AllMessages)
     end
    }}.


set_up() ->
  start_app(),
  Name = generate_name(),
  start_named_pool(Name),
  Name.


tear_down(Name) ->
  stop_named_pool(Name).


start_app() ->
  application:start(otp_pool).


generate_name() ->
  list_to_atom(lists:flatten(io_lib:format("~p", [now()]))).


start_named_pool(Name) ->
  otp_pool:start_pool(Name, 2, {otp_pool_test_worker, start_link, []}).


stop_named_pool(Name) ->
  otp_pool:stop_pool(Name).


flush() ->
  receive
    Msg ->
      [Msg | flush()]
    after 0 ->
      []
  end.
