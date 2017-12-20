-module(otp_pool_test).
-include_lib("eunit/include/eunit.hrl").

-export([]).


start_stop_test_() ->
  {
    "It should be possible to start and stop named workers pool",
    {
      foreach,
      fun set_up/0,
      fun tear_down/1,
      [
        fun (Name) ->
          ?_assertNotEqual(undefined, whereis(Name))
        end,

        fun (Name) ->
          otp_pool:stop_pool(Name),
          ?_assertEqual(undefined, whereis(Name))
        end
      ]
    }
  }.


run_worker_test_() ->
  {
    "Can start worker process using Module Function Arguments",
    {
      setup,
      fun set_up/0,
      fun tear_down/1,
      fun (Name) ->
        otp_pool:run_task_sync(Name, [i_am_running, 1, 1, self()]),
        Msg = receive
                {_Pid, i_am_running} ->
                  ok
                after 1000 ->
                  timeout
              end,
        ?_assertEqual(ok, Msg)
      end
    }
  }.


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
