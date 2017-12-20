-module(otp_pool_test).
-include_lib("eunit/include/eunit.hrl").

-export([]).


start_stop_test_() ->
  {
    "It should be possible to start and stop named workers pool",
    {
      foreach,

      fun () ->
        start_app(),
        Name = generate_name(),
        start_named_pool(Name),
        Name
      end,

      fun (Name) ->
        stop_named_pool(Name)
      end,

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





start_app() ->
  application:start(otp_pool).


generate_name() ->
  list_to_atom(lists:flatten(io_lib:format("~p", [now()]))).


start_named_pool(Name) ->
  otp_pool:start_pool(Name, 2, {otp_pool_test_worker, start_link, []}).


stop_named_pool(Name) ->
  otp_pool:stop_pool(Name).
