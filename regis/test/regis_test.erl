-module(regis_test).
-include_lib("eunit/include/eunit.hrl").

-define(APP, regis).


start_stop_test_() ->
  {"Application should start and stop", {
    foreach, fun start_app/0, fun stop_app/1,
    [
      fun (ok) ->
        ?_assertNotEqual(undefined, whereis(regis_sup))
      end,

      fun (ok) ->
        application:stop(?APP),
        ?_assertEqual(undefined, whereis(regis_sup))
      end
    ]
  }}.


register_process_test_() ->
  {"Application should register process pid and find it by name", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid = spawn(fun () -> ok end),
      ?APP:register("first pid", Pid),
      ?_assertEqual(Pid, ?APP:whereis("first pid"))
    end
  }}.


register_already_registered_name_test_() ->
  {"Application should return error if name already registered", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid1 = spawn(fun () -> ok end),
      Pid2 = spawn(fun () -> ok end),
      ?APP:register("test", Pid1),
      [
        ?_assertEqual({error, name_already_registered}, ?APP:register("test", Pid2)),
        ?_assertEqual(Pid1, ?APP:whereis("test"))
      ]
    end
  }}.


start_app() ->
  application:start(?APP).


stop_app(ok) ->
  application:stop(?APP).