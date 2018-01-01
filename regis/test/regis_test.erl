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
      Pid = generate_pid(100),
      ?APP:register("first pid", Pid),
      ?_assertEqual(Pid, ?APP:whereis("first pid"))
    end
  }}.


register_not_pid_returns_error_test_() ->
  {"Application should return error when try to register not pid", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      [
        ?_assertEqual({error, not_pid}, ?APP:register("not a pid", some_atom)),
        ?_assertEqual(undefined, ?APP:whereis("not a pid"))
      ]
    end
  }}.


register_already_registered_name_test_() ->
  {"Application should return error if name already registered", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid1 = generate_pid(100),
      Pid2 = generate_pid(100),
      ?APP:register("test", Pid1),
      [
        ?_assertEqual({error, name_already_registered}, ?APP:register("test", Pid2)),
        ?_assertEqual(Pid1, ?APP:whereis("test"))
      ]
    end
  }}.


register_already_registered_pid_test_() ->
  {"Application should return error if pid already registered", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid = generate_pid(100),
      ?APP:register("test", Pid),
      [
        ?_assertEqual({error, pid_already_registered}, ?APP:register("test2", Pid)),
        ?_assertEqual(Pid, ?APP:whereis("test")),
        ?_assertEqual(undefined, ?APP:whereis("test2"))
      ]
    end
  }}.


register_many_different_names_test_() ->
  {"Application should register different names with different pids", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid1 = generate_pid(100),
      Pid2 = generate_pid(100),
      ?APP:register("pid1", Pid1),
      ?APP:register("pid2", Pid2),
      [
        ?_assertEqual(Pid1, ?APP:whereis("pid1")),
        ?_assertEqual(Pid2, ?APP:whereis("pid2"))
      ]
    end
  }}.


unregister_already_registered_name_test_() ->
  {"Application should unregister already registered name", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid = generate_pid(100),
      ?APP:register("pid name", Pid),
      [
        ?_assertEqual(ok, ?APP:unregister("pid name")),
        ?_assertEqual(undefined, ?APP:whereis("pid name")),
        ?_assertEqual(ok, ?APP:register("pid name", Pid)),
        ?_assertEqual(Pid, ?APP:whereis("pid name"))
      ]
    end
  }}.


unregister_not_registered_name_test_() ->
  {"Application should unregister not registered name without crash", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      ?_assertEqual(ok, ?APP:unregister("unknown"))
    end
  }}.


auto_unregister_on_die_test_() ->
  {"Application should unregister process when it dies", {
    setup, fun start_app/0, fun stop_app/1,
    fun (ok) ->
      Pid = generate_pid(1000),
      ?APP:register("pid", Pid),
      exit(Pid, kill),
      [
        ?_assertEqual(undefined, ?APP:whereis("pid")),
        ?_assertEqual(ok, ?APP:register("pid", generate_pid(100)))
      ]
    end
  }}.


start_app() ->
  application:start(?APP).


stop_app(ok) ->
  application:stop(?APP).


generate_pid(TimeToLive) ->
  spawn(fun () -> timer:sleep(TimeToLive), ok end).
