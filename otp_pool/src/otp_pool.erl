-module(otp_pool).
-behaviour(application).

-export([start/2, stop/1, start_pool/3, stop_pool/1, run_task_strict/2,
         run_task_sync/2, run_task_async/2]).


start(normal, _Args) ->
  otp_pool_app_sup:start_link().


stop(_State) ->
  otp_pool_app_sup:stop().


start_pool(Name, Limit, {Module, Func, Args}) ->
  otp_pool_app_sup:start_pool(Name, Limit, {Module, Func, Args}).


stop_pool(Name) ->
  otp_pool_app_sup:stop_pool(Name).


run_task_strict(Name, Args) ->
  otp_pool_server:run_task_strict(Name, Args).


run_task_sync(Name, Args) ->
  otp_pool_server:run_task_sync(Name, Args).


run_task_async(Name, Args) ->
  otp_pool_server:run_task_async(Name, Args).
