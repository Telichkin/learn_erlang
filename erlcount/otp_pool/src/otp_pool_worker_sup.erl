-module(otp_pool_worker_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(ModuleFuncArg = {_, _, _}) ->
  supervisor:start_link(?MODULE, ModuleFuncArg).


init({Module, Func, Arg}) ->
  MaxRestart = 5,
  MaxTime = 3600,
  RestartStrategy = {simple_one_for_one, MaxRestart, MaxTime},

  ChildSpec = {
    otp_pool_worker,
    {Module, Func, Arg},
    temporary, 5000, worker, [Module]
  },

  {ok, {RestartStrategy, [ChildSpec]}}.