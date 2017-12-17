-module(otp_pool_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).


start_link(Name, Limit, ModuleFuncArg) ->
  supervisor:start_link(?MODULE, {Name, Limit, ModuleFuncArg}).


init({Name, Limit, ModuleFuncArg}) ->
  MaxRestart = 1,
  MaxTime = 3600,
  RestartStrategy = {one_for_all, MaxRestart, MaxTime},

  ChildSpec = {
    server,
    {otp_pool_server, start_link, [Name, Limit, self(), ModuleFuncArg]},
    permanent, 5000, worker, [otp_pool_server]
  },

  {ok, {RestartStrategy, [ChildSpec]}}.
