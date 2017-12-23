-module(erlcount_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
  supervisor:start_link(?MODULE, []).


init([]) ->
  MaxRestarts = 5,
  MaxTime = 100,
  RestartStrategy = {one_for_one, MaxRestarts, MaxTime},
  ChildSpec = {
    dispatch,
    {erlcount_dispatch, start_link, []},
    trancient, 60000, worker, [erlcount_dispatch]
  },

  {ok, RestartStrategy, [ChildSpec]}.