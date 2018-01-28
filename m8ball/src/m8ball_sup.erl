-module(m8ball_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  supervisor:start_link({global, ?SERVER}, ?MODULE, []).


init([]) ->
  SupervisorFlags = {one_for_one, 1, 10},
  ChildSpecs = [{
    m8ball_server,
    {m8ball_server, start_link, []},
    permanent,
    5000,
    worker,
    [m8ball_server]
  }],
  {ok, {SupervisorFlags, ChildSpecs}}.
