-module(socket_server_sup).
-behavior(supervisor).

-export([start_link/0, start_listener/0]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
  {ok, Port} = application:get_env(port),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}, {packet, line}]),

  %% Put it inside a spawn_link/1 call for a simple reason: the supervisor process
  %% is in its init/1 phase and cannot answer any messages. If we were to call ourselves
  %% from within the init function, the process would deadlock and never finish running.
  %% An external process is needed just for this reason.
  spawn_link(fun () -> start_multiple_listeners(20) end),

  MaxRestarts = 60,
  MaxTime = 3600,
  RestartStrategy = {simple_one_for_one, MaxRestarts, MaxTime},
  ChildSpec = {
    socket_server,
    {socket_server_server, start_link, [ListenSocket]},
    temporary, 1000, worker, [socket_server_server]
  },

  {ok, {RestartStrategy, [ChildSpec]}}.


start_multiple_listeners(N) ->
  [start_listener() || _ <- lists:seq(1, N)],
  ok.


start_listener() ->
  supervisor:start_child(?MODULE, []).
