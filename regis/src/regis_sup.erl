%%%-------------------------------------------------------------------
%% @doc regis top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(regis_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, regis_sup).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = {one_for_one, 60, 1000},
  ChildSpecs = [{
    regis_server,
    {regis_server, start_link, []},
    permanent, 1000, worker, [regis_server]
  }],
  {ok, {RestartStrategy, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
