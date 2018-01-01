%%%-------------------------------------------------------------------
%% @doc regis public API
%% @end
%%%-------------------------------------------------------------------

-module(regis).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([register/2, whereis/1, unregister/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  regis_sup:start_link().


stop(_State) ->
  ok.


register(Name, Pid) ->
  regis_server:register(Name, Pid).


whereis(Name) ->
  regis_server:whereis(Name).


unregister(Name) ->
  regis_server:unregister(Name).