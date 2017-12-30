-module(socket_server).
-behaviour(application).

-export([start/2, stop/1]).


start(normal, _StartArgs) ->
  socket_server_sup:start_link().


stop(_State) ->
  ok.