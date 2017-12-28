-module(socket_server_server).

-export([start_server/1]).


%% This is a naive implementation of a tcp-server. It
%% should be rewritten in order to works correctly with socket_server_sup.
start_server(Port) ->
  Pid = spawn_link(fun () ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    spawn(fun () -> accept_connection_loop(ListenSocket) end),
    timer:sleep(infinity)
  end),
  {ok, Pid}.


accept_connection_loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun () -> accept_connection_loop(ListenSocket) end),
  handle_message_loop(Socket).


handle_message_loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Message} ->
      gen_tcp:send(Socket, Message),
      handle_message_loop(Socket)
  end.