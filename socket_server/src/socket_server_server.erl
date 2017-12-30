-module(socket_server_server).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {player_name, next_step, current_socket}).

-define(DELAY, 800).


start_link(ListenSocket) ->
  gen_server:start_link(?MODULE, ListenSocket, []).


init(ListenSocket) ->
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  random:seed(A, B, C),
  gen_server:cast(self(), accept),
  {ok, #state{current_socket = ListenSocket}}.


handle_call(_Message, _From, State) ->
  {noreply, State}.


handle_cast(accept, State=#state{current_socket = ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  socket_server_sup:start_listener(),
  send(AcceptSocket, "What's your character name?", []),
  {noreply, State#state{current_socket = AcceptSocket, next_step = name}};

handle_cast({name_accepted, Name}, State=#state{current_socket = AcceptSocket}) ->
  send(AcceptSocket, "You successfully set name to '~s'~n"
                     "Send 'quit' to leave the game", [Name]),
  {noreply, State#state{next_step = game, player_name = Name}};

handle_cast({name_declined, Name}, State=#state{current_socket = AcceptSocket}) ->
  send(AcceptSocket, "You declined name '~p'. Pleas, enter new character name", [Name]),
  {noreply, State#state{next_step = name}}.


handle_info({tcp, _Socket, Message}, State=#state{next_step = name, current_socket = AcceptSocket}) ->
  Name = string_from_message(Message),
  send(AcceptSocket, "Your name is '~s'.~n"
                     "Do you agree with this? (y/n)", [Name]),
  {noreply, State#state{next_step = {accept_name, Name}}};

handle_info({tcp, AcceptSocket, Message},
            State=#state{next_step = {accept_name, Name}, current_socket = AcceptSocket}) ->
  case string_from_message(Message) of
    "y" ->
      gen_server:cast(self(), {name_accepted, Name});
    "n" ->
      gen_server:cast(self(), {name_declined, Name});
    _ ->
      send(AcceptSocket, "Please, answer with y (yes) or n (no)", [])
  end,
  {noreply, State};

handle_info({tcp, _ActiveSocket, "quit" ++ _}, State=#state{player_name = Name, current_socket = Socket}) ->
  send(Socket, "Player ~s leave the game", [Name]),
  gen_tcp:close(Socket),
  {stop, normal, State};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({tcp_error, _Socket, _}, State) ->
  {stop, normal, State};

handle_info(Error, State) ->
  io:format("Unexpected error ~p~n", [Error]),
  {noreply, State}.


terminate(normal, _State) ->
  ok;

terminate(Reason, _State) ->
  io:format("Terminated with reason ~p~n", [Reason]).


%%%%%%%%%%%
%% Utils %%
%%%%%%%%%%%
send(Socket, Text, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Text ++ "~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.


string_from_message(Message) when is_list(Message) ->
  hd(string:tokens(Message, "\r\n")).