-module(otp_pool_test_worker).
-behaviour(gen_server).

-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Task, Delay, Max, SendTo) ->
  gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).


stop(Pid) ->
  gen_server:call(Pid, stop).


init(State = {_Task, Delay, _Max, _SendTo}) ->
  %% It will produce 'timeout' INFO message,
  %% because third element of the tuple (timeout) provided and
  %% a timeout occurs unless a request or a message is received within Delay milliseconds
  {ok, State, Delay}.


handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(timeout, {Task, Delay, Max, SendTo}) ->
  SendTo ! {self(), Task},
  if Max =:= infinity ->
      {noreply, {Task, Delay, Max, SendTo}, Delay};
     Max =< 1 ->
       {stop, normal, {Task, Delay, 0, SendTo}};
     Max > 1 ->
       {noreply, {Task, Delay, Max - 1, SendTo}, Delay}
  end.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.