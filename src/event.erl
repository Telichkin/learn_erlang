-module(event).

%% API
-export([start/2, start_link/2, cancel/1, init/3]).
-record(state, {subscriber, name="", timeouts_list}).


start(EventName, Timeout) ->
  spawn(?MODULE, init, [self(), EventName, Timeout]).


start_link(EventName, Timeout) ->
  spawn_link(?MODULE, init, [self(), EventName, Timeout]).


init(Subscriber, EventName, Timeout) ->
  loop(#state{subscriber = Subscriber,
              name = EventName,
              timeouts_list = normalize_timeouts(Timeout)}).


loop(S = #state{subscriber = Subscriber, timeouts_list = [Timeout | Next]}) ->
  receive
    {Subscriber, Ref, cancel} ->
      Subscriber ! {Ref, ok}
    after Timeout * 1000 ->
      AllTimeoutsIsOver = Next =:= [],

      case AllTimeoutsIsOver of
        true ->
          Subscriber ! {done, S#state.name};
        false ->
          loop(S#state{timeouts_list = Next})
      end
  end.


%% Because Erlang's 'after' is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize_timeouts(FullTimeout) ->
  Limit = 49 * 24 * 60 * 60,
  [FullTimeout rem Limit | lists:duplicate(FullTimeout div Limit, Limit)].


cancel(Pid) ->
  %% If the process is already dead, it sends 'DOWN' message to me instantly.
  Ref = erlang:monitor(process, Pid),

  %% Send 'cancel' message to event server, specified in loop function.
  Pid ! {self(), Ref, cancel},

  receive
    %% After successful cancellation of the process I remove the reference
    %% to avoid receiving messages when I no longer care about them.
    {Ref, ok} ->
      %% flush option will purge the 'DOWN' message if it was sent before we had the time to demonitor.
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.
