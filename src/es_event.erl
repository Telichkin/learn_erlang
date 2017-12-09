-module(es_event).

%% API
-export([start/2, start_link/2, cancel/1, init/3]).
-record(state, {subscriber, name="", timeouts_list}).


start(EventName, DateTime) ->
  spawn(?MODULE, init, [self(), EventName, DateTime]).


start_link(EventName, DateTime) ->
  spawn_link(?MODULE, init, [self(), EventName, DateTime]).


init(Subscriber, EventName, DateTime) ->
  loop(#state{subscriber = Subscriber,
              name = EventName,
              timeouts_list = datetime_to_timeouts_list(DateTime)}).


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


datetime_to_timeouts_list(DateTime = {{_, _, _}, {_, _, _}}) ->
  Now = calendar:local_time(),
  Seconds = calendar:datetime_to_gregorian_seconds(DateTime) -
            calendar:datetime_to_gregorian_seconds(Now),
  Timeout = if Seconds > 0 -> Seconds;
               Seconds =< 0 -> 0
            end,
  normalize_timeouts(Timeout).


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
