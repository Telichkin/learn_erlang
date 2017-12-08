-module(event).

%% API
-export([event/1]).
-record(state, {subscriber, name="", expires_after_sec}).


event(S = #state{subscriber = Subscriber}) ->
  receive
    {Subscriber, Ref, cancel} ->
      Subscriber ! {Ref, ok}
    after S#state.expires_after_sec * 1000 ->
      Subscriber ! {done, S#state.name}
  end.
