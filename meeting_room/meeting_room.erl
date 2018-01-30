-module(meeting_room).

%% API
-export([start/0, stop/0, rent_room/1, rent_chairs/1, rent_projector/1, get_all_bookings/0]).

-record(booking, {
  room,
  chairs,
  projector
}).


start() ->
  Pid = spawn(fun() -> loop(#booking{}) end),
  register(?MODULE, Pid).

stop() ->
  ?MODULE ! stop.

rent_room(ForWhom) ->
  ?MODULE ! {rent_room, ForWhom}.

rent_chairs(ForWhom) ->
  ?MODULE ! {rent_chairs, ForWhom}.

rent_projector(ForWhom) ->
  ?MODULE ! {rent_projector, ForWhom}.

get_all_bookings() ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, all_bookings},
  receive
    {Ref, Reply} -> Reply
  end.


loop(State = #booking{}) ->
  receive
    stop -> ok;
    {rent_room, ForWhom} -> loop(State#booking{room = ForWhom});
    {rent_chairs, ForWhom} -> loop(State#booking{chairs = ForWhom});
    {rent_projector, ForWhom} -> loop(State#booking{projector = ForWhom});
    {From, Ref, all_bookings} ->
      From ! {Ref, [
        {room, State#booking.room},
        {chairs, State#booking.chairs},
        {projector, State#booking.projector}
      ]},
      loop(State)
  end.