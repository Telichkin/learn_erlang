-module(meeting_room_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([all_owners_should_be_same/1, owner_1/1, owner_2/1, owner_3/1]).


all() -> [{group, session}].

groups() -> [
  {
    session,
    [],
    [{group, owners}, all_owners_should_be_same]
  }, {
    owners,
    [parallel, {repeat, 10}],
    [owner_1, owner_2, owner_3]
  }
].

init_per_group(session, Config) ->
  meeting_room:start(),
  Config;
init_per_group(_, Config) ->
  Config.

end_per_group(session, _Config) ->
  meeting_room:stop();
end_per_group(_, _Config) ->
  ok.


all_owners_should_be_same(_Config) ->
  [{_, Owner}, {_, Owner}, {_, Owner}] = meeting_room:get_all_bookings().

owner_1(_Config) ->
  meeting_room:rent_chairs(owner_1),
  wait(),
  meeting_room:rent_room(owner_1),
  wait(),
  meeting_room:rent_projector(owner_1).

owner_2(_Config) ->
  meeting_room:rent_room(owner_2),
  wait(),
  meeting_room:rent_projector(owner_2),
  wait(),
  meeting_room:rent_chairs(owner_2).

owner_3(_Config) ->
  meeting_room:rent_projector(owner_3),
  wait(),
  meeting_room:rent_chairs(owner_3),
  wait(),
  meeting_room:rent_room(owner_3).

wait() -> timer:sleep(10).