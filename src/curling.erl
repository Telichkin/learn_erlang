-module(curling).

-export([start_link/2, set_teams/3, add_points/3, next_round/1, join_feed/2, leave_feed/2, game_info/1]).


%% This is an event manager, that passing different events to appropriate handlers
start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link(),
  gen_event:add_handler(Pid, curling_scoreboard, []),
  gen_event:add_handler(Pid, curling_accumulator, []),
  set_teams(Pid, TeamA, TeamB),
  {ok, Pid}.


set_teams(Pid, TeamA, TeamB) ->
  gen_event:notify(Pid, {set_teams, TeamA, TeamB}).


add_points(Pid, Team, N) ->
  gen_event:notify(Pid, {add_points, Team, N}).


next_round(Pid) ->
  gen_event:notify(Pid, next_round).


%% Proxy all events incoming to ?MODULE to the SubscriberPid using '!' operator
join_feed(Pid, SubscriberPid) ->
  %% Use tuple of handler name (atom) and Ref instead of name alone.
  %% It helps us personalize every subscriber and add an opportunity
  %% to delete handlers for particular subscriber
  HandlerId = {curling_feed, make_ref()},
  gen_event:add_handler(Pid, HandlerId, [SubscriberPid]),
  HandlerId.


leave_feed(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, leave_feed).


game_info(Pid) ->
  gen_event:call(Pid, curling_accumulator, game_data).