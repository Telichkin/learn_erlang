-module(curling_accumulator).
-behaviour(gen_event).


%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {teams = orddict:new(),
                round = 0}).


init([]) ->
  {ok, #state{}}.


handle_event({set_teams, TeamA, TeamB}, State = #state{teams = CurrentTeams}) ->
  UpdatedTeams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, CurrentTeams)),
  {ok, State#state{teams = UpdatedTeams}};

handle_event({add_points, Team, Count}, State = #state{teams = CurrentTeams}) ->
  UpdatedTeams = orddict:update_counter(Team, Count, CurrentTeams),
  {ok, State#state{teams = UpdatedTeams}};

handle_event(next_round, State = #state{}) ->
  {ok, State#state{round = State#state.round + 1}};

handle_event(_Event, State) ->
  {ok, State}.


handle_call(game_data, State = #state{teams = CurrentTeams, round = Round}) ->
  {ok, {orddict:to_list(CurrentTeams), {round, Round}}, State};

handle_call(_Request, State) ->
  {ok, ok, State}.


handle_info(_Info, State) ->
  {ok, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.