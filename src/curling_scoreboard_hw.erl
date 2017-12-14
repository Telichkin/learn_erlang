-module(curling_scoreboard_hw).

-export([set_teams/2, next_round/0, add_point/1, reset_board/0]).


set_teams(TeamA, TeamB) ->
  io:format("Scoreboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).


next_round() ->
  io:format("Scoreboad: Round is over~n").


add_point(Team) ->
  io:format("Scoreboard: Increased score of team ~s by 1~n", [Team]).


reset_board() ->
  io:format("Scoreboard: All teams are undefined and all scores are 0~n").
