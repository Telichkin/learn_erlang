-module(mafiapp_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).
-export([add_service_between_friends/1, find_friend_by_name/1, find_friend_with_services_by_name/1,
         find_friend_by_expertise/1, find_friends_number_of_debts/1]).


all() -> [add_service_between_friends, find_friend_by_name, find_friend_with_services_by_name,
          find_friend_by_expertise, find_friends_number_of_debts].

init_per_suite(Config) ->
  DatabaseDir = ?config(db_dir, Config),
  application:set_env(mnesia, dir, DatabaseDir),
  mafiapp:install([node()]),
  application:start(mnesia),
  application:start(mafiapp),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

add_service_between_friends(_Config) ->
  ok = mafiapp:add_friend("Don Corleone", [], [boss], boss),
  ok = mafiapp:add_friend("Roman Telichkin", [{email, "job@telichk.in"}], [python, javascript, elixir], programming),
  ok = mafiapp:add_service("Roman Telichkin", "Don Corleone", {31, 01, 2018}, "Wrote this program"),

  {error, unknown_friend} = mafiapp:add_service("Name1", "Name2", {01, 01, 1970}, "Description").

find_friend_by_name(_Config) ->
  Name = "DHH",
  ContactList = [],
  InfoList = [storytelling, ruby, basecamp],
  Expertise = entrepreneur,

  ok = mafiapp:add_friend(Name, ContactList, InfoList, Expertise),

  {Name, ContactList, InfoList, Expertise, []} = mafiapp:find_friend_by_name(Name),

  UnknownFriend = make_ref(),
  undefined = mafiapp:find_friend_by_name(UnknownFriend).


find_friend_with_services_by_name(_Config) ->
  ok = mafiapp:add_friend("Test User 1", [], [], first),
  ok = mafiapp:add_friend("Test User 2", [], [], second),

  ok = mafiapp:add_service("Test User 1", "Test User 2", {31, 01, 2018}, "1 -> 2"),
  ok = mafiapp:add_service("Test User 2", "Test User 1", {01, 02, 2018}, "2 -> 1"),

  {"Test User 1", [], [], first, [
    {to, "Test User 2", {31, 01, 2018}, "1 -> 2"},
    {from, "Test User 2", {01, 02, 2018}, "2 -> 1"}
  ]} = mafiapp:find_friend_by_name("Test User 1").


find_friend_by_expertise(_Config) ->
  ok = mafiapp:add_friend("Test User 3", [], [], test_expertise),
  ok = mafiapp:add_friend("Test User 4", [], [], test_expertise),

  [{"Test User 4", [], [], test_expertise, []},
   {"Test User 3", [], [], test_expertise, []}] = mafiapp:find_friend_by_expertise(test_expertise),

  [] = mafiapp:find_friend_by_expertise(unknown_expertise).

find_friends_number_of_debts(_Config) ->
  ok = mafiapp:add_friend("Test User 5", [], [], expertise_5),
  ok = mafiapp:add_friend("Test User 6", [], [], expertise_6),
  ok = mafiapp:add_friend("Test User 7", [], [], expertise_7),

  ok = mafiapp:add_service("Test User 5", "Test User 6", {01, 02, 2018}, "5 -> 6"),
  ok = mafiapp:add_service("Test User 6", "Test User 5", {02, 02, 2018}, "6 -> 5"),
  ok = mafiapp:add_service("Test User 5", "Test User 7", {31, 01, 2018}, "5 -> 7"),
  ok = mafiapp:add_service("Test User 6", "Test User 7", {31, 01, 2018}, "6 -> 7"),
  ok = mafiapp:add_service("Test User 7", "Test User 6", {31, 01, 2018}, "7 -> 6"),

  [{"Test User 6", 0}, {"Test User 7", 1}] = mafiapp:friend_debts("Test User 5"),

  UnknownFriend = make_ref(),
  [] = mafiapp:friend_debts(UnknownFriend).