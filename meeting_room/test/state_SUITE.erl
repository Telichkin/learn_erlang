-module(state_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([ets_test/1]).


all() -> [ets_test].

init_per_testcase(_TestCaseName = ets_test, Config) ->
  TableId = ets:new(account, [ordered_set, public]),
  ets:insert(TableId, {a, 1}),
  ets:insert(TableId, {b, 2}),
  ets:insert(TableId, {c, 3}),
  [{table, TableId} | Config].

end_per_testcase(_TestCaseName = ets_test, Config) ->
  ets:delete(?config(table, Config)).

ets_test(Config) ->
  TableId = ?config(table, Config),
  [{b, 2}] = ets:lookup(TableId, b),
  c = ets:last(TableId),
  true = ets:insert(TableId, {d, 4}),
  [{d, 4}] = ets:lookup(TableId, d),
  d = ets:last(TableId).