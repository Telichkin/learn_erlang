-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test_1/1, test_2/1]).


all() ->
  [test_1, test_2].

test_1(_Config) ->
  1 = 1.

test_2(_Config) ->
  Zero = 0,
  1/Zero.

