-module(curling_feed).
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).


init([SubscriberPid]) -> {ok, SubscriberPid}.


handle_event(Event, SubscriberPid) ->
  SubscriberPid ! Event,
  {ok, SubscriberPid}.


handle_call(_, SubscriberPid) ->
  {ok, SubscriberPid}.


handle_info(_, SubscriberPid) ->
  {ok, SubscriberPid}.


terminate(_Reason, _SubscriberPid) ->
  ok.


code_change(_OldVsn, SubscriberPid, _Extra) ->
  {ok, SubscriberPid}.