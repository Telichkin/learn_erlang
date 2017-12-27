-module(erlcount_counter).
-behaviour(gen_server).

%% API
-export([start_link/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dispatcher, ref, file, re}).


start_link(DispatcherPid, Ref, FileName, Regex) ->
  gen_server:start_link(?MODULE, [DispatcherPid, Ref, FileName, Regex], []).


init([DispatcherPid, Ref, FileName, Regex]) ->
  self() ! start,
  {ok, #state{dispatcher = DispatcherPid, ref = Ref, file = FileName, re = Regex}}.


handle_call(_Message, _From, State) ->
  {noreply, State}.


handle_cast(_Message, State) ->
  {noreply, State}.


handle_info(start, State=#state{re = Re, ref = Ref, file = FileName, dispatcher = DispatcherPid}) ->
  {ok, FileData} = file:read_file(FileName),
  PatternCount = erlcount_lib:get_pattern_count(Re, FileData),
  erlcount_dispatch:complete(DispatcherPid, Re, Ref, PatternCount),
  {stop, normal, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
