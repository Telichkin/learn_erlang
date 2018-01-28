-module(m8ball_server).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0, stop/0, ask/1]).

-define(SERVER, ?MODULE).


start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


stop() ->
  gen_server:call({global, ?SERVER}, stop).


ask(_Question) ->
  gen_server:call({global, ?SERVER}, ask).


init([]) ->
  rand:seed(exsp),
  {ok, []}.


handle_call(ask, _From, State) ->
  {ok, Answers} = application:get_env(m8ball, answers),
  Answer = element(rand:uniform(tuple_size(Answers)), Answers),
  {reply, Answer, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Message, _From, State) ->
  {noreply, State}.


handle_cast(_Message, State) ->
  {noreply, State}.


handle_info(_Message, State) ->
  {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


terminate(_Reason, _State) ->
  ok.