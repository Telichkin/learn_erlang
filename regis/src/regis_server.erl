-module(regis_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register/2, whereis/1]).
-export([init/1, handle_call/3, handle_cast/2]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register(Name, Pid) when is_pid(Pid) ->
  gen_server:call(?MODULE, {register, Name, Pid}).


whereis(Name) ->
  gen_server:call(?MODULE, {whereis, Name}).


init([]) ->
  {ok, maps:new()}.


handle_call({register, Name, Pid}, _From, SavedPids) ->
  NewSavedPids = maps:put(Name, Pid, SavedPids),
  {reply, ok, NewSavedPids};

handle_call({whereis, Name}, _From, SavedPids) ->
  {ok, Pid} = maps:find(Name, SavedPids),
  {reply, Pid, SavedPids}.


handle_cast(_Request, _State) ->
  erlang:error(not_implemented).