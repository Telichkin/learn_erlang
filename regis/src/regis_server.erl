-module(regis_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register/2, whereis/1, unregister/1]).
-export([init/1, handle_call/3, handle_cast/2]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register(Name, Pid) ->
  gen_server:call(?MODULE, {register, Name, Pid}).


whereis(Name) ->
  gen_server:call(?MODULE, {whereis, Name}).


unregister(Name) ->
  gen_server:call(?MODULE, {unregister, Name}).


init([]) ->
  {ok, maps:new()}.


handle_call({register, Name, Pid}, _From, NameToPid) when is_pid(Pid) ->
  Pids = maps:values(NameToPid),
  case lists:member(Pid, Pids) of
    true ->
      {reply, {error, pid_already_registered}, NameToPid};
    false ->
      case maps:find(Name, NameToPid) of
        error ->
          {reply, ok, maps:put(Name, Pid, NameToPid)};
        {ok, _} ->
          {reply, {error, name_already_registered}, NameToPid}
      end
  end;

handle_call({register, _Name, NotAPid}, _From, NameToPid) when not is_pid(NotAPid) ->
  {reply, {error, not_pid}, NameToPid};

handle_call({whereis, Name}, _From, NameToPid) ->
  case maps:find(Name, NameToPid) of
    error ->
      {reply, undefined, NameToPid};
    {ok, Pid} ->
      {reply, Pid, NameToPid}
  end;

handle_call({unregister, Name}, _From, NameToPid) ->
  {reply, ok, maps:remove(Name, NameToPid)}.


handle_cast(_Request, _State) ->
  erlang:error(not_implemented).
