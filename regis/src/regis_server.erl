-module(regis_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0, register/2, whereis/1, unregister/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register(Name, Pid) ->
  gen_server:call(?MODULE, {register, Name, Pid}).


whereis(Name) ->
  gen_server:call(?MODULE, {whereis, Name}).


unregister(Name) ->
  gen_server:call(?MODULE, {unregister, Name}).


init([]) ->
  ets:new(?MODULE, [set, named_table, protected]),
  {ok, ?MODULE}.


handle_call({register, Name, Pid}, _From, NamedPidsTable) when is_pid(Pid) ->
  Spec = ets:fun2ms(fun ({N, P, _}) when P == Pid; N == Name -> {N, P} end),

  case ets:select(NamedPidsTable, Spec) of
    [{_, Pid}] ->
      {reply, {error, pid_already_registered}, NamedPidsTable};
    [{Name, _}] ->
      {reply, {error, name_already_registered}, NamedPidsTable};
    [] ->
      Ref = erlang:monitor(process, Pid),
      ets:insert(NamedPidsTable, {Name, Pid, Ref}),
      {reply, ok, NamedPidsTable}
  end;

handle_call({register, _Name, NotAPid}, _From, NamedPidsTable) when not is_pid(NotAPid) ->
  {reply, {error, not_pid}, NamedPidsTable};

handle_call({whereis, Name}, _From, NamedPidsTable) ->
  case ets:lookup(NamedPidsTable, Name) of
    [] ->
      {reply, undefined, NamedPidsTable};
    [{Name, Pid, _Ref}] ->
      {reply, Pid, NamedPidsTable}
  end;

handle_call({unregister, Name}, _From, NamedPidsTable) ->
  ets:delete(NamedPidsTable, Name),
  {reply, ok, NamedPidsTable}.


handle_cast(_Request, NamedPidsTable) ->
  {noreply, NamedPidsTable}.


handle_info({'DOWN', Ref, process, _Pid, _Reason}, NamedPidsTable) ->
  ets:match_delete(NamedPidsTable, {'_', '_', Ref}),
  {noreply, NamedPidsTable};

handle_info(_Message, NamedPidsTable) ->
  {noreply, NamedPidsTable}.