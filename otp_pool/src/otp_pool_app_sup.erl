-module(otp_pool_app_sup).
-behaviour(supervisor).

-export([start_link/0, start_pool/3, stop_pool/1]).
-export([init/1]).


start_link() ->
  %% Here we gave the top level process pool supervisor the name otp_pool.
  %% This is because we know we will only have one otp_pool per Erlang
  %% node and we can give it a name without worrying about clashes.
  supervisor:start_link({local, otp_pool}, ?MODULE, []).


init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.


start_pool(Name, Limit, ModuleFuncArg) ->
  ChildSpec = {
    Name,
    {otp_pool_sup, start_link, [Name, Limit, ModuleFuncArg]},
    permanent, 10500, supervisor, [otp_pool_sup]
  },
  supervisor:start_child(otp_pool, ChildSpec).


stop_pool(Name) ->
  supervisor:terminate_child(otp_pool, Name),
  supervisor:delete_child(otp_pool, Name).