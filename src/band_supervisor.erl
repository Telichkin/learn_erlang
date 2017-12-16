-module(band_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).


start_link(BandManagerMood) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, BandManagerMood).


%% The band supervisor will allow its band members to make a few
%% mistakes before shutting down all operations, based on what
%% mood he's in. A lenient supervisor will tolerate more mistakes
%% than an angry supervisor, who'll tolerate more than a
%% complete jerk supervisor
init(lenient) ->
  %% one_for_one basically means that if your supervisor supervises
  %% many workers and one of them fails, only that one should be restarted.
  init({one_for_one, 3, 60});

init(angry) ->
  %% rest_for_one basically, if a process dies, all the ones that were started
  %% after it (depend on it) get restarted, but not the other way around.
  init({rest_for_one, 2, 60});

init(jerk) ->
  %% one_for_all basically restarts all supervised processes if one process dies.
  init({one_for_all, 1, 60});


%% This kind of supervision is static. We specify all the children we'd have right in the source code
%% and let everything run after that. This is how most of your supervisors might end up being set in
%% real world applications; they're usually there for the supervision of architectural components.
init({RestartStrategy, MaxRestart, MaxTime}) ->
  ChildSpecs = [
    %% {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
    {
      singer,  %% <- The ChildId is just an internal name used by the supervisor internally.
               %%    You will rarely need to use it yourself, although it might be useful
               %%    for debugging purposes.
      {musicians, start_link, [singer, good]},  %% <- StartFunc is a tuple that tells how to start
                                                %%    the child. It's the standard {M,F,A}.

      permanent,  %% <- Restart tells the supervisor how to react when that particular child dies.
                  %%    A permanent process should always be restarted, no matter what.

      1000,  %% <- The Shutdown value of a child specification is thus used to give a deadline
             %%    on the termination. You might want to use a certain cutoff time, either in
             %%    milliseconds or 'infinity' if you are really patient. If the time passes and
             %%    nothing happens, the process is then brutally killed with exit(Pid, kill).
             %%    If you don't care about the child and it can pretty much die without any
             %%    consequences without any timeout needed, the atom brutal_kill is also an acceptable
             %%    value. 'brutal_kill' will make it so the child is killed with exit(Pid, kill),
             %%    which is untrappable and instantaneous.

      worker,  %% <- Type simply lets the supervisor know whether the child is a worker or a supervisor.

      [musicians]  %% <- Modules is a list of one element, the name of the callback module used by
                   %%    the child behavior. Atom 'dynamic' is also acceptable.
    }, {
      bass,
      {musicians, start_link, [bass, good]},
      temporary,  %% <- A temporary process is a process that should never be restarted.
      1000, worker, [musicians]
    }, {
      drum,
      {musicians, start_link, [drum, bad]},
      transient,  %% <- A transient run until they terminate normally and then they won't be restarted.
      1000, worker, [musicians]
    }, {
      keytar,
      {musicians, start_link, [keytar, good]},
      transient, 1000, worker, [musicians]
    }
  ],
  {ok, {{RestartStrategy, MaxRestart, MaxTime}, ChildSpecs}};


%% This is a supervisor that can create children in a dynamic manner.
init(dynamic) ->
  ChildSpec = {
    jam_musician,
    {musicians, start_link, []},  %% <- With simple_one_for_one strategy instead of doing
                                  %%    supervisor:start_child(Sup, Spec), which would call
                                  %%    erlang:apply(M,F,A), we now have
                                  %%    supervisor:start_child(Sup, Args),
                                  %%    which calls erlang:apply(M,F,A++Args).
    temporary, 1000, worker, [musicans]
  },

  %% You can't do supervisor:terminate_child(band_supervisor, Role).
  %% It'll return {error,simple_one_for_one}. You should stop musicians directly.
  %% Since version R14B03, it is possible to terminate children with the function
  %% supervisor:terminate_child(SupRef, Pid)
  {ok, {{simple_one_for_one, 3, 60}, [ChildSpec]}}.