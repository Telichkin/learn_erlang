-module(musicians).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DELAY, 750).
-record(musician, {name = "",
                   skill = good,
                   role}).


%%%%%%%%%%%%%%%%
%% Public API %%
%%%%%%%%%%%%%%%%
start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).


stop(Role) ->
  gen_server:call(Role, stop).
  %% Can be replaced by this one -> gen_server:stop(Role, normal, infinity).


%%%%%%%%%%%%%%%
%% Callbacks %%
%%%%%%%%%%%%%%%
init([Role, Skill]) ->
  %% To know when the parent shuts down
  process_flag(trap_exit, true),
  random:seed(now()),
  Name = generate_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
  TimeToPlay = random:uniform(3000),
  {ok, #musician{name = Name, role = StrRole, skill = Skill}, TimeToPlay}.


generate_name() ->
  FirstNames = ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
                "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"],
  LastNames = ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
               "Terese", "Tennelli", "Jamal", "Li", "Perlstein"],
  lists:nth(random:uniform(10), FirstNames) ++ " " ++ lists:nth(random:uniform(10), LastNames).


handle_call(stop, _From, Musician=#musician{}) ->
  {stop, normal, ok, Musician};

handle_call(_Message, _From, Musician) ->
  %% If we receive an unexpected message, we do not reply to it and the caller will crash.
  %% Not our problem.
  {noreply, Musician, ?DELAY}.  %% <- ?DELAY produce 'timeout' message after 750 ms.
  %% If an integer time-out value is provided, a time-out occurs unless a request
  %% or a message is received within Timeout milliseconds. A time-out is represented
  %% by the atom timeout, which is to be handled by the Module:handle_info/2 callback
  %% function. The atom infinity can be used to wait indefinitely, this is the default value.


handle_cast(_Message, Musician=#musician{}) ->
  {noreply, Musician, ?DELAY}.


handle_info(timeout, Musician=#musician{name = Name, skill = good}) ->
  io:format("~s produced sound!~n", [Name]),
  {noreply, Musician, ?DELAY};

handle_info(timeout, Musician=#musician{name = Name, skill = bad}) ->
  case random:uniform(5) of
    1 ->
      io:format("~s played a false note. Uh oh~n", [Name]),
      {stop, bad_note, Musician};
    _ ->
      io:format("~s produced sound!~n", [Name]),
      {noreply, Musician, ?DELAY}
  end;

handle_info(_Message, Musician) ->
  {noreply, Musician, ?DELAY}.


terminate(normal, #musician{name = Name, role = Role}) ->
  io:format("~s left the room (~s)~n", [Name, Role]);

terminate(bad_note, #musician{name = Name, role = Role}) ->
  io:format("~s sucks! Kicked that member out of the band! (~s)~n", [Name, Role]);

terminate(shutdown, #musician{name = Name}) ->
  io:format("The manager is mad and fired the whole band! "
            "~s just got back to playing in the subway~n", [Name]);

terminate(_Reason, #musician{name = Name, role = Role}) ->
  io:format("~s has been kicked out (~s)~n", [Name, Role]).


code_change(_OldVsn, Musician, _Extra) ->
  {ok, Musician}.