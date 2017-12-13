-module(trade_fsm).
-behaviour(gen_fsm).

%% trade_fsm API
-export([start/1, start_link/1, init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4, idle/2, idle/3, idle_wait/2,
         idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3]).

%% Public API
-export([trade/2, make_offer/2, retract_offer/2, accept_trade/1, ready/1, cancel/1]).

-record(state, {name="",
                other,   %% Other FSM Pid
                own_items=[],
                other_items=[],
                monitor,
                from}).  %% Client Pid


%% Public API
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).


start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).


trade(OwnPid, OtherPid) ->
gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).


accept_trade(OwnPid) ->
gen_fsm:sync_send_event(OwnPid, accept_negotiate).


make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).


retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).


ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).


cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%% FSM Callbacks
init(Name) ->
  {ok, idle, #state{name=Name}}.


handle_event(cancel, _StateName, State=#state{}) ->
  notice(State, "Received cancel event"),
  {stop, other_cancelled, State};

handle_event(Event, StateName, State) ->
  unexpected(Event, StateName),
  {next_state, StateName, State}.


handle_sync_event(cancel, _From, _StateName, State = #state{other = OtherPid}) ->
  notify_cancel(OtherPid),
  notice(State, "Cancelling trade, sending cancel event"),
  {stop, cancelled, ok, State};

%% Note: DO NOT reply to unexpected calls. Let the call-maker crash!
handle_sync_event(Event, _From, StateName, State) ->
  unexpected(Event, State),
  {next_state, StateName, State}.


handle_info({'DOWN', Ref, process, Pid, Reason}, _StateName, State=#state{other = Pid, monitor = Ref}) ->
  notice(State, "Other side dead"),
  {stop, {other_down, Reason}, State};

handle_info(Info, StateName, State) ->
  unexpected(Info, StateName),
  {next_state, StateName, State}.


terminate(normal, ready, State=#state{}) ->
  notice(State, "FSM leaving.");

terminate(_Reason, _StateName, _StateData) ->
  ok.


code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


%% Async callbacks (arity =:= 2)
idle({ask_negotiate, OtherPid}, State=#state{}) ->
  Ref = monitor(process, OtherPid),
  notice(State, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, State#state{other = OtherPid, monitor = Ref}};

idle(Event, State) ->
  %% We will report all unexpected events and
  %% ignore them by staying in the state we were already in (idle)
  unexpected(Event, idle),
  {next_state, idle, State}.


%% Sync callbacks (arity =:= 3)
%% Available answers:
%%  {reply, Reply, NextStateName, NewStateData}
%%  {reply, Reply, NextStateName, NewStateData, Timeout}
%%  {reply, Reply, NextStateName, NewStateData, hibernate}
%%
%%  {next_state, NextStateName, NewStateData}
%%  {next_state, NextStateName, NewStateData, Timeout}
%%  {next_state, NextStateName, NewStateData, hibernate}
%%
%%  {stop, Reason, Reply, NewStateData}
%%  {stop, Reason, NewStateData}
idle({negotiate, OtherPid}, From, State=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(State, "Asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, State#state{other = OtherPid, monitor = Ref, from = From}};

idle(Event, _From, State) ->
  unexpected(Event, idle),
  {next_state, idle, State}.


idle_wait({ask_negotiate, OtherPid}, State=#state{other = OtherPid}) ->
  gen_fsm:reply(State#state.from, ok),
  notice(State, "Starting negotiation"),
  {next_state, negotiate, State};

idle_wait({accept_negotiate, OtherPid}, State=#state{other = OtherPid}) ->
  gen_fsm:reply(State#state.from, ok),
  notice(State, "Starting negotiation"),
  {next_state, negotiate, State};

idle_wait(Event, State) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, State}.


idle_wait(accept_negotiate, _From, State=#state{other = OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(State, "Accepting negotiation"),
  {reply, ok, negotiate, State};

idle_wait(Event, _From, State) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, State}.


%% Callbacks for Public API
%% 'Own' side offer/retract item
negotiate({make_offer, Item}, State=#state{own_items = OwnItems}) ->
  do_offer(State#state.other, Item),
  notice(State, "Offering ~p", [Item]),
  {next_state, negotiate, State#state{own_items = add(Item, OwnItems)}};

negotiate({retract_offer, Item}, State=#state{own_items = OwnItems}) ->
  undo_offer(State#state.other, Item),
  notice(State, "Cancelling offer on ~p", [Item]),
  {next_state, negotiate, State#state{own_items = remove(Item, OwnItems)}};

%% Callbacks for FSM to FSM API
%% 'Other' side make/retract item
negotiate({do_offer, Item}, State=#state{other_items = OtherItems}) ->
  notice(State, "Other player offering ~p", [Item]),
  {next_state, negotiate, State#state{other_items = add(Item, OtherItems)}};

negotiate({undo_offer, Item}, State=#state{other_items = OtherItems}) ->
  notice(State, "Other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, State#state{other_items = remove(Item, OtherItems)}};

negotiate(are_you_ready, State=#state{other = OtherPid}) ->
  io:format("Other user ready to trande.~n"),
  notice(State,
         "Other user ready to transfer goods:~n"
         "You get ~p, The other side gets ~p",
         [State#state.other_items, State#state.own_items]),
  not_yet(OtherPid),
  {next_state, negotiate, State};

negotiate(Event, State) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, State}.


negotiate(ready, From, State = #state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  notice(State, "Asking if ready, waiting"),
  {next_state, wait, State#state{from = From}};

negotiate(Event, _From, State) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, State}.


wait({do_offer, Item}, State=#state{other_items = OtherItems}) ->
  gen_fsm:reply(State#state.from, offer_changed),
  notice(State, "Other side offering ~p", [Item]),
  {next_state, negotiate, State#state{other_items = add(Item, OtherItems)}};

wait({undo_offer, Item}, State=#state{other_items = OtherItems}) ->
  gen_fsm:reply(State#state.from, offer_chnaged),
  notice(State, "Other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, State#state{other_items = remove(Item, OtherItems)}};

wait(are_you_ready, State=#state{other = OtherPid}) ->
  am_ready(OtherPid),
  notice(State, "Asked if ready, and I am. Waiting for the same reply"),
  {next_state, wait, State};

wait(not_yet, State=#state{}) ->
  notice(State, "Other not ready yet"),
  {next_state, wait, State};

wait('ready!', State=#state{other = OtherPid, from = From}) ->
  am_ready(OtherPid),
  ack_trans(OtherPid),
  gen_fsm:reply(From, ok),
  notice(State, "Other side is ready. Moving to ready state"),
  {next_state, ready, State};

wait(Event, State) ->
  unexpected(Event, wait),
  {next_state, wait, State}.


ready(ack, State=#state{other = OtherPid}) ->
  %% Only one FSM can start the transaction.
  %% Otherwise handling of this state can end up deadlocked
  OwnPid = self(),
  case who_should_initiate_commit(OwnPid, OtherPid) of
    OwnPid ->
      try
        notice(State, "asking for commit"),
        ready_commit = ask_commit(OtherPid),
        notice(State, "commiting..."),
        ok = do_commit(OtherPid),
        commit(State),
        {stop, normal, State}
      catch Class:Reason ->
        notice(State, "commit failed"),
        {stop, {Class, Reason}, State}
      end;
    OtherPid ->
      {next_state, ready, State}
  end;

ready(Event, State) ->
  unexpected(Event, ready),
  {next_state, ready, State}.


ready(ask_commit, _From, State) ->
  notice(State, "replying to ask_commit"),
  {reply, ready_commit, ready, State};

ready(do_commit, _From, State) ->
  notice(State, "commiting..."),
  commit(State),
  {stop, normal, ok, State};

ready(Event, _From, State) ->
  unexpected(Event, ready),
  {next_state, ready, State}.


%% FSM to FSM API
%% All calls are asynchronous to avoid deadlocks
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).


accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).


do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).


undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).


are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).


not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).


am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').


%% Internal API used by both FSMs when doing the commit in the ready state.
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).


ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).


do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).


notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).


%% Utility functions
notice(State=#state{}, Message) ->
  notice(State, Message, []).


notice(#state{name=Name}, Message, Args) ->
  io:format("~s: " ++ Message ++ "~n", [Name | Args]).


unexpected(Message, State) ->
  io:format("~p received unknown event ~p while in the state ~p~n", [self(), Message, State]).


add(Item, Items) -> [Item | Items].


remove(Item, Items) -> Items -- [Item].


who_should_initiate_commit(OwnPid, OtherPid) when OwnPid > OtherPid -> OwnPid;

who_should_initiate_commit(OwnPid, OtherPid) when OwnPid < OtherPid -> OtherPid.


commit(State = #state{}) ->
  io:format("Transaction completed for ~s. "
            "Items sent are:~n~p,~n received are:~n~p.~n"
            "This operation should have some atomic save "
            "in a database.~n",
            [State#state.name, State#state.own_items, State#state.other_items]).
