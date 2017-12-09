-module(event_server).

%% API
-export([loop/1, init/0, start/0, start_link/0, terminate/0, subscribe/1, add_event/3, cancel/1, wait_messages/1]).

-record(state, {events,
                clients}).

-record(event, {name="",
                description="",
                pid,
                date_time={{1970, 1, 1}, {0, 0, 0}}}).


start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.


start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.


terminate() ->
  ?MODULE ! shutdown.


subscribe(ClientPid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, ClientPid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
    after 5000 ->
    {error, timeout}
  end.


add_event(Name, Description, DateTime) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, DateTime}},
  receive
    {Ref, Message} ->
      Message
    after 5000 ->
    {error, timeout}
  end.


cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} ->
      ok
    after 5000 ->
      {error, timeout}
  end.


wait_messages(Seconds) ->
  receive
    Message = {done, _Name, _Description} ->
      [Message | wait_messages(0)]
    after Seconds * 1000 ->
      []
  end.


init() ->
  loop(#state{events = orddict:new(),
              clients = orddict:new()}).


loop(State = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, State#state.clients),
      Pid ! {MsgRef, ok},
      loop(State#state{clients = NewClients});

    {Pid, MsgRef, {add, Name, Description, DateTime}} ->
      case valid_datetime(DateTime) of
        true ->
          EventPid = event:start_link(Name, DateTime),
          NewEvents = orddict:store(Name, #event{name = Name,
                                                 description = Description,
                                                 pid = EventPid,
                                                 date_time = DateTime}, State#state.events),
          Pid ! {MsgRef, ok},
          loop(State#state{events = NewEvents});
        false ->
          Pid ! {MsgRef, {error, incorrect_datetime}},
          loop(State)
      end;

    {Pid, MsgRef, {cancel, Name}} ->
      NewEvents = case orddict:find(Name, State#state.events) of
        {ok, Event} ->
          event:cancel(Event#event.pid),
          orddict:erase(Name, State#state.events);
        error ->
          State#state.events
      end,
      Pid ! {MsgRef, ok},
      loop(State#state{events = NewEvents});

    {done, Name} ->
      NewEvents = case orddict:find(Name, State#state.events) of
        {ok, Event} ->
          notify_clients({done, Event#event.name, Event#event.description}, State#state.clients),
          orddict:erase(Name, State#state.events);
        error ->
          State#state.events
      end,
      loop(State#state{events = NewEvents});

    shutdown ->
      exit(shutdown);

    {'DOWN', Ref, process, _Pid, _Reason} ->
      NewClients = orddict:erase(Ref, State#state.clients),
      loop(State#state{clients = NewClients});

    code_change ->
      ?MODULE:loop(State);

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(State)
  end.


valid_datetime({Date, Time}) ->
  try
      calendar:valid_date(Date) andalso valid_time(Time)
  catch
      %% Not in {{Y, M, D}, {H, Min, S}} format.
      error:function_clause -> false
  end;
valid_datetime(_) -> false.


valid_time({H, M, S}) when H >= 0, H < 24,
                           M >= 0, M < 60,
                           S >= 0, S < 60 -> true;
valid_time(_) -> false.


notify_clients(Message, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Message end, Clients).