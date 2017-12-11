-module(kitty_gen_server).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/0, handle_info/2, terminate/2,
         order_cat/4, return_cat/2, close_shop/1]).
-record(cat, {name, color, description}).


start_link() ->
  gen_server:start_link(?MODULE, [], []).  %% The third parameter is debugging options.


order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).  %% The third parameter is timeout. Default is 5000 ms.


return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).


close_shop(Pid) ->
  gen_server:call(Pid, terminate).


%% Callbacks
init(Cats = []) -> {ok, Cats}.


%% Possible handle_call replies:
%%  {reply,Reply,NewState}
%%  {reply,Reply,NewState,Timeout}
%%  {reply,Reply,NewState,hibernate}
%%  {noreply,NewState}
%%  {noreply,NewState,Timeout}
%%  {noreply,NewState,hibernate}
%%  {stop,Reason,Reply,NewState}
%%  {stop,Reason,NewState}
handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
       {reply, #cat{name = Name, color = Color, description = Description}, Cats};
     Cats =/= [] ->
       {reply, hd(Cats), tl(Cats)}
  end;

handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.


%% Possible handle_cast replies:
%%  {noreply,NewState}
%%  {noreply,NewState,Timeout}
%%  {noreply,NewState,hibernate}
%%  {stop,Reason,NewState}
handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat | Cats]}.


handle_info(UnknownMessage, Cats) ->
  io:format("Unknown message: ~p~n", [UnknownMessage]),
  {noreply, Cats}.


terminate(normal, Cats) ->
  [io:format("~p was set free!~n", [Cat]) || Cat <- Cats],
  ok.