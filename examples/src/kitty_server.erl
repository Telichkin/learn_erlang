-module(kitty_server).

%% API
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1,
         init/1, handle_call/3, handle_cast/2]).

-record(cat, {name,
              color=green,
              description}).


start_link() ->
  generic_server:start_link(?MODULE, []).


order_cat(Pid, Name, Color, Description) ->
  generic_server:call(Pid, {order, Name, Color, Description}).


return_cat(Pid, Cat = #cat{}) ->
  generic_server:cast(Pid, {return, Cat}).


close_shop(Pid) ->
  generic_server:call(Pid, terminate).


%% Server functions
init([]) -> [].


handle_call({order, Name, Color, Description}, From, Cats) ->
  if Cats =:= [] ->
      generic_server:reply(From, #cat{name = Name,
                                      color = Color,
                                      description = Description}),
      Cats;
    Cats =/= [] ->
      generic_server:reply(From, hd(Cats)),
      tl(Cats)
  end;

handle_call(terminate, From, Cats) ->
  generic_server:reply(From, ok),
  terminate(Cats).


handle_cast({return, Cat = #cat{}}, Cats) ->
  [Cat | Cats].


terminate(Cats) ->
  [io:format("~p was set free.~n", [Cat#cat.name]) || Cat <- Cats],
  exit(normal).
