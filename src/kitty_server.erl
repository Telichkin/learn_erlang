-module(kitty_server).

%% API
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name,
              color=green,
              description}).


start_link() ->
  spawn_link(fun init/0).


%% Sync call
order_cat(Pid, Name, Color, Description) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, {order, Name, Color, Description}},

  receive
    {Ref, Cat} ->
      erlang:demonitor(Ref, [flush]),
      Cat;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
    after 5000 ->
      erlang:error(timeout)
  end.


%% Async call
return_cat(Pid, Cat = #cat{}) ->
  Pid ! {return, Cat},
  ok.


%% Sync call
close_shop(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, terminate},

  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
    after 5000 ->
      erlang:error(timeout)
  end.


init() ->
  loop([]).


loop(Cats) ->
  receive
    {Pid, Ref, {order, Name, Color, Description}} ->
      NewCats = if Cats =:= [] ->
                    Pid ! {Ref, #cat{name = Name,
                                     color = Color,
                                     description = Description}},
                    Cats;
                  Cats =/= [] ->
                    Pid ! {Ref, hd(Cats)},
                    tl(Cats)
                end,
      loop(NewCats);

    {return, Cat = #cat{}} ->
      loop([Cat | Cats]);

    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok},
      terminate(Cats);

    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(Cats)
  end.


terminate(Cats) ->
  [io:format("~p was set free.~n", [Cat#cat.name]) || Cat <- Cats],
  ok.
