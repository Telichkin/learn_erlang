-module(mafiapp).
-behaviour(application).
-include_lib("stdlib/include/ms_transform.hrl").

-export([start/2, stop/1, install/1, add_friend/4, add_service/4, find_friend_by_name/1]).

-record(mafiapp_friends, {name,
                          contact = [],
                          info = [],
                          expertise}).

-record(mafiapp_services, {from,
                           to,
                           date,
                           description}).


start(_StartType = normal, _StartArg = []) ->
  mnesia:wait_for_tables([mafiapp_friends, mafiapp_services], 5000),
  mafiapp_sup:start_link().

stop(_State) ->
    ok.

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(mafiapp_friends, [
    {attributes, record_info(fields, mafiapp_friends)},
    {index, [#mafiapp_friends.expertise]},
    {disc_copies, Nodes}
  ]),
  mnesia:create_table(mafiapp_services, [
    {attributes, record_info(fields, mafiapp_services)},
    {index, [#mafiapp_services.to]},
    {disc_copies, Nodes},
    {type, bag}
  ]),
  application:stop(mnesia).

add_friend(Name, ContactList, InfoList, Expertise) ->
  AddFriendFun = fun() ->
    mnesia:write(#mafiapp_friends{name = Name,
                                  contact = ContactList,
                                  info = InfoList,
                                  expertise = Expertise})
  end,
  mnesia:activity(transaction, AddFriendFun).

add_service(FromName, ToName, Date, Description) ->
  AddServiceFun = fun() ->
    ThisNamesExist = mnesia:read({mafiapp_friends, FromName}) =/= [] orelse
                     mnesia:read({mafiapp_friends, ToName}) =/= [],

    case ThisNamesExist of
      true ->
        mnesia:write(#mafiapp_services{from = FromName,
                                       to = ToName,
                                       date = Date,
                                       description = Description});
      false ->
        {error, unknown_friend}
    end
  end,

  mnesia:activity(transaction, AddServiceFun).

find_friend_by_name(Name) ->
  FindFriendFun = fun() ->
    case mnesia:read({mafiapp_friends, Name}) of
      [#mafiapp_friends{contact = ContactList, info = InfoList, expertise = Expertise}] ->
        Services = find_services_for_name(Name),
        {Name, ContactList, InfoList, Expertise, Services};
      [] ->
        undefined
    end
  end,

  mnesia:activity(transaction, FindFriendFun).


find_services_for_name(Name) ->
  Services = ets:fun2ms(
    fun(#mafiapp_services{from = FromName, to = ToName,
                          date = Date, description = Description}) when FromName =:= Name ->
         {to, ToName, Date, Description};
       (#mafiapp_services{from = FromName, to = ToName,
                          date = Date, description = Description}) when ToName =:= Name ->
         {from, FromName, Date, Description}
    end
  ),
  mnesia:select(mafiapp_services, Services).
