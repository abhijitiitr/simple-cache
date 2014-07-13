-module(sc_app).
-behaviour(application).
-export([start/2, stop/1,ensure_contact/0,ensure_contact/1]).
start(_StartType, _StartArgs) ->
  ensure_contact(),
  sc_resource_discovery:add_local_resource(simple_cache, node()),
  sc_resource_discovery:add_target_resource_type(simple_cache),
  sc_resource_discovery:trade_resources(),
  timer:sleep(4000),
  sc_store:init(),
  case sc_sup:start_link() of 
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.
stop(_State) ->
  ok.


ensure_contact() -> 
  case get_env(simple_cache, contact_nodes, []) of
    {ok, []} ->
      error_logger:info_msg("no contact nodes~n");
    {ok, ContactNodes} ->
      ok = ensure_contact(ContactNodes),
      {ok, WaitTime} = get_env(simple_cache, wait_time, 6000),
      wait_for_nodes(length(ContactNodes), WaitTime)
  end.

ensure_contact([Node|T]) ->
  case net_adm:ping(Node) of
    pong ->
      lists:foreach(fun(N) -> net_adm:ping(N) end, T);
    pang ->
      ensure_contact(T)
  end;

ensure_contact([]) ->
  {error, no_contact_nodes_reachable}.

wait_for_nodes(ContactNodes, WaitTime) ->
  wait_for_nodes(ContactNodes, round(WaitTime / 3), 3).

wait_for_nodes(_, _, 0) ->
  ok;

wait_for_nodes(ContactNodes, WaitSlice, Iterations) ->
  case length(nodes()) > length(ContactNodes) of
    true -> ok;
    false ->
      timer:sleep(WaitSlice),
      wait_for_nodes(ContactNodes, WaitSlice, Iterations - 1)
  end.

get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined -> {ok, Default};
    Found     -> Found
  end.
