-module(sc_store).
-export([
  init/0,
  insert/2,
  delete/1,
  lookup/1
  ]).

-record(key_to_pid, {key, pid}).

-define(TABLE_ID, ?MODULE).

init() ->
  {ok, CacheNodes} = sc_resource_discovery:fetch_resources(simple_cache),
  dynamic_db_init(lists:delete(node(), CacheNodes)).
%%mnesia:create_schema([node()]),
%%    mnesia:start(),
%%    mnesia:create_table(key_to_pid, [{index, [pid]}, {attributes, record_info(fields, key_to_pid)}]).

insert(Key, Pid) when is_pid(Pid) ->
  mnesia:write(#key_to_pid{key = Key, pid = Pid}).

lookup(Key) ->
  Fun = fun() ->
      [{key_to_pid, Key, Pid}] = mnesia:read(key_to_pid, Key),
      Pid end,
  case mnesia:transaction(Fun) of
    {atomic, Pid}      -> {ok, Pid};
    {aborted, _Reason} -> {error, not_found}
  end.

delete(Pid) ->
  try
    [#key_to_pid{} = Record] = mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid), 
    mnesia:dirty_delete_object(Record)
  catch
    _C:_E -> ok
  end.

dynamic_db_init([]) -> 
  delete_schema(),
  mnesia:create_schema([node()]),
  mnesia:create_table(key_to_pid, [{index, [pid]},{attributes,record_info(fields, key_to_pid)}]);

dynamic_db_init(CacheNodes) -> 
  delete_schema(),
  add_extra_nodes(CacheNodes).
%% deletes a local schema.

delete_schema() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]),
  mnesia:start().

add_extra_nodes([Node|T]) ->
  case mnesia:change_config(extra_db_nodes, [Node]) of 
        {ok, [Node]} ->
      Res = mnesia:add_table_copy(schema, node(), ram_copies), 
      error_logger:info_msg("remote_init schema type ~p~n", [Res]),
      Res1 = mnesia:add_table_copy(key_to_pid, node(), ram_copies), 
      error_logger:info_msg("remote_init add_table copy = ~p~n",
                            [Res1]),
      Tables = mnesia:system_info(tables), 
      mnesia:wait_for_tables(Tables, 3000);
    _ ->
      add_extra_nodes(T)
  end.
