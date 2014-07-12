-module(simple_cache).
-export([
        insert/2,
        delete/1,
        lookup/1
       ]).


insert(Key, Value) ->
	case sc_store:lookup(Key) of
	   {ok, Pid} ->
	   	   sc_event:replace(Key),
	       sc_element:replace(Pid, Value);
	   {error, _Reason} ->
	       sc_event:create(Key),
	       {ok, Pid} = sc_element:create(Value),
	       sc_store:insert(Key, Pid)
end.

lookup(Key) ->
	try
	   {ok, Pid} = sc_store:lookup(Key),
	   {ok, Value} = sc_element:fetch(Pid),
	   {ok, Value}
	catch
	   _Class:_Exception ->
	       {error, not_found}
end.

delete(Key) ->
	case sc_store:lookup(Key) of
	   {ok, Pid} ->
	       sc_element:delete(Pid);
	   {error, _Reason} ->
	       ok
end.