-module(resource_discovery).
-behaviour(gen_server).
-export([
        start_link/0
       ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {local_resources, target_resource_types, resources}). 
	
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	{ok, #state{resources = dict:new(),
	target_resource_types = [],
	local_resources = dict:new()}}.

add_local_resource(Type, Instance) ->
	gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

add_target_resource_type(Type) ->
	gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_resource(Type, Identifier, Dict) ->
	case dict:find(Type, Dict) of
		{ok, ResourceList} ->
			NewList = [Resource|lists:delete(Identifier, ResourceList)]
			dict:store(Type, NewList, Dict);
		error ->
			dict:store(Type, [Identifier], Dict)
end.

fetch_resources(Type) ->
	gen_server:call(?SERVER, {fetch_resources, Type}).

handle_call({fetch_resources, Type}, _From, State) ->
	{reply, dict:find(Type, State#state.resources), State}.

handle_cast({add_local_resource, {Type, Instance}}, State)  -> 
	#state{local_resources = LocalResources} = State, NewLocalResources = add_resource(Type, Instance, LocalResources), 
	{noreply, State#state{local_resources = NewLocalResources}};

handle_cast({add_target_resource_type, Type}, State) ->
	#state{target_resource_types = TargetTypes} = State,
	NewTargetTypes = [Type|lists:delete(Type, TargetTypes)],
	{noreply, State#state{target_resource_types = NewTargetTypes}};

handle_info(timeout, State) -> 
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
