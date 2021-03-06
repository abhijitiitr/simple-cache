-module(sc_sup).
-behaviour(supervisor).
-export([
        start_link/0,
        start_child/2
       ]).
-export([init/1]).
-define(SERVER, ?MODULE).
start_link() -> 
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_child(Key, Value) -> 
    supervisor:start_child(?SERVER, [Key, Value]).
init([]) ->
   RestartStrategy = simple_one_for_one,
   MaxRestarts = 0,
   MaxSecondsBetweenRestarts = 1,
   SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
   Restart = temporary,
   Shutdown = brutal_kill,
   Type = worker,
   AChild = {sc_element, {sc_element, start_link, []},
             Restart, Shutdown, Type, [sc_element]},
   Event = {sc_event, {sc_event, start_link, []},
              Restart, Shutdown, Type, [sc_event]},
   {ok, {SupFlags, [AChild, Event]}}.