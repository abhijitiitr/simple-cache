-module(sc_event).

-export([start_link/0,lookup/1, create/2, replace/2, delete/1,add_handler/2	]).

-define(SERVER, ?MODULE).
start_link() ->
   gen_event:start_link({local, ?SERVER}).