-module(sc_event).

-export([start_link/0,lookup/1, create/2, replace/2, delete/1,add_handler/2	]).

-define(SERVER, ?MODULE).

start_link() ->
   gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
   gen_event:add_handler(?SERVER, Handler, Args).

create(Key, Value) ->
   gen_event:notify(?SERVER, {create, {Key, Value}}).
lookup(Key) ->
   gen_event:notify(?SERVER, {lookup, Key}).
delete(Key) ->
   gen_event:notify(?SERVER, {delete, Key}).

replace(Key, Value) ->
   gen_event:notify(?SERVER, {replace, {Key, Value}}).