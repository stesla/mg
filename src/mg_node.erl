-module(mg_node).

-export([start/0, stop/0]).

start() ->
  application:start(mg_node).

stop() ->
  application:stop(mg_node).
