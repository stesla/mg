-module(mg).

-export([start/0, stop/0]).

start() ->
  application:start(mg).

stop() ->
  application:stop(mg).
