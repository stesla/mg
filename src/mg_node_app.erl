-module(mg_node_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  mg_node_sup:start_link().

stop(_State) ->
  ok.
