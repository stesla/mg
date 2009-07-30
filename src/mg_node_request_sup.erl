-module(mg_node_request_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRetries = 10,
  RetryInterval = 3600,
  Children = children(),
  {ok, {{RestartStrategy, MaxRetries, RetryInterval}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

children() ->
  Query = query_spec(),
  [Query].

query_spec() ->
  Name = mg_node_query_sup,
  StartFunc = {mg_node_query_sup, start_link, []},
  Restart = permanent,
  Shutdown = infinity,
  Modules = [mg_node_query_sup],
  Type = supervisor,
  {Name, StartFunc, Restart, Shutdown, Type, Modules}.
