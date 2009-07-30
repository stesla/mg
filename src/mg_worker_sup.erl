-module(mg_worker_sup).

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
  supervisor:start_link(?MODULE, []).

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
  Worker = worker_spec(),
  [Worker].

worker_spec() ->
  Name = undefined,
  StartFunc = {mg_worker, start_link, []},
  Restart = permanent,
  Shutdown = brutal_kill,
  Modules = [mg_worker],
  Type = worker,
  {Name, StartFunc, Restart, Shutdown, Type, Modules}.
