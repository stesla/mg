-module(mg_sup).

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
  RestartStrategy = one_for_all,
  MaxRetries = 10,
  RetryInterval = 3600,
  Children = children(),
  {ok, {{RestartStrategy, MaxRetries, RetryInterval}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

children() ->
  Storage = storage_spec(),
  RequestSup = request_sup_spec(),
  [Storage, RequestSup].

storage_spec() ->
  Name = mg_storage,
  StartFunc = {mg_storage, start_link, []},
  Restart = permanent,
  Shutdown = brutal_kill,
  Modules = [mg_storage],
  Type = worker,
  {Name, StartFunc, Restart, Shutdown, Type, Modules}.

request_sup_spec() ->
  Name = mg_request_sup,
  StartFunc = {mg_request_sup, start_link, []},
  Restart = permanent,
  Shutdown = infinity,
  Modules = [mg_request_sup],
  Type = supervisor,
  {Name, StartFunc, Restart, Shutdown, Type, Modules}.  
