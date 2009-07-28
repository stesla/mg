-module(mg_node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
  SupervisorFlags = supervisor_flags(),
  Children = children(),
  {ok, {SupervisorFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

supervisor_flags() ->
  RestartStrategy = one_for_all,
  MaxRetries = 0,
  RetryInterval = 1,
  {RestartStrategy, MaxRetries, RetryInterval}.

children() ->
  Storage = storage_spec(),
  [Storage].

%% child_spec(Args) ->
%%   Name = NAME,
%%   StartFunc = {MODULE, start_link, [Args]},
%%   Restart = permanent, 
%%   Shutdown = infinity,
%%   Modules = [MODULES],
%%   Type = supervisor,
%%   {Name, StartFunc, Restart, Shutdown, Type, Modules}.

storage_spec() ->
  Name = mg_node_storage,
  StartFunc = {mg_node_storage, start_link, []},
  Restart = permanent,
  Shutdown = brutal_kill,
  Modules = [mg_node_storage],
  Type = worker,
  {Name, StartFunc, Restart, Shutdown, Type, Modules}.
