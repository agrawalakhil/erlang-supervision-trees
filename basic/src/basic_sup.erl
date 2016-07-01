%%%-------------------------------------------------------------------
%% @doc basic top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(basic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
    SupFlags = {one_for_one, 1, 5},
    ChildSpecs = [{basic_worker_spec,
                   {basic_worker, start_link, []},
		   permanent,
		   brutal_kill,
		   worker,
		   [basic_worker]}],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
