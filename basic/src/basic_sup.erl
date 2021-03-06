%%%-------------------------------------------------------------------
%% @doc basic top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(basic_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link(_Module, _Args) ->
    start_link().

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args) ->
    error_logger:info_msg("[basic_sup] starting with pid ~p and args ~p~n", [self(), Args]),
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
