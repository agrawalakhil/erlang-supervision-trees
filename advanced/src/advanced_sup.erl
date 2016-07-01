%%%-------------------------------------------------------------------
%% @doc advanced top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(advanced_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [{one_for_one, 1, 5}]).
start_link({RestartStrategy, Intensity, Period}) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [{RestartStrategy, Intensity, Period}]).   

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args = [{RestartStrategy, Intensity, Period}]) ->
    error_logger:info_msg("[advanced_sup] starting with pid ~p and args ~p~n", [self(), Args]),
    SupFlags = {RestartStrategy, Intensity, Period},    
    ChildSpecs = case RestartStrategy of
		     simple_one_for_one -> [%% dynamic_worker
					    {dynamic_worker_spec,
					     {dynamic_worker, start_link, []},
					     permanent,
					     brutal_kill,
					     worker,
					     [dynamic_worker]}
					   ];
		     _ ->
			 [%% advanced_worker		  
			  {advanced_worker_spec,
			   {advanced_worker, start_link, []},
			   permanent,
			   brutal_kill,
			   worker,
			   [advanced_worker]},
			  %% basic_sup
			  {basic_sup_spec,
			   {basic_sup, start_link, []},
			   permanent,
			   brutal_kill,
			   supervisor,
			   [basic_sup, basic_worker]}]
		 end,
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
