%%%-------------------------------------------------------------------
%% @doc classic top level supervisor (without supervisor behavior).
%% @end
%%%-------------------------------------------------------------------

-module(classic_sup).

%% API
-export([start_link/0, supervisor/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->    
    application:start(sasl),
    error_logger:start(),
    SupPid = spawn(classic_sup, supervisor, [basic_worker, start_link, undefined]),
    register(classic_sup, SupPid),
    SupPid ! {'INIT'},
    {ok, SupPid}.

supervisor(Module, Function, Pid) ->    
    receive	
	{'INIT'} -> process_flag(trap_exit, true),
		    {ok, NewPid} = apply(Module, Function, []),
		    link(NewPid),
		    error_logger:info_msg("Initializing the worker with pid ~p~n", [NewPid]),
		    supervisor(Module, Function, NewPid);
	{'EXIT', Pid, Reason} ->
	    {ok, NewPid} = apply(Module, Function, []),
	    link(NewPid),
	    error_logger:info_msg("Process basic_worker pid ~p terminated due to ~p, new pid is ~p~n", [Pid, Reason, NewPid]),	    
	    supervisor(Module, Function, NewPid);
	{'EXIT', From, Reason} ->
	    error_logger:error_msg("Process with pid ~p terminated due to ~p~n", [From, Reason]),
	    exit(Pid, kill),
	    exit(kill);
	Unknown ->
	    error_logger:error_msg("Unknown message ~p to process ~p~n", [Unknown, self()]),
	    supervisor(Module, Function, Pid)
    end.
