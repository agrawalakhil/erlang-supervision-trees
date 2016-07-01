-module(advanced_sup_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% advanced_supervisor tests
%%--------------------------------------------------------------------
one_for_one_test_() ->    
    {foreachx, fun(Args) -> strategy_setup(Args) end, fun(Args, Pid) -> strategy_cleanup(Args, Pid) end, 
     [
      {{one_for_one, 1, 5}, named_advanced_worker_restarted("one_for_one_advanced_worker_restarted")},
      {{one_for_one, 1, 5}, named_basic_sup_restarted("one_for_one_basic_sup_restarted")}
     ]}.
one_for_all_test_() ->
    {foreachx, fun(Args) -> strategy_setup(Args) end, fun(Args, Pid) -> strategy_cleanup(Args, Pid) end,
     [
      {{one_for_all, 1, 5}, named_advanced_worker_restarted("one_for_all_advanced_worker_restarted")},
      {{one_for_all, 1, 5}, named_basic_sup_restarted("one_for_all_basic_sup_restarted")}
     ]}.
rest_for_one_test_() ->
    {foreachx, fun(Args) -> strategy_setup(Args) end, fun(Args, Pid) -> strategy_cleanup(Args, Pid) end,
     [
      {{rest_for_one, 1, 5}, named_advanced_worker_restarted("rest_for_one_advanced_worker_restarted")},
      {{rest_for_one, 1, 5}, named_basic_sup_restarted("rest_for_one_basic_sup_restarted")}
     ]}.

strategy_setup(Args) ->    
    ?debugFmt("setup for strategy ~p~n", [Args]),
    process_flag(trap_exit, true),
    {ok, Pid} = advanced_sup:start_link(Args),
    Pid.
named_advanced_worker_restarted(Name) ->
    fun(Args, Pid) -> (advanced_worker_restarted(Name, Args))(Pid) end.
advanced_worker_restarted(Name, Args) ->
    fun(_Pid) ->
	    {Name, fun() ->
			   ?debugFmt("advanced_worker_restarted for args ~p~n", [Args]),
			   exit(whereis(advanced_worker), kill),
			   timer:sleep(1000),
			   ?debugFmt("Supervisor children ~p~n", [supervisor:which_children(whereis(advanced_sup))]),
			   ?assertEqual(true, is_process_alive(whereis(advanced_worker)))
		   end
	    }
    end.   
named_basic_sup_restarted(Name) ->
    fun(Args, Pid) -> (basic_sup_restarted(Name, Args))(Pid) end.
basic_sup_restarted(Name, Args) ->
    fun(_Pid) ->
	    {Name, fun() ->
			   ?debugFmt("basic_sup_restarted for args ~p~n", [Args]),
			   exit(whereis(basic_sup), kill),
			   timer:sleep(1000),
			   ?debugFmt("Supervisor children ~p~n", [supervisor:which_children(whereis(advanced_sup))]),
			   ?assertEqual(true, is_process_alive(whereis(basic_sup)))
		   end
	    }
    end.   

strategy_cleanup(Args, Pid) ->    
    ?debugFmt("cleanup for strategy ~p~n", [Args]),
    exit(Pid, kill),
    %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
