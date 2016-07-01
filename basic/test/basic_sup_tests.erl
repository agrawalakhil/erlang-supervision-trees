-module(basic_sup_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% gen_supervisor tests
%%--------------------------------------------------------------------
basic_sup_test_() ->
    {foreach, fun setup/0, fun cleanup/1, 
     [
      fun(Pid) -> fun() -> server_is_alive(Pid) end end,
      fun(Pid) -> fun() -> server_is_registered(Pid) end end,
      fun(Pid) -> fun() -> worker_started(Pid) end end,
      fun(Pid) -> fun() -> worker_restarted(Pid) end end
     ]}.

setup() ->    
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = basic_sup:start_link(),
    Pid.

server_is_alive(Pid) ->
    ?assertEqual(true, is_process_alive(Pid)).

server_is_registered(Pid) ->
    ?assertEqual(Pid, whereis(basic_sup)).

worker_started(_Pid) ->    
    ?assertEqual(true, is_process_alive(whereis(basic_worker))).

worker_restarted(_Pid) ->
    exit(whereis(basic_worker), kill),
    timer:sleep(100),
    ?assertEqual(true, is_process_alive(whereis(basic_worker))).   

cleanup(Pid) ->    
    ?debugMsg("cleanup"),
    exit(Pid, kill),
    %% brutal kill!
    ?assertEqual(false, is_process_alive(Pid)).
