%%%-------------------------------------------------------------------
%% @doc turnstile top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(turnstile_sup).

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

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},

    ChildSpecs = [
                  #{id => turn_fsm,
                    start => {turn_fsm, start_link, ["22"]}},
                  #{id => main_serv,
                    start => {main_serv, start_link, []}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
