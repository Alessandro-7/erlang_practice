-module(turn_fsm).
-behaviour(gen_statem).

%% public API
-export([start_link/1, attach/0]).

%% callback
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([locked/3, opened/3]).

name() -> turn_statem.

start(Id) ->
  gen_statem:start(?MODULE, Id, []).

start_link(Id) ->
  gen_statem:start_link({local,name()}, ?MODULE, Id, []).

attach() ->
  gen_statem:cast(name(), attach).

terminate(_Reason, _State, _Data) ->
  void.
code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

init(Id) ->
  Data = #{id => Id, timer => 0},
  {ok, locked, Data}.

callback_mode() -> state_functions.

locked(cast, attach, Data) ->
  io:fwrite("opened for 3 seconds~n"),
  Tref = erlang:start_timer(3000, self(), lock),
  {next_state, opened, Data#{timer := Tref}};

locked(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


opened(info, {timeout, Tref, lock}, #{timer := Tref} = Data) ->
  io:fwrite("closed~n"),
  {next_state, locked, Data};
opened(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state, Data}.
