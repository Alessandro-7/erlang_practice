-module(turn_fsm).
-behaviour(gen_statem).

%% public API
-export([start_link/1, attach/1]).

%% callback
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([locked/3, opened/3, validation/3]).

name() -> turn_statem.

start(Id) ->
  gen_statem:start(?MODULE, Id, []).

start_link(Id) ->
  gen_statem:start_link({local, name()}, ?MODULE, Id, []).

attach(CardId) ->
  gen_statem:cast(name(), {attach, CardId}).

terminate(_Reason, _State, _Data) ->
  void.
code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

init(Id) ->
  Data = #{id => Id, timer => 0, cardId => []},
  {ok, locked, Data}.

callback_mode() -> state_functions.


locked(cast, {attach, CardId}, Data) ->
  ControlSum = lists:foldl(fun(X, Sum) -> (X - $0) + Sum end, 0, CardId),
  case ControlSum of
    10 ->
      gen_statem:cast(name(), {valid_card, CardId}),
      {next_state, validation, Data};
    _ ->
      io:fwrite("not valid card~n"),
      {keep_state, Data}
  end;
locked(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

validation(cast, {valid_card, CardId}, Data) ->
  case main_serv:callver(CardId) of
    authorized ->
      io:fwrite("opened for 3 seconds~n"),
      Tref = erlang:start_timer(3000, self(), lock),
      {next_state, opened, Data#{timer := Tref}};
    not_authorized ->
      io:fwrite("you are not authorized~n"),
      {next_state, locked, Data};
    _ ->
      io:fwrite("something wrong~n"),
      {next_state, locked, Data}
    end;
validation(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

opened(info, {timeout, Tref, lock}, #{timer := Tref} = Data) ->
  io:fwrite("closed~n"),
  {next_state, locked, Data};
opened(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state, Data}.
