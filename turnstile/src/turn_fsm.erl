-module(turn_fsm).
-behaviour(gen_statem).

-export([start_link/1, attach/1]).

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
  Data = #{id => Id, timer => 0},
  {ok, locked, Data}.

callback_mode() -> state_functions.

% default locked state
locked(cast, {attach, CardId}, Data) ->
  ControlSum = lists:foldl(fun(X, Sum) -> (X - $0) + Sum end, 0, CardId),
  % checking sum of CardId numbers
  % if sum is 10 then fsm going to validation state
  case ControlSum of
    10 ->
      gen_statem:cast(name(), {valid_card, CardId}),
      {next_state, validation, Data};
    _ ->
      ok = turnstile_show("not valid card"),
      {keep_state, Data}
  end;
locked(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

% validation state sends request to the server
validation(cast, {valid_card, CardId}, Data) ->
  case main_serv:requestVer(CardId) of
    authorized ->
      ok = turnstile_open(),
      Tref = erlang:start_timer(3000, self(), lock),
      {next_state, opened, Data#{timer := Tref}};
    not_authorized ->
      ok = turnstile_show("you are not authorized"),
      {next_state, locked, Data};
    _ ->
      ok = turnstile_show("something wrong"),
      {next_state, locked, Data}
    end;
validation(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

% opened state with timeout
opened(info, {timeout, Tref, lock}, #{timer := Tref} = Data) ->
  ok = turnstile_show("closed"),
  {next_state, locked, Data};
opened(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

handle_event(_, _, Data) ->
    {keep_state, Data}.

turnstile_open() ->
  io:fwrite("opened for 3 seconds~n").
turnstile_show(Msg) ->
  io:fwrite("~s~n", [Msg]).
