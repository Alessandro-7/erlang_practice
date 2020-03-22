-module(main_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, code_change/3, terminate/1]).
-export([start_link/0, callver/1]).

callver(CardId) ->
  gen_server:call({global, ?MODULE}, {verification, CardId}).

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, []}.

handle_call({verification, CardId}, _From, State) ->
  case CardId == "28" of
    true ->
      Reply = authorized;
    _ ->
      Reply = not_authorized
  end,
  {reply, Reply, State}.



terminate(normal) ->
  io:format("its OK~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
