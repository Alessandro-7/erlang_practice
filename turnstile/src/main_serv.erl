-module(main_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, code_change/3, terminate/1]).
-export([start_link/0, requestVer/1]).

% call function for card validation
requestVer(CardId) ->
  gen_server:call(?MODULE, {verification, CardId}, 1000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, []}.

% here handle function makes request to the db for information about CardId
% now only CardId == 28 is valid one
handle_call({verification, CardId}, _From, State) ->
  case CardId == "28" of
    true ->
      Reply = authorized;
    _ ->
      Reply = not_authorized
  end,
  {reply, Reply, State};
handle_call(Other, From, State) ->
  logger:error("Unknown command to ~s from ~p: ~p~n", [?MODULE, From, Other]),
  {reply, ignored, State}.

terminate(normal) ->
  io:fwrite("its OK~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
