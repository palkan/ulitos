%%% @doc
%%% Common utils functions.
%%% @end

-module(ulitos).
-author("palkan").

%% API
-export([timestamp/0,get_var/3,print_stacktrace/0]).

%% @doc Return current UTC time in ms (uses <code>os:timestamp/0</code>).
%% @end

-spec timestamp() -> number().
timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  ((Mega * 1000000 + Sec) * 1000000 + Micro) div 1000.

%% @doc
%% Return application environment variable called <code>Var</code> if exists; otherwise return <code>Def</code>.
%% @end

-spec get_var(atom(),atom(),any()) -> any().
get_var(App, Var, Def) ->
  case application:get_env(App, Var) of
    {ok, Val} -> Val;
    _ -> Def
  end.

%% @doc
%% Generate exception (using <code>smth:bark()</code>) and print stack trace using <code>lager</code> or <code>io</code>.
%% @end

print_stacktrace() ->
  {M,F} = case code:is_loaded(lager) of
            {ok,_} -> {lager,debug};
            _ -> {io,format}
              end,
  try
    smth:bark()
  catch
    _:_ -> M:F("Print stack:~n ~p~n",[erlang:get_stacktrace()])
  end.
