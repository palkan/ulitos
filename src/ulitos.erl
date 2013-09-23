%%% @doc
%%% Common utils functions.
%%% @end

-module(ulitos).
-author("palkan").

%% API
-export([timestamp/0,get_var/3,print_stacktrace/0, load_config/2, random_string/1, binary_to_hex/1]).

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


%% @doc
%% Set application <code>App</code> environment vars from file with name <code>File</code> located in app priv_dir.
%% @end


-spec load_config(atom(),list()) -> ok.

load_config(App,File) ->
  Path = code:priv_dir(App)++"/"++File,
  Env = load_file_config(Path),
  io:format("env from file:~p",[Env]),
  [application:set_env(App, Key, Value) || {Key, Value} <- Env],
  ok.


load_file_config(Path) ->
  case file:consult(Path) of
    {ok, Env} -> Env;
     _ -> []
  end.



%% @doc
%% Simple random string generator (using <code>random:uniform/1</code> and latin characters).
%% @end

-spec random_string(Length::non_neg_integer()) -> string().

random_string(0) -> [];

random_string(Length) -> [random_char() | random_string(Length-1)].

random_char() -> random:uniform(25) + 97.


%% @private

hex(V) ->
  if
    V < 10 ->
      $0 + V;
    true ->
      $a + (V - 10)
  end.

%% @private

binary_to_hex(Bin) ->
  lists:foldl(fun(E, Acc) ->
    [hex(E bsr 4) | [hex(E band 16#F) | Acc]] end,
    [],
    lists:reverse(binary_to_list(Bin))).


