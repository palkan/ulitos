%%% @doc
%%% Common utils functions.
%%% @end
-module(ulitos).
-author("palkan").

%% API
-export([timestamp/0, random_string/1, to_hex/1, flush_box/0]).

%% @doc Return current UTC time in ms (uses <code>os:timestamp/0</code>).
%% @end
-spec timestamp() -> number().
timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  ((Mega * 1000000 + Sec) * 1000000 + Micro) div 1000.

%% @doc Empty caller process message box
%% @end
-spec flush_box() -> ok.
flush_box() ->
  receive
    _M ->
      flush_box()
  after
    0 -> ok
  end.

%% @doc
%% Simple random string generator (using <code>random:uniform/1</code> and latin characters).
%% @end
-spec random_string(Length::non_neg_integer()) -> string().
random_string(0) -> [];

random_string(Length) -> [random_char() | random_string(Length - 1)].

%% @private
random_char() -> random:uniform(25) + 97.

-spec to_hex(Str::string()) -> Hex::string().
to_hex(Bin) when is_binary(Bin) ->
  list_to_binary(
    lists:foldl(fun(E, Acc) ->
      [hex(E bsr 4) | [hex(E band 16#F) | Acc]] end,
      [],
      lists:reverse(binary_to_list(Bin)))
    );

to_hex(Str) when is_list(Str) ->
  to_hex(list_to_binary(Str)).

%% @private
hex(V) ->
  if
    V < 10 ->
      $0 + V;
    true ->
      $a + (V - 10)
  end.

%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

binary_to_hex_test() ->
  ?assertEqual(<<"7465737420737472696e67">>, to_hex(<<"test string">>)),
  ?assertEqual(<<"7465737420737472696e67">>, to_hex("test string")).

-endif.
