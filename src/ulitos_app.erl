%%% @doc Utils function for work with applications
%%% @end
-module(ulitos_app).
-author("palkan").

%% API
-export([ensure_started/1,ensure_loaded/1,stop_apps/1,reload/1,reload_modules/1]).


%% @doc
%% Tries to start applications or check whether they have started.
%% @end

-spec ensure_started(list(atom()) | atom()) -> ok
|error.

ensure_started([]) -> ok;

ensure_started(App) when is_atom(App) -> ensure_started([App]);

ensure_started([App | Apps]) ->
  case application:start(App) of
    ok -> ensure_started(Apps);
    {error, {already_started, App}} -> ensure_started(Apps);
    {error, {not_started, Dep}} -> ensure_started(Dep), ensure_started([App | Apps])
  end.


%% @doc
%% Tries to load modules.
%% @end

-spec ensure_loaded(list(atom()) | atom()) -> ok
|error.

ensure_loaded([]) -> ok;

ensure_loaded(Mod) when is_atom(Mod) -> ensure_started([Mod]);

ensure_loaded([Mod | Mods]) ->
  {module,_} = code:ensure_loaded(Mod),
  ensure_loaded(Mods).

%% @doc
%% Stop applications.
%% @end

-spec stop_apps(list(atom())|atom()) -> ok.

stop_apps([]) -> ok;
stop_apps(App) when is_atom(App) -> stop_apps([App]);
stop_apps([App | Apps]) ->
  application:stop(App),
  stop_apps(Apps).


%% @doc Compiles and reloads application modules
%% @end

-spec reload(atom()) -> any().

reload(App) ->
  application:load(App),
  case application:get_key(App,modules) of
    undefined ->
      ok;
    {ok,Modules} ->
      reload_modules(lists:usort(Modules))
  end.


-spec reload_mod(atom()) -> true.

reload_mod(Module) when is_atom(Module) ->
  code:soft_purge(Module),
  code:purge(Module),
  code:load_file(Module),
  true.

%% @doc
%% @end

-spec reload_modules(list(atom()) | atom()) -> ok.

reload_modules([]) -> ok;
reload_modules([?MODULE | T]) -> reload_modules(T);
reload_modules(M) when is_atom(M) -> reload_modules([M]);
reload_modules([H|T]) ->
  lager:debug("reload module: ~p",[H]),
  reload_mod(H),
  reload_modules(T).