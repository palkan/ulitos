[![Build Status](https://travis-ci.org/palkan/ulitos.svg?branch=master)](https://travis-ci.org/palkan/ulitos)
# Erlang common utils

Library contains several modules.

## ulitos

| Function                    | Description   |
|-----------------------------|---------------|
| `timestamp/0`               | Return current system time in milliseconds (**Note**: use `os:system_time(:secs)` in >= 18.0)| 
| `flush_box/0`               | Flush current process's mailbox |
| `random_string/1`           | Return _simply_ (not-safe, uniform) random string as list |
| `to_hex/1`                  | Convert string to hexadecimal representation |

## ulitos_file

| Function                    | Description   |
|-----------------------------|---------------|
| `recursively_list_dir/1, 2`    | Return list of all files and directories within directory (including subdirectories); optional boolean argument specifies wherether list only files (not directories) |
| `recursively_del_dir/1`    | Delete directory with all files and subdirectories |
| `dir_traversal/2,3` | Traverse through directory from top to bottom and execute function on each path. Fun must have be of a form `fun(Path::string(), Acc:list()) -> list()`. |

## ulitos_app

| Function                    | Description   |
|-----------------------------|---------------|
| `ensure_started/1`          | Tries to start all specified applications |
| `ensure_loaded/1`           | Tries to load all specified modules |
| `stop_apps/1`               | Stop  all specified applications |
| `reload/1`                  | Reload application and all its modules |
| `reload_modules/1`          | Reload specified modules |
| `get_var/2,3`               | _Safe_ `application:get_env/2` with default value |
| `load_config/1,2,3`         | Load enviromnent vars for application. Looks for config file in app's priv dir by default. |



