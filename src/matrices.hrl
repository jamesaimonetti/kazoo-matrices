-ifndef(MATRICES_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(APP_NAME, <<"matrices">>).
-define(APP_VERSION, <<"0.0.1">>).

-define(CACHE_NAME, 'matrices_cache').

-define(SIGIL_USER_ID, "@").
-define(SIGIL_ROOM_ID, "!").
-define(SIGIL_EVENT_ID, "$").
-define(SIGIL_GROUP_ID, "+").
-define(SIGIL_ROOM_ALIAS, "#").

-define(DEFAULT_HOMESERVER_PORT, 8448).

-define(MATRICES_HRL, 'true').
-endif.
