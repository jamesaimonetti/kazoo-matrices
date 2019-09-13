-module(matricies_identifiers_test).

-include_lib("eunit/include/eunit.hrl").
-include("matrices.hrl").

matrix_ids_test_() ->
    [user_ids()
     %% ,room_ids()
     %% ,event_ids()
     %% ,group_ids()
     %% ,room_aliases()
    ].

user_ids() ->
    Tests = [%% Using Domains
             {<<?SIGIL_USER_ID, "user:host.com">>
             ,#{sigil => 'user'
               ,localpart => <<"user">>
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, "user:host.com:8888">>
             ,#{sigil => 'user'
               ,localpart => <<"user">>
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
             %% Using IPv4
            ,{<<?SIGIL_USER_ID, "user:1.2.3.4">>
             ,#{sigil => 'user'
               ,localpart => <<"user">>
               ,domain => {'ipv4', {1, 2, 3, 4}, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, "user:1.2.3.4:8888">>
             ,#{sigil => 'user'
               ,localpart => <<"user">>
               ,domain => {'ipv4', {1, 2, 3, 4}, 8888}
               }
             }
             %% Using IPv6
            ,{<<?SIGIL_USER_ID, "user:[1234:5678::abcd]">>
             ,#{sigil => 'user'
               ,localpart => <<"user">>
               ,domain => {'ipv6', {4660,22136,0,0,0,0,0,43981}, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, "user:[1234:5678::abcd]:8888">>
             ,#{sigil => 'user'
               ,localpart => <<"user">>
               ,domain => {'ipv6', {4660,22136,0,0,0,0,0,43981}, 8888}
               }
             }
             %% Parsing "historical" user IDs
            ,{<<?SIGIL_USER_ID, "user#!name:host.com">>
             ,#{sigil => 'user'
               ,localpart => <<"user#!name">>
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, "user#!name:host.com:8888">>
             ,#{sigil => 'user'
               ,localpart => <<"user#!name">>
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
            ],
    [encode_decode(Encoded, Decoded) || {Encoded, Decoded} <- Tests].

encode_decode(Encoded, Decoded) ->
    [?_assertEqual(Decoded, matrices_identifiers:decode(Encoded))
    ,?_assertEqual(Encoded, matrices_identifiers:encode(Decoded))
    ,?_assertEqual(Decoded, matrices_identifiers:decode(matrices_identifiers:encode(Decoded)))
    ,?_assertEqual(Encoded, matrices_identifiers:encode(matrices_identifiers:decode(Encoded)))
    ].
