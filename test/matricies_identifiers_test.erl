-module(matricies_identifiers_test).

-include_lib("eunit/include/eunit.hrl").
-include("matrices.hrl").

matrix_ids_test_() ->
    [user_ids()
    ,room_ids()
    ,event_ids()
    ,group_ids()
    ,room_aliases()
    ,matrix_to_uris()
    ].

user_ids() ->
    UserId = kz_binary:rand_hex(16),
    Tests = [%% Using Domains
             {<<?SIGIL_USER_ID, UserId/binary, ":host.com">>
             ,#{sigil => 'user'
               ,localpart => UserId
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, UserId/binary, ":host.com:8888">>
             ,#{sigil => 'user'
               ,localpart => UserId
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
             %% Using IPv4
            ,{<<?SIGIL_USER_ID, UserId/binary, ":1.2.3.4">>
             ,#{sigil => 'user'
               ,localpart => UserId
               ,domain => {'ipv4', {1, 2, 3, 4}, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, UserId/binary, ":1.2.3.4:8888">>
             ,#{sigil => 'user'
               ,localpart => UserId
               ,domain => {'ipv4', {1, 2, 3, 4}, 8888}
               }
             }
             %% Using IPv6
            ,{<<?SIGIL_USER_ID, UserId/binary, ":[1234:5678::abcd]">>
             ,#{sigil => 'user'
               ,localpart => UserId
               ,domain => {'ipv6', {4660,22136,0,0,0,0,0,43981}, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, UserId/binary, ":[1234:5678::abcd]:8888">>
             ,#{sigil => 'user'
               ,localpart => UserId
               ,domain => {'ipv6', {4660,22136,0,0,0,0,0,43981}, 8888}
               }
             }
             %% Parsing "historical" user IDs
            ,{<<?SIGIL_USER_ID, UserId/binary, "#!name:host.com">>
             ,#{sigil => 'user'
               ,localpart => <<UserId/binary, "#!name">>
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_USER_ID, UserId/binary, "#!name:host.com:8888">>
             ,#{sigil => 'user'
               ,localpart => <<UserId/binary, "#!name">>
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
            ],
    {"user ID processing", build_tests(Tests)}.

room_ids() ->
    OpaqueId = kz_binary:rand_hex(16),
    Tests = [{<<?SIGIL_ROOM_ID, OpaqueId/binary, ":host.com">>
             ,#{sigil => 'room'
               ,localpart => OpaqueId
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_ROOM_ID, OpaqueId/binary, ":host.com:8888">>
             ,#{sigil => 'room'
               ,localpart => OpaqueId
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
            ],
    {"room ID processing", build_tests(Tests)}.

event_ids() ->
    OpaqueId = kz_binary:rand_hex(16),
    Tests = [{<<?SIGIL_EVENT_ID, OpaqueId/binary, ":host.com">>
             ,#{sigil => 'event'
               ,localpart => OpaqueId
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_EVENT_ID, OpaqueId/binary, ":host.com:8888">>
             ,#{sigil => 'event'
               ,localpart => OpaqueId
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
            ],
    {"event ID processing", build_tests(Tests)}.

group_ids() ->
    GroupId = kz_binary:rand_hex(16),
    Tests = [{<<?SIGIL_GROUP_ID, GroupId/binary, ":host.com">>
             ,#{sigil => 'group'
               ,localpart => GroupId
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_GROUP_ID, GroupId/binary, ":host.com:8888">>
             ,#{sigil => 'group'
               ,localpart => GroupId
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
            ],
    {"group ID processing", build_tests(Tests)}.

room_aliases() ->
    RoomAlias = kz_binary:rand_hex(16),
    Tests = [{<<?SIGIL_ROOM_ALIAS, RoomAlias/binary, ":host.com">>
             ,#{sigil => 'room_alias'
               ,localpart => RoomAlias
               ,domain => {'fqdn', <<"host.com">>, ?DEFAULT_HOMESERVER_PORT}
               }
             }
            ,{<<?SIGIL_ROOM_ALIAS, RoomAlias/binary, ":host.com:8888">>
             ,#{sigil => 'room_alias'
               ,localpart => RoomAlias
               ,domain => {'fqdn', <<"host.com">>, 8888}
               }
             }
            ],
    {"room aliases processing", build_tests(Tests)}.

matrix_to_uris() ->
    Tests = [{"room alias"
             ,<<?MATRIX_TO_PREFIX, "%23somewhere%3Aexample.org">>
             ,#{matrix_id => #{sigil => 'room_alias'
                              ,localpart => <<"somewhere">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => 'undefined'
               ,additional_arguments => []
               }
             }
            ,{"room"
             ,<<?MATRIX_TO_PREFIX, "!somewhere%3Aexample.org">>
             ,#{matrix_id => #{sigil => 'room'
                              ,localpart => <<"somewhere">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => 'undefined'
               ,additional_arguments => []
               }
             }
            ,{"permalink by room"
             ,<<?MATRIX_TO_PREFIX, "!somewhere%3Aexample.org/%24event%3Aexample.org">>
             ,#{matrix_id => #{sigil => 'room'
                              ,localpart => <<"somewhere">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => #{sigil => 'event'
                                    ,localpart => <<"event">>
                                    ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                                    }
               ,additional_arguments => []
               }
             }
            ,{"permalink by room alias"
             ,<<?MATRIX_TO_PREFIX, "%23somewhere:example.org/%24event%3Aexample.org">>
             ,#{matrix_id => #{sigil => 'room_alias'
                              ,localpart => <<"somewhere">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => #{sigil => 'event'
                                    ,localpart => <<"event">>
                                    ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                                    }
               ,additional_arguments => []
               }
             }
            ,{"user"
             ,<<?MATRIX_TO_PREFIX, "%40alice%3Aexample.org">>
             ,#{matrix_id => #{sigil => 'user'
                              ,localpart => <<"alice">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => 'undefined'
               ,additional_arguments => []
               }
             }
            ,{"group"
             ,<<?MATRIX_TO_PREFIX, "%2Bexample%3Aexample.org">>
             ,#{matrix_id => #{sigil => 'group'
                              ,localpart => <<"example">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => 'undefined'
               ,additional_arguments => []
               }
             }
            ,{"routable room"
             ,<<?MATRIX_TO_PREFIX, "!somewhere%3Aexample.org?via=example.org&via=alt.example.org">>
             ,#{matrix_id => #{sigil => 'room'
                              ,localpart => <<"somewhere">>
                              ,domain => {'fqdn', <<"example.org">>, ?DEFAULT_HOMESERVER_PORT}
                              }
               ,extra_parameter => 'undefined'
               ,additional_arguments => [{<<"via">>,<<"example.org">>}
                                        ,{<<"via">>,<<"alt.example.org">>}
                                        ]
               }
             }
            ],

    [{"Decoding matrix.to URI for " ++ Label, ?_assertEqual(Decoded, matrices_identifiers:decode(Encoded))}
     || {Label, Encoded, Decoded} <- Tests
    ].

build_tests(Tests) ->
    [encode_decode(Encoded, Decoded) || {Encoded, Decoded} <- Tests].

encode_decode(Encoded, Decoded) ->
    [{"decoding binary", ?_assertEqual(Decoded, matrices_identifiers:decode(Encoded))}
    ,{"encoding symbolic", ?_assertEqual(Encoded, matrices_identifiers:encode(Decoded))}
    ,{"round trip symbolic", ?_assertEqual(Decoded, matrices_identifiers:decode(matrices_identifiers:encode(Decoded)))}
    ,{"round trip binary", ?_assertEqual(Encoded, matrices_identifiers:encode(matrices_identifiers:decode(Encoded)))}
    ].
