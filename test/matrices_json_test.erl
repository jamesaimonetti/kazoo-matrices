-module(matrices_json_test).

-include_lib("eunit/include/eunit.hrl").

%% Examples taken from https://matrix.org/docs/spec/appendices#canonical-json
-define(ONE_TWO, <<"{
    \"one\": 1,
    \"two\": \"Two\"
}"/utf8>>).

-define(B_A_PRETTY, <<"{
    \"b\": \"2\",
    \"a\": \"1\"
}"/utf8>>).

-define(B_A_COMPACT, <<"{\"b\":\"2\",\"a\":\"1\"}"/utf8>>).

-define(AUTH_PRETTY, <<"{
    \"auth\": {
        \"success\": true,
        \"mxid\": \"@john.doe:example.com\",
        \"profile\": {
            \"display_name\": \"John Doe\",
            \"three_pids\": [
                {
                    \"medium\": \"email\",
                    \"address\": \"john.doe@example.org\"
                },
                {
                    \"medium\": \"msisdn\",
                    \"address\": \"123456789\"
                }
            ]
        }
    }
}"/utf8>>).

-define(A_GLYPH, <<"{
    \"a\": \"日本語\"
}"/utf8>>).

-define(GLYPH_1_2, <<"{
    \"本\": 2,
    \"日\": 1
}"/utf8>>).

-define(SLASH_U, <<"{
    \"a\": \"\u65E5\"
}"/utf8>>).

-define(A_NULL, <<"{
    \"a\": null
}
"/utf8>>).

canonical_test_() ->
    Tests = [{<<"{}"/utf8>>, <<"{}"/utf8>>}
            ,{?ONE_TWO, <<"{\"one\":1,\"two\":\"Two\"}"/utf8>>}
            ,{?B_A_PRETTY, <<"{\"a\":\"1\",\"b\":\"2\"}"/utf8>>}
            ,{?B_A_COMPACT, <<"{\"a\":\"1\",\"b\":\"2\"}"/utf8>>}
            ,{?AUTH_PRETTY, <<"{\"auth\":{\"mxid\":\"@john.doe:example.com\",\"profile\":{\"display_name\":\"John Doe\",\"three_pids\":[{\"address\":\"john.doe@example.org\",\"medium\":\"email\"},{\"address\":\"123456789\",\"medium\":\"msisdn\"}]},\"success\":true}}"/utf8>>}
            ,{?A_GLYPH, <<"{\"a\":\"日本語\"}"/utf8>>}
            ,{?GLYPH_1_2, <<"{\"日\":1,\"本\":2}"/utf8>>}
            ,{?SLASH_U, <<"{\"a\":\"日\"}"/utf8>>}
            ,{?A_NULL, <<"{\"a\":null}"/utf8>>}
            ],
    [canonicalize(Original, Canonical)
     || {Original, Canonical} <- Tests
    ].

canonicalize(Original, Canonical) ->
    TestName = lists:flatten(io_lib:format("~ts", [Canonical])),
    Decoded = matrices_json:decode(Original),
    RoundTrip = matrices_json:encode(Decoded),
    {TestName, ?_assertEqual(Canonical, RoundTrip)}.
