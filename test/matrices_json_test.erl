-module(matrices_json_test).

-include_lib("eunit/include/eunit.hrl").

%% Examples taken from https://matrix.org/docs/spec/appendices#canonical-json
-define(ONE_TWO, <<"{
    \"one\": 1,
    \"two\": \"Two\"
}">>).

-define(B_A_PRETTY, <<"{
    \"b\": \"2\",
    \"a\": \"1\"
}">>).

-define(B_A_COMPACT, <<"{\"b\":\"2\",\"a\":\"1\"}">>).

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
}">>).

-define(A_GLYPH, <<"{
    \"a\": \"日本語\"
}">>).

-define(GLYPH_1_2, <<"{
    \"本\": 2,
    \"日\": 1
}">>).

-define(SLASH_U, <<"{
    \"a\": \"\u65E5\"
}
">>).

-define(A_NULL, <<"{
    \"a\": null
}
">>).

canonical_test_() ->
    Tests = [{<<"{}">>, <<"{}">>}
            ,{?ONE_TWO, <<"{\"one\":1,\"two\":\"Two\"}">>}
            ,{?B_A_PRETTY, <<"{\"a\":\"1\",\"b\":\"2\"}">>}
            ,{?B_A_COMPACT, <<"{\"a\":\"1\",\"b\":\"2\"}">>}
            ,{?AUTH_PRETTY, <<"{\"auth\":{\"mxid\":\"@john.doe:example.com\",\"profile\":{\"display_name\":\"John Doe\",\"three_pids\":[{\"address\":\"john.doe@example.org\",\"medium\":\"email\"},{\"address\":\"123456789\",\"medium\":\"msisdn\"}]},\"success\":true}}">>}
            ,{?A_GLYPH, <<"{\"a\":\"日本語\"}">>}
            ,{?GLYPH_1_2, <<"{\"日\":1,\"本\":2}">>}
            ,{?SLASH_U, <<"{\"a\":\"日\"}">>}
            ,{?A_NULL, <<"{\"a\":null}">>}
            ],
    [?_assertMatch(Canonical, matrices_json:encode(matrices_json:decode(Original)))
     || {Original, Canonical} <- Tests
    ].
