%%%-----------------------------------------------------------------------------
%%% @doc Create a Cononical JSON binary from a JSON object
%%%
%%% As specified by https://matrix.org/docs/spec/appendices#canonical-json
%%% @end
%%%-----------------------------------------------------------------------------
-module(matrices_json).

-export([decode/1
        ,encode/1
        ]).

-include("matrices.hrl").

-spec encode(kz_json:object()) -> iodata().
encode(JObj) ->
    kz_json:encode(canonicalize(JObj)).

-spec decode(iodata()) -> kz_json:object().
decode(JSON) ->
    case re:run(JSON, <<"(u[a-fA-F0-9]{4})|(U+[a-fA-F0-9]{4})">>, [{'capture', 'all_but_first', 'binary'}, 'global']) of
        {'match', Codepoints} -> decode(JSON, Codepoints);
        'nomatch' -> kz_json:decode(JSON)
    end.

decode(JSON, CodePoints) ->
    CleanedUp = lists:foldl(fun convert_point/2, JSON, CodePoints),
    kz_json:decode(CleanedUp).

convert_point(<<"\u", PointBin/binary>>=P, JSON) ->
    replace(JSON, P, PointBin);
convert_point(<<"U+", PointBin/binary>>=P, JSON) ->
    replace(JSON, P, PointBin);
convert_point([P], JSON) ->
    convert_point(P, JSON).

replace(JSON, P, PointBin) ->
    Point = binary_to_integer(PointBin, 16),
    Bin = unicode:characters_to_binary([Point]),
    binary:replace(JSON, P, Bin).

canonicalize(JObj) ->
    kz_json:from_map(kz_json:to_map(JObj)).
