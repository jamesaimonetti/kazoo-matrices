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
encode(JObj) -> kz_json:encode(JObj).

-spec decode(iodata()) -> kz_json:object().
decode(JSON) -> kz_json:decode(JSON).
