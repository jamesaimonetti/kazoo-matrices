-module(matrices_identifiers).

-export([decode/1
        ,decode_user_id/1
        ,decode_room_id/1
        ,decode_event_id/1
        ,decode_group_id/1
        ,decode_room_alias/1
        ,decode_matrix_to/1
        ]).

-export([encode/1]).

-export([sigil_to_char/1
        ,char_to_sigil/1
        ]).

-include("matrices.hrl").

-type sigil() :: 'user' |
                 'room' |
                 'event' |
                 'group' |
                 'room_alias'.

-type domain() :: {'ipv4', inet:ip4_address(), inet:port_number()} |
                  {'ipv6', inet:ip6_address(), inet:port_number()} |
                  {'fqdn', kz_term:ne_binary(), inet:port_number()}.

-type matrix_id() :: #{sigil => sigil()
                      ,localpart => kz_term:ne_binary()
                      ,domain => domain()
                      }.

-type matrix_to() :: #{matrix_id => matrix_id()
                      ,extra_parameter => kz_term:api_ne_binary()
                      ,additional_arguments => kz_term:proplist()
                      }.

-export_type([matrix_id/0
             ,matrix_to/0
             ,sigil/0
             ,domain/0
             ]).

-spec encode(matrix_id() | matrix_to()) -> kz_term:ne_binary().
encode(#{sigil := Sigil
        ,localpart := LocalPart
        ,domain := Domain
        }) ->
    iolist_to_binary([sigil_to_char(Sigil)
                     ,LocalPart
                     ,":"
                     ,encode_domain(Domain)
                     ]);
encode(#{matrix_id := ID
        ,extra_parameter := ExtraParameter
        ,additional_arguments := AdditionalArguments
        }) ->
    iolist_to_binary([?MATRIX_TO_PREFIX
                     ,kz_http_util:urlencode(encode(ID))
                     ,encode_extra_parameter(ExtraParameter)
                     ,encode_additional_argumetns(AdditionalArguments)
                     ]).

encode_extra_parameter('undefined') -> <<>>;
encode_extra_parameter(ExtraParameter) ->
    ["/", kz_http_util:urlencode(encode(ExtraParameter))].

encode_additional_argumetns([]) -> <<>>;
encode_additional_argumetns(AdditionalArguments) ->
    ["?", kz_http_util:props_to_querystring(AdditionalArguments)].

-spec decode(kz_term:ne_binary()) -> matrix_id() | matrix_to().
decode(<<?MATRIX_TO_PREFIX, _/binary>>=URI) ->
    decode_matrix_to(URI);
decode(<<SigilChar:1/binary, ID/binary>>) ->
    Sigil = char_to_sigil(SigilChar),
    decode_id(Sigil, ID).

-type split_uri() :: {binary(), binary(), binary(), binary(), binary()}.

-spec decode_matrix_to(kz_term:ne_binary() | split_uri()) -> matrix_to().
decode_matrix_to(<<?MATRIX_TO_PREFIX, Path/binary>>) ->
    decode_matrix_to(kz_http_util:urlsplit(Path));
decode_matrix_to({<<>>=_Scheme
                 ,<<>>=_Host
                 ,<<Path/binary>>
                 ,<<QueryString/binary>>
                 ,<<>>=_Fragment
                 }) ->
    case binary:split(Path, <<"/">>) of
        [ID] ->
            #{matrix_id => decode(kz_http_util:urldecode(ID))
             ,extra_parameter => 'undefined'
             ,additional_arguments => kz_http_util:parse_query_string(QueryString)
             };
        [ID, Extra] ->
            #{matrix_id => decode(kz_http_util:urldecode(ID))
             ,extra_parameter => decode(kz_http_util:urldecode(Extra))
             ,additional_arguments => kz_http_util:parse_query_string(QueryString)
             }
    end.

-spec decode_id(sigil(), kz_term:ne_binary()) -> matrix_id().
decode_id('user', ID) ->       decode_user_id(ID);
decode_id('room', ID) ->       decode_room_id(ID);
decode_id('event', ID) ->      decode_event_id(ID);
decode_id('group', ID) ->      decode_group_id(ID);
decode_id('room_alias', ID) -> decode_room_alias(ID).

%% @doc https://matrix.org/docs/spec/appendices#user-matrix_ids
-spec decode_user_id(kz_term:ne_binary()) -> matrix_id().
decode_user_id(<<?SIGIL_USER_ID, ID/binary>>) -> decode_user_id(ID);
decode_user_id(ID) when byte_size(ID) < 255 ->
    %% length of ID, including sigil and domain, must not exceed 255 characters
    [LocalPart, Domain] = binary:split(ID, <<":">>),
    'true' = is_valid_user_local_part(LocalPart),
    DecodedDomain = decode_domain(Domain),
    #{sigil => 'user'
     ,localpart => LocalPart
     ,domain => DecodedDomain
     }.

-spec decode_room_id(kz_term:ne_binary()) -> matrix_id().
decode_room_id(<<?SIGIL_ROOM_ID, ID/binary>>) -> decode_room_id(ID);
decode_room_id(ID) -> decode_room_or_event_id(ID, 'room').

-spec decode_event_id(kz_term:ne_binary()) -> matrix_id().
decode_event_id(<<?SIGIL_EVENT_ID, ID/binary>>) -> decode_event_id(ID);
decode_event_id(ID) -> decode_room_or_event_id(ID, 'event').

-spec decode_room_or_event_id(kz_term:ne_binary(), sigil()) -> matrix_id().
decode_room_or_event_id(ID, Sigil) ->
    [OpaqueId, Domain] = binary:split(ID, <<":">>),
    DecodedDomain = decode_domain(Domain),
    #{sigil => Sigil
     ,localpart => OpaqueId
     ,domain => DecodedDomain
     }.

-spec decode_group_id(kz_term:ne_binary()) -> matrix_id().
decode_group_id(<<?SIGIL_GROUP_ID, ID/binary>>) -> decode_group_id(ID);
decode_group_id(ID) ->
    [LocalPart, Domain] = binary:split(ID, <<":">>),
    'true' = is_valid_local_part(LocalPart),
    DecodedDomain = decode_domain(Domain),
    #{sigil => 'group'
     ,localpart => LocalPart
     ,domain => DecodedDomain
     }.

-spec decode_room_alias(kz_term:ne_binary()) -> matrix_id().
decode_room_alias(<<?SIGIL_ROOM_ALIAS, ID/binary>>) -> decode_room_alias(ID);
decode_room_alias(ID) ->
    [LocalPart, Domain] = binary:split(ID, <<":">>),
    DecodedDomain = decode_domain(Domain),
    #{sigil => 'room_alias'
     ,localpart => LocalPart
     ,domain => DecodedDomain
     }.

is_valid_user_local_part(<<LocalPart/binary>>) ->
    lists:all(fun is_valid_user_local_part_char/1, binary_to_list(LocalPart)).

is_valid_user_local_part_char(Char) ->
    is_valid_user_local_part(Char, is_valid_local_part_character(Char)).

is_valid_user_local_part(_Char, 'true') -> 'true';
%% https://matrix.org/docs/spec/appendices#historical-user-ids
%% older user IDs may contain these chars too
%% decode them as valid (but let's not create new ones)
is_valid_user_local_part(ExtendedChar, 'false') when ExtendedChar >= 16#21, ExtendedChar =< 16#39 -> 'true';
is_valid_user_local_part(ExtendedChar, 'false') when ExtendedChar >= 16#3B, ExtendedChar =< 16#7E -> 'true';
is_valid_user_local_part(_Char, 'false') -> 'false'.

-spec is_valid_local_part(kz_term:ne_binary()) -> 'true'.
is_valid_local_part(LocalPart) when byte_size(LocalPart) > 0 ->
    lists:all(fun is_valid_local_part_character/1, binary_to_list(LocalPart)).

is_valid_local_part_character(AtoZ) when AtoZ >= $a, AtoZ =< $z -> 'true';
is_valid_local_part_character(ZeroToNine) when ZeroToNine >= $0, ZeroToNine =< $9 -> 'true';
is_valid_local_part_character($.) -> 'true';
is_valid_local_part_character($_) -> 'true';
is_valid_local_part_character($=) -> 'true';
is_valid_local_part_character($-) -> 'true';
is_valid_local_part_character($/) -> 'true';
is_valid_local_part_character(_Char) -> 'false'.

-spec decode_domain(kz_term:ne_binary()) -> domain().
decode_domain(<<"[", IPv6Bin/binary>>) ->
    case binary:split(IPv6Bin, <<"]">>) of
        [Host, <<>>] ->
            {Type, Term} = decode_domain(Host, inet:parse_strict_address(kz_term:to_list(Host))),
            {Type, Term, ?DEFAULT_HOMESERVER_PORT};
        [Host, <<":", PortBin/binary>>] ->
            {Type, Term} = decode_domain(Host, inet:parse_strict_address(kz_term:to_list(Host))),
            {Type, Term, binary_to_integer(PortBin)}
    end;
decode_domain(Domain) ->
    case binary:split(Domain, <<":">>) of
        [Host] ->
            {Type, Term} = decode_domain(Host, inet:parse_strict_address(kz_term:to_list(Host))),
            {Type, Term, ?DEFAULT_HOMESERVER_PORT};
        [Host, Port] ->
            {Type, Term} = decode_domain(Host, inet:parse_strict_address(kz_term:to_list(Host))),
            {Type, Term, binary_to_integer(Port)}
    end.

decode_domain(_Domain, {'ok', {_A, _B, _C, _D}=IPv4}) ->
    {'ipv4', IPv4};
decode_domain(_Domain, {'ok', {_A, _B, _C, _D, _E, _F, _G, _H}=IPv6}) ->
    {'ipv6', IPv6};
decode_domain(Domain, {'error', 'einval'}) ->
    %% punting unless someone wants to dig into RFC5890: https://tools.ietf.org/html/rfc5890
    {'fqdn', Domain}.

-spec encode_domain(domain()) -> kz_term:ne_binary().
encode_domain({'ipv4', IPv4, Port}) ->
    kz_term:to_binary([inet:ntoa(IPv4), encode_port(Port)]);
encode_domain({'ipv6', IPv6, Port}) ->
    kz_term:to_binary(["[",inet:ntoa(IPv6),"]", encode_port(Port)]);
encode_domain({'fqdn', <<Domain/binary>>, Port}) ->
    kz_term:to_binary([Domain, encode_port(Port)]).

-spec encode_port(inet:port_number()) -> binary().
encode_port(?DEFAULT_HOMESERVER_PORT) -> <<>>;
encode_port(Port) -> <<":", (integer_to_binary(Port))/binary>>.

-spec char_to_sigil(binary()) -> sigil().
char_to_sigil(<<?SIGIL_USER_ID>>) -> 'user';
char_to_sigil(<<?SIGIL_ROOM_ID>>) -> 'room';
char_to_sigil(<<?SIGIL_EVENT_ID>>) -> 'event';
char_to_sigil(<<?SIGIL_GROUP_ID>>) -> 'group';
char_to_sigil(<<?SIGIL_ROOM_ALIAS>>) -> 'room_alias'.

-spec sigil_to_char(sigil()) -> binary().
sigil_to_char('user') -> <<?SIGIL_USER_ID>>;
sigil_to_char('room') -> <<?SIGIL_ROOM_ID>>;
sigil_to_char('event') -> <<?SIGIL_EVENT_ID>>;
sigil_to_char('group') -> <<?SIGIL_GROUP_ID>>;
sigil_to_char('room_alias') -> <<?SIGIL_ROOM_ALIAS>>.
