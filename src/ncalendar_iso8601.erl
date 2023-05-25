%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License
-module(ncalendar_iso8601).

%%% BEHAVIOURS
-behaviour(ncalendar_format).

%%% EXTERNAL EXPORTS
-export([
    from_datetimezone/2,
    is_valid/2,
    to_datetimezone/1
]).

%% TYPES
-type opts() :: #{precision => precision(), extended => boolean()}.
-type precision() :: millisecond.

-export_type([
    opts/0,
    precision/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
from_datetimezone({Datetime, Subseconds, <<"Z">>}, Opts) ->
    from_datetimezone({Datetime, Subseconds, +0000}, Opts);
%% Extended Milliseconds
from_datetimezone({_Datetime, {millisecond, Milliseconds}, Timezone} = Datetimezone, #{
    extended := true, precision := millisecond
}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = ncalendar_util:datetimezone_to_datetime(Datetimezone),
    erlang:list_to_binary([
        ncalendar_util:pad(4, Year),
        "-",
        ncalendar_util:pad(2, Month),
        "-",
        ncalendar_util:pad(2, Day),
        "T",
        ncalendar_util:pad(2, Hour),
        ":",
        ncalendar_util:pad(2, Min),
        ":",
        ncalendar_util:pad(2, Sec),
        ".",
        ncalendar_util:pad(3, Milliseconds),
        format_timezone(Timezone)
    ]);
%% Extended
from_datetimezone({_Datetime, _Subseconds, Timezone} = Datetimezone, #{extended := true}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = ncalendar_util:datetimezone_to_datetime(Datetimezone),
    erlang:list_to_binary([
        ncalendar_util:pad(4, Year),
        "-",
        ncalendar_util:pad(2, Month),
        "-",
        ncalendar_util:pad(2, Day),
        "T",
        ncalendar_util:pad(2, Hour),
        ":",
        ncalendar_util:pad(2, Min),
        ":",
        ncalendar_util:pad(2, Sec),
        format_timezone(Timezone)
    ]);
%% Milliseconds
from_datetimezone({_Datetime, {millisecond, Milliseconds}, Timezone} = Datetimezone, #{
    precision := millisecond
}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = ncalendar_util:datetimezone_to_datetime(Datetimezone),
    erlang:list_to_binary([
        ncalendar_util:pad(4, Year),
        ncalendar_util:pad(2, Month),
        ncalendar_util:pad(2, Day),
        "T",
        ncalendar_util:pad(2, Hour),
        ncalendar_util:pad(2, Min),
        ncalendar_util:pad(2, Sec),
        ".",
        ncalendar_util:pad(3, Milliseconds),
        format_timezone(Timezone)
    ]);
%% Default
from_datetimezone({_Datetime, _Subseconds, Timezone} = Datetimezone, _Opts) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = ncalendar_util:datetimezone_to_datetime(Datetimezone),
    erlang:list_to_binary([
        ncalendar_util:pad(4, Year),
        ncalendar_util:pad(2, Month),
        ncalendar_util:pad(2, Day),
        "T",
        ncalendar_util:pad(2, Hour),
        ncalendar_util:pad(2, Min),
        ncalendar_util:pad(2, Sec),
        format_timezone(Timezone)
    ]).

is_valid(Value, Opts) when is_binary(Value) ->
    is_valid(erlang:binary_to_list(Value), Opts);
%% Extended Milliseconds
is_valid(
    [
        Y1,
        Y2,
        Y3,
        Y4,
        $-,
        Mo1,
        Mo2,
        $-,
        D1,
        D2,
        $T,
        H1,
        H2,
        $:,
        Mi1,
        Mi2,
        $:,
        S1,
        S2,
        $.,
        _Ml1,
        _Ml2,
        _Ml3
        | TZ
    ],
    _Opts
) ->
    try
        Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
        true = ncalendar_util:is_valid_time(Time)
    catch
        _Class:_Term ->
            false
    end;
%% Extended
is_valid(
    [
        _Y1,
        _Y2,
        _Y3,
        _Y4,
        $-,
        _Mo1,
        _Mo2,
        $-,
        _D1,
        _D2,
        $T,
        _H1,
        _H2,
        $:,
        _Mi1,
        _Mi2,
        $:,
        _S1,
        _S2
        | _TZ
    ],
    #{precision := millisecond}
) ->
    false;
is_valid(
    [Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2, $T, H1, H2, $:, Mi1, Mi2, $:, S1, S2 | TZ], _Opts
) ->
    try
        Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
        true = ncalendar_util:is_valid_time(Time)
    catch
        _Class:_Term ->
            false
    end;
%% Milliseconds
is_valid(
    [
        _Y1,
        _Y2,
        _Y3,
        _Y4,
        _Mo1,
        _Mo2,
        _D1,
        _D2,
        $T,
        _H1,
        _H2,
        _Mi1,
        _Mi2,
        _S1,
        _S2,
        $.,
        _Ml1,
        _Ml2,
        _Ml3
        | _TZ
    ],
    #{extended := true}
) ->
    false;
is_valid(
    [Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2, $., _Ml1, _Ml2, _Ml3 | TZ],
    _Opts
) ->
    try
        Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
        true = ncalendar_util:is_valid_time(Time)
    catch
        _Class:_Term ->
            false
    end;
%% Default
is_valid([_Y1, _Y2, _Y3, _Y4, _Mo1, _Mo2, _D1, _D2, $T, _H1, _H2, _Mi1, _Mi2, _S1, _S2 | _TZ], #{
    precision := millisecond
}) ->
    false;
is_valid([_Y1, _Y2, _Y3, _Y4, _Mo1, _Mo2, _D1, _D2, $T, _H1, _H2, _Mi1, _Mi2, _S1, _S2 | _TZ], #{
    extended := true
}) ->
    false;
is_valid([Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2 | TZ], _Opts) ->
    try
        Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
        true = ncalendar_util:is_valid_time(Time)
    catch
        _Class:_Term ->
            false
    end;
is_valid(_Value, _Opts) ->
    false.

to_datetimezone(Value) when is_binary(Value) ->
    to_datetimezone(erlang:binary_to_list(Value));
%% Extended Milliseconds
to_datetimezone(
    [
        Y1,
        Y2,
        Y3,
        Y4,
        $-,
        Mo1,
        Mo2,
        $-,
        D1,
        D2,
        $T,
        H1,
        H2,
        $:,
        Mi1,
        Mi2,
        $:,
        S1,
        S2,
        $.,
        Ml1,
        Ml2,
        Ml3
        | TZ
    ]
) ->
    Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
    RawDate = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    Millisec = erlang:list_to_integer([Ml1, Ml2, Ml3]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, Millisec}, Timezone);
%% Extended
to_datetimezone(
    [
        Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2, $T, H1, H2, $:, Mi1, Mi2, $:, S1, S2 | TZ
    ]
) ->
    Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
    RawDate = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, 0}, Timezone);
%% Milliseconds
to_datetimezone(
    [
        Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2, $., Ml1, Ml2, Ml3 | TZ
    ]
) ->
    Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
    RawDate = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    Millisec = erlang:list_to_integer([Ml1, Ml2, Ml3]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, Millisec}, Timezone);
%% Default
to_datetimezone([Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2 | TZ]) ->
    Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
    RawDate = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, 0}, Timezone);
to_datetimezone(Value) ->
    erlang:throw({error, ncalendar_iso8601, {unrecognized_value, Value}}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
do_format_timezone(undefined) ->
    <<>>;
do_format_timezone(Val) when Val > 999 ->
    erlang:list_to_binary([$+, erlang:integer_to_binary(Val)]);
do_format_timezone(Val) when Val > 99 ->
    erlang:list_to_binary([$+, $0, erlang:integer_to_binary(Val)]);
do_format_timezone(Val) when Val > 9 ->
    erlang:list_to_binary([$+, $0, $0, erlang:integer_to_binary(Val)]);
do_format_timezone(Val) when Val > 0 ->
    erlang:list_to_binary([$+, $0, $0, $0, erlang:integer_to_binary(Val)]);
do_format_timezone(+0000) ->
    <<"Z">>;
do_format_timezone(Val) when Val > -10 ->
    erlang:list_to_binary([$-, $0, $0, $0, erlang:integer_to_binary(erlang:abs(Val))]);
do_format_timezone(Val) when Val > -100 ->
    erlang:list_to_binary([$-, $0, $0, erlang:integer_to_binary(erlang:abs(Val))]);
do_format_timezone(Val) when Val > -1000 ->
    erlang:list_to_binary([$-, $0, erlang:integer_to_binary(erlang:abs(Val))]);
do_format_timezone(Val) ->
    erlang:integer_to_binary(Val).

format_timezone(Timezone) ->
    case ncalendar_util:is_valid_timezone(Timezone) of
        true ->
            do_format_timezone(Timezone);
        _False ->
            erlang:throw({error, ncalendar, {unsupported_timezone, Timezone}})
    end.

resolve_timezone_alias("Z") ->
    "+0000";
resolve_timezone_alias(TZ) ->
    TZ.
