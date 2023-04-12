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
    timezone/1,
    to_datetimezone/1
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
        ncalendar_iso8601:timezone(Timezone)
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
        ncalendar_iso8601:timezone(Timezone)
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
        timezone(Timezone)
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
        timezone(Timezone)
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
        Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
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
        Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
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
        Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
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
        Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        Date = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        true = ncalendar_util:is_valid_date(Date),
        Time = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
        true = ncalendar_util:is_valid_time(Time)
    catch
        _Class:_Term ->
            false
    end;
is_valid(_Value, _Opts) ->
    false.

timezone(Timezone) ->
    case ncalendar_util:is_valid_timezone(Timezone) of
        true ->
            format_timezone(Timezone);
        _False ->
            erlang:throw({error, ncalendar, {unsupported_timezone, Timezone}})
    end.

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
    Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
    RawDate = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    Millisec = erlang:list_to_integer([Ml1, Ml2, Ml3]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, Millisec}, Timezone);
%% Extended
to_datetimezone(
    [
        Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2, $T, H1, H2, $:, Mi1, Mi2, $:, S1, S2 | TZ
    ]
) ->
    Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
    RawDate = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, 0}, Timezone);
%% Milliseconds
to_datetimezone(
    [
        Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2, $., Ml1, Ml2, Ml3 | TZ
    ]
) ->
    Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
    RawDate = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    Millisec = erlang:list_to_integer([Ml1, Ml2, Ml3]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, Millisec}, Timezone);
%% Default
to_datetimezone([Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2 | TZ]) ->
    Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
    RawDate = to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, 0}, Timezone);
to_datetimezone(Value) ->
    erlang:throw({error, ncalendar_iso8601, {unrecognized_value, Value}}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
format_timezone(Val) when Val > 999 ->
    erlang:list_to_binary([$+, erlang:integer_to_binary(Val)]);
format_timezone(Val) when Val > 99 ->
    erlang:list_to_binary([$+, $0, erlang:integer_to_binary(Val)]);
format_timezone(Val) when Val > 9 ->
    erlang:list_to_binary([$+, $0, $0, erlang:integer_to_binary(Val)]);
format_timezone(Val) when Val > 0 ->
    erlang:list_to_binary([$+, $0, $0, $0, erlang:integer_to_binary(Val)]);
format_timezone(+0000) ->
    <<"Z">>;
format_timezone(Val) when Val > -10 ->
    erlang:list_to_binary([$-, $0, $0, $0, erlang:integer_to_binary(erlang:abs(Val))]);
format_timezone(Val) when Val > -100 ->
    erlang:list_to_binary([$-, $0, $0, erlang:integer_to_binary(erlang:abs(Val))]);
format_timezone(Val) when Val > -1000 ->
    erlang:list_to_binary([$-, $0, erlang:integer_to_binary(erlang:abs(Val))]);
format_timezone(Val) ->
    erlang:integer_to_binary(Val).

resolve_timezone_alias("Z") ->
    "+0000";
resolve_timezone_alias(TZ) ->
    TZ.

to_date(YearList, MonthList, DayList) ->
    Year = erlang:list_to_integer(YearList),
    Month = erlang:list_to_integer(MonthList),
    Day = erlang:list_to_integer(DayList),
    {Year, Month, Day}.

to_time(HourList, MinList, SecList) ->
    Hour = erlang:list_to_integer(HourList),
    Min = erlang:list_to_integer(MinList),
    Sec = erlang:list_to_integer(SecList),
    {Hour, Min, Sec}.
