%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
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
%
%% @doc <code>ncalendar</code>'s <code>imf-fixdate</code> format module.
-module(ncalendar_imf_fixdate).

%%% BEHAVIOURS
-behaviour(ncalendar_format).

%%% FORMAT EXPORTS
-export([
    from_datetimezone/2,
    is_valid/2,
    to_datetimezone/1
]).

%%% MACROS
-define(JANUARY, "Jan").
-define(FEBRARY, "Feb").
-define(MARCH, "Mar").
-define(APRIL, "Apr").
-define(MAY, "May").
-define(JUNY, "Jun").
-define(JULY, "Jul").
-define(AUGUST, "Aug").
-define(SEPTEMBER, "Sep").
-define(OCTOBER, "Oct").
-define(NOVEMBER, "Nov").
-define(DECEMBER, "Dec").

%%%-----------------------------------------------------------------------------
%%% FORMAT EXPORTS
%%%-----------------------------------------------------------------------------
%% @private
%% @doc Converts a <code>datetimezone()</code> value to an <code>imf-fixdate</code> binary.
from_datetimezone({Datetime, Subseconds, <<"GMT">>}, Opts) ->
    from_datetimezone({Datetime, Subseconds, +0000}, Opts);
from_datetimezone({_Datetime, _Subseconds, Timezone} = Datetimezone, _Opts) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = ncalendar_util:datetimezone_to_datetime(Datetimezone),
    FDay = format_day(calendar:day_of_the_week({Year, Month, Day})),
    FMonth = format_month(Month),
    erlang:list_to_binary([
        FDay,
        ", ",
        ncalendar_util:pad(2, Day),
        " ",
        FMonth,
        " ",
        ncalendar_util:pad(4, Year),
        " ",
        ncalendar_util:pad(2, Hour),
        ":",
        ncalendar_util:pad(2, Min),
        ":",
        ncalendar_util:pad(2, Sec),
        " ",
        format_timezone(Timezone)
    ]).

%% @private
%% @doc Checks if a value is a valid <code>imf-fixdate</code> datetime.
is_valid(Value, Opts) when is_binary(Value) ->
    is_valid(erlang:binary_to_list(Value), Opts);
is_valid(
    [
        FD1,
        FD2,
        FD3,
        $,,
        $\s,
        D1,
        D2,
        $\s,
        FM1,
        FM2,
        FM3,
        $\s,
        Y1,
        Y2,
        Y3,
        Y4,
        $\s,
        H1,
        H2,
        $:,
        Mi1,
        Mi2,
        $:,
        S1,
        S2,
        $\s
        | TZ
    ],
    _Opts
) ->
    try
        Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
        true = ncalendar_util:is_valid_timezone(Timezone),
        [Mo1, Mo2] = ncalendar_util:pad(2, from_month([FM1, FM2, FM3])),
        {_Year, Month, _Day} =
            Date = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
        FDay = [FD1, FD2, FD3],
        FDay = format_day(calendar:day_of_the_week(Date)),
        FMonth = [FM1, FM2, FM3],
        FMonth = format_month(Month),
        true = ncalendar_util:is_valid_date(Date),
        Time = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
        true = ncalendar_util:is_valid_time(Time)
    catch
        _Class:_Term ->
            false
    end;
is_valid(_Value, _Opts) ->
    false.

%% @private
%% @doc Converts an <code>imf-fixdate</code> binary to a <code>datetimezone()</code> value.
to_datetimezone(Value) when is_binary(Value) ->
    to_datetimezone(erlang:binary_to_list(Value));
to_datetimezone(
    [
        _FD1,
        _FD2,
        _FD3,
        $,,
        $\s,
        D1,
        D2,
        $\s,
        FM1,
        FM2,
        FM3,
        $\s,
        Y1,
        Y2,
        Y3,
        Y4,
        $\s,
        H1,
        H2,
        $:,
        Mi1,
        Mi2,
        $:,
        S1,
        S2,
        $\s
        | TZ
    ]
) ->
    Timezone = ncalendar_util:to_timezone(resolve_timezone_alias(TZ)),
    [Mo1, Mo2] = ncalendar_util:pad(2, from_month([FM1, FM2, FM3])),
    RawDate = ncalendar_util:to_date([Y1, Y2, Y3, Y4], [Mo1, Mo2], [D1, D2]),
    RawTime = ncalendar_util:to_time([H1, H2], [Mi1, Mi2], [S1, S2]),
    ncalendar_util:datetime_to_datetimezone(
        {RawDate, RawTime}, {millisecond, 0}, Timezone
    );
to_datetimezone(Value) ->
    erlang:throw({error, ncalendar_imf_fixdate, {unrecognized_value, Value}}).

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
    <<"GMT">>;
do_format_timezone(Val) when Val > -10 ->
    erlang:list_to_binary([$-, $0, $0, $0, erlang:integer_to_binary(erlang:abs(Val))]);
do_format_timezone(Val) when Val > -100 ->
    erlang:list_to_binary([$-, $0, $0, erlang:integer_to_binary(erlang:abs(Val))]);
do_format_timezone(Val) when Val > -1000 ->
    erlang:list_to_binary([$-, $0, erlang:integer_to_binary(erlang:abs(Val))]);
do_format_timezone(Val) ->
    erlang:integer_to_binary(Val).

format_day(1) ->
    "Mon";
format_day(2) ->
    "Tue";
format_day(3) ->
    "Wed";
format_day(4) ->
    "Thu";
format_day(5) ->
    "Fri";
format_day(6) ->
    "Sat";
format_day(7) ->
    "Sun".

format_month(1) ->
    ?JANUARY;
format_month(2) ->
    ?FEBRARY;
format_month(3) ->
    ?MARCH;
format_month(4) ->
    ?APRIL;
format_month(5) ->
    ?MAY;
format_month(6) ->
    ?JUNY;
format_month(7) ->
    ?JULY;
format_month(8) ->
    ?AUGUST;
format_month(9) ->
    ?SEPTEMBER;
format_month(10) ->
    ?OCTOBER;
format_month(11) ->
    ?NOVEMBER;
format_month(12) ->
    ?DECEMBER.

format_timezone(Timezone) ->
    case ncalendar_util:is_valid_timezone(Timezone) of
        true ->
            do_format_timezone(Timezone);
        _False ->
            erlang:throw({error, ncalendar_imf_fixdate, {unsupported_timezone, Timezone}})
    end.

from_month(?JANUARY) ->
    1;
from_month(?FEBRARY) ->
    2;
from_month(?MARCH) ->
    3;
from_month(?APRIL) ->
    4;
from_month(?MAY) ->
    5;
from_month(?JUNY) ->
    6;
from_month(?JULY) ->
    7;
from_month(?AUGUST) ->
    8;
from_month(?SEPTEMBER) ->
    9;
from_month(?OCTOBER) ->
    10;
from_month(?NOVEMBER) ->
    11;
from_month(?DECEMBER) ->
    12.

resolve_timezone_alias("GMT") ->
    "+0000";
resolve_timezone_alias([]) ->
    [];
resolve_timezone_alias(TZ) ->
    case ncalendar_util:is_valid_timezone(erlang:list_to_integer(TZ)) of
        true ->
            TZ;
        false ->
            erlang:throw({error, ncalendar_imf_fixdate, {unrecognized_timezone, TZ}})
    end.
