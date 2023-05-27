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
%% @doc <code>ncalendar</code>'s <code>HTTP Date</code> format module.
-module(ncalendar_http_date).

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
-define(GMT_TIMEZONE, "+0000").

%%%-----------------------------------------------------------------------------
%%% FORMAT EXPORTS
%%%-----------------------------------------------------------------------------
%% @private
%% @doc Converts a <code>datetimezone()</code> value to an <code>HTTP Date</code> binary.
from_datetimezone({Datetime, Subseconds, <<"GMT">>}, Opts) ->
    from_datetimezone({Datetime, Subseconds, +0000}, Opts);
from_datetimezone({{{Year, Month, Day}, {Hour, Min, Sec}}, _Subseconds, +0000}, _Opts) ->
    FDay = format_day(calendar:day_of_the_week({Year, Month, Day})),
    FMonth = format_month(Month),
    erlang:list_to_binary([
        FDay,
        ", ",
        ncalendar_util:pad(2, Day),
        "-",
        FMonth,
        "-",
        ncalendar_util:pad(4, Year),
        " ",
        ncalendar_util:pad(2, Hour),
        ":",
        ncalendar_util:pad(2, Min),
        ":",
        ncalendar_util:pad(2, Sec),
        " GMT"
    ]);
from_datetimezone({Datetime, Subseconds, _TZ}, Opts) ->
    from_datetimezone({Datetime, Subseconds, +0000}, Opts).

%% @private
%% @doc Checks if a value is a valid <code>HTTP Date</code> datetime.
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
        $-,
        FM1,
        FM2,
        FM3,
        $-,
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
        ?GMT_TIMEZONE = resolve_timezone_alias(TZ),
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
%% @doc Converts an <code>HTTP Date</code> binary to a <code>datetimezone()</code> value.
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
        $-,
        FM1,
        FM2,
        FM3,
        $-,
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
    erlang:throw({error, ncalendar_http_date, {unrecognized_value, Value}}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
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
    ?GMT_TIMEZONE;
resolve_timezone_alias(TZ) ->
    erlang:throw({error, ncalendar_http_date, {unrecognized_timezone, TZ}}).
