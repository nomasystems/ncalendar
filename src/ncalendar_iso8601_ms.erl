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
-module(ncalendar_iso8601_ms).

%%% BEHAVIOURS
-behaviour(ncalendar_format).

%%% EXTERNAL EXPORTS
-export([
    from_datetimezone/1,
    is_valid/1,
    to_datetimezone/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
from_datetimezone({Datetime, Subseconds, <<"Z">>}) ->
    from_datetimezone({Datetime, Subseconds, +0000});
from_datetimezone({_Datetime, {millisecond, Milliseconds}, Timezone} = Datetimezone) ->
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
    ]).

is_valid(Value) when is_binary(Value) ->
    is_valid(erlang:binary_to_list(Value));
is_valid([Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2, $., _Ml1, _Ml2, _Ml3 | TZ]) ->
    try
        true = ncalendar_util:is_valid_timezone(erlang:list_to_integer(resolve_timezone_alias(TZ))),
        Year = erlang:list_to_integer([Y1, Y2, Y3, Y4]),
        Month = erlang:list_to_integer([Mo1, Mo2]),
        Day = erlang:list_to_integer([D1, D2]),
        true = ncalendar_util:is_valid_date({Year, Month, Day}),
        Hour = erlang:list_to_integer([H1, H2]),
        Min = erlang:list_to_integer([Mi1, Mi2]),
        Sec = erlang:list_to_integer([S1, S2]),
        true = ncalendar_util:is_valid_time({Hour, Min, Sec})
    catch
        _Class:_Term ->
            false
    end;
is_valid(_Value) ->
    false.

to_datetimezone(Value) when is_binary(Value) ->
    to_datetimezone(erlang:binary_to_list(Value));
to_datetimezone([
    Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2, $., Ml1, Ml2, Ml3 | TZ
]) ->
    Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
    Year = erlang:list_to_integer([Y1, Y2, Y3, Y4]),
    Month = erlang:list_to_integer([Mo1, Mo2]),
    Day = erlang:list_to_integer([D1, D2]),
    RawDate = {Year, Month, Day},
    Hour = erlang:list_to_integer([H1, H2]),
    Min = erlang:list_to_integer([Mi1, Mi2]),
    Sec = erlang:list_to_integer([S1, S2]),
    RawTime = {Hour, Min, Sec},
    Millisec = erlang:list_to_integer([Ml1, Ml2, Ml3]),
    ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, {millisecond, Millisec}, Timezone);
to_datetimezone(Value) ->
    erlang:throw({error, ncalendar_iso8601_ms, {unrecognized_value, Value}}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
timezone(Timezone) ->
    case ncalendar_util:is_valid_timezone(Timezone) of
        true ->
            format_timezone(Timezone);
        _False ->
            erlang:throw({error, ncalendar, {unsupported_timezone, Timezone}})
    end.

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
