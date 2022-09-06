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
    from_datetimezone/1,
    is_valid/1,
    to_datetimezone/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
from_datetimezone({Datetime, <<"Z">>}) ->
    from_datetimezone({Datetime, +0000});
from_datetimezone({_Datetime, Timezone} = Datetimezone) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = ncalendar_util:datetimezone_to_datetime(Datetimezone),
    erlang:list_to_binary([
        pad(4, Year),
        pad(2, Month),
        pad(2, Day),
        "T",
        pad(2, Hour),
        pad(2, Min),
        pad(2, Sec),
        timezone(Timezone)
    ]).

is_valid(Value) ->
    try
        [Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2 | TZ] = erlang:binary_to_list(
            Value
        ),
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
    end.

to_datetimezone(Value) ->
    try
        [Y1, Y2, Y3, Y4, Mo1, Mo2, D1, D2, $T, H1, H2, Mi1, Mi2, S1, S2 | TZ] = erlang:binary_to_list(
            Value
        ),
        Timezone = erlang:list_to_integer(resolve_timezone_alias(TZ)),
        Year = erlang:list_to_integer([Y1, Y2, Y3, Y4]),
        Month = erlang:list_to_integer([Mo1, Mo2]),
        Day = erlang:list_to_integer([D1, D2]),
        RawDate = {Year, Month, Day},
        Hour = erlang:list_to_integer([H1, H2]),
        Min = erlang:list_to_integer([Mi1, Mi2]),
        Sec = erlang:list_to_integer([S1, S2]),
        RawTime = {Hour, Min, Sec},
        ncalendar_util:datetime_to_datetimezone({RawDate, RawTime}, Timezone)
    catch
        _Class:_Term ->
            erlang:throw({error, ncalendar_iso8601, {unrecognized_value, Value}})
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
pad(2, N) when N > 9 ->
    erlang:integer_to_list(N);
pad(2, N) ->
    [$0 | erlang:integer_to_list(N)];
pad(4, N) when N > 999 ->
    erlang:integer_to_list(N);
pad(4, N) when N > 99 ->
    [$0 | erlang:integer_to_list(N)];
pad(4, N) when N > 9 ->
    [$0, $0 | erlang:integer_to_list(N)];
pad(4, N) ->
    [$0, $0, $0 | erlang:integer_to_list(N)].

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
