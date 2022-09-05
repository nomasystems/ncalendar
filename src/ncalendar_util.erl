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
-module(ncalendar_util).

%%% INCLUDE FILES
-include("ncalendar.hrl").

%%% EXTERNAL EXPORTS
-export([
    datetime_to_datetimezone/2,
    datetimezone_to_datetime/1,
    is_valid_date/1,
    is_valid_time/1,
    is_valid_timezone/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec datetime_to_datetimezone(Datetime, Timezone) -> Result when
    Datetime :: datetime(),
    Timezone :: timezone(),
    Result :: datetimezone().
datetime_to_datetimezone(Datetime, +0000) ->
    {Datetime, +0000};
datetime_to_datetimezone(RawDatetime, Timezone) ->
    GregorianSeconds =
        calendar:datetime_to_gregorian_seconds(RawDatetime) - timezone_diff(Timezone),
    Datetime = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    {Datetime, Timezone}.

-spec datetimezone_to_datetime(Datetimezone) -> Result when
    Datetimezone :: datetimezone(),
    Result :: datetime().
datetimezone_to_datetime({Datetime, +0000}) ->
    Datetime;
datetimezone_to_datetime({Datetime, Timezone}) ->
    LocalSeconds = calendar:datetime_to_gregorian_seconds(Datetime) + timezone_diff(Timezone),
    calendar:gregorian_seconds_to_datetime(LocalSeconds).

-spec is_valid_date(Date) -> Result when
    Date :: date(),
    Result :: boolean().
is_valid_date({Year, Month, Day}) ->
    calendar:valid_date(Year, Month, Day).

-spec is_valid_time(Time) -> Result when
    Time :: time(),
    Result :: boolean().
is_valid_time({H, M, S}) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 ->
    true;
is_valid_time(_Time) ->
    false.

-spec is_valid_timezone(Timezone) -> Result when
    Timezone :: timezone(),
    Result :: boolean().
is_valid_timezone(Timezone) ->
    lists:member(Timezone, ?TIMEZONES).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec timezone_diff(Timezone) -> Result when
    Timezone :: timezone(),
    Result :: integer().
timezone_diff(Timezone) ->
    Min = erlang:abs(Timezone rem 100),
    Hour = Timezone div 100,
    timezone_diff(Hour, Min).

timezone_diff(Hour, Min) when Hour < 0 ->
    -1 * ((erlang:abs(Hour) * 3600) + (Min * 60));
timezone_diff(Hour, Min) ->
    (Hour * 3600) + (Min * 60).
