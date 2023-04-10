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
    datetime_to_datetimezone/3,
    datetimezone_to_datetime/1,
    datetimezone_to_gregorian_seconds/1,
    datetimezone_to_timestamp/1,
    is_valid_date/1,
    is_valid_time/1,
    is_valid_timezone/1,
    milliseconds_to_datetimezone/2,
    timestamp_to_milliseconds/1
]).

-define(JANUARY_1ST_1970, 62167219200).
%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec datetime_to_datetimezone(Datetime, Subseconds, Timezone) -> Result when
    Datetime :: datetime(),
    Subseconds :: sub_seconds(),
    Timezone :: timezone(),
    Result :: datetimezone().
datetime_to_datetimezone(Datetime, Subseconds, +0000) ->
    {Datetime, Subseconds, +0000};
datetime_to_datetimezone(RawDatetime, Subseconds, Timezone) ->
    GregorianSeconds =
        calendar:datetime_to_gregorian_seconds(RawDatetime) - timezone_diff(Timezone),
    Datetime = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    {Datetime, Subseconds, Timezone}.

-spec datetimezone_to_datetime(Datetimezone) -> Result when
    Datetimezone :: datetimezone(),
    Result :: datetime().
datetimezone_to_datetime({Datetime, _Subseconds, +0000}) ->
    Datetime;
datetimezone_to_datetime({Datetime, _Subseconds, Timezone}) ->
    LocalSeconds = calendar:datetime_to_gregorian_seconds(Datetime) + timezone_diff(Timezone),
    calendar:gregorian_seconds_to_datetime(LocalSeconds).

-spec datetimezone_to_gregorian_seconds(Datetimezone) -> Result when
    Datetimezone :: datetimezone(),
    Result :: gregorian_seconds().
datetimezone_to_gregorian_seconds({Datetime, _Subseconds, Timezone}) ->
    calendar:datetime_to_gregorian_seconds(Datetime) + timezone_diff(Timezone).

-spec datetimezone_to_timestamp(Datetimezone) -> Result when
    Datetimezone :: datetimezone(),
    Result :: timestamp().
datetimezone_to_timestamp({Datetime, {millisecond, Milliseconds}, Timezone}) ->
    GregorianSeconds = calendar:datetime_to_gregorian_seconds(Datetime) + timezone_diff(Timezone),
    Secs = GregorianSeconds - ?JANUARY_1ST_1970,
    MicroSecs = Milliseconds * 1000,
    {Secs div 1000000, Secs rem 1000000, MicroSecs}.

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

-spec milliseconds_to_datetimezone(Milliseconds, Timezone) -> Result when
    Milliseconds :: non_neg_integer(),
    Timezone :: timezone(),
    Result :: datetimezone().
milliseconds_to_datetimezone(Milliseconds, TimeZone) ->
    GregorianSeconds = Milliseconds div 1000,
    RestMilliseconds = Milliseconds - GregorianSeconds * 1000,
    Subseconds = {millisecond, RestMilliseconds},
    Datetime = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    {Datetime, Subseconds, TimeZone}.

-spec timestamp_to_milliseconds(Timestamp) -> Result when
    Timestamp :: timestamp(),
    Result :: non_neg_integer().
timestamp_to_milliseconds({MSecs, Secs, MicroSecs}) ->
    MilliSecs = MicroSecs div 1000,
    GregorianSeconds = MSecs * 1000000 + Secs + ?JANUARY_1ST_1970,
    GregorianSeconds * 1000 + MilliSecs.

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
