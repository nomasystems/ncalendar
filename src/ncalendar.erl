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
-module(ncalendar).

%%% INCLUDE FILES
-include("ncalendar.hrl").

%%% EXTERNAL EXPORTS
-export([
    convert/3,
    convert/4,
    from_datetime/2,
    from_datetime/3,
    from_gregorian_seconds/2,
    from_gregorian_seconds/3,
    from_timestamp/2,
    from_timestamp/3,
    is_valid/2,
    is_valid/3,
    now/1,
    now/2,
    now/3,
    to_datetime/2,
    to_gregorian_seconds/2,
    to_timestamp/2
]).

%%% TYPES
-export_type([
    datetime/0,
    format/0,
    gregorian_seconds/0,
    opts/0,
    timestamp/0,
    timezone/0,
    timezone_alias/0,
    value/0
]).

%%% MACROS
-define(JANUARY_1ST_1970_MS, 62167219200000).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec convert(From, To, Value) -> Result when
    From :: format(),
    To :: format(),
    Value :: value(),
    Result :: value().
convert(From, To, Value) ->
    convert(From, To, Value, #{}).

-spec convert(From, To, Value, Opts) -> Result when
    From :: format(),
    To :: format(),
    Value :: value(),
    Opts :: opts(),
    Result :: value().
convert(From, To, Value, Opts) ->
    ModFrom = mod(From),
    ModTo = mod(To),
    ModTo:from_datetimezone(ModFrom:to_datetimezone(Value), Opts).

-spec from_datetime(Format, Datetime) -> Result when
    Format :: format(),
    Datetime :: datetime(),
    Result :: value().
from_datetime(Format, Datetime) ->
    from_datetime(Format, Datetime, #{}).

-spec from_datetime(Format, Datetime, Opts) -> Result when
    Format :: format(),
    Datetime :: datetime(),
    Opts :: opts(),
    Result :: value().
from_datetime(Format, Datetime, Opts) ->
    Datetimezone = {Datetime, {millisecond, 0}, +0000},
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec from_gregorian_seconds(Format, GregorianSeconds) -> Result when
    Format :: format(),
    GregorianSeconds :: gregorian_seconds(),
    Result :: value().
from_gregorian_seconds(Format, GregorianSeconds) ->
    from_gregorian_seconds(Format, GregorianSeconds, #{}).

-spec from_gregorian_seconds(Format, GregorianSeconds, Opts) -> Result when
    Format :: format(),
    GregorianSeconds :: gregorian_seconds(),
    Opts :: opts(),
    Result :: value().
from_gregorian_seconds(Format, GregorianSeconds, Opts) ->
    Datetime = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    Datetimezone = {Datetime, {millisecond, 0}, +0000},
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec from_timestamp(Format, Timestamp) -> Result when
    Format :: format(),
    Timestamp :: timestamp(),
    Result :: value().
from_timestamp(Format, Timestamp) ->
    from_timestamp(Format, Timestamp, #{}).

-spec from_timestamp(Format, Timestamp, Opts) -> Result when
    Format :: format(),
    Timestamp :: timestamp(),
    Opts :: opts(),
    Result :: value().
from_timestamp(Format, Timestamp, Opts) ->
    Milliseconds = ncalendar_util:timestamp_to_milliseconds(Timestamp),
    Datetimezone = ncalendar_util:milliseconds_to_datetimezone(Milliseconds, +0000),
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec is_valid(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: boolean().
is_valid(Format, Value) ->
    is_valid(Format, Value, #{}).

-spec is_valid(Format, Value, Opts) -> Result when
    Format :: format(),
    Value :: value(),
    Opts :: opts(),
    Result :: boolean().
is_valid(Format, Value, Opts) ->
    Mod = mod(Format),
    Mod:is_valid(Value, Opts).

-spec now(Format) -> Result when
    Format :: format(),
    Result :: value().
now(Format) ->
    now(Format, +0000).

-spec now(Format, Timezone) -> Result when
    Format :: format(),
    Timezone :: timezone(),
    Result :: value().
now(Format, Timezone) ->
    now(Format, Timezone, #{}).

-spec now(Format, Timezone, Opts) -> Result when
    Format :: format(),
    Timezone :: timezone(),
    Opts :: opts(),
    Result :: value().
now(Format, Timezone, Opts) ->
    Datetimezone = ncalendar_util:milliseconds_to_datetimezone(
        erlang:system_time(millisecond) + ?JANUARY_1ST_1970_MS,
        Timezone
    ),
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec to_datetime(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: datetime().
to_datetime(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_datetime(Datetimezone).

-spec to_gregorian_seconds(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: gregorian_seconds().
to_gregorian_seconds(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_gregorian_seconds(Datetimezone).

-spec to_timestamp(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: timestamp().
to_timestamp(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_timestamp(Datetimezone).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
mod(iso8601) ->
    ncalendar_iso8601;
mod(rfc2109) ->
    ncalendar_rfc2109;
mod(imf_fixdate) ->
    ncalendar_imf_fixdate;
mod(Format) ->
    erlang:throw({error, ncalendar, {unsupported_format, Format}}).
