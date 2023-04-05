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
    from_datetime/2,
    from_gregorian_seconds/2,
    from_timestamp/2,
    is_valid/2,
    now/1,
    now/2,
    to_datetime/2,
    to_gregorian_seconds/2,
    to_timestamp/2
]).

%%% TYPES
-export_type([
    datetime/0,
    format/0,
    gregorian_seconds/0,
    timestamp/0,
    timezone/0,
    timezone_alias/0,
    value/0
]).

%%% MACROS
-define(JANUARY_1ST_1970, 62167219200).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec convert(From, To, Value) -> Result when
    From :: format(),
    To :: format(),
    Value :: value(),
    Result :: value().
convert(From, To, Value) ->
    ModFrom = mod(From),
    ModTo = mod(To),
    ModTo:from_datetimezone(ModFrom:to_datetimezone(Value)).

-spec from_datetime(Format, Datetime) -> Result when
    Format :: format(),
    Datetime :: datetime(),
    Result :: value().
from_datetime(Format, Datetime) ->
    Datetimezone = {Datetime, {0, 0}, +0000},
    from_datetimezone(Format, Datetimezone).

-spec from_gregorian_seconds(Format, GregorianSeconds) -> Result when
    Format :: format(),
    GregorianSeconds :: gregorian_seconds(),
    Result :: value().
from_gregorian_seconds(Format, GregorianSeconds) ->
    Datetime = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    Datetimezone = {Datetime, {0, 0}, +0000},
    from_datetimezone(Format, Datetimezone).

-spec from_timestamp(Format, Timestamp) -> Result when
    Format :: format(),
    Timestamp :: timestamp(),
    Result :: value().
from_timestamp(Format, Timestamp) ->
    Datetimezone = ncalendar_util:timestamp_to_datetimezone(Timestamp, +0000),
    from_datetimezone(Format, Datetimezone).

-spec is_valid(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: boolean().
is_valid(Format, Value) ->
    Mod = mod(Format),
    Mod:is_valid(Value).

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
    Datetimezone = ncalendar_util:timestamp_to_datetimezone(erlang:timestamp(), Timezone),
    from_datetimezone(Format, Datetimezone).

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
from_datetimezone(Format, Datetimezone) ->
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone).

mod(iso8601) ->
    ncalendar_iso8601;
mod(iso8601_milliseconds) ->
    ncalendar_iso8601_milliseconds;
mod(Format) ->
    erlang:throw({error, ncalendar, {unsupported_format, Format}}).
