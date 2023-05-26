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
    from_posix_time/2,
    from_posix_time/3,
    from_timestamp/2,
    from_timestamp/3,
    is_valid/2,
    is_valid/3,
    now/1,
    now/2,
    now/3,
    timezone/2,
    to_datetime/2,
    to_gregorian_seconds/2,
    to_posix_time/2,
    to_timestamp/2
]).

%%% UTIL EXPORTS
-export([
    timezones/0,
    shift_timezone/3,
    shift_timezone/4
]).

%%% TYPES
-type format() :: iso8601 | http_date | imf_fixdate.
-type gregorian_seconds() :: non_neg_integer().
-type opts() :: map().
-type timezone() ::
    undefined
    | -1200
    | -1100
    | -1000
    | -0930
    | -0900
    | -0800
    | -0700
    | -0600
    | -0500
    | -0430
    | -0400
    | -0330
    | -0300
    | -0230
    | -0200
    | -0100
    | +0000
    | +0100
    | +0200
    | +0300
    | +0330
    | +0400
    | +0430
    | +0500
    | +0530
    | +0545
    | +0600
    | +0630
    | +0700
    | +0730
    | +0800
    | +0900
    | +0930
    | +1000
    | +1030
    | +1100
    | +1130
    | +1200
    | +1245
    | +1300
    | +1345
    | +1400.
-type timezone_alias() :: binary().
-type posix_time() :: non_neg_integer().
-type value() :: binary().

%%% EXPORT TYPES
-export_type([
    format/0,
    opts/0,
    timezone/0,
    timezone_alias/0,
    value/0
]).

%%% MACROS
-define(EPOCH_DELTA, 62167219200).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec convert(From, To, Value) -> Result when
    From :: format(),
    To :: format(),
    Value :: value(),
    Result :: value().
%% @equiv convert(From, To, Value, #{})
convert(From, To, Value) ->
    convert(From, To, Value, #{}).

-spec convert(From, To, Value, Opts) -> Result when
    From :: format(),
    To :: format(),
    Value :: value(),
    Opts :: opts(),
    Result :: value().
%% @doc Converts a binary representation of a datetime from one format to another.
convert(From, To, Value, Opts) ->
    ModFrom = mod(From),
    ModTo = mod(To),
    ModTo:from_datetimezone(ModFrom:to_datetimezone(Value), Opts).

-spec from_datetime(Format, Datetime) -> Result when
    Format :: format(),
    Datetime :: calendar:datetime(),
    Result :: value().
%% @equiv from_datetime(Format, Datetime, #{})
from_datetime(Format, Datetime) ->
    from_datetime(Format, Datetime, #{}).

-spec from_datetime(Format, Datetime, Opts) -> Result when
    Format :: format(),
    Datetime :: calendar:datetime(),
    Opts :: opts(),
    Result :: value().
%% @doc Converts a <code>calendar:datetime()</code> value to a binary representation in the specified format.
from_datetime(Format, Datetime, Opts) ->
    Datetimezone = {Datetime, {millisecond, 0}, +0000},
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec from_gregorian_seconds(Format, GregorianSeconds) -> Result when
    Format :: format(),
    GregorianSeconds :: gregorian_seconds(),
    Result :: value().
%% @equiv from_gregorian_seconds(Format, GregorianSeconds, #{})
from_gregorian_seconds(Format, GregorianSeconds) ->
    from_gregorian_seconds(Format, GregorianSeconds, #{}).

-spec from_gregorian_seconds(Format, GregorianSeconds, Opts) -> Result when
    Format :: format(),
    GregorianSeconds :: gregorian_seconds(),
    Opts :: opts(),
    Result :: value().
%% @doc Converts the given amout of gregorian seconds to a binary representation in the specified format.
from_gregorian_seconds(Format, GregorianSeconds, Opts) ->
    Datetime = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    Datetimezone = {Datetime, {millisecond, 0}, +0000},
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec from_posix_time(Format, PosixTime) -> Result when
    Format :: format(),
    PosixTime :: posix_time(),
    Result :: value().
%% @equiv from_posix_time(Format, PosixTime, #{})
from_posix_time(Format, PosixTime) ->
    from_posix_time(Format, PosixTime, #{}).

-spec from_posix_time(Format, PosixTime, Opts) -> Result when
    Format :: format(),
    PosixTime :: posix_time(),
    Opts :: opts(),
    Result :: value().
%% @doc Converts the given POSIX time (in seconds) to a binary representation in the specified format.
from_posix_time(Format, PosixTime, Opts) ->
    GregorianSeconds = PosixTime + ?EPOCH_DELTA,
    from_gregorian_seconds(Format, GregorianSeconds, Opts).

-spec from_timestamp(Format, Timestamp) -> Result when
    Format :: format(),
    Timestamp :: erlang:timestamp(),
    Result :: value().
%% @equiv from_timestamp(Format, Timestamp, #{})
from_timestamp(Format, Timestamp) ->
    from_timestamp(Format, Timestamp, #{}).

-spec from_timestamp(Format, Timestamp, Opts) -> Result when
    Format :: format(),
    Timestamp :: erlang:timestamp(),
    Opts :: opts(),
    Result :: value().
%% @doc Converts the given <code>erlang:timestamp</code> value to a binary representation in the specified format.
from_timestamp(Format, Timestamp, Opts) ->
    Milliseconds = ncalendar_util:timestamp_to_milliseconds(Timestamp),
    Datetimezone = ncalendar_util:milliseconds_to_datetimezone(Milliseconds, +0000),
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec is_valid(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: boolean().
%% @equiv is_valid(Format, Value, #{})
is_valid(Format, Value) ->
    is_valid(Format, Value, #{}).

-spec is_valid(Format, Value, Opts) -> Result when
    Format :: format(),
    Value :: value(),
    Opts :: opts(),
    Result :: boolean().
%% @doc Checks if the given binary representation of a datetime is valid for the specified format.
is_valid(Format, Value, Opts) ->
    Mod = mod(Format),
    Mod:is_valid(Value, Opts).

-spec now(Format) -> Result when
    Format :: format(),
    Result :: value().
%% @equiv now(Format, +0000)
now(Format) ->
    now(Format, +0000).

-spec now(Format, Timezone) -> Result when
    Format :: format(),
    Timezone :: timezone(),
    Result :: value().
%% @equiv now(Format, Timezone, #{})
now(Format, Timezone) ->
    now(Format, Timezone, #{}).

-spec now(Format, Timezone, Opts) -> Result when
    Format :: format(),
    Timezone :: timezone(),
    Opts :: opts(),
    Result :: value().
%% @doc Returns the current datetime in the specified format.
now(Format, Timezone, Opts) ->
    Datetimezone = ncalendar_util:milliseconds_to_datetimezone(
        erlang:system_time(millisecond) + (?EPOCH_DELTA * 1000),
        Timezone
    ),
    Mod = mod(Format),
    Mod:from_datetimezone(Datetimezone, Opts).

-spec timezone(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: timezone().
%% @doc Returns the timezone of the given binary representation of a datetime.
timezone(Format, Value) ->
    Mod = mod(Format),
    {_Date, _Time, Timezone} = Mod:to_datetimezone(Value),
    Timezone.

-spec to_datetime(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: calendar:datetime().
%% @doc Converts the given binary representation of a datetime to a <code>calendar:datetime()</code> value.
to_datetime(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_datetime(Datetimezone).

-spec to_gregorian_seconds(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: gregorian_seconds().
%% @doc Converts the given binary representation of a datetime to gregorian seconds.
to_gregorian_seconds(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_gregorian_seconds(Datetimezone).

-spec to_posix_time(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: posix_time().
%% @doc Converts the given binary representation of a datetime to POSIX time (in seconds).
to_posix_time(Format, Value) ->
    GregorianSeconds = to_gregorian_seconds(Format, Value),
    GregorianSeconds - ?EPOCH_DELTA.

-spec to_timestamp(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: erlang:timestamp().
%% @doc Converts the given binary representation of a datetime to a <code>erlang:timestamp()</code> value.
to_timestamp(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_timestamp(Datetimezone).

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
-spec timezones() -> Result when
    Result :: [timezone()].
%% @doc Returns a list of all supported timezones.
timezones() ->
    ?TIMEZONES.

-spec shift_timezone(Format, Value, Timezone) -> Result when
    Format :: format(),
    Value :: value(),
    Timezone :: timezone(),
    Result :: value().
%% @equiv shift_timezone(Format, Value, Timezone, #{})
shift_timezone(Format, Value, Timezone) ->
    shift_timezone(Format, Value, Timezone, #{}).

-spec shift_timezone(Format, Value, Timezone, Opts) -> Result when
    Format :: format(),
    Value :: value(),
    Timezone :: timezone(),
    Opts :: opts(),
    Result :: value().
%% @doc Shifts the timezone of the given binary representation of a datetime to the specified timezone.
shift_timezone(Format, Value, Timezone, Opts) ->
    case timezone(Format, Value) of
        undefined ->
            erlang:throw({error, ncalendar, {no_shift_allowed_without_timezone, Value}});
        _Otherwise ->
            Mod = mod(Format),
            {Datetime, Subseconds, _Timezone} = Mod:to_datetimezone(Value),
            Mod:from_datetimezone({Datetime, Subseconds, Timezone}, Opts)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
mod(iso8601) ->
    ncalendar_iso8601;
mod(http_date) ->
    ncalendar_http_date;
mod(imf_fixdate) ->
    ncalendar_imf_fixdate;
mod(Format) ->
    erlang:throw({error, ncalendar, {unsupported_format, Format}}).
