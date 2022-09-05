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
    from_datetime/3,
    is_valid/2,
    now/1,
    now/2,
    to_datetime/2
]).

%%% TYPES
-export_type([
    datetime/0,
    format/0,
    timezone/0,
    timezone_alias/0,
    value/0
]).

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
    from_datetime(Format, Datetime, +0000).

-spec from_datetime(Format, Datetime, Timezone) -> Result when
    Format :: format(),
    Datetime :: datetime(),
    Timezone :: timezone(),
    Result :: value().
from_datetime(Format, Datetime, Timezone) ->
    Mod = mod(Format),
    Mod:from_datetimezone({Datetime, Timezone}).

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
    {Date, Time} = erlang:universaltime(),
    Mod = mod(Format),
    Mod:from_datetimezone({{Date, Time}, Timezone}).

-spec to_datetime(Format, Value) -> Result when
    Format :: format(),
    Value :: value(),
    Result :: datetime().
to_datetime(Format, Value) ->
    Mod = mod(Format),
    Datetimezone = Mod:to_datetimezone(Value),
    ncalendar_util:datetimezone_to_datetime(Datetimezone).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
mod(iso8601) ->
    ncalendar_iso8601;
mod(Format) ->
    erlang:throw({error, ncalendar, {unsupported_format, Format}}).
