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
    is_valid/2,
    now/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec convert(From, To, Value) -> Result when
    From :: format(),
    To :: format(),
    Value :: binary(),
    Result :: binary().
convert(From, To, Value) ->
    ModFrom = mod(From),
    ModTo = mod(To),
    ModTo:from_datetime(ModFrom:to_datetime(Value)).

-spec is_valid(Format, Value) -> Result when
    Format :: format(),
    Value :: binary(),
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
    Mod:from_datetime({Date, Time, Timezone}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
mod(iso8601) ->
    ncalendar_iso8601;
mod(Format) ->
    erlang:throw({error, ncalendar, {unsupported_format, Format}}).
