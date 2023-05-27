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
%
%% @private
%% @doc Behaviour for <code>ncalendar</code> format modules.
-module(ncalendar_format).

%%% TYPES
-type datetimezone() :: {
    calendar:datetime(), sub_seconds(), ncalendar:timezone() | ncalendar:timezone_alias()
}.
% Datetime is: UTC time | local time (if no timezone is specified).
-type milliseconds() :: non_neg_integer().
-type sub_seconds() :: {millisecond, milliseconds()}.

%%% EXPORT TYPES
-export_type([
    datetimezone/0,
    milliseconds/0,
    sub_seconds/0
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback from_datetimezone(Datetimezone, Opts) -> Result when
    Datetimezone :: datetimezone(),
    Opts :: map(),
    Result :: ncalendar:value().
% Converts a <code>datetimezone()</code> value to a binary representation of the implementer format.

-callback is_valid(Value, Opts) -> Result when
    Value :: ncalendar:value(),
    Opts :: map(),
    Result :: boolean().
% Checks if a value is a valid datetime in the implementer format.

-callback to_datetimezone(Value) -> Result when
    Value :: ncalendar:value(),
    Result :: datetimezone().
% Converts a binary representation of the implementer format to a <code>datetimezone()</code> value.
