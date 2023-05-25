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
-ifndef(ncalendar).
-define(ncalendar, true).

%%% TYPES
-type date() :: calendar:date().
-type datetime() :: calendar:datetime().
% Datetime is a UTC time.
-type datetimezone() :: {datetime(), sub_seconds(), timezone() | timezone_alias()}.
-type format() :: iso8601.
-type gregorian_seconds() :: non_neg_integer().
-type milliseconds() :: non_neg_integer().
-type opts() :: map().
-type sub_seconds() :: {millisecond, milliseconds()}.
-type time() :: calendar:time().
-type timestamp() :: erlang:timestamp().
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
%% <<"Z">>
-type timezone_alias() :: binary().
-type posix_time() :: non_neg_integer().
-type value() :: binary().

%%% MACROS
-define(TIMEZONES, [
    undefined,
    -1200,
    -1100,
    -1000,
    -0930,
    -0900,
    -0800,
    -0700,
    -0600,
    -0500,
    -0430,
    -0400,
    -0330,
    -0300,
    -0230,
    -0200,
    -0100,
    +0000,
    +0100,
    +0200,
    +0300,
    +0330,
    +0400,
    +0430,
    +0500,
    +0530,
    +0545,
    +0600,
    +0630,
    +0700,
    +0730,
    +0800,
    +0900,
    +0930,
    +1000,
    +1030,
    +1100,
    +1130,
    +1200,
    +1245,
    +1300,
    +1345,
    +1400
]).

% -ifndef(ncalendar)
-endif.
