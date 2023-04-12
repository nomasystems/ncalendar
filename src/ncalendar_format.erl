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
-module(ncalendar_format).

%%% INCLUDE FILES
-include("ncalendar.hrl").

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback from_datetimezone(Datetimezone, Opts) -> Result when
    Datetimezone :: datetimezone(),
    Opts :: map(),
    Result :: value().

-callback is_valid(Value, Opts) -> Result when
    Value :: value(),
    Opts :: map(),
    Result :: boolean().

-callback to_datetimezone(Value) -> Result when
    Value :: value(),
    Result :: datetimezone().
