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
%% limitations under the License.
-module(ncalendar_dom).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%% EXTERNAL EXPORTS
-export([
    format/0
]).

%%% MACROS
-define(OPTS_MS, #{precision => millisecond}).
-define(OPTS_EXT, #{extended => true}).
-define(OPTS_EXT_MS, #{precision => millisecond, extended => true}).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
format() ->
    triq_dom:oneof([iso8601, iso8601_ms, {iso8601, ?OPTS_MS}, {iso8601, ?OPTS_EXT}, {iso8601, ?OPTS_EXT_MS}]).
