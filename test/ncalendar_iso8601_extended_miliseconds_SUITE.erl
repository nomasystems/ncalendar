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
-module(ncalendar_iso8601_extended_miliseconds_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        from_datetime_zone,
        is_valid,
        to_datetimezone
    ].

suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
from_datetime_zone() ->
    [{userdata, [{doc, "Tests transform datetime to iso8601"}]}].

from_datetime_zone(_Conf) ->
    <<"2014-05-19T10:00:00.000Z">> = ncalendar_iso8601_extended_miliseconds:from_datetimezone({
        {{2014, 5, 19}, {10, 0, 0}}, <<"Z">>
    }),
    <<"2014-05-19T12:00:00.000Z">> = ncalendar_iso8601_extended_miliseconds:from_datetimezone({
        {{2014, 5, 19}, {12, 0, 0}}, +0000
    }),
    <<"2014-06-01T08:00:00.000+1100">> = ncalendar_iso8601_extended_miliseconds:from_datetimezone({
        {{2014, 5, 31}, {21, 0, 0}}, +1100
    }).

is_valid() ->
    [{userdata, [{doc, "Tests is a binary valid iso8601"}]}].

is_valid(_Conf) ->
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00Z">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00+0300">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00-0230">>),
    true = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00.000Z">>),
    true = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00.000+0300">>),
    true = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00.000-0230">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2015-11-31T10:00:00Z">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T24:00:00+0300">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00-3430">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2015-11-31T10:00:00.000Z">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T24:00:00.000+0300">>),
    false = ncalendar_iso8601_extended_miliseconds:is_valid(<<"2014-05-19T10:00:00.000-3430">>).

to_datetimezone() ->
    [{userdata, [{doc, "Tests is a binary valid iso8601"}]}].

to_datetimezone(_Conf) ->
    {{{2014, 5, 19}, {10, 0, 0}}, +0000} = ncalendar_iso8601_extended:to_datetimezone(
        <<"2014-05-19T10:00:00.000Z">>
    ),
    {{{2014, 5, 19}, {7, 0, 0}}, +0300} = ncalendar_iso8601_extended:to_datetimezone(
        <<"2014-05-19T10:00:00.000+0300">>
    ),
    {{{2014, 5, 19}, {12, 30, 0}}, -0230} = ncalendar_iso8601_extended:to_datetimezone(
        <<"2014-05-19T10:00:00.000-0230">>
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
