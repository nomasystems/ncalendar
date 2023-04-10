%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
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
-module(ncalendar_iso8601_SUITE).

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
from_datetime_zone(_Conf) ->
    <<"20140519T100000Z">> = ncalendar_iso8601:from_datetimezone({
        {{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, <<"Z">>
    }),
    <<"20140519T120000Z">> = ncalendar_iso8601:from_datetimezone({
        {{2014, 5, 19}, {12, 0, 0}}, {millisecond, 30}, +0000
    }),
    <<"20140601T080000+1100">> = ncalendar_iso8601:from_datetimezone({
        {{2014, 5, 31}, {21, 0, 0}}, {millisecond, 0}, +1100
    }).

is_valid(_Conf) ->
    true = ncalendar_iso8601:is_valid(<<"20140519T100000Z">>),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000+0300">>),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000-0230">>),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.000Z">>),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.000+0300">>),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.000-0230">>),
    false = ncalendar_iso8601:is_valid(<<"20151131T100000Z">>),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000+0300">>),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-3430">>),
    false = ncalendar_iso8601:is_valid(<<"20151131T100000.000Z">>),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000.000+0300">>),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-3430">>).

to_datetimezone(_Conf) ->
    {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, +0000} = ncalendar_iso8601:to_datetimezone(
        <<"20140519T100000Z">>
    ),
    {{{2014, 5, 19}, {7, 0, 0}}, {millisecond, 0}, +0300} = ncalendar_iso8601:to_datetimezone(
        <<"20140519T100000+0300">>
    ),
    {{{2014, 5, 19}, {12, 30, 0}}, {millisecond, 0}, -0230} = ncalendar_iso8601:to_datetimezone(
        <<"20140519T100000-0230">>
    ),
    {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, +0000} = ncalendar_iso8601:to_datetimezone(
        <<"20140519T100000.000Z">>
    ),
    {{{2014, 5, 19}, {7, 0, 0}}, {millisecond, 0}, +0300} = ncalendar_iso8601:to_datetimezone(
        <<"20140519T100000.000+0300">>
    ),
    {{{2014, 5, 19}, {12, 30, 0}}, {millisecond, 0}, -0230} = ncalendar_iso8601:to_datetimezone(
        <<"20140519T100000.030-0230">>
    ).
