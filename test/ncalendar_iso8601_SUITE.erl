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

%%% MACROS
-define(DATETIMEZONE_1, {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, <<"Z">>}).
-define(DATETIMEZONE_2, {{{2014, 5, 19}, {12, 0, 0}}, {millisecond, 30}, +0000}).
-define(DATETIMEZONE_3, {{{2014, 5, 31}, {21, 0, 0}}, {millisecond, 30}, +1100}).
-define(OPTS_DEFAULT, #{}).
-define(OPTS_MS, #{precision => millisecond}).
-define(OPTS_EXT, #{extended => true}).
-define(OPTS_EXT_MS, #{precision => millisecond, extended => true}).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        from_datetimezone,
        from_datetimezone_ms,
        from_datetimezone_ext,
        from_datetimezone_ext_ms,
        is_valid,
        is_valid_ms,
        is_valid_ext,
        is_valid_ext_ms,
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
from_datetimezone(_Conf) ->
    Opts = ?OPTS_DEFAULT,
    <<"20140519T100000Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_1, Opts),
    <<"20140519T120000Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_2, Opts),
    <<"20140601T080000+1100">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_3, Opts).

from_datetimezone_ms(_Conf) ->
    Opts = ?OPTS_MS,
    <<"20140519T100000.000Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_1, Opts),
    <<"20140519T120000.030Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_2, Opts),
    <<"20140601T080000.030+1100">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_3, Opts).

from_datetimezone_ext(_Conf) ->
    Opts = ?OPTS_EXT,
    <<"2014-05-19T10:00:00Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_1, Opts),
    <<"2014-05-19T12:00:00Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_2, Opts),
    <<"2014-06-01T08:00:00+1100">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_3, Opts).

from_datetimezone_ext_ms(_Conf) ->
    Opts = ?OPTS_EXT_MS,
    <<"2014-05-19T10:00:00.000Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_1, Opts),
    <<"2014-05-19T12:00:00.030Z">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_2, Opts),
    <<"2014-06-01T08:00:00.030+1100">> = ncalendar_iso8601:from_datetimezone(?DATETIMEZONE_3, Opts).

is_valid(_Conf) ->
    Opts = ?OPTS_DEFAULT,
    true = ncalendar_iso8601:is_valid(<<"20140519T100000Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"20140519T100000.030Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.030+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.000-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-3430">>, Opts),
    
    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-3430">>, Opts).

is_valid_ms(_Conf) ->
    Opts = ?OPTS_MS,
    false = ncalendar_iso8601:is_valid(<<"20140519T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"20140519T100000.030Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.030+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"20140519T100000.000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-3430">>, Opts),
    
    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-3430">>, Opts).

is_valid_ext(_Conf) ->
    Opts = ?OPTS_EXT,
    false = ncalendar_iso8601:is_valid(<<"20140519T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20140519T100000.030Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.030+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-3430">>, Opts),
    
    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-3430">>, Opts).

is_valid_ext_ms(_Conf) ->
    Opts = ?OPTS_EXT_MS,
    false = ncalendar_iso8601:is_valid(<<"20140519T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20140519T100000.030Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.030+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-0230">>, Opts),

    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000Z">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000+0300">>, Opts),
    true = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-0230">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"20151131T100000.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T240000.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"20140519T100000.000-3430">>, Opts),

    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00-3430">>, Opts),
    
    false = ncalendar_iso8601:is_valid(<<"2015-11-31T10:00:00.000Z">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T24:00:00.000+0300">>, Opts),
    false = ncalendar_iso8601:is_valid(<<"2014-05-19T10:00:00.000-3430">>, Opts).

to_datetimezone(_Conf) ->
    Date1 = {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, +0000},
    Date1 = ncalendar_iso8601:to_datetimezone(<<"20140519T100000Z">>),
    Date1 = ncalendar_iso8601:to_datetimezone(<<"2014-05-19T10:00:00Z">>),

    Date2 = {{{2014, 5, 19}, {7, 0, 0}}, {millisecond, 0}, +0300},
    Date2 = ncalendar_iso8601:to_datetimezone(<<"20140519T100000+0300">>),
    Date2 = ncalendar_iso8601:to_datetimezone(<<"2014-05-19T10:00:00+0300">>),

    Date3 = {{{2014, 5, 19}, {12, 30, 0}}, {millisecond, 0}, -0230},
    Date3 = ncalendar_iso8601:to_datetimezone(<<"20140519T100000-0230">>),
    Date3 = ncalendar_iso8601:to_datetimezone(<<"2014-05-19T10:00:00-0230">>),

    Date4 = {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 30}, +0000},
    Date4 = ncalendar_iso8601:to_datetimezone(<<"20140519T100000.030Z">>),
    Date4 = ncalendar_iso8601:to_datetimezone(<<"2014-05-19T10:00:00.030Z">>),

    Date5 = {{{2014, 5, 19}, {7, 0, 0}}, {millisecond, 30}, +0300},
    Date5 = ncalendar_iso8601:to_datetimezone(<<"20140519T100000.030+0300">>),
    Date5 = ncalendar_iso8601:to_datetimezone(<<"2014-05-19T10:00:00.030+0300">>),

    Date6 = {{{2014, 5, 19}, {12, 30, 0}}, {millisecond, 30}, -0230},
    Date6 = ncalendar_iso8601:to_datetimezone(<<"20140519T100000.030-0230">>),
    Date6 = ncalendar_iso8601:to_datetimezone(<<"2014-05-19T10:00:00.030-0230">>).
