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
-module(ncalendar_SUITE).

-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(OPTS_MS, #{precision => millisecond}).
-define(OPTS_EXT, #{extended => true}).
-define(OPTS_EXT_MS, #{precision => millisecond, extended => true}).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, properties},
        convert,
        convert_without_tz,
        datetime,
        gregorian_seconds,
        now,
        shift_timezone,
        timestamp,
        timezone,
        posix_time
    ].

groups() ->
    [
        {properties, [parallel], [
            there_and_back_again
        ]}
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
there_and_back_again(Conf) ->
    ct_property_test:quickcheck(
        ncalendar_properties:prop_there_and_back_again(),
        Conf
    ).

convert(_Conf) ->
    ISO8601 = <<"20140519T100000+0100">>,
    ISO8601Ms = <<"20140519T100000.000+0100">>,
    ISO8601Ext = <<"2014-05-19T10:00:00+0100">>,
    ISO8601ExtMs = <<"2014-05-19T10:00:00.000+0100">>,
    HTTPDate = <<"Mon, 19-May-2014 09:00:00 GMT">>,
    IMFFixdate = <<"Mon, 19 May 2014 10:00:00 +0100">>,

    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601Ms),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601Ms, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601Ms, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601Ms),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601Ms),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601Ext),
    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601Ext, ?OPTS_MS),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601Ext, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601Ext),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601Ext),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs),
    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs, ?OPTS_EXT),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601ExtMs),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601ExtMs).

convert_without_tz(_Conf) ->
    ISO8601 = <<"20140519T100000Z">>,
    ISO8601Ms = <<"20140519T100000.000Z">>,
    ISO8601Ext = <<"2014-05-19T10:00:00Z">>,
    ISO8601ExtMs = <<"2014-05-19T10:00:00.000Z">>,
    HTTPDate = <<"Mon, 19-May-2014 10:00:00 GMT">>,
    IMFFixdate = <<"Mon, 19 May 2014 10:00:00 GMT">>,

    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601Ms),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601Ms, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601Ms, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601Ms),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601Ms),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601Ext),
    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601Ext, ?OPTS_MS),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601Ext, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601Ext),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601Ext),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs),
    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs, ?OPTS_EXT),
    HTTPDate = ncalendar:convert(iso8601, http_date, ISO8601ExtMs),
    IMFFixdate = ncalendar:convert(iso8601, imf_fixdate, ISO8601ExtMs),

    ISO8601 = ncalendar:convert(http_date, iso8601, HTTPDate),
    ISO8601Ms = ncalendar:convert(http_date, iso8601, HTTPDate, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(http_date, iso8601, HTTPDate, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(http_date, iso8601, HTTPDate, ?OPTS_EXT_MS),
    IMFFixdate = ncalendar:convert(http_date, imf_fixdate, HTTPDate),

    ISO8601 = ncalendar:convert(imf_fixdate, iso8601, IMFFixdate),
    ISO8601Ms = ncalendar:convert(imf_fixdate, iso8601, IMFFixdate, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(imf_fixdate, iso8601, IMFFixdate, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(imf_fixdate, iso8601, IMFFixdate, ?OPTS_EXT_MS),
    HTTPDate = ncalendar:convert(imf_fixdate, http_date, IMFFixdate).

datetime(_Conf) ->
    Datetime = calendar:universal_time(),
    Bin = ncalendar:from_datetime(iso8601, Datetime),
    Datetime = ncalendar:to_datetime(iso8601, Bin),

    ISO8601UTC = <<"20140519T100000Z">>,
    ISO8601 = <<"20140519T100000+1000">>,
    UTC = ncalendar:to_datetime(iso8601, ISO8601UTC),
    Zone = ncalendar:to_datetime(iso8601, ISO8601),
    ?assertEqual(UTC, Zone).

gregorian_seconds(_Conf) ->
    Datetime = calendar:universal_time(),
    GregorianSeconds = calendar:datetime_to_gregorian_seconds(Datetime),
    Bin = ncalendar:from_gregorian_seconds(iso8601, GregorianSeconds),
    GregorianSeconds = ncalendar:to_gregorian_seconds(iso8601, Bin),

    ISO8601UTC = <<"20140519T100000Z">>,
    ISO8601 = <<"20140519T100000+1000">>,
    UTC = ncalendar:to_gregorian_seconds(iso8601, ISO8601UTC),
    Zone = ncalendar:to_gregorian_seconds(iso8601, ISO8601),
    ?assertNotEqual(UTC, Zone).

now(_Conf) ->
    DateTime = calendar:now_to_datetime(erlang:timestamp()),
    Iso8601 = ncalendar:now(iso8601),
    CalendarSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    Iso8601Seconds = ncalendar:to_gregorian_seconds(iso8601, Iso8601),
    1 >= Iso8601Seconds - CalendarSeconds.

timestamp(_Conf) ->
    Timestamp = erlang:timestamp(),
    Bin = ncalendar:from_timestamp(iso8601, Timestamp),
    {MSecs, Secs, _MicroSecs1} = Timestamp,
    {MSecs, Secs, 0} = ncalendar:to_timestamp(iso8601, Bin),

    ISO8601UTC = <<"20140519T100000Z">>,
    ISO8601 = <<"20140519T100000+1000">>,
    UTC = ncalendar:to_timestamp(iso8601, ISO8601UTC),
    Zone = ncalendar:to_timestamp(iso8601, ISO8601),
    ?assertNotEqual(UTC, Zone).

shift_timezone(_Conf) ->
    ISO8601 = <<"20140519T100000+1000">>,
    ISO8601Ms = <<"20140519T100000.000+1000">>,
    ISO8601Ext = <<"2014-05-19T10:00:00-0700">>,
    ISO8601ExtMs = <<"2014-05-19T10:00:00.000Z">>,
    HTTPDate = <<"Mon, 19-May-2014 10:00:00 GMT">>,
    IMFFixdate = <<"Mon, 19 May 2014 10:00:00 GMT">>,

    <<"20140519T000000Z">> = ncalendar:shift_timezone(iso8601, ISO8601, 0),
    <<"20140518T140000.000-1000">> = ncalendar:shift_timezone(iso8601, ISO8601Ms, -1000, ?OPTS_MS),
    <<"2014-05-20T03:00:00+1000">> = ncalendar:shift_timezone(iso8601, ISO8601Ext, 1000, ?OPTS_EXT),
    <<"2014-05-19T22:00:00.000+1200">> = ncalendar:shift_timezone(
        iso8601, ISO8601ExtMs, 1200, ?OPTS_EXT_MS
    ),
    <<"Mon, 19-May-2014 10:00:00 GMT">> = ncalendar:shift_timezone(http_date, HTTPDate, 0),
    <<"Mon, 19 May 2014 12:00:00 +0200">> = ncalendar:shift_timezone(imf_fixdate, IMFFixdate, 200).

timezone(_Conf) ->
    ISO8601 = <<"20140519T100000">>,
    ISO8601Ms = <<"20140519T100000.000+1000">>,
    ISO8601Ext = <<"2014-05-19T10:00:00-0700">>,
    ISO8601ExtMs = <<"2014-05-19T10:00:00.000Z">>,
    HTTPDate = <<"Mon, 19-May-2014 10:00:00 GMT">>,
    IMFFixdate = <<"Mon, 19 May 2014 10:00:00 GMT">>,

    undefined = ncalendar:timezone(iso8601, ISO8601),
    1000 = ncalendar:timezone(iso8601, ISO8601Ms),
    -0700 = ncalendar:timezone(iso8601, ISO8601Ext),
    0000 = ncalendar:timezone(iso8601, ISO8601ExtMs),
    0000 = ncalendar:timezone(http_date, HTTPDate),
    0000 = ncalendar:timezone(imf_fixdate, IMFFixdate).

posix_time(_Conf) ->
    UnixTime = os:system_time(second),
    Bin = ncalendar:from_posix_time(iso8601, UnixTime),
    UnixTime = ncalendar:to_posix_time(iso8601, Bin).
