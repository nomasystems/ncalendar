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
        datetime,
        gregorian_seconds,
        timestamp
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
    ISO8601 = <<"20140519T100000Z">>,
    ISO8601Ms = <<"20140519T100000.000Z">>,
    ISO8601Ext = <<"2014-05-19T10:00:00Z">>,
    ISO8601ExtMs = <<"2014-05-19T10:00:00.000Z">>,

    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601, ?OPTS_EXT_MS),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601Ms),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601Ms, ?OPTS_EXT),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601Ms, ?OPTS_EXT_MS),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601Ext),
    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601Ext, ?OPTS_MS),
    ISO8601ExtMs = ncalendar:convert(iso8601, iso8601, ISO8601Ext, ?OPTS_EXT_MS),

    ISO8601 = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs),
    ISO8601Ms = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs, ?OPTS_MS),
    ISO8601Ext = ncalendar:convert(iso8601, iso8601, ISO8601ExtMs, ?OPTS_EXT).

datetime(_Conf) ->
    Datetime = calendar:universal_time(),
    Bin = ncalendar:from_datetime(iso8601, Datetime),
    Datetime = ncalendar:to_datetime(iso8601, Bin).

gregorian_seconds(_Conf) ->
    Datetime = calendar:universal_time(),
    GregorianSeconds = calendar:datetime_to_gregorian_seconds(Datetime),
    Bin = ncalendar:from_gregorian_seconds(iso8601, GregorianSeconds),
    GregorianSeconds = ncalendar:to_gregorian_seconds(iso8601, Bin).

timestamp(_Conf) ->
    Timestamp = erlang:timestamp(),
    Bin = ncalendar:from_timestamp(iso8601, Timestamp),
    {MSecs, Secs, _MicroSecs1} = Timestamp,
    {MSecs, Secs, 0} = ncalendar:to_timestamp(iso8601, Bin).
