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
-module(ncalendar_http_date_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(DATETIMEZONE_1, {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, <<"GMT">>}).
-define(DATETIMEZONE_2, {{{2014, 5, 19}, {12, 0, 0}}, {millisecond, 30}, +0000}).
-define(DATETIMEZONE_3, {{{2014, 5, 31}, {21, 0, 0}}, {millisecond, 30}, +1100}).
-define(OPTS_DEFAULT, #{}).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        from_datetimezone,
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
from_datetimezone(_Conf) ->
    Opts = ?OPTS_DEFAULT,
    <<"Mon, 19-May-2014 10:00:00 GMT">> = ncalendar_http_date:from_datetimezone(
        ?DATETIMEZONE_1, Opts
    ),
    <<"Mon, 19-May-2014 12:00:00 GMT">> = ncalendar_http_date:from_datetimezone(
        ?DATETIMEZONE_2, Opts
    ),
    <<"Sat, 31-May-2014 21:00:00 GMT">> = ncalendar_http_date:from_datetimezone(
        ?DATETIMEZONE_3, Opts
    ).

is_valid(_Conf) ->
    Opts = ?OPTS_DEFAULT,
    true = ncalendar_http_date:is_valid(<<"Mon, 19-May-2014 10:00:00 GMT">>, Opts),
    true = ncalendar_http_date:is_valid(<<"Mon, 19-May-2014 12:00:00 GMT">>, Opts),
    true = ncalendar_http_date:is_valid(<<"Sun, 01-Jun-2014 08:00:00 GMT">>, Opts),

    false = ncalendar_http_date:is_valid(<<"Mon, 01-Jun-2014 21:00:00 GMT">>, Opts).

to_datetimezone(_Conf) ->
    Date1 = {{{2014, 5, 19}, {10, 0, 0}}, {millisecond, 0}, +0000},
    Date1 = ncalendar_http_date:to_datetimezone(<<"Mon, 19-May-2014 10:00:00 GMT">>).
