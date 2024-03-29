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
-module(ncalendar_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%% EXPORTS
-compile([export_all, nowarn_export_all]).

prop_there_and_back_again() ->
    ?FORALL(
        {Format, Opts, Tz},
        ?LET(
            {
                Format, 
                Tz
            },
            {
                ncalendar_dom:format(),
                ncalendar_dom:timezone()
            },
            {
                Format,
                ncalendar_dom:opts(Format),
                Tz
            }
        ),
        begin
            Now = ncalendar:now(Format, Tz, Opts),
            true = ncalendar:is_valid(Format, Now, Opts),
            Now = 
                case Tz of
                    undefined ->
                        ncalendar:convert(Format, Format, Now, Opts);
                    _Otherwise ->
                        ncalendar:shift_timezone(Format, ncalendar:convert(Format, Format, Now, Opts), Tz, Opts)
                end,
            true
        end
    ).
