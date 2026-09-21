-module(ncalendar_properties).

-moduledoc "Properties for the `ncalendar` API, checked with triq.".

%%%-----------------------------------------------------------------------------
%% INCLUDE FILES
%%%-----------------------------------------------------------------------------
-include_lib("triq/include/triq.hrl").

%%%-----------------------------------------------------------------------------
%% PROPERTIES
%%%-----------------------------------------------------------------------------
-export([prop_there_and_back_again/0]).

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
