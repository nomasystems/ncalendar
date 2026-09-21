-module(ncalendar_dom).

-moduledoc "triq generators for the `ncalendar` property tests.".

%%%-----------------------------------------------------------------------------
%% INCLUDE FILES
%%%-----------------------------------------------------------------------------
-include_lib("triq/include/triq.hrl").
-include_lib("ncalendar/src/ncalendar.hrl").

%%%-----------------------------------------------------------------------------
%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-export([
    format/0,
    opts/1,
    timezone/0
]).

%%%-----------------------------------------------------------------------------
%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
format() ->
    triq_dom:oneof([iso8601, http_date, imf_fixdate]).

opts(iso8601) ->
    ?LET(
        {
            Precision,
            Extended
        },
        {
            triq_dom:elements([millisecond, undefined]),
            triq_dom:bool()
        },
        #{
            precision => Precision,
            extended => Extended
        }
    );
opts(_Format) ->
    triq_dom:return(#{}).

timezone() ->
    triq_dom:elements([undefined | ?TIMEZONES]).
