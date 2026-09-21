-module(ncalendar_format).

-moduledoc false.

%%%-----------------------------------------------------------------------------
%% TYPES
%%%-----------------------------------------------------------------------------
-doc """
A datetime, its subsecond part, and the timezone it was read from or is
written to.

The datetime is always UTC, except when the timezone is `undefined`, where
it is the wall clock of the value itself.
""".
-type datetimezone() :: {
    calendar:datetime(), sub_seconds(), ncalendar:timezone() | ncalendar:timezone_alias()
}.

-type milliseconds() :: non_neg_integer().

-type sub_seconds() :: {millisecond, milliseconds()}.

%%%-----------------------------------------------------------------------------
%% TYPE EXPORTS
%%%-----------------------------------------------------------------------------
-export_type([
    datetimezone/0,
    milliseconds/0,
    sub_seconds/0
]).

%%%-----------------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-doc "Converts a `t:datetimezone/0` value to a binary in the implementer format.".
-callback from_datetimezone(Datetimezone, Opts) -> Result when
    Datetimezone :: datetimezone(),
    Opts :: map(),
    Result :: ncalendar:value().

-doc "Checks if a value is a valid datetime in the implementer format.".
-callback is_valid(Value, Opts) -> Result when
    Value :: ncalendar:value(),
    Opts :: map(),
    Result :: boolean().

-doc "Converts a binary in the implementer format to a `t:datetimezone/0` value.".
-callback to_datetimezone(Value) -> Result when
    Value :: ncalendar:value(),
    Result :: datetimezone().
