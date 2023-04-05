# ncalendar

![ncalendar](https://github.com/nomasystems/ncalendar/actions/workflows/build.yml/badge.svg)

`ncalendar` is an OTP library for the manipulation of a binary representation of dates and times.

## Setup

Add `ncalendar` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {ncalendar, {git, "git@github.com:nomasystems/ncalendar.git", {tag, "0.1.0"}}}
]}.
```

## API

`ncalendar` exposes its functionalities via the following API:

| Function | Description |
| --------  | ------------ |
| `ncalendar:convert/3` | Converts a binary representation of a datetime in a given format to the specified target format |
| `ncalendar:from_datetime/2` | Generates a binary representation of the given `calendar:datetime()` value in the given format |
| `ncalendar:from_gregorian_seconds/2` | Generates a binary representation of the given number of gregorian seconds in the given format |
| `ncalendar:from_timestamp/2` | Generates a binary representation of the given `erlang:timestamp()` value in the given format |
| `ncalendar:is_valid/2` | Checks the validity of a binary representation of a datetime with respect to the specified format |
| `ncalendar:now/1` | Generates a binary representation of the current datetime with no UTC offset in the given format |
| `ncalendar:now/2` | Generates a binary representation of the current datetime in the given format and timezone |
| `ncalendar:to_datetime/2` | Converts a binary representation of a datetime in the given format to a `calendar:datetime()` value |
| `ncalendar:to_gregorian_seconds/2` | Converts a binary representation of a datetime in the given format to the integer value of gregorian seconds |
| `ncalendar:to_timestamp/2` | Converts a binary representation of a datetime in the given format to a `erlang:timestamp()` value |

## Supported formats

Currently, `ncalendar` supports the following formats:
- `iso8601`
- `iso8601_milliseconds`

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/ncalendar/issues).
