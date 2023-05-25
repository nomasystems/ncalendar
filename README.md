# ncalendar

![ncalendar](https://github.com/nomasystems/ncalendar/actions/workflows/ci.yml/badge.svg)

`ncalendar` is an OTP library for the manipulation of a binary representation of dates and times.

## Setup

Add `ncalendar` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {ncalendar, {git, "git@github.com:nomasystems/ncalendar.git", {tag, "0.2.0"}}}
]}.
```

## API

`ncalendar` exposes its functionalities via the following API:

| Function | Description |
| --------  | ------------ |
| `ncalendar:convert/3` | Converts a binary representation of a datetime in a given format to the specified target format |
| `ncalendar:convert/4` | Converts a binary representation of a datetime in a given format to the specified target format and format opts |
| `ncalendar:from_datetime/2` | Generates a binary representation of the given `calendar:datetime()` value in the given format |
| `ncalendar:from_datetime/3` | Generates a binary representation of the given `calendar:datetime()` value in the given format and format opts|
| `ncalendar:from_gregorian_seconds/2` | Generates a binary representation of the given number of gregorian seconds in the given format |
| `ncalendar:from_gregorian_seconds/3` | Generates a binary representation of the given number of gregorian seconds in the given format and format opts |
| `ncalendar:from_posix_time/2` | Generates a binary representation of the given number of seconds since Epoch in the given format |
| `ncalendar:from_posix_time/3` | Generates a binary representation of the given number of seconds since Epoch in the given format and format opts |
| `ncalendar:from_timestamp/2` | Generates a binary representation of the given `erlang:timestamp()` value in the given format |
| `ncalendar:from_timestamp/3` | Generates a binary representation of the given `erlang:timestamp()` value in the given format and format opts |
| `ncalendar:is_valid/2` | Checks the validity of a binary representation of a datetime with respect to the specified format |
| `ncalendar:is_valid/3` | Checks the validity of a binary representation of a datetime with respect to the specified format and format opts |
| `ncalendar:now/1` | Generates a binary representation of the current datetime with no UTC offset in the given format |
| `ncalendar:now/2` | Generates a binary representation of the current datetime in the given format and timezone |
| `ncalendar:now/3` | Generates a binary representation of the current datetime in the given format, format opts and timezone |
| `ncalendar:timezone/2` | Returns the timezone of the given binary representation and format |
| `ncalendar:timezone/3` | Returns the timezone of the given binary representation, format and format opts |
| `ncalendar:to_datetime/2` | Converts a binary representation of a datetime in the given format to a `calendar:datetime()` value |
| `ncalendar:to_gregorian_seconds/2` | Converts a binary representation of a datetime in the given format to the integer value of gregorian seconds |
| `ncalendar:to_posix_time/2` | Converts a binary representation of a datetime in the given format to a POSIX time value (seconds since Epoch) |
| `ncalendar:to_timestamp/2` | Converts a binary representation of a datetime in the given format to an `erlang:timestamp()` value |

## Supported formats

<table>
<tr>
<td>Format</td>
<td>Options</td>
</tr>
<tr>
<td><code>iso8601</code></td>
<td>

```erl
#{
    precision => millisecond,
    extended => boolean()
}
```
</td>
</tr>
<tr>
<td><code>rfc2109</code></td>
<td>

```erl
#{}
```
</td>
</tr>
<tr>
<td><code>imf_fixdate</code></td>
<td>

```erl
#{}
```
</td>
</tr>
</table>

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/ncalendar/issues).
