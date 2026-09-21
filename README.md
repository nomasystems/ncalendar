# ncalendar

![ncalendar](https://github.com/nomasystems/ncalendar/actions/workflows/ci.yml/badge.svg)

`ncalendar` is an OTP library for the manipulation of a binary representation of dates and times.

## Setup

Add `ncalendar` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {ncalendar, {git, "git@github.com:nomasystems/ncalendar.git", {tag, "0.3.1"}}}
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
| `ncalendar:to_datetime/2` | Converts a binary representation of a datetime in the given format to a `calendar:datetime()` value |
| `ncalendar:to_gregorian_seconds/2` | Converts a binary representation of a datetime in the given format to the integer value of gregorian seconds |
| `ncalendar:to_posix_time/2` | Converts a binary representation of a datetime in the given format to a POSIX time value (seconds since Epoch) |
| `ncalendar:to_timestamp/2` | Converts a binary representation of a datetime in the given format to an `erlang:timestamp()` value |
| `ncalendar:timezones/0` | Returns the list of timezones as UTC offsets |

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
<td><code>netscape_cookie_date</code></td>
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

### Which one to use

| Format | Looks like | Where it comes from |
| ------ | ---------- | ------------------- |
| `iso8601` | `20140519T100000Z`, or `2014-05-19T10:00:00Z` with `extended` | [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html). The default choice for storing and exchanging timestamps |
| `imf_fixdate` | `Mon, 19 May 2014 10:00:00 GMT` | IMF-fixdate, the only form [RFC 9110 §5.6.7](https://www.rfc-editor.org/rfc/rfc9110#section-5.6.7) allows a sender to generate in an HTTP header field such as `Date` or `Last-Modified`, and the one [RFC 6265 §4.1.1](https://www.rfc-editor.org/rfc/rfc6265#section-4.1.1) asks for in a cookie `Expires` |
| `netscape_cookie_date` | `Mon, 19-May-2014 10:00:00 GMT` | The `Expires` date of Netscape's original cookie proposal. No RFC defines it: [RFC 2109 §10.1.2](https://www.rfc-editor.org/rfc/rfc2109#section-10.1.2) only records the shape as a note on that proposal, and [RFC 6265 §5.1.1](https://www.rfc-editor.org/rfc/rfc6265#section-5.1.1) still parses it. Use it to read an old cookie, not to write a new date |

`netscape_cookie_date` is not an HTTP-date. RFC 9110 admits IMF-fixdate, the RFC 850
form (full day name, two digit year) and asctime, and the hyphenated form
with an abbreviated day name and a four digit year is none of the three.

If you come from cowboy, this is the shape that `cow_date:rfc2109/1` writes
and `cow_cookie` puts in an `Expires` attribute. The `rfc2109` label is
cowlib's, and it is what this format was called here until 1.0.0.

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/ncalendar/issues).
