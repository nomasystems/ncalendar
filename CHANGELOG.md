# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- The `http_date` format is now `netscape_cookie_date`, and its module
  `ncalendar_http_date` is now `ncalendar_netscape_cookie_date`. It writes
  `Mon, 19-May-2014 10:00:00 GMT`, the `Expires` date of Netscape's original
  cookie proposal, which is not an HTTP-date: RFC 9110 Section 5.6.7 allows
  only IMF-fixdate, the RFC 850 form and asctime, so the old name sent
  callers to the wrong format for an HTTP header field. Use `imf_fixdate`
  there. `rfc2109` was dropped as a name because that RFC does not define
  the shape, it only notes it in Section 10.1.2, and `cookie_date` because
  RFC 6265 Section 4.1.1 also asks a sender for an IMF-fixdate
- Requires Erlang/OTP 27 or newer, for `-moduledoc` and `-doc`. OTP 25 and 26
  are no longer supported

### Removed

- The `http_date` format atom. Use `netscape_cookie_date` to read the
  `Expires` of an old cookie, and `imf_fixdate` to write a date into an HTTP
  header field. A call with `http_date` now throws
  `{error, ncalendar, {unsupported_format, http_date}}`

## [0.3.1] - 2024-03-15

### Fixed

- `to_timestamp/2` and `to_gregorian_seconds/2` no longer apply the UTC offset
  twice, so a value with an offset converts to the instant it names

## [0.3.0] - 2023-05-27

### Added

- `imf_fixdate` format, `Mon, 19 May 2014 10:00:00 GMT`
- `timezone/2`, which reads the offset of a value
- `from_posix_time/2,3` and `to_posix_time/2`
- `timezones/0`, the list of supported offsets
- `shift_timezone/3,4`, which rewrites a value in another offset

### Changed

- The offset is optional in `iso8601`, so a value with no offset reads as
  `undefined` instead of failing

## [0.2.0] - 2023-04-17

### Added

- Subsecond precision with `#{precision => millisecond}`
- Extended `iso8601` with `#{extended => true}`, `2014-05-19T10:00:00Z`
- `http_date` format, `Mon, 19-May-2014 10:00:00 GMT`
- Conversions to and from `calendar:datetime()`, `erlang:timestamp()` and
  gregorian seconds

### Fixed

- Accept `Z` as the zulu time designator in `iso8601`
- Pad the year to four digits in `iso8601`
- `now/1,2` returns the current date. It read the value of
  `erlang:system_time(millisecond)` as gregorian milliseconds, so the answer
  fell 1970 years in the past

## [0.1.0] - 2022-08-09

Initial release.

### Added

- `iso8601` format, `20140519T100000Z`
