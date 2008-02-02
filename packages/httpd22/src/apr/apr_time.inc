{ Copyright 2000-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{
 * @file apr_time.h
 * @brief APR Time Library
 }

{#include "apr.h"
#include "apr_pools.h"
#include "apr_errno.h"}

{
 * @defgroup apr_time Time Routines
 * @ingroup APR 
 }

{ month names }
//APR_DECLARE_DATA extern const char apr_month_snames[12][4];
{ day names }
//APR_DECLARE_DATA extern const char apr_day_snames[7][4];


{ number of microseconds since 00:00:00 january 1, 1970 UTC }
type
  apr_time_t = apr_int64_t;
  Papr_time_t = ^apr_time_t;


{ mechanism to properly type apr_time_t literals }
//#define APR_TIME_C(val) APR_INT64_C(val)

{ mechanism to properly print apr_time_t values }
//  APR_TIME_T_FMT = APR_INT64_T_FMT;

{ intervals for I/O timeouts, in microseconds }
  apr_interval_time_t = apr_int64_t;
  Papr_interval_time_t = ^apr_interval_time_t;
{ short interval for I/O timeouts, in microseconds }
  apr_short_interval_time_t = apr_int32_t;

{ number of microseconds per second }
//  APR_USEC_PER_SEC APR_TIME_C(1000000)

{ @return apr_time_t as a second }
//#define apr_time_sec(time) ((time) / APR_USEC_PER_SEC)

{ @return apr_time_t as a usec }
//#define apr_time_usec(time) ((time) % APR_USEC_PER_SEC)

{ @return apr_time_t as a msec }
//#define apr_time_msec(time) (((time) / 1000) % 1000)

{ @return apr_time_t as a msec }
//#define apr_time_as_msec(time) ((time) / 1000)

{ @return a second as an apr_time_t }
//#define apr_time_from_sec(sec) ((apr_time_t)(sec) * APR_USEC_PER_SEC)

{ @return a second and usec combination as an apr_time_t }
//#define apr_time_make(sec, usec) ((apr_time_t)(sec) * APR_USEC_PER_SEC \
//                                + (apr_time_t)(usec))

{
 * @return the current time
 }
function apr_time_now: apr_time_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_now' + LibSuff0;

{ @see apr_time_exp_t }
type
  Papr_time_exp_t = ^apr_time_exp_t;

{
 * a structure similar to ANSI struct tm with the following differences:
 *  - tm_usec isn't an ANSI field
 *  - tm_gmtoff isn't an ANSI field (it's a bsdism)
 }
  apr_time_exp_t = record
    { microseconds past tm_sec }
    tm_usec: apr_int32_t;
    { (0-61) seconds past tm_min }
    tm_sec: apr_int32_t;
    { (0-59) minutes past tm_hour }
    tm_min: apr_int32_t;
    { (0-23) hours past midnight }
    tm_hour: apr_int32_t;
    { (1-31) day of the month }
    tm_mday: apr_int32_t;
    { (0-11) month of the year }
    tm_mon: apr_int32_t;
    { year since 1900 }
    tm_year: apr_int32_t;
    { (0-6) days since sunday }
    tm_wday: apr_int32_t;
    { (0-365) days since jan 1 }
    tm_yday: apr_int32_t;
    { daylight saving time }
    tm_isdst: apr_int32_t;
    { seconds east of UTC }
    tm_gmtoff: apr_int32_t;
  end;

{
 * convert an ansi time_t to an apr_time_t
 * @param result the resulting apr_time_t
 * @param input the time_t to convert
 }
function apr_time_ansi_put(result: Papr_time_t;
 input: time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_ansi_put' + LibSuff8;

{
 * convert a time to its human readable components using an offset
 * from GMT
 * @param result the exploded time
 * @param input the time to explode
 * @param offs the number of seconds offset to apply
 }
function apr_time_exp_tz(result: Papr_time_exp_t;
 input: apr_time_t; offs: apr_int32_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_exp_tz' + LibSuff16;

{
 * convert a time to its human readable components in GMT timezone
 * @param result the exploded time
 * @param input the time to explode
 }
function apr_time_exp_gmt(result: Papr_time_exp_t;
 input: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_exp_gmt' + LibSuff12;

{
 * convert a time to its human readable components in local timezone
 * @param result the exploded time
 * @param input the time to explode
 }
function apr_time_exp_lt(result: Papr_time_exp_t;
 input: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_exp_lt' + LibSuff12;

{
 * Convert time value from human readable format to a numeric apr_time_t 
 * e.g. elapsed usec since epoch
 * @param result the resulting imploded time
 * @param input the input exploded time
 }
function apr_time_exp_get(result: Papr_time_t;
 input: Papr_time_exp_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_exp_get' + LibSuff8;

{
 * Convert time value from human readable format to a numeric apr_time_t that
 * always represents GMT
 * @param result the resulting imploded time
 * @param input the input exploded time
 }
function apr_time_exp_gmt_get(result: Papr_time_t;
 input: Papr_time_exp_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_exp_gmt_get' + LibSuff8;

{
 * Sleep for the specified number of micro-seconds.
 * @param t desired amount of time to sleep.
 * @warning May sleep for longer than the specified time. 
 }
procedure apr_sleep(t: apr_interval_time_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_sleep' + LibSuff8;

{ length of a RFC822 Date }
const APR_RFC822_DATE_LEN = (30);
{
 * apr_rfc822_date formats dates in the RFC822
 * format in an efficient manner.  It is a fixed length
 * format which requires the indicated amount of storage,
 * including the trailing null byte.
 * @param date_str String to write to.
 * @param t the time to convert 
 }
function apr_rfc822_date(date_str: PChar; t: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_rfc822_date' + LibSuff12;

{ length of a CTIME date }
const APR_CTIME_LEN = (25);
{
 * apr_ctime formats dates in the ctime() format
 * in an efficient manner.  it is a fixed length format
 * and requires the indicated amount of storage including
 * the trailing null byte.
 * Unlike ANSI/ISO C ctime(), apr_ctime() does not include
 * a \n at the end of the string.
 * @param date_str String to write to.
 * @param t the time to convert 
 }
function apr_ctime(date_str: PChar; t: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_ctime' + LibSuff12;

{
 * formats the exploded time according to the format specified
 * @param s string to write to
 * @param retsize The length of the returned string
 * @param max The maximum length of the string
 * @param format The format for the time string
 * @param tm The time to convert
 }
function apr_strftime(s: PChar; retsize: apr_size_t;
 max: apr_size_t; const format: PChar;
 tm: Papr_time_exp_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_strftime' + LibSuff20;

{
 * Improve the clock resolution for the lifetime of the given pool.
 * Generally this is only desireable on benchmarking and other very
 * time-sensitive applications, and has no impact on most platforms.
 * @param p The pool to associate the finer clock resolution 
 }
procedure apr_time_clock_hires(p: Papr_pool_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_time_clock_hires' + LibSuff4;

