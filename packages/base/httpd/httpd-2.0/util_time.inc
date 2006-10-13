{ Copyright 2001-2005 The Apache Software Foundation or its licensors, as
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

{#include "apr.h"
#include "apr_time.h"
#include "httpd.h"}

{
 * @package Apache date-time handling functions
 }

{ Maximum delta from the current time, in seconds, for a past time
 * to qualify as "recent" for use in the ap_explode_recent_*() functions:
 * (Must be a power of two minus one!)
 }
const AP_TIME_RECENT_THRESHOLD = 15;

{
 * convert a recent time to its human readable components in local timezone
 * @param tm the exploded time
 * @param t the time to explode: MUST be within the last
 *          AP_TIME_RECENT_THRESHOLD seconds
 * @note This is a faster alternative to apr_explode_localtime that uses
 *       a cache of pre-exploded time structures.  It is useful for things
 *       that need to explode the current time multiple times per second,
 *       like loggers.
 * @return APR_SUCCESS iff successful
 }
function ap_explode_recent_localtime(tm: Papr_time_exp_t; t: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_explode_recent_localtime' + LibSuff12;

{
 * convert a recent time to its human readable components in GMT timezone
 * @param tm the exploded time
 * @param t the time to explode: MUST be within the last
 *          AP_TIME_RECENT_THRESHOLD seconds
 * @note This is a faster alternative to apr_time_exp_gmt that uses
 *       a cache of pre-exploded time structures.  It is useful for things
 *       that need to explode the current time multiple times per second,
 *       like loggers.
 * @return APR_SUCCESS iff successful
 }
function ap_explode_recent_gmt(tm: Papr_time_exp_t; t: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_explode_recent_gmt' + LibSuff12;

{
 * format a recent timestamp in the ctime() format.
 * @param date_str String to write to.
 * @param t the time to convert 
 }
function ap_recent_ctime(date_str: PChar; t: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_recent_ctime' + LibSuff12;

{
 * format a recent timestamp in the RFC822 format
 * @param date_str String to write to (must have length >= APR_RFC822_DATE_LEN)
 * @param t the time to convert 
 }
function ap_recent_rfc822_date(date_str: PChar; t: apr_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_recent_rfc822_date' + LibSuff12;

