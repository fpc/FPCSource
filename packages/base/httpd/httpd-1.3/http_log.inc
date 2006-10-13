{ Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{$ifdef HAVE_SYSLOG}
//#include <syslog.h>

  APLOG_EMERG    = LOG_EMERG;     { system is unusable }
  APLOG_ALERT    = LOG_ALERT;     { action must be taken immediately }
  APLOG_CRIT     = LOG_CRIT;      { critical conditions }
  APLOG_ERR      = LOG_ERR;       { error conditions }
  APLOG_WARNING  = LOG_WARNING;   { warning conditions }
  APLOG_NOTICE   = LOG_NOTICE;    { normal but significant condition }
  APLOG_INFO     = LOG_INFO;      { informational }
  APLOG_DEBUG    = LOG_DEBUG;     { debug-level messages }

  APLOG_LEVELMASK= LOG_PRIMASK;   { mask off the level value }

{$else}

  APLOG_EMERG	= 0;	{ system is unusable }
  APLOG_ALERT	= 1;	{ action must be taken immediately }
  APLOG_CRIT	= 2;	{ critical conditions }
  APLOG_ERR	= 3;	{ error conditions }
  APLOG_WARNING	= 4;	{ warning conditions }
  APLOG_NOTICE	= 5;	{ normal but significant condition }
  APLOG_INFO	= 6;	{ informational }
  APLOG_DEBUG	= 7;	{ debug-level messages }

  APLOG_LEVELMASK= 7;	{ mask off the level value }

{$endif}

  APLOG_NOERRNO	=	(APLOG_LEVELMASK + 1);
{$ifdef WINDOWS}
{ Set to indicate that error msg should come from Win32's GetLastError(),
 * not errno. }
  APLOG_WIN32ERROR =	((APLOG_LEVELMASK + 1) * 2);
{$endif}

  DEFAULT_LOGLEVEL =	APLOG_WARNING;

//#define APLOG_MARK	__FILE__,__LINE__

procedure ap_open_logs(s: Pserver_rec; p: PPool);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ The two primary logging functions, ap_log_error and ap_log_rerror,
 * use a printf style format string to build the log message.  It is
 * VERY IMPORTANT that you not include any raw data from the network,
 * such as the request-URI or request header fields, within the format
 * string.  Doing so makes the server vulnerable to a denial-of-service
 * attack and other messy behavior.  Instead, use a simple format string
 * like "%s", followed by the string containing the untrusted data.
 }
procedure ap_log_error(
 const file_: PChar; line, level: Integer;
 const s: Pserver_rec; const fmt: PChar; others: array of const);
 cdecl; external LibHTTPD;

//			    __attribute__((format(printf,5,6)));

procedure ap_log_rerror(
 const file_: PChar; line, level: Integer;
 const s: Prequest_rec; const fmt: PChar; others: array of const);
 cdecl; external LibHTTPD;

//			    __attribute__((format(printf,5,6)));

procedure ap_error_log2stderr(s: Pserver_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

procedure ap_log_pid(p: PPool; fname: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{ These are for legacy code, new code should use ap_log_error,
 * or ap_log_rerror.
 }
{API_EXPORT(void) ap_log_error_old(const char *err, server_rec *s);
API_EXPORT(void) ap_log_unixerr(const char *routine, const char *file,
			     const char *msg, server_rec *s);
API_EXPORT_NONSTD(void) ap_log_printf(const server_rec *s, const char *fmt, ...)
			    __attribute__((format(printf,2,3)));
API_EXPORT(void) ap_log_reason(const char *reason, const char *fname,
			    request_rec *r);}

type
  piped_log = record
    p: PPool;
//#if !defined(NO_RELIABLE_PIPED_LOGS) || defined(TPF)
    program_: PChar;
    pid: cint;
    fds: array[1..2] of cint;
//#else
//    FILE *write_f;
//#endif
  end;
  
  Ppiped_log = ^piped_log;

procedure ap_open_piped_log(p: PPool; const program_: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

function ap_close_piped_log(p: Ppiped_log): Ppiped_log;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} external LibHTTPD;

{#if !defined(NO_RELIABLE_PIPED_LOGS) || defined(TPF)
#define ap_piped_log_read_fd(pl)	((pl)->fds[0])
#define ap_piped_log_write_fd(pl)	((pl)->fds[1])
#else}
//#define ap_piped_log_read_fd(pl)	(-1)
//#define ap_piped_log_write_fd(pl)	(fileno((pl)->write_f))
//#endif

