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

//#include "apr_thread_proc.h"

{
 * @package Apache logging library
 }

{#ifdef HAVE_SYSLOG
#include <syslog.h>}

const
  LOG_PRIMASK = 7;

  APLOG_EMERG	= 0;	{ system is unusable }
  APLOG_ALERT	= 1;	{ action must be taken immediately }
  APLOG_CRIT	= 2;	{ critical conditions }
  APLOG_ERR	= 3;	{ error conditions }
  APLOG_WARNING	= 4;	{ warning conditions }
  APLOG_NOTICE	= 5;	{ normal but significant condition }
  APLOG_INFO	= 6;	{ informational }
  APLOG_DEBUG	= 7;	{ debug-level messages }

  APLOG_LEVELMASK = 7;	{ mask off the level value }

{ APLOG_NOERRNO is ignored and should not be used.  It will be
 * removed in a future release of Apache.
 }
  APLOG_NOERRNO	=	(APLOG_LEVELMASK + 1);

{ Use APLOG_TOCLIENT on ap_log_rerror() to give content
 * handlers the option of including the error text in the 
 * ErrorDocument sent back to the client. Setting APLOG_TOCLIENT
 * will cause the error text to be saved in the request_rec->notes 
 * table, keyed to the string "error-notes", if and only if:
 * - the severity level of the message is APLOG_WARNING or greater
 * - there are no other "error-notes" set in request_rec->notes
 * Once error-notes is set, it is up to the content handler to
 * determine whether this text should be sent back to the client.
 * Note: Client generated text streams sent back to the client MUST 
 * be escaped to prevent CSS attacks.
 }
  APLOG_TOCLIENT =         ((APLOG_LEVELMASK + 1) * 2);

{ normal but significant condition on startup, usually printed to stderr }
  APLOG_STARTUP =          ((APLOG_LEVELMASK + 1) * 4);

  DEFAULT_LOGLEVEL =	APLOG_WARNING;

//extern int AP_DECLARE_DATA ap_default_loglevel;

//  APLOG_MARK = __FILE__,__LINE__;

{
 * Set up for logging to stderr.
 * @param p The pool to allocate out of
 }
procedure ap_open_stderr_log(p: Papr_pool_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_open_stderr_log' + LibSuff4;

{
 * Replace logging to stderr with logging to the given file.
 * @param p The pool to allocate out of
 * @param file Name of the file to log stderr output
 }
function ap_replace_stderr_log(p: Papr_pool_t;
 file_: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_replace_stderr_log' + LibSuff8;

{
 * Open the error log and replace stderr with it.
 * @param pconf Not used
 * @param plog  The pool to allocate the logs from
 * @param ptemp Pool used for temporary allocations
 * @param s_main The main server
 * @tip ap_open_logs isn't expected to be used by modules, it is
 * an internal core function 
 }
{int ap_open_logs(apr_pool_t *pconf, apr_pool_t *plog,
                 apr_pool_t *ptemp, server_rec *s_main);}

//#ifdef CORE_PRIVATE

{
 * Perform special processing for piped loggers in MPM child
 * processes.
 * @param p Not used
 * @param s Not used
 * @tip ap_logs_child_init is not for use by modules; it is an
 * internal core function
 }
//void ap_logs_child_init(apr_pool_t *p, server_rec *s);

//#endif { CORE_PRIVATE }

{
 * The primary logging functions, ap_log_error, ap_log_rerror, ap_log_cerror,
 * and ap_log_perror use a printf style format string to build the log message.  
 * It is VERY IMPORTANT that you not include any raw data from the network, 
 * such as the request-URI or request header fields, within the format 
 * string.  Doing so makes the server vulnerable to a denial-of-service 
 * attack and other messy behavior.  Instead, use a simple format string 
 * like "%s", followed by the string containing the untrusted data.
 }

{
 * ap_log_error() - log messages which are not related to a particular
 * request or connection.  This uses a printf-like format to log messages
 * to the error_log.
 * @param file The file in which this function is called
 * @param line The line number on which this function is called
 * @param level The level of this error message
 * @param status The status code from the previous command
 * @param s The server on which we are logging
 * @param fmt The format string
 * @param ... The arguments to use to fill out fmt.
 * @tip Use APLOG_MARK to fill out file and line
 * @tip If a request_rec is available, use that with ap_log_rerror()
 * in preference to calling this function.  Otherwise, if a conn_rec is
 * available, use that with ap_log_cerror() in preference to calling
 * this function.
 * @warning It is VERY IMPORTANT that you not include any raw data from 
 * the network, such as the request-URI or request header fields, within 
 * the format string.  Doing so makes the server vulnerable to a 
 * denial-of-service attack and other messy behavior.  Instead, use a 
 * simple format string like "%s", followed by the string containing the 
 * untrusted data.
 * @deffunc void ap_log_error(const char *file, int line, int level, apr_status_t status, const server_rec *s, const char *fmt, ...) 
 }
procedure ap_log_error(
 const file_: PChar; line, level: Integer;
 status: apr_status_t; const s: Pserver_rec;
 const fmt: PChar; others: array of const);
 cdecl; external LibHTTPD name 'ap_log_error';

//			    __attribute__((format(printf,6,7)));

{
 * ap_log_perror() - log messages which are not related to a particular
 * request, connection, or virtual server.  This uses a printf-like
 * format to log messages to the error_log.
 * @param file The file in which this function is called
 * @param line The line number on which this function is called
 * @param level The level of this error message
 * @param status The status code from the previous command
 * @param p The pool which we are logging for
 * @param fmt The format string
 * @param ... The arguments to use to fill out fmt.
 * @tip Use APLOG_MARK to fill out file and line
 * @warning It is VERY IMPORTANT that you not include any raw data from 
 * the network, such as the request-URI or request header fields, within 
 * the format string.  Doing so makes the server vulnerable to a 
 * denial-of-service attack and other messy behavior.  Instead, use a 
 * simple format string like "%s", followed by the string containing the 
 * untrusted data.
 * @deffunc void ap_log_perror(const char *file, int line, int level, apr_status_t status, apr_pool_t *p, const char *fmt, ...) 
 }
procedure ap_log_perror(
 const file_: PChar; line, level: Integer;
 status: apr_status_t; p: Papr_pool_t;
 const fmt: PChar; others: array of const);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name 'ap_log_perror';

{		    __attribute__((format(printf,6,7)));}

{
 * ap_log_rerror() - log messages which are related to a particular
 * request.  This uses a a printf-like format to log messages to the
 * error_log.
 * @param file The file in which this function is called
 * @param line The line number on which this function is called
 * @param level The level of this error message
 * @param status The status code from the previous command
 * @param r The request which we are logging for
 * @param fmt The format string
 * @param ... The arguments to use to fill out fmt.
 * @tip Use APLOG_MARK to fill out file and line
 * @warning It is VERY IMPORTANT that you not include any raw data from 
 * the network, such as the request-URI or request header fields, within 
 * the format string.  Doing so makes the server vulnerable to a 
 * denial-of-service attack and other messy behavior.  Instead, use a 
 * simple format string like "%s", followed by the string containing the 
 * untrusted data.
 * @deffunc void ap_log_rerror(const char *file, int line, int level, apr_status_t status, const request_rec *r, const char *fmt, ...)
 }
procedure ap_log_rerror(
 const file_: PChar; line, level: Integer;
 status: apr_status_t; const r: Prequest_rec;
 const fmt: PChar; others: array of const);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name 'ap_log_rerror';

{    __attribute__((format(printf,6,7)));}

{
 * ap_log_cerror() - log messages which are related to a particular
 * connection.  This uses a a printf-like format to log messages to the
 * error_log.
 * @param file The file in which this function is called
 * @param line The line number on which this function is called
 * @param level The level of this error message
 * @param status The status code from the previous command
 * @param c The connection which we are logging for
 * @param fmt The format string
 * @param ... The arguments to use to fill out fmt.
 * @tip Use APLOG_MARK to fill out file and line
 * @tip If a request_rec is available, use that with ap_log_rerror()
 * in preference to calling this function.
 * @warning It is VERY IMPORTANT that you not include any raw data from 
 * the network, such as the request-URI or request header fields, within 
 * the format string.  Doing so makes the server vulnerable to a 
 * denial-of-service attack and other messy behavior.  Instead, use a 
 * simple format string like "%s", followed by the string containing the 
 * untrusted data.
 * @note ap_log_cerror() is available starting with Apache 2.0.55.
 * @deffunc void ap_log_cerror(const char *file, int line, int level, apr_status_t status, const conn_rec *c, const char *fmt, ...)
 }
procedure ap_log_cerror(
 const file_: PChar; line, level: Integer;
 status: apr_status_t; const c: Pconn_rec;
 const fmt: PChar; others: array of const);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name 'ap_log_cerror';

{    __attribute__((format(printf,6,7)));}

{
 * Convert stderr to the error log
 * @param s The current server
 * @deffunc void ap_error_log2stderr(server_rec *s)
 }
procedure ap_error_log2stderr(s: Pserver_rec);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_error_log2stderr' + LibSuff4;

{
 * Log the current pid of the parent process
 * @param p The pool to use for logging
 * @param fname The name of the file to log to
 }
procedure ap_log_pid(p: Papr_pool_t; const fname: PChar);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_log_pid' + LibSuff8;

{
 * Retrieve the pid from a pidfile.
 * @param p The pool to use for logging
 * @param filename The name of the file containing the pid
 * @param mypid Pointer to pid_t (valid only if return APR_SUCCESS)
 }
function ap_read_pid(p: Papr_pool_t; const filename: PChar; mypid: Ppid_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_read_pid' + LibSuff12;

{
 * The piped logging structure.  Piped logs are used to move functionality
 * out of the main server.  For example, log rotation is done with piped logs.
 }
type
  piped_log = record
    { The pool to use for the piped log }
    p: Papr_pool_t;
    { The pipe between the server and the logging process }
    fds: array[0..2] of Papr_file_t;
    { XXX - an #ifdef that needs to be eliminated from public view. Shouldn't
     * be hard }
{$ifdef AP_HAVE_RELIABLE_PIPED_LOGS}
    { The name of the program the logging process is running }
    program: PChar;
    { The pid of the logging process }
    pid: Papr_proc_t;
{$endif}
  end;
  Ppiped_log = ^piped_log;
{
 * Open the piped log process
 * @param p The pool to allocate out of
 * @param program The program to run in the logging process
 * @return The piped log structure
 * @deffunc piped_log *ap_open_piped_log(apr_pool_t *p, const char *program)
 }
function ap_open_piped_log(p: Papr_pool_t; const program_: PChar): Ppiped_log;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_open_piped_log' + LibSuff8;

{
 * Close the piped log and kill the logging process
 * @param pl The piped log structure
 * @deffunc void ap_close_piped_log(piped_log *pl)
 }
procedure ap_close_piped_log(pl: Ppiped_log);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_close_piped_log' + LibSuff4;

{
 * A macro to access the read side of the piped log pipe
 * @param pl The piped log structure
 * @return The native file descriptor
 * @deffunc ap_piped_log_read_fd(pl)
 }
//#define ap_piped_log_read_fd(pl)	((pl)->fds[0])

{
 * A macro to access the write side of the piped log pipe
 * @param pl The piped log structure
 * @return The native file descriptor
 * @deffunc ap_piped_log_read_fd(pl)
 }
//#define ap_piped_log_write_fd(pl)	((pl)->fds[1])

type
  ap_HOOK_error_log_t = procedure(const _file: PChar; line: Integer;
    level: Integer; status: apr_status_t; const s: Pserver_rec;
    const r: Prequest_rec; p: Papr_pool_t; const errstr: PChar); cdecl;

{
 * hook method to log error messages
 * @ingroup hooks
 * @param file The file in which this function is called
 * @param line The line number on which this function is called
 * @param level The level of this error message
 * @param status The status code from the previous command
 * @param s The server which we are logging for
 * @param r The request which we are logging for
 * @param pool Memory pool to allocate from
 * @param errstr message to log
 }
procedure ap_hook_error_log(pf: ap_HOOK_error_log_t; const aszPre: PPChar;
 const aszSucc: PPChar; nOrder: Integer);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_hook_error_log' + LibSuff16;

