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
 * @file apr_lib.h
 * This is collection of oddballs that didn't fit anywhere else,
 * and might move to more appropriate headers with the release
 * of APR 1.0.
 * @brief APR general purpose library routines
 }

{#include "apr.h"
#include "apr_errno.h"

#if APR_HAVE_CTYPE_H
#include <ctype.h>
#endif
#if APR_HAVE_STDARG_H
#include <stdarg.h>
#endif}

{
 * @defgroup apr_lib General Purpose Library Routines
 * @ingroup APR 
 * This is collection of oddballs that didn't fit anywhere else,
 * and might move to more appropriate headers with the release
 * of APR 1.0.
 }

{ A constant representing a 'large' string. }
const HUGE_STRING_LEN = 8192;

{
 * Define the structures used by the APR general-purpose library.
 }

{ @see apr_vformatter_buff_t }
type
  Papr_vformatter_buff_t = ^apr_vformatter_buff_t;

{
 * Structure used by the variable-formatter routines.
 }
  apr_vformatter_buff_t = record
    { The current position }
    curpos: PChar;
    { The end position of the format string }
    endpos: PChar;
  end;

{
 * return the final element of the pathname
 * @param pathname The path to get the final element of
 * @return the final element of the path
 * @remark
 * <PRE>
 * For example:
 *                 "/foo/bar/gum"    -> "gum"
 *                 "/foo/bar/gum/"   -> ""
 *                 "gum"             -> "gum"
 *                 "bs\\path\\stuff" -> "stuff"
 * </PRE>
 }
function apr_filepath_name_get(const pathname: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_name_get' + LibSuff4;

{
 * apr_killpg
 * Small utility macros to make things easier to read.  Not usually a
 * goal, to be sure..
 }

//#ifdef WIN32
//#define apr_killpg(x, y)
//#else { WIN32 }
//#ifdef NO_KILLPG
//#define apr_killpg(x, y)        (kill (-(x), (y)))
//#else { NO_KILLPG }
//#define apr_killpg(x, y)        (killpg ((x), (y)))
//#endif { NO_KILLPG }
//#endif { WIN32 }

{
 * apr_vformatter() is a generic printf-style formatting routine
 * with some extensions.
 * @param flush_func The function to call when the buffer is full
 * @param c The buffer to write to
 * @param fmt The format string
 * @param ap The arguments to use to fill out the format string.
 *
 * @remark
 * <PRE>
 * The extensions are:
 *
 * %%pA	takes a struct in_addr *, and prints it as a.b.c.d
 * %%pI	takes an apr_sockaddr_t * and prints it as a.b.c.d:port or
 *      [ipv6-address]:port
 * %%pT takes an apr_os_thread_t * and prints it in decimal
 *      ('0' is printed if !APR_HAS_THREADS)
 * %%pt takes an apr_os_thread_t * and prints it in hexadecimal
 *      ('0' is printed if !APR_HAS_THREADS)
 * %%pp takes a void * and outputs it in hex
 *
 * The %%p hacks are to force gcc's printf warning code to skip
 * over a pointer argument without complaining.  This does
 * mean that the ANSI-style %%p (output a void * in hex format) won't
 * work as expected at all, but that seems to be a fair trade-off
 * for the increased robustness of having printf-warnings work.
 *
 * Additionally, apr_vformatter allows for arbitrary output methods
 * using the apr_vformatter_buff and flush_func.
 *
 * The apr_vformatter_buff has two elements curpos and endpos.
 * curpos is where apr_vformatter will write the next byte of output.
 * It proceeds writing output to curpos, and updating curpos, until
 * either the end of output is reached, or curpos == endpos (i.e. the
 * buffer is full).
 *
 * If the end of output is reached, apr_vformatter returns the
 * number of bytes written.
 *
 * When the buffer is full, the flush_func is called.  The flush_func
 * can return -1 to indicate that no further output should be attempted,
 * and apr_vformatter will return immediately with -1.  Otherwise
 * the flush_func should flush the buffer in whatever manner is
 * appropriate, re apr_pool_t nitialize curpos and endpos, and return 0.
 *
 * Note that flush_func is only invoked as a result of attempting to
 * write another byte at curpos when curpos >= endpos.  So for
 * example, it's possible when the output exactly matches the buffer
 * space available that curpos == endpos will be true when
 * apr_vformatter returns.
 *
 * apr_vformatter does not call out to any other code, it is entirely
 * self-contained.  This allows the callers to do things which are
 * otherwise "unsafe".  For example, apr_psprintf uses the "scratch"
 * space at the unallocated end of a block, and doesn't actually
 * complete the allocation until apr_vformatter returns.  apr_psprintf
 * would be completely broken if apr_vformatter were to call anything
 * that used this same pool.  Similarly http_bprintf() uses the "scratch"
 * space at the end of its output buffer, and doesn't actually note
 * that the space is in use until it either has to flush the buffer
 * or until apr_vformatter returns.
 * </PRE>
 }
type
  flush_func_t = function (b: Papr_vformatter_buff_t): Integer;
  
function apr_vformatter(flush_func: flush_func_t;
 c: Papr_vformatter_buff_t; const fmt: PChar; ap: va_list): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_vformatter' + LibSuff16;

{
 * Display a prompt and read in the password from stdin.
 * @param prompt The prompt to display
 * @param pwbuf Buffer to store the password
 * @param bufsize The length of the password buffer.
 * @remark If the password entered must be truncated to fit in
 * the provided buffer, APR_ENAMETOOLONG will be returned.
 * Note that the bufsize paramater is passed by reference for no
 * reason; its value will never be modified by the apr_password_get()
 * function.
 }
function apr_password_get(const prompt: PChar;
 pwbuf: PChar; bufsize: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_password_get' + LibSuff12;

{
 * @defgroup apr_ctype ctype functions
 * These macros allow correct support of 8-bit characters on systems which
 * support 8-bit characters.  Pretty dumb how the cast is required, but
 * that's legacy libc for ya.  These new macros do not support EOF like
 * the standard macros do.  Tough.
 }
{ @see isalnum }
//#define apr_isalnum(c) (isalnum(((unsigned char)(c))))
{ @see isalpha }
//#define apr_isalpha(c) (isalpha(((unsigned char)(c))))
{ @see iscntrl }
//#define apr_iscntrl(c) (iscntrl(((unsigned char)(c))))
{ @see isdigit }
//#define apr_isdigit(c) (isdigit(((unsigned char)(c))))
{ @see isgraph }
//#define apr_isgraph(c) (isgraph(((unsigned char)(c))))
{ @see islower}
//#define apr_islower(c) (islower(((unsigned char)(c))))
{ @see isascii }
{#ifdef isascii
#define apr_isascii(c) (isascii(((unsigned char)(c))))
#else
#define apr_isascii(c) (((c) & ~0x7f)==0)
#endif}
{ @see isprint }
//#define apr_isprint(c) (isprint(((unsigned char)(c))))
{ @see ispunct }
//#define apr_ispunct(c) (ispunct(((unsigned char)(c))))
{ @see isspace }
//#define apr_isspace(c) (isspace(((unsigned char)(c))))
{ @see isupper }
//#define apr_isupper(c) (isupper(((unsigned char)(c))))
{ @see isxdigit }
//#define apr_isxdigit(c) (isxdigit(((unsigned char)(c))))
{ @see tolower }
function apr_tolower(c: Char): Char;
{ @see toupper }
function apr_toupper(c: Char): Char;

