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

{ Portions of this file are covered by }
{ -*- mode: c; c-file-style: "k&r" -*-

  strnatcmp.c -- Perform 'natural order' comparisons of strings in C.
  Copyright (C) 2000 by Martin Pool <mbp@humbug.org.au>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
}

{
 * @file apr_strings.h
 * @brief APR Strings library
 }

{#include "apr.h"
#include "apr_errno.h"
#include "apr_pools.h"
#define APR_WANT_IOVEC
#include "apr_want.h"

#if APR_HAVE_STDARG_H
#include <stdarg.h>
#endif}

{
 * @defgroup apr_strings String routines
 * @ingroup APR 
 }

{
 * Do a natural order comparison of two strings.
 * @param a The first string to compare
 * @param b The second string to compare
 * @return Either <0, 0, or >0.  If the first string is less than the second
 *          this returns <0, if they are equivalent it returns 0, and if the
 *          first string is greater than second string it retuns >0.
 }
function apr_strnatcmp(a, b: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_strnatcmp' + LibSuff8;

{
 * Do a natural order comparison of two strings ignoring the case of the 
 * strings.
 * @param a The first string to compare
 * @param b The second string to compare
 * @return Either <0, 0, or >0.  If the first string is less than the second
 *         this returns <0, if they are equivalent it returns 0, and if the
 *         first string is greater than second string it retuns >0.
 }
function apr_strnatcasecmp(a, b: PChar): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_strnatcasecmp' + LibSuff8;

{
 * duplicate a string into memory allocated out of a pool
 * @param p The pool to allocate out of
 * @param s The string to duplicate
 * @return The new string
 }
function apr_pstrdup(p: Papr_pool_t; s: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pstrdup' + LibSuff8;

{
 * Create a null-terminated string by making a copy of a sequence
 * of characters and appending a null byte
 * @param p The pool to allocate out of
 * @param s The block of characters to duplicate
 * @param n The number of characters to duplicate
 * @return The new string
 * @remark This is a faster alternative to apr_pstrndup, for use
 *         when you know that the string being duplicated really
 *         has 'n' or more characters.  If the string might contain
 *         fewer characters, use apr_pstrndup.
 }
function apr_pstrmemdup(p: Papr_pool_t; s: PChar; n: apr_size_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pstrmemdup' + LibSuff12;

{
 * duplicate the first n characters of a string into memory allocated 
 * out of a pool; the new string will be null-terminated
 * @param p The pool to allocate out of
 * @param s The string to duplicate
 * @param n The number of characters to duplicate
 * @return The new string
 }
function apr_pstrndup(p: Papr_pool_t; s: PChar; n: apr_size_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pstrndup' + LibSuff12;

{
 * Duplicate a block of memory.
 *
 * @param p The pool to allocate from
 * @param m The memory to duplicate
 * @param n The number of bytes to duplicate
 * @return The new block of memory
 }
function apr_pmemdup(p: Papr_pool_t; m: Pointer; n: apr_size_t): Pointer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pmemdup' + LibSuff12;

{
 * Concatenate multiple strings, allocating memory out a pool
 * @param p The pool to allocate out of
 * @param ... The strings to concatenate.  The final string must be NULL
 * @return The new string
 }
function apr_pstrcat(p: Papr_pool_t; others: array of const): PChar;
 cdecl; external LibAPR name 'apr_pstrcat';

{
 * Concatenate multiple strings specified in a writev-style vector
 * @param p The pool from which to allocate
 * @param vec The strings to concatenate
 * @param nvec The number of strings to concatenate
 * @param nbytes (output) strlen of new string (pass in NULL to omit)
 * @return The new string
 }
function apr_pstrcatv(p: Papr_pool_t; const vec: Piovec;
 nvec: apr_size_t; nbytes: Papr_size_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pstrcatv' + LibSuff16;

{
 * printf-style style printing routine.  The data is output to a string 
 * allocated from a pool
 * @param p The pool to allocate out of
 * @param fmt The format of the string
 * @param ap The arguments to use while printing the data
 * @return The new string
 }
function apr_pvsprintf(p: Papr_pool_t; const fmt: PChar; ap: va_list): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_pvsprintf' + LibSuff12;

{
 * printf-style style printing routine.  The data is output to a string 
 * allocated from a pool
 * @param p The pool to allocate out of
 * @param fmt The format of the string
 * @param ... The arguments to use while printing the data
 * @return The new string
 }
function apr_psprintf(p: Papr_pool_t; const fmt: PChar; others: array of const): PChar;
 cdecl; external LibAPR name 'apr_psprintf';

{
 * copy n characters from src to dst
 * @param dst The destination string
 * @param src The source string
 * @param dst_size The space available in dst; dst always receives
 *                 null-termination, so if src is longer than
 *                 dst_size, the actual number of characters copied is
 *                 dst_size - 1.
 * @remark
 * <PRE>
 * We re-implement this function to implement these specific changes:
 *       1) strncpy() doesn't always null terminate and we want it to.
 *       2) strncpy() null fills, which is bogus, esp. when copy 8byte strings
 *          into 8k blocks.
 *       3) Instead of returning the pointer to the beginning of the
 *          destination string, we return a pointer to the terminating null
 *          to allow us to check for truncation.
 * </PRE>
 }
function apr_cpystrn(dst: PChar; const src: PChar;
 dst_size: apr_size_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_cpystrn' + LibSuff12;

{
 * Strip spaces from a string
 * @param dest The destination string.  It is okay to modify the string
 *             in place.  Namely dest == src
 * @param src The string to rid the spaces from.
 }
function apr_collapse_spaces(dst: PChar; const src: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_collapse_spaces' + LibSuff8;

{
 * Convert the arguments to a program from one string to an array of 
 * strings terminated by a NULL pointer
 * @param arg_str The arguments to convert
 * @param argv_out Output location.  This is a pointer to an array of strings.
 * @param token_context Pool to use.
 }
function apr_tokenize_to_argv(const arg_str: PChar;
 var argv_out: PPChar; token_context: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_tokenize_to_argv' + LibSuff12;

{
 * Split a string into separate null-terminated tokens.  The tokens are 
 * delimited in the string by one or more characters from the sep
 * argument.
 * @param str The string to separate; this should be specified on the
 *            first call to apr_strtok() for a given string, and NULL
 *            on subsequent calls.
 * @param sep The set of delimiters
 * @param last Internal state saved by apr_strtok() between calls.
 * @return The next token from the string
 }
function apr_strtok(str: PChar;
 const sep: PChar; last: PPChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_strtok' + LibSuff12;

{
 * @defgroup APR_Strings_Snprintf snprintf implementations
 * @warning
 * These are snprintf implementations based on apr_vformatter().
 *
 * Note that various standards and implementations disagree on the return
 * value of snprintf, and side-effects due to %n in the formatting string.
 * apr_snprintf (and apr_vsnprintf) behaves as follows:
 *
 * Process the format string until the entire string is exhausted, or
 * the buffer fills.  If the buffer fills then stop processing immediately
 * (so no further %n arguments are processed), and return the buffer
 * length.  In all cases the buffer is NUL terminated. It will return the
 * number of characters inserted into the buffer, not including the
 * terminating NUL. As a special case, if len is 0, apr_snprintf will
 * return the number of characters that would have been inserted if
 * the buffer had been infinite (in this case, *buffer can be NULL)
 *
 * In no event does apr_snprintf return a negative number.
 * @{
 }

{
 * snprintf routine based on apr_vformatter.  This means it understands the
 * same extensions.
 * @param buf The buffer to write to
 * @param len The size of the buffer
 * @param format The format string
 * @param ... The arguments to use to fill out the format string.
 }
function apr_snprintf(buf: PChar; len: apr_size_t;
 const format: PChar; others: array of const): PChar;
 cdecl; external LibAPR name 'apr_snprintf';

{
 * vsnprintf routine based on apr_vformatter.  This means it understands the
 * same extensions.
 * @param buf The buffer to write to
 * @param len The size of the buffer
 * @param format The format string
 * @param ap The arguments to use to fill out the format string.
 }
function apr_vsnprintf(buf: PChar; len: apr_size_t;
 const format: PChar; ap: va_list): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_vsnprintf' + LibSuff16;

{
 * create a string representation of an int, allocated from a pool
 * @param p The pool from which to allocate
 * @param n The number to format
 * @return The string representation of the number
 }
function apr_itoa(p: Papr_pool_t; n: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_itoa' + LibSuff8;

{
 * create a string representation of a long, allocated from a pool
 * @param p The pool from which to allocate
 * @param n The number to format
 * @return The string representation of the number
 }
function apr_ltoa(p: Papr_pool_t; n: Integer): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_ltoa' + LibSuff8;

{
 * create a string representation of an apr_off_t, allocated from a pool
 * @param p The pool from which to allocate
 * @param n The number to format
 * @return The string representation of the number
 }
function apr_off_t_toa(p: Papr_pool_t; n: apr_off_t): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_off_t_toa' + LibSuff12;

{
 * parse a numeric string into a 64-bit numeric value
 * @param buf The string to parse. It may contain optional whitespace,
 *   followed by an optional '+' (positive, default) or '-' (negative)
 *   character, followed by an optional '0x' prefix if base is 0 or 16,
 *   followed by numeric digits appropriate for base.
 * @param end A pointer to the end of the valid character in buf. If
 *   not nil, it is set to the first invalid character in buf.
 * @param base A numeric base in the range between 2 and 36 inclusive,
 *   or 0.  If base is zero, buf will be treated as base ten unless its
 *   digits are prefixed with '0x', in which case it will be treated as
 *   base 16.
 * @return The numeric value of the string.
 }
function apr_strtoi64(const buf: PChar; end_: PPChar; base: Integer): apr_int64_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_strtoi64' + LibSuff12;

{
 * parse a base-10 numeric string into a 64-bit numeric value.
 * Equivalent to apr_strtoi64(buf, (char**)NULL, 10).
 * @param buf The string to parse
 * @return The numeric value of the string
 }
function apr_atoi64(const buf: PChar): apr_int64_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_atoi64' + LibSuff4;

{
 * Format a binary size (magnitiudes are 2^10 rather than 10^3) from an apr_off_t,
 * as bytes, K, M, T, etc, to a four character compacted human readable string.
 * @param size The size to format
 * @param buf The 5 byte text buffer (counting the trailing null)
 * @return The buf passed to apr_strfsize()
 * @remark All negative sizes report '  - ', apr_strfsize only formats positive values.
 }
function apr_strfsize(size: apr_off_t; buf: PChar): PChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_strfsize' + LibSuff12;

