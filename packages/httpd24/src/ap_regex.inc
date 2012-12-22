{* Licensed to the Apache Software Foundation (ASF) under one or more
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
 *}

{* Derived from PCRE's pcreposix.h.

            Copyright (c) 1997-2004 University of Cambridge

-----------------------------------------------------------------------------
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of the University of Cambridge nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
*}

{**
 * @file ap_regex.h
 * @brief Apache Regex defines
 *}

//#ifndef AP_REGEX_H
//#define AP_REGEX_H

//#include "apr.h"

//* Allow for C++ users */

//* Options for ap_regcomp, ap_regexec, and ap_rxplus versions: */
const
  AP_REG_ICASE    = $01; //** use a case-insensitive match */
  AP_REG_NEWLINE  = $02; //** don't match newlines against '.' etc */
  AP_REG_NOTBOL   = $04; //** ^ will not match against start-of-string */
  AP_REG_NOTEOL   = $08; //** $ will not match against end-of-string */

  AP_REG_EXTENDED = 0;   //** unused */
  AP_REG_NOSUB    = 0;   //** unused */

  AP_REG_MULTI    = $10; //* perl's /g (needs fixing) */
  AP_REG_NOMEM    = $20; //* nomem in our code */
  AP_REG_DOTALL   = $40; //* perl's /s flag */

//* Error values: */
  AP_REG_ASSERT   = 1;//** internal error ? */
  AP_REG_ESPACE   = 2;//** failed to get memory */
  AP_REG_INVARG   = 3;//** invalid argument */
  AP_REG_NOMATCH  = 4;//** match failed */

//* The structure representing a compiled regular expression. */
type
  Pap_regex_t = ^ap_regex_t;
  ap_regex_t = record
    re_pcre: Pointer;
    re_nsub: Integer;
    re_erroffset: apr_size_t;
  end;

//* The structure in which a captured offset is returned. */
  Pap_regmatch_t = ^ap_regmatch_t;
  ap_regmatch_t = record
    rm_so: Integer;
    rm_eo: Integer;
  end;

//* The functions */

{**
 * Compile a regular expression.
 * @param preg Returned compiled regex
 * @param regex The regular expression string
 * @param cflags Bitwise OR of AP_REG_* flags (ICASE and NEWLINE supported,
 *                                             other flags are ignored)
 * @return Zero on success or non-zero on error
 *}
//AP_DECLARE(int) ap_regcomp(ap_regex_t *preg, const char *regex, int cflags);
function ap_regcomp(preg: Pap_regex_t; const regex: PChar; cflags: Integer): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_regcomp' + LibSuff12;

{**
 * Match a NUL-terminated string against a pre-compiled regex.
 * @param preg The pre-compiled regex
 * @param string The string to match
 * @param nmatch Provide information regarding the location of any matches
 * @param pmatch Provide information regarding the location of any matches
 * @param eflags Bitwise OR of AP_REG_* flags (NOTBOL and NOTEOL supported,
 *                                             other flags are ignored)
 * @return 0 for successful match, \p AP_REG_NOMATCH otherwise
 *}
//AP_DECLARE(int) ap_regexec(const ap_regex_t *preg, const char *string,
//                           apr_size_t nmatch, ap_regmatch_t *pmatch, int eflags);
function ap_regexec(const preg: Pap_regex_t; const string_: PChar;
                    nmatch: apr_size_t; pmatch: Pap_regmatch_t; eflags: Integer): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_regexec' + LibSuff20;

{**
 * Match a string with given length against a pre-compiled regex. The string
 * does not need to be NUL-terminated.
 * @param preg The pre-compiled regex
 * @param buff The string to match
 * @param len Length of the string to match
 * @param nmatch Provide information regarding the location of any matches
 * @param pmatch Provide information regarding the location of any matches
 * @param eflags Bitwise OR of AP_REG_* flags (NOTBOL and NOTEOL supported,
 *                                             other flags are ignored)
 * @return 0 for successful match, AP_REG_NOMATCH otherwise
 *}
//AP_DECLARE(int) ap_regexec_len(const ap_regex_t *preg, const char *buff,
//                               apr_size_t len, apr_size_t nmatch,
//                               ap_regmatch_t *pmatch, int eflags);
function ap_regexec_len(const preg: Pap_regex_t; const buff: PChar;
                        len, nmatch: apr_size_t;
                        pmatch: Pap_regmatch_t; eflags: Integer): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_regexec_len' + LibSuff24;

{**
 * Return the error code returned by regcomp or regexec into error messages
 * @param errcode the error code returned by regexec or regcomp
 * @param preg The precompiled regex
 * @param errbuf A buffer to store the error in
 * @param errbuf_size The size of the buffer
 *}
//AP_DECLARE(apr_size_t) ap_regerror(int errcode, const ap_regex_t *preg,
//                                   char *errbuf, apr_size_t errbuf_size);
function ap_regerror(errcode: Integer; const preg: Pap_regex_t;
                     errbuf: PChar; errbuf_size: apr_size_t): apr_size_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_regerror' + LibSuff16;

{** Destroy a pre-compiled regex.
 * @param preg The pre-compiled regex to free.
 *}
//AP_DECLARE(void) ap_regfree(ap_regex_t *preg);
procedure ap_regfree(preg: Pap_regex_t);
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_regfree' + LibSuff4;

//* ap_rxplus: higher-level regexps */
type
  Pap_rxplus_t = ^ap_rxplus_t;
  ap_rxplus_t = record
    rx: ap_regex_t;
    flags: apr_uint32_t;
    subs,
    match: PChar;
    nmatch: apr_size_t;
    pmatch: Pap_regmatch_t;
  end;{ap_rxplus_t}

{**
 * Compile a pattern into a regexp.
 * supports perl-like formats
 *    match-string
 *    /match-string/flags
 *    s/match-string/replacement-string/flags
 *    Intended to support more perl-like stuff as and when round tuits happen
 * match-string is anything supported by ap_regcomp
 * replacement-string is a substitution string as supported in ap_pregsub
 * flags should correspond with perl syntax: treat failure to do so as a bug
 *                                           (documentation TBD)
 * @param pool Pool to allocate from
 * @param pattern Pattern to compile
 * @return Compiled regexp, or NULL in case of compile/syntax error
 *}
//AP_DECLARE(ap_rxplus_t*) ap_rxplus_compile(apr_pool_t *pool, const char *pattern);
function ap_rxplus_compile(pool: Papr_pool_t; const pattern: PChar): Pap_rxplus_t;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_rxplus_compile' + LibSuff8;

{**
 * Apply a regexp operation to a string.
 * @param pool Pool to allocate from
 * @param rx The regex match to apply
 * @param pattern The string to apply it to
 *                NOTE: This MUST be kept in scope to use regexp memory
 * @param newpattern The modified string (ignored if the operation doesn't
 *                                        modify the string)
 * @return Number of times a match happens.  Normally 0 (no match) or 1
 *         (match found), but may be greater if a transforming pattern
 *         is applied with the 'g' flag.
 *}
//AP_DECLARE(int) ap_rxplus_exec(apr_pool_t *pool, ap_rxplus_t *rx,
//                               const char *pattern, char **newpattern);
function ap_rxplus_exec(pool: Papr_pool_t; rx: Pap_rxplus_t;
                        const pattern: PChar; newpattern: PPChar): Integer;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_rxplus_exec' + LibSuff16;

{#ifdef DOXYGEN
/**
 * Number of matches in the regexp operation's memory
 * This may be 0 if no match is in memory, or up to nmatch from compilation
 * @param rx The regexp
 * @return Number of matches in memory
 */
AP_DECLARE(int) ap_rxplus_nmatch(ap_rxplus_t *rx);
#else
#define ap_rxplus_nmatch(rx) (((rx)->match != NULL) ? (rx)->nmatch : 0)
#endif}

{**
 * Get a pointer to a match from regex memory
 * NOTE: this relies on the match pattern from the last call to
 *       ap_rxplus_exec still being valid (i.e. not freed or out-of-scope)
 * @param rx The regexp
 * @param n The match number to retrieve (must be between 0 and nmatch)
 * @param len Returns the length of the match.
 * @param match Returns the match pattern
 *}
//AP_DECLARE(void) ap_rxplus_match(ap_rxplus_t *rx, int n, int *len,
//                                 const char **match);
procedure ap_rxplus_match(rx: Pap_rxplus_t; len: Integer;
                          const match: PPChar);
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_rxplus_match' + LibSuff16;

{**
 * Get a match from regex memory in a string copy
 * NOTE: this relies on the match pattern from the last call to
 *       ap_rxplus_exec still being valid (i.e. not freed or out-of-scope)
 * @param pool Pool to allocate from
 * @param rx The regexp
 * @param n The match number to retrieve (must be between 0 and nmatch)
 * @return The matched string
 *}
//AP_DECLARE(char*) ap_rxplus_pmatch(apr_pool_t *pool, ap_rxplus_t *rx, int n);
//
function ap_rxplus_pmatch(pool: Papr_pool_t; rx: Pap_rxplus_t; n: Integer): PChar;
  {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  external LibHTTPD name LibNamePrefix + 'ap_rxplus_pmatch' + LibSuff12;

//#endif /* AP_REGEX_T */

