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

{ Derived from PCRE's pcreposix.h.

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
}

{
 * @file ap_regex.h
 * @brief Apache Regex defines
 }

//#include "apr.h"

{ Options for ap_regexec: }

const
  AP_REG_ICASE    = $01; { use a case-insensitive match }
  AP_REG_NEWLINE  = $02; { don't match newlines against '.' etc }
  AP_REG_NOTBOL   = $04; { ^ will not match against start-of-string }
  AP_REG_NOTEOL   = $08; { $ will not match against end-of-string }

  AP_REG_EXTENDED = (0);  { unused }
  AP_REG_NOSUB    = (0);  { unused }

{ Error values: }

  AP_REG_ASSERT  = 1;     { internal error ? }
  AP_REG_ESPACE  = 2;     { failed to get memory }
  AP_REG_INVARG  = 3;     { invalid argument }
  AP_REG_NOMATCH = 4;     { match failed }

{ The structure representing a compiled regular expression. }
type
  Pap_regex_t = ^ap_regex_t;
  
  ap_regex_t = record
    re_pcre: Pointer;
    re_nsub: apr_size_t;
    re_erroffset: apr_size_t;
  end;

{ The structure in which a captured offset is returned. }
  Pap_regmatch_t = ^ap_regmatch_t;

  ap_regmatch_t = record
    rm_so: Integer;
    rm_eo: Integer;
  end;

{ The functions }

{
 * Compile a regular expression.
 * @param preg Returned compiled regex
 * @param regex The regular expression string
 * @param cflags Must be zero (currently).
 * @return Zero on success or non-zero on error
 }
function ap_regcomp(preg: Pap_regex_t; const regex: PChar; cflags: Integer): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_regcomp' + LibSuff12;

{
 * Match a NUL-terminated string against a pre-compiled regex.
 * @param preg The pre-compiled regex
 * @param string The string to match
 * @param nmatch Provide information regarding the location of any matches
 * @param pmatch Provide information regarding the location of any matches
 * @param eflags Bitwise OR of any of AP_REG_* flags 
 * @return 0 for successful match, #REG_NOMATCH otherwise
 }
function ap_regexec(const preg: Pap_regex_t; const string_: PChar;
 nmatch: apr_size_t; pmatch: Pap_regmatch_t; eflags: Integer): Integer;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_regexec' + LibSuff20;

{
 * Return the error code returned by regcomp or regexec into error messages
 * @param errcode the error code returned by regexec or regcomp
 * @param preg The precompiled regex
 * @param errbuf A buffer to store the error in
 * @param errbuf_size The size of the buffer
 }
function ap_regerror(errcord: Integer; const preg: Pap_regex_t;
 errbuf: PChar; errbuf_size: apr_size_t): apr_size_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_regerror' + LibSuff16;

{ Destroy a pre-compiled regex.
 * @param preg The pre-compiled regex to free.
 }
procedure ap_regfree(preg: Pap_regex_t);
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_regfree' + LibSuff4;

