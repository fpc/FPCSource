{ Copyright 1999-2005 The Apache Software Foundation or its licensors, as
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
 * @package Apache MD5 library
 }

//#include "apr_md5.h"

{
 * Create an MD5 checksum of a given string
 * @param a Pool to allocate out of
 * @param string String to get the checksum of
 * @return The checksum
 * @deffunc AnsiChar *ap_md5(apr_pool_t *a, const unsigned AnsiChar *string)
 }
function ap_md5(p: Papr_pool_t; const string_: PAnsiChar): PAnsiChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_md5' + LibSuff8;

{
 * Create an MD5 checksum of a string of binary data
 * @param a Pool to allocate out of
 * @param buf Buffer to generate checksum for
 * @param len The length of the buffer
 * @return The checksum
 * @deffunc AnsiChar *ap_md5_binary(apr_pool_t *a, const unsigned AnsiChar *buf, int len)
 }
function ap_md5_binary(a: Papr_pool_t; const buf: PAnsiChar; len: Integer): PAnsiChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_md5_binary' + LibSuff12;

{
 * Convert an MD5 checksum into a base64 encoding
 * @param p The pool to allocate out of
 * @param context The context to convert
 * @return The converted encoding
 * @deffunc AnsiChar *ap_md5contextTo64(apr_pool_t *p, apr_md5_ctx_t *context)
 }
function ap_md5contextTo64(a: Papr_pool_t; context: Papr_md5_ctx_t): PAnsiChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_md5contextTo64' + LibSuff8;

{
 * Create an MD5 Digest for a given file
 * @param p The pool to allocate out of
 * @param infile The file to create the digest for
 * @deffunc AnsiChar *ap_md5digest(apr_pool_t *p, apr_file_t *infile)
 }
function ap_md5digest(p: Papr_pool_t; infile: Papr_file_t): PAnsiChar;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibHTTPD name LibNamePrefix + 'ap_md5digest' + LibSuff8;

