{
 * This is work is derived from material Copyright RSA Data Security, Inc.
 *
 * The RSA copyright statement and Licence for that original material is
 * included below. This is followed by the Apache copyright statement and
 * licence for the modifications made to that material.
 }

{ Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
   rights reserved.

   License to copy and use this software is granted provided that it
   is identified as the "RSA Data Security, Inc. MD5 Message-Digest
   Algorithm" in all material mentioning or referencing this software
   or this function.

   License is also granted to make and use derivative works provided
   that such works are identified as "derived from the RSA Data
   Security, Inc. MD5 Message-Digest Algorithm" in all material
   mentioning or referencing the derived work.

   RSA Data Security, Inc. makes no representations concerning either
   the merchantability of this software or the suitability of this
   software for any particular purpose. It is provided "as is"
   without express or implied warranty of any kind.

   These notices must be retained in any copies of any part of this
   documentation and/or software.
 }

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

{#include "apu.h"}
{$include apr_xlate.inc}

{
 * @file apr_md5.h
 * @brief APR MD5 Routines
 }

{
 * @defgroup APR_MD5 MD5 Routines
 * @ingroup APR
 }

const
{ The MD5 digest size }
  APR_MD5_DIGESTSIZE = 16;
  MD5_DIGESTSIZE = APR_MD5_DIGESTSIZE;   {< @deprecated }

{ @see apr_md5_ctx_t }
type
  TDigestArray = array [0..APR_MD5_DIGESTSIZE] of Char;

  Papr_md5_ctx_t = ^apr_md5_ctx_t;

{ MD5 context. }
  apr_md5_ctx_t = record
    { state (ABCD) }
    state: array [1..4] of apr_uint32_t;
    { number of bits, modulo 2^64 (lsb first) }
    count: array [1..2] of apr_uint32_t;
    { input buffer }
    buffer: array [1..64] of Char;
    { translation handle
     *  ignored if xlate is unsupported
     }
    xlate: Papr_xlate_t;
  end;

{
 * MD5 Initialize.  Begins an MD5 operation, writing a new context.
 * @param context The MD5 context to initialize.
 }
function apr_md5_init(context: Papr_md5_ctx_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_md5_init' + LibSuff4;

{
 * MD5 translation setup.  Provides the APR translation handle to be used 
 * for translating the content before calculating the digest.
 * @param context The MD5 content to set the translation for.
 * @param xlate The translation handle to use for this MD5 context 
 }
function apr_md5_set_xlate(context: Papr_md5_ctx_t;
 xlate: Papr_xlate_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_md5_set_xlate' + LibSuff8;

{
 * MD5 block update operation.  Continue an MD5 message-digest operation, 
 * processing another message block, and updating the context.
 * @param context The MD5 content to update.
 * @param input next message block to update
 * @param inputLen The length of the next message block
 }
function apr_md5_update(context: Papr_md5_ctx_t;
 input: Pointer; inputLen: apr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_md5_update' + LibSuff12;

{
 * MD5 finalization.  Ends an MD5 message-digest operation, writing the 
 * message digest and zeroing the context
 * @param digest The final MD5 digest
 * @param context The MD5 content we are finalizing.
 }
function apr_md5_final(digest: TDigestArray;
 context: Papr_md5_ctx_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_md5_final' + LibSuff8;

{
 * MD5 in one step
 * @param digest The final MD5 digest
 * @param input The message block to use
 * @param inputLen The length of the message block
 }
function apr_md5(digest: TDigestArray;
 input: Pointer; inputLen: apr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_md5' + LibSuff12;

{
 * Encode a password using an MD5 algorithm
 * @param password The password to encode
 * @param salt The salt to use for the encoding
 * @param result The string to store the encoded password in
 * @param nbytes The length of the string
 }
function apr_md5_encode(const password, salt: PChar;
 result: PChar; nbytes: apr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_md5_encode' + LibSuff16;

{
 * Validate any password encypted with any algorithm that APR understands
 * @param passwd The password to validate
 * @param hash The password to validate against
 }
function apr_password_validate(const passwd, hash: PChar): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_password_validate' + LibSuff8;

