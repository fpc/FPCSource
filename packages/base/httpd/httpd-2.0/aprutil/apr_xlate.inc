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

{#include "apu.h"
#include "apr_pools.h"
#include "apr_errno.h"}

{
 * @file apr_xlate.h
 * @brief APR I18N translation library
 }

{
 * @defgroup APR_XLATE I18N translation library
 * @ingroup APR
 }
{ Opaque translation buffer }
type
  Papr_xlate_t = Pointer;
  PPapr_xlate_t = ^Papr_xlate_t;

{
 * Set up for converting text from one charset to another.
 * @param convset The handle to be filled in by this function
 * @param topage The name of the target charset
 * @param frompage The name of the source charset
 * @param pool The pool to use
 * @remark
 *  Specify APR_DEFAULT_CHARSET for one of the charset
 *  names to indicate the charset of the source code at
 *  compile time.  This is useful if there are literal
 *  strings in the source code which must be translated
 *  according to the charset of the source code.
 *  APR_DEFAULT_CHARSET is not useful if the source code
 *  of the caller was not encoded in the same charset as
 *  APR at compile time.
 *
 * @remark
 *  Specify APR_LOCALE_CHARSET for one of the charset
 *  names to indicate the charset of the current locale.
 *
 * @remark
 *  Return APR_EINVAL if unable to procure a convset, or APR_ENOTIMPL
 *  if charset transcoding is not available in this instance of
 *  apr-util at all (i.e., APR_HAS_XLATE is undefined).
 }
function apr_xlate_open(convset: PPapr_xlate_t;
 const topage, frompage: PChar; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xlate_open' + LibSuff16;

{
 * This is to indicate the charset of the sourcecode at compile time
 * names to indicate the charset of the source code at
 * compile time.  This is useful if there are literal
 * strings in the source code which must be translated
 * according to the charset of the source code.
 }
//#define APR_DEFAULT_CHARSET (const char *)0
{
 * To indicate charset names of the current locale 
 }
//#define APR_LOCALE_CHARSET (const char *)1

{
 * Find out whether or not the specified conversion is single-byte-only.
 * @param convset The handle allocated by apr_xlate_open, specifying the 
 *                parameters of conversion
 * @param onoff Output: whether or not the conversion is single-byte-only
 * @remark
 *  Return APR_ENOTIMPL if charset transcoding is not available
 *  in this instance of apr-util (i.e., APR_HAS_XLATE is undefined).
 }
function apr_xlate_sb_get(convset: Papr_xlate_t; onoff: PInteger): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xlate_sb_get' + LibSuff8;

{ @deprecated @see apr_xlate_sb_get }
function apr_xlate_get_sb(convset: Papr_xlate_t; onoff: PInteger): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xlate_get_sb' + LibSuff8;

{
 * Convert a buffer of text from one codepage to another.
 * @param convset The handle allocated by apr_xlate_open, specifying 
 *                the parameters of conversion
 * @param inbuf The address of the source buffer
 * @param inbytes_left Input: the amount of input data to be translated
 *                     Output: the amount of input data not yet translated    
 * @param outbuf The address of the destination buffer
 * @param outbytes_left Input: the size of the output buffer
 *                      Output: the amount of the output buffer not yet used
 * @remark
 *  Return APR_ENOTIMPL if charset transcoding is not available
 *  in this instance of apr-util (i.e., APR_HAS_XLATE is undefined).
 }
function apr_xlate_conv_buffer(convset: Papr_xlate_t; const inbuf: PChar;
 inbytes_left: Papr_size_t; outbuf: PChar; outbytes_left: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xlate_conv_buffer' + LibSuff20;

{ @see apr_file_io.h the comment in apr_file_io.h about this hack }
{$ifdef APR_NOT_DONE_YET}
{
 * The purpose of apr_xlate_conv_char is to translate one character
 * at a time.  This needs to be written carefully so that it works
 * with double-byte character sets. 
 * @param convset The handle allocated by apr_xlate_open, specifying the
 *                parameters of conversion
 * @param inchar The character to convert
 * @param outchar The converted character
 }
APU_DECLARE(apr_status_t) apr_xlate_conv_char(apr_xlate_t *convset, 
                                              char inchar, char outchar);
{$endif}

{
 * Convert a single-byte character from one charset to another.
 * @param convset The handle allocated by apr_xlate_open, specifying the 
 *                parameters of conversion
 * @param inchar The single-byte character to convert.
 * @warning This only works when converting between single-byte character sets.
 *          -1 will be returned if the conversion can't be performed.
 }
function apr_xlate_conv_byte(convset: Papr_xlate_t; inchar: Char): apr_int32_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xlate_conv_byte' + LibSuff8;

{
 * Close a codepage translation handle.
 * @param convset The codepage translation handle to close
 * @remark
 *  Return APR_ENOTIMPL if charset transcoding is not available
 *  in this instance of apr-util (i.e., APR_HAS_XLATE is undefined).
 }
function apr_xlate_close(convset: Papr_xlate_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPRUtil name LibNamePrefix + 'apr_xlate_close' + LibSuff4;

