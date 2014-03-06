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
unit apr24;
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}
{$ifdef Unix}
  {$PACKRECORDS C}
{$endif}

{$DEFINE Apache2_4}
{$DEFINE FPCAPACHE_2_4}

interface

uses
{$ifdef WINDOWS}
  Windows,
{$ELSE}
  UnixType,
{$ENDIF}
  SysUtils, ctypes;
  
const
{$IFDEF WINDOWS}
  LibAPR = 'libapr-1.dll';
{$ELSE}
  LibAPR = '';
{$ENDIF}

{$IF DEFINED(WINDOWS) AND NOT DEFINED(WIN64)}//Win64 is same as Linux, no funky function name changes
  LibNamePrefix = '_';
  LibSuff_ = '_';
  LibSuff0 = '@0';
  LibSuff4 = '@4';
  LibSuff8 = '@8';
  LibSuff12 = '@12';
  LibSuff16 = '@16';
  LibSuff20 = '@20';
  LibSuff24 = '@24';
  LibSuff28 = '@28';
  LibSuff32 = '@32';
{$ELSE}
  LibNamePrefix = '';
  LibSuff_ = '_';
  LibSuff0 = '';
  LibSuff4 = '';
  LibSuff8 = '';
  LibSuff12 = '';
  LibSuff16 = '';
  LibSuff20 = '';
  LibSuff24 = '';
  LibSuff28 = '';
  LibSuff32 = '';
{$ENDIF}

{**********************************************************}
{ Declarations moved here to be on top of all declarations }
{**********************************************************}
{from apr-util-X.X.X/include/apr_hooks.h}
//* Hook orderings */
//** run this hook first, before ANYTHING */
APR_HOOK_REALLY_FIRST    = (-10);
//** run this hook first */
APR_HOOK_FIRST           = 0;
//** run this hook somewhere */
APR_HOOK_MIDDLE		 = 10;
//** run this hook after every other hook which is defined*/
APR_HOOK_LAST		 = 20;
//** run this hook last, after EVERYTHING */
APR_HOOK_REALLY_LAST	 = 30;

type
  apr_int64_t = Int64;
  apr_uint64_t = QWord;
  apr_off_t = Int64;
  Papr_off_t = ^apr_off_t;
  apr_size_t = size_t;
  Papr_size_t = ^apr_size_t;
  apr_int32_t = LongInt;
  apr_uint32_t = LongWord;
  apr_ino_t = apr_uint64_t;
  pid_t = Longint;
  Ppid_t = ^pid_t;
  time_t = LongInt;//ansi time_t
  Papr_file_t = Pointer;//
  Pap_filter_t = Pointer;//temporary, it is in util_filter.inc

  {from apr_hash.h (real struct is defined in apr_hash.c)}
  apr_hash_t = record end;
  Papr_hash_t = ^apr_hash_t;

  {apr_bucket_alloc_t is found in apr-util-X.X.X/buckets/apr_buckets_alloc.c
   and not .h header file declared}
  Papr_bucket_alloc_t = Pointer;//^apr_bucket_alloc_t;

{$IFNDEF WINDOWS}
  va_list = Pointer;
{$ENDIF}

{$include apr_errno.inc}
{$include apr_pools.inc}
//{$include apr_general.inc}
//{$include apr_dso.inc}
{$include apr_user.inc}
{$include apr_time.inc}
{$include apr_tables.inc}
{$include apr_file_info.inc}
//{$include apr_file_io.inc}
{$include apr_strings.inc}
//{$include apr_lib.inc}
//{$include apr_signal.inc}
//{$include apr_network_io.inc}
//{$include apr_hash.inc}
   {.$include apr_portable.inc}

   {.$include ../aprutils/apr_uri.inc}

//{$include apr_thread_proc.inc}
{$include apr_version.inc}
//{$include apr_poll.inc}

//moved from http_protocol.h(http_protocol.inc)
{$include apr_mmap.inc}

implementation

end.
