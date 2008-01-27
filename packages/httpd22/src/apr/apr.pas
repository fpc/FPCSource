{
 apr.pas

 Copyright (C) 2006 Felipe Monteiro de Carvalho

 This unit is a pascal binding for the Apache 2.0.58 headers.
 The headers were released under the following copyright:
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
unit apr;

interface

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

{$IFNDEF FPC}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

{$ifdef Unix}
  {$PACKRECORDS C}
{$endif}

uses
{$ifdef WINDOWS}
  Windows, winsock,
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

{$IFDEF WINDOWS}
  LibNamePrefix = '_';
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

type
  uid_t = Integer;
  gid_t = Integer;
  time_t = LongInt;
  size_t = Integer;
  pid_t = Integer;
  Ppid_t = ^pid_t;
  apr_uint16_t = Word;
  apr_uint32_t = Cardinal;
  apr_int64_t = Int64;
  apr_uint64_t = Int64;
  apr_socklen_t = Integer;
  apr_byte_t = Byte;

  apr_uint32_tso_handle_t = cuint;

type
  {$IFDEF WINDOWS}
  apr_off_t = Int64;
  {$ENDIF}
  {$IFDEF UNIX}
  {$ifdef CPU64}
  apr_off_t = int64;
  {$else}
  apr_off_t = Integer;
  {$endif}
  {$ENDIF}

  apr_int32_t = Integer;
  Papr_int32_t = ^Integer;
  apr_size_t = size_t;
  Papr_size_t = ^apr_size_t;
  apr_int16_t = SmallInt;
  Papr_int16_t = ^SmallInt;

  // Network structures
  
  sockaddr = record
    sa_family: cushort;    // address family, AF_xxx
    sa_data: array [1..14] of Char;  // (NBO) 14 bytes of protocol address
  end;
  
  in_addr = record
    s_addr: culong;        // load with inet_aton()
  end;

{$ifndef windows}

  va_list = Pointer;

  sockaddr_in = record
    sin_family: cshort;    // e.g. AF_INET
    sin_port: cushort;     // e.g. htons(3490)
    sin_addr: in_addr;     // see struct in_addr, below
    sin_zero: array [1..8] of Char;  // zero this if you want to
  end;
  
{$endif}

  in6_addr = record
   Case Integer of
    1: (u6_addr8: array [1..16] of Byte);
    2: (u6_addr16: array [1..8] of Word);
    3: (u6_addr32: array [1..4] of Cardinal);
  end;
//#define s6_addr      in6_u.u6_addr8
//#define s6_addr16    in6_u.u6_addr16
//#define s6_addr32    in6_u.u6_addr32

  sockaddr_in6 = record
   sin6_family: cushort;
    sin6_port: Word;
    sin6_flowinfo: Cardinal;
    sin6_addr: in6_addr;
    sin6_scope_id: Cardinal;
  end;

  // TEMPORARY
  
  Papr_xml_ns_scope = Pointer;

  Pap_method_list_t = Pointer;
  Pcore_output_filter_ctx_t = Pointer;
  Pap_directive_t = Pointer;
  Pap_filter_t = Pointer;
  Papr_file_t = Pointer;
  Papr_off_t = Pointer;

  iovec = record
    /// byte count to read/write
    iov_len: culong;
    /// data to be read/written
    iov_base: PChar;
  end;
  
  Piovec = ^iovec;

{$include apr_errno.inc}
{$include apr_pools.inc}
{$include apr_general.inc}
{$include apr_dso.inc}
{$include apr_user.inc}
{$include apr_time.inc}
{$include apr_tables.inc}
{$include apr_file_info.inc}
{$include apr_file_io.inc}
{$include apr_strings.inc}
{$include apr_lib.inc}
{$include apr_signal.inc}
{$include apr_network_io.inc}
{.$include apr_portable.inc}

{.$include ../aprutils/apr_uri.inc}

{$include apr_thread_proc.inc}
{$include apr_version.inc}
{$include apr_poll.inc}

implementation

{
  Macros transformed into functions in the translation
}

{ apr_pools.inc }

{$ifndef DOXYGEN}
function apr_pool_create(newpool: PPapr_pool_t; parent: Papr_pool_t): apr_status_t;
begin
  Result := apr_pool_create_ex(newpool, parent, nil, nil);
end;
{$endif}

function apr_pool_sub_make(newpool: PPapr_pool_t; parent: Papr_pool_t;
 abort_fn: apr_abortfunc_t): apr_status_t;
begin
  Result := apr_pool_create_ex(newpool, parent, abort_fn, nil);
end;

{ apr_lib.inc }

function apr_tolower(c: Char): Char;
var
  buf: array[0..1] of Char;
begin
  buf[0] := c;
  buf[1] := #0;
  
  buf := StrLower(@buf[0]);
  
  Result := buf[0];
end;

function apr_toupper(c: Char): Char;
var
  buf: array[0..1] of Char;
begin
  buf[0] := c;
  buf[1] := #0;

  buf := StrUpper(@buf[0]);

  Result := buf[0];
end;

end.

