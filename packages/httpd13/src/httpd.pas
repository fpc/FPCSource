{
 httpd.pas

 Copyright (C) 2006 Felipe Monteiro de Carvalho

 This unit is a pascal binding for the Apache 1.3.37 headers.
 The headers were released under the following copyright:
}
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
unit httpd;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

{$IFNDEF FPC}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFDEF WIN64}
  {$DEFINE WINDOWS}
{$ENDIF}

{$ifdef Unix}
  {$PACKRECORDS C}
{$endif}

{$define Apache1_3}

interface

uses
{$ifdef WINDOWS}
  Windows,
{$ELSE}
  UnixType,
{$ENDIF}
  ctypes;

const
{$ifndef fpc}
  LineEnding = #13#10;
{$endif}

{$IFDEF WINDOWS}
  LibHTTPD = 'ApacheCore.dll';
{$ELSE}
  LibHTTPD = '';
{$ENDIF}

{ Declarations moved here to be on top of all declarations }

{ Various types}
type
  time_t = LongInt;
  size_t = Integer;

{ configuration vector structure }
type
  ap_conf_vector_t = record end;
  Pap_conf_vector_t = ^ap_conf_vector_t;
  PPap_conf_vector_t = ^Pap_conf_vector_t;

{
  Main httpd header files

  Note: There are more include files other then these, because some include files
 include more files.
}

{.$include ap_provider.inc}
{.$include util_cfgtree.inc}

{$include httpd.inc}
{$include http_config.inc}
{$include http_core.inc}
{$include http_log.inc}
{$include http_main.inc}
{$include http_protocol.inc}
{$include http_request.inc}
{$include http_vhost.inc}

{.$include util_script.inc}
{.$include util_time.inc}
{.$include util_md5.inc}
{.$include ap_mpm.inc}

implementation

{
  Macros transformed into functions in the translation
}

{ from httpd.inc }

{ Internal representation for a HTTP protocol number, e.g., HTTP/1.1 }
function HTTP_VERSION(major, minor: Integer): Integer;
begin
  Result := (1000*(major)+(minor));
end;

{ Major part of HTTP protocol }
function HTTP_VERSION_MAJOR(number: Integer): Integer;
begin
  Result := number div 1000;
end;

{ Minor part of HTTP protocol }
function HTTP_VERSION_MINOR(number: Integer): Integer;
begin
  Result := number mod 1000;
end;

{function ap_escape_uri(p: Papr_pool_t; const path: PChar): PChar;
begin
  Result := ap_os_escape_path(p, path, 1);
end;}

{ from http_config.inc }

{ Use this in all standard modules }

procedure STANDARD_MODULE_STUFF(var mod_: module);
begin
  mod_.version := MODULE_MAGIC_NUMBER_MAJOR;
  mod_.minor_version := MODULE_MAGIC_NUMBER_MINOR;
  mod_.module_index := -1;
//  mod_.name: PChar;
  mod_.dynamic_load_handle := nil;
  mod_.next := nil;
  mod_.magic := MODULE_MAGIC_COOKIE;
end;

{ Use this only in MPMs }
//procedure MPM20_MODULE_STUFF(var mod_: module);
//begin
//  mod_.version := MODULE_MAGIC_NUMBER_MAJOR;
//  mod_.minor_version := MODULE_MAGIC_NUMBER_MINOR;
//  mod_.module_index := -1;
//  mod_.name: PChar;
//  mod_.dynamic_load_handle := nil;
//  mod_.next := nil;
//  mod_.magic := MODULE_MAGIC_COOKIE;
//end;

function ap_get_module_config(v: Pap_conf_vector_t; m: Pmodule): Pap_conf_vector_t;
begin
  Result := Pointer(PtrInt(v) + m^.module_index);
end;

procedure ap_set_module_config(v: Pap_conf_vector_t; m: Pmodule; val: Pap_conf_vector_t);
var
  P: PPointer;
begin
  P := PPointer(PtrInt(v) + m^.module_index);
  P^ := val;
end;

end.

