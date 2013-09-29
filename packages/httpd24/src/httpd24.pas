{
 httpd.pas

 Copyright (C) 2006 Felipe Monteiro de Carvalho
 (based on the Apache 2.0.58 headers)
 Updated by Attila Borka in 2012 for the Apache 2.4.3 headers

 This unit is a pascal binding for the Apache 2.4.3 headers.
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

{*
 * @file httpd.h
 * @brief HTTP Daemon routines
 *
 * @defgroup APACHE Apache HTTP Server
 *
 * Top level group of which all other groups are a member
 * @
 *
 * @defgroup APACHE_MODS Loadable modules
 *           Top level group for modules
 * @defgroup APACHE_OS Operating System Specific
 * @defgroup APACHE_INTERNAL Internal interfaces
 * @defgroup APACHE_CORE Core routines
 * @
 * @defgroup APACHE_CORE_DAEMON HTTP Daemon Routine
 * @
  }
unit httpd24;
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}
{$ifdef Unix}
  {$PACKRECORDS C}
{$endif}

{$IFDEF Apache1_3}
  {$WARNING Apache1_3 is defined somewhere, but the HTTPD unit included is for Apache2_4}
{$ENDIF}
{$IFDEF Apache2_0}
  {$WARNING Apache2_0 is defined somewhere, but the HTTPD unit included is for Apache2_4}
{$ENDIF}
{$IFDEF Apache2_2}
  {$WARNING Apache2_2 is defined somewhere, but the HTTPD unit included is for Apache2_4}
{$ENDIF}

{$IFDEF FPCAPACHE_1_3}
  {$WARNING FPCAPACHE_1_3 is defined somewhere, but the HTTPD unit included is for FPCAPACHE_2_4}
{$ENDIF}
{$IFDEF FPCAPACHE_2_0}
  {$WARNING FPCAPACHE_2_0 is defined somewhere, but the HTTPD unit included is for FPCAPACHE_2_4}
{$ENDIF}
{$IFDEF FPCAPACHE_2_2}
  {$WARNING FPCAPACHE_2_2 is defined somewhere, but the HTTPD unit included is for FPCAPACHE_2_4}
{$ENDIF}

{$DEFINE Apache2_4}
{$DEFINE FPCAPACHE_2_4}

interface

uses
{$ifdef WINDOWS}
  Windows,
{$ELSE}
  UnixType,
{$ENDIF}
  ctypes, apr24;

const
{$ifndef fpc}
  LineEnding = #13#10;
{$endif}

{$IFDEF WINDOWS}
  LibHTTPD = 'libhttpd.dll';
{$ELSE}
  LibHTTPD = '';
{$ENDIF}

{$IFDEF WINDOWS}
  LibAPRUtil = 'libaprutil-1.dll';
{$ELSE}
  LibAPRUtil = '';
{$ENDIF}

type
  { configuration vector structure , moved from http_config.inc (http_config.h)}
  ap_conf_vector_t = record end;
  Pap_conf_vector_t = ^ap_conf_vector_t;
  PPap_conf_vector_t = ^Pap_conf_vector_t;

  {*
   Shortcuts for FPC, so no extra includes are needed.
   It would require more of the header files from the Apache httpd, apr and apr-util
   source code packages.
   *}
  {apr_thread_mutex_t is OS dependent, found in apr-X.X.X/include/arch/.../apr_arch_thread_mutex.h}
  Papr_thread_mutex_t = Pointer;//^apr_thread_mutex_t;   used in http.inc -> request_rec record

  {from apr-X.X.X/include/apr_network_io.h  used in server_addr_rec record in httpd.inc}
  Papr_sockaddr_t = Pointer;//^apr_sockaddr_t
  apr_port_t = word;//apr_uint16_t
  {end apr_network_io.h}

  { A structure to represent sockets }
  apr_socket_t = record end;
  Papr_socket_t = ^apr_socket_t;
  PPapr_socket_t = ^Papr_socket_t;
  {end apr_network_io.h}

  {from apr-X.X.X/include/apr_thread_proc.h , used in http_log.h (http_log.inc)}
  apr_cmdtype_e = (
      APR_SHELLCMD,           //**< use the shell to invoke the program */
      APR_PROGRAM,            //**< invoke the program directly, no copied env */
      APR_PROGRAM_ENV,        //**< invoke the program, replicating our environment */
      APR_PROGRAM_PATH,       //**< find program on PATH, use our environment */
      APR_SHELLCMD_ENV        {/**< use the shell to invoke the program,
                               *   replicating our environment
                               *}
  );
  {*
   end Shortcuts for FPC
   *}
{
  Main httpd header files

  Note: There are more include files other then these, because some include files
        include more files.
}

//{$include ap_provider.inc}
{$include util_cfgtree.inc}

{$include httpd.inc}
{$include http_config.inc}
{$include http_core.inc}
{$include http_log.inc}
//{$include http_main.inc}
{$include http_protocol.inc}
//{$include http_request.inc}
//{$include http_connection.inc}
//{$include http_vhost.inc}

//{$include util_script.inc}
//{$include util_time.inc}
//{$include util_md5.inc}
//{$include ap_mpm.inc}

implementation
  { Internal representation for a HTTP protocol number, e.g., HTTP/1.1 }
  function HTTP_VERSION(major, minor: Integer): Integer;
  begin
    HTTP_VERSION := (1000 * major + minor);
  end;

  { Major part of HTTP protocol }
  function HTTP_VERSION_MAJOR(number: Integer): Integer;
  begin
    HTTP_VERSION_MAJOR := number div 1000;
  end;

  { Minor part of HTTP protocol }
  function HTTP_VERSION_MINOR(number: Integer): Integer;
  begin
    HTTP_VERSION_MINOR := number mod 1000;
  end;

  function ap_is_HTTP_INFO(x : Integer): Boolean;  
  begin
    ap_is_HTTP_INFO := ((x>=100) and (x<200));
  end;

  function ap_is_HTTP_SUCCESS(x : Integer) : Boolean;
  begin
    ap_is_HTTP_SUCCESS := ((x>=200) and (x<300));
  end;

  function ap_is_HTTP_REDIRECT(x : Integer) : Boolean;
  begin
    ap_is_HTTP_REDIRECT := ((x>=300) and (x<400));
  end;

  function ap_is_HTTP_ERROR(x : Integer) : Boolean;
  begin
    ap_is_HTTP_ERROR := ((x>=400) and (x<600));
  end;

  function ap_is_HTTP_CLIENT_ERROR(x : Integer) : Boolean;
  begin
    ap_is_HTTP_CLIENT_ERROR := ((x>=400) and (x<500));
  end;

  function ap_is_HTTP_SERVER_ERROR(x : Integer) : Boolean;
  begin
    ap_is_HTTP_SERVER_ERROR := ((x>=500) and (x<600));
  end;

  function ap_is_HTTP_VALID_RESPONSE(x : Integer) : Boolean;
  begin
    ap_is_HTTP_VALID_RESPONSE := ((x>=100) and (x<600));
  end;

  function ap_status_drops_connection(x : Integer): Boolean;
  begin
    case x of
      HTTP_BAD_REQUEST,
      HTTP_REQUEST_TIME_OUT,
      HTTP_LENGTH_REQUIRED,
      HTTP_REQUEST_ENTITY_TOO_LARGE,
      HTTP_REQUEST_URI_TOO_LARGE,
      HTTP_INTERNAL_SERVER_ERROR,
      HTTP_SERVICE_UNAVAILABLE,
      HTTP_NOT_IMPLEMENTED:
        Result := true;
      else
        Result := false;
    end;
  end;

  function ap_escape_uri(ppool: Papr_pool_t; const path: PChar) : PChar;  
  begin
    ap_escape_uri:=ap_os_escape_path(ppool,path,1);
  end;

  function ap_escape_html(p: Papr_pool_t; const s: PChar) : PChar;
  begin
    ap_escape_html:=ap_escape_html2(p,s,0);
  end;

//********************************************************************
  { from http_config.inc }

  { Use this in all standard modules }
  procedure STANDARD20_MODULE_STUFF(var mod_: module);
  begin
    mod_.version := MODULE_MAGIC_NUMBER_MAJOR;
    mod_.minor_version := MODULE_MAGIC_NUMBER_MINOR;
    mod_.module_index := -1;
  //  mod_.name: PChar;
    mod_.dynamic_load_handle := nil;
    mod_.next := nil;
    mod_.magic := MODULE_MAGIC_COOKIE;
    mod_.rewrite_args := nil;
  end;

  { Use this only in MPMs }
  procedure MPM20_MODULE_STUFF(var mod_: module);
  begin
    mod_.version := MODULE_MAGIC_NUMBER_MAJOR;
    mod_.minor_version := MODULE_MAGIC_NUMBER_MINOR;
    mod_.module_index := -1;
  //  mod_.name: PChar;
    mod_.dynamic_load_handle := nil;
    mod_.next := nil;
    mod_.magic := MODULE_MAGIC_COOKIE;
  end;

end.
