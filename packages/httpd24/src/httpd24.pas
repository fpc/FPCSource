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

{$PACKENUM 4}

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

{$include util_script.inc}
//{$include util_time.inc}
//{$include util_md5.inc}
//{$include ap_mpm.inc}

// APRUtil External Variables //

var

  {/* All of the bucket types implemented by the core */
  /**
   * The flush bucket type.  This signifies that all data should be flushed to
   * the next filter.  The flush bucket should be sent with the other buckets.
   */}
  apr_bucket_type_flush: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The EOS bucket type.  This signifies that there will be no more data, ever.
   * All filters MUST send all data to the next filter when they receive a
   * bucket of this type
   */}
  apr_bucket_type_eos: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The FILE bucket type.  This bucket represents a file on disk
   */}
  apr_bucket_type_file: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The HEAP bucket type.  This bucket represents a data allocated from the
   * heap.
   */}
  apr_bucket_type_heap: apr_bucket_type_t external LibAPRUtil;
  {$IFDEF APR_HAS_MMAP}
    {/**
     * The MMAP bucket type.  This bucket represents an MMAP'ed file
     */}
    apr_bucket_type_mmap: apr_bucket_type_t external LibAPRUtil;
  {$ENDIF}
  {/**
   * The POOL bucket type.  This bucket represents a data that was allocated
   * from a pool.  IF this bucket is still available when the pool is cleared,
   * the data is copied on to the heap.
   */}
  apr_bucket_type_pool: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The PIPE bucket type.  This bucket represents a pipe to another program.
   */}
  apr_bucket_type_pipe: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The IMMORTAL bucket type.  This bucket represents a segment of data that
   * the creator is willing to take responsibility for.  The core will do
   * nothing with the data in an immortal bucket
   */}
  apr_bucket_type_immortal: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The TRANSIENT bucket type.  This bucket represents a data allocated off
   * the stack.  When the setaside function is called, this data is copied on
   * to the heap
   */}
  apr_bucket_type_transient: apr_bucket_type_t external LibAPRUtil;
  {/**
   * The SOCKET bucket type.  This bucket represents a socket to another machine
   */}
  apr_bucket_type_socket: apr_bucket_type_t external LibAPRUtil;

//********************************************************************
  { from apr_buckets.inc }

  function APR_BRIGADE_SENTINEL(b: Papr_bucket_brigade): Papr_bucket;
  function APR_BRIGADE_FIRST(b: Papr_bucket_brigade): Papr_bucket;
  function APR_BRIGADE_LAST(b: Papr_bucket_brigade): Papr_bucket;
  function APR_BUCKET_NEXT(e: Papr_bucket): Papr_bucket;
  function APR_BUCKET_PREV(e: Papr_bucket): Papr_bucket;
  procedure APR_BUCKET_REMOVE(e: Papr_bucket);
  function APR_BUCKET_IS_METADATA(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_FLUSH(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_EOS(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_FILE(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_PIPE(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_SOCKET(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_HEAP(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_TRANSIENT(e: Papr_bucket): boolean;
  function APR_BUCKET_IS_IMMORTAL(e: Papr_bucket): boolean;
  {$IFDEF APR_HAS_MMAP}
    function APR_BUCKET_IS_MMAP(e: Papr_bucket): boolean;
  {$ENDIF}
  function APR_BUCKET_IS_POOL(e: Papr_bucket): boolean;
  function apr_bucket_read(e: Papr_bucket; const str: PPChar; len: Papr_size_t;
    block: apr_read_type_e): apr_status_t;

  function AP_INIT_TAKE1(directive: Pchar; const take1func : ttake1func;
    mconfig: Pointer; where: Integer; help: Pchar): command_rec;
  function AP_INIT_TAKE2(directive: Pchar; const take2func: ttake2func;
    mconfig: Pointer; where: Integer; help: Pchar): command_rec;
  function AP_INIT_TAKE3(directive: Pchar; const take3func: ttake3func;
    mconfig: Pointer; where: Integer; help: Pchar): command_rec;

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
  { from apr_buckets.inc }

  function APR_BRIGADE_FIRST(b: Papr_bucket_brigade): Papr_bucket; inline;
  begin
    APR_BRIGADE_FIRST := b^.list.next;
  end;

  function APR_BRIGADE_LAST(b: Papr_bucket_brigade): Papr_bucket; inline;
  begin
    APR_BRIGADE_LAST := b^.list.prev;
  end;

  function APR_BRIGADE_SENTINEL(b: Papr_bucket_brigade): Papr_bucket; inline;
  var b_: apr_bucket; // This should technically be <type> and link shouldn't be hard-coded..
  begin
    APR_BRIGADE_SENTINEL := Papr_bucket(pointer(@b^.list.next) - (pointer(@b_.Link) - pointer(@b_) ) );
  end;

  function APR_BUCKET_IS_METADATA(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_METADATA := e^.type_^.is_metadata = APR_BUCKET_METADATA;
  end;

  function APR_BUCKET_IS_FLUSH(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_FLUSH := e^.type_ = @apr_bucket_type_flush;
  end;

  function APR_BUCKET_IS_EOS(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_EOS := e^.type_ = @apr_bucket_type_eos;
  end;

  function APR_BUCKET_IS_FILE(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_FILE := e^.type_ = @apr_bucket_type_file;
  end;

  function APR_BUCKET_IS_PIPE(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_PIPE := e^.type_ = @apr_bucket_type_pipe;
  end;

  function APR_BUCKET_IS_SOCKET(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_SOCKET := e^.type_ = @apr_bucket_type_socket;
  end;

  function APR_BUCKET_IS_HEAP(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_HEAP := e^.type_ = @apr_bucket_type_heap;
  end;

  function APR_BUCKET_IS_TRANSIENT(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_TRANSIENT := e^.type_ = @apr_bucket_type_transient;
  end;

  function APR_BUCKET_IS_IMMORTAL(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_IMMORTAL := e^.type_ = @apr_bucket_type_immortal;
  end;

  {$IFDEF APR_HAS_MMAP}
    function APR_BUCKET_IS_MMAP(e: Papr_bucket): boolean; inline;
    begin
      APR_BUCKET_IS_MMAP := e^.type_ = @apr_bucket_type_mmap;
    end;
  {$ENDIF}

  function APR_BUCKET_IS_POOL(e: Papr_bucket): boolean; inline;
  begin
    APR_BUCKET_IS_POOL := e^.type_ = @apr_bucket_type_pool;
  end;

  function APR_BUCKET_NEXT(e: Papr_bucket): Papr_bucket; inline;
  begin
    APR_BUCKET_NEXT := e^.link.next;
  end;

  function APR_BUCKET_PREV(e: Papr_bucket): Papr_bucket; inline;
  begin
    APR_BUCKET_PREV := e^.link.prev;
  end;

  procedure APR_BUCKET_REMOVE(e: Papr_bucket); inline;
  begin
    APR_BUCKET_PREV(e)^.link.next := APR_BUCKET_NEXT(e);
    APR_BUCKET_NEXT(e)^.link.prev := APR_BUCKET_PREV(e);
  end;

  function apr_bucket_read(e: Papr_bucket; const str: PPChar; len: Papr_size_t;
    block: apr_read_type_e): apr_status_t; inline;
  begin
    apr_bucket_read := e^.type_^.read(e, str, len, block);
  end;

  function AP_INIT_TAKE1(directive: Pchar; const take1func: ttake1func;
    mconfig: Pointer; where: Integer; help: Pchar): command_rec; inline;
  begin
    with result DO
    begin
      name         := directive;
      func.take1   := take1func;
      cmd_data     := mconfig;
      req_override := where;
      args_how     := TAKE1;
      errmsg       := help;
    end;
  end;

  function AP_INIT_TAKE2(directive: Pchar; const take2func: ttake2func;
    mconfig: Pointer; where: Integer; help: Pchar): command_rec; inline;
  begin
    with result DO
    begin
      name         := directive;
      func.take2   := take2func;
      cmd_data     := mconfig;
      req_override := where;
      args_how     := TAKE2;
      errmsg       := help;
    end;
  end;

  function AP_INIT_TAKE3(directive: Pchar; const take3func: ttake3func;
    mconfig: Pointer; where: Integer; help: Pchar): command_rec; inline;
  begin
    with result DO
    begin
      name         := directive;
      func.take3   := take3func;
      cmd_data     := mconfig;
      req_override := where;
      args_how     := TAKE3;
      errmsg       := help;
    end;
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
