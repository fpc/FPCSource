(*
     This file is part of libmicrohttpd
     Copyright (C) 2007, 2012 Christian Grothoff (and other contributing authors)

     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Lesser General Public
     License as published by the Free Software Foundation; either
     version 2.1 of the License, or (at your option) any later version.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Lesser General Public License for more details.

     You should have received a copy of the GNU Lesser General Public
     License along with this library; if not, write to the Free Software
     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)
(**
 * @file dual_stack_example.pp (Original: dual_stack_example.c)
 * @brief how to use MHD with both IPv4 and IPv6 support (dual-stack)
 * @author Christian Grothoff / Silvio Clécio
 *)

// To test it, just execute: $ curl -g -6 "http://[::1]:8888/"

program dual_stack_example;

{$mode objfpc}{$H+}

uses
  sysutils, libmicrohttpd;

const
  PAGE: Pcchar = '<html><head><title>libmicrohttpd demo</title></head><body>libmicrohttpd demo</body></html>';

  function ahc_echo(cls: Pointer; connection: PMHD_Connection; url: Pcchar;
    method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  const
    aptr: cint = 0;
  var
    me: Pcchar;
    response: PMHD_Response;
    ret: cint;
  begin
    me := cls;
    if 0 <> strcomp(method, 'GET') then
      Exit(MHD_NO); (* unexpected method *)
    if @aptr <> ptr^ then
    begin
      (* do never respond on first call *)
      ptr^ := @aptr;
      Exit(MHD_YES);
    end;
    ptr^ := nil; (* reset when done *)
    response := MHD_create_response_from_buffer(strlen(me), Pointer(me),
                  MHD_RESPMEM_PERSISTENT);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

var
  d: PMHD_Daemon;
begin
  if argc <> 2 then
  begin
    WriteLn(argv[0], ' PORT');
    Halt(1);
  end;
  d := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY or MHD_USE_DEBUG or
         MHD_USE_DUAL_STACK, StrToInt(argv[1]), nil, nil, @ahc_echo, PAGE,
         MHD_OPTION_CONNECTION_TIMEOUT, cuint(120), MHD_OPTION_END);
  ReadLn;
  MHD_stop_daemon(d);
end.

