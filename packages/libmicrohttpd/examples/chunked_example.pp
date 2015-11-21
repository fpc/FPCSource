(*
     This file is part of libmicrohttpd
     Copyright (C) 2015 Christian Grothoff (and other contributing authors)

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
 * @file chunked_example.pp (original: chunked_example.c)
 * @brief example for generating chunked encoding with libmicrohttpd
 * @author Christian Grothoff / Silvio Clécio / Gilson Nunes
 *)

program chunked_example;

{$mode objfpc}{$H+}

uses
  sysutils, libmicrohttpd;

  function callback(cls: Pointer; pos: cuint64; buf: Pcchar;
    max: size_t): ssize_t; cdecl;
  begin
    Result := MHD_CONTENT_READER_END_OF_STREAM;
  end;

  function ahc_echo(cls:  Pointer; connection: PMHD_Connection; url: Pcchar;
    method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  const
    aptr: cint = 0;
  var
    response: PMHD_Response;
    ret: cint;
  begin
    if 0 <> strcomp(method, 'GET') then
      Exit(MHD_NO);
    if @aptr <> ptr^ then
    begin
      ptr^ := @aptr;
      Exit(MHD_YES);
    end;
    ptr^ := nil;
    response := MHD_create_response_from_callback(UInt64(MHD_SIZE_UNKNOWN), 1024,
      @callback, nil, nil);
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
  d := MHD_start_daemon(// MHD_USE_SELECT_INTERNALLY or MHD_USE_DEBUG or MHD_USE_POLL,
         MHD_USE_SELECT_INTERNALLY or MHD_USE_DEBUG,
         // MHD_USE_THREAD_PER_CONNECTION or MHD_USE_DEBUG or MHD_USE_POLL,
         // MHD_USE_THREAD_PER_CONNECTION or MHD_USE_DEBUG,
         StrToInt(argv[1]), nil, nil, @ahc_echo, nil,
         MHD_OPTION_CONNECTION_TIMEOUT, 120, MHD_OPTION_END);
  if d = nil then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(d);
end.

