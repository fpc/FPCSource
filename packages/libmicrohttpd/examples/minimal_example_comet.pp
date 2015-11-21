(*
     This file is part of libmicrohttpd
     Copyright (C) 2007, 2008 Christian Grothoff (and other contributing authors)

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
 * @file minimal_example.pp (original: minimal_example.c)
 * @brief minimal example for how to generate an infinite stream with libmicrohttpd
 * @author Christian Grothoff / Silvio Clécio / Gilson Nunes
 *)

program minimal_example_comet;

{$mode objfpc}{$H+}

uses
  sysutils, cutils, libmicrohttpd;

  function data_generator(cls: Pointer; pos: cuint64; buf: Pcchar;
    max: size_t): ssize_t; cdecl;
  begin
    if max < 80 then
      Exit(0);
    memset(buf, Ord('A'), max - 1);
    buf[79] := #10;
    Exit(80);
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
    response := MHD_create_response_from_callback(UInt64(MHD_SIZE_UNKNOWN), 80,
      @data_generator, nil, nil);
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
  d := MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION or MHD_USE_DEBUG,
         StrToInt(argv[1]), nil, nil, @ahc_echo, nil, MHD_OPTION_END);
  if d = nil then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(d);
end.

