(*
     This file is part of libmicrohttpd
     Copyright (C) 2007 Christian Grothoff (and other contributing authors)

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
 * @file fileserver_example.pp (Original: fileserver_example.c)
 * @brief minimal example for how to use libmicrohttpd to serve files
 * @author Christian Grothoff / Silvio Clécio
 *)

program fileserver_example;

{$mode objfpc}{$H+}

uses
  sysutils, BaseUnix, cutils, libmicrohttpd;

const
  PAGE: Pcchar = '<html><head><title>File not found</title></head><body>File not found</body></html>';

  function file_reader(cls: Pointer; pos: cuint64; buf: Pcchar;
    max: size_t): ssize_t; cdecl;
  var
    &file: FILEptr;
  begin
    &file := cls;
    fseek(&file, pos, SEEK_SET);
    Result := fread(buf, 1, max, &file);
  end;

  procedure free_callback(cls: Pointer); cdecl;
  var
    &file: FILEptr;
  begin
    &file := cls;
    fclose(&file);
  end;

  function ahc_echo(cls: Pointer; connection: PMHD_Connection; url: Pcchar;
    method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  const
    aptr: cint = 0;
  var
    response: PMHD_Response;
    ret: cint;
    &file: FILEptr;
    buf: stat;
  begin
    if (0 <> strcomp(method, MHD_HTTP_METHOD_GET)) and
      (0 <> strcomp(method, MHD_HTTP_METHOD_HEAD)) then
      Exit(MHD_NO); (* unexpected method *)
    if @aptr <> ptr^ then
    begin
      (* do never respond on first call *)
      ptr^ := @aptr;
      Exit(MHD_YES);
    end;
    ptr^ := nil; (* reset when done *)
    if 0 = FpStat(@url[1], buf) then
      &file := fopen(@url[1], fopenread)
    else
      &file := nil;
    if nil = &file then
    begin
      response := MHD_create_response_from_buffer(strlen(PAGE), Pointer(PAGE),
                    MHD_RESPMEM_PERSISTENT);
      ret := MHD_queue_response(connection, MHD_HTTP_NOT_FOUND, response);
      MHD_destroy_response(response);
    end
    else
    begin
      response := MHD_create_response_from_callback(buf.st_size, 32 * 1024, (* 32k page size *)
                    @file_reader, &file, @free_callback);
      if nil = response then
      begin
        fclose(&file);
        Exit(MHD_NO);
      end;
      ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
      MHD_destroy_response(response);
    end;
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
         StrToInt(argv[1]), nil, nil, @ahc_echo, PAGE, MHD_OPTION_END);
  if d = nil then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(d);
end.

