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
 * @file fileserver_example_external_select.pp (Original: fileserver_example_external_select.c)
 * @brief minimal example for how to use libmicrohttpd to server files
 * @author Christian Grothoff / Silvio Clécio
 *)

program fileserver_example_external_select;

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
    if 0 <> strcomp(method, MHD_HTTP_METHOD_GET) then
      Exit(MHD_NO); (* unexpected method *)
    if @aptr <> ptr^ then
    begin
      (* do never respond on first call *)
      ptr^ := @aptr;
      Exit(MHD_YES);
    end;
    ptr^ := nil; (* reset when done *)
    if (0 = FpStat(@url[1], buf)) and fpS_ISREG(buf.st_mode) then
      &file := fopen(@url[1], fopenread)
    else
      &file := nil;
    if &file = nil then
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
      if response = nil then
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
  &end: time_t;
  t: time_t;
  tv: timeval;
  rs: TFDSet;
  ws: TFDSet;
  es: TFDSet;
  max: MHD_socket;
  mhd_timeout: MHD_UNSIGNED_LONG_LONG;
begin
  if argc <> 3 then
  begin
    WriteLn(argv[0], ' PORT SECONDS-TO-RUN');
    Halt(1);
  end;
  d := MHD_start_daemon(MHD_USE_DEBUG, StrToInt(argv[1]), nil, nil, @ahc_echo,
         PAGE, MHD_OPTION_END);
  if d = nil then
    Halt(1);
  &end := fptime + StrToInt(argv[2]);
  while True do
  begin
    t := fptime;
    if not (t < &end) then
      Break;
    tv.tv_sec := &end - t;
    tv.tv_usec := 0;
    max := 0;
    fpFD_ZERO(rs);
    fpFD_ZERO(ws);
    fpFD_ZERO(es);
    if MHD_YES <> MHD_get_fdset (d, @rs, @ws, @es, @max) then
      Break; (* fatal internal error *)
    if MHD_get_timeout(d, @mhd_timeout) = MHD_YES then
    begin
      if MHD_UNSIGNED_LONG_LONG(tv.tv_sec) < mhd_timeout div clonglong(1000) then
      begin
        tv.tv_sec := mhd_timeout div clonglong(1000);
        tv.tv_usec := (mhd_timeout - (tv.tv_sec * clonglong(1000))) * clonglong(1000);
      end;
    end;
    fpSelect(max + 1, @rs, @ws, @es, @tv);
    MHD_run(d);
  end;
  MHD_stop_daemon(d);
end.

