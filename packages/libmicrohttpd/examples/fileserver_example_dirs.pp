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
 * @file fileserver_example_dirs.pp (Original: fileserver_example_dirs.c)
 * @brief example for how to use libmicrohttpd to serve files (with directory support)
 * @author Christian Grothoff / Silvio Clécio
 *)

program fileserver_example_dirs;

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

  procedure file_free_callback(cls: Pointer); cdecl;
  var
    &file: FILEptr;
  begin
    &file := cls;
    fclose(&file);
  end;

  procedure dir_free_callback(cls: Pointer); cdecl;
  var
    dir: pDir;
  begin
    dir := cls;
    if dir <> nil then
      FpClosedir(dir^);
  end;

  function dir_reader(cls: Pointer; pos: cuint64; buf: Pcchar;
    max: size_t): ssize_t; cdecl;
  var
    dir: pDir;
    e: pDirent;
  begin
    dir := cls;
    if max < 512 then
      Exit(0);
    repeat
      e := FpReaddir(dir^);
      if e = nil then
        Exit(MHD_CONTENT_READER_END_OF_STREAM);
    until not (e^.d_name[0] = '.');
    Result := snprintf(buf, max, '<a href="/%s">%s</a><br>', e^.d_name,
      e^.d_name);
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
    dir: pDir;
    buf: stat;
    emsg: array[0..1023] of AnsiChar;
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
      dir := FpOpendir(PChar('.'));
      if dir = nil then
      begin
        (* most likely cause: more concurrent requests than
           available file descriptors / 2 *)
        snprintf(emsg, SizeOf(emsg), 'Failed to open directory `.'': %s'#10,
          strerror(errno^));
        response := MHD_create_response_from_buffer(strlen(emsg), @emsg,
          MHD_RESPMEM_MUST_COPY);
        if response = nil then
          Exit(MHD_NO);
        ret := MHD_queue_response(connection, MHD_HTTP_SERVICE_UNAVAILABLE,
          response);
        MHD_destroy_response(response);
      end
      else
      begin
        response := MHD_create_response_from_callback(cuint64(MHD_SIZE_UNKNOWN),
                      32 * 1024, @dir_reader, dir, @dir_free_callback);
        if response = nil then
        begin
          FpClosedir(dir^);
          Exit(MHD_NO);
        end;
        ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
        MHD_destroy_response(response);
      end;
    end
    else
    begin
      response := MHD_create_response_from_callback(buf.st_size, 32 * 1024, (* 32k page size *)
                    @file_reader, &file, @file_free_callback);
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

