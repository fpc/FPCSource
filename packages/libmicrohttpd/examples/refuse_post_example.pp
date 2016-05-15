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
 * @file refuse_post_example.pp (Original: refuse_post_example.c)
 * @brief example for how to refuse a POST request properly
 * @author Christian Grothoff, Sebastian Gerhardt and Silvio Clécio
 *)

program refuse_post_example;

{$mode objfpc}{$H+}

uses
  sysutils, libmicrohttpd;

const
  askpage: Pcchar =
    '<html><body>'#10+
    'Upload a file, please!<br>'#10+
    '<form action="/filepost" method="post" enctype="multipart/form-data">'#10+
    '<input name="file" type="file">'#10+
    '<input type="submit" value=" Send "></form>'#10+
    '</body></html>';

  BUSYPAGE: Pcchar = '<html><head><title>Webserver busy</title></head><body>We are too busy to process POSTs right now.</body></html>';

  function ahc_echo(cls:  Pointer; connection: PMHD_Connection; url: Pcchar;
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
    if (0 <> strcomp(method, 'GET')) and (0 <> strcomp(method, 'POST')) then
      Exit(MHD_NO); (* unexpected method *)
    if @aptr <> ptr^ then
    begin
      ptr^ := @aptr;
      (* always to busy for POST requests *)
      if 0 = strcomp(method, 'POST') then
      begin
        response := MHD_create_response_from_buffer(strlen(BUSYPAGE),
                      Pointer(BUSYPAGE), MHD_RESPMEM_PERSISTENT);
        ret := MHD_queue_response (connection, MHD_HTTP_SERVICE_UNAVAILABLE,
                 response);
        MHD_destroy_response (response);
        Exit(ret);
      end;
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
  d := MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION or MHD_USE_DEBUG,
         StrToInt(argv[1]), nil, nil, @ahc_echo, Pointer(askpage),
         MHD_OPTION_END);
  if d = nil then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(d);
end.

