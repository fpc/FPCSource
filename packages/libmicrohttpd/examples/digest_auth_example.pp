(*
     This file is part of libmicrohttpd
     Copyright (C) 2010 Christian Grothoff (and other contributing authors)

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
 * @file digest_auth_example.pp (Original: digest_auth_example.c)
 * @brief minimal example for how to use digest auth with libmicrohttpd
 * @author Amr Ali / Silvio Clécio
 *)

program digest_auth_example;

{$mode objfpc}{$H+}

uses
  sysutils, BaseUnix, cmem, cutils, libmicrohttpd;

const
  PAGE: Pcchar = '<html><head><title>libmicrohttpd demo</title></head><body>Access granted</body></html>';
  DENIED: Pcchar = '<html><head><title>libmicrohttpd demo</title></head><body>Access denied</body></html>';
  MY_OPAQUE_STR = '11733b200778ce33060f31c9af70a870ba96ddd4';

  function ahc_echo(cls: Pointer; connection: PMHD_Connection; url: Pcchar;
    method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  const
    password: Pcchar = 'testpass';
    realm: Pcchar = 'test@example.com';
  var
    response: PMHD_Response;
    username: Pcchar;
    ret: cint;
    signal_stale: cint;
  begin
    username := MHD_digest_auth_get_username(connection);
    if username = nil then
    begin
      response := MHD_create_response_from_buffer(strlen(DENIED), DENIED,
                    MHD_RESPMEM_PERSISTENT);
      ret := MHD_queue_auth_fail_response(connection, realm, MY_OPAQUE_STR,
               response, MHD_NO);
      MHD_destroy_response(response);
      Exit(ret);
    end;
    ret := MHD_digest_auth_check(connection, realm, username, password, 300);
    Free(username);
    if (ret = MHD_INVALID_NONCE) or (ret = MHD_NO) then
    begin
      response := MHD_create_response_from_buffer(strlen(DENIED), DENIED,
                    MHD_RESPMEM_PERSISTENT);
      if nil = response then
        Exit(MHD_NO);
      if ret = MHD_INVALID_NONCE then
        signal_stale := MHD_YES
      else
        signal_stale := MHD_NO;
      ret := MHD_queue_auth_fail_response(connection, realm, MY_OPAQUE_STR,
               response, signal_stale);
      MHD_destroy_response(response);
      Exit(ret);
    end;
    response := MHD_create_response_from_buffer(strlen(PAGE), PAGE,
      MHD_RESPMEM_PERSISTENT);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

var
  fd: cint;
  rnd: array[0..7] of AnsiChar;
  len: ssize_t;
  off: size_t;
  d: PMHD_Daemon;
begin
  if argc <> 2 then
  begin
    WriteLn(argv[0], ' PORT');
    Halt(1);
  end;
  fd := FpOpen('/dev/urandom', O_RDONLY);
  if -1 = fd then
  begin
    WriteLn(stderr, Format('Failed to open `%s'': %s', [
      '/dev/urandom', strerror(errno^)]));
    Halt(1);
  end;
  off := 0;
  while off < 8 do
  begin
    len := FpRead(fd, rnd, 8);
    if len = -1 then
    begin
      WriteLn(stderr, Format('Failed to read `%s'': %s', [
        '/dev/urandom', strerror(errno^)]));
      FpClose(fd);
      Halt(1);
    end;
    off += len;
  end;
  FpClose(fd);
  d := MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION or MHD_USE_DEBUG,
         StrToInt(argv[1]), nil, nil, @ahc_echo, PAGE,
         MHD_OPTION_DIGEST_AUTH_RANDOM, SizeOf(rnd), rnd,
         MHD_OPTION_NONCE_NC_SIZE, 300,
         MHD_OPTION_CONNECTION_TIMEOUT, cuint(120),
         MHD_OPTION_END);
  if d = nil then
    Halt(1);
  ReadLn;
  MHD_stop_daemon (d);
end.

