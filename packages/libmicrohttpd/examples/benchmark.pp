(*
     This file is part of libmicrohttpd
     Copyright (C) 2007, 2013 Christian Grothoff (and other contributing authors)

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
 * @file benchmark.pp (Original: benchmark.c)
 * @brief minimal code to benchmark MHD GET performance
 * @author Christian Grothoff / Silvio Clécio
 *)

program benchmark;

{$mode objfpc}{$H+}
{$MACRO ON}
{$IF DEFINED(CPU_COUNT) and (CPU_COUNT + 0) < 2}
  {$UNDEF CPU_COUNT}
{$ENDIF}
{$IF NOT DEFINED(CPU_COUNT)}
  {$DEFINE CPU_COUNT := 2}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  WinSock2,
{$ELSE}
  BaseUnix, Unix,
{$ENDIF}
  cmem, sysutils, cutils, libmicrohttpd;

const
  PAGE: Pcchar = '<html><head><title>libmicrohttpd demo</title></head><body>libmicrohttpd demo</body></html>';
  SMALL = 1024 * 128;
  NUMBER_OF_THREADS = CPU_COUNT;

var
  small_deltas: array[0..SMALL] of cuint;
  response: PMHD_Response;

  procedure completed_callback(cls: Pointer; connection: PMHD_Connection;
    con_cls: PPointer; toe: MHD_RequestTerminationCode); cdecl;
  var
    tv: ptimeval;
    tve: timeval;
    delta: cuint64;
  begin
    tv := con_cls^;
    if nil = tv then
      Exit;
    fpgettimeofday(@tve, nil);
    delta := 0;
    if tve.tv_usec >= tv^.tv_usec then
      delta += (tve.tv_sec - tv^.tv_sec) * 1000000 +
        (tve.tv_usec - tv^.tv_usec)
    else
      delta += (tve.tv_sec - tv^.tv_sec) * 1000000 -
        tv^.tv_usec + tve.tv_usec;
    if delta < SMALL then
      Inc(small_deltas[delta])
    else
      WriteLn(stdout, Format('D: %u 1', [delta]));
    Free(tv);
  end;

  function uri_logger_cb(cls: Pointer; uri: Pcchar): Pointer; cdecl;
  var
    tv: ptimeval;
  begin
    tv := Malloc(SizeOf(timeval));
    if nil <> tv then
      fpgettimeofday(tv, nil);
    Result := tv;
  end;

  function ahc_echo(cls: Pointer; connection: PMHD_Connection; url: Pcchar;
    method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; ptr: PPointer): cint; cdecl;
  begin
    if 0 <> strcomp(method, 'GET') then
      Exit(MHD_NO);
    Result := MHD_queue_response(connection, MHD_HTTP_OK, response);
  end;

var
  d: PMHD_Daemon;
  i: cuint;
begin
  if argc <> 2 then
  begin
    WriteLn(argv[0] + ' PORT');
    Halt(1);
  end;
  response := MHD_create_response_from_buffer(Length(PAGE), Pointer(PAGE),
    MHD_RESPMEM_PERSISTENT);
{$IF 0}
  MHD_add_response_header (response, MHD_HTTP_HEADER_CONNECTION, 'close');
{$ENDIF}
  d := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY or MHD_SUPPRESS_DATE_NO_CLOCK
{$IFDEF EPOLL_SUPPORT}
         or MHD_USE_EPOLL_LINUX_ONLY or MHD_USE_EPOLL_TURBO
{$ENDIF},
         StrToInt(argv[1]), nil, nil, @ahc_echo, nil,
         MHD_OPTION_CONNECTION_TIMEOUT, 120,
         MHD_OPTION_THREAD_POOL_SIZE, NUMBER_OF_THREADS,
         MHD_OPTION_URI_LOG_CALLBACK, @uri_logger_cb, nil,
         MHD_OPTION_NOTIFY_COMPLETED, @completed_callback, nil,
         MHD_OPTION_CONNECTION_LIMIT, 1000,
         MHD_OPTION_END);
  if d = nil then
    Halt(1);
  ReadLn;
  MHD_stop_daemon(d);
  MHD_destroy_response(response);
  for i := 0 to SMALL do
    if 0 <> small_deltas[i] then
      WriteLn(stdout, Format('D: %d %u', [i, small_deltas[i]]));
end.

