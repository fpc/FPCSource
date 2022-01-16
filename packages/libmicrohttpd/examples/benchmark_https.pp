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
 * @file benchmark_https.pp (Original: benchmark_https.c)
 * @brief minimal code to benchmark MHD GET performance with HTTPS
 * @author Christian Grothoff / Silvio Clécio
 *)

program benchmark_https;

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

const
  srv_signed_key_pem: array[0..1674] of AnsiChar =
    '-----BEGIN RSA PRIVATE KEY-----'#10+
    'MIIEowIBAAKCAQEAvfTdv+3fgvVTKRnP/HVNG81cr8TrUP/iiyuve/THMzvFXhCW'#10+
    '+K03KwEku55QvnUndwBfU/ROzLlv+5hotgiDRNFT3HxurmhouySBrJNJv7qWp8IL'#10+
    'q4sw32vo0fbMu5BZF49bUXK9L3kW2PdhTtSQPWHEzNrCxO+YgCilKHkY3vQNfdJ0'#10+
    '20Q5EAAEseD1YtWCIpRvJzYlZMpjYB1ubTl24kwrgOKUJYKqM4jmF4DVQp4oOK/6'#10+
    'QYGGh1QmHRPAy3CBII6sbb+sZT9cAqU6GYQVB35lm4XAgibXV6KgmpVxVQQ69U6x'#10+
    'yoOl204xuekZOaG9RUPId74Rtmwfi1TLbBzo2wIDAQABAoIBADu09WSICNq5cMe4'#10+
    '+NKCLlgAT1NiQpLls1gKRbDhKiHU9j8QWNvWWkJWrCya4QdUfLCfeddCMeiQmv3K'#10+
    'lJMvDs+5OjJSHFoOsGiuW2Ias7IjnIojaJalfBml6frhJ84G27IXmdz6gzOiTIer'#10+
    'DjeAgcwBaKH5WwIay2TxIaScl7AwHBauQkrLcyb4hTmZuQh6ArVIN6+pzoVuORXM'#10+
    'bpeNWl2l/HSN3VtUN6aCAKbN/X3o0GavCCMn5Fa85uJFsab4ss/uP+2PusU71+zP'#10+
    'sBm6p/2IbGvF5k3VPDA7X5YX61sukRjRBihY8xSnNYx1UcoOsX6AiPnbhifD8+xQ'#10+
    'Tlf8oJUCgYEA0BTfzqNpr9Wxw5/QXaSdw7S/0eP5a0C/nwURvmfSzuTD4equzbEN'#10+
    'd+dI/s2JMxrdj/I4uoAfUXRGaabevQIjFzC9uyE3LaOyR2zhuvAzX+vVcs6bSXeU'#10+
    'pKpCAcN+3Z3evMaX2f+z/nfSUAl2i4J2R+/LQAWJW4KwRky/m+cxpfUCgYEA6bN1'#10+
    'b73bMgM8wpNt6+fcmS+5n0iZihygQ2U2DEud8nZJL4Nrm1dwTnfZfJBnkGj6+0Q0'#10+
    'cOwj2KS0/wcEdJBP0jucU4v60VMhp75AQeHqidIde0bTViSRo3HWKXHBIFGYoU3T'#10+
    'LyPyKndbqsOObnsFXHn56Nwhr2HLf6nw4taGQY8CgYBoSW36FLCNbd6QGvLFXBGt'#10+
    '2lMhEM8az/K58kJ4WXSwOLtr6MD/WjNT2tkcy0puEJLm6BFCd6A6pLn9jaKou/92'#10+
    'SfltZjJPb3GUlp9zn5tAAeSSi7YMViBrfuFiHObij5LorefBXISLjuYbMwL03MgH'#10+
    'Ocl2JtA2ywMp2KFXs8GQWQKBgFyIVv5ogQrbZ0pvj31xr9HjqK6d01VxIi+tOmpB'#10+
    '4ocnOLEcaxX12BzprW55ytfOCVpF1jHD/imAhb3YrHXu0fwe6DXYXfZV4SSG2vB7'#10+
    'IB9z14KBN5qLHjNGFpMQXHSMek+b/ftTU0ZnPh9uEM5D3YqRLVd7GcdUhHvG8P8Q'#10+
    'C9aXAoGBAJtID6h8wOGMP0XYX5YYnhlC7dOLfk8UYrzlp3xhqVkzKthTQTj6wx9R'#10+
    'GtC4k7U1ki8oJsfcIlBNXd768fqDVWjYju5rzShMpo8OCTS6ipAblKjCxPPVhIpv'#10+
    'tWPlbSn1qj6wylstJ5/3Z+ZW5H4wIKp5jmLiioDhcP0L/Ex3Zx8O'#10+
    '-----END RSA PRIVATE KEY-----'#10;

  srv_signed_cert_pem: array[0..1138] of AnsiChar =
    '-----BEGIN CERTIFICATE-----'#10+
    'MIIDGzCCAgWgAwIBAgIES0KCvTALBgkqhkiG9w0BAQUwFzEVMBMGA1UEAxMMdGVz'#10+
    'dF9jYV9jZXJ0MB4XDTEwMDEwNTAwMDcyNVoXDTQ1MDMxMjAwMDcyNVowFzEVMBMG'#10+
    'A1UEAxMMdGVzdF9jYV9jZXJ0MIIBHzALBgkqhkiG9w0BAQEDggEOADCCAQkCggEA'#10+
    'vfTdv+3fgvVTKRnP/HVNG81cr8TrUP/iiyuve/THMzvFXhCW+K03KwEku55QvnUn'#10+
    'dwBfU/ROzLlv+5hotgiDRNFT3HxurmhouySBrJNJv7qWp8ILq4sw32vo0fbMu5BZ'#10+
    'F49bUXK9L3kW2PdhTtSQPWHEzNrCxO+YgCilKHkY3vQNfdJ020Q5EAAEseD1YtWC'#10+
    'IpRvJzYlZMpjYB1ubTl24kwrgOKUJYKqM4jmF4DVQp4oOK/6QYGGh1QmHRPAy3CB'#10+
    'II6sbb+sZT9cAqU6GYQVB35lm4XAgibXV6KgmpVxVQQ69U6xyoOl204xuekZOaG9'#10+
    'RUPId74Rtmwfi1TLbBzo2wIDAQABo3YwdDAMBgNVHRMBAf8EAjAAMBMGA1UdJQQM'#10+
    'MAoGCCsGAQUFBwMBMA8GA1UdDwEB/wQFAwMHIAAwHQYDVR0OBBYEFOFi4ilKOP1d'#10+
    'XHlWCMwmVKr7mgy8MB8GA1UdIwQYMBaAFP2olB4s2T/xuoQ5pT2RKojFwZo2MAsG'#10+
    'CSqGSIb3DQEBBQOCAQEAHVWPxazupbOkG7Did+dY9z2z6RjTzYvurTtEKQgzM2Vz'#10+
    'GQBA+3pZ3c5mS97fPIs9hZXfnQeelMeZ2XP1a+9vp35bJjZBBhVH+pqxjCgiUflg'#10+
    'A3Zqy0XwwVCgQLE2HyaU3DLUD/aeIFK5gJaOSdNTXZLv43K8kl4cqDbMeRpVTbkt'#10+
    'YmG4AyEOYRNKGTqMEJXJoxD5E3rBUNrVI/XyTjYrulxbNPcMWEHKNeeqWpKDYTFo'#10+
    'Bb01PCthGXiq/4A2RLAFosadzRa8SBpoSjPPfZ0b2w4MJpReHqKbR5+T2t6hzml6'#10+
    '4ToyOKPDmamiTuN5KzLN3cw7DQlvWMvqSOChPLnA3Q=='#10+
    '-----END CERTIFICATE-----'#10;


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
  d := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY or MHD_USE_SSL
{$IFDEF EPOLL_SUPPORT}
         or MHD_USE_EPOLL_LINUX_ONLY or MHD_USE_EPOLL_TURBO
{$ENDIF},
         StrToInt(argv[1]), nil, nil, @ahc_echo, nil,
         MHD_OPTION_CONNECTION_TIMEOUT, 120,
         MHD_OPTION_THREAD_POOL_SIZE, NUMBER_OF_THREADS,
         MHD_OPTION_URI_LOG_CALLBACK, @uri_logger_cb, nil,
         MHD_OPTION_NOTIFY_COMPLETED, @completed_callback, nil,
         MHD_OPTION_CONNECTION_LIMIT, 1000,
         MHD_OPTION_HTTPS_MEM_KEY, srv_signed_key_pem,
         MHD_OPTION_HTTPS_MEM_CERT, srv_signed_cert_pem,
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

