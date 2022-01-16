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
 * @file https_fileserver_example.pp (Original: https_fileserver_example.c)
 * @brief a simple HTTPS file server using TLS.
 *
 * Usage :
 *
 *  'https_fileserver_example HTTP-PORT'
 *
 * The certificate & key are required by the server to operate,  Omitting the
 * path arguments will cause the server to use the hard coded example certificate & key.
 *
 * 'certtool' may be used to generate these if required.
 *
 * @author Sagie Amir / Silvio Clécio
 *)

program https_fileserver_example;

{$mode objfpc}{$H+}

uses
  sysutils, BaseUnix, cutils, libmicrohttpd;

const
  BUF_SIZE = 1024;
  MAX_URL_LEN = 255;

  // TODO remove if unused
  CAFILE: Pcchar = 'ca.pem';
  CRLFILE: Pcchar = 'crl.pem';

  EMPTY_PAGE: Pcchar = '<html><head><title>File not found</title></head><body>File not found</body></html>';

  (* Test Certificate *)
  cert_pem: array[0..980] of AnsiChar =
    '-----BEGIN CERTIFICATE-----'#10+
    'MIICpjCCAZCgAwIBAgIESEPtjjALBgkqhkiG9w0BAQUwADAeFw0wODA2MDIxMjU0'#10+
    'MzhaFw0wOTA2MDIxMjU0NDZaMAAwggEfMAsGCSqGSIb3DQEBAQOCAQ4AMIIBCQKC'#10+
    'AQC03TyUvK5HmUAirRp067taIEO4bibh5nqolUoUdo/LeblMQV+qnrv/RNAMTx5X'#10+
    'fNLZ45/kbM9geF8qY0vsPyQvP4jumzK0LOJYuIwmHaUm9vbXnYieILiwCuTgjaud'#10+
    '3VkZDoQ9fteIo+6we9UTpVqZpxpbLulBMh/VsvX0cPJ1VFC7rT59o9hAUlFf9jX/'#10+
    'GmKdYI79MtgVx0OPBjmmSD6kicBBfmfgkO7bIGwlRtsIyMznxbHu6VuoX/eVxrTv'#10+
    'rmCwgEXLWRZ6ru8MQl5YfqeGXXRVwMeXU961KefbuvmEPccgCxm8FZ1C1cnDHFXh'#10+
    'siSgAzMBjC/b6KVhNQ4KnUdZAgMBAAGjLzAtMAwGA1UdEwEB/wQCMAAwHQYDVR0O'#10+
    'BBYEFJcUvpjvE5fF/yzUshkWDpdYiQh/MAsGCSqGSIb3DQEBBQOCAQEARP7eKSB2'#10+
    'RNd6XjEjK0SrxtoTnxS3nw9sfcS7/qD1+XHdObtDFqGNSjGYFB3Gpx8fpQhCXdoN'#10+
    '8QUs3/5ZVa5yjZMQewWBgz8kNbnbH40F2y81MHITxxCe1Y+qqHWwVaYLsiOTqj2/'#10+
    '0S3QjEJ9tvklmg7JX09HC4m5QRYfWBeQLD1u8ZjA1Sf1xJriomFVyRLI2VPO2bNe'#10+
    'JDMXWuP+8kMC7gEvUnJ7A92Y2yrhu3QI3bjPk8uSpHea19Q77tul1UVBJ5g+zpH3'#10+
    'OsF5p0MyaVf09GTzcLds5nE/osTdXGUyHJapWReVmPm3Zn6gqYlnzD99z+DPIgIV'#10+
    'RhZvQx74NQnS6g=='#10+
    '-----END CERTIFICATE-----'#10;

  key_pem: array[0..1674] of AnsiChar =
    '-----BEGIN RSA PRIVATE KEY-----'#10+
    'MIIEowIBAAKCAQEAtN08lLyuR5lAIq0adOu7WiBDuG4m4eZ6qJVKFHaPy3m5TEFf'#10+
    'qp67/0TQDE8eV3zS2eOf5GzPYHhfKmNL7D8kLz+I7psytCziWLiMJh2lJvb2152I'#10+
    'niC4sArk4I2rnd1ZGQ6EPX7XiKPusHvVE6VamacaWy7pQTIf1bL19HDydVRQu60+'#10+
    'faPYQFJRX/Y1/xpinWCO/TLYFcdDjwY5pkg+pInAQX5n4JDu2yBsJUbbCMjM58Wx'#10+
    '7ulbqF/3lca0765gsIBFy1kWeq7vDEJeWH6nhl10VcDHl1PetSnn27r5hD3HIAsZ'#10+
    'vBWdQtXJwxxV4bIkoAMzAYwv2+ilYTUOCp1HWQIDAQABAoIBAArOQv3R7gmqDspj'#10+
    'lDaTFOz0C4e70QfjGMX0sWnakYnDGn6DU19iv3GnX1S072ejtgc9kcJ4e8VUO79R'#10+
    'EmqpdRR7k8dJr3RTUCyjzf/C+qiCzcmhCFYGN3KRHA6MeEnkvRuBogX4i5EG1k5l'#10+
    '/5t+YBTZBnqXKWlzQLKoUAiMLPg0eRWh+6q7H4N7kdWWBmTpako7TEqpIwuEnPGx'#10+
    'u3EPuTR+LN6lF55WBePbCHccUHUQaXuav18NuDkcJmCiMArK9SKb+h0RqLD6oMI/'#10+
    'dKD6n8cZXeMBkK+C8U/K0sN2hFHACsu30b9XfdnljgP9v+BP8GhnB0nCB6tNBCPo'#10+
    '32srOwECgYEAxWh3iBT4lWqL6bZavVbnhmvtif4nHv2t2/hOs/CAq8iLAw0oWGZc'#10+
    '+JEZTUDMvFRlulr0kcaWra+4fN3OmJnjeuFXZq52lfMgXBIKBmoSaZpIh2aDY1Rd'#10+
    'RbEse7nQl9hTEPmYspiXLGtnAXW7HuWqVfFFP3ya8rUS3t4d07Hig8ECgYEA6ou6'#10+
    'OHiBRTbtDqLIv8NghARc/AqwNWgEc9PelCPe5bdCOLBEyFjqKiT2MttnSSUc2Zob'#10+
    'XhYkHC6zN1Mlq30N0e3Q61YK9LxMdU1vsluXxNq2rfK1Scb1oOlOOtlbV3zA3VRF'#10+
    'hV3t1nOA9tFmUrwZi0CUMWJE/zbPAyhwWotKyZkCgYEAh0kFicPdbABdrCglXVae'#10+
    'SnfSjVwYkVuGd5Ze0WADvjYsVkYBHTvhgRNnRJMg+/vWz3Sf4Ps4rgUbqK8Vc20b'#10+
    'AU5G6H6tlCvPRGm0ZxrwTWDHTcuKRVs+pJE8C/qWoklE/AAhjluWVoGwUMbPGuiH'#10+
    '6Gf1bgHF6oj/Sq7rv/VLZ8ECgYBeq7ml05YyLuJutuwa4yzQ/MXfghzv4aVyb0F3'#10+
    'QCdXR6o2IYgR6jnSewrZKlA9aPqFJrwHNR6sNXlnSmt5Fcf/RWO/qgJQGLUv3+rG'#10+
    '7kuLTNDR05azSdiZc7J89ID3Bkb+z2YkV+6JUiPq/Ei1+nDBEXb/m+/HqALU/nyj'#10+
    'P3gXeQKBgBusb8Rbd+KgxSA0hwY6aoRTPRt8LNvXdsB9vRcKKHUFQvxUWiUSS+L9'#10+
    '/Qu1sJbrUquKOHqksV5wCnWnAKyJNJlhHuBToqQTgKXjuNmVdYSe631saiI7PHyC'#10+
    'eRJ6DxULPxABytJrYCRrNqmXi5TCiqR2mtfalEMOPxz8rUU8dYyx'#10+
    '-----END RSA PRIVATE KEY-----'#10;

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

  function http_ahc(cls: Pointer; connection: PMHD_Connection; url: Pcchar;
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
      response := MHD_create_response_from_buffer(strlen(EMPTY_PAGE),
                    Pointer(EMPTY_PAGE), MHD_RESPMEM_PERSISTENT);
      ret := MHD_queue_response(connection, MHD_HTTP_NOT_FOUND, response);
      MHD_destroy_response(response);
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
  TLS_daemon: PMHD_Daemon;
begin
  if argc = 2 then
  begin
    (* TODO check if this is truly necessary -  disallow usage of the blocking /dev/random *)
    (* gcry_control(GCRYCTL_ENABLE_QUICK_RANDOM, 0); *)
    TLS_daemon := MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION or
                    MHD_USE_DEBUG or MHD_USE_SSL, StrToInt(argv[1]), nil, nil,
                    @http_ahc, nil, MHD_OPTION_CONNECTION_TIMEOUT, 256,
                    MHD_OPTION_HTTPS_MEM_KEY, key_pem,
                    MHD_OPTION_HTTPS_MEM_CERT, cert_pem,
                    MHD_OPTION_END);
  end
  else
  begin
    WriteLn(' Usage: ', argv[0], ' HTTP-PORT');
    Halt(1);
  end;
  if TLS_daemon = nil then
  begin
    WriteLn(stderr, 'Error: failed to start TLS_daemon');
    Halt(1);
  end
  else
    WriteLn('MHD daemon listening on port ', argv[1]);
  ReadLn;
  MHD_stop_daemon(TLS_daemon);
end.

