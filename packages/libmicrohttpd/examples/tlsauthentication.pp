(* Feel free to use this example code in any way
   you see fit (Public Domain) *)

// Original example: https://gnunet.org/svn/libmicrohttpd/doc/examples/tlsauthentication.c

(*
 * Generate PEM files for test this example:
 *
 * openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout key.pem -out cert.pem
 *
 * or
 *
 * openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout server.key -out server.pem
 *)

program tlsauthentication;

{$mode objfpc}{$H+}

uses
  SysUtils, ctypes, cmem, cutils, libmicrohttpd;

const
  PORT = 8888;
  REALM = '"Maintenance"';
  USER = 'a legitimate user';
  PASSWORD = 'and his password';

  SERVERKEYFILE = 'server.key';
  SERVERCERTFILE = 'server.pem';

  function iif(c: cbool; t, f: culong): culong;
  begin
    if c then
      Result := t
    else
      Result := f;
  end;

  function string_to_base64(message: Pcchar): Pcchar;
  var
    lookup: Pcchar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    l: culong;
    i: cint;
    tmp: Pcchar;
    len: SizeInt;
  begin
    len := strlen(message);
    tmp := Malloc(len * 2);
    if nil = tmp then
      Exit(tmp);
    tmp[0] := #0;
    i := 0;
    while i < len do
    begin
      l := (culong(message[i]) shl 16)
        or iif((i + 1) < len, culong(message[i + 1]) shl 8, 0)
        or iif((i + 2) < len, culong(message[i + 2]), 0);
      strncat(tmp, @lookup[(l shr 18) and $3F], 1);
      strncat(tmp, @lookup[(l shr 12) and $3F], 1);
      if i + 1 < len then
        strncat(tmp, @lookup[(l shr 6) and $3F], 1);
      if i + 2 < len then
        strncat(tmp, @lookup[l and $3F], 1);
      i += 3;
    end;
    if (len mod 3 = 1) then
      strncat(tmp, '===', 3 - len mod 3);
    Result := tmp;
  end;

  function get_file_size(filename: Pcchar): clong;
  var
    fp: FILEptr;
    size: clong;
  begin
    fp := fopen(filename, fopenread);
    if Assigned(fp) then
    begin
      if 0 <> fseek(fp, 0, SEEK_END) then
        size := 0;
      size := ftell(fp);
      if -1 = size then
        size := 0;
      fclose(fp);
      Result := size;
    end
    else
      Result := 0;
  end;

  function load_file(filename: Pcchar): Pcchar;
  var
    fp: FILEptr;
    buffer: Pcchar;
    size: clong;
  begin
    size := get_file_size(filename);
    if size = 0 then
      Exit(nil);
    fp := fopen(filename, fopenread);
    if not Assigned(fp) then
      Exit(nil);
    buffer := Malloc(size);
    if not Assigned(buffer) then
    begin
      fclose(fp);
      Exit(nil);
    end;
    if size <> fread(buffer, 1, size, fp) then
    begin
      free(buffer);
      buffer := nil;
    end;
    fclose(fp);
    Result := buffer;
  end;

  function ask_for_authentication(connection: PMHD_Connection;
    realm: Pcchar): cint; cdecl;
  var
    ret: cint;
    response: PMHD_Response;
    headervalue: Pcchar;
    strbase: Pcchar = 'Basic realm=';
  begin
    response := MHD_create_response_from_buffer(0, nil, MHD_RESPMEM_PERSISTENT);
    if not Assigned(response) then
      Exit(MHD_NO);
    headervalue := Malloc(strlen(strbase) + strlen(realm) + 1);
    if not Assigned(headervalue) then
      Exit(MHD_NO);
    strcpy(headervalue, strbase);
    strcat(headervalue, realm);
    ret := MHD_add_response_header(response, 'WWW-Authenticate', headervalue);
    Free(headervalue);
    if ret <> 1 then
    begin
      MHD_destroy_response(response);
      Exit(MHD_NO);
    end;
    ret := MHD_queue_response(connection, MHD_HTTP_UNAUTHORIZED, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  function is_authenticated(connection: PMHD_Connection;
    username, password: Pcchar): cint; cdecl;
  var
    headervalue: Pcchar;
    expected_b64, expected: Pcchar;
    strbase: Pcchar = 'Basic ';
    authenticated: cint;
  begin
    headervalue := MHD_lookup_connection_value(connection, MHD_HEADER_KIND,
      'Authorization');
    if nil = headervalue then
      Exit(0);
    if 0 <> strncmp(headervalue, strbase, strlen(strbase)) then
      Exit(0);
    expected := malloc(strlen(username) + 1 + strlen(password) + 1);
    if nil = expected then
      Exit(0);
    strcpy(expected, username);
    strcat(expected, ':');
    strcat(expected, password);
    expected_b64 := string_to_base64(expected);
    free(expected);
    if nil = expected_b64 then
      Exit(0);
    authenticated := cint(strcomp(headervalue + strlen(strbase), expected_b64) = 0);
    Free(expected_b64);
    Result := authenticated;
  end;

  function secret_page(connection: PMHD_Connection): cint; cdecl;
  var
    ret: cint;
    response: PMHD_Response;
    page: Pcchar = '<html><body>A secret.</body></html>';
  begin
    response := MHD_create_response_from_buffer(strlen(page), Pointer(page),
      MHD_RESPMEM_PERSISTENT);
    if not Assigned(response) then
      Exit(MHD_NO);
    ret := MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    Result := ret;
  end;

  function answer_to_connection(cls: Pointer; connection: PMHD_Connection;
    url: Pcchar; method: Pcchar; version: Pcchar; upload_data: Pcchar;
    upload_data_size: Psize_t; con_cls: PPointer): cint; cdecl;
  begin
    if 0 <> strcomp(method, 'GET') then
      Exit(MHD_NO);
    if nil = con_cls^ then
    begin
      con_cls^ := connection;
      Exit(MHD_YES);
    end;
    if is_authenticated(connection, USER, PASSWORD) <> 1 then
      Exit(ask_for_authentication(connection, REALM));
    Result := secret_page(connection);
  end;

var
  daemon: PMHD_Daemon;
  key_pem: Pcchar;
  cert_pem: Pcchar;
begin
  key_pem := load_file(SERVERKEYFILE);
  cert_pem := load_file(SERVERCERTFILE);
  if (key_pem = nil) or (cert_pem = nil) then
  begin
    WriteLn('The key/certificate files could not be read.');
    Halt(1);
  end;
  daemon := MHD_start_daemon(MHD_USE_SELECT_INTERNALLY or MHD_USE_SSL, PORT,
    nil, nil, @answer_to_connection, nil, MHD_OPTION_HTTPS_MEM_KEY, key_pem,
    MHD_OPTION_HTTPS_MEM_CERT, cert_pem, MHD_OPTION_END);
  if nil = daemon then
  begin
    WriteLn(cert_pem);
    Free(key_pem);
    Free(cert_pem);
    Halt(1);
  end;
  ReadLn;
  MHD_stop_daemon(daemon);
  Free(key_pem);
  Free(cert_pem);
end.

