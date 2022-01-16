program httpget;

{$mode objfpc}
{$h+}

uses sysutils, ssockets, gnutls, uriparser;

Const
  logLevel = 0; // Set to positive value to enable logging.
  // Correct this for your system.
  DefaultCerts : PChar =  '/etc/ssl/certs/ca-certificates.crt';

  MAX_BUF = 1024*256;
  MSG = 'GET %s HTTP/1.0'#13#10'Host: %s'#13#10#13#10;


Procedure MyLogFunc(level : longint; msg : PChar); cdecl;
begin
  writeln(StdErr,'Log[',Level:2,']: ',msg);
end;

Var
  sock : TInetSocket;
  ret : integer;
  session : tgnutls_session_t;
  buf : Array[0..MAX_BUF] of char;
  cred : tgnutls_certificate_credentials_t;
  errptr,desc : pchar;
  FN, URL,S, HostName : String;
  port : word;

  uri : TURI;

begin
  if paramCount<1 then
    begin
    writeln('Usage : ',ExtractFileName(ParamStr(0)),' url');
    Halt(1);
    end;
  url:=ParamStr(1);
  uri:=ParseURI(URL,'https',443);
  hostname:=uri.Host;
  if uri.Protocol<>'https' then
    begin
    Writeln('Only https supported');
    Halt(1);
    end;
  Port:=URI.Port;
  FN:=uri.Path+URI.Document;
  if (URI.Params<>'') then
    FN:=FN+'?'+URI.Params;
  if FN='' then FN:='/';
  LoadGNutls();
  gnutls_global_init();
  ret := gnutls_certificate_allocate_credentials (@cred);
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_certificate_allocate_credentials: ', gnutls_strerror(ret));
    halt(1);
    end;
  ret := gnutls_certificate_set_x509_trust_file(cred, defaultcerts, GNUTLS_X509_FMT_PEM);
  if (ret = 0) then
    begin
    writeln(stderr, 'error: no certificates found in:', defaultcerts);
    halt(1);
    end
  else if (ret < 0) then
    begin
    writeln(stderr, 'error: gnutls_certificate_set_x509_trust_files(',defaultcerts,'): ',
	     gnutls_strerror(ret));
    halt(1);
    end;
  if (logLevel>0) then
    begin
    gnutls_global_set_log_function(@MyLogFunc);
    gnutls_global_set_log_level(logLevel);
    end;
  gnutls_init(@session, GNUTLS_CLIENT);
  // We can also use
  //  ret:=gnutls_set_default_priority(session);
  ret := gnutls_priority_set_direct(session, 'NORMAL', @errptr);
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_priority_set_direct: ',gnutls_strerror(ret) , ' error: at: ', errptr);
    halt(1);
    end;

  ret := gnutls_credentials_set(session, GNUTLS_CRD_CERTIFICATE, cred);
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_credentials_set: ', gnutls_strerror(ret));
    halt(1);
    end;

  Sock:=TINetSocket.Create(HostName,Port);
  gnutls_transport_set_int(session, Sock.Handle);
  gnutls_handshake_set_timeout(session,GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT);

  ret := gnutls_server_name_set(session, GNUTLS_NAME_DNS,pchar(HostName), length(HostName));
  if (ret <> GNUTLS_E_SUCCESS) then
    begin
    writeln(stderr, 'error: gnutls_server_name_set: ', gnutls_strerror(ret));
    halt(1);
    end;
  
  gnutls_session_set_verify_cert(session,pchar(HostName),0);
  
  Repeat
    ret:=gnutls_handshake(session);
    if Ret<>GNUTLS_E_SUCCESS then
      Case ret of
        GNUTLS_E_AGAIN : Writeln(StdErr,'Handshake again');
        GNUTLS_E_INTERRUPTED : Writeln(StdErr,'Handshake interrupted');
      else
        Writeln(StdErr,'Error ',ret,' received, fatal : ',gnutls_error_is_fatal(ret));
      end;
  until (ret>=0)  or (gnutls_error_is_fatal(ret) <> 0);
  if (ret < 0) then
    begin
    writeln(stderr, '*** Handshake failed');
    gnutls_perror(ret);
    end
  else  
    begin
    desc := gnutls_session_get_desc(session);
    writeln(StdErr,'- Session info: ', desc);
//    gnutls_free(desc);
    end;
  S:=Format(Msg,[FN,HostName]);
  Writeln(StdErr,'Sending request : ',S);
  gnutls_record_send(session, Pchar(S), length(S));
  repeat
    ret := gnutls_record_recv(session, @buf, MAX_BUF);
    if (ret=0) then
      writeln(StdErr,'- Peer has closed the TLS connection\n')
    else if ((ret < 0) and (gnutls_error_is_fatal(ret) = 0)) then
      writeln(stderr, '*** Warning: ', gnutls_strerror(ret))
    else if (ret < 0) and (ret<>GNUTLS_E_PREMATURE_TERMINATION) then
      Writeln(stderr, '*** Error: ', ret, ' : ',gnutls_strerror(ret))
    else if (ret > 0) then
        begin
        writeln(StdErr,'- Received ',ret,' bytes: ');
        SetLength(S,Ret);
        Move(Buf[0],S[1],Ret);
        Write(S);
        end;
  until (ret<=0) and Not ((ret=GNUTLS_E_INTERRUPTED) or (Ret=GNUTLS_E_AGAIN));
  Writeln;
  gnutls_bye(session, GNUTLS_SHUT_RDWR);
  Sock.Free;
  gnutls_deinit(session);
  gnutls_certificate_free_credentials(cred);
  gnutls_global_deinit();
  FreeGnuTLS;
end.
