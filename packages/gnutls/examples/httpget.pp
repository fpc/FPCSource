program httpget;
{$mode objfpc}
{$h+}
uses ssockets, gnutls;

Const
  MAX_BUF = 1024*256;
  MSG = 'GET / HTTP/1.0'#13#10#13#10;
  DefaultCerts : PChar =  '/etc/ssl/certs/ca-certificates.crt';

Procedure MyLogFunc(level : longint; msg : PChar); cdecl;
begin
  writeln('Log[',Level:2,']: ',msg);
end;

Var
  sock : TInetSocket;
  ret : integer;
  session : tgnutls_session_t;
  buf : Array[0..MAX_BUF] of char;
  cred : tgnutls_certificate_credentials_t;
  errptr,desc : pchar;
  S : String;
  HostName : String;
  port : word;
      
begin
  hostname:='www.freepascal.org';
//  hostname:='www.google.be';
  port:=443;
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
  gnutls_global_set_log_function(@MyLogFunc);
  gnutls_global_set_log_level(5);
  gnutls_init(@session, GNUTLS_CLIENT);
//  gnutls_priority_set_direct(session,'PERFORMANCE:+ANON-ECDH:+ANON-DH',Nil);
  ret:=gnutls_set_default_priority(session);
//  ret := gnutls_priority_set_direct(session, 'SECURE256', @errptr);
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
  until (ret>0)  or (gnutls_error_is_fatal(ret) <> 0);
  if (ret < 0) then
    begin
    writeln(stderr, '*** Handshake failed');
    gnutls_perror(ret);
    end
  else  
    begin
    desc := gnutls_session_get_desc(session);
    writeln('- Session info: ', desc);
//    gnutls_free(desc);
    end;
  gnutls_record_send(session, @MSG[1], length(MSG));
  ret := gnutls_record_recv(session, @buf, MAX_BUF);
  if (ret=0) then
    writeln('- Peer has closed the TLS connection\n')
  else if ((ret < 0) and (gnutls_error_is_fatal(ret) = 0)) then
    writeln(stderr, '*** Warning: ', gnutls_strerror(ret))
  else if (ret < 0) then
    Writeln(stderr, '*** Error: ', gnutls_strerror(ret))
  else if (ret > 0) then
      begin
      writeln('- Received %d bytes: ', ret);
      SetLength(S,Ret);
      Move(Buf[0],S[1],Ret);
      Writeln(S);
      gnutls_bye(session, GNUTLS_SHUT_RDWR);
      end;
  Sock.Free;
  gnutls_deinit(session);
  gnutls_certificate_free_credentials(cred);
  gnutls_global_deinit();
  FreeGnuTLS;
end.
