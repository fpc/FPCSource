{
  This file is part of the Free Component Library.
  Interoperability check against the QUIC stack of OpenSSL 3.5: its client against TQuicServer, TQuicClient against its server.
  Usage: opensslinterop <libssl> <libcrypto> <certificate.pem> <key.pem>
}
program opensslinterop;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, ctypes, dynlibs, openssl, fpsockets, FpQuic.Buffers, FpQuic.OpenSSL, FpQuic.Endpoint;

type
  TMethodFunc = function: Pointer; cdecl;
  TNewListener = function(ctx: Pointer; flags: cuint64): Pointer; cdecl;
  TSSLFunc = function(s: Pointer): cint; cdecl;
  TAccept = function(s: Pointer; flags: cuint64): Pointer; cdecl;
  TReadWriteEx = function(s: Pointer; buf: Pointer; num: csize_t; done: pcsize_t): cint; cdecl;
  TConclude = function(s: Pointer; flags: cuint64): cint; cdecl;
  TSetPeer = function(s: Pointer; addr: Pointer): cint; cdecl;
  TSetAlpn = function(s: Pointer; protos: PByte; len: cuint): cint; cdecl;
  TSetBio = procedure(s: Pointer; rbio, wbio: Pointer); cdecl;
  TBioNew = function(method: Pointer): Pointer; cdecl;
  TBioCtrl = function(b: Pointer; cmd: cint; larg: clong; parg: Pointer): clong; cdecl;
  TAddrNew = function: Pointer; cdecl;
  TAddrRawMake = function(ap: Pointer; family: cint; where: Pointer; wherelen: csize_t; port: cushort): cint; cdecl;
  TUseKeyFile = function(ctx: Pointer; aFile: PAnsiChar; aType: cint): cint; cdecl;

var
  OSSL_QUIC_client_method, OSSL_QUIC_server_method, BIO_s_datagram: TMethodFunc;
  SSL_new_listener: TNewListener;
  SSL_listen, SSL_connect: TSSLFunc;
  SSL_accept_connection, SSL_accept_stream: TAccept;
  SSL_read_ex, SSL_write_ex: TReadWriteEx;
  SSL_stream_conclude: TConclude;
  SSL_set1_initial_peer_addr: TSetPeer;
  SSL_set_alpn_protos: TSetAlpn;
  SSL_set_bio: TSetBio;
  BIO_new: TBioNew;
  BIO_ctrl: TBioCtrl;
  BIO_ADDR_new: TAddrNew;
  BIO_ADDR_rawmake: TAddrRawMake;
  SSL_CTX_use_PrivateKey_file: TUseKeyFile;
  CertFile, KeyFile: String;

const
  BIO_C_SET_FD = 104;
  AlpnWire: array[0..4] of Byte = (4,Ord('e'),Ord('c'),Ord('h'),Ord('o'));
  Message = 'interoperability check message';

// Loads aName from aLib or stops the program.
function Need(aLib: TLibHandle; const aName: String): Pointer;

begin
  Result:=GetProcAddress(aLib,aName);
  if Result=nil then
    begin
    WriteLn('Missing OpenSSL function ',aName);
    Halt(2);
    end;
end;


// Wraps the descriptor of aSocket in an OpenSSL datagram BIO.
function DatagramBio(const aSocket: TFPSocket): Pointer;

var
  lFD: cint;

begin
  Result:=BIO_new(BIO_s_datagram());
  lFD:=aSocket.FD;
  BIO_ctrl(Result,BIO_C_SET_FD,0,@lFD);
end;


// ALPN selection callback of the OpenSSL server.
function SelectAlpn(s: Pointer; aOut: PPByte; aOutLen: PByte; aIn: PByte; aInLen: cuint; aArg: Pointer): cint; cdecl;

begin
  if SslSelectNextProto(aOut,aOutLen,@AlpnWire[0],Length(AlpnWire),aIn,aInLen)=1 then
    Result:=SSL_TLSEXT_ERR_OK
  else
    Result:=SSL_TLSEXT_ERR_ALERT_FATAL;
end;


type
  // Echo service of TQuicServer for the OpenSSL client test.
  TEchoThread = class(TThread)
  public
    Server: TQuicServer;
    procedure Execute; override;
  end;

procedure TEchoThread.Execute;

var
  lConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;
  lBuffer: array[0..4095] of Byte;
  lCount: Integer;

begin
  lConnection:=Server.Accept(10000);
  if lConnection=nil then
    Exit;
  lStream:=lConnection.AcceptStream(10000);
  if lStream<>nil then
    begin
    repeat
      lCount:=lStream.Read(lBuffer[0],SizeOf(lBuffer));
      if lCount>0 then
        lStream.WriteBuffer(lBuffer[0],lCount);
    until lCount=0;
    lStream.Finish;
    lConnection.WaitClosed(3000);
    lStream.Free;
    end;
  lConnection.Free;
end;


// OpenSSL QUIC client against TQuicServer; returns True when the echo matches.
function OpenSSLClientToOurServer: Boolean;

var
  lTLS: TQuicTLSConfig;
  lServer: TQuicServer;
  lEcho: TEchoThread;
  lCtx, lSSL, lAddr: Pointer;
  lSocket: TFPSocket;
  lIP: array[0..3] of Byte = (127,0,0,1);
  lDone: csize_t;
  lBuffer: array[0..4095] of AnsiChar;
  lReply: String;

begin
  lTLS:=Default(TQuicTLSConfig);
  lTLS.Alpn:=['echo'];
  lTLS.CertificateFile:=CertFile;
  lTLS.PrivateKeyFile:=KeyFile;
  lServer:=TQuicServer.Create(lTLS,QuicDefaultEndpointParams);
  lServer.Listen('127.0.0.1',0);
  lEcho:=TEchoThread.Create(True);
  lEcho.Server:=lServer;
  lEcho.Start;
  lCtx:=SslCtxNew(OSSL_QUIC_client_method());
  lSSL:=SslNew(lCtx);
  lSocket:=UDPSocket(stIPv4);
  Bind(lSocket,'0.0.0.0',0);
  SSL_set_bio(lSSL,DatagramBio(lSocket),DatagramBio(lSocket));
  lAddr:=BIO_ADDR_new();
  BIO_ADDR_rawmake(lAddr,2,@lIP[0],4,NtoBE(lServer.Port));
  SSL_set1_initial_peer_addr(lSSL,lAddr);
  SSL_set_alpn_protos(lSSL,@AlpnWire[0],Length(AlpnWire));
  SslSetTlsextHostName(lSSL,'localhost');
  Result:=False;
  if SSL_connect(lSSL)<>1 then
    WriteLn('  OpenSSL client: handshake failed')
  else
    begin
    SSL_write_ex(lSSL,PAnsiChar(Message),Length(Message),@lDone);
    SSL_stream_conclude(lSSL,0);
    lReply:='';
    while SSL_read_ex(lSSL,@lBuffer[0],SizeOf(lBuffer),@lDone)=1 do
      lReply:=lReply+Copy(lBuffer,0,lDone);
    WriteLn('  OpenSSL client received: ',lReply);
    Result:=lReply=Message;
    end;
  SslShutdown(lSSL);
  SslFree(lSSL);
  SslCtxFree(lCtx);
  lEcho.WaitFor;
  lEcho.Free;
  lServer.Free;
end;


type
  // OpenSSL QUIC server echoing one stream.
  TOpenSSLServerThread = class(TThread)
  public
    Listener: Pointer;
    Ok: Boolean;
    procedure Execute; override;
  end;

procedure TOpenSSLServerThread.Execute;

var
  lConn, lStream: Pointer;
  lBuffer: array[0..4095] of Byte;
  lDone, lWritten: csize_t;
  lData: TBytes;

begin
  lConn:=SSL_accept_connection(Listener,0);
  if lConn=nil then
    Exit;
  lStream:=SSL_accept_stream(lConn,0);
  if lStream=nil then
    Exit;
  lData:=nil;
  while SSL_read_ex(lStream,@lBuffer[0],SizeOf(lBuffer),@lDone)=1 do
    lData:=Concat(lData,QuicBytesOf(@lBuffer[0],lDone));
  SSL_write_ex(lStream,PByte(lData),Length(lData),@lWritten);
  SSL_stream_conclude(lStream,0);
  Ok:=True;
  Sleep(500);
  SslFree(lStream);
  SslFree(lConn);
end;


// TQuicClient against the OpenSSL QUIC server; returns True when the echo matches.
function OurClientToOpenSSLServer: Boolean;

var
  lCtx, lListener: Pointer;
  lSocket: TFPSocket;
  lPort: Word;
  lServer: TOpenSSLServerThread;
  lTLS: TQuicTLSConfig;
  lClient: TQuicClient;
  lConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;
  lBuffer: array[0..4095] of AnsiChar;
  lReply: String;
  lCount: Integer;

begin
  Result:=False;
  lCtx:=SslCtxNew(OSSL_QUIC_server_method());
  SslCtxUseCertificateChainFile(lCtx,CertFile);
  SSL_CTX_use_PrivateKey_file(lCtx,PAnsiChar(AnsiString(KeyFile)),SSL_FILETYPE_PEM);
  SslCtxSetAlpnSelectCb(lCtx,@SelectAlpn,nil);
  lListener:=SSL_new_listener(lCtx,0);
  lSocket:=UDPSocket(stIPv4);
  Bind(lSocket,'127.0.0.1',0);
  lPort:=LocalEndpoint(lSocket).Second;
  SSL_set_bio(lListener,DatagramBio(lSocket),DatagramBio(lSocket));
  if SSL_listen(lListener)<>1 then
    begin
    WriteLn('  OpenSSL server: listen failed');
    Exit;
    end;
  lServer:=TOpenSSLServerThread.Create(True);
  lServer.Listener:=lListener;
  lServer.Start;
  lTLS:=Default(TQuicTLSConfig);
  lTLS.Alpn:=['echo'];
  lTLS.ServerName:='localhost';
  lTLS.VerifyPeer:=True;
  lTLS.CAFile:=CertFile;
  lClient:=TQuicClient.Create(lTLS,QuicDefaultEndpointParams);
  try
    lConnection:=lClient.Connect('127.0.0.1',lPort,5000);
    if lConnection=nil then
      WriteLn('  Our client: handshake failed')
    else
      begin
      lStream:=lConnection.OpenStream;
      try
        lStream.WriteBuffer(Message[1],Length(Message));
        lStream.Finish;
        lReply:='';
        repeat
          lCount:=lStream.Read(lBuffer[0],SizeOf(lBuffer));
          lReply:=lReply+Copy(lBuffer,0,lCount);
        until lCount=0;
        WriteLn('  Our client received: ',lReply);
        Result:=lReply=Message;
      finally
        lStream.Free;
      end;
      lConnection.Close(0,'done');
      end;
  finally
    lClient.Free;
  end;
  lServer.WaitFor;
  lServer.Free;
  SslFree(lListener);
  SslCtxFree(lCtx);
end;


var
  lOk1, lOk2: Boolean;

begin
  if (ParamCount<4) or not QuicInitOpenSSL(ParamStr(1),ParamStr(2)) then
    begin
    WriteLn('Usage: opensslinterop <libssl> <libcrypto> <certificate.pem> <key.pem> (OpenSSL 3.5+)');
    Halt(1);
    end;
  CertFile:=ParamStr(3);
  KeyFile:=ParamStr(4);
  Pointer(OSSL_QUIC_client_method):=Need(SSLLibHandle,'OSSL_QUIC_client_method');
  Pointer(OSSL_QUIC_server_method):=Need(SSLLibHandle,'OSSL_QUIC_server_method');
  Pointer(SSL_new_listener):=Need(SSLLibHandle,'SSL_new_listener');
  Pointer(SSL_listen):=Need(SSLLibHandle,'SSL_listen');
  Pointer(SSL_connect):=Need(SSLLibHandle,'SSL_connect');
  Pointer(SSL_accept_connection):=Need(SSLLibHandle,'SSL_accept_connection');
  Pointer(SSL_accept_stream):=Need(SSLLibHandle,'SSL_accept_stream');
  Pointer(SSL_read_ex):=Need(SSLLibHandle,'SSL_read_ex');
  Pointer(SSL_write_ex):=Need(SSLLibHandle,'SSL_write_ex');
  Pointer(SSL_stream_conclude):=Need(SSLLibHandle,'SSL_stream_conclude');
  Pointer(SSL_set1_initial_peer_addr):=Need(SSLLibHandle,'SSL_set1_initial_peer_addr');
  Pointer(SSL_set_alpn_protos):=Need(SSLLibHandle,'SSL_set_alpn_protos');
  Pointer(SSL_set_bio):=Need(SSLLibHandle,'SSL_set_bio');
  Pointer(SSL_CTX_use_PrivateKey_file):=Need(SSLLibHandle,'SSL_CTX_use_PrivateKey_file');
  Pointer(BIO_s_datagram):=Need(SSLUtilHandle,'BIO_s_datagram');
  Pointer(BIO_new):=Need(SSLUtilHandle,'BIO_new');
  Pointer(BIO_ctrl):=Need(SSLUtilHandle,'BIO_ctrl');
  Pointer(BIO_ADDR_new):=Need(SSLUtilHandle,'BIO_ADDR_new');
  Pointer(BIO_ADDR_rawmake):=Need(SSLUtilHandle,'BIO_ADDR_rawmake');
  WriteLn('OpenSSL QUIC client -> TQuicServer');
  lOk1:=OpenSSLClientToOurServer;
  WriteLn('TQuicClient -> OpenSSL QUIC server');
  lOk2:=OurClientToOpenSSLServer;
  WriteLn('Results: ',lOk1,' ',lOk2);
  if not (lOk1 and lOk2) then
    Halt(1);
end.
