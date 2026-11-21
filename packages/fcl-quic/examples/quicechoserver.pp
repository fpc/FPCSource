{
  This file is part of the Free Component Library.
  QUIC echo server: echoes every bidirectional stream back to the client (ALPN "echo").
  Usage: quicechoserver <certificate.pem> <key.pem> [port] [libssl] [libcrypto]
}
program quicechoserver;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, FpQuic.OpenSSL, FpQuic.Endpoint;

type
  // Serves one connection.
  TConnectionThread = class(TThread)
  private
    FConnection: TQuicEndpointConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(aConnection: TQuicEndpointConnection);
  end;

constructor TConnectionThread.Create(aConnection: TQuicEndpointConnection);

begin
  FConnection:=aConnection;
  FreeOnTerminate:=True;
  inherited Create(False);
end;


procedure TConnectionThread.Execute;

var
  lStream: TQuicStreamAdapter;
  lBuffer: array[0..65535] of Byte;
  lCount: Integer;

begin
  try
    repeat
      lStream:=FConnection.AcceptStream(60000);
      if lStream=nil then
        Break;
      try
        repeat
          lCount:=lStream.Read(lBuffer[0],SizeOf(lBuffer));
          if lCount>0 then
            lStream.WriteBuffer(lBuffer[0],lCount);
        until lCount=0;
        lStream.Finish;
      finally
        lStream.Free;
      end;
    until False;
  except
    on E: Exception do
      WriteLn('Connection error: ',E.Message);
  end;
  FConnection.WaitClosed(5000);
  FConnection.Free;
end;


var
  lTLS: TQuicTLSConfig;
  lServer: TQuicServer;
  lConnection: TQuicEndpointConnection;
  lPort: Word;

begin
  if ParamCount<2 then
    begin
    WriteLn('Usage: quicechoserver <certificate.pem> <key.pem> [port] [libssl] [libcrypto]');
    Halt(1);
    end;
  if not QuicInitOpenSSL(ParamStr(4),ParamStr(5)) then
    begin
    WriteLn('OpenSSL 3.5 or later is required');
    Halt(1);
    end;
  lPort:=StrToIntDef(ParamStr(3),4433);
  lTLS:=Default(TQuicTLSConfig);
  lTLS.Alpn:=['echo'];
  lTLS.CertificateFile:=ParamStr(1);
  lTLS.PrivateKeyFile:=ParamStr(2);
  lServer:=TQuicServer.Create(lTLS,QuicDefaultEndpointParams);
  try
    lServer.Listen('0.0.0.0',lPort);
    WriteLn('Listening on UDP port ',lServer.Port);
    repeat
      lConnection:=lServer.Accept(1000);
      if lConnection<>nil then
        begin
        WriteLn('Connection from ',lConnection.Address.Address,':',lConnection.Port);
        TConnectionThread.Create(lConnection);
        end;
    until False;
  finally
    lServer.Free;
  end;
end.
