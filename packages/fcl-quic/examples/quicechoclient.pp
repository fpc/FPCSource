{
  This file is part of the Free Component Library.
  QUIC echo client: sends a message on a stream and prints the echo (ALPN "echo").
  Usage: quicechoclient <address> <port> <message> [ca.pem] [libssl] [libcrypto]
}
program quicechoclient;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, FpQuic.OpenSSL, FpQuic.Endpoint;

var
  lTLS: TQuicTLSConfig;
  lClient: TQuicClient;
  lConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;
  lMessage, lReply: String;
  lBuffer: array[0..4095] of AnsiChar;
  lCount: Integer;

begin
  if ParamCount<3 then
    begin
    WriteLn('Usage: quicechoclient <address> <port> <message> [ca.pem] [libssl] [libcrypto]');
    Halt(1);
    end;
  if not QuicInitOpenSSL(ParamStr(5),ParamStr(6)) then
    begin
    WriteLn('OpenSSL 3.5 or later is required');
    Halt(1);
    end;
  lTLS:=Default(TQuicTLSConfig);
  lTLS.Alpn:=['echo'];
  lTLS.ServerName:='localhost';
  lTLS.VerifyPeer:=ParamStr(4)<>'';
  lTLS.CAFile:=ParamStr(4);
  lClient:=TQuicClient.Create(lTLS,QuicDefaultEndpointParams);
  try
    lConnection:=lClient.Connect(ParamStr(1),StrToInt(ParamStr(2)),5000);
    if lConnection=nil then
      begin
      WriteLn('Connection failed');
      Halt(1);
      end;
    lStream:=lConnection.OpenStream;
    try
      lMessage:=ParamStr(3);
      lStream.WriteBuffer(lMessage[1],Length(lMessage));
      lStream.Finish;
      lReply:='';
      repeat
        lCount:=lStream.Read(lBuffer[0],SizeOf(lBuffer));
        lReply:=lReply+Copy(lBuffer,0,lCount);
      until lCount=0;
      WriteLn('Echo: ',lReply);
    finally
      lStream.Free;
    end;
    lConnection.Close(0,'done');
  finally
    lClient.Free;
  end;
end.
