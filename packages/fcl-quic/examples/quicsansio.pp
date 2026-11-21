{
  This file is part of the Free Component Library.
  Sans-I/O demonstration: a client and a server TQuicConnection exchange datagrams in memory.
  Usage: quicsansio <certificate.pem> <key.pem> [libssl] [libcrypto]
}
program quicsansio;

{$mode objfpc}{$H+}

uses
  SysUtils, FpQuic.Consts, FpQuic.Buffers, FpQuic.Packets, FpQuic.TransportParams, FpQuic.OpenSSL, FpQuic.Connection;

var
  lNow: QWord;

// Delivers every datagram aFrom has ready to aTo; returns True when something was sent.
function Pump(aFrom, aTo: TQuicConnection): Boolean;

var
  lData: TBytes;

begin
  Result:=False;
  while aFrom.NextDatagram(lNow,lData) do
    begin
    aTo.ReceiveDatagram(lData,lNow);
    Result:=True;
    end;
end;


var
  lFactory: TQuicOpenSSLCipherFactory;
  lClientContext, lServerContext: TQuicOpenSSLContext;
  lClientTLS, lServerTLS: TQuicOpenSSLTLS;
  lTLSConfig: TQuicTLSConfig;
  lConfig: TQuicConnectionConfig;
  lClient, lServer: TQuicConnection;
  lData: TBytes;
  lHeader: TQuicPacketHeader;
  lId: QWord;
  lBuffer: array[0..255] of Byte;
  lCount: Integer;

begin
  if ParamCount<2 then
    begin
    WriteLn('Usage: quicsansio <certificate.pem> <key.pem> [libssl] [libcrypto]');
    Halt(1);
    end;
  if not QuicInitOpenSSL(ParamStr(3),ParamStr(4)) then
    begin
    WriteLn('OpenSSL 3.5 or later is required');
    Halt(1);
    end;
  lNow:=1000000;
  lFactory:=TQuicOpenSSLCipherFactory.Create;
  lTLSConfig:=Default(TQuicTLSConfig);
  lTLSConfig.Alpn:=['demo'];
  lClientContext:=TQuicOpenSSLContext.Create(False,lTLSConfig);
  lTLSConfig.CertificateFile:=ParamStr(1);
  lTLSConfig.PrivateKeyFile:=ParamStr(2);
  lServerContext:=TQuicOpenSSLContext.Create(True,lTLSConfig);
  lClientTLS:=TQuicOpenSSLTLS.Create(lClientContext);
  lServerTLS:=TQuicOpenSSLTLS.Create(lServerContext);
  lConfig:=Default(TQuicConnectionConfig);
  lConfig.CipherFactory:=lFactory;
  lConfig.Params:=QuicDefaultTransportParams;
  lConfig.Params.InitialMaxData:=65536;
  lConfig.Params.InitialMaxStreamDataBidiLocal:=65536;
  lConfig.Params.InitialMaxStreamDataBidiRemote:=65536;
  lConfig.Params.InitialMaxStreamsBidi:=4;
  lConfig.TLS:=lClientTLS;
  lClient:=TQuicConnection.CreateClient(lConfig,lNow);
  lClient.NextDatagram(lNow,lData);
  QuicParseHeader(PByte(lData),Length(lData),QUIC_LOCAL_CID_LENGTH,lHeader);
  lConfig.IsServer:=True;
  lConfig.TLS:=lServerTLS;
  lServer:=TQuicConnection.CreateServer(lConfig,lHeader.DestCid,lNow);
  lServer.ReceiveDatagram(lData,lNow);
  while Pump(lServer,lClient) or Pump(lClient,lServer) do
    Inc(lNow,1000);
  WriteLn('Handshake complete: ',lClient.HandshakeComplete,', ALPN: ',lClient.Alpn);
  lClient.OpenStream(True,lId);
  lClient.WriteStream(lId,TEncoding.UTF8.GetBytes('Hello, sans-I/O QUIC'),True);
  while Pump(lClient,lServer) or Pump(lServer,lClient) do
    Inc(lNow,1000);
  lCount:=lServer.ReadStream(lId,@lBuffer[0],SizeOf(lBuffer));
  WriteLn('Server received: ',TEncoding.UTF8.GetString(QuicBytesOf(@lBuffer[0],lCount)));
  lClient.Close(0,'bye',lNow);
  Pump(lClient,lServer);
  WriteLn('Server state after close: ',Ord(lServer.State),', reason: ',lServer.CloseReason);
  lClient.Free;
  lServer.Free;
  lClientTLS.Free;
  lServerTLS.Free;
  lClientContext.Free;
  lServerContext.Free;
  lFactory.Free;
end.
