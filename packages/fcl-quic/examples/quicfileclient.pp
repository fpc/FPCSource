{
  This file is part of the Free Component Library.
  QUIC file client: downloads a file from or uploads a file to quicfileserver (ALPN "qfile").
  Usage: quicfileclient <address> <port> get|put <remote name> <local file> [ca.pem] [libssl] [libcrypto]
}
program quicfileclient;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, DateUtils, FpQuic.OpenSSL, FpQuic.Endpoint;

const
  MaxLineLength = 1024;

// Reads one line terminated by LF from aStream; raises an exception when it is too long or missing.
function ReadLine(aStream: TStream): String;

var
  lChar: AnsiChar;

begin
  Result:='';
  repeat
    if aStream.Read(lChar,1)=0 then
      raise Exception.Create('Incomplete reply line');
    if lChar<>#10 then
      Result:=Result+lChar;
    if Length(Result)>MaxLineLength then
      raise Exception.Create('Reply line too long');
  until lChar=#10;
end;


// Writes aLine followed by LF to aStream.
procedure WriteLine(aStream: TStream; const aLine: String);

var
  lData: String;

begin
  lData:=aLine+#10;
  aStream.WriteBuffer(lData[1],Length(lData));
end;


// Reads a reply line and raises an exception unless it starts with OK; returns the text after OK.
function ExpectOK(aStream: TStream): String;

var
  lLine: String;

begin
  lLine:=ReadLine(aStream);
  if Copy(lLine,1,2)<>'OK' then
    raise Exception.Create('Server: '+lLine);
  Result:=Trim(Copy(lLine,3,Length(lLine)));
end;


// Downloads aRemoteName into aLocalFile; returns the number of bytes received.
function Download(aStream: TQuicStreamAdapter; const aRemoteName, aLocalFile: String): Int64;

var
  lFile: TFileStream;
  lBuffer: array[0..65535] of Byte;
  lCount: Integer;
  lExpected: Int64;

begin
  WriteLine(aStream,'GET '+aRemoteName);
  aStream.Finish;
  lExpected:=StrToInt64(ExpectOK(aStream));
  lFile:=TFileStream.Create(aLocalFile,fmCreate);
  try
    repeat
      lCount:=aStream.Read(lBuffer[0],SizeOf(lBuffer));
      if lCount>0 then
        lFile.WriteBuffer(lBuffer[0],lCount);
    until lCount=0;
    Result:=lFile.Size;
  finally
    lFile.Free;
  end;
  if Result<>lExpected then
    raise Exception.CreateFmt('Received %d of %d bytes',[Result,lExpected]);
end;


// Uploads aLocalFile as aRemoteName; returns the number of bytes the server stored.
function Upload(aStream: TQuicStreamAdapter; const aRemoteName, aLocalFile: String): Int64;

var
  lFile: TFileStream;

begin
  lFile:=TFileStream.Create(aLocalFile,fmOpenRead or fmShareDenyWrite);
  try
    WriteLine(aStream,'PUT '+aRemoteName);
    ExpectOK(aStream);
    aStream.CopyFrom(lFile,0);
    aStream.Finish;
    Result:=StrToInt64(ExpectOK(aStream));
    if Result<>lFile.Size then
      raise Exception.CreateFmt('Server stored %d of %d bytes',[Result,lFile.Size]);
  finally
    lFile.Free;
  end;
end;


var
  lTLS: TQuicTLSConfig;
  lClient: TQuicClient;
  lConnection: TQuicEndpointConnection;
  lStream: TQuicStreamAdapter;
  lCommand: String;
  lStart: TDateTime;
  lBytes, lMilliseconds: Int64;

begin
  lCommand:=LowerCase(ParamStr(3));
  if (ParamCount<5) or ((lCommand<>'get') and (lCommand<>'put')) then
    begin
    WriteLn('Usage: quicfileclient <address> <port> get|put <remote name> <local file> [ca.pem] [libssl] [libcrypto]');
    Halt(1);
    end;
  if not QuicInitOpenSSL(ParamStr(7),ParamStr(8)) then
    begin
    WriteLn('OpenSSL 3.5 or later is required');
    Halt(1);
    end;
  lTLS:=Default(TQuicTLSConfig);
  lTLS.Alpn:=['qfile'];
  lTLS.ServerName:='localhost';
  lTLS.VerifyPeer:=ParamStr(6)<>'';
  lTLS.CAFile:=ParamStr(6);
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
      try
        lStart:=Now;
        if lCommand='get' then
          lBytes:=Download(lStream,ParamStr(4),ParamStr(5))
        else
          lBytes:=Upload(lStream,ParamStr(4),ParamStr(5));
        lMilliseconds:=MilliSecondsBetween(Now,lStart);
        if lMilliseconds=0 then
          lMilliseconds:=1;
        WriteLn(Format('%d bytes in %d ms (%.1f MB/s)',[lBytes,lMilliseconds,lBytes/lMilliseconds/1000]));
      except
        on E: Exception do
          begin
          WriteLn('Transfer failed: ',E.Message);
          ExitCode:=1;
          end;
      end;
    finally
      lStream.Free;
    end;
    lConnection.Close(0,'done');
  finally
    lClient.Free;
  end;
end.
