{
  This file is part of the Free Component Library.
  QUIC file server: serves and stores files of one directory, one request per stream (ALPN "qfile").
  Usage: quicfileserver <certificate.pem> <key.pem> <directory> [port] [libssl] [libcrypto]
}
program quicfileserver;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, FpQuic.OpenSSL, FpQuic.Endpoint;

const
  MaxLineLength = 1024;

type
  // Serves the requests of one connection.
  TConnectionThread = class(TThread)
  private
    FConnection: TQuicEndpointConnection;
    FDirectory: String;
    procedure HandleRequest(aStream: TQuicStreamAdapter);
    procedure SendFile(aStream: TQuicStreamAdapter; const aFileName: String);
    procedure ReceiveFile(aStream: TQuicStreamAdapter; const aFileName: String);
  protected
    procedure Execute; override;
  public
    constructor Create(aConnection: TQuicEndpointConnection; const aDirectory: String);
  end;

// Reads one line terminated by LF from aStream; raises an exception when it is too long or missing.
function ReadLine(aStream: TStream): String;

var
  lChar: AnsiChar;

begin
  Result:='';
  repeat
    if aStream.Read(lChar,1)=0 then
      raise Exception.Create('Incomplete request line');
    if lChar<>#10 then
      Result:=Result+lChar;
    if Length(Result)>MaxLineLength then
      raise Exception.Create('Request line too long');
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


// True when aName names a file directly inside the served directory.
function IsPlainFileName(const aName: String): Boolean;

begin
  Result:=(aName<>'') and (aName<>'.') and (aName<>'..') and (ExtractFileName(aName)=aName)
    and (Pos('/',aName)=0) and (Pos('\',aName)=0);
end;


constructor TConnectionThread.Create(aConnection: TQuicEndpointConnection; const aDirectory: String);

begin
  FConnection:=aConnection;
  FDirectory:=IncludeTrailingPathDelimiter(aDirectory);
  FreeOnTerminate:=True;
  inherited Create(False);
end;


procedure TConnectionThread.SendFile(aStream: TQuicStreamAdapter; const aFileName: String);

var
  lFile: TFileStream;

begin
  if not FileExists(aFileName) then
    begin
    WriteLine(aStream,'ERR file not found');
    Exit;
    end;
  lFile:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    WriteLine(aStream,'OK '+IntToStr(lFile.Size));
    aStream.CopyFrom(lFile,0);
  finally
    lFile.Free;
  end;
end;


procedure TConnectionThread.ReceiveFile(aStream: TQuicStreamAdapter; const aFileName: String);

var
  lFile: TFileStream;
  lBuffer: array[0..65535] of Byte;
  lCount: Integer;
  lSize: Int64;

begin
  try
    lFile:=TFileStream.Create(aFileName,fmCreate);
  except
    WriteLine(aStream,'ERR cannot create file');
    Exit;
  end;
  WriteLine(aStream,'OK');
  try
    try
      repeat
        lCount:=aStream.Read(lBuffer[0],SizeOf(lBuffer));
        if lCount>0 then
          lFile.WriteBuffer(lBuffer[0],lCount);
      until lCount=0;
      lSize:=lFile.Size;
    finally
      lFile.Free;
    end;
  except
    DeleteFile(aFileName);
    raise;
  end;
  WriteLine(aStream,'OK '+IntToStr(lSize));
end;


procedure TConnectionThread.HandleRequest(aStream: TQuicStreamAdapter);

var
  lLine, lCommand, lName: String;
  lSpace: Integer;

begin
  lLine:=ReadLine(aStream);
  lSpace:=Pos(' ',lLine);
  lCommand:=Copy(lLine,1,lSpace-1);
  lName:=Copy(lLine,lSpace+1,Length(lLine));
  if (lSpace=0) or not IsPlainFileName(lName) then
    WriteLine(aStream,'ERR invalid request')
  else if lCommand='GET' then
    begin
    WriteLn('GET ',lName);
    SendFile(aStream,FDirectory+lName);
    end
  else if lCommand='PUT' then
    begin
    WriteLn('PUT ',lName);
    ReceiveFile(aStream,FDirectory+lName);
    end
  else
    WriteLine(aStream,'ERR unknown command');
end;


procedure TConnectionThread.Execute;

var
  lStream: TQuicStreamAdapter;

begin
  try
    repeat
      lStream:=FConnection.AcceptStream(60000);
      if lStream=nil then
        Break;
      try
        try
          HandleRequest(lStream);
          lStream.Finish;
        except
          on E: Exception do
            begin
            WriteLn('Request failed: ',E.Message);
            lStream.Reset(1);
            end;
        end;
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
  if ParamCount<3 then
    begin
    WriteLn('Usage: quicfileserver <certificate.pem> <key.pem> <directory> [port] [libssl] [libcrypto]');
    Halt(1);
    end;
  if not DirectoryExists(ParamStr(3)) then
    begin
    WriteLn('Directory not found: ',ParamStr(3));
    Halt(1);
    end;
  if not QuicInitOpenSSL(ParamStr(5),ParamStr(6)) then
    begin
    WriteLn('OpenSSL 3.5 or later is required');
    Halt(1);
    end;
  lPort:=StrToIntDef(ParamStr(4),4433);
  lTLS:=Default(TQuicTLSConfig);
  lTLS.Alpn:=['qfile'];
  lTLS.CertificateFile:=ParamStr(1);
  lTLS.PrivateKeyFile:=ParamStr(2);
  lServer:=TQuicServer.Create(lTLS,QuicDefaultEndpointParams);
  try
    lServer.Listen('::',lPort);
    WriteLn('Serving ',ExpandFileName(ParamStr(3)),' on UDP port ',lServer.Port);
    repeat
      lConnection:=lServer.Accept(1000);
      if lConnection<>nil then
        begin
        WriteLn('Connection from ',lConnection.Address.Address,':',lConnection.Port);
        TConnectionThread.Create(lConnection,ParamStr(3));
        end;
    until False;
  finally
    lServer.Free;
  end;
end.
