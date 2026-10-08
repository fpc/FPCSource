program testschannel;
{$mode objfpc}{$H+}
uses
{$IFDEF TEST_DOTTEDUNITS}
  System.SysUtils, System.Classes, WinApi.Windows, System.Net.Ssockets,
  System.Net.Sslsockets, System.Net.Sslbase, System.Net.SchannelSSLSockets,
  WinApi.SchannelSSPI, FpWeb.Http.Client;
{$ELSE}
  SysUtils, Classes, Windows, ssockets, sslsockets, sslbase, schannelsslsockets,
  schannelsspi, fphttpclient;
{$ENDIF}

type
  TObserver = class
    Calls: Integer;
    procedure RawVerify(Sender: TObject; var Allow: Boolean);
    procedure HTTPVerify(Sender: TObject; Handler: TSSLSocketHandler; var Allow: Boolean);
  end;

procedure Check(Condition: Boolean; const MessageText: String);
begin
  if not Condition then raise Exception.Create(MessageText);
end;

function TickCount: QWord;
begin
  Result:={$IFDEF TEST_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetTickCount64;
end;

procedure TObserver.RawVerify(Sender: TObject; var Allow: Boolean);
begin
  Inc(Calls);
  Check(Allow,'Unexpected callback input');
  Allow:=ParamStr(7)<>'veto';
end;

procedure TObserver.HTTPVerify(Sender: TObject; Handler: TSSLSocketHandler; var Allow: Boolean);
begin
  RawVerify(Handler,Allow);
end;

procedure CheckModules;
var Snapshot: THandle; Entry: MODULEENTRY32; Name: String;
begin
  Snapshot:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,GetCurrentProcessId);
  Check(Snapshot<>INVALID_HANDLE_VALUE,'Cannot enumerate client modules');
  try
    FillChar(Entry,SizeOf(Entry),0);
    Entry.dwSize:=SizeOf(Entry);
    Check(Module32First(Snapshot,Entry),'Cannot read client module list');
    repeat
      Name:=LowerCase(StrPas(Entry.szModule));
      Check((Pos('libssl',Name)=0) and (Pos('libcrypto',Name)=0) and
        (Pos('gnutls',Name)=0) and (Name<>'ssleay32.dll') and
        (Name<>'libeay32.dll'),'Third-party TLS module loaded: '+Name);
    until not Module32Next(Snapshot,Entry);
  finally CloseHandle(Snapshot) end;
end;

procedure Construct;
var Handler: TSchannelSocketHandler; Rejected: Boolean;
begin
  Writeln('LAYOUT handle=',SizeOf(TSecHandle),' buffer=',SizeOf(TSecBuffer),
    ' desc=',SizeOf(TSecBufferDesc),' cred=',SizeOf(TSchannelCred),
    ' modern=',SizeOf(TSchCredentials));
  Check(SizeOf(TSecHandle)=2*SizeOf(Pointer),'SSPI handle layout differs');
  Check(SizeOf(TSecStreamSizes)=20,'SSPI stream-size layout differs');
{$IFDEF CPU64}
  Check((SizeOf(TSecBuffer)=16) and (SizeOf(TSecBufferDesc)=16) and
    (SizeOf(TSchannelCred)=80) and (SizeOf(TSchCredentials)=72),'Win64 SSPI layout differs');
{$ELSE}
  Check((SizeOf(TSecBuffer)=12) and (SizeOf(TSecBufferDesc)=12) and
    (SizeOf(TSchannelCred)=56) and (SizeOf(TSchCredentials)=44),'Win32 SSPI layout differs');
{$ENDIF}
  Handler:=TSchannelSocketHandler.Create;
  try
    Check(Handler.CertGenerator<>nil,'Certificate-generator contract broken');
    Handler.RemoteHostName:='example.com';
    Check(Handler.CertGenerator.HostName='example.com','Legacy hostname setter broken');
    Rejected:=False;
    try Handler.CreateSelfSignedCertificate
    except on E: ESSLSocketError do Rejected:=True end;
    Check(Rejected,'Certificate generation accepted');
    Check(not Handler.VerifyPeerCert,'Handler verification default changed');
    Check(not Handler.Accept,'Client-only handler accepted a connection');
    Check((not Handler.SSLActive) and (Handler.LastSSLErrorCode<>0),'Accept did not fail closed');
    Check(TSSLSocketHandler.GetDefaultHandlerClass=TSchannelSocketHandler,'Registration failed');
  finally Handler.Free end;
end;

procedure CheckPattern(const Buffer; Count, Offset: Integer);
var I: Integer;
begin
  for I:=0 to Count-1 do
    Check((PByte(@Buffer)+I)^=Byte((Offset+I) mod 251),'Body differs at '+IntToStr(Offset+I));
end;

procedure CheckFailure(const ErrorText: String);
begin
  Check(ParamStr(4)='reject','Unexpected error: '+ErrorText);
  Check((ParamStr(6)<>'') and (Pos(UpperCase(ParamStr(6)),UpperCase(ErrorText))>0),
    'Expected specific TLS error '+ParamStr(6)+', got '+ErrorText);
end;

procedure HTTPTest;
var Client: TFPHTTPClient; Observer: TObserver; Body: String; Failed: Boolean;
begin
  Client:=TFPHTTPClient.Create(nil);
  Observer:=TObserver.Create;
  try
    Check(not Client.VerifySSLCertificate,'HTTP verification default changed');
    if ParamStr(3)<>'default' then Client.VerifySSLCertificate:=StrToBool(ParamStr(3));
    if ParamStr(7)<>'none' then Client.OnVerifySSLCertificate:=@Observer.HTTPVerify;
    Client.ConnectTimeout:=5000;
    Client.IOTimeout:=3000;
    Client.AllowRedirect:=False;
    Failed:=False;
    try Body:=Client.Get(ParamStr(2))
    except on E: Exception do
      begin Failed:=True; Writeln('ERROR ',E.Message); CheckFailure(E.Message) end end;
    Check(Failed=(ParamStr(4)='reject'),'Invalid certificate or TLS failure accepted');
    if not Failed then
      begin
      Check(Client.ResponseStatusCode=200,'Expected HTTP 200');
      if ParamStr(5)='fixture' then Check(Body='local tls fixture'+#10,'HTTP body differs')
      else if ParamStr(5)='pattern' then
        begin
        Check(Length(Body)=1048576,'Large HTTP body length differs');
        CheckPattern(Body[1],Length(Body),0);
        end
      else Check(Length(Body)>0,'Empty public HTTPS response');
      Writeln('BODY ',Length(Body));
      end;
    Check(Observer.Calls=StrToInt(ParamStr(8)),'Unexpected HTTP callback count');
  finally Observer.Free; Client.Free end;
end;

procedure RawTest;
var Handler: TSchannelSocketHandler; Stream: TInetSocket; Observer: TObserver;
    Buffer: array[0..4095] of Byte; Upload: TBytes;
    N, Total, Expected, I: Integer; Started: QWord; Failed: Boolean; Mode: String;
begin
  Handler:=TSchannelSocketHandler.Create;
  Observer:=TObserver.Create;
  if ParamStr(3)<>'default' then Handler.VerifyPeerCert:=StrToBool(ParamStr(3));
  Handler.ServerName:=ParamStr(9);
  if Handler.ServerName='nul' then Handler.ServerName:='localhost'+#0+'invalid';
  if ParamStr(10)='tls12' then Handler.SSLType:=stTLSv1_2;
  if ParamStr(7)<>'none' then Handler.OnVerifyCertificate:=@Observer.RawVerify;
  if ParamStr(12)='' then Stream:=TInetSocket.Create('127.0.0.1',StrToInt(ParamStr(2)),Handler)
  else Stream:=TInetSocket.Create(ParamStr(12),StrToInt(ParamStr(2)),Handler);
  try
    Stream.ConnectTimeout:=3000;
    Stream.IOTimeout:=3000;
    Mode:=ParamStr(5);
    if Mode='handshake-timeout' then Stream.ConnectTimeout:=150;
    if Mode='unsupported' then Handler.CertificateData.CertCA.FileName:='unsupported-test-ca.pem';
    Failed:=False;
    Started:=TickCount;
    try
      Stream.Connect;
      Check(Handler.SSLActive,'Handshake did not activate TLS');
      if Mode='connect-only' then
        begin
        Check(ParamStr(4)='ok','Expected certificate rejection missing');
        Check(Observer.Calls=StrToInt(ParamStr(8)),'Unexpected raw callback count');
        Check(Handler.Shutdown(False),'Connected TLS close failed');
        Check(not Handler.SSLActive,'Close left TLS active');
        Check(Handler.Shutdown(False),'Repeated close failed');
        Exit;
        end;
      if Mode='read-timeout' then
        begin
        Stream.IOTimeout:=150;
        Started:=TickCount;
        Check(Stream.Read(Buffer,SizeOf(Buffer))=-1,'Read timeout did not fail');
        raise Exception.Create(Handler.LastSSLErrorString);
        end;
      if Mode='shutdown-timeout' then
        begin
        Stream.IOTimeout:=150;
        Started:=TickCount;
        Check(not Handler.Shutdown(True),'Unanswered bidirectional close succeeded');
        raise Exception.Create(Handler.LastSSLErrorString);
        end;
      if (Mode='send') or (Mode='send-timeout') then
        begin
        N:=1048576;
        if Mode='send-timeout' then
          begin N:=16777216; Stream.IOTimeout:=150; Started:=TickCount end;
        SetLength(Upload,N);
        for I:=0 to N-1 do Upload[I]:=Byte(I mod 251);
        I:=Stream.Write(Upload[0],N);
        if I<0 then raise Exception.Create(Handler.LastSSLErrorString);
        Check(I=N,'Partial send reported as success');
        end;
      Expected:=StrToInt(ParamStr(11));
      Total:=0;
      repeat
        Check(Stream.CanRead(3000),'Receive readiness timed out');
        N:=SizeOf(Buffer);
        if Total<16 then N:=1;
        N:=Stream.Read(Buffer,N);
        if N<0 then raise Exception.Create(Handler.LastSSLErrorString);
        CheckPattern(Buffer,N,Total);
        Inc(Total,N);
        Check(Total<=Expected,'Extra TLS control bytes delivered as plaintext');
      until N=0;
      Check(Total=Expected,'Raw body length differs');
      Check(Stream.Read(Buffer,1)=0,'EOF was not stable');
      Check(Handler.Shutdown(True),'TLS close failed: '+Handler.LastSSLErrorString);
      Check(not Handler.SSLActive,'Shutdown left TLS active');
      Check(Handler.Shutdown(False),'Repeated shutdown failed');
      Writeln('BODY ',Total);
    except on E: Exception do
      begin
      Failed:=True;
      Writeln('ERROR ',E.Message);
      CheckFailure(E.Message);
      Check(not Handler.SSLActive,'Failed TLS remained active');
      end end;
    if (Pos('timeout',Mode)>0) then Check(TickCount-Started<2000,'Deadline exceeded');
    Check(Failed=(ParamStr(4)='reject'),'Expected TLS rejection missing');
    Check(Observer.Calls=StrToInt(ParamStr(8)),'Unexpected raw callback count');
  finally Stream.Free; Observer.Free end;
end;

begin
  if ParamStr(1)='construct' then Construct
  else if ParamStr(1)='http' then HTTPTest
  else if ParamStr(1)='raw' then RawTest
  else raise Exception.Create('Usage: construct | http/raw target verify expected mode code callback calls [name protocol bytes]');
  CheckModules;
  Writeln('PASS ',ParamStr(1),' no third-party TLS modules');
end.
