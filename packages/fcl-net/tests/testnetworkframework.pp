program testnetworkframework;

{ Tests the Network.framework TLS handler. Needs network access. }

{$mode objfpc}{$H+}

uses
  SysUtils, ctypes, ssockets, sslsockets, fphttpclient, networkframeworksslsockets;

function _dyld_image_count: cuint32; cdecl; external;
function _dyld_get_image_name(image_index: cuint32): PAnsiChar; cdecl; external;

type
  TVeto = class
    Calls : Integer;
    procedure Verify(Sender: TObject; aHandler: TSSLSocketHandler; var aAllow: Boolean);
  end;

procedure TVeto.Verify(Sender: TObject; aHandler: TSSLSocketHandler; var aAllow: Boolean);
begin
  Inc(Calls);
  aAllow:=False;
end;

procedure Check(Condition: Boolean; const Msg: String);
begin
  if not Condition then
    begin
    Writeln('FAIL: ',Msg);
    Halt(1);
    end;
  Writeln('ok   ',Msg);
end;

// HTTP status, or -1 if the request raised an exception.
function Get(const aURL: String; aVerify: Boolean; aVeto: TVeto = Nil): Integer;
var
  C : TFPHTTPClient;
  Body : String;
begin
  C:=TFPHTTPClient.Create(Nil);
  try
    C.VerifySSLCertificate:=aVerify;
    C.ConnectTimeout:=10000;
    C.IOTimeout:=10000;
    if Assigned(aVeto) then
      C.OnVerifySSLCertificate:=@aVeto.Verify;
    try
      Body:=C.Get(aURL);
      Result:=C.ResponseStatusCode;
      if Body='' then
        Result:=-2;
    except
      on E: Exception do
        begin
        Writeln('     ',aURL,': ',E.Message);
        Result:=-1;
        end;
    end;
  finally
    C.Free;
  end;
end;

procedure TestRaw;
var
  S : TInetSocket;
  Request, Response : AnsiString;
  Buf : array[0..4095] of AnsiChar;
  N : Integer;
begin
  S:=TInetSocket.Create('example.com',443,TNetworkFrameworkSocketHandler.Create);
  try
    S.ConnectTimeout:=10000;
    S.IOTimeout:=10000;
    S.Connect;
    Request:='GET / HTTP/1.0'#13#10'Host: example.com'#13#10#13#10;
    Check(S.Write(Request[1],Length(Request))=Length(Request),'raw write');
    Check(S.CanRead(10000),'raw CanRead');
    Response:='';
    repeat
      N:=S.Read(Buf,SizeOf(Buf));
      if N>0 then
        Response:=Response+Copy(Buf,1,N);
    until N<=0;
    Check(N=0,'raw read ends with EOF');
    Check(Copy(Response,1,7)='HTTP/1.','raw response: '+Copy(Response,1,Pos(#13,Response)-1));
  finally
    S.Free;
  end;
end;

procedure TestRefused;
var
  S : TInetSocket;
  Raised : Boolean;
begin
  S:=TInetSocket.Create('127.0.0.1',1,TNetworkFrameworkSocketHandler.Create);
  try
    S.ConnectTimeout:=5000;
    Raised:=False;
    try
      S.Connect;
    except
      on E: ESocketError do
        begin
        Writeln('     refused: ',E.Message);
        Raised:=E.Code=seConnectFailed;
        end;
    end;
    Check(Raised,'connection refused raises seConnectFailed');
  finally
    S.Free;
  end;
end;

procedure CheckNoTLSLibraries;
var
  I : cuint32;
  Name : String;
begin
  for I:=0 to _dyld_image_count-1 do
    begin
    Name:=LowerCase(StrPas(_dyld_get_image_name(I)));
    // The system's own libraries (e.g. /usr/lib/libcrypto.46.dylib) are used by Apple frameworks.
    if (Pos('/usr/lib/',Name)=1) or (Pos('/system/',Name)=1) then
      Continue;
    if (Pos('libssl',Name)>0) or (Pos('libcrypto',Name)>0) or (Pos('gnutls',Name)>0) then
      Check(False,'third-party TLS library loaded: '+Name);
    end;
  Check(True,'no third-party TLS library loaded');
end;

var
  Veto : TVeto;

begin
  Check(TSSLSocketHandler.GetDefaultHandlerClass=TNetworkFrameworkSocketHandler,'handler registered');
  Check(Get('https://example.com/',True)=200,'trusted host with verification');
  Check(Get('https://expired.badssl.com/',True)=-1,'expired certificate rejected');
  Check(Get('https://expired.badssl.com/',False)=200,'expired certificate accepted without verification');
  Check(Get('https://wrong.host.badssl.com/',True)=-1,'wrong host rejected');
  Check(Get('https://wrong.host.badssl.com/',False)=200,'wrong host accepted without verification');
  Veto:=TVeto.Create;
  try
    Check(Get('https://example.com/',True,Veto)=-1,'OnVerifySSLCertificate veto');
    Check(Veto.Calls=1,'veto called once');
  finally
    Veto.Free;
  end;
  TestRaw;
  TestRefused;
  CheckNoTLSLibraries;
  Writeln('PASS');
end.
