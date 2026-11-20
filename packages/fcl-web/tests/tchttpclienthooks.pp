{ Tests the protected request and response hooks of TFPHTTPClient and the client factory of TFPHTTPWebClient. }
unit tchttpclienthooks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uriparser, fphttpclient, fpwebclient, fphttpwebclient;

type

  { THookClient }

  THookClient = class(TFPHTTPClient)
  public
    // Calls PrepareRequestHeaders for aURL into aHeaders.
    procedure Prepare(const aURL: String; aHeaders: TStrings);
    // Calls AddResponseHeader.
    procedure AddLine(const aLine: String);
    // Calls ResetResponse.
    procedure Reset;
  end;

  { THookWebClient }

  THookWebClient = class(TFPHTTPWebClient)
  private
    FCreated: Integer;
  protected
    function CreateHTTPClient: TFPHTTPClient; override;
  public
    // Number of HTTP clients created.
    property Created: Integer read FCreated;
  end;

  { TTestHTTPClientHooks }

  TTestHTTPClientHooks = class(TTestCase)
  private
    FClient: THookClient;
    FHeaders: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPrepareHeaders;
    procedure TestPrepareAuthorization;
    procedure TestPrepareContentLength;
    procedure TestPrepareCookies;
    procedure TestAddResponseHeader;
    procedure TestAddResponseCookie;
    procedure TestAddResponseHeaderLimit;
    procedure TestResetResponse;
    procedure TestWebClientFactory;
  end;

implementation

{ THookClient }

procedure THookClient.Prepare(const aURL: String; aHeaders: TStrings);

begin
  PrepareRequestHeaders('GET',ParseURI(aURL,False),aHeaders);
end;


procedure THookClient.AddLine(const aLine: String);

begin
  AddResponseHeader(aLine);
end;


procedure THookClient.Reset;

begin
  ResetResponse;
end;


{ THookWebClient }

function THookWebClient.CreateHTTPClient: TFPHTTPClient;

begin
  Inc(FCreated);
  Result:=inherited CreateHTTPClient;
end;


{ TTestHTTPClientHooks }

procedure TTestHTTPClientHooks.SetUp;

begin
  FClient:=THookClient.Create(nil);
  FHeaders:=TStringList.Create;
end;


procedure TTestHTTPClientHooks.TearDown;

begin
  FreeAndNil(FHeaders);
  FreeAndNil(FClient);
end;


procedure TTestHTTPClientHooks.TestPrepareHeaders;

begin
  FClient.AddHeader('Accept','text/plain');
  FClient.Prepare('http://example.org:8080/a',FHeaders);
  AssertEquals('Host line first','Host: example.org:8080',FHeaders[0]);
  AssertTrue('User header line',FHeaders.IndexOf('Accept: text/plain')>0);
  AssertEquals('No request line',-1,FHeaders.IndexOf('GET / HTTP/1.1'));
end;


procedure TTestHTTPClientHooks.TestPrepareAuthorization;

begin
  FClient.UserName:='user';
  FClient.Password:='pass';
  FClient.Prepare('http://example.org/',FHeaders);
  AssertEquals('Basic authorization line','Authorization: Basic dXNlcjpwYXNz',FHeaders[0]);
end;


procedure TTestHTTPClientHooks.TestPrepareContentLength;

var
  lBody: TStringStream;

begin
  lBody:=TStringStream.Create('12345');
  try
    FClient.RequestBody:=lBody;
    FClient.Prepare('http://example.org/',FHeaders);
    AssertTrue('Content-Length line',FHeaders.IndexOf('Content-Length: 5')>=0);
    AssertEquals('Content-Length not left in RequestHeaders',-1,FClient.IndexOfHeader('Content-Length'));
  finally
    FClient.RequestBody:=nil;
    lBody.Free;
  end;
end;


procedure TTestHTTPClientHooks.TestPrepareCookies;

begin
  FClient.Cookies.Add('a=1');
  with FClient.RequestCookies.Add do
    begin
    Name:='b';
    Value:='2';
    end;
  FClient.Prepare('http://example.org/',FHeaders);
  AssertEquals('Cookie line last','Cookie: a=1; b=2',FHeaders[FHeaders.Count-1]);
  AssertEquals('Pending cookies moved to the sent cookies',0,FClient.Cookies.Count);
end;


procedure TTestHTTPClientHooks.TestAddResponseHeader;

begin
  FClient.AddLine('Content-Type: text/plain');
  AssertEquals('Header added','text/plain',FClient.GetHeader(FClient.ResponseHeaders,'Content-Type'));
  AssertEquals('No cookie',0,FClient.ResponseCookies.Count);
end;


procedure TTestHTTPClientHooks.TestAddResponseCookie;

begin
  FClient.AddLine('Set-Cookie: id=42; Path=/');
  AssertEquals('Response cookie parsed',1,FClient.ResponseCookies.Count);
  AssertEquals('Cookie name','id',FClient.ResponseCookies[0].Name);
  AssertTrue('Cookie kept for the next request',FClient.Cookies.IndexOf('id=42')>=0);
end;


procedure TTestHTTPClientHooks.TestAddResponseHeaderLimit;

var
  lRaised: Boolean;

begin
  FClient.MaxHeaderCount:=1;
  FClient.AddLine('A: 1');
  FClient.AddLine('B: 2');
  lRaised:=False;
  try
    FClient.AddLine('C: 3');
  except
    on EHTTPClient do
      lRaised:=True;
  end;
  AssertTrue('Header count limit enforced',lRaised);
end;


procedure TTestHTTPClientHooks.TestResetResponse;

begin
  FClient.AddLine('Set-Cookie: id=42');
  FClient.Reset;
  AssertEquals('Headers cleared',0,FClient.ResponseHeaders.Count);
  AssertEquals('Response cookies cleared',0,FClient.ResponseCookies.Count);
  AssertEquals('Status cleared',0,FClient.ResponseStatusCode);
end;


procedure TTestHTTPClientHooks.TestWebClientFactory;

var
  lWeb: THookWebClient;
  lRequest: TWebClientRequest;

begin
  lWeb:=THookWebClient.Create(nil);
  try
    lRequest:=lWeb.CreateRequest;
    lRequest.Free;
    AssertEquals('Request created through CreateHTTPClient',1,lWeb.Created);
  finally
    lWeb.Free;
  end;
end;


initialization
  RegisterTest(TTestHTTPClientHooks);
end.
