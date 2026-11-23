{ Tests the CORS origin headers that TSQLDBRestDispatcher sets on a response. }
unit tcrestcors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, httpdefs, httpprotocol, sqldbrestio, sqldbrestbridge;

type

  { TRestCORSTestResponse }

  // Response that sends nothing.
  TRestCORSTestResponse = class(TResponse)
  protected
    procedure DoSendHeaders(aHeaders: TStrings); override;
    procedure DoSendContent; override;
  end;

  { TCORSTestDispatcher }

  // Dispatcher that exposes the CORS response header handling.
  TCORSTestDispatcher = class(TSQLDBRestDispatcher)
  public
    // Call HandleCorsResponseHeaders for IO.
    procedure CallCorsResponseHeaders(IO: TRestIO);
  end;

  { TTestRestCORS }

  TTestRestCORS = class(TTestCase)
  private
    FDispatcher: TCORSTestDispatcher;
    FRequest: TRequest;
    FResponse: TRestCORSTestResponse;
    FIO: TRestIO;
    procedure Handle(const aOrigin: String);
    function AllowOrigin: String;
    function AllowCredentials: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNotHandledSetsNothing;
    procedure TestListedOriginEchoed;
    procedure TestListedOriginSendsVary;
    procedure TestUnlistedOriginGetsNoHeader;
    procedure TestUnlistedOriginGetsNoCredentials;
    procedure TestHostEntryWithPort;
    procedure TestEmptyListIsStar;
    procedure TestEmptyListStarSendsNoVary;
    procedure TestEmptyListMirrorsWithOption;
    procedure TestMirrorFromReferer;
    procedure TestCredentialsOffSendsNoHeader;
  end;

implementation


{ TRestCORSTestResponse }

procedure TRestCORSTestResponse.DoSendHeaders(aHeaders: TStrings);

begin
end;


procedure TRestCORSTestResponse.DoSendContent;

begin
end;


{ TCORSTestDispatcher }

procedure TCORSTestDispatcher.CallCorsResponseHeaders(IO: TRestIO);

begin
  HandleCorsResponseHeaders(IO);
end;


{ TTestRestCORS }

procedure TTestRestCORS.Handle(const aOrigin: String);

begin
  if aOrigin<>'' then
    FRequest.SetCustomHeader('Origin',aOrigin);
  FDispatcher.CallCorsResponseHeaders(FIO);
end;


function TTestRestCORS.AllowOrigin: String;

begin
  Result:=FResponse.GetCustomHeader('Access-Control-Allow-Origin');
end;


function TTestRestCORS.AllowCredentials: String;

begin
  Result:=FResponse.GetCustomHeader('Access-Control-Allow-Credentials');
end;


procedure TTestRestCORS.SetUp;

begin
  FDispatcher:=TCORSTestDispatcher.Create(nil);
  FDispatcher.DispatchOptions:=FDispatcher.DispatchOptions+[rdoHandleCORS];
  FRequest:=TRequest.Create;
  FRequest.Method:='GET';
  FResponse:=TRestCORSTestResponse.Create(FRequest);
  FIO:=TRestIO.Create(FRequest,FResponse);
end;


procedure TTestRestCORS.TearDown;

begin
  FreeAndNil(FIO);
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  FreeAndNil(FDispatcher);
end;


procedure TTestRestCORS.TestNotHandledSetsNothing;

begin
  FDispatcher.DispatchOptions:=FDispatcher.DispatchOptions-[rdoHandleCORS];
  Handle('https://a.example');
  AssertEquals('Without rdoHandleCORS no Allow-Origin is set', '', AllowOrigin);
  AssertEquals('Without rdoHandleCORS no Allow-Credentials is set', '', AllowCredentials);
end;


procedure TTestRestCORS.TestListedOriginEchoed;

begin
  FDispatcher.CORSAllowedOrigins:='https://a.example, https://b.example';
  Handle('https://b.example');
  AssertEquals('Listed origin is sent back alone', 'https://b.example', AllowOrigin);
  AssertEquals('Listed origin gets Allow-Credentials', 'true', AllowCredentials);
end;


procedure TTestRestCORS.TestListedOriginSendsVary;

begin
  FDispatcher.CORSAllowedOrigins:='https://a.example';
  Handle('https://a.example');
  AssertEquals('Matched origin sends Vary: Origin', 'Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestRestCORS.TestUnlistedOriginGetsNoHeader;

begin
  FDispatcher.CORSAllowedOrigins:='https://a.example, https://b.example';
  Handle('https://evil.example');
  AssertEquals('Unlisted origin gets no Allow-Origin', '', AllowOrigin);
  AssertEquals('Unlisted origin still sends Vary: Origin', 'Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestRestCORS.TestUnlistedOriginGetsNoCredentials;

begin
  FDispatcher.CORSAllowedOrigins:='https://a.example';
  Handle('https://evil.example');
  AssertEquals('Unlisted origin gets no Allow-Credentials', '', AllowCredentials);
end;


procedure TTestRestCORS.TestHostEntryWithPort;

begin
  FDispatcher.CORSAllowedOrigins:='localhost';
  Handle('http://localhost:8080');
  AssertEquals('localhost entry allows a localhost origin with a port', 'http://localhost:8080', AllowOrigin);
end;


procedure TTestRestCORS.TestEmptyListIsStar;

begin
  Handle('https://evil.example');
  AssertEquals('Empty list without rdoEmptyCORSDomainToOrigin sends a star', '*', AllowOrigin);
end;


procedure TTestRestCORS.TestEmptyListStarSendsNoVary;

begin
  Handle('https://evil.example');
  AssertEquals('A star does not depend on the origin', '', FResponse.GetHeader(hhVary));
end;


procedure TTestRestCORS.TestEmptyListMirrorsWithOption;

begin
  FDispatcher.DispatchOptions:=FDispatcher.DispatchOptions+[rdoEmptyCORSDomainToOrigin];
  Handle('https://a.example');
  AssertEquals('Empty list with rdoEmptyCORSDomainToOrigin mirrors the origin', 'https://a.example', AllowOrigin);
  AssertEquals('Mirrored origin sends Vary: Origin', 'Origin', FResponse.GetHeader(hhVary));
end;


procedure TTestRestCORS.TestMirrorFromReferer;

begin
  FDispatcher.DispatchOptions:=FDispatcher.DispatchOptions+[rdoEmptyCORSDomainToOrigin];
  FRequest.Referer:='https://a.example:8443/page.html';
  Handle('');
  AssertEquals('Without Origin the referer origin is mirrored', 'https://a.example:8443', AllowOrigin);
end;


procedure TTestRestCORS.TestCredentialsOffSendsNoHeader;

begin
  FDispatcher.CORSAllowedOrigins:='https://a.example';
  FDispatcher.CORSAllowCredentials:=False;
  Handle('https://a.example');
  AssertEquals('CORSAllowCredentials=False sends no Allow-Credentials', '', AllowCredentials);
end;

initialization
  RegisterTest(TTestRestCORS);
end.
