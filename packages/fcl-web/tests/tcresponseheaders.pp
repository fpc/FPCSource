{ Tests that TResponse.CollectHeaders sends no CR, LF or NUL and no invalid header names. }
unit tcresponseheaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, httpdefs;

type

  { THeaderCaptureResponse }

  // Response that keeps the header lines passed to DoSendHeaders.
  THeaderCaptureResponse = class(TResponse)
  private
    FSent: TStringList;
  protected
    procedure DoSendHeaders(aHeaders: TStrings); override;
    procedure DoSendContent; override;
  public
    // Create the response for aRequest with an empty list of sent lines.
    constructor Create(aRequest: TRequest); overload;
    destructor Destroy; override;
    // Header lines as passed to DoSendHeaders.
    property Sent: TStringList read FSent;
  end;

  { TTestResponseHeaders }

  TTestResponseHeaders = class(TTestCase)
  private
    FRequest: TRequest;
    FResponse: THeaderCaptureResponse;
    procedure AssertNoLineBreaks;
    function SentLine(const aPrefix: String): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetCustomHeaderValue;
    procedure TestCustomHeadersValuesDirect;
    procedure TestCustomHeaderNameWithLineBreak;
    procedure TestCustomHeaderNameWithColon;
    procedure TestCustomHeaderNameWithSpace;
    procedure TestValidCustomHeaderName;
    procedure TestCodeText;
    procedure TestCleanHeaderKeepsArgument;
  end;

implementation


{ THeaderCaptureResponse }

constructor THeaderCaptureResponse.Create(aRequest: TRequest);

begin
  inherited Create(aRequest);
  FSent:=TStringList.Create;
end;


destructor THeaderCaptureResponse.Destroy;

begin
  FSent.Free;
  inherited Destroy;
end;


procedure THeaderCaptureResponse.DoSendHeaders(aHeaders: TStrings);

begin
  FSent.Assign(aHeaders);
end;


procedure THeaderCaptureResponse.DoSendContent;

begin
end;


{ TTestResponseHeaders }

procedure TTestResponseHeaders.SetUp;

begin
  FRequest:=TRequest.Create;
  FResponse:=THeaderCaptureResponse.Create(FRequest);
end;


procedure TTestResponseHeaders.TearDown;

begin
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
end;


// Fail when a sent header line contains CR, LF or NUL.
procedure TTestResponseHeaders.AssertNoLineBreaks;

var
  S: String;

begin
  for S in FResponse.Sent do
    AssertEquals('No CR, LF or NUL in header line "'+S+'"', 0,
      Pos(#13,S)+Pos(#10,S)+Pos(#0,S));
end;


// Return the first sent line that starts with aPrefix, or an empty string.
function TTestResponseHeaders.SentLine(const aPrefix: String): String;

var
  S: String;

begin
  Result:='';
  for S in FResponse.Sent do
    if Copy(S,1,Length(aPrefix))=aPrefix then
      Exit(S);
end;


procedure TTestResponseHeaders.TestSetCustomHeaderValue;

begin
  FResponse.SetCustomHeader('X-Test','a'#13#10'Injected: yes');
  FResponse.SendHeaders;
  AssertNoLineBreaks;
  AssertEquals('Value with line break replaced by spaces', 'X-Test: a  Injected: yes', SentLine('X-Test:'));
end;


procedure TTestResponseHeaders.TestCustomHeadersValuesDirect;

begin
  FResponse.CustomHeaders.Values['X-Test']:='a'#13#10'Injected: yes';
  FResponse.SendHeaders;
  AssertNoLineBreaks;
  AssertEquals('Value set through CustomHeaders.Values is cleaned', 'X-Test: a  Injected: yes', SentLine('X-Test:'));
end;


procedure TTestResponseHeaders.TestCustomHeaderNameWithLineBreak;

begin
  FResponse.SetCustomHeader('X-A'#13#10'Injected','b');
  FResponse.SendHeaders;
  AssertNoLineBreaks;
  AssertEquals('Header with a line break in its name is not sent', '', SentLine('X-A'));
end;


procedure TTestResponseHeaders.TestCustomHeaderNameWithColon;

begin
  FResponse.SetCustomHeader('X-A: b'#9'X-B','c');
  FResponse.SendHeaders;
  AssertEquals('Header with a colon in its name is not sent', '', SentLine('X-A'));
end;


procedure TTestResponseHeaders.TestCustomHeaderNameWithSpace;

begin
  FResponse.SetCustomHeader('X A','c');
  FResponse.SendHeaders;
  AssertEquals('Header with a space in its name is not sent', '', SentLine('X A'));
end;


procedure TTestResponseHeaders.TestValidCustomHeaderName;

begin
  FResponse.SetCustomHeader('X-Valid_Name.1~','ok');
  FResponse.SendHeaders;
  AssertEquals('Header with a valid token name is sent', 'X-Valid_Name.1~: ok', SentLine('X-Valid'));
end;


procedure TTestResponseHeaders.TestCodeText;

begin
  FResponse.Code:=200;
  FResponse.CodeText:='OK'#13#10'Injected: yes';
  FResponse.SendHeaders;
  AssertNoLineBreaks;
  AssertEquals('Status line with line break replaced by spaces', 'Status: 200 OK  Injected: yes', SentLine('Status:'));
end;


procedure TTestResponseHeaders.TestCleanHeaderKeepsArgument;

var
  lValue: String;

begin
  lValue:='a'#13#10'b';
  UniqueString(lValue);
  FResponse.SetCustomHeader('X-Test',lValue);
  AssertEquals('Caller string is not modified', 'a'#13#10'b', lValue);
  AssertEquals('Stored value is cleaned', 'a  b', FResponse.GetCustomHeader('X-Test'));
end;

initialization
  RegisterTest(TTestResponseHeaders);
end.
