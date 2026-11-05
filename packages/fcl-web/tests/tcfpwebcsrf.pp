{ Tests the CSRF token check of TFPWebModule with and without a session. }
unit tcfpwebcsrf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, httpdefs, fphttp, fpweb;

type

  { TTestCSRFResponse }

  TTestCSRFResponse = class(TResponse)
  protected
    procedure DoSendHeaders(aHeaders: TStrings); override;
    procedure DoSendContent; override;
  end;

  { TTestCSRFSession }

  TTestCSRFSession = class(TCustomSession)
  private
    FVariables: TStrings;
  protected
    function GetSessionVariable(const aVarName: String): String; override;
    procedure SetSessionVariable(const aVarName: String; const aValue: String); override;
  public
    // Create a session that keeps its variables in memory
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    // Nothing to write to the response
    procedure UpdateResponse(aResponse: TResponse); override;
    // Remove variable aVariableName
    procedure RemoveVariable(const aVariableName: String); override;
    // Clear all variables
    procedure Terminate; override;
    // True if aVarName is set
    function SessionVariableExists(const aVarName: String): Boolean; override;
  end;

  { TTestCSRFSessionFactory }

  TTestCSRFSessionFactory = class(TSessionFactory)
  protected
    function DoCreateSession(aRequest: TRequest): TCustomSession; override;
    procedure DoDoneSession(var aSession: TCustomSession); override;
    procedure DoCleanupSessions; override;
  end;

  { TTestFPWebCSRF }

  TTestFPWebCSRF = class(TTestCase)
  private
    FModule: TFPWebModule;
    FRequest: TRequest;
    FResponse: TTestCSRFResponse;
    FHandled: Boolean;
    procedure DoAction(Sender: TObject; aRequest: TRequest; aResponse: TResponse; var aHandled: Boolean);
    procedure UseSessionWithToken(const aToken: String);
    procedure Run(const aMethod: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPostWithoutSession;
    procedure TestGetWithoutSession;
    procedure TestPostWithSessionWithoutTokenVariable;
    procedure TestPostWithSessionMissingToken;
    procedure TestPostWithSessionWrongToken;
    procedure TestPostWithSessionMatchingToken;
    procedure TestGetWithSessionMissingToken;
  end;

implementation

const
  cToken = 'a1b2c3';


{ TTestCSRFResponse }

procedure TTestCSRFResponse.DoSendHeaders(aHeaders: TStrings);

begin
end;


procedure TTestCSRFResponse.DoSendContent;

begin
end;


{ TTestCSRFSession }

constructor TTestCSRFSession.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FVariables:=TStringList.Create;
end;


destructor TTestCSRFSession.Destroy;

begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;


function TTestCSRFSession.GetSessionVariable(const aVarName: String): String;

begin
  Result:=FVariables.Values[aVarName];
end;


procedure TTestCSRFSession.SetSessionVariable(const aVarName: String; const aValue: String);

begin
  FVariables.Values[aVarName]:=aValue;
end;


procedure TTestCSRFSession.UpdateResponse(aResponse: TResponse);

begin
end;


procedure TTestCSRFSession.RemoveVariable(const aVariableName: String);

var
  lIdx: Integer;

begin
  lIdx:=FVariables.IndexOfName(aVariableName);
  if lIdx>=0 then
    FVariables.Delete(lIdx);
end;


procedure TTestCSRFSession.Terminate;

begin
  FVariables.Clear;
end;


function TTestCSRFSession.SessionVariableExists(const aVarName: String): Boolean;

begin
  Result:=FVariables.IndexOfName(aVarName)>=0;
end;


{ TTestCSRFSessionFactory }

function TTestCSRFSessionFactory.DoCreateSession(aRequest: TRequest): TCustomSession;

begin
  Result:=TTestCSRFSession.Create(nil);
end;


procedure TTestCSRFSessionFactory.DoDoneSession(var aSession: TCustomSession);

begin
  FreeAndNil(aSession);
end;


procedure TTestCSRFSessionFactory.DoCleanupSessions;

begin
end;


{ TTestFPWebCSRF }

procedure TTestFPWebCSRF.SetUp;

var
  lAction: TFPWebAction;

begin
  FModule:=TFPWebModule.CreateNew(nil, 0);
  lAction:=FModule.Actions.Add;
  lAction.Name:='go';
  lAction.Default:=True;
  lAction.OnRequest:=@DoAction;
  FRequest:=TRequest.Create;
  FRequest.PathInfo:='/go';
  FResponse:=TTestCSRFResponse.Create(FRequest);
  FHandled:=False;
end;


procedure TTestFPWebCSRF.TearDown;

begin
  FreeAndNil(FModule);
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
end;


procedure TTestFPWebCSRF.DoAction(Sender: TObject; aRequest: TRequest; aResponse: TResponse;
  var aHandled: Boolean);

begin
  aResponse.Content:='ok';
  aHandled:=True;
  FHandled:=True;
end;


procedure TTestFPWebCSRF.UseSessionWithToken(const aToken: String);

var
  lSession: TTestCSRFSession;

begin
  { The module hands the session to the session factory, which frees it. }
  lSession:=TTestCSRFSession.Create(nil);
  if aToken<>'' then
    lSession.Variables[cCSRFVariable]:=aToken;
  FModule.Session:=lSession;
end;


procedure TTestFPWebCSRF.Run(const aMethod: String);

begin
  FRequest.Method:=aMethod;
  FModule.HandleRequest(FRequest, FResponse);
end;


procedure TTestFPWebCSRF.TestPostWithoutSession;

begin
  Run('POST');
  AssertTrue('POST without session reaches the action', FHandled);
  AssertEquals('POST without session gets the action content', 'ok', Trim(FResponse.Content));
  AssertFalse('handling the request creates no session', FModule.SessionExists);
end;


procedure TTestFPWebCSRF.TestGetWithoutSession;

begin
  Run('GET');
  AssertTrue('GET without session reaches the action', FHandled);
  AssertFalse('handling the request creates no session', FModule.SessionExists);
end;


procedure TTestFPWebCSRF.TestPostWithSessionWithoutTokenVariable;

begin
  UseSessionWithToken('');
  Run('POST');
  AssertTrue('session without token: POST reaches the action', FHandled);
end;


procedure TTestFPWebCSRF.TestPostWithSessionMissingToken;

var
  lStatus: Integer;

begin
  UseSessionWithToken(cToken);
  lStatus:=0;
  try
    Run('POST');
  except
    on E: EHTTP do
      lStatus:=E.StatusCode;
  end;
  AssertEquals('POST without the session token is forbidden', 403, lStatus);
  AssertFalse('the action is not run', FHandled);
end;


procedure TTestFPWebCSRF.TestPostWithSessionWrongToken;

var
  lStatus: Integer;

begin
  UseSessionWithToken(cToken);
  FRequest.ContentFields.Values[cCSRFVariable]:='wrong';
  lStatus:=0;
  try
    Run('POST');
  except
    on E: EHTTP do
      lStatus:=E.StatusCode;
  end;
  AssertEquals('POST with a different token is forbidden', 403, lStatus);
  AssertFalse('the action is not run', FHandled);
end;


procedure TTestFPWebCSRF.TestPostWithSessionMatchingToken;

begin
  UseSessionWithToken(cToken);
  FRequest.ContentFields.Values[cCSRFVariable]:=cToken;
  Run('POST');
  AssertTrue('POST with the session token reaches the action', FHandled);
end;


procedure TTestFPWebCSRF.TestGetWithSessionMissingToken;

begin
  UseSessionWithToken(cToken);
  Run('GET');
  AssertTrue('GET is not checked for the token', FHandled);
end;


initialization
  if SessionFactoryClass=nil then
    SessionFactoryClass:=TTestCSRFSessionFactory;
  RegisterTest(TTestFPWebCSRF);
end.
