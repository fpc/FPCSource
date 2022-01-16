{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017 by the Free Pascal development team

    Various helper classes to help in unit testing fpweb based code.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit tcwebmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fphttp, fpcunit, custweb;

Type

  { TFakeRequest }

  TFakeRequest = Class(TRequest)
  Protected
    Procedure InitRequest;
  Public
    Procedure SetAuthentication(Const AUserName,APassword : String);
  end;

  { TFakeResponse }

  TFakeResponse = Class(TResponse)
  private
    FSCCC: Integer;
    FSentContent: TStringStream;
    FFields : TStrings;
    FSentHeaders: TStrings;
    FSHCC: Integer;
    function GetSCS: Ansistring;
  protected
    Function GetFieldValue(Index : Integer) : String; override;
    Procedure SetFieldValue(Index : Integer; Value : String); override;
    Procedure DoSendHeaders(Headers : TStrings); override;
    Procedure DoSendContent; override;
  Public
    Destructor Destroy; override;
    Property SendHeaderCallCount: Integer Read FSHCC;
    Property SendContentCallCount: Integer Read FSCCC;
    Property SentHeaders : TStrings Read FSentHeaders;
    Property SentContent : TStringStream Read FSentContent;
    Property SentContentAsString : Ansistring Read GetSCS;
  end;

  { TFakeSession }

  TFakeSession = Class(TCustomSession)
  private
    FValues : Tstrings;
    procedure CheckValues;
    function GetValues: TStrings;
  Protected
    Destructor Destroy; override;
    Function GetSessionVariable(VarName : String) : String; override;
    procedure SetSessionVariable(VarName : String; const AValue: String);override;
    Property Values : TStrings Read GetValues;
  end;

  { TFakeSessionFactory }

  TFakeSessionFactory = Class(TSessionFactory)
  public
    Class Var FSession: TCustomSession;
  published
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
    Procedure DoDoneSession(Var ASession : TCustomSession); override;
    Procedure DoCleanupSessions; override;
  end;

  { TFakeWebHandler }

  TFakeWebHandler = Class(TWebhandler)
  private
    FFakeRequest: TRequest;
    FFakeResponse: TResponse;
  Protected
    // Sets terminated to true after being called
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean; override;
    // Do not free request/response, as we're not the owner
    procedure EndRequest(ARequest : TRequest;AResponse : TResponse); override;
  Public
    // Set these to make WaitForRequest return true. They will be cleared when EndRequest is called.
    Property FakeRequest : TRequest Read FFakeRequest Write FFakeRequest;
    Property FakeResponse : TResponse Read FFakeResponse Write FFakeResponse;
  end;

  { TTestWebModule }

  TTestWebModule = Class(TTestCase)
  private
    FRequest: TFakeRequest;
    FResponse: TFakeResponse;
    FSession: TCustomSession;
    FUseFakeSession: Boolean;
    procedure SetSession(AValue: TCustomSession);
  Protected
    Procedure Setup; override;
    Procedure TearDown; override;
    function GetFakeSessionFactoryClass: TSessionFactoryClass; virtual;
    Procedure TestWebModule(AModuleClass : TCustomHTTPModuleClass; Stream : Boolean);
    Procedure AssertStatus(Const Msg : String; AStatus : Integer; Const AStatusText: String);
    Property Request : TFakeRequest Read FRequest;
    Property Response : TFakeResponse Read FResponse;
    Property Session : TCustomSession Read FSession Write SetSession;
    Property UseFakeSession : Boolean Read FUseFakeSession Write FUseFakeSession;
  end;

implementation

uses base64;

{ TFakeWebHandler }

function TFakeWebHandler.WaitForRequest(out ARequest: TRequest; out AResponse: TResponse): boolean;
begin
  Result:=Assigned(FFakeRequest);
  if Result then
    begin
    ARequest:=FFakeRequest;
    AResponse:=FFakeResponse;
    Terminate;
    end;
end;

procedure TFakeWebHandler.EndRequest(ARequest: TRequest; AResponse: TResponse);
begin
  if ARequest=FFakeRequest then
    begin
    FFakeRequest:=Nil;
    FFakeResponse:=Nil;
    end;
end;

{ TFakeRequest }

procedure TFakeRequest.InitRequest;
begin
  if (Method='') then
    Method:='GET';
  InitRequestVars;
end;

procedure TFakeRequest.SetAuthentication(const AUserName, APassword: String);
begin
  Authorization:='Basic ' + EncodeStringBase64(AUserName + ':' + APassword);
end;

{ TFakeSessionFactory }

function TFakeSessionFactory.DoCreateSession(ARequest: TRequest
  ): TCustomSession;
begin
  Result:=FSession;
end;

procedure TFakeSessionFactory.DoDoneSession(var ASession: TCustomSession);
begin
  If (ASession<>FSession) then
    FreeAndNil(ASession);
end;

procedure TFakeSessionFactory.DoCleanupSessions;
begin
  // Do nothing
end;

{ TFakeSession }

Procedure TFakeSession.CheckValues;

begin
  If not Assigned(FValues) then
    FValues:=TStringList.Create;
end;

function TFakeSession.GetValues: TStrings;
begin
  CheckValues;
  Result:=FValues;
end;

destructor TFakeSession.Destroy;
begin
  FreeAndNil(FValues);
  inherited Destroy;
end;

function TFakeSession.GetSessionVariable(VarName: String): String;
begin
  If Assigned(FValues) then
    Result:=FValues.Values[VarName]
  else
    Result:='';
end;

procedure TFakeSession.SetSessionVariable(VarName: String; const AValue: String);
begin
  CheckValues;
  FValues.Values[VarName]:=AValue;
end;

{ TTestWebModule }

procedure TTestWebModule.SetSession(AValue: TCustomSession);
begin
  if FSession=AValue then Exit;
  FreeAndNil(FSession);
  FSession:=AValue;
end;

procedure TTestWebModule.Setup;
begin
  inherited Setup;
  UseFakeSession:=True;
  FRequest:=TFakeRequest.Create;
  FResponse:=TFakeResponse.Create(FRequest);
  FSession:=TFakeSession.Create(Nil);
end;

procedure TTestWebModule.TearDown;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FSession);
  inherited TearDown;
end;

Function TTestWebModule.GetFakeSessionFactoryClass : TSessionFactoryClass;

begin
  Result:=TFakeSessionFactory;
end;


procedure TTestWebModule.TestWebModule(AModuleClass: TCustomHTTPModuleClass; Stream : Boolean);

Var
  M : TCustomHTTPModule;
  F : TSessionFactoryClass;

begin
  F:=SessionFactoryClass;
  If UseFakeSession then
    begin
    SessionFactoryClass:=GetFakeSessionFactoryClass;
    if SessionFactoryClass=TFakeSessionFactory then
      TFakeSessionFactory.FSession:=Self.Session;
    end;
  try
    Request.InitRequest;

    if Stream then
      M:=AModuleClass.Create(Nil)
    else
      M:=AModuleClass.CreateNew(Nil,0);
    try
      M.DoAfterInitModule(Request);
      M.HandleRequest(Request,Response);
    finally
      FreeAndNil(M);
    end;
  finally
    SessionFactoryClass:=F;
  end;
end;

procedure TTestWebModule.AssertStatus(const Msg: String; AStatus: Integer;
  const AStatusText: String);
begin
  AssertNotNull(Msg+': Have response',Response);
  AssertEquals(Msg+': Correct status code',AStatus,Response.Code);
  AssertEquals(Msg+': Correct status text',AStatusText,Response.CodeText);
end;

{ TFakeResponse }

function TFakeResponse.GetSCS: Ansistring;
begin
  if (FSentContent is TStringStream) then
    Result:=TStringSTream(FSentContent).DataString
  else
    Result:='';
end;

function TFakeResponse.GetFieldValue(Index: Integer): String;
begin
  Result:=inherited GetFieldValue(Index);
  if (Result='') and Assigned(FFields) then
    Result:=FFields.Values[IntToStr(Index)];
end;

procedure TFakeResponse.SetFieldValue(Index: Integer; Value: String);
begin
  inherited SetFieldValue(Index, Value);
  If (Value<>'') and (GetFieldValue(Index)='') then
    begin
    if (FFields=Nil) then
      FFields:=TStringList.Create;
    FFields.Add(IntToStr(Index)+'='+Value);
    end;
end;

destructor TFakeResponse.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FSentContent);
  FreeAndNil(FSentHeaders);
  inherited Destroy;
end;

procedure TFakeResponse.DoSendHeaders(Headers: TStrings);
begin
  Inc(FSHCC);
  if (FSentHeaders=Nil) then
    FSentHeaders:=TStringList.Create;
  FSentHeaders.Assign(Headers)
end;

procedure TFakeResponse.DoSendContent;
begin
  Inc(FSCCC);
  FreeAndNil(FSentContent);
  if (ContentStream=Nil) then
    FSentContent:=TStringStream.Create(Content)
  else
    begin
    FSentContent:=TStringStream.Create('');
    FSentContent.CopyFrom(ContentStream,0);
    end;
end;

end.

