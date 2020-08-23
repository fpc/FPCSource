{
    This file is part of the Free Component Library

    JSON-RPC functionality - http dependant part
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit webjsonrpc;

{$mode objfpc}{$H+}
{ $define debugjsonrpc}

interface

uses
  Classes, SysUtils, fpjson, fpjsonrpc, httpdefs, fphttp, jsonscanner, jsonparser;

Type
{ ---------------------------------------------------------------------
  HTTP handling and content producing methods
  ---------------------------------------------------------------------}

  { TCustomJSONRPCContentProducer }

  TCustomJSONRPCContentProducer = Class(THTTPContentProducer)
  Protected
    Function GetIDProperty : String; virtual;
    Procedure DoGetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean); override;
    Function GetDispatcher : TCustomJSONRPCDispatcher; virtual; abstract;
  end;

  { TJSONRPCContentProducer }

  TJSONRPCContentProducer = Class(TCustomJSONRPCContentProducer)
  private
    FDispatcher: TCustomJSONRPCDispatcher;
    procedure SetDispatcher(const AValue: TCustomJSONRPCDispatcher);
  Protected
    Function GetDispatcher : TCustomJSONRPCDispatcher; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  Published
    Property Dispatcher :  TCustomJSONRPCDispatcher Read FDispatcher Write SetDispatcher;
  end;


  { TJSONRPCSessionContext }

  TJSONRPCSessionContext = Class(TJSONRPCCallContext)
  private
    FSession: TCustomSession;
  Public
    Constructor CreateSession(ASession : TCustomSession);
    Property Session : TCustomSession Read FSession;
  end;

  { TSessionJSONRPCDispatcher }

  TSessionJSONRPCDispatcher = Class(TCustomJSONRPCDispatcher)
  Protected
    Function FindHandler(Const AClassName,AMethodName : TJSONStringType;AContext : TJSONRPCCallContext; Out FreeObject : TComponent) : TCustomJSONRPCHandler; override;
  Published
    Property OnStartBatch;
    Property OnDispatchRequest;
    Property OnFindHandler;
    Property OnEndBatch;
    Property Options;
  end;

  { TJSONRPCDispatchModule }

  TJSONRPCDispatchModule = Class(TSessionHTTPModule)
  protected
    Function CreateContext : TJSONRPCSessionContext;
    Function DispatchRequest(Const ARequest : TRequest; ADispatcher : TCustomJSONRPCDispatcher) : TJSONData;
  end;

  { TCustomJSONRPCModule }
  TAPIRequestSource = (asURL,  // Next part of URL: RPC/API
                       asQuery // Next part of URL: RPC?API=1
                      );
Const
  DefaultAPIRequestSources = [asURL, asQuery];

type
  TAPIRequestSources = Set of TAPIRequestSource;

  TCustomJSONRPCModule = Class(TJSONRPCDispatchModule)
  private
    FAPICreateOptions: TCreateAPIOptions;
    FAPIRequestName: String;
    FAPIRequestSources: TAPIRequestSources;
    FDispatcher: TCustomJSONRPCDispatcher;
    FOptions: TJSONRPCDispatchOptions;
    FRequest: TRequest;
    FResponse: TResponse;
    FResponseContentType: String;
    procedure SetDispatcher(const AValue: TCustomJSONRPCDispatcher);
  Protected
    function GetAPI(aDisp: TCustomJSONRPCDispatcher; ARequest: TRequest): TJSONStringType; virtual;
    Function GetResponseContentType : String;
    Function CreateDispatcher : TCustomJSONRPCDispatcher; virtual;
    Function IsAPIRequest(ARequest : TRequest) : Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property Dispatcher :  TCustomJSONRPCDispatcher Read FDispatcher Write SetDispatcher;
    // Options to use when creating a custom dispatcher
    Property DispatchOptions : TJSONRPCDispatchOptions Read FOptions Write FOptions default DefaultDispatchOptions;
    // Where to look for API request
    property APIRequestSources : TAPIRequestSources Read FAPIRequestSources Write FAPIRequestSources default DefaultAPIRequestSources;
    // URL part or variable name to check for API request
    property APIRequestName : String Read FAPIRequestName Write FAPIRequestName;
    // API create options when creating a custom dispatcher
    Property APICreateOptions : TCreateAPIOptions Read FAPICreateOptions Write FAPICreateOptions;
  Public
    Constructor CreateNew(AOwner : TComponent; CreateMode : Integer); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
    // Access to request
    Property Request: TRequest Read FRequest;
    // Access to response
    Property Response: TResponse Read FResponse;
    // Response Content-Type. If left empty, application/json is used.
    Property ResponseContentType : String Read FResponseContentType Write FResponseContentType;
    // Must we handle CORS ?
    Property CORS;
  end;

  { TJSONRPCDataModule }

  { TJSONRPCModule }

  TJSONRPCModule = Class(TCustomJSONRPCModule)
  Published
    Property Dispatcher;
    // Only if Dispatcher is not set
    Property DispatchOptions;
    Property ResponseContentType;
    Property CORS;
    Property APIRequestSources;
    Property APIRequestName;
  end;

implementation

{$ifdef debugjsonrpc}
uses dbugintf;
{$endif}

Const
  SApplicationJSON = 'application/json';

{ TCustomJSONRPCContentProducer }

function TCustomJSONRPCContentProducer.GetIDProperty: String;
begin
  Result:='id';
end;


procedure TCustomJSONRPCContentProducer.DoGetContent(ARequest: TRequest;
  Content: TStream; var Handled: Boolean);

Var
  Disp : TCustomJSONRPCDispatcher;
  P : TJSONParser;
  Req,res : TJSONData;
  R : TJSONStringType;

begin
  Disp:=Self.GetDispatcher;
  P:= TJSONParser.Create(ARequest.Content,[joUTF8]);
  try
    Res:=Nil;
    Req:=Nil;
    try
      try
        Req:=P.Parse;
        If (Disp<>Nil) then
          Res:=Disp.Execute(Req,Nil)
        else // No dispatcher, create error(s)
          Res:=CreateErrorForRequest(Req,CreateJSON2ErrorResponse(SErrNoDispatcher,EJSONRPCInternalError,Nil,GetIDProperty));
      except
        On E : Exception Do
          begin
          Res:=CreateJSON2ErrorResponse(E.Message,EJSONRPCParseError,Nil,GetIDProperty);
          end;
      end;
      try
        If Assigned(Res) then
          begin
          R:=Res.AsJSON;
          Content.WriteBuffer(R[1],Length(R));
          end;
        Handled:=True;
      finally
        FreeAndNil(Res);
      end;
    finally
      Req.Free;
    end;
  finally
    P.Free;
  end;
end;

{ TJSONRPCContentProducer }

procedure TJSONRPCContentProducer.SetDispatcher(
  const AValue: TCustomJSONRPCDispatcher);
begin
  if FDispatcher=AValue then exit;
  If Assigned(FDispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  If Assigned(FDispatcher) then
    FDispatcher.FreeNotification(Self);
end;

function TJSONRPCContentProducer.GetDispatcher: TCustomJSONRPCDispatcher;
begin
  Result:=FDispatcher;
end;

procedure TJSONRPCContentProducer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FDispatcher) then
    FDispatcher:=Nil;
end;

{ TCustomJSONRPCModule }

procedure TCustomJSONRPCModule.SetDispatcher(
  const AValue: TCustomJSONRPCDispatcher);
begin
  if FDispatcher=AValue then exit;
  If Assigned(FDispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  If Assigned(FDispatcher) then
    FDispatcher.FreeNotification(Self);
end;

function TCustomJSONRPCModule.GetResponseContentType: String;
begin
  Result:=FResponseContentType;
  if Result='' then
    Result:=SApplicationJSON;
end;

function TCustomJSONRPCModule.CreateDispatcher: TCustomJSONRPCDispatcher;

Var
  S : TSessionJSONRPCDispatcher;

begin
  S:=TSessionJSONRPCDispatcher.Create(Self);
  S.Options:=DispatchOptions;
  S.APICreator.DefaultOptions:=APICreateOptions;
  S.APICreator.URL:=Self.BaseURL;
  Result:=S;
end;

function TCustomJSONRPCModule.IsAPIRequest(ARequest: TRequest): Boolean;
begin
  Result:=False;
  if (asURL in APIRequestSources) then
    Result:=SameText(aRequest.GetNextPathInfo,APIRequestName);
  if (asQuery in APIRequestSources) then
    Result:=Result or (aRequest.QueryFields.Values[APIRequestName]<>'');
end;


procedure TCustomJSONRPCModule.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FDispatcher) then
    FDispatcher:=Nil;
end;

constructor TCustomJSONRPCModule.CreateNew(AOwner: TComponent;
  CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FOptions := DefaultDispatchOptions+[jdoSearchRegistry];
  APIRequestSources := DefaultAPIRequestSources;
  APICreateOptions:=[caoFullParams];
end;

Function TCustomJSONRPCModule.GetAPI(aDisp : TCustomJSONRPCDispatcher; ARequest: TRequest) : TJSONStringType;

var
  B : Boolean;
  APIOptions : TCreateAPIOptions;

begin
  B:=False;
  APIOptions:=[];
  if (aRequest.QueryFields.Values['extended']<>'') or (aRequest.QueryFields.Values['full']<>'') then
    begin
    Include(APIOptions,caoFullParams);
    B:=true;
    end;
  if (aRequest.QueryFields.Values['formatted']<>'') or (aRequest.QueryFields.Values['humanreadable']<>'') then
    begin
    Include(APIOptions,caoFormatted);
    B:=true;
    end;
  if Not B then
    APIOptions:=aDisp.APICreator.DefaultOptions;
  Result:=aDisp.APIAsString(APIOptions);
end;

procedure TCustomJSONRPCModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  Disp : TCustomJSONRPCDispatcher;
  res : TJSONData;
  R : TJSONStringType;

begin
  if CORS.HandleRequest(aRequest,aResponse,[hcDetect,hcSend]) then
    exit;
  If (Dispatcher=Nil) then
    Dispatcher:=CreateDispatcher;
  Disp:=Dispatcher;
  R:='';
  if IsAPIRequest(aRequest) then
    begin
    if (jdoAllowAPI in TJSONRPCDispatcher(Disp).Options) then
      R:=GetAPI(Disp,aRequest)
    else
      begin
      Response.Code:=403;
      Response.CodeText:='FORBIDDEN';
      end;
    end
  else
    begin
    Res:=DispatchRequest(ARequest,Disp);
    try
      if Assigned(Res) then
        R:=Res.AsJSON;
    finally
      Res.Free;
    end;
    end;
  AResponse.ContentType:=GetResponseContentType;
  if (R<>'') then
    begin
    AResponse.FreeContentStream:=True;
    AResponse.ContentStream:=TMemoryStream.Create;
    AResponse.ContentStream.WriteBuffer(R[1],Length(R));
    AResponse.ContentLength:=AResponse.ContentStream.Size;
    R:=''; // Free up mem
    end;
  AResponse.SendResponse;
end;

{ TJSONRPCSessionContext }

constructor TJSONRPCSessionContext.CreateSession(ASession: TCustomSession);
begin
  FSession:=ASession;
end;

{ TJSONRPCDispatchModule }

function TJSONRPCDispatchModule.CreateContext: TJSONRPCSessionContext;
begin
  If CreateSession then
    Result:=TJSONRPCSessionContext.CreateSession(Session)
  else
    Result:=TJSONRPCSessionContext.CreateSession(Nil);
end;

Function TJSONRPCDispatchModule.DispatchRequest(const ARequest: TRequest;
  ADispatcher: TCustomJSONRPCDispatcher): TJSONData;
var
  P : TJSONParser;
  Req : TJSONData;
  C : TJSONRPCSessionContext;


begin
  P:= TJSONParser.Create(ARequest.Content,[joUTF8]);
  try
    Result:=Nil;
    Req:=Nil;
    try
      try
        Req:=P.Parse;
        C:=CreateContext;
        try
         {$ifdef debugjsonrpc}SendDebugFmt('Dispatching request : "%s"',[Req.AsJSON]);{$endif}
          Result:=ADispatcher.Execute(Req,C);
        finally
          C.Free;
        end;
      except
        On E : Exception Do
          Result:=CreateJSON2ErrorResponse(E.Message,EJSONRPCParseError,Nil,ADispatcher.TransactionProperty);
      end;
    finally
      Req.Free;
    end;
  finally
    P.Free;
  end;
end;

{ TSessionJSONRPCDispatcher }

function TSessionJSONRPCDispatcher.FindHandler(const AClassName,
  AMethodName: TJSONStringType; AContext: TJSONRPCCallContext; out
  FreeObject: TComponent): TCustomJSONRPCHandler;
begin
  Result:=Inherited FindHandler(AClassName,AMethodName,AContext,FreeObject);
  If (AContext is TJSONRPCSessionContext) and (FreeObject is TCustomJSONRPCModule) then
    TCustomJSONRPCModule(FreeObject).Session:=TJSONRPCSessionContext(AContext).Session;
end;

end.

