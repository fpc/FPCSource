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
{$IFNDEF FPC_DOTTEDUNITS}
unit webjsonrpc;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{ $define debugjsonrpc}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpJson.Data, FpWeb.JsonRpc.Base, FpWeb.Http.Defs, FpWeb.Http.Base, FpJson.Scanner, FpJson.Parser;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpjson, fpjsonrpc, httpdefs, fphttp, jsonscanner, jsonparser;
{$ENDIF FPC_DOTTEDUNITS}

Type
{ ---------------------------------------------------------------------
  HTTP handling and content producing methods
  ---------------------------------------------------------------------}

  { TCustomJsonRpcContentProducer }

  TCustomJsonRpcContentProducer = Class(THTTPContentProducer)
  Protected
    Function GetIDProperty : String; virtual;
    Procedure DoGetContent(ARequest : TRequest; Content : TStream; Var Handled : Boolean); override;
    Function GetDispatcher : TCustomJsonRpcDispatcher; virtual; abstract;
  end;

  { TJsonRpcContentProducer }

  TJsonRpcContentProducer = Class(TCustomJsonRpcContentProducer)
  private
    FDispatcher: TCustomJsonRpcDispatcher;
    procedure SetDispatcher(const AValue: TCustomJsonRpcDispatcher);
  Protected
    Function GetDispatcher : TCustomJsonRpcDispatcher; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
  Published
    Property Dispatcher :  TCustomJsonRpcDispatcher Read FDispatcher Write SetDispatcher;
  end;


  { TJsonRpcSessionContext }

  TJsonRpcSessionContext = Class(TJsonRpcCallContext)
  private
    FSession: TCustomSession;
  Public
    Constructor CreateSession(ASession : TCustomSession);
    Property Session : TCustomSession Read FSession;
  end;

  { TSessionJsonRpcDispatcher }

  TSessionJsonRpcDispatcher = Class(TCustomJsonRpcDispatcher)
  Protected
    Function FindHandler(Const AClassName,AMethodName : TJSONStringType;AContext : TJsonRpcCallContext; Out FreeObject : TComponent) : TCustomJsonRpcHandler; override;
  Published
    Property OnStartBatch;
    Property OnDispatchRequest;
    Property OnFindHandler;
    Property OnEndBatch;
    Property Options;
  end;

  { TJsonRpcDispatchModule }

  TJsonRpcDispatchModule = Class(TSessionHTTPModule)
  protected
    Function CreateContext : TJsonRpcSessionContext;
    Function DispatchRequest(Const ARequest : TRequest; ADispatcher : TCustomJsonRpcDispatcher) : TJSONData;
  end;

  { TCustomJsonRpcModule }
  TAPIRequestSource = (asURL,  // Next part of URL: RPC/API
                       asQuery // Next part of URL: RPC?API=1
                      );
Const
  DefaultAPIRequestSources = [asURL, asQuery];

type
  TAPIRequestSources = Set of TAPIRequestSource;

  TCustomJsonRpcModule = Class(TJsonRpcDispatchModule)
  private
    FAPICreateOptions: TCreateAPIOptions;
    FAPIRequestName: String;
    FAPIRequestSources: TAPIRequestSources;
    FDispatcher: TCustomJsonRpcDispatcher;
    FOptions: TJsonRpcDispatchOptions;
    FRequest: TRequest;
    FResponse: TResponse;
    FResponseContentType: String;
    procedure SetDispatcher(const AValue: TCustomJsonRpcDispatcher);
  Protected
    function GetAPI(aDisp: TCustomJsonRpcDispatcher; ARequest: TRequest): TJSONStringType; virtual;
    Function GetResponseContentType : String;
    Function CreateDispatcher : TCustomJsonRpcDispatcher; virtual;
    Function IsAPIRequest(ARequest : TRequest) : Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property Dispatcher :  TCustomJsonRpcDispatcher Read FDispatcher Write SetDispatcher;
    // Options to use when creating a custom dispatcher
    Property DispatchOptions : TJsonRpcDispatchOptions Read FOptions Write FOptions default DefaultDispatchOptions;
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

  { TJsonRpcDataModule }

  { TJsonRpcModule }

  TJsonRpcModule = Class(TCustomJsonRpcModule)
  Published
    Property Dispatcher;
    // Only if Dispatcher is not set
    Property DispatchOptions;
    Property ResponseContentType;
    Property CORS;
    Property APIRequestSources;
    Property APIRequestName;
    Property Session;
    Property OnNewSession;
    Property OnSessionExpired;
    Property Kind;
    Property BaseURL;
    Property AfterInitModule;
  end;

implementation


{$IFDEF FPC_DOTTEDUNITS}
uses {$ifdef debugjsonrpc}System.Dbugintf,{$endif} FpWeb.JsonRpc.Strings;
{$ELSE FPC_DOTTEDUNITS}
uses {$ifdef debugjsonrpc}dbugintf,{$endif} fprpcstrings;
{$ENDIF FPC_DOTTEDUNITS}


Const
  SApplicationJSON = 'application/json';

{ TCustomJsonRpcContentProducer }

function TCustomJsonRpcContentProducer.GetIDProperty: String;
begin
  Result:='id';
end;


procedure TCustomJsonRpcContentProducer.DoGetContent(ARequest: TRequest;
  Content: TStream; var Handled: Boolean);

Var
  Disp : TCustomJsonRpcDispatcher;
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
          Res:=CreateErrorForRequest(Req,CreateJSON2ErrorResponse(SErrNoDispatcher,EJsonRpcInternalError,Nil,GetIDProperty));
      except
        On E : Exception Do
          begin
          Res:=CreateJSON2ErrorResponse(E.Message,EJsonRpcParseError,Nil,GetIDProperty);
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

{ TJsonRpcContentProducer }

procedure TJsonRpcContentProducer.SetDispatcher(
  const AValue: TCustomJsonRpcDispatcher);
begin
  if FDispatcher=AValue then exit;
  If Assigned(FDispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  If Assigned(FDispatcher) then
    FDispatcher.FreeNotification(Self);
end;

function TJsonRpcContentProducer.GetDispatcher: TCustomJsonRpcDispatcher;
begin
  Result:=FDispatcher;
end;

procedure TJsonRpcContentProducer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FDispatcher) then
    FDispatcher:=Nil;
end;

{ TCustomJsonRpcModule }

procedure TCustomJsonRpcModule.SetDispatcher(
  const AValue: TCustomJsonRpcDispatcher);
begin
  if FDispatcher=AValue then exit;
  If Assigned(FDispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  If Assigned(FDispatcher) then
    FDispatcher.FreeNotification(Self);
end;

function TCustomJsonRpcModule.GetResponseContentType: String;
begin
  Result:=FResponseContentType;
  if Result='' then
    Result:=SApplicationJSON;
end;

function TCustomJsonRpcModule.CreateDispatcher: TCustomJsonRpcDispatcher;

Var
  S : TSessionJsonRpcDispatcher;

begin
  S:=TSessionJsonRpcDispatcher.Create(Self);
  S.Options:=DispatchOptions;
  S.APICreator.DefaultOptions:=APICreateOptions;
  S.APICreator.URL:=Self.BaseURL;
  Result:=S;
end;

function TCustomJsonRpcModule.IsAPIRequest(ARequest: TRequest): Boolean;
begin
  Result:=False;
  if APIRequestName<>'' then
    begin
    if (asURL in APIRequestSources) then
      Result:=SameText(aRequest.GetNextPathInfo,APIRequestName);
    if (asQuery in APIRequestSources) then
      Result:=Result or (aRequest.QueryFields.Values[APIRequestName]<>'');
    end;
end;


procedure TCustomJsonRpcModule.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) and (AComponent=FDispatcher) then
    FDispatcher:=Nil;
end;

constructor TCustomJsonRpcModule.CreateNew(AOwner: TComponent;
  CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  FOptions := DefaultDispatchOptions+[jdoSearchRegistry];
  APIRequestSources := DefaultAPIRequestSources;
  APICreateOptions:=[caoFullParams];
  APIRequestName:='API';
end;

Function TCustomJsonRpcModule.GetAPI(aDisp : TCustomJsonRpcDispatcher; ARequest: TRequest) : TJSONStringType;

  Function GetV(Name1,Name2 : String) : string;

  begin
    Result:=aRequest.QueryFields.Values[Name1];
    if (Result='') and (Name2<>'') then
      Result:=aRequest.QueryFields.Values[Name2];
  end;


var
  B,asPascal : Boolean;
  APIOptions : TCreateAPIOptions;
  aUnit,Fmt : string;

begin
  B:=False;
  APIOptions:=[];
  Fmt:=GetV('format','fmt');
  asPascal:=(Fmt='pas') or (fmt='pascal');
  if asPascal then
    begin
    APIOptions:=[caoFullParams];
    aUnit:=GetV('unit','unitname');
    B:=true;
    end
  else
    begin
    Fmt:=GetV('extended','full');
    if (Fmt<>'') then
      begin
      Include(APIOptions,caoFullParams);
      B:=true;
      end;
    Fmt:=GetV('formatted','humanreadable');
    if (Fmt<>'') then
      begin
      Include(APIOptions,caoFormatted);
      B:=true;
      end;
    end;
  if Not B then
    APIOptions:=aDisp.APICreator.DefaultOptions;
  if asPascal then
    Result:=aDisp.APIAsPascal(APIOptions,aUnit)
  else
    Result:=aDisp.APIAsString(APIOptions);
end;

procedure TCustomJsonRpcModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  Disp : TCustomJsonRpcDispatcher;
  res : TJSONData;
  R : TJSONStringType;

begin
  // We must set SessionRequest
  Inherited HandleRequest(aRequest,aResponse);
  if CORS.HandleRequest(aRequest,aResponse,[hcDetect,hcSend]) then
    exit;
  CheckSession(ARequest);
  If (Dispatcher=Nil) then
    Dispatcher:=CreateDispatcher;
  Disp:=Dispatcher;
  R:='';
  if IsAPIRequest(aRequest) then
    begin
    if (jdoAllowAPI in TJsonRpcDispatcher(Disp).Options) then
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
  if not (AResponse.HeadersSent and AResponse.ContentSent) then
    AResponse.SendResponse;
end;

{ TJsonRpcSessionContext }

constructor TJsonRpcSessionContext.CreateSession(ASession: TCustomSession);
begin
  FSession:=ASession;
end;

{ TJsonRpcDispatchModule }

function TJsonRpcDispatchModule.CreateContext: TJsonRpcSessionContext;
begin
  If CreateSession then
    Result:=TJsonRpcSessionContext.CreateSession(Session)
  else
    Result:=TJsonRpcSessionContext.CreateSession(Nil);
end;

Function TJsonRpcDispatchModule.DispatchRequest(const ARequest: TRequest;
  ADispatcher: TCustomJsonRpcDispatcher): TJSONData;
var
  P : TJSONParser;
  Req : TJSONData;
  C : TJsonRpcSessionContext;


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
          Result:=CreateJSON2ErrorResponse(E.Message,EJsonRpcParseError,Nil,ADispatcher.TransactionProperty);
      end;
    finally
      Req.Free;
    end;
  finally
    P.Free;
  end;
end;

{ TSessionJsonRpcDispatcher }

function TSessionJsonRpcDispatcher.FindHandler(const AClassName,
  AMethodName: TJSONStringType; AContext: TJsonRpcCallContext; out
  FreeObject: TComponent): TCustomJsonRpcHandler;
begin
  Result:=Inherited FindHandler(AClassName,AMethodName,AContext,FreeObject);
  If (AContext is TJsonRpcSessionContext) and (FreeObject is TCustomJsonRpcModule) then
    TCustomJsonRpcModule(FreeObject).Session:=TJsonRpcSessionContext(AContext).Session;
end;

end.

