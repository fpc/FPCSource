{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    Open API service - client proxy parent

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpopenapiclient;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}
{$H+}
{$IFNDEF VER3_2}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$ENDIF}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpWeb.Client;
  {$ELSE}
  Classes, SysUtils, fpwebclient;
  {$ENDIF}

Type
  EOpenAPIClient = class(Exception);
  TServiceRequestID = string;

  TServiceResponse = Record
    RequestID : TServiceRequestID;
    StatusCode : Integer;
    StatusText : String;
    Content : String;
  end;

  { TServiceResult }

  generic TServiceResult<T> = record
    RequestID : TServiceRequestID;
    ErrorCode : Integer;
    ErrorText : String;
    Value : T;
    constructor create(aServiceResponse : TServiceResponse);
    function Success : Boolean;
  end;

  TVoidServiceResult = specialize TServiceResult<Boolean>;

  {$IFNDEF VER3_2}
  TServiceResponseCallback = reference to procedure (aResult: TServiceResponse);
  TVoidServiceResultCallBack = reference to procedure (aResult: TVoidServiceResult);
  {$ELSE}
  TServiceResponseCallback = procedure (aResult: TServiceResponse) of object;
  TVoidServiceResultCallBack = procedure (aResult: TVoidServiceResult) of object;
  {$ENDIF}

  { TFPOpenAPIServiceClient }
  TAPIServicePrepareRequestEvent = procedure(aSender : TObject; aRequest : TWebClientRequest) of object;
  TAPIServiceProcessResponseEvent = procedure(aSender : TObject; aResponse : TWebClientResponse) of object;

  TFPOpenAPIServiceClient = Class(TComponent)
  private
    FBaseURL: String;
    FOnPrepareRequest: TAPIServicePrepareRequestEvent;
    FOnProcessResponse: TAPIServiceProcessResponseEvent;
    FWebClient: TAbstractWebClient;
    procedure SetBaseURL(AValue: String);
    procedure SetWebClient(AValue: TAbstractWebClient);
  protected
    procedure PrepareRequest(aRequest: TWebClientRequest); virtual;
    procedure ProcessResponse(aResponse: TWebClientResponse); virtual;
    procedure ProcessServiceException(aReq : TWebClientRequest; aError : Exception);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function BuildEndPointURL(const aEndPoint : string) : string; virtual;
    function ReplacePathParam (const aPath : String; const aParamName : string; const aParamValue : String) : String; virtual;
    function ConcatRestParam(const aQueryParam: string; const aParamName: string; const aParamValue: string): string; virtual;
    function ExecuteRequest(const aMethod,aURL,aBody : String; aRequestID : TServiceRequestID = '') : TServiceResponse; virtual;
    {$IFNDEF VER3_2}
    function ExecuteRequest(const aMethod,aURL: String; aBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; virtual;
    function ExecuteRequest(const aMethod,aURL,aBody : String; aCallback : TServiceResponseCallback; aRequestID : TServiceRequestID = '') : TServiceRequestID;virtual;
    {$ENDIF}
  Published
    Property WebClient : TAbstractWebClient Read FWebClient Write SetWebClient;
    Property BaseURL : String Read FBaseURL Write SetBaseURL;
    Property OnPrepareRequest : TAPIServicePrepareRequestEvent Read FOnPrepareRequest Write FOnPrepareRequest;
    Property OnProcessResponse : TAPIServiceProcessResponseEvent Read FOnProcessResponse Write FOnProcessResponse;
  end;

const
  cRestBooleans : Array[Boolean] of string = ('false','true');

function cRestFormatSettings: TFormatSettings;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.Http.Protocol;
{$ELSE}
uses httpprotocol;
{$ENDIF}

var
  _FormatSettings : TFormatSettings;

function cRestFormatSettings: TFormatSettings;

begin
  if _FormatSettings.DateSeparator=#0 then
    _FormatSettings:=DefaultFormatSettings;
  Result:=_FormatSettings;
end;

{ TServiceResult }

constructor TServiceResult.create(aServiceResponse: TServiceResponse);

begin
  Value:=Default(T);
  if (aServiceResponse.StatusCode div 100)<>2 then
    begin
    ErrorCode:=aServiceResponse.StatusCode;
    ErrorText:=aServiceResponse.StatusText;
    end
  else
    begin
    ErrorCode:=0;
    ErrorText:='';
    end;
end;

function TServiceResult.Success: Boolean;

begin
  Result:=ErrorCode=0;
end;

{ TFPOpenAPIClient }

procedure TFPOpenAPIServiceClient.SetBaseURL(AValue: String);

begin
  if FBaseURL=AValue then Exit;
  FBaseURL:=AValue;
end;


procedure TFPOpenAPIServiceClient.SetWebClient(AValue: TAbstractWebClient);

begin
  if FWebClient=AValue then Exit;
  if Assigned(FWebClient) then
    FWebClient.RemoveFreeNotification(Self);
  FWebClient:=AValue;
  if Assigned(FWebClient) then
    FWebClient.FreeNotification(Self);
end;


procedure TFPOpenAPIServiceClient.PrepareRequest(aRequest: TWebClientRequest);

begin
  aRequest.Headers.Values['Content-Type']:='application/json';
  aRequest.Headers.Values['Accept']:='application/json';
  if assigned(OnPrepareRequest) then
    OnPrepareRequest(Self,aRequest);
end;


procedure TFPOpenAPIServiceClient.ProcessResponse(aResponse: TWebClientResponse);

begin
  if Assigned(FOnProcessResponse) then
    FOnProcessResponse(Self,aResponse);
end;


procedure TFPOpenAPIServiceClient.ProcessServiceException(aReq: TWebClientRequest; aError: Exception);

begin
  // Do nothing
end;


procedure TFPOpenAPIServiceClient.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
    if aComponent=FWebClient then
      FWebClient:=Nil;
end;


function TFPOpenAPIServiceClient.BuildEndPointURL(const aEndPoint: string): string;

var
  lEndPoint : String;

begin
  Result:=BaseURL;
  if (Result<>'') and (Result[Length(Result)]<>'/') then
    Result:=Result+'/';
  lEndPoint:=aEndPoint;
  if (aEndPoint<>'') and (aEndPoint[1]='/') then
    lEndPoint:=Copy(lEndPoint,2);
  Result:=Result+lEndPoint;
end;


function TFPOpenAPIServiceClient.ReplacePathParam(const aPath: String; const aParamName: string; const aParamValue: String): String;

var
  lEncoded : String;

begin
  lEncoded:=HTTPEncode(aParamValue);
  Result:=StringReplace(aPath,'{'+aParamName+'}',lEncoded,[rfReplaceAll]);
end;


function TFPOpenAPIServiceClient.ConcatRestParam(const aQueryParam: string; const aParamName: string; const aParamValue: string): string;

begin
  Result := aQueryParam;
  if (aParamValue = '') then
    exit;
  if Result='' then
    Result:=Result+'?'
  else
    Result:=Result+'&';
  Result:=Result+aParamName;
  Result:=Result+'='+HTTPEncode(aParamValue);
end;


{$IFNDEF VER3_2}
function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod,aURL: String; aBody : TStream; aRequestID : TServiceRequestID = '') : TServiceResponse; 

var
  lReq : TWebClientRequest;
  lResponse : TWebClientResponse;

begin
  Result:=Default(TServiceResponse);
  if Not Assigned(WebClient) then
    Raise EOpenAPIClient.Create('No webclient assigned');
  try
    lReq:=WebClient.CreateRequest(False,aRequestID);
    Result.RequestID:=lReq.RequestID;
    lReq.Content:=aBody;
    lReq.OwnsStream:=True;
    try
      PrepareRequest(lReq);
      lResponse:=WebClient.ExecuteRequest(aMethod,aURL,lReq);
      ProcessResponse(lResponse);
      Result.StatusCode:=lResponse.StatusCode;
      Result.StatusText:=lResponse.StatusText;
      Result.Content:=lResponse.GetContentAsString;
    except
      on E : Exception do
        begin
        ProcessServiceException(lReq,E);
        Result.StatusCode:=999;
        Result.StatusText:=Format('%s: %s',[E.ClassName,E.Message]);
        end;
    end;
  finally
    lReq.Free;
    lResponse.Free;
  end;
end;

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod, aURL, aBody: String; aRequestID: TServiceRequestID): TServiceResponse;

var
  lBody : TStringStream;
begin
  lBody:=TStringStream.Create(aBody);
  Result:=ExecuteRequest(aMethod,aURL,lBody,aRequestID);
end;

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod,aURL,aBody : String; aCallback : TServiceResponseCallback; aRequestID : TServiceRequestID = '') : TServiceRequestID;

var
  lReq : TWebClientRequest;
  lResponse : TWebClientResponse;
  lExResponse : TServiceResponse;

begin
  Result:=aRequestID;
  if Not Assigned(WebClient) then
    Raise EOpenAPIClient.Create('No webclient assigned');
  try
    lReq:=WebClient.CreateRequest(True,aRequestID);
    Result:=lReq.RequestID;
    if aBody<>'' then
      lReq.SetContentFromString(aBody);
    try
      PrepareRequest(lReq);
      WebClient.ExecuteRequest(aMethod,aURL,lReq,procedure(aResponse : TWebClientResponseResult)
         var
           aResult : TServiceResponse;
         begin
           if not aResponse.Success then
             begin
             ProcessServiceException(lReq,aResponse.Error);
             With aResponse.Error do
               begin
               aResult.StatusText:=Format('%s : %s',[ClassName,Message]);
               aResult.StatusCode:=999;
               aResult.Content:='';
               end
             end
           else
             begin
             ProcessResponse(aResponse.Response);
             aResult.StatusCode:=aResponse.Response.StatusCode;
             aResult.StatusText:=aResponse.Response.StatusText;
             aResult.Content:=aResponse.Response.GetContentAsString;
             end;
           aCallBack(aResult);
         end);
      lReq:=Nil;
      lResponse:=Nil;
    except
      on E : Exception do
        begin
        ProcessServiceException(lReq,E);
        lExResponse.RequestID:=lReq.RequestID;
        lExResponse.StatusCode:=999;
        lExResponse.StatusText:=Format('%s: %s',[E.ClassName,E.Message]);
        aCallBack(lExResponse);
        end;
    end;
  finally
    lReq.Free;
    lResponse.Free;
  end;
end;

{$ELSE}

function TFPOpenAPIServiceClient.ExecuteRequest(const aMethod, aURL, aBody: String; aRequestID: TServiceRequestID): TServiceResponse;

var
  lReq : TWebClientRequest;
  lResponse : TWebClientResponse;

begin
  Result:=Default(TServiceResponse);
  if Not Assigned(WebClient) then
    Raise EOpenAPIClient.Create('No webclient assigned');
  try
    Result.RequestID:=aRequestID;
    lReq:=WebClient.CreateRequest;
    if aBody<>'' then
      lReq.SetContentFromString(aBody);
    try
      PrepareRequest(lReq);
      lResponse:=WebClient.ExecuteRequest(aMethod,aURL,lReq);
      ProcessResponse(lResponse);
      Result.StatusCode:=lResponse.StatusCode;
      Result.StatusText:=lResponse.StatusText;
      Result.Content:=lResponse.GetContentAsString;
    except
      on E : Exception do
        begin
        ProcessServiceException(lReq,E);
        Result.StatusCode:=999;
        Result.StatusText:=Format('%s: %s',[E.ClassName,E.Message]);
        end;
    end;
  finally
    lReq.Free;
    lResponse.Free;
  end;
end;
{$ENDIF}

end.

