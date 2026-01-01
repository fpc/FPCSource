{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM (Firebase Cloud Messaging) - Component to send a message through FCM.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpfcmclient;
{$ENDIF}

{$mode ObjFPC}{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses    
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, Jwt.Types, Fcm.Types, FpWeb.Client, System.Types;
{$ELSE}
  Classes, SysUtils, fpjson, fpjwt, fpfcmtypes, fpwebclient, types;
{$ENDIF}

type
  TFCMErrorStage = (esConfig, esAccessToken, esPost);

  { TFCMErrorStageHelper }

  TFCMErrorStageHelper = type helper for TFCMErrorStage
    function ToString: string;
  end;

  { TFCMError }

  TFCMError = record
    Content: string;
    Message: string;
    Stage: TFCMErrorStage;
    constructor Create(const aStage: TFCMErrorStage; const aContent, aMessage: string);
  end;



  { TFCMResponse }

  TFCMResponse = record
    Response: string;
    Headers : TStringDynArray;
    constructor Create(const aResponse: string; aHeaders : TStringDynArray);
  end;

  TFCMResponseEvent = procedure(Sender: TObject; const aResponse: TFCMResponse) of object;
  TFCMBearerTokenEvent = procedure(Sender: TObject; const aToken: TBearerToken) of object;
  TFCMErrorEvent = procedure(Sender: TObject; const aError: TFCMError) of object;


  { TFCMClient }

  TFCMClient = class(TComponent)
  private
    FBearerToken: TBearerToken;
    FLogFile: String;
    FOnNewBearerToken: TFCMBearerTokenEvent;
    FServiceAccount: TServiceAccountData;
    FOnError: TFCMErrorEvent;
    FOnResponse: TFCMResponseEvent;
    FWebClient: TAbstractWebClient;
    function ExtractContentType(const aHeader: String): string;
    function GetWebClient: TAbstractWebClient;
    procedure SetBearerToken(AValue: TBearerToken);
    procedure SetWebClient(AValue: TAbstractWebClient);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //
    // General stuff
    // Get send endpoint
    function GetSendEndPoint(const aProjectID: string): UTF8string;
    // Create JWT token to get access token. Called by GetAccessToken
    function GenerateJWT: string;
    // Get an access token.
    function GetAccessToken: Boolean;
    //
    // HTTP Transport
    //
    // Override these if you don't want to use TAbstractWebClient.
    // Must set bearertoken.  Must raise exception on error.
    procedure FetchAccessToken(const aUrl, aJWT: string); virtual;
    // Must raise an exception on error
    procedure HandlePost(const aURL, aJSON: UTF8String); virtual;
    //
    // Default HTTP implementation using WebClient
    //
    // Create webclient if needed.
    function CreateWebClient: TAbstractWebClient; virtual;
    // Handle WebClient token response
    procedure HandleTokenResponse(aResponse: TWebClientResponse);
    // Handle WebClient send response
    procedure HandlePostResponse(aResponse: TWebClientResponse);
    //
    // Events & Errors
    //
    // Convert exception to error string.
    function ExceptionToString(E: Exception): string; virtual;
    // Handle error. If this returns true, the error was handled. If false, caller must raise an exception.
    function HandleError(const aError: TFCMError): Boolean; virtual;
    // Called when new bearer token is available.
    procedure HandleNewBearerToken; virtual;
    // Call event handler for bearer token.
    procedure HandleAccessToken; virtual;
    // Call response event handler
    function HandleResponse(const aResponse: TFCMResponse) : boolean; virtual;
    // Send the message
    function Post(const aJSON: UTF8String): Boolean;
  public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;

    // These calls will raise an exception, regardless of OnError being set.
    Procedure InitServiceAccount(const aFileName: string; aRoot: TJSONStringType);
    Procedure InitServiceAccount(const aJSON : TStream; aRoot: TJSONStringType);
    Procedure InitServiceAccount(const aJSON : TJSONStringType; aRoot: TJSONStringType);
    Procedure InitServiceAccount(const aJSON : TJSONObject);
    // Web client to use for requests. A default is created if none is set.
    Property WebClient : TAbstractWebClient Read GetWebClient Write SetWebClient;
    // Serialize and send.
    function Send(aMsg : TNotificationMessage; aRecipient : UTF8String) : Boolean;
    // Serialize and send to multiple recipients
    function Send(aMsg : TNotificationMessage; aRecipients : Array of UTF8String) : Boolean;
    // Current bearer token. You can set this if you stored it somewhere
    property BearerToken: TBearerToken read FBearerToken write SetBearerToken;
    // Service account data. This will be used to get a bearer token.
    property ServiceAccount: TServiceAccountData read FServiceAccount;
    // Set this if you wish to handle errors. If not set, an exception is raised on error.
    property OnError: TFCMErrorEvent read FOnError write FOnError;
    // Called when a new access token was received.
    property OnNewBearerToken : TFCMBearerTokenEvent read FOnNewBearerToken write FOnNewBearerToken;
    // Called with send response.
    property OnResponse: TFCMResponseEvent read FOnResponse write FOnResponse;
    // Set this if you want a log of the HTTP communications with google FCM servers
    Property LogFile: String Read FLogFile Write FLogFile;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils, FpWeb.Http.Protocol, Fcm.Strings, Jwt.Jwa.Rsa, System.Hash.Pem; 
{$ELSE}
uses dateutils, httpprotocol, fpfcmstrings, fpjwarsa, fppem;
{$ENDIF}


{ TFCMErrorStageHelper }

function TFCMErrorStageHelper.ToString: string;
begin
  WriteStr(Result,Self);
end;

{ TFCMError }

constructor TFCMError.Create(const aStage: TFCMErrorStage; const aContent, aMessage: string);
begin
  Stage:=aStage;
  Content:=aContent;
  Message:=aMessage;
end;

{ TSenderResponse }

constructor TFCMResponse.Create(const aResponse: string; aHeaders: TStringDynArray);
begin
  Response:=aResponse;
end;

{ TFCMClient }

procedure TFCMClient.SetWebClient(AValue: TAbstractWebClient);
begin
  if FWebClient=AValue then Exit;
  if Assigned(FWebCLient) and (csSubComponent in FWebCLient.ComponentStyle) then
    FreeAndNil(FWebClient);
  FWebClient:=AValue;
end;

function TFCMClient.ExceptionToString(E: Exception): string;
begin
  With E do
    Result:=Format(SErrorMessage,[ClassName,Message])
end;

function TFCMClient.HandleError(const aError: TFCMError) : Boolean;
begin
  Result:=Assigned(FOnError);
  if Result then
    FOnError(Self,aError);
end;

function TFCMClient.GetSendEndPoint(const aProjectID: string): UTF8string;

begin
  Result:=Format(SFCMSendURL,[aProjectID]);
end;

procedure TFCMClient.HandlePost(const aURL,aJSON: UTF8String);

var
  Request : TWebClientRequest;
  Response : TWebClientResponse;

begin
  Response:=Nil;
  Request:=WebClient.CreateRequest;
  try
    Request.Headers.Values[HeaderAccept]:=SContentTypeApplicationJSON;
    Request.Headers.Values[HeaderContentType]:=SContentTypeApplicationJSON;
    Request.Headers.Values[HeaderAuthorization]:='Bearer '+BearerToken.access_token;
    Request.SetContentFromString(AJSON);
    Response:=WebClient.ExecuteRequest('POST',aURL,Request);
    HandlePostResponse(Response);
  finally
    Request.Free;
    Response.Free;
  end;
end;

function TFCMClient.ExtractContentType(const aHeader : String) : string;

var
  P : Integer;

begin
  // Handle things like:
  // application/json ; charset=UTF8
  P:=Pos(';',aHeader);
  if P>0 then
    Result:=Trim(Copy(aHeader,1,P-1))
  else
    Result:=aHeader;
end;

procedure TFCMClient.HandlePostResponse(aResponse : TWebClientResponse);

var
  Resp : TFCMResponse;
  CT : String;
  Headers : TStringDynArray;
  I : Integer;

begin
  Headers:=[];
  if not (aResponse.StatusCode in [200,204]) then
    Raise EFCM.CreateFmt(SErrInvalidResponseStatus,[aResponse.StatusCode,aResponse.StatusText,aResponse.GetContentAsString]);
  CT:=ExtractContentType(aResponse.Headers.Values[HeaderContentType]);
  if not SameText(CT,SContentTypeApplicationJSON) then
    Raise EFCM.CreateFmt(SErrInvalidResponseContentType,[CT]);
  if Assigned(FOnResponse) then
    begin
    SetLength(Headers,aResponse.Headers.Count);
    For I:=0 to Length(Headers)-1 do
     Headers[I]:=aResponse.Headers[i];
    Resp:=TFCMResponse.Create(aResponse.GetContentAsString,Headers);
    HandleResponse(Resp);
    end;
end;

procedure TFCMClient.HandleTokenResponse(aResponse : TWebClientResponse);

var
  JSONData : TJSONData;
  Ct : String;

begin
  if not (aResponse.StatusCode in [200,204]) then
    Raise EFCM.CreateFmt(SErrInvalidResponseStatus,[aResponse.StatusCode,aResponse.StatusText,aResponse.GetContentAsString]);
  CT:=ExtractContentType(aResponse.Headers.Values[HeaderContentType]);
  if not SameText(CT,SContentTypeApplicationJSON) then
    Raise EFCM.CreateFmt(SErrInvalidResponseContentType,[CT]);
  JSONData:=GetJSON(aResponse.Content);
  try
    if not (JSONData is TJSONObject) then
      Raise EFCM.Create(SErrInvalidJSONResponse);
    FBearerToken.LoadFromJSON(JSONData as TJSONObject);
    FBearerToken.TokenDateTime:=Now;
  finally
    JSONData.Free;
  end;
end;

procedure TFCMClient.FetchAccessToken(const aUrl, aJWT: string);

var
  Request : TWebClientRequest;
  Response : TWebClientResponse;

begin
  Response:=Nil;
  Request:=WebClient.CreateRequest;
  try
    Request.SetContentFromString(Format(SFCMAccessTokenQuery,[HTTPEncode(SFCMGrantType),HTTPEncode(AJWT)]));
    Request.Headers.Values[HeaderContentType]:=SContentTypeApplicationFormUrlEncoded;
    Response:=WebClient.ExecuteRequest('POST',aUrl,Request);
    HandleTokenResponse(Response);
  finally
    Request.Free;
    Response.Free;
  end;
end;


function TFCMClient.HandleResponse(const aResponse: TFCMResponse): boolean;
begin
  Result:=Assigned(FOnResponse);
  if Result then
    FOnResponse(Self, AResponse);
end;

procedure TFCMClient.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FWebClient) then
    FWebClient:=Nil;
end;

procedure TFCMClient.HandleAccessToken;
begin

end;

function TFCMClient.GenerateJWT: string;

var
  LJWT: TGoogleJWT;
  Signer : TJWTSignerRS256;
  Key : TJWTKey;
  pkt : TPrivateKeyType;
  pk,ec : TBytes;

begin
  Result:='';
  try
    if not FServiceAccount.IsValid then
      Raise EFCM.Create(SErrInvalidServiceData);
    Signer:=Nil;
    LJWT:=TGoogleJWT.Create;
    try
      LJWT.JOSE.Alg := 'RS256';
      LJWT.JOSE.Typ := 'JWT';
      LJWT.Claims.Aud := SFCMAudience;
      LJWT.Claims.Iss:=FServiceAccount.client_email;
      LJWT.Claims.iat:=DateTimeToUnix(Now, False);
      LJWT.Claims.exp:=DateTimeToUnix(IncHour(Now, 1), False);
      LJWT.GoogleClaims.scope:=SFCMScopes;
      Signer:=TJWTSignerRS256.Create;
      PemLoadPrivateKeyAsDER(FServiceAccount.private_key,pk,ec,pkt);
      if pk=Nil then
        Raise EFCM.Create(SErrInvalidPrivateKey);
      Key:=TJWTKey.Create(pk);
      LJWT.Signature:=Signer.CreateSignature(LJWT,Key);
      Result:=LJWT.AsString;
    finally
      LJWT.Free;
      Signer.Free;
    end;
  except
    on E: Exception do
      if not HandleError(TFCMError.Create(esConfig,'',ExceptionToString(E))) then
        Raise;
  end;
end;

function TFCMClient.CreateWebClient: TAbstractWebClient;

begin
  if not Assigned(DefaultWebClientClass) then
    Raise EFCM.Create(SErrNoWebclientSet);
  Result:=DefaultWebClientClass.Create(Self);
  Result.SetSubComponent(True);
  if Self.LogFile<>'' then
    Result.LogFile:=Self.LogFile;
end;

function TFCMClient.GetWebClient: TAbstractWebClient;
begin
  if FWebClient=Nil then
    FWebClient:=CreateWebClient;
  Result:=FWebClient;
end;

procedure TFCMClient.SetBearerToken(AValue: TBearerToken);
begin
  if FBearerToken=AValue then Exit;
  FBearerToken.Assign(AValue);
end;

procedure TFCMClient.HandleNewBearerToken;

begin
  If Assigned(FOnNewBearerToken) then
    FOnNewBearerToken(Self,FBearerToken);
end;

function TFCMClient.GetAccessToken: Boolean;

var
  JWT: string;

begin
  JWT:=GenerateJWT;
  try
    Result:=JWT<>'';
    FetchAccessToken(FServiceAccount.token_uri,JWT);
    Result:=not BearerToken.IsExpired;
    if Result then
      HandleNewBearerToken
    else
      Raise EFCM.Create(SErrReceivedExpiredToken);
  except
    on E: Exception do
      if not HandleError(TFCMError.Create(esAccessToken,JWT,ExceptionToString(E))) then
          Raise;
  end;
end;

procedure TFCMClient.InitServiceAccount(const aFileName: string; aRoot: TJSONStringType);

var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    InitServiceAccount(F,aRoot);
  finally
    F.Free;
  end;
end;

procedure TFCMClient.InitServiceAccount(const aJSON: TStream; aRoot: TJSONStringType);

var
  Data : TJSONData;
  Obj : TJSONObject absolute Data;
  AccountObj : TJSONObject;

begin
  AccountObj:=nil;
  Data:=GetJSON(aJSON);
  try
    if not (Data is TJSONObject) then
      Raise EFCM.Create(SErrInvalidJSONServiceData);
    if aRoot='' then
      AccountObj:=Obj
    else
      if not Obj.Find(aRoot,AccountObj) then
        Raise EFCM.CreateFmt(SErrNoServiceDataAt,[aRoot]);
    FServiceAccount.LoadFromJSON(AccountObj);
  finally
    Data.Free;
  end;
end;

procedure TFCMClient.InitServiceAccount(const aJSON: TJSONStringType; aRoot: TJSONStringType);

var
  S : TBytesStream;

begin
  S:=TBytesStream.Create(TEncoding.UTF8.GetAnsiBytes(aJSON));
  try
    InitServiceAccount(S,aRoot);
  finally
    S.Free;
  end;
end;

procedure TFCMClient.InitServiceAccount(const aJSON: TJSONObject);
begin
  FServiceAccount.LoadFromJSON(aJSON);
  If not FServiceAccount.IsValidData then
    Raise EFCM.Create(SErrInvalidServiceData);
end;

function TFCMClient.Send(aMsg: TNotificationMessage; aRecipient: UTF8String): Boolean;
begin
  Result:=Send(aMsg,[aRecipient]);
end;

function TFCMClient.Send(aMsg: TNotificationMessage; aRecipients: array of UTF8String): Boolean;

var
  aJSON,aMsgJSON : TJSONObject;
  I : integer;

begin
    Result:=True;
    I:=Low(aRecipients);
    While Result and (I<=High(aRecipients)) do
      begin
      aJSON:=TJSONObject.Create;
      try
        aMsgJSON:=TJSONObject.Create;
        aMsg.Recipient:=aRecipients[i];
        aMsg.RecipientType:=rtToken;
        aMsg.ToJSON(aMsgJSON);
        aJSON.Add('message',aMsgJSON);
        Result:=Post(aJSON.AsJSON);
      finally
        aJSON.Free;
      end;
      Inc(I);
      end;
end;

function TFCMClient.Post(const aJSON: UTF8String): Boolean;

var
  URL : UTF8String;

begin
  try
    Result:=not BearerToken.IsExpired;
    If not Result then
      Result:=GetAccessToken;
    if Result then
      begin
      URL:=GetSendEndPoint(ServiceAccount.project_id);
      HandlePost(URL,aJSON)
      end;
  except
    on E: Exception do
      if not HandleError(TFCMError.Create(esPost,aJSON,ExceptionToString(E))) then
        Raise;
  end;
end;

constructor TFCMClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FServiceAccount:=TServiceAccountData.Create;
  FBearerToken:=TBearerToken.Create;
end;

destructor TFCMClient.Destroy;
begin
  FreeAndNil(FBearerToken);
  FreeAndNil(FServiceAccount);
  inherited Destroy;
end;

end.

