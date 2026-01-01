{
    This file is part of the Free Component Library

    Webassembly HTTP API - object-oriented interface.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.http.objects;

{$mode ObjFPC}
{$h+}
{$modeswitch functionreferences}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs,
{$ELSE}
  Classes, sysutils, contnrs,
{$ENDIF}
  wasm.http.shared, wasm.http.api;

Type
  EWasmHTTP = Class(Exception);

  TWasmHTTPHeaders = Class;
  TWasmHTTPRequest = Class;
  TWasmHTTPResponse = Class;

  // Keep these in the same order as the definition in wasm.http.shared !
  TWasmHTTPCache = (whcDefault,whcNoStore,whcReload,whcNoCache,whcForceCache,whcOnlyIfCached);
  TWasmHTTPCredentials = (whrSameOrigin,whrOmit,whrInclude);
  TWasmHTTPMode = (whmCors,whmSameOrigin,whmNoCors,whmNavigate,whmWebSocket);
  TWasmHTTPPriority = (whpAuto,whpLow,whpHigh);
  TWasmHTTPRedirect = (whdFollow,whdError,whdManual);

  { TWasmHTTPHeaders }

  TWasmHTTPHeaders = Class(TPersistent)
  private
    FBody: TBytes;
    FHeaders: TStrings;
    procedure SetBody(AValue: TBytes);
    procedure SetHeaders(AValue: TStrings);
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure Assign(aSource : TPersistent); override;
    Function BodyAsUTF8 : UTF8String;
    Property Headers : TStrings Read FHeaders Write SetHeaders;
    Property Body : TBytes Read FBody Write SetBody;
  end;

    { TWasmHTTPResponse }
  TWasmHTTPResponse = Class(TWasmHTTPHeaders)
  private
    FRequest: TWasmHTTPRequest;
    FStatus: Longint;
    FStatusText: String;
    FRequestID: TWasmHTTPRequestID;
  protected
    function CheckRes(Res: TWasmHTTPResult; const aOperation: String): Boolean; virtual;
    function GetBody: boolean; virtual;
    function GetHeaderName(aIndex: Integer; out aValue: String): Boolean; virtual;
    function GetHeaderValue(const aName: String; out aValue: String): Boolean; virtual;
    function GetStatus: Boolean; virtual;
    function GetStatusText: Boolean; virtual;
    function Init: Boolean; virtual;
    function StdBodyLength : Longint; virtual;
  Public
    const
      DefaultBodyLength  = 64*1024; // 64Kb
  public
    Constructor Create(aRequest : TWasmHTTPRequest); reintroduce;
    Destructor Destroy; override;
    Procedure Assign(aSource : TPersistent); override;
    Property RequestID : TWasmHTTPRequestID Read FRequestID;
    Property Request : TWasmHTTPRequest Read FRequest;
    Property StatusText : String Read FStatusText;
    Property Status : Integer Read FStatus;
  end;


  // The response object (and corresponding
  TOnWasmHTTPResponseEvent = Procedure (aResponse : TWasmHTTPResponse) of Object;
  TOnWasmHTTPResponseCallback = Procedure (aResponse : TWasmHTTPResponse);
  TOnWasmHTTPResponseHandler = Reference to Procedure (aResponse : TWasmHTTPResponse);


  { TWasmHTTPRequest }

  TWasmHTTPRequest = Class(TWasmHTTPHeaders)
  private
    FAbortSignal: Boolean;
    FCache: TWasmHTTPCache;
    FCredentials: TWasmHTTPCredentials;
    FIntegrity: String;
    FKeepAlive: Boolean;
    FMethod: String;
    FMode: TWasmHTTPMode;
    FPriority: TWasmHTTPPriority;
    FRedirect: TWasmHTTPRedirect;
    FReferrer: String;
    FReferrerPolicy: String;
    FURL: String;
    FApiBytes : Array of TBytes;
    FApiHeaders : Array of TWasmString;
    FScheduled : Boolean;
    FRequestID : TWasmHTTPRequestID;
  Protected
    function AsAPIRequest : TWasmHTTPApiRequest;
    Function Scheduled : Boolean;
    procedure CheckNotScheduled;
  Public
    constructor Create(const aURL : String);
    Procedure Assign(aSource : TPersistent); override;
    Function Execute(OnResponse : TOnWasmHTTPResponseEvent): TWasmHTTPRequestID;
    Function Execute(OnResponse : TOnWasmHTTPResponseCallback): TWasmHTTPRequestID;
    Function Execute(OnResponse : TOnWasmHTTPResponseHandler): TWasmHTTPRequestID;
    function ToString : RTLString; override;
    Property URL : String Read FURL Write FURL;
    Property Method : String Read FMethod Write FMethod;
    Property Cache : TWasmHTTPCache Read FCache Write FCache;
    Property Credentials : TWasmHTTPCredentials Read FCredentials Write FCredentials;
    Property Mode: TWasmHTTPMode Read FMode Write FMode;
    Property Priority : TWasmHTTPPriority Read FPriority Write FPriority;
    Property Redirect : TWasmHTTPRedirect Read FRedirect Write FRedirect;
    Property Referrer : String Read FReferrer Write FReferrer;
    Property ReferrerPolicy : String Read FReferrerPolicy Write FReferrerPolicy;
    Property Integrity : String Read FIntegrity Write FIntegrity;
    Property KeepAlive : Boolean Read FKeepAlive Write FKeepAlive;
    Property AbortSignal : Boolean Read FAbortSignal Write FAbortSignal;
  end;



  TWasmHTTP = Class(TObject)
  Private
    Type

       { TRequest }

       TRequest = Class(TObject)
       Private
         FResponse: TWasmHTTPResponse;
       Public
         constructor Create(aRequest : TWasmHTTPRequest);
         procedure DoCallBack; Virtual; abstract;
         procedure CallBack;
       end;

       { TEventedRequest }

       TEventedRequest = class(TRequest)
         FEvent : TOnWasmHTTPResponseEvent;
         constructor Create(aRequest : TWasmHTTPRequest; aEvent: TOnWasmHTTPResponseEvent); reintroduce;
         procedure DoCallBack; override;
       end;

       { TCallbackedRequest }

       TCallbackedRequest = class(TRequest)
         FEvent : TOnWasmHTTPResponseCallback;
         constructor Create(aRequest : TWasmHTTPRequest; aEvent: TOnWasmHTTPResponseCallback); reintroduce;
         procedure DoCallBack; override;
       end;

       { THandleRequest }

       THandleRequest = class(TRequest)
         FEvent : TOnWasmHTTPResponseHandler;
         constructor Create(aRequest : TWasmHTTPRequest; aEvent: TOnWasmHTTPResponseHandler); reintroduce;
         procedure DoCallBack; override;
       end;

    class var
      _list : TFPObjectList;
    class function DoHTTPRequest(aRequest: TRequest): TWasmHTTPRequestID;
    class procedure HandleResponse(aRequestID : Longint; aUserData : Pointer; aStatus : TWasmHTTPResponseStatus; var Deallocate : Boolean); static;
    class procedure LogResponseError(aResponse : TWasmHTTPResponse; E : Exception);
  public
    Class Constructor Init;
    Class Destructor Done;
    // Ownership of aRequest is transferred to TWasmHTTP, it will free the request once the response is handled.
    Class Function HTTPRequest(aRequest : TWasmHTTPRequest; OnResponse : TOnWasmHTTPResponseEvent): TWasmHTTPRequestID;
    Class Function HTTPRequest(aRequest : TWasmHTTPRequest; OnResponse : TOnWasmHTTPResponseCallback): TWasmHTTPRequestID;
    Class Function HTTPRequest(aRequest : TWasmHTTPRequest; OnResponse : TOnWasmHTTPResponseHandler): TWasmHTTPRequestID;
  end;



implementation

{ TWasmHTTPHeaders }

procedure TWasmHTTPHeaders.SetHeaders(AValue: TStrings);
begin

  if FHeaders=AValue then Exit;
  FHeaders.Assign(AValue);
end;

procedure TWasmHTTPHeaders.SetBody(AValue: TBytes);
begin

  if FBody=AValue then Exit;
  FBody:=AValue;

end;

constructor TWasmHTTPHeaders.Create;
begin
  FHeaders:=TStringList.Create;
  FHeaders.NameValueSeparator:=':';
end;

destructor TWasmHTTPHeaders.Destroy;
begin
  FreeAndNil(FHeaders);
  inherited Destroy;
end;

procedure TWasmHTTPHeaders.Assign(aSource: TPersistent);

var
  Src : TWasmHTTPHeaders absolute aSource;

begin

  if aSource is TWasmHTTPHeaders then
    begin
    Headers.Assign(Src.Headers);
    Body:=Copy(Src.Body,0,Length(Src.Body));
    end
  else
    inherited Assign(aSource);

end;

function TWasmHTTPHeaders.BodyAsUTF8: UTF8String;
begin
   Result:=TEncoding.UTF8.GetAnsiString(FBody);
end;

{ TWasmHTTPResponse }

function TWasmHTTPResponse.CheckRes(Res: TWasmHTTPResult; const aOperation: String): Boolean;

begin
  Result:=False;
  if Res=WASMHTTP_RESULT_INPROGRESS then
    begin
    __wasmhttp_log(hllError,'Request %d still in progress',[RequestID]);
    exit;
    end;
  if Res=WASMHTTP_RESULT_INVALIDID then
    begin
    __wasmhttp_log(hllError,'Request %d does not exist',[RequestID]);
    exit;
    end;
  if Res<>WASMHTTP_RESULT_SUCCESS then
    begin
    __wasmhttp_log(hllWarning,'Failed to %s : request %d does not exist',[aOperation,RequestID]);
    exit;
    end;
  Result:=True;

end;

function TWasmHTTPResponse.GetHeaderName(aIndex: Integer; out aValue: String): Boolean;

var
  S : String = '';
  sLen : Longint;
  Res : TWasmHTTPResult;

begin

  SLen:=256;
  SetLength(S,sLen);
  Res:=__wasmhttp_response_get_headername(RequestID,aIndex,PByte(S),@sLen);
  if (Res=WASMHTTP_RESULT_INSUFFICIENTMEM) then
    begin
    SetLength(S,sLen);
    Res:=__wasmhttp_response_get_headername(RequestID,aIndex,PByte(S),@sLen);
    end;
  Result:=CheckRes(Res,'Get header name');
  if not Result then
    aValue:=''
  else
    begin
    if (SLen<>Length(S)) then
      SetLength(S,sLen);
    aValue:=S;
    end;

end;

function TWasmHTTPResponse.GetHeaderValue(const aName: String; out aValue: String): Boolean;

var
  S : UTF8String = '';
  sLen : Longint;
  Res : TWasmHTTPResult;

begin

  SLen:=256;
  SetLength(S,sLen);
  Res:=__wasmhttp_response_get_header(RequestID,PByte(aName),Length(aName),PByte(S),@sLen);
  if (Res=WASMHTTP_RESULT_INSUFFICIENTMEM) then
    begin
    SetLength(S,sLen);
    Res:=__wasmhttp_response_get_header(RequestID,PByte(aName),Length(aName),PByte(S),@sLen);
    end;
  Result:=CheckRes(Res,'Get header value');
  if not Result then
    aValue:=''
  else
    begin
    if (SLen<>Length(S)) then
      SetLength(S,sLen);
    aValue:=S;
    end
end;

function TWasmHTTPResponse.GetStatus : Boolean;

begin
  Result:=CheckRes(__wasmhttp_response_get_status(RequestID,@FStatus),'get status');
end;

function TWasmHTTPResponse.GetStatusText : Boolean;

var
  S : UTF8String = '';
  sLen : Longint;
  Res : TWasmHTTPResult;

begin
  SLen:=256;
  SetLength(S,sLen);
  Res:=__wasmhttp_response_get_statustext(RequestID,PByte(S),@sLen);
  if (Res=WASMHTTP_RESULT_INSUFFICIENTMEM) then
    begin
    SetLength(S,sLen);
    Res:=__wasmhttp_response_get_statustext(RequestID,PByte(S),@sLen);
    end;
  Result:=CheckRes(Res,'Get status text');
  if not result then
    exit;
  if sLen<>Length(S) then
    SetLength(S,sLen);
  FStatusText:=S;

end;

function TWasmHTTPResponse.StdBodyLength: Longint;

begin
  Result:=DefaultBodyLength;
end;

function TWasmHTTPResponse.GetBody: boolean;

var
  B : TBytes;
  bLen : Longint;
  Res : TWasmHTTPResult;

begin
  B:=[];
  bLen:=StdBodyLength;
  SetLength(B,bLen);
  Res:=__wasmhttp_response_get_body(RequestID,PByte(B),@bLen);
  if (Res=WASMHTTP_RESULT_INSUFFICIENTMEM) then
    begin
    SetLength(B,bLen);
    Res:=__wasmhttp_response_get_body(RequestID,PByte(B),@bLen);
    end;
  Result:=CheckRes(Res,'Get status text');
  if not result then
    exit;
  if bLen<>Length(B) then
    SetLength(B,BLen);
  FBody:=B;
end;

function TWasmHTTPResponse.Init : Boolean;

var
  H,N : String;
  i,aCount : Longint;

begin
  Result:=False;
  aCount:=0;
  if not GetStatus then
    exit;
  if not GetStatusText then
    exit;
  if not CheckRes(__wasmhttp_response_get_headercount(RequestID,aCount),'get header count') then
    exit;
  For I:=0 to aCount-1 do
    begin
    if not GetHeaderName(I,N) then
      exit;
    if not GetHeaderValue(N,H) then
      exit;
    FHeaders.Values[N]:=H;
    end;
  if not GetBody then
    exit;
  Result:=True;
end;

constructor TWasmHTTPResponse.Create(aRequest: TWasmHTTPRequest);
begin
  Inherited Create();
  FRequest:=aRequest;
end;

destructor TWasmHTTPResponse.Destroy;
begin
  FreeAndNil(FRequest);
  inherited Destroy;
end;

procedure TWasmHTTPResponse.Assign(aSource: TPersistent);
var
  Src : TWasmHTTPResponse absolute aSource;
begin
  if aSource is TWasmHTTPResponse then
    begin
    FStatus:=Src.Status;
    FStatusText:=Src.StatusText;
    end;
  inherited Assign(aSource);
end;


class procedure TWasmHTTP.HandleResponse(aRequestID: Longint; aUserData: Pointer; aStatus: TWasmHTTPResponseStatus; var Deallocate: Boolean);

var
  Req : TRequest;

begin
  DeAllocate:=True;
  Req:=TRequest(aUserData);
  try
    if aRequestID<>Req.Fresponse.RequestID then
      Raise EWasmHTTP.CreateFmt('Inconsistent data: Response Request ID %d is not the stored ID %d',[Req.Fresponse.RequestID]);
    Req.FResponse.Init;
  except
    On E : Exception do
      LogResponseError(Req.FResponse,E);
  end;
  Req.Callback;
  _List.Remove(Req);
end;

class procedure TWasmHTTP.LogResponseError(aResponse: TWasmHTTPResponse; E: Exception);

var
  Msg : String;

begin
  With aResponse do
    Msg:=SafeFormat('Error %s handling response of %d ("%s %s") : %s',[RequestID,E.ClassName,Request.Method,Request.URL,E.Message]);
  __wasmhttp_log(hllError,Msg);
end;

class constructor TWasmHTTP.Init;
begin
  WasmHTTPResponseCallback:=@HandleResponse;
  _list:=TFPObjectList.Create(True);
end;

class destructor TWasmHTTP.Done;
begin
  WasmHTTPResponseCallback:=Nil;
  FreeAndNil(_List);
end;


class function TWasmHTTP.DoHTTPRequest(aRequest: TRequest): TWasmHTTPRequestID;

var
  lResp : TWasmHTTPResponse;
  lReq : TWasmHTTPRequest;
  WasmReq : TWasmHTTPApiRequest;
  ID : TWasmHTTPRequestID;
  Res : TWasmHTTPResult;
  Msg : String;

begin
  lResp:=aRequest.FResponse;
  lReq:=lResp.Request;
  WasmReq:=lReq.AsAPIRequest;
  Res:=__wasmhttp_request_allocate(@WasmReq,aRequest,@ID);
  if Res=WASMHTTP_RESULT_SUCCESS then
    begin
    lReq.FScheduled:=True;
    lResp.FRequestID:=ID;
    _list.Add(aRequest);
    Res:=__wasmhttp_request_execute(ID);
    if Res<>WASMHTTP_RESULT_SUCCESS then
      begin
      msg:=SafeFormat('Failed to execute request for request %s %s',[lReq.Method,lReq.URL]);
      __wasmhttp_log(hllError,Msg);
      end;
    Result:=ID;
    end
  else
    begin
    msg:=SafeFormat('Failed to allocate request for request %s %s',[lReq.Method,lReq.URL]);
    __wasmhttp_log(hllError,Msg);
    aRequest.Free;
    Raise EWasmHTTP.Create(Msg);
    end;

end;

class function TWasmHTTP.HTTPRequest(aRequest: TWasmHTTPRequest; OnResponse: TOnWasmHTTPResponseEvent): TWasmHTTPRequestID;


begin
  Result:=DoHTTPRequest(TEventedRequest.Create(aRequest,OnResponse));
end;

class function TWasmHTTP.HTTPRequest(aRequest: TWasmHTTPRequest; OnResponse: TOnWasmHTTPResponseCallback): TWasmHTTPRequestID;

begin
  Result:=DoHTTPRequest(TCallbackedRequest.Create(aRequest,OnResponse));
end;


class function TWasmHTTP.HTTPRequest(aRequest: TWasmHTTPRequest; OnResponse: TOnWasmHTTPResponseHandler): TWasmHTTPRequestID;
begin
  Result:=DoHTTPRequest(THandleRequest.Create(aRequest,OnResponse));
end;

{ TWasmHTTP.TRequest }

constructor TWasmHTTP.TRequest.Create(aRequest: TWasmHTTPRequest);

var
  Resp : TWasmHTTPResponse;

begin
  Resp:=TWasmHTTPResponse.Create(aRequest);
  FResponse:=Resp;
end;


procedure TWasmHTTP.TRequest.CallBack;
begin
  try
    DoCallback
  except
    On E : Exception Do
      LogResponseError(FResponse,E)
  end;
end;

{ TWasmHTTP.TEventedRequest }

constructor TWasmHTTP.TEventedRequest.Create(aRequest: TWasmHTTPRequest; aEvent: TOnWasmHTTPResponseEvent);
begin
  Inherited Create(aRequest);
  FEvent:=aEvent;
end;

procedure TWasmHTTP.TEventedRequest.DoCallBack;
begin
  FEvent(FResponse);
end;

{ TWasmHTTP.TCallbackedRequest }

constructor TWasmHTTP.TCallbackedRequest.Create(aRequest: TWasmHTTPRequest; aEvent: TOnWasmHTTPResponseCallback);
begin
  Inherited Create(aRequest);
  FEvent:=aEvent;
end;

procedure TWasmHTTP.TCallbackedRequest.DoCallBack;
begin
  FEvent(FResponse);
end;

{ TWasmHTTP.THandleRequest }

constructor TWasmHTTP.THandleRequest.Create(aRequest: TWasmHTTPRequest; aEvent: TOnWasmHTTPResponseHandler);
begin
  Inherited Create(aRequest);
  FEvent:=aEvent;
end;

procedure TWasmHTTP.THandleRequest.DoCallBack;
begin
  FEvent(FResponse);
end;

{ TWasmHTTPRequest }

function TWasmHTTPRequest.AsAPIRequest: TWasmHTTPApiRequest;


  Function StringToWasmString(var Idx : Integer; const S : String): TWasmString;

  var
    B : TBytes;

  begin
    {$IF SIZEOF(CHAR)=2}
      B:=TEncoding.UTF8.GetBytes(S);
    {$ELSE}
      B:=TEncoding.UTF8.GetAnsiBytes(S);
    {$ENDIF}
    FAPIBytes[Idx]:=B;
    Result.Len:=Length(B);
    Result.Data:=PAnsiChar(PByte(B));
    Inc(Idx);
  end;

var
  Idx,Idx2 : Integer;
  H : String;

begin
  Idx:=0;
  Result:=Default(TWasmHTTPApiRequest);
  SetLength(FAPIBytes,FHeaders.Count+5);
  Result.Url:=StringToWasmString(Idx,URL);
  Result.Method:=StringToWasmString(Idx,Method);
  Result.Referrer:=StringToWasmString(Idx,Referrer);
  Result.ReferrerPolicy:=StringToWasmString(Idx,ReferrerPolicy);
  Result.Integrity:=StringToWasmString(Idx,Integrity);
  Result.HeaderCount:=FHeaders.Count;
  SetLength(FAPIHeaders,FHeaders.Count);
  Idx2:=0;
  For H in FHeaders do
    begin
    FAPIHeaders[Idx2]:=StringToWasmString(Idx,H);
    Inc(Idx2);
    end;
  Result.Headers:=PWasmString(FAPIHeaders);
  Result.AbortSignal:=Ord(AbortSignal);
  Result.KeepAlive:=Ord(KeepAlive);
  Result.Cache:=Ord(Cache);
  Result.Mode:=Ord(Mode);
  Result.Priority:=Ord(Priority);
  Result.Redirect:=Ord(Redirect);
  Result.Credentials:=Ord(Credentials);
  Result.Body.Len:=Length(FBody);
  if Result.Body.Len>0 then
    Result.Body.Data:=PByte(FBody);
end;

function TWasmHTTPRequest.Scheduled: Boolean;
begin
  Result:=FScheduled;
end;

constructor TWasmHTTPRequest.Create(const aURL: String);
begin
  Inherited Create;
  FURL:=aURL;
  Method:='GET';
end;

procedure TWasmHTTPRequest.Assign(aSource: TPersistent);

var
  Src : TWasmHTTPRequest absolute aSource;

begin
  if aSource is TWasmHTTPRequest then
    begin
    URL:=Src.URL;
    Method:=Src.Method;
    Cache:=Src.Cache;
    Credentials:=Src.Credentials;
    Mode:=Src.Mode;
    Priority:=Src.Priority;
    Redirect:=Src.Redirect;
    Referrer:=Src.Referrer;
    ReferrerPolicy:=Src.ReferrerPolicy;
    Integrity:=Src.Integrity;
    KeepAlive:=Src.KeepAlive;
    AbortSignal:=Src.AbortSignal;
    end;
  inherited Assign(aSource);
end;

procedure TWasmHTTPRequest.CheckNotScheduled;

begin
  If Fscheduled then
    Raise EWasmHTTP.CreateFmt('Request "%s" is already scheduled with ID: %d',[ToString,FRequestID]);
end;

function TWasmHTTPRequest.Execute(OnResponse: TOnWasmHTTPResponseEvent): TWasmHTTPRequestID;
begin
  CheckNotScheduled;
  Result:=TWasmHTTP.HTTPRequest(Self,OnResponse);
end;

function TWasmHTTPRequest.Execute(OnResponse: TOnWasmHTTPResponseCallback): TWasmHTTPRequestID;
begin
  CheckNotScheduled;
  Result:=TWasmHTTP.HTTPRequest(Self,OnResponse);
end;

function TWasmHTTPRequest.Execute(OnResponse: TOnWasmHTTPResponseHandler): TWasmHTTPRequestID;
begin
  CheckNotScheduled;
  Result:=TWasmHTTP.HTTPRequest(Self,OnResponse);
end;

function TWasmHTTPRequest.ToString: RTLString;
begin
  Result:=Method +' '+URL;
end;

end.

