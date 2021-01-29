{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    TCustomMicroHTTPApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ $define CGIDEBUG}
{$mode objfpc}
{$H+}

unit custmicrohttpapp;

Interface

uses
  Classes, SysUtils, httpprotocol, httpdefs, custweb, libmicrohttpd;

Type
  TCustomMicroHTTPApplication = Class;
  TMicroServerOption = (
     mcoDebug,
     mcoSSL,
     mcoThreadPerConnection,
     mcoSelectInternally,
     mcoIPV6,
     mcoPedanticChecks,
     mcoPoll,
     mcoSuppressDateNoClock,
     mcoNoListenSocket,
     mcoEPollLinuxOnly,
     mcoPipeForShutdown,
     mcoDualStack,
     mcoEPollTurbo,
     mcoSuspendResume,
     mcoTCPFastOpen
  );
  TMicroServerOptions = Set of TMicroServerOption;

  TMicroHTTPHandler = Class;
  TRequestHandler = Class;

  { TMicroRequest }

  TMicroRequest = Class(TRequest)
  Private
    FHandler : TRequestHandler;
    FMyQueryString : String;
    // Return amount of data handled
    Procedure DoSetHeader(K,V : String);
    Procedure AddQueryField(K,V : String);
    Function AddData(Data: PAnsiChar; DataSize: Size_t) : Size_t;
    Procedure Initialize(const aUrl, aMethod, aVersion: String);
    procedure InitRequestVars; override;
  Protected
    Property Handler : TRequestHandler Read FHandler;
  end;

  { TMicroResponse }

  TMicroResponse = Class(TResponse)
  Private
    FHandler : TRequestHandler;
    FResponse : PMHD_Response;
  Protected
    Procedure MaybeAllocateResponse; virtual;
    Procedure DoSendHeaders(Headers: TStrings); override;
    Procedure DoSendContent; override;
    Property Handler : TRequestHandler Read FHandler;
    Property Response : PMHD_Response Read FResponse;
  Public
    Destructor Destroy; override;
  end;

  { TRequestHandler }

  TRequestHandler = Class
    FConnection : PMHD_Connection;
    FWebHandler : TMicroHTTPHandler;
    FRequest : TMicroRequest;
    FResponse : TMicroResponse;
  Public
    Constructor Create(aHandler : TMicroHTTPHandler; aConnection :PMHD_Connection);
    Destructor Destroy; override;
    Procedure Initialize(const aUrl, aMethod, aVersion: String);
    Function ContinueRequest(Data: PAnsiChar; var DataSize: Size_t) : Cint;
    Property Connection : PMHD_Connection Read FConnection;
    Property WebHandler : TMicroHTTPHandler Read FWebHandler;
    Property Request : TMicroRequest Read FRequest;
    Property Response : TMicroResponse Read FResponse;
  end;

  { TMicroHTTPHandler }

  TAcceptHandler = Procedure (Sender : TObject; Addr : PSockAddr; addrLen : socklen_t; var Allow : Boolean) of object;
  TRequestErrorHandler = Procedure (Sender : TObject; E : Exception) of object;

  TMicroHTTPHandler = class(TWebHandler)
  Private
    FAcceptHandler: TAcceptHandler;
    FExtraHeaders: TStrings;
    FOnRequestError: TRequestErrorHandler;
    FPort : Word;
    FOptions: TMicroServerOptions;
    FServer: PMHD_Daemon;
    FHostName : string;
    procedure MaybeStopServer;
    procedure RequestCompleted(aRequest: TRequestHandler);
    function DoRequest(connection: PMHD_Connection; const aUrl, aMethod, aVersion: String; Data: PAnsiChar; var DataSize: Size_t): TRequestHandler;
    procedure SetExtraHeaders(AValue: TStrings);
    procedure SetHostName(AValue: String);
    procedure SetOptions(AValue: TMicroServerOptions);
    procedure SetPort(const AValue: Word);
  protected
    function OptionsToFlags : Integer;
    Function DoAcceptConnection(Addr : PSockAddr; addrLen : socklen_t) : Boolean;
    procedure CheckInactive;
    function CreateServer: PMHD_Daemon; virtual;
    procedure HandleRequestError(Sender: TObject; E: Exception); virtual;
    Procedure InitRequest(ARequest : TRequest); override;
    Procedure InitResponse(AResponse : TResponse); override;
    function WaitForRequest(out ARequest : TRequest; out AResponse : TResponse) : boolean; override;
    Property Daemon : PMHD_Daemon Read FServer;
  Public
    Procedure Run; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Port to listen on.
    Property Port : Word Read FPort Write SetPort Default 80;
    // HostName to use when using SSL
    Property HostName : String Read FHostName Write SetHostName;
    // ServerOPtions
    Property Options : TMicroServerOptions Read FOptions Write SetOptions;
    // On Accept handler
    Property OnAccept : TAcceptHandler Read FAcceptHandler Write FAcceptHandler;
    // Handle On Request error. If not set, error is logged.
    Property OnRequestError : TRequestErrorHandler Read FOnRequestError Write FOnRequestError;
    // Extra non-standard headers which can be accepted as part of requests
    Property ExtraHeaders : TStrings Read FExtraHeaders Write SetExtraHeaders;
  end;

  { TCustomMicroHTTPApplication }

  TCustomMicroHTTPApplication = Class(TCustomWebApplication)
  private
    function GetExtraHeaders: TStrings;
    function GetHostName: String;
    function GetOptions: TMicroServerOptions;
    function GetPort: Word;
    function GetUseSSL: Boolean;
    procedure SetExtraHeaders(AValue: TStrings);
    procedure SetHostName(AValue: String);
    procedure SetOptions(AValue: TMicroServerOptions);
    procedure SetPort(AValue: Word);
    procedure SetUseSSL(AValue: Boolean);
  protected
    function InitializeWebHandler: TWebHandler; override;
    Function HTTPHandler : TMicroHTTPHandler;
  Public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    Property Port : Word Read GetPort Write SetPort Default 80;
    // Hostname to use when using SSL
    Property HostName : String Read GetHostName Write SetHostName;
    // ServerOptions
    Property Options : TMicroServerOptions Read GetOptions Write SetOptions;
    // For compatibility
    Property UseSSL : Boolean Read GetUseSSL Write SetUSeSSL;
    // Extra non-standard headers which can be accepted as part of requests
    Property ExtraHeaders : TStrings Read GetExtraHeaders Write SetExtraHeaders;
  end;


Implementation

Resourcestring
  SErrServerActive = 'Operation cannot be performed while server is active';
  SErrFailedToStartServer = 'Failed to start server';


Const
  BoolToYesNo : Array[Boolean] of Integer = (MHD_NO,MHD_YES);

Const
  OptionFlags :   Array[TMicroServerOption] of Integer = (
  MHD_USE_DEBUG,
  MHD_USE_SSL,
  MHD_USE_THREAD_PER_CONNECTION,
  MHD_USE_SELECT_INTERNALLY,
  MHD_USE_IPv6,
  MHD_USE_PEDANTIC_CHECKS,
  MHD_USE_POLL,
  MHD_SUPPRESS_DATE_NO_CLOCK,
  MHD_USE_NO_LISTEN_SOCKET,
  MHD_USE_EPOLL_LINUX_ONLY,
  MHD_USE_PIPE_FOR_SHUTDOWN,
  MHD_USE_DUAL_STACK,
  MHD_USE_EPOLL_TURBO,
  MHD_USE_SUSPEND_RESUME,
  MHD_USE_TCP_FASTOPEN);

{ ---------------------------------------------------------------------
  libmicrohttp Callbacks
  ---------------------------------------------------------------------}

Function MaybeS(p : pchar) : String;
begin
  if Assigned(P) then Result:=P else Result:='';
end;

function GetRequestData(cls: Pointer; kind: MHD_ValueKind; key: Pcchar; value: Pcchar): cint; cdecl;

var
  K,V : String;


begin
  K:=MaybeS(key);
  V:=MaybeS(Value);
  if kind=MHD_HEADER_KIND then
    TMicroRequest(Cls).DoSetHeader(K,V)
  else if kind=MHD_GET_ARGUMENT_KIND then
    TMicroRequest(Cls).AddQueryField(K,V);
  Result:=MHD_YES;
end;


procedure DoPanic(cls: Pointer; &file: Pcchar; line: cuint; reason: Pcchar); cdecl;

begin
  if Assigned(cls) then
    TCustomMicroHTTPApplication(Cls).Log(etError,Format('Panic at %s(%d): %s ',[MaybeS(&File),line,MaybeS(reason)]))
  else if IsConsole then
    writeln('Panic: File ',MaybeS(&File),'(',line,')',MaybeS(Reason));
end;


function DoReadResponse(cls: pointer; pos: cuint64; buf: Pcchar; max: size_t): ssize_t; cdecl;

Var
  Resp : TMicroResponse;

begin
  Resp:=TMicroResponse(cls);
  if Pos<>Resp.ContentStream.Position then
    Resp.ContentStream.Position:=Pos;
  Result:=Resp.ContentStream.Read(buf^,max);
end;


function AcceptCallBack(cls: Pointer; addr: psockaddr; addrlen: socklen_t): cint; cdecl;
begin
  Result:=BoolToYesNo[TMicroHTTPHandler(Cls).DoAcceptConnection(addr,addrlen)];
end;


function DoMHDRequest(cls: Pointer; connection: PMHD_Connection; url: Pcchar; method: Pcchar; version: Pcchar; upload_data: Pcchar;
  upload_data_size: pSize_t; con_cls: PPointer): cint; cdecl;

Var
  aURL : String;
  aMethod : String;
  aVersion : String;
  H : TMicroHTTPHandler;

begin
  aURL:=URl;
  aMethod:=Method;
  aVersion:=Version;
  if (Con_cls^=Nil) then
    begin
    H:=TMicroHTTPHandler(Cls);
    Con_cls^:=H.DoRequest(connection,aURL,aMethod,aVersion,Upload_Data,Upload_data_size^);
    Result:=BoolToYesNo[con_cls^<>Nil];
    end
  else
    Result:=TRequestHandler(Con_cls^).ContinueRequest(Upload_Data,Upload_data_size^);
end;

procedure HandleRequestCompleted(ACls: Pointer; AConnection: PMHD_Connection; AConCls: PPointer; AToe: MHD_RequestTerminationCode); cdecl;
var
  Req: TRequestHandler;
  H :  TMicroHTTPHandler;
begin
  Req:=TRequestHandler(AConCls^);
  if not Assigned(Req) then
    Exit;
  H:=TMicroHTTPHandler(aCls);
  if not Assigned(H) then
    H:=Req.WebHandler;
  if Assigned(H) then
    H.RequestCompleted(Req)
  else
    Req.Free;
  AConCls^ := nil;
end;


{ ---------------------------------------------------------------------
  TMicroRequest
  ---------------------------------------------------------------------}


procedure TMicroRequest.DoSetHeader(K, V: String);

Var
  H :  THeader;

begin
  H:=HeaderType(K);
  if hdRequest in HTTPHeaderDirections[h] then
    SetHeader(H,V)
  else
    SetCustomHeader(K,V);
end;

procedure TMicroRequest.AddQueryField(K, V: String);

Var
  S : String;

begin
  if V<>'' then
    QueryFields.Values[K]:=V
  else
    QueryFields.Add(K+'=');
  S:=FMyQueryString;
  if S<>'' then
    S:=S+'&';
  FMyQueryString:=S+K+'='+HTTPEncode(V);
end;

function TMicroRequest.AddData(Data: PAnsiChar; DataSize: Size_t): Size_t;

Var
  C : String;
  L : Integer;

begin
  C:=Content;
  L:=Length(C);
  SetLength(C,L+Datasize);
  Move(Data^,C[L+1],DataSize);
  InitContent(C);
  Result:=Datasize;
end;

procedure TMicroRequest.Initialize(const aUrl, aMethod, aVersion: String);

begin
  SetHTTPVariable(hvURL,aURL);
  SetHTTPVariable(hvMethod,aMethod);
  SetHTTPVariable(hvHTTPVersion,aVersion);
  InitRequestVars;
end;

procedure TMicroRequest.InitRequestVars;

Var
  P : Pchar;
  N,S  : String;
  I : integer;

begin
  MHD_get_connection_values(FHandler.FConnection, MHD_GET_ARGUMENT_KIND,@GetRequestData,Self);
  MHD_get_connection_values(FHandler.FConnection, MHD_HEADER_KIND,@GetRequestData,Self);
  for N in FHandler.WebHandler.ExtraHeaders do
    begin
    P:=MHD_lookup_connection_value(FHandler.FConnection, MHD_HEADER_KIND,Pchar(N));
    If P<>Nil then
      SetCustomHeader(N,P);
    end;
  S:=URL;
  I:=Pos('?',S);
  if (I>0) then
    S:=Copy(S,1,I-1);
  If (Length(S)>1) and (S[1]<>'/') then
    S:='/'+S
  else if S='/' then
    S:='';
  PathInfo:=S;
  Inherited;
  // We set this afterwards, otherwise double processing
  if FMyQueryString<>'' then
    SetHTTPVariable(hvQuery,FMyQueryString)
end;

{ ---------------------------------------------------------------------
  TMicroResponse
  ---------------------------------------------------------------------}

procedure TMicroResponse.MaybeAllocateResponse;

Var
  L : Integer;
  P : PChar;
  B : TBytes;

begin
  if FResponse<>Nil then exit;
  if Assigned(ContentStream) then
    begin
    ContentStream.Position:=0;
    L:=ContentStream.Size;
    if FreeContentStream then
      FResponse:=MHD_create_response_from_callback(L,4096,@DoReadResponse,Self,Nil)
    else
      // We must copy the bytes, because we don't know when the stream is freed.
      begin
      SetLength(B,L);
      ContentStream.ReadBuffer(B[0],L);
      P:=Pchar(B);
      FResponse:=MHD_create_response_from_buffer(L,P,MHD_RESPMEM_MUST_COPY);
      end;
    end
  else
    begin
    L:=Length(Content);
    P:=PChar(Content);
    FResponse:=MHD_create_response_from_buffer(L,P,MHD_RESPMEM_MUST_COPY);
    end;
end;

procedure TMicroResponse.DoSendHeaders(Headers: TStrings);

Var
  I : Integer;
  N,V : String;

begin
  // Note that if the response is allocated, then you cannot set the content stream any more...
  MaybeAllocateResponse;
  Headers.NameValueSeparator:=':';
  For I:=0 to Headers.Count-1 do
    begin
    Headers.GetNameValue(I,N,V);
    MHD_add_response_header(FResponse,PAnsiChar(N),PAnsiChar(V));
    end;
end;

procedure TMicroResponse.DoSendContent;
begin
  MaybeAllocateResponse;
  MHD_queue_response(FHandler.FConnection,Self.Code,FResponse);
end;

destructor TMicroResponse.Destroy;
begin
  if (FResponse<>Nil) then
    MHD_destroy_response(FResponse);
  inherited Destroy;
end;

{ ---------------------------------------------------------------------
  TRequestHandler
  ---------------------------------------------------------------------}

constructor TRequestHandler.Create(aHandler: TMicroHTTPHandler; aConnection: PMHD_Connection);
begin
  FWebHandler:=aHandler;
  FConnection:=aConnection;
  FRequest:=TMicroRequest.Create;
  FRequest.FHandler:=Self;
  FResponse:=TMicroResponse.Create(FRequest);
  FResponse.FHandler:=Self;
end;

destructor TRequestHandler.Destroy;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  inherited Destroy;
end;

procedure TRequestHandler.Initialize(const aUrl, aMethod, aVersion: String);
begin
  FRequest.Initialize(aURL,aMethod,aVersion);
end;

function TRequestHandler.ContinueRequest(Data: PAnsiChar; var DataSize: Size_t): Cint;

Var
  CanHandleRequest : Boolean;

begin
  CanHandleRequest:=Datasize=0;
  if Datasize>0 then
    DataSize:=DataSize-FRequest.AddData(Data,Datasize);
  If Not CanHandleRequest then
    Result:=BoolToYesNo[DataSize=0]
  else
    begin
    try
      WebHandler.HandleRequest(FRequest,FResponse);
      If Not FResponse.ContentSent then
        try
          FResponse.SendContent;
        except
          On E : Exception do
            WebHandler.HandleRequestError(WebHandler,E);
        end;
      Result:=MHD_YES;
    except
      On E : Exception do
        begin
        Result:=MHD_NO;
        WebHandler.HandleRequestError(WebHandler,E);
        end;
    end;
    end;
end;


{ ---------------------------------------------------------------------
  TMicroHTTPHandler
  ---------------------------------------------------------------------}

procedure TMicroHTTPHandler.RequestCompleted(aRequest: TRequestHandler);

begin
  try
    EndRequest(aRequest.FRequest,aRequest.FResponse);
    aRequest.FRequest:=Nil;
    aRequest.FResponse:=Nil;
    aRequest.Free;
  except
    On E: Exception do
      HandleRequestError(Self,E);
  end;
end;

function TMicroHTTPHandler.DoRequest(connection: PMHD_Connection; Const aUrl,aMethod,aVersion: String; Data: PAnsiChar; var DataSize: Size_t) : TRequestHandler;

begin
  Result:=TRequestHandler.Create(Self,Connection);
  Result.Initialize(aURl,aMethod,AVersion);
  if (DataSize>0) then
    if Result.ContinueRequest(Data,Datasize)<>MHD_YES then
      FreeAndNil(Result);
end;

procedure TMicroHTTPHandler.SetExtraHeaders(AValue: TStrings);
begin
  if FExtraHeaders=AValue then Exit;
  FExtraHeaders.Assign(AValue);
end;

procedure TMicroHTTPHandler.HandleRequestError(Sender: TObject; E: Exception);
begin
  Try
    If Assigned(FOnRequestError) then
      FOnRequestError(Sender,E)
    else
      Log(etError,Format('Error (%s) handling request : %s',[E.ClassName,E.Message]));
  except
    // Do not let errors escape
  end;
end;

procedure TMicroHTTPHandler.CheckInactive;

begin
  if Assigned(FServer) then
    Raise EHTTP.Create(SErrServerActive);
end;

procedure TMicroHTTPHandler.SetHostName(AValue: String);
begin
  CheckInactive;
  FHostName:=aValue;
end;


procedure TMicroHTTPHandler.SetOptions(AValue: TMicroServerOptions);
begin
  if FOptions=AValue then Exit;
  CheckInactive;
  FOptions:=AValue;
end;

procedure TMicroHTTPHandler.SetPort(const AValue: Word);
begin
  CheckInactive;
  FPort:=Avalue
end;

procedure TMicroHTTPHandler.InitRequest(ARequest: TRequest);
begin
  inherited InitRequest(ARequest);
end;

procedure TMicroHTTPHandler.InitResponse(AResponse: TResponse);
begin
  inherited InitResponse(AResponse);
end;

function TMicroHTTPHandler.WaitForRequest(out ARequest: TRequest;
  out AResponse: TResponse): boolean;
begin
  Result:=False;
  ARequest:=Nil;
  AResponse:=Nil;
end;

Function TMicroHTTPHandler.DoAcceptConnection(Addr : PSockAddr; addrLen : socklen_t) : Boolean;

begin
  Result:=True;
  if Assigned(FAcceptHandler) then
    FAcceptHandler(Self,Addr,addrLen,Result);
end;


function TMicroHTTPHandler.OptionsToFlags : Integer;

Var
  O : TMicroServerOption;

begin
  Result:=0;
  For O in TMicroServerOption do
    if O in Options then
      Result:=Result or OptionFlags[O];
end;


function TMicroHTTPHandler.CreateServer: PMHD_Daemon;

Var
  F,P : Integer;

begin
  F:=OptionsToFlags;
  P:=Port;
  Result:= MHD_start_daemon(F,P,
    @AcceptCallBack, Self,
    @DoMHDRequest, Self,
    MHD_OPTION_NOTIFY_COMPLETED, @HandleRequestCompleted,
    Nil,MHD_OPTION_END);
end;

procedure TMicroHTTPHandler.Run;
begin
  FServer:=CreateServer;
  if (FServer=Nil) then
    Raise EHTTP.Create(SErrFailedToStartServer);
  Repeat
    Sleep(50);
  Until Terminated;
end;

procedure TMicroHTTPHandler.MaybeStopServer;

begin
  if Assigned(FServer) then
    begin
    MHD_stop_daemon(FServer);
    FServer:=Nil;
    end;
end;

constructor TMicroHTTPHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExtraHeaders:=TStringList.Create;
  Options:=[mcoSelectInternally];
  Port:=80;
end;

destructor TMicroHTTPHandler.Destroy;
begin
  MaybeStopServer;
  FreeAndNil(FExtraHeaders);
  inherited Destroy;
end;


{ ---------------------------------------------------------------------
  TCustomMicroHTTPApplication
  ---------------------------------------------------------------------}


procedure TCustomMicroHTTPApplication.SetHostName(AValue: String);
begin
  HTTPHandler.HostName:=aValue;
end;

procedure TCustomMicroHTTPApplication.SetOptions(AValue: TMicroServerOptions);
begin
  HTTPHandler.Options:=aValue;
end;

procedure TCustomMicroHTTPApplication.SetPort(AValue: Word);
begin
  HTTPHandler.Port:=aValue;
end;

procedure TCustomMicroHTTPApplication.SetUSeSSL(AValue: Boolean);
begin
  if AValue then
    Options:=Options+[mcoSSL]
  else
    Options:=Options-[mcoSSL]
end;

function TCustomMicroHTTPApplication.GetPort: Word;
begin
  Result:=HTTPHandler.Port;
end;

function TCustomMicroHTTPApplication.GetUseSSL: Boolean;
begin
  Result:=mcoSSL in Options;
end;

procedure TCustomMicroHTTPApplication.SetExtraHeaders(AValue: TStrings);
begin
  HTTPHandler.ExtraHeaders.Assign(AValue);
end;

function TCustomMicroHTTPApplication.InitializeWebHandler: TWebHandler;
begin
  Result:=TMicroHTTPHandler.Create(Self);
end;

function TCustomMicroHTTPApplication.HTTPHandler: TMicroHTTPHandler;
begin
  Result:=Webhandler as TMicroHTTPHandler;
end;

constructor TCustomMicroHTTPApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  MHD_set_panic_func(@DoPanic,Self);
end;

destructor TCustomMicroHTTPApplication.Destroy;
begin
  MHD_set_panic_func(@DoPanic,Nil);
  inherited Destroy;
end;

function TCustomMicroHTTPApplication.GetExtraHeaders: TStrings;
begin
  Result:=HTTPHandler.ExtraHeaders;
end;

function TCustomMicroHTTPApplication.GetHostName: String;
begin
  Result:=HTTPHandler.HostName;
end;

function TCustomMicroHTTPApplication.GetOptions: TMicroServerOptions;
begin
  Result:=HTTPHandler.Options;
end;


end.
