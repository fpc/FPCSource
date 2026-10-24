{
    This file is part of the Free Component Library (FCL).
    macOS Network.framework client TLS support for ssockets.
    See COPYING.FPC for details about the copyright.

    Network.framework owns the TCP connection, so this handler establishes
    the connection itself (see TSocketHandler.HandlesConnect). It is a client
    only and needs macOS 10.14 or later. Like Network.framework, it does not
    distinguish a TLS close_notify from a plain TCP close: both end the stream.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit networkframeworksslsockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{$modeswitch cblocks}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Net.Ssockets, System.Net.Sslsockets, System.Net.Sslbase;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, ssockets, sslsockets, sslbase;
{$ENDIF FPC_DOTTEDUNITS}

type
  TNetworkFrameworkSocketHandler = class(TSSLSocketHandler)
  private
    FClient : TObject;
    FErrorCode : Integer;
    FErrorText : String;
    function Fail(const aText: String; aCode: Integer = -1): Boolean;
    function FailFromClient: Boolean;
    procedure CloseClient;
  protected
    function GetLastSSLErrorString: String; override;
    function GetLastSSLErrorCode: Integer; override;
  public
    destructor Destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function HandlesConnect: Boolean; override;
    function Connect: Boolean; override;
    function Accept: Boolean; override;
    function Close: Boolean; override;
    function Shutdown(BiDirectional: Boolean): Boolean; override;
    function Select(aCheck: TSocketStates; TimeOut: Integer): TSocketStates; override;
    function Recv(const Buffer; Count: Integer): Integer; override;
    function Send(const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{$linkframework Network}
{$linkframework Security}

{ Network.framework, Security.framework and libdispatch declarations. }

type
  TSecVerifyComplete = reference to procedure(Verified: Boolean); cdecl; cblock;
  TSecVerifyBlock = reference to procedure(Metadata, Trust: Pointer; Complete: TSecVerifyComplete); cdecl; cblock;
  TNWConfigureBlock = reference to procedure(Options: Pointer); cdecl; cblock;
  TNWStateBlock = reference to procedure(State: cint; Error: Pointer); cdecl; cblock;
  TNWReceiveBlock = reference to procedure(Data, Context: Pointer; IsComplete: Boolean; Error: Pointer); cdecl; cblock;
  TNWSendBlock = reference to procedure(Error: Pointer); cdecl; cblock;
  TDispatchBlock = reference to procedure; cdecl; cblock;

const
  nw_connection_state_waiting = 1;
  nw_connection_state_ready = 3;
  nw_connection_state_failed = 4;
  nw_connection_state_cancelled = 5;
  DISPATCH_TIME_NOW = 0;
  DISPATCH_TIME_FOREVER = High(cuint64);
  NSEC_PER_MSEC = 1000000;

function nw_parameters_create_secure_tcp(configure_tls, configure_tcp: TNWConfigureBlock): Pointer; cdecl; external;
function nw_tls_copy_sec_protocol_options(options: Pointer): Pointer; cdecl; external;
function nw_endpoint_create_host(hostname, port: PAnsiChar): Pointer; cdecl; external;
function nw_connection_create(endpoint, parameters: Pointer): Pointer; cdecl; external;
procedure nw_connection_set_queue(connection, queue: Pointer); cdecl; external;
procedure nw_connection_set_state_changed_handler(connection: Pointer; handler: TNWStateBlock); cdecl; external;
procedure nw_connection_start(connection: Pointer); cdecl; external;
procedure nw_connection_cancel(connection: Pointer); cdecl; external;
procedure nw_connection_receive(connection: Pointer; minimum_incomplete_length, maximum_length: cuint32; completion: TNWReceiveBlock); cdecl; external;
procedure nw_connection_send(connection, content, context: Pointer; is_complete: Boolean; completion: TNWSendBlock); cdecl; external;
function nw_content_context_create(identifier: PAnsiChar): Pointer; cdecl; external;
procedure nw_content_context_set_is_final(context: Pointer; is_final: Boolean); cdecl; external;
function nw_content_context_get_is_final(context: Pointer): Boolean; cdecl; external;
function nw_error_get_error_domain(error: Pointer): cint; cdecl; external;
function nw_error_get_error_code(error: Pointer): cint; cdecl; external;
procedure nw_release(obj: Pointer); cdecl; external;
procedure sec_protocol_options_set_verify_block(options: Pointer; verify_block: TSecVerifyBlock; verify_block_queue: Pointer); cdecl; external;
procedure sec_release(obj: Pointer); cdecl; external;
function dispatch_queue_create(label_: PAnsiChar; attr: Pointer): Pointer; cdecl; external;
procedure dispatch_release(obj: Pointer); cdecl; external;
function dispatch_semaphore_create(value: clong): Pointer; cdecl; external;
function dispatch_semaphore_signal(dsema: Pointer): clong; cdecl; external;
function dispatch_semaphore_wait(dsema: Pointer; timeout: cuint64): clong; cdecl; external;
function dispatch_time(when: cuint64; delta: cint64): cuint64; cdecl; external;
function dispatch_data_create(buffer: Pointer; size: csize_t; queue: Pointer; destructor_: TDispatchBlock): Pointer; cdecl; external;
function dispatch_data_create_map(data: Pointer; buffer_ptr: PPointer; size_ptr: pcsize_t): Pointer; cdecl; external;

resourcestring
  SErrNoCertificateGeneration = 'Network.framework TLS does not generate certificates';
  SErrClientOnly = 'Network.framework TLS supports client connections only';
  SErrNotInetSocket = 'Network.framework TLS requires a TInetSocket';
  SErrUnsupported = 'Network.framework TLS does not support custom credentials, trust, ciphers, ALPN, protocol selection or disabling SNI';
  SErrNative = 'Network.framework %s failed (domain %d, code %d)';
  SErrTimeout = 'Network.framework %s timed out';
  SErrNotConnected = 'Network.framework TLS is not connected';
  SErrRejected = 'Certificate rejected by OnVerifyCertificate';

const
  ReceiveSize = 16384;
  // How long closing waits for the outstanding callbacks.
  CancelTimeout = 5000;

type
  TNetworkFrameworkCertificate = class(TX509Certificate)
  public
    function CreateCertificateAndKey: TCertAndKey; override;
  end;

  { One nw_connection. Network.framework calls back on a private serial queue.
    At most one receive and one send are outstanding; while one is, its
    callback owns the matching fields and signals its semaphore when done.
    The semaphore also orders memory, so no lock is needed. The callbacks
    only copy bytes and integers, so they need no Pascal thread setup. }
  TNWClient = class
  private
    FConnection : Pointer;
    FQueue : Pointer;
    FStateSignal : Pointer;
    FReceiveSignal : Pointer;
    FSendSignal : Pointer;
    FVerify : Boolean;
    // Written by the state callback.
    FState : cint;
    FStateFailed : Boolean;
    FStateDomain, FStateCode : cint;
    // Owned by the receive callback while FReceivePending.
    FReceivePending : Boolean;
    FData : array[0..ReceiveSize-1] of Byte;
    FDataStart, FDataEnd : Integer;
    FEOF : Boolean;
    FReceiveFailed : Boolean;
    FReceiveDomain, FReceiveCode : cint;
    // Owned by the send callback while FSendPending.
    FSendPending : Boolean;
    FSendFailed : Boolean;
    FSendDomain, FSendCode : cint;
    procedure ConfigureTLS(Options: Pointer); cdecl;
    procedure StateChanged(State: cint; Error: Pointer); cdecl;
    procedure Received(Data, Context: Pointer; IsComplete: Boolean; Error: Pointer); cdecl;
    procedure Sent(Error: Pointer); cdecl;
    procedure SetError(const aOperation: String; aDomain, aCode: cint);
    procedure StartReceive;
    function FinishReceive(aTimeout: Integer): Boolean;
    function FinishSend(aTimeout: Integer): Boolean;
  public
    // Set on the caller's thread only.
    Failed : Boolean;
    ErrorText : String;
    ErrorCode : Integer;
    constructor Create;
    destructor Destroy; override;
    function Connect(const aHost: String; aPort: Word; aVerify: Boolean; aTimeout: Integer): Boolean;
    function Read(aBuffer: PByte; aCount, aTimeout: Integer): Integer;
    function Write(aBuffer: PByte; aCount, aTimeout: Integer): Integer;
    function Readable(aTimeout: Integer): Boolean;
    function Available: Integer;
    // Cancel and wait for the outstanding callbacks; False if they did not arrive.
    function Cancel: Boolean;
  end;

{ Waits for one signal; aTimeout in milliseconds, negative waits forever. }
function WaitSignal(aSignal: Pointer; aTimeout: Integer): Boolean;
var
  T : cuint64;
begin
  if aTimeout<0 then
    T:=DISPATCH_TIME_FOREVER
  else
    T:=dispatch_time(DISPATCH_TIME_NOW,cint64(aTimeout)*NSEC_PER_MSEC);
  Result:=dispatch_semaphore_wait(aSignal,T)=0;
end;

{ Remaining time until aDeadline (GetTickCount64), negative if aDeadline is 0 (no limit). }
function Remaining(aDeadline: QWord): Integer;
var
  Now : QWord;
begin
  if aDeadline=0 then
    Exit(-1);
  Now:=GetTickCount64;
  if Now>=aDeadline then
    Result:=0
  else
    Result:=aDeadline-Now;
end;

{ ssockets timeouts: 0 means no limit. }
function TimeoutOf(aValue: Integer): Integer;
begin
  if aValue>0 then
    Result:=aValue
  else
    Result:=-1;
end;

procedure ConfigureTCP(Options: Pointer); cdecl;
begin
  // Keep the default TCP options.
end;

// Verify block used when VerifyPeerCert is False.
procedure AcceptAnyPeer(Metadata, Trust: Pointer; Complete: TSecVerifyComplete); cdecl;
begin
  Complete(True);
end;

function TNetworkFrameworkCertificate.CreateCertificateAndKey: TCertAndKey;
begin
  Raise ESSLSocketError.Create(SErrNoCertificateGeneration);
end;

{ TNWClient }

constructor TNWClient.Create;
begin
  inherited Create;
  FQueue:=dispatch_queue_create('org.freepascal.networkframework',Nil);
  FStateSignal:=dispatch_semaphore_create(0);
  FReceiveSignal:=dispatch_semaphore_create(0);
  FSendSignal:=dispatch_semaphore_create(0);
end;

destructor TNWClient.Destroy;
begin
  dispatch_release(FSendSignal);
  dispatch_release(FReceiveSignal);
  dispatch_release(FStateSignal);
  dispatch_release(FQueue);
  inherited Destroy;
end;

procedure TNWClient.SetError(const aOperation: String; aDomain, aCode: cint);
begin
  Failed:=True;
  ErrorCode:=aCode;
  ErrorText:=Format(SErrNative,[aOperation,aDomain,aCode]);
end;

procedure TNWClient.ConfigureTLS(Options: Pointer); cdecl;
var
  Security : Pointer;
  Verify : TSecVerifyBlock;
begin
  // With verification, the system trust and host name checks apply.
  if FVerify then
    Exit;
  Security:=nw_tls_copy_sec_protocol_options(Options);
  Verify:=@AcceptAnyPeer;
  sec_protocol_options_set_verify_block(Security,Verify,FQueue);
  sec_release(Security);
end;

procedure TNWClient.StateChanged(State: cint; Error: Pointer); cdecl;
begin
  if Error<>Nil then
    begin
    FStateDomain:=nw_error_get_error_domain(Error);
    FStateCode:=nw_error_get_error_code(Error);
    FStateFailed:=True;
    end;
  FState:=State;
  dispatch_semaphore_signal(FStateSignal);
end;

procedure TNWClient.Received(Data, Context: Pointer; IsComplete: Boolean; Error: Pointer); cdecl;
var
  Map, P : Pointer;
  Size : csize_t;
begin
  FDataStart:=0;
  FDataEnd:=0;
  if Data<>Nil then
    begin
    Map:=dispatch_data_create_map(Data,@P,@Size);
    if Map=Nil then
      begin
      FReceiveFailed:=True;
      FReceiveDomain:=1; // POSIX
      FReceiveCode:=12;  // ENOMEM
      end
    else
      begin
      if Size>ReceiveSize then
        Size:=ReceiveSize;
      Move(P^,FData[0],Size);
      FDataEnd:=Size;
      dispatch_release(Map);
      end;
    end;
  if IsComplete and ((Context=Nil) or nw_content_context_get_is_final(Context)) then
    FEOF:=True;
  if Error<>Nil then
    begin
    FReceiveFailed:=True;
    FReceiveDomain:=nw_error_get_error_domain(Error);
    FReceiveCode:=nw_error_get_error_code(Error);
    end;
  dispatch_semaphore_signal(FReceiveSignal);
end;

procedure TNWClient.Sent(Error: Pointer); cdecl;
begin
  if Error<>Nil then
    begin
    FSendFailed:=True;
    FSendDomain:=nw_error_get_error_domain(Error);
    FSendCode:=nw_error_get_error_code(Error);
    end;
  dispatch_semaphore_signal(FSendSignal);
end;

function TNWClient.Connect(const aHost: String; aPort: Word; aVerify: Boolean; aTimeout: Integer): Boolean;
var
  TLS, TCP : TNWConfigureBlock;
  StateBlock : TNWStateBlock;
  Parameters, Endpoint : Pointer;
  Port : AnsiString;
  Deadline : QWord;
begin
  Result:=False;
  FVerify:=aVerify;
  TLS:=@ConfigureTLS;
  TCP:=@ConfigureTCP;
  Parameters:=nw_parameters_create_secure_tcp(TLS,TCP);
  Port:=IntToStr(aPort);
  Endpoint:=nw_endpoint_create_host(PAnsiChar(AnsiString(aHost)),PAnsiChar(Port));
  if (Parameters<>Nil) and (Endpoint<>Nil) then
    FConnection:=nw_connection_create(Endpoint,Parameters);
  if Endpoint<>Nil then
    nw_release(Endpoint);
  if Parameters<>Nil then
    nw_release(Parameters);
  if FConnection=Nil then
    begin
    SetError('connection setup',1,22); // POSIX EINVAL
    Exit;
    end;
  nw_connection_set_queue(FConnection,FQueue);
  StateBlock:=@StateChanged;
  nw_connection_set_state_changed_handler(FConnection,StateBlock);
  nw_connection_start(FConnection);
  if aTimeout<0 then
    Deadline:=0
  else
    Deadline:=GetTickCount64+QWord(aTimeout);
  repeat
    case FState of
      nw_connection_state_ready:
        Exit(True);
      nw_connection_state_failed,
      nw_connection_state_cancelled:
        Break;
    end;
    // Waiting with an error: Network.framework would retry, report it now.
    if FStateFailed then
      Break;
    if not WaitSignal(FStateSignal,Remaining(Deadline)) then
      begin
      Failed:=True;
      ErrorText:=Format(SErrTimeout,['connect']);
      Exit;
      end;
  until False;
  SetError('connect',FStateDomain,FStateCode);
end;

procedure TNWClient.StartReceive;
var
  Block : TNWReceiveBlock;
begin
  if FReceivePending or FEOF or Failed or (FDataStart<FDataEnd) then
    Exit;
  Block:=@Received;
  FReceivePending:=True;
  nw_connection_receive(FConnection,1,ReceiveSize,Block);
end;

// Wait for the outstanding receive. False on timeout; check Failed for errors.
function TNWClient.FinishReceive(aTimeout: Integer): Boolean;
begin
  if not FReceivePending then
    Exit(True);
  if not WaitSignal(FReceiveSignal,aTimeout) then
    Exit(False);
  FReceivePending:=False;
  if FReceiveFailed then
    SetError('receive',FReceiveDomain,FReceiveCode);
  Result:=True;
end;

function TNWClient.FinishSend(aTimeout: Integer): Boolean;
begin
  if not FSendPending then
    Exit(True);
  if not WaitSignal(FSendSignal,aTimeout) then
    Exit(False);
  FSendPending:=False;
  if FSendFailed then
    SetError('send',FSendDomain,FSendCode);
  Result:=True;
end;

function TNWClient.Read(aBuffer: PByte; aCount, aTimeout: Integer): Integer;
var
  Deadline : QWord;
begin
  if aTimeout<0 then
    Deadline:=0
  else
    Deadline:=GetTickCount64+QWord(aTimeout);
  // A receive may complete without data, so loop until data, EOF or error.
  while (FDataStart=FDataEnd) and not FEOF and not Failed do
    begin
    StartReceive;
    if not FinishReceive(Remaining(Deadline)) then
      begin
      ErrorText:=Format(SErrTimeout,['receive']);
      Exit(-1);
      end;
    end;
  if FDataStart<FDataEnd then
    begin
    Result:=FDataEnd-FDataStart;
    if Result>aCount then
      Result:=aCount;
    Move(FData[FDataStart],aBuffer^,Result);
    Inc(FDataStart,Result);
    end
  else if Failed then
    Result:=-1
  else
    Result:=0;
end;

function TNWClient.Write(aBuffer: PByte; aCount, aTimeout: Integer): Integer;
var
  Data, Context : Pointer;
  Block : TNWSendBlock;
begin
  // A previous send which timed out must complete first.
  if not FinishSend(aTimeout) then
    begin
    ErrorText:=Format(SErrTimeout,['send']);
    Exit(-1);
    end;
  if Failed then
    Exit(-1);
  if aCount<=0 then
    Exit(0);
  // The default destructor makes dispatch copy the bytes.
  Data:=dispatch_data_create(aBuffer,aCount,FQueue,Nil);
  Context:=nw_content_context_create('org.freepascal.send');
  nw_content_context_set_is_final(Context,False);
  Block:=@Sent;
  FSendFailed:=False;
  FSendPending:=True;
  nw_connection_send(FConnection,Data,Context,True,Block);
  nw_release(Context);
  dispatch_release(Data);
  if not FinishSend(aTimeout) then
    begin
    ErrorText:=Format(SErrTimeout,['send']);
    Exit(-1);
    end;
  if Failed then
    Result:=-1
  else
    Result:=aCount;
end;

function TNWClient.Readable(aTimeout: Integer): Boolean;
begin
  if (FDataStart=FDataEnd) and not FEOF and not Failed then
    begin
    StartReceive;
    FinishReceive(aTimeout);
    end;
  Result:=(FDataStart<FDataEnd) or FEOF or Failed;
end;

function TNWClient.Available: Integer;
begin
  Result:=FDataEnd-FDataStart;
end;

function TNWClient.Cancel: Boolean;
begin
  if FConnection=Nil then
    Exit(True);
  nw_connection_cancel(FConnection);
  // The outstanding callbacks reference this object.
  FinishReceive(CancelTimeout);
  FinishSend(CancelTimeout);
  while (FState<>nw_connection_state_cancelled) and WaitSignal(FStateSignal,CancelTimeout) do ;
  Result:=not FReceivePending and not FSendPending and (FState=nw_connection_state_cancelled);
  if Result then
    begin
    nw_release(FConnection);
    FConnection:=Nil;
    end;
end;

{ TNetworkFrameworkSocketHandler }

destructor TNetworkFrameworkSocketHandler.Destroy;
begin
  CloseClient;
  inherited Destroy;
end;

function TNetworkFrameworkSocketHandler.Fail(const aText: String; aCode: Integer): Boolean;
begin
  FErrorText:=aText;
  FErrorCode:=aCode;
  FLastError:=aCode;
  Result:=False;
end;

function TNetworkFrameworkSocketHandler.FailFromClient: Boolean;
begin
  Result:=Fail(TNWClient(FClient).ErrorText,TNWClient(FClient).ErrorCode);
end;

procedure TNetworkFrameworkSocketHandler.CloseClient;
begin
  SetSSLActive(False);
  if FClient=Nil then
    Exit;
  // If callbacks are still outstanding, keep the object alive for them.
  if TNWClient(FClient).Cancel then
    FClient.Free;
  FClient:=Nil;
end;

function TNetworkFrameworkSocketHandler.GetLastSSLErrorString: String;
begin
  Result:=FErrorText;
end;

function TNetworkFrameworkSocketHandler.GetLastSSLErrorCode: Integer;
begin
  Result:=FErrorCode;
end;

function TNetworkFrameworkSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TNetworkFrameworkCertificate.Create;
end;

function TNetworkFrameworkSocketHandler.HandlesConnect: Boolean;
begin
  Result:=True;
end;

function TNetworkFrameworkSocketHandler.Connect: Boolean;
var
  S : TInetSocket;
begin
  CloseClient;
  FErrorText:='';
  FErrorCode:=0;
  if not (Socket is TInetSocket) then
    Exit(Fail(SErrNotInetSocket));
  if not CertificateData.Certificate.Empty or not CertificateData.PrivateKey.Empty or
     not CertificateData.CertCA.Empty or not CertificateData.TrustedCertificate.Empty or
     not CertificateData.PFX.Empty or (CertificateData.TrustedCertsDir<>'') or
     (CertificateData.ALPNProtocols<>'') or (CertificateData.CipherList<>'DEFAULT') or
     (SSLType<>stAny) or not SendHostAsSNI then
    Exit(Fail(SErrUnsupported));
  S:=TInetSocket(Socket);
  FClient:=TNWClient.Create;
  if not TNWClient(FClient).Connect(S.NetworkAddress.Address,S.Port,VerifyPeerCert,TimeoutOf(S.ConnectTimeout)) then
    begin
    Result:=FailFromClient;
    CloseClient;
    Exit;
    end;
  SetSSLActive(True);
  if not DoVerifyCert then
    begin
    CloseClient;
    Exit(Fail(SErrRejected));
    end;
  Result:=True;
end;

function TNetworkFrameworkSocketHandler.Accept: Boolean;
begin
  Result:=Fail(SErrClientOnly);
end;

function TNetworkFrameworkSocketHandler.Close: Boolean;
begin
  CloseClient;
  Result:=True;
end;

function TNetworkFrameworkSocketHandler.Shutdown(BiDirectional: Boolean): Boolean;
begin
  Result:=Close;
end;

function TNetworkFrameworkSocketHandler.Select(aCheck: TSocketStates; TimeOut: Integer): TSocketStates;
begin
  Result:=[];
  if FClient=Nil then
    Exit;
  if (sosCanWrite in aCheck) and SSLActive then
    Include(Result,sosCanWrite);
  if (sosCanRead in aCheck) and TNWClient(FClient).Readable(TimeOut) then
    Include(Result,sosCanRead);
end;

function TNetworkFrameworkSocketHandler.Recv(const Buffer; Count: Integer): Integer;
begin
  if FClient=Nil then
    begin
    Fail(SErrNotConnected);
    Exit(-1);
    end;
  Result:=TNWClient(FClient).Read(PByte(@Buffer),Count,TimeoutOf(Socket.IOTimeout));
  if Result<0 then
    FailFromClient;
end;

function TNetworkFrameworkSocketHandler.Send(const Buffer; Count: Integer): Integer;
begin
  if FClient=Nil then
    begin
    Fail(SErrNotConnected);
    Exit(-1);
    end;
  Result:=TNWClient(FClient).Write(PByte(@Buffer),Count,TimeoutOf(Socket.IOTimeout));
  if Result<0 then
    FailFromClient;
end;

function TNetworkFrameworkSocketHandler.BytesAvailable: Integer;
begin
  if FClient=Nil then
    Result:=0
  else
    Result:=TNWClient(FClient).Available;
end;

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TNetworkFrameworkSocketHandler);
end.
