{
    This file is part of the Free Component Library (FCL).
    Windows Schannel client TLS support for ssockets.
    See COPYING.FPC for details about the copyright.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit schannelsslsockets;
{$ENDIF}
{$mode objfpc}{$H+}
interface
{$IFDEF FPC_DOTTEDUNITS}
uses System.Classes, System.SysUtils, WinApi.Windows, WinApi.WinSock2,
  System.Net.Ssockets, System.Net.Sslsockets, System.Net.Sslbase, WinApi.SchannelSSPI;
{$ELSE}
uses Classes, SysUtils, Windows, WinSock2, ssockets, sslsockets, sslbase, schannelsspi;
{$ENDIF}
type
  TSchannelSocketHandler = class(TSSLSocketHandler)
  private
    FCredential, FContext: TSecHandle;
    FHaveCredential, FHaveContext, FPeerShutdown, FFailed: Boolean;
    FSizes: TSecStreamSizes;
    FEncrypted, FPlain: TBytes;
    FPlainOffset: SizeInt;
    FTarget: UnicodeString;
    FServerName: String;
    FErrorCode: Integer;
    FErrorText: String;
    function Fail(Code: Integer; const Operation: String): Boolean;
    function Deadline(Timeout: Integer): QWord;
    function WaitSocket(Writing: Boolean; UntilTime: QWord): Boolean;
    function SendRaw(Data: Pointer; Count: Integer; UntilTime: QWord): Boolean;
    function ReadMore(UntilTime: QWord): Boolean;
    function Handshake(UntilTime: QWord; Initial: Boolean): Boolean;
    procedure KeepExtra(const Buffers: array of TSecBuffer);
    procedure ReleaseContext;
    function RecvUntil(const Buffer; Count: Integer; UntilTime: QWord): Integer;
  protected
    function GetLastSSLErrorString: String; override;
    function GetLastSSLErrorCode: Integer; override;
  public
    destructor Destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function Connect: Boolean; override;
    function Accept: Boolean; override;
    function Close: Boolean; override;
    function Shutdown(BiDirectional: Boolean): Boolean; override;
    function Send(const Buffer; Count: Integer): Integer; override;
    function Recv(const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    function Select(Check: TSocketStates; Timeout: Integer): TSocketStates; override;
    // Optional authenticated peer name when TCP connects to an IP or a tunnel.
    // An empty value authenticates TInetSocket.NetworkAddress.Address.
    property ServerName: String read FServerName write FServerName;
  end;
implementation

type
  { Keep the existing SSL base class's certificate-generator contract without
    loading a cryptographic library for this client-only handler. }
  TSchannelCertificate = class(TX509Certificate)
  public
    function CreateCertificateAndKey: TCertAndKey; override;
  end;

resourcestring
  SErrSchannelCertificateGeneration = 'Schannel client does not generate certificates';

function TSchannelCertificate.CreateCertificateAndKey: TCertAndKey;
begin
  raise ESSLSocketError.Create(SErrSchannelCertificateGeneration);
end;

const
  // A full TLS record plus handshake fragmentation, bounded against hostile input.
  MaxEncryptedBuffer = 262144;
  ContextFlags = ISC_REQ_REPLAY_DETECT or ISC_REQ_SEQUENCE_DETECT or
    ISC_REQ_CONFIDENTIALITY or ISC_REQ_ALLOCATE_MEMORY or
    ISC_REQ_EXTENDED_ERROR or ISC_REQ_STREAM;

procedure BufferDesc(out Desc: TSecBufferDesc; var Buffers: array of TSecBuffer);
begin
  FillChar(Desc,SizeOf(Desc),0);
  Desc.Count:=Length(Buffers);
  Desc.Buffers:=@Buffers[0];
end;

function TSchannelSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TSchannelCertificate.Create;
end;

destructor TSchannelSocketHandler.Destroy;
begin
  ReleaseContext;
  inherited Destroy;
end;

procedure TSchannelSocketHandler.ReleaseContext;
begin
  SetSSLActive(False);
  if FHaveContext then DeleteSecurityContext(@FContext);
  if FHaveCredential then FreeCredentialsHandle(@FCredential);
  FHaveContext:=False;
  FHaveCredential:=False;
  FillChar(FContext,SizeOf(FContext),0);
  FillChar(FCredential,SizeOf(FCredential),0);
  FEncrypted:=nil;
  FPlain:=nil;
  FPlainOffset:=0;
end;

function TSchannelSocketHandler.Fail(Code: Integer; const Operation: String): Boolean;
begin
  FErrorCode:=Code;
  FErrorText:=Operation+' (0x'+IntToHex(Cardinal(Code),8)+')';
  FLastError:=Code;
  FFailed:=True;
  SetSSLActive(False);
  Result:=False;
end;

function TSchannelSocketHandler.GetLastSSLErrorString: String;
begin Result:=FErrorText end;
function TSchannelSocketHandler.GetLastSSLErrorCode: Integer;
begin Result:=FErrorCode end;

function TSchannelSocketHandler.Deadline(Timeout: Integer): QWord;
begin
  if Timeout>0 then Result:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetTickCount64+QWord(Timeout) else Result:=0;
end;

function TSchannelSocketHandler.WaitSocket(Writing: Boolean; UntilTime: QWord): Boolean;
var
  FDS: TFDSet;
  TV: TTimeVal;
  PTV: PTimeVal;
  Remaining, NowTime: QWord;
  R: Integer;
begin
  FillChar(FDS,SizeOf(FDS),0);
  FD_Set(Socket.Handle,FDS);
  PTV:=nil;
  if UntilTime<>0 then
    begin
    NowTime:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetTickCount64;
    if NowTime>=UntilTime then Exit(Fail(WSAETIMEDOUT,'TLS I/O timeout'));
    Remaining:=UntilTime-NowTime;
    TV.tv_sec:=Remaining div 1000;
    TV.tv_usec:=(Remaining mod 1000)*1000;
    PTV:=@TV;
    end;
  if Writing then R:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}WinSock2.Select(0,nil,@FDS,nil,PTV)
  else R:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}WinSock2.Select(0,@FDS,nil,nil,PTV);
  if R=0 then Exit(Fail(WSAETIMEDOUT,'TLS I/O timeout'));
  if R<0 then Exit(Fail(WSAGetLastError,'TLS socket select'));
  Result:=True;
end;

function TSchannelSocketHandler.SendRaw(Data: Pointer; Count: Integer; UntilTime: QWord): Boolean;
var N, Error: Integer;
begin
  while Count>0 do
    begin
    if not WaitSocket(True,UntilTime) then Exit(False);
    N:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}WinSock2.Send(Socket.Handle,Data^,Count,0);
    if N=SOCKET_ERROR then
      begin
      Error:=WSAGetLastError;
      if Error=WSAEWOULDBLOCK then Continue;
      Exit(Fail(Error,'TLS socket send'));
      end;
    if N=0 then Exit(Fail(WSAECONNRESET,'TLS socket send returned zero'));
    Inc(PByte(Data),N);
    Dec(Count,N);
    end;
  Result:=True;
end;

function TSchannelSocketHandler.ReadMore(UntilTime: QWord): Boolean;
var
  Chunk: array[0..16383] of Byte;
  N, OldSize, Error: Integer;
begin
  repeat
  if not WaitSocket(False,UntilTime) then Exit(False);
  N:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}WinSock2.Recv(Socket.Handle,Chunk,SizeOf(Chunk),0);
  if N=SOCKET_ERROR then
    begin
    Error:=WSAGetLastError;
    if Error<>WSAEWOULDBLOCK then Exit(Fail(Error,'TLS socket receive'));
    end;
  until N<>SOCKET_ERROR;
  // A TCP EOF without TLS close_notify is truncation, never a clean TLS EOF.
  if N=0 then Exit(Fail(SEC_E_ILLEGAL_MESSAGE,'TLS connection truncated without close_notify'));
  OldSize:=Length(FEncrypted);
  if OldSize+N>MaxEncryptedBuffer then Exit(Fail(SEC_E_ILLEGAL_MESSAGE,'TLS input buffer limit'));
  SetLength(FEncrypted,OldSize+N);
  Move(Chunk,FEncrypted[OldSize],N);
  Result:=True;
end;

procedure TSchannelSocketHandler.KeepExtra(const Buffers: array of TSecBuffer);
var I, N: Integer;
begin
  N:=0;
  for I:=0 to High(Buffers) do
    if Buffers[I].BufferType=SECBUFFER_EXTRA then N:=Buffers[I].Size;
  if (N>0) and (N<=Length(FEncrypted)) then
    Move(FEncrypted[Length(FEncrypted)-N],FEncrypted[0],N);
  SetLength(FEncrypted,N);
end;

function TSchannelSocketHandler.Handshake(UntilTime: QWord; Initial: Boolean): Boolean;
var
  Input: array[0..1] of TSecBuffer;
  Output: array[0..0] of TSecBuffer;
  ID, OD: TSecBufferDesc;
  IP: PSecBufferDesc;
  CP: PSecHandle;
  Attr: Cardinal;
  Status: TSecurityStatus;
  NeedRead: Boolean;
begin
  // Schannel may consume an entire post-handshake message and return
  // SEC_I_RENEGOTIATE without EXTRA. Re-enter ISC before reading more bytes.
  NeedRead:=False;
  repeat
    if NeedRead and not ReadMore(UntilTime) then Exit(False);
    FillChar(Input,SizeOf(Input),0);
    FillChar(Output,SizeOf(Output),0);
    BufferDesc(ID,Input);
    BufferDesc(OD,Output);
    IP:=nil;
    CP:=nil;
    if not Initial then
      begin
      CP:=@FContext;
      Input[0].BufferType:=SECBUFFER_TOKEN;
      Input[0].Size:=Length(FEncrypted);
      if Length(FEncrypted)>0 then Input[0].Data:=@FEncrypted[0];
      IP:=@ID;
      end;
    Output[0].BufferType:=SECBUFFER_TOKEN;
    Status:=InitializeSecurityContextW(@FCredential,CP,PWideChar(FTarget),
      ContextFlags,0,0,IP,0,@FContext,@OD,@Attr,nil);
    if (Status=SEC_E_OK) or (Status=SEC_I_CONTINUE_NEEDED) then FHaveContext:=True;
    try
      if (Output[0].Size>0) and ((Status=SEC_E_OK) or (Status=SEC_I_CONTINUE_NEEDED)) then
        if not SendRaw(Output[0].Data,Output[0].Size,UntilTime) then Exit(False);
    finally
      if Output[0].Data<>nil then FreeContextBuffer(Output[0].Data);
    end;
    if Status=SEC_E_INCOMPLETE_MESSAGE then
      begin NeedRead:=True; Continue end;
    if (Status<>SEC_E_OK) and (Status<>SEC_I_CONTINUE_NEEDED) then
      Exit(Fail(Status,'Schannel handshake / system certificate validation'));
    if not Initial then KeepExtra(Input);
    Initial:=False;
    NeedRead:=Length(FEncrypted)=0;
  until Status=SEC_E_OK;
  if (Attr and ISC_REQ_CONFIDENTIALITY)=0 then
    Exit(Fail(SEC_E_ILLEGAL_MESSAGE,'Schannel did not negotiate confidentiality'));
  Status:=QueryContextAttributesW(@FContext,SECPKG_ATTR_STREAM_SIZES,@FSizes);
  if Status<>SEC_E_OK then Exit(Fail(Status,'Schannel stream sizes'));
  if (FSizes.MaximumMessage=0) or (FSizes.MaximumMessage>65536) or
     (FSizes.Header+FSizes.Trailer>65536) then
    Exit(Fail(SEC_E_INTERNAL_ERROR,'Invalid Schannel stream sizes'));
  Result:=True;
end;

function TSchannelSocketHandler.Connect: Boolean;
var
  Modern: TSchCredentials;
  Legacy: TSchannelCred;
  Status: TSecurityStatus;
  NonBlocking: Cardinal;
  T: Integer;
begin
  CheckSocket;
  ReleaseContext;
  FFailed:=False;
  FPeerShutdown:=False;
  FErrorCode:=0;
  FLastError:=0;
  FErrorText:='';
  Result:=False;
  if not (Socket is TInetSocket) then
    Exit(Fail(SEC_E_UNSUPPORTED_FUNCTION,'Schannel requires TInetSocket'));
  if not CertificateData.Certificate.Empty or not CertificateData.PrivateKey.Empty or
     not CertificateData.CertCA.Empty or not CertificateData.TrustedCertificate.Empty or
     not CertificateData.PFX.Empty or (CertificateData.TrustedCertsDir<>'') or
     (CertificateData.ALPNProtocols<>'') or (CertificateData.CipherList<>'DEFAULT') or
     not SendHostAsSNI then
    Exit(Fail(SEC_E_UNSUPPORTED_FUNCTION,'Schannel custom credentials, trust, cipher list or ALPN are unsupported'));
  if not (SSLType in [stAny,stTLSv1_2]) then
    Exit(Fail(SEC_E_UNSUPPORTED_FUNCTION,'Schannel legacy protocol selection is unsupported'));
  if FServerName<>'' then FTarget:=UTF8Decode(FServerName)
  else FTarget:=UTF8Decode(TInetSocket(Socket).NetworkAddress.Address);
  if (FTarget='') or (Pos(#0,FTarget)>0) then
    Exit(Fail(SEC_E_ILLEGAL_MESSAGE,'Schannel target hostname is empty or contains NUL'));
  if FTarget[Length(FTarget)]='.' then Delete(FTarget,Length(FTarget),1);
  if FTarget='' then Exit(Fail(SEC_E_ILLEGAL_MESSAGE,'Schannel target hostname is empty'));
  FillChar(Modern,SizeOf(Modern),0);
  Modern.Version:=SCH_CREDENTIALS_VERSION;
  // Honor the existing handler option without changing OS policy or TLS
  // protocols. The application callback remains a post-handshake veto.
  Modern.Flags:=SCH_CRED_NO_DEFAULT_CREDS or SCH_USE_STRONG_CRYPTO;
  if VerifyPeerCert then
    Modern.Flags:=Modern.Flags or SCH_CRED_AUTO_CRED_VALIDATION
  else
    Modern.Flags:=Modern.Flags or SCH_CRED_MANUAL_CRED_VALIDATION or SCH_CRED_NO_SERVERNAME_CHECK;
  Status:=SEC_E_UNSUPPORTED_FUNCTION;
  if SSLType=stAny then
    Status:=AcquireCredentialsHandleW(nil,'Microsoft Unified Security Protocol Provider',
      SECPKG_CRED_OUTBOUND,nil,@Modern,nil,nil,@FCredential,nil);
  // Feature detection occurs before connecting TLS, never after a certificate
  // or handshake failure. V4 permits older systems to use their enabled TLS.
  if (Status=SEC_E_UNSUPPORTED_FUNCTION) or (Status=SEC_E_UNKNOWN_CREDENTIALS) or
     (Status=SEC_E_INVALID_TOKEN) then
    begin
    FillChar(Legacy,SizeOf(Legacy),0);
    Legacy.Version:=SCHANNEL_CRED_VERSION;
    Legacy.Flags:=Modern.Flags;
    if SSLType=stTLSv1_2 then Legacy.EnabledProtocols:=SP_PROT_TLS1_2_CLIENT;
    Status:=AcquireCredentialsHandleW(nil,'Microsoft Unified Security Protocol Provider',
      SECPKG_CRED_OUTBOUND,nil,@Legacy,nil,nil,@FCredential,nil);
    end;
  if Status<>SEC_E_OK then Exit(Fail(Status,'Schannel acquire credentials'));
  FHaveCredential:=True;
  T:=Socket.ConnectTimeout;
  if T<=0 then T:=Socket.IOTimeout;
  NonBlocking:=1;
  if ioctlsocket(Socket.Handle,LongInt(FIONBIO),@NonBlocking)<>0 then
    Exit(Fail(WSAGetLastError,'TLS nonblocking transport'));
  Result:=Handshake(Deadline(T),True);
  if Result then
    begin
    Result:=DoVerifyCert;
    if not Result then Fail(SEC_E_ILLEGAL_MESSAGE,'Certificate rejected by application');
    end;
  SetSSLActive(Result);
  if not Result then ReleaseContext;
end;

function TSchannelSocketHandler.Accept: Boolean;
begin
  Result:=Fail(SEC_E_UNSUPPORTED_FUNCTION,'Schannel socket handler is client-only');
end;

function TSchannelSocketHandler.Send(const Buffer; Count: Integer): Integer;
var
  Data: TBytes;
  Buffers: array[0..3] of TSecBuffer;
  Desc: TSecBufferDesc;
  Status: TSecurityStatus;
  I, N, Total: Integer;
  UntilTime: QWord;
begin
  Result:=-1;
  if not SSLActive or FFailed then Exit;
  if Count<=0 then Exit(0);
  UntilTime:=Deadline(Socket.IOTimeout);
  Total:=0;
  while Total<Count do
    begin
    N:=Count-Total;
    if Cardinal(N)>FSizes.MaximumMessage then N:=FSizes.MaximumMessage;
    SetLength(Data,FSizes.Header+N+FSizes.Trailer);
    Move((PByte(@Buffer)+Total)^,Data[FSizes.Header],N);
    FillChar(Buffers,SizeOf(Buffers),0);
    BufferDesc(Desc,Buffers);
    Buffers[0].BufferType:=SECBUFFER_STREAM_HEADER;
    Buffers[0].Size:=FSizes.Header;
    Buffers[0].Data:=@Data[0];
    Buffers[1].BufferType:=SECBUFFER_DATA;
    Buffers[1].Size:=N;
    Buffers[1].Data:=@Data[FSizes.Header];
    Buffers[2].BufferType:=SECBUFFER_STREAM_TRAILER;
    Buffers[2].Size:=FSizes.Trailer;
    Buffers[2].Data:=@Data[FSizes.Header+N];
    Status:=EncryptMessage(@FContext,0,@Desc,0);
    if Status<>SEC_E_OK then begin Fail(Status,'Schannel encrypt'); Exit end;
    for I:=0 to 2 do
      if not SendRaw(Buffers[I].Data,Buffers[I].Size,UntilTime) then Exit;
    Inc(Total,N);
    end;
  Result:=Total;
end;

function TSchannelSocketHandler.Recv(const Buffer; Count: Integer): Integer;
begin
  CheckSocket;
  Result:=RecvUntil(Buffer,Count,Deadline(Socket.IOTimeout));
end;

function TSchannelSocketHandler.RecvUntil(const Buffer; Count: Integer; UntilTime: QWord): Integer;
var
  Buffers: array[0..3] of TSecBuffer;
  Desc: TSecBufferDesc;
  Status: TSecurityStatus;
  I, N: Integer;
  HasExtra: Boolean;
begin
  Result:=-1;
  if Count<=0 then Exit(0);
  if FPeerShutdown and (BytesAvailable=0) then Exit(0);
  if not SSLActive or FFailed then Exit;
  while BytesAvailable=0 do
    begin
    if (Length(FEncrypted)=0) and not ReadMore(UntilTime) then Exit;
    FillChar(Buffers,SizeOf(Buffers),0);
    BufferDesc(Desc,Buffers);
    Buffers[0].BufferType:=SECBUFFER_DATA;
    Buffers[0].Size:=Length(FEncrypted);
    Buffers[0].Data:=@FEncrypted[0];
    Status:=DecryptMessage(@FContext,@Desc,0,nil);
    if Status=SEC_E_INCOMPLETE_MESSAGE then
      begin if not ReadMore(UntilTime) then Exit; Continue end;
    if (Status<>SEC_E_OK) and (Status<>SEC_I_RENEGOTIATE) and
       (Status<>SEC_I_CONTEXT_EXPIRED) then
      begin Fail(Status,'Schannel decrypt'); Exit end;
    // Control statuses do not authenticate the input DATA buffer as application
    // plaintext. In particular, close_notify can leave that buffer unchanged.
    if Status=SEC_I_CONTEXT_EXPIRED then
      begin
      FEncrypted:=nil;
      FPeerShutdown:=True;
      Exit(0);
      end;
    if Status=SEC_I_RENEGOTIATE then
      begin
      HasExtra:=False;
      for I:=0 to High(Buffers) do
        if Buffers[I].BufferType=SECBUFFER_EXTRA then HasExtra:=True;
      if HasExtra then KeepExtra(Buffers)
      else
        begin
        // DecryptMessage can return a modified token without EXTRA. Pass it to
        // InitializeSecurityContext before trying to receive another record.
        N:=Buffers[0].Size;
        if (N<0) or (N>Length(FEncrypted)) or
           ((N>0) and ((PtrUInt(Buffers[0].Data)<PtrUInt(@FEncrypted[0])) or
            (PtrUInt(Buffers[0].Data)-PtrUInt(@FEncrypted[0])>PtrUInt(Length(FEncrypted)-N)))) then
          begin Fail(SEC_E_INTERNAL_ERROR,'Invalid Schannel handshake token'); Exit end;
        if N>0 then Move(Buffers[0].Data^,FEncrypted[0],N);
        SetLength(FEncrypted,N);
        end;
      if not Handshake(UntilTime,False) then Exit;
      if not DoVerifyCert then
        begin Fail(SEC_E_ILLEGAL_MESSAGE,'Certificate rejected by application'); Exit end;
      Continue;
      end;
    for I:=0 to High(Buffers) do
      if (Buffers[I].BufferType=SECBUFFER_DATA) and (Buffers[I].Size>0) then
        begin
        N:=Length(FPlain);
        SetLength(FPlain,N+Buffers[I].Size);
        Move(Buffers[I].Data^,FPlain[N],Buffers[I].Size);
        end;
    KeepExtra(Buffers);
    end;
  N:=BytesAvailable;
  if N>Count then N:=Count;
  Move(FPlain[FPlainOffset],PByte(@Buffer)^,N);
  Inc(FPlainOffset,N);
  if FPlainOffset=Length(FPlain) then
    begin FPlain:=nil; FPlainOffset:=0 end;
  Result:=N;
end;

function TSchannelSocketHandler.BytesAvailable: Integer;
begin
  Result:=Length(FPlain)-FPlainOffset;
end;

function TSchannelSocketHandler.Select(Check: TSocketStates; Timeout: Integer): TSocketStates;
var
  ReadSet, WriteSet, ErrorSet: TFDSet;
  ReadPtr, WritePtr, ErrorPtr: PFDSet;
  TV: TTimeVal;
  TVPtr: PTimeVal;
  R: Integer;
  procedure Prepare(var FDSet: TFDSet; out Ptr: PFDSet; State: TSocketState);
  begin
    Ptr:=nil;
    if State in Check then
      begin
      FillChar(FDSet,SizeOf(FDSet),0);
      FD_Set(Socket.Handle,FDSet);
      Ptr:=@FDSet;
      end;
  end;
begin
  CheckSocket;
  Result:=[];
  if (sosCanRead in Check) and ((BytesAvailable>0) or
     (Length(FEncrypted)>0) or FPeerShutdown or FFailed) then
    begin
    Include(Result,sosCanRead);
    Exclude(Check,sosCanRead);
    Timeout:=0;
    end;
  if Check=[] then Exit;
  Prepare(ReadSet,ReadPtr,sosCanRead);
  Prepare(WriteSet,WritePtr,sosCanWrite);
  Prepare(ErrorSet,ErrorPtr,sosException);
  TVPtr:=nil;
  if Timeout>=0 then
    begin
    TV.tv_sec:=Timeout div 1000;
    TV.tv_usec:=(Timeout mod 1000)*1000;
    TVPtr:=@TV;
    end;
  R:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}WinSock2.Select(0,ReadPtr,WritePtr,ErrorPtr,TVPtr);
  if R<0 then FLastError:=WSAGetLastError else FLastError:=0;
  if R>0 then
    begin
    if (ReadPtr<>nil) and FD_IsSet(Socket.Handle,ReadSet) then Include(Result,sosCanRead);
    if (WritePtr<>nil) and FD_IsSet(Socket.Handle,WriteSet) then Include(Result,sosCanWrite);
    if (ErrorPtr<>nil) and FD_IsSet(Socket.Handle,ErrorSet) then Include(Result,sosException);
    end;
end;

function TSchannelSocketHandler.Shutdown(BiDirectional: Boolean): Boolean;
var
  Token: Cardinal;
  Input, Output: array[0..0] of TSecBuffer;
  ID, OD: TSecBufferDesc;
  Attr: Cardinal;
  Status: TSecurityStatus;
  UntilTime: QWord;
  Discard: array[0..1023] of Byte;
begin
  Result:=True;
  try
    if not FHaveContext or FFailed then Exit;
    UntilTime:=Deadline(Socket.IOTimeout);
    // Closing must not block forever when no I/O timeout was configured.
    if UntilTime=0 then UntilTime:=Deadline(2000);
    Token:=SCHANNEL_SHUTDOWN;
    FillChar(Input,SizeOf(Input),0);
    BufferDesc(ID,Input);
    Input[0].BufferType:=SECBUFFER_TOKEN;
    Input[0].Size:=SizeOf(Token);
    Input[0].Data:=@Token;
    Status:=ApplyControlToken(@FContext,@ID);
    if Status<>SEC_E_OK then Exit(Fail(Status,'Schannel shutdown token'));
    FillChar(Output,SizeOf(Output),0);
    BufferDesc(OD,Output);
    Output[0].BufferType:=SECBUFFER_TOKEN;
    Status:=InitializeSecurityContextW(@FCredential,@FContext,PWideChar(FTarget),
      ContextFlags,0,0,nil,0,@FContext,@OD,@Attr,nil);
    try
      if (Status<>SEC_E_OK) and (Status<>SEC_I_CONTEXT_EXPIRED) then
        Exit(Fail(Status,'Schannel shutdown'));
      if Output[0].Size>0 then
        if not SendRaw(Output[0].Data,Output[0].Size,UntilTime) then Exit(False);
    finally
      if Output[0].Data<>nil then FreeContextBuffer(Output[0].Data);
    end;
    if BiDirectional then
      while not FPeerShutdown do
        if RecvUntil(Discard,SizeOf(Discard),UntilTime)<0 then Exit(False);
  finally ReleaseContext end;
end;

function TSchannelSocketHandler.Close: Boolean;
begin Result:=Shutdown(False) end;

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TSchannelSocketHandler);
end.
