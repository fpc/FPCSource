{$IFNDEF FPC_DOTTEDUNITS}
unit opensslsockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Net.Sockets, System.Net.Ssockets, System.Net.Sslsockets, System.Net.Sslbase, Api.Openssl, System.Net.Fpopenssl;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sockets, ssockets, sslsockets, sslbase, openssl, fpopenssl;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TOpenSSLSocketHandler }

  TOpenSSLSocketHandler = Class(TSSLSocketHandler)
  Private
    FSSL: TSSL;
    FCTX : TSSLContext;
    FSSLLastErrorString: string;
    FSSLLastError : Integer;
  Protected
    procedure SetSSLLastErrorString(AValue: string);
    Function FetchErrorInfo: Boolean;
    function CheckSSL(SSLResult: Integer): Boolean;
    function CheckSSL(SSLResult: Pointer): Boolean;
    function CreateSSLContext(AType: TSSLType): TSSLContext; virtual;
    function InitContext(NeedCertificate: Boolean): Boolean; virtual;
    function DoneContext: Boolean; virtual;
    function InitSslKeys: boolean;virtual;
    Function GetLastSSLErrorString : String; override;
    Function GetLastSSLErrorCode : Integer; override;
  Public
    Constructor create; override;
    destructor destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function Connect : Boolean; override;
    function Close : Boolean; override;
    function Accept : Boolean; override;
    function Shutdown(BiDirectional : Boolean): boolean; override;
    function Send(Const Buffer; Count: Integer): Integer; override;
    function Recv(Const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    // Result of last CheckSSL call.
    Function SSLLastError: integer;
    // Negotiated ALPN protocol after a completed handshake; '' when nothing negotiated
    // or FSSL is not yet created. Overrides the provider-neutral base virtual (Story 4.4).
    function GetSelectedALPNProtocol: string; override;
    property SSLLastErrorString: string read FSSLLastErrorString write SetSSLLastErrorString;
    property SSL: TSSL read FSSL; // allow more lower level info and control
  end;

// --- RFC 9113 §9.2 TLS floor predicates (Story 4.5) ---------------------------
// Pure, socket-free decisions over the negotiated TLS version string and cipher
// name. They reference no socket, no FSSL, no connection state and no globals,
// so they are callable from a throwaway test with literal strings (NFR7). The
// live policy in TOpenSSLSocketHandler.InitContext / Accept (below) is the only
// caller in the library.

// True when the ALPN list (comma-separated, e.g. 'h2,http/1.1') offers the 'h2'
// token. Case/space-insensitive *token* match -- NOT a substring test, so
// 'h2c' and 'h2-foo' must NOT match.
Function ALPNListOffersH2(const aALPNList: string): Boolean;

// RFC 9113 §9.2: TLS 1.2 or higher. aVersion is TSSL.Version
// ('TLSv1.2','TLSv1.3','TLSv1.1','TLSv1','SSLv3',...).
Function H2TLSVersionAdequate(const aVersion: string): Boolean;

// RFC 9113 §9.2.2: prohibit the Appendix-A cipher suites. Practical equivalent
// (see story Dev Notes): require an AEAD cipher (GCM / CHACHA20-POLY1305 / CCM)
// with ephemeral key exchange (ECDHE/DHE), OR any TLS 1.3 cipher (TLS 1.3 suites
// are never on the §9.2.2 list). aCipherName is TSSL.CipherName (OpenSSL name,
// e.g. 'ECDHE-RSA-AES128-GCM-SHA256').
Function H2CipherAdequate(const aVersion, aCipherName: string): Boolean;

// The combined §9.2 floor decision used by the Accept wiring.
Function H2TLSFloorMet(const aVersion, aCipherName: string): Boolean;

implementation

{ TSocketHandler }
Resourcestring
  SErrNoLibraryInit = 'Could not initialize OpenSSL library';
  SErrCouldNotCreateSelfSignedCertificate = 'Failed to create self-signed certificate';
  SErrCouldNotInitSSLKeys = 'Failed to initialize SSL keys';
  SErrH2InadequateSecurity = 'h2 refused: TLS parameters below RFC 9113 §9.2 floor (version=%s cipher=%s)';

Const
  // openssl/tls1.h. Passed to TSSLContext.SetMinProtoVersion to enforce the
  // RFC 9113 §9.2 floor (TLS >= 1.2) when h2 is offered (Story 4.5). Lives here,
  // at the call site, rather than in fpopenssl/openssl.pas (see story Task 1).
  TLS1_2_VERSION = $0303;

{ --- RFC 9113 §9.2 TLS floor predicates (Story 4.5): pure, socket-free --- }

// Parse an OpenSSL TLS version string ('TLSv1.2','TLSv1.3','TLSv1',...) into
// major/minor. Returns False for anything that is not a 'TLSv' string (e.g.
// 'SSLv3'), which the floor treats as inadequate.
Function ParseTLSVersion(const aVersion: string; out aMajor, aMinor: Integer): Boolean;
var
  v, rest: string;
  dotPos, code: Integer;
begin
  Result := False;
  aMajor := 0;
  aMinor := 0;
  v := UpperCase(Trim(aVersion));
  if Copy(v, 1, 4) <> 'TLSV' then
    Exit;                              // SSLv3 and unknowns -> not adequate
  rest := Copy(v, 5, Length(v));       // '1.2', '1.3', '1', '1.1', ...
  dotPos := Pos('.', rest);
  if dotPos > 0 then
    begin
    Val(Copy(rest, 1, dotPos - 1), aMajor, code);
    if code <> 0 then Exit;
    Val(Copy(rest, dotPos + 1, Length(rest)), aMinor, code);
    if code <> 0 then Exit;
    end
  else
    begin
    Val(rest, aMajor, code);           // 'TLSv1' -> major 1, minor 0
    if code <> 0 then Exit;
    aMinor := 0;
    end;
  Result := True;
end;

Function ALPNListOffersH2(const aALPNList: string): Boolean;
var
  i, start, len: Integer;
begin
  Result := False;
  // Token match on a comma-separated list, case/space-insensitive. Hand-rolled
  // split on ',' (rather than strutils.SplitString) to avoid adding a new unit
  // dependency to this package. Splitting on ',' guarantees 'h2c'/'h2-foo' are
  // distinct tokens and never match the 'h2' token.
  len := Length(aALPNList);
  start := 1;
  for i := 1 to len + 1 do
    if (i > len) or (aALPNList[i] = ',') then
      begin
      if LowerCase(Trim(Copy(aALPNList, start, i - start))) = 'h2' then
        Exit(True);
      start := i + 1;
      end;
end;

Function H2TLSVersionAdequate(const aVersion: string): Boolean;
var
  major, minor: Integer;
begin
  // §9.2: TLS >= 1.2.
  Result := ParseTLSVersion(aVersion, major, minor)
            and ((major > 1) or ((major = 1) and (minor >= 2)));
end;

Function H2CipherAdequate(const aVersion, aCipherName: string): Boolean;
var
  major, minor: Integer;
  c: string;
  isAEAD, isPFS: Boolean;
begin
  // TLS 1.3 (or higher): every standardized suite is AEAD + forward-secret and
  // none appear on the RFC 9113 §9.2.2 (Appendix A) prohibited list -> adequate
  // for any cipher.
  if ParseTLSVersion(aVersion, major, minor)
     and ((major > 1) or ((major = 1) and (minor >= 3))) then
    Exit(True);
  // TLS 1.2: §9.2.2 prohibits everything that is NOT an AEAD cipher using
  // ephemeral (forward-secret) key exchange. Expressed as the AEAD+PFS allowlist
  // -- the practical equivalent of the ~270-entry Appendix-A blocklist, matching
  // how mainstream h2 servers (nginx/h2o/nghttp2) enforce it. This is the
  // security-load-bearing decision: keep the token set named and commented.
  c := UpperCase(aCipherName);
  isAEAD := (Pos('GCM', c) > 0) or (Pos('CHACHA20', c) > 0) or (Pos('CCM', c) > 0);
  isPFS  := (Pos('ECDHE', c) > 0) or (Pos('EDH', c) > 0) or (Pos('DHE', c) > 0);
  Result := isAEAD and isPFS;
end;

Function H2TLSFloorMet(const aVersion, aCipherName: string): Boolean;
begin
  Result := H2TLSVersionAdequate(aVersion) and H2CipherAdequate(aVersion, aCipherName);
end;

Procedure MaybeInitSSLInterface;

begin
  if not IsSSLloaded then
    if not InitSSLInterface then
      Raise EInOutError.Create(SErrNoLibraryInit);
end;

function TopenSSLSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TOpenSSLX509Certificate.Create;
end;

function TOpenSSLSocketHandler.CreateSSLContext(AType: TSSLType): TSSLContext;
begin
  Result := TSSLContext.Create(AType);
end;

procedure TOpenSSLSocketHandler.SetSSLLastErrorString(AValue: string);
begin
  if FSSLLastErrorString=AValue then Exit;
  FSSLLastErrorString:=AValue;
end;

function NormalizeHostNameForSNI(const AHostName: AnsiString): AnsiString;
begin
  Result:=LowerCase(AHostName);
  if (Length(Result)>0) and (Result[Length(Result)]='.') then
    Delete(Result,Length(Result),1);
end;

function TOpenSSLSocketHandler.Connect: Boolean;

var
  SNIHostName: AnsiString;

begin
  Result:=Inherited Connect;
  Result := Result and InitContext(False);
  if Result then
    begin
    Result:=CheckSSL(FSSL.SetFD(Socket.Handle));
    if Result then
     begin
     if SendHostAsSNI  and (Socket is TInetSocket) then
       begin
       SNIHostName:=NormalizeHostNameForSNI(AnsiString((Socket as TInetSocket).NetworkAddress.Address));
       FSSL.Ctrl(SSL_CTRL_SET_TLSEXT_HOSTNAME,TLSEXT_NAMETYPE_host_name,PAnsiChar(SNIHostName));
       end;
     if VerifyPeerCert and (Socket is TInetSocket) then
       FSSL.Set1Host(NormalizeHostNameForSNI(AnsiString((Socket as TInetSocket).Host)));
     Result:=CheckSSL(FSSL.Connect);
     //if Result and VerifyPeerCert then
     //  Result:=(FSSL.VerifyResult<>0) or (not DoVerifyCert);
     if Result then
       Result:= DoVerifyCert;
     if Result then
       SetSSLActive(True);
     end;
    end;
end;

function TOpenSSLSocketHandler.Close: Boolean;
begin
  Result:=Shutdown(False);
end;

Function TOpenSSLSocketHandler.FetchErrorInfo : Boolean;

var
  S : AnsiString;

begin
  FSSLLastErrorString:='';
  FSSLLastError:=ErrGetError;
  ErrClearError;
  Result:=(FSSLLastError<>0);
  if Result then
    begin
    S:=StringOfChar(#0,256);
    ErrErrorString(FSSLLastError,S,256);
    FSSLLastErrorString:=s;
    end;
end;

function TOpenSSLSocketHandler.CheckSSL(SSLResult : Integer) : Boolean;

begin
  Result:=SSLResult>=1;
  if Not Result then
     begin
     FSSLLastError:=SSLResult;
     FetchErrorInfo;
     end;
end;

function TOpenSSLSocketHandler.CheckSSL(SSLResult: Pointer): Boolean;
begin
  Result:=(SSLResult<>Nil);
  if not Result then
    Result:=FetchErrorInfo;
end;

function TOpenSSLSocketHandler.DoneContext: Boolean;

begin
  FreeAndNil(FSSL);
  FreeAndNil(FCTX);
  ErrRemoveState(0);
  SetSSLActive(False);
  Result:=True;
end;

Function HandleSSLPwd(buf : PAnsiChar; len:Integer; flags:Integer; UD : Pointer):Integer; cdecl;

var
  Pwd: AnsiString;
  H :  TOpenSSLSocketHandler;

begin
  if Not Assigned(UD) then
    PWD:=''
  else
    begin
    H:=TOpenSSLSocketHandler(UD);
    Pwd:=H.CertificateData.KeyPassword;
    end;
   Result:=Length(Pwd);
   if Result=0 then
     Exit;
   if Result>len then
     Result:=len;
   Move(Pointer(Pwd)^,Buf^,Result);
end;

function TOpenSSLSocketHandler.InitSslKeys: boolean;

begin
  Result:=(FCTX<>Nil);
  if not Result then
    Exit;
  if not CertificateData.Certificate.Empty then
    Result:=CheckSSL(FCTX.UseCertificate(CertificateData.Certificate));
  if Result and not CertificateData.PrivateKey.Empty then
    Result:=CheckSSL(FCTX.UsePrivateKey(CertificateData.PrivateKey));
  if Result and ((CertificateData.CertCA.FileName<>'') or (CertificateData.TrustedCertsDir<>'')) then
    Result:=CheckSSL(FCTX.LoadVerifyLocations(CertificateData.CertCA.FileName,CertificateData.TrustedCertsDir));
  if Result and not CertificateData.PFX.Empty then
    Result:=CheckSSL(FCTX.LoadPFX(CertificateData.PFX,CertificateData.KeyPassword));
end;

function TOpenSSLSocketHandler.GetLastSSLErrorString: String;
begin
  Result:=FSSLLastErrorString;
end;

function TOpenSSLSocketHandler.GetLastSSLErrorCode: Integer;
begin
  Result:=FSSLLastError;
end;

constructor TOpenSSLSocketHandler.create;
begin
  inherited create;
  MaybeInitSSLInterface;
end;

destructor TOpenSSLSocketHandler.destroy;
begin
  FreeAndNil(FCTX);
  FreeAndNil(FSSL);
  inherited destroy;
end;

function TOpenSSLSocketHandler.InitContext(NeedCertificate:Boolean): Boolean;

Const
  VO : Array[Boolean] of Integer = (SSL_VERIFY_NONE,SSL_VERIFY_PEER);

var
  s: AnsiString;

begin
  Result:=DoneContext;
  if Not Result then
    Exit;
  try
    FCTX:=CreateSSLContext(SSLType);
  Except
    CheckSSL(Nil);
    raise;
  end;
  S:=CertificateData.CipherList;
  FCTX.SetCipherList(S);
  FCTX.SetVerify(VO[VerifypeerCert],Nil);
  FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
  FCTX.SetDefaultPasswdCbUserdata(self);
  If NeedCertificate and CertificateData.NeedCertificateData  then
    if Not CreateSelfSignedCertificate then
      begin
      DoneContext;
      raise ESSL.Create(SErrCouldNotCreateSelfSignedCertificate);
      end;
   if Not InitSSLKeys then
     begin
     DoneContext;
     raise ESSL.Create(SErrCouldNotInitSSLKeys);
     end;
   // ALPN install — runs on both Accept (server, drives select callback) and Connect
   // (client, sends offer). Sourced from the provider-neutral CertificateData.ALPNProtocols
   // (Story 4.4). Inert when empty, so existing TLS users are byte-for-byte unchanged (NFR1).
   // Must happen after FCTX config and before FSSL exists.
   if CertificateData.ALPNProtocols <> '' then
     FCTX.SetALPNProtocols(CertificateData.ALPNProtocols);
   // RFC 9113 §9.2 TLS floor (Story 4.5): when h2 is offered, require TLS >= 1.2,
   // so a peer that can only do TLS < 1.2 simply fails the handshake. Gated on the
   // h2 offer so non-h2 TLS users are byte-for-byte unchanged (NFR1). No max-version
   // cap -> TLS 1.3 stays available.
   if ALPNListOffersH2(CertificateData.ALPNProtocols) then
     FCTX.SetMinProtoVersion(TLS1_2_VERSION);
   try
     FSSL:=TSSL.Create(FCTX);
     Result:=True;
   Except
     CheckSSL(Nil);
     DoneContext;
     raise;
   end;
end;

function TOpenSSLSocketHandler.Accept: Boolean;

begin
  Result:=InitContext(True);
  if Result then
    begin
    Result:=CheckSSL(FSSL.setfd(Socket.Handle));
    if Result then
      Result:=CheckSSL(FSSL.Accept);
    end;
  // RFC 9113 §9.2.2 (Story 4.5): a negotiated h2 connection MUST meet the cipher
  // floor; otherwise refuse it (INADEQUATE_SECURITY-class). The (version,cipher)
  // pair is inspected ONLY when h2 was actually negotiated, so non-h2 TLS
  // connections are never affected (NFR1). H2TLSFloorMet also re-checks the TLS
  // version, catching any build/config that somehow negotiated h2 below TLS 1.2
  // despite the preventive InitContext gate.
  if Result and (GetSelectedALPNProtocol = 'h2')
     and not H2TLSFloorMet(FSSL.Version, FSSL.CipherName) then
    begin
    SetSSLLastErrorString(Format(SErrH2InadequateSecurity, [FSSL.Version, FSSL.CipherName]));
    DoneContext;            // frees FSSL/FCTX + SetSSLActive(False): same failed state as any Accept failure
    Result:=False;
    Exit;                   // DoneContext already called SetSSLActive(False)
    end;
  SetSSLActive(Result);
end;


function TOpenSSLSocketHandler.Shutdown(BiDirectional : Boolean): boolean;

var
  r : integer;

begin
  Result:=assigned(FSsl);
  if Result then
    If Not BiDirectional then
      Result:=CheckSSL(FSSL.Shutdown)
    else
      begin
      r:=FSSL.Shutdown;
      if r<>0 then
        Result:=CheckSSL(r)
      else
        begin
        Result:=fpShutdown(Socket.Handle,1)=0;
        if Result then
          Result:=CheckSSL(FSsl.Shutdown);
        end
      end;
  If Result then
    Result:=DoneContext;
end;

function TOpenSSLSocketHandler.Send(Const Buffer; Count: Integer): Integer;
var
  e: integer;
begin
  FLastError:=0;
  FSSLLastError := 0;
  FSSLLastErrorString:='';
  repeat
    Result:=FSsl.Write(@Buffer,Count);
    e:=FSsl.GetError(Result);
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    begin
    FSSLLastError:=e;
    if e=SSL_ERROR_SYSCALL then
      FLastError:=socketerror;
    end;
end;

function TOpenSSLSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

var
  e: integer;
begin
  FLastError:=0;
  FSSLLastError:=0;
  FSSLLastErrorString:= '';
  repeat
    Result:=FSSL.Read(@Buffer ,Count);
    e:=FSSL.GetError(Result);
    if (e=SSL_ERROR_WANT_READ) and (Socket.IOTimeout>0) then
      e:=SSL_ERROR_ZERO_RETURN;
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    begin
    FSSLLastError:=e;
    if e=SSL_ERROR_SYSCALL then
      FLastError:=socketerror;
    end;
end;

function TOpenSSLSocketHandler.BytesAvailable: Integer;
begin
  Result:= FSSL.Pending;
end;

Function TOpenSSLSocketHandler.SSLLastError: integer;
begin
  Result:=FSSLLastError;
end;

function TOpenSSLSocketHandler.GetSelectedALPNProtocol: string;
begin
  if Assigned(FSSL) then
    Result:=FSSL.GetSelectedALPNProtocol
  else
    Result:='';
end;

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TOpenSSLSocketHandler);
end.

