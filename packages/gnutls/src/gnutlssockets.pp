{$IFNDEF FPC_DOTTEDUNITS}
unit gnutlssockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Net.Sockets, System.Net.Ssockets, System.Net.Sslsockets,
  System.DateUtils, System.CTypes, System.Net.Sslbase, Api.GnuTls;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sockets, ssockets, sslsockets, dateUtils,
  cTypes, sslbase, gnutls;
{$ENDIF FPC_DOTTEDUNITS}

Const
  DefCertSize = 8192;

Type
  EGnuTLS = Class(Exception);

  { TGNUTLSSocketHandler }

  TGNUTLSSocketHandler = Class(TSSLSocketHandler)
  Private
    FSession : tgnutls_session_t;
    FCred : tgnutls_certificate_credentials_t;
    FGNUTLSLastErrorString: ansistring;
    FGNUTLSLastError : Integer;
    FCurrentHostName : AnsiString;
    FCurrentCypherlist : AnsiString;
    function LoadTrustedCertificate(aCertData: TSSLData): Boolean;
    function MaybeAllocateCredentials: Boolean;
    procedure SetGNUTLSLastErrorString(AValue: Ansistring);
    function SetTrustedCertificateDir(const aFileName: string): Boolean;
  Protected
    function LoadCertificate(aData, aKey: TSSLData): Boolean;
    procedure FreeCredentials;
    procedure FreeSession;
    function DoHandShake: Boolean;
    function GetCipherListString: String; virtual;
    Function FetchErrorInfo: Boolean;
    function Check(aResult: cInt) : cInt;
    function CheckOK(aResult: cInt) : Boolean;
    function InitSession(AsServer: Boolean): Boolean; virtual;
    function DoneSession: Boolean; virtual;
    function InitSslKeys: boolean;virtual;
    function GetLastSSLErrorCode: Integer; override;
    function GetLastSSLErrorString: String; override;
    // Install CertificateData.ALPNProtocols on FSession before the handshake.
    // Inert (no-op) when the list is empty, so non-ALPN users are unchanged.
    function InstallALPNProtocols: Boolean; virtual;
    // Negotiated TLS version/cipher/key-exchange names ('' when unavailable).
    procedure GetNegotiatedTLSNames(out aVersion, aCipher, aKX: string);
    // True when the live session meets the RFC 9113 §9.2 TLS floor for h2.
    function NegotiatedH2TLSFloorMet: Boolean; virtual;
  Public
    Constructor create; override;
    destructor destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function Connect : Boolean; override;
    function Close : Boolean; override;
    function Accept : Boolean; override;
    // Negotiated ALPN protocol after a completed handshake; '' when none.
    function GetSelectedALPNProtocol: string; override;
    function Shutdown(BiDirectional : Boolean): boolean; override;
    function Send(Const Buffer; Count: Integer): Integer; override;
    function Recv(Const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    // Result of last CheckSSL call.
    Function GNUTLSLastError: integer;
    property GNUTLSLastErrorString: Ansistring read FGNUTLSLastErrorString write SetGNUTLSLastErrorString;
  end;

  { TGNUTLSX509Certificate }

  TGNUTLSX509Certificate = class(TX509Certificate)
  private
    FMyFormat : tgnutls_x509_crt_fmt_t;
    procedure Check(Aret: cint); inline;
    procedure Check(Aexp: Boolean; Aret: cint); inline;
    function GenCACert(const Aca_priv_key: TBytes; const Acommon_name, Aserial: AnsiString; Adays: Word): TBytes;
    function GenPrivKey: TBytes;
    function GenSrvCert(const Aca_priv_key, Aca_pem, Asrv_priv_key: TBytes; const Acommon_name, Aorganization, Aserial: AnsiString;
      Adays: Word): TBytes;
  public
    constructor create;
    function CreateCertificateAndKey: TCertAndKey; override;
  end;

// RFC 9113 §9.2 TLS-floor predicates. Pure decisions over GnuTLS name strings
// (version 'TLS1.2'/'TLS1.3', cipher 'AES-128-GCM', key-exchange 'ECDHE-RSA'),
// so they are testable with literals, independent of any session.

// True when the comma-separated ALPN list offers the 'h2' token (exact token
// match, case/space-insensitive; 'h2c' and 'h2-foo' never match).
Function ALPNListOffersH2(const aALPNList: string): Boolean;
// §9.2: TLS >= 1.2. aVersionName is gnutls_protocol_get_name output.
Function H2TLSVersionAdequate(const aVersionName: string): Boolean;
// §9.2.2: TLS 1.3 is always adequate; TLS 1.2 requires an AEAD cipher
// (GCM/CHACHA20/CCM) with an ephemeral key exchange (ECDHE/DHE).
Function H2CipherAdequate(const aVersionName, aCipherName, aKXName: string): Boolean;
// Combined §9.2 floor used by the Accept wiring.
Function H2TLSFloorMet(const aVersionName, aCipherName, aKXName: string): Boolean;

implementation

Const
  SErrH2InadequateSecurity = 'h2 refused: TLS parameters below RFC 9113 §9.2 floor (version=%s cipher=%s kx=%s)';
  // gnutls_alpn_flags_t: prefer our (server) protocol order over the client's.
  GNUTLS_ALPN_SERVER_PRECEDENCE = 1 shl 1;

{ TSocketHandler }

Procedure MaybeInitGNUTLS;

begin
  if not GnuTLSloaded then
     LoadGnuTLS();
end;

{ --- RFC 9113 §9.2 TLS-floor predicates: pure, session-free --- }

// Parse a GnuTLS 'TLS1.x' version name into major/minor. False for anything
// that is not a TLS version (SSL3, DTLS, unknown), which the floor rejects.
Function ParseTLSVersionName(const aVersionName: string; out aMajor, aMinor: Integer): Boolean;
var
  v, rest: string;
  dotPos, code: Integer;
begin
  Result := False;
  aMajor := 0;
  aMinor := 0;
  v := UpperCase(Trim(aVersionName));
  if Copy(v, 1, 3) <> 'TLS' then
    Exit;                              // SSL3 / DTLS / unknown -> not adequate
  rest := Copy(v, 4, Length(v));       // '1.2', '1.3', '1.0', ...
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
    Val(rest, aMajor, code);
    if code <> 0 then Exit;
    end;
  Result := True;
end;

Function ALPNListOffersH2(const aALPNList: string): Boolean;
var
  i, start, len: Integer;
begin
  Result := False;
  // Hand-rolled comma split (no extra unit dependency): each token is matched
  // whole, so 'h2c'/'h2-foo' are distinct from the 'h2' token.
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

Function H2TLSVersionAdequate(const aVersionName: string): Boolean;
var
  major, minor: Integer;
begin
  Result := ParseTLSVersionName(aVersionName, major, minor)
            and ((major > 1) or ((major = 1) and (minor >= 2)));
end;

Function H2CipherAdequate(const aVersionName, aCipherName, aKXName: string): Boolean;
var
  major, minor: Integer;
  c, k: string;
  isAEAD, isPFS: Boolean;
begin
  // TLS 1.3: every standardized suite is AEAD + forward-secret and GnuTLS reports
  // no key exchange, so accept any cipher.
  if ParseTLSVersionName(aVersionName, major, minor)
     and ((major > 1) or ((major = 1) and (minor >= 3))) then
    Exit(True);
  // TLS 1.2: require an AEAD cipher with an ephemeral (forward-secret) key
  // exchange -- the practical equivalent of the §9.2.2 Appendix-A blocklist.
  c := UpperCase(aCipherName);
  k := UpperCase(aKXName);
  isAEAD := (Pos('GCM', c) > 0) or (Pos('CHACHA20', c) > 0) or (Pos('CCM', c) > 0);
  isPFS  := (Pos('ECDHE', k) > 0) or (Pos('DHE', k) > 0);
  Result := isAEAD and isPFS;
end;

Function H2TLSFloorMet(const aVersionName, aCipherName, aKXName: string): Boolean;
begin
  Result := H2TLSVersionAdequate(aVersionName)
            and H2CipherAdequate(aVersionName, aCipherName, aKXName);
end;

{ TGNUTLSX509Certificate }

procedure TGNUTLSX509Certificate.Check(Aret: cint); inline;

begin
  if Aret <> GNUTLS_E_SUCCESS then
    raise EGnuTLS.Create(gnutls_strerror(Aret));
end;

procedure TGNUTLSX509Certificate.Check(Aexp: Boolean; Aret: cint); inline;
begin
  if Aexp then
    raise EGnuTLS.Create(gnutls_strerror(Aret));
end;

function TGNUTLSX509Certificate.GenPrivKey : TBytes;

var
  akey: Tgnutls_x509_privkey_t;
  aSize: cuint;

begin
  Result:=Default(TBytes);
  try
    Check(gnutls_x509_privkey_init(@akey));
    aSize := gnutls_sec_param_to_pk_bits(GNUTLS_PK_RSA, GNUTLS_SEC_PARAM_HIGH);
    SetLength(Result,asize);
    Check(gnutls_x509_privkey_generate(akey, GNUTLS_PK_RSA, aSize, 0));
    Check(gnutls_x509_privkey_export(akey,FMyFormat,Pointer(Result), @aSize));
    SetLength(Result,asize);
  except
    gnutls_x509_privkey_deinit(akey);
    raise;
  end;
end;


Function TGNUTLSX509Certificate.GenCACert(const Aca_priv_key: TBytes; const Acommon_name, Aserial: AnsiString; Adays: Word) : TBytes;

var
  Vkey: Tgnutls_x509_privkey_t;
  Vcrt: Tgnutls_x509_crt_t = nil;
  Vdata: Tgnutls_datum_t;
  Vkeyid: TBytes;
  Vkeyidsize: csize_t;
  Vactivation: ttime_t;
  Vca_pem_size: csize_t;
  Vret: cint;

begin
  Vkeyid:=Default(TBytes);
  Result:=Default(TBytes);
  try
    Check(gnutls_x509_privkey_init(@Vkey));
    Vdata.data := Pointer(Aca_priv_key);
    Vdata.size := Length(Aca_priv_key);
    Check(gnutls_x509_privkey_import(Vkey, @Vdata, FMyFormat));
    Check(gnutls_x509_crt_init(@Vcrt));
    Check(gnutls_x509_crt_set_key(Vcrt, Vkey));
    Check(gnutls_x509_crt_set_dn_by_oid(Vcrt, GNUTLS_OID_X520_COMMON_NAME,0, @Acommon_name[1], Length(Acommon_name)));
    Check(gnutls_x509_crt_set_version(Vcrt, 3));
    Check(gnutls_x509_crt_set_serial(Vcrt, @Aserial[1], Length(Aserial)));
    Vactivation := DateTimeToUnix(Now,False);
    Check(gnutls_x509_crt_set_activation_time(Vcrt, Vactivation));
    Check(gnutls_x509_crt_set_expiration_time(Vcrt, Vactivation + (Adays * 86400)));
    Check(gnutls_x509_crt_set_ca_status(Vcrt, Ord(True)));
    Check(gnutls_x509_crt_set_key_usage(Vcrt, GNUTLS_KEY_KEY_CERT_SIGN));
    Vkeyidsize := 0;
    Vret := gnutls_x509_crt_get_key_id(Vcrt, GNUTLS_KEYID_USE_SHA1, nil, @Vkeyidsize);
    Check((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or (Vkeyidsize < 1), Vret);
    SetLength(Vkeyid, Pred(Vkeyidsize));
    Check(gnutls_x509_crt_get_key_id(Vcrt, GNUTLS_KEYID_USE_SHA1, Pointer(Vkeyid), @Vkeyidsize));
    Check(gnutls_x509_crt_set_subject_key_id(Vcrt, Pointer(Vkeyid), Vkeyidsize));
    Check(gnutls_x509_crt_sign2(Vcrt, Vcrt, Vkey, GNUTLS_DIG_SHA256, 0));
    SetLength(Result, DefCertSize);
    Check(gnutls_x509_crt_export(Vcrt, FMyFormat, Pointer(Result), @Vca_pem_size));
    SetLength(Result, Pred(Vca_pem_size));
  except
    gnutls_x509_privkey_deinit(Vkey);
    gnutls_x509_crt_deinit(Vcrt);
    raise;
  end;
end;

Function TGNUTLSX509Certificate.GenSrvCert(const Aca_priv_key, Aca_pem, Asrv_priv_key: TBytes;  const Acommon_name, Aorganization,  Aserial: AnsiString; Adays: Word) : TBytes;

var
  Vsrv_key: Tgnutls_x509_privkey_t = nil;
  Vca_key: Tgnutls_x509_privkey_t = nil;
  Vca_crt: Tgnutls_x509_crt_t = nil;
  Vsrv_crt: Tgnutls_x509_crt_t = nil;
  Vdata: Tgnutls_datum_t;
  Vkeyid: TBytes;
  Vkeyidsize: csize_t;
  Vactivation: ttime_t;
  Vsrv_pem_size: csize_t;
  Vret: cint;

begin
  Vkeyid:=Default(TBytes);
  Result:=Default(TBytes);
  try
    Check(gnutls_x509_privkey_init(@Vca_key));
    Vdata.data := Pointer(Aca_priv_key);
    Vdata.size := Length(Aca_priv_key);
    Check(gnutls_x509_privkey_import(Vca_key, @Vdata, FMyFormat));
    Check(gnutls_x509_privkey_init(@Vsrv_key));
    Vdata.data := Pointer(Asrv_priv_key);
    Vdata.size := Length(Asrv_priv_key);
    Check(gnutls_x509_privkey_import(Vsrv_key, @Vdata, FMyFormat));
    Check(gnutls_x509_crt_init(@Vca_crt));
    Vdata.data := Pointer(Aca_pem);
    Vdata.size := Length(Aca_pem);
    Check(gnutls_x509_crt_import(Vca_crt, @Vdata, FMyFormat));
    Check(gnutls_x509_crt_init(@Vsrv_crt));
    Check(gnutls_x509_crt_set_key(Vsrv_crt, Vsrv_key));
    Check(gnutls_x509_crt_set_dn_by_oid(Vsrv_crt, GNUTLS_OID_X520_COMMON_NAME, 0, @Acommon_name[1], Length(Acommon_name)));
    if (AOrganization<>'') then
      Check(gnutls_x509_crt_set_dn_by_oid(Vsrv_crt, GNUTLS_OID_X520_ORGANIZATION_NAME, 0, @Aorganization[1], Length(Aorganization)));
    Check(gnutls_x509_crt_set_version(Vsrv_crt, 3));
    Check(gnutls_x509_crt_set_serial(Vsrv_crt, @Aserial[1],Length(Aserial)));
    Vactivation := DateTimeToUnix(Now,False);
    Check(gnutls_x509_crt_set_activation_time(Vsrv_crt, Vactivation));
    Check(gnutls_x509_crt_set_expiration_time(Vsrv_crt, Vactivation + (Adays * 86400)));
    Check(gnutls_x509_crt_set_ca_status(Vsrv_crt, Ord(False)));
    Check(gnutls_x509_crt_set_key_purpose_oid(Vsrv_crt,  @GNUTLS_KP_TLS_WWW_SERVER[1], Ord(False)));
    Vkeyidsize := 0;
    Vret := gnutls_x509_crt_get_subject_key_id(Vca_crt, nil, @Vkeyidsize, nil);
    Check((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or  (Vkeyidsize < 1), Vret);
    SetLength(Vkeyid, Pred(Vkeyidsize));
    Check(gnutls_x509_crt_get_subject_key_id(Vca_crt, Pointer(Vkeyid), @Vkeyidsize, nil));
    Check(gnutls_x509_crt_set_subject_key_id(Vsrv_crt, Pointer(Vkeyid), Vkeyidsize));
    Vkeyidsize := 0;
    gnutls_x509_crt_get_key_id(Vsrv_crt, GNUTLS_KEYID_USE_SHA1, nil, @Vkeyidsize);
    Check((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or (Vkeyidsize < 1), Vret);
    SetLength(Vkeyid, Pred(Vkeyidsize));
    Check(gnutls_x509_crt_get_key_id(Vsrv_crt, GNUTLS_KEYID_USE_SHA1, Pointer(Vkeyid), @Vkeyidsize));
    Check(gnutls_x509_crt_set_authority_key_id(Vsrv_crt,  Pointer(Vkeyid), Vkeyidsize));
    Check(gnutls_x509_crt_sign2(Vsrv_crt, Vca_crt, Vca_key,  GNUTLS_DIG_SHA256, 0));
    Vsrv_pem_size := DefCertSize;
    SetLength(Result, Pred(Vsrv_pem_size));
    Check(gnutls_x509_crt_export(Vsrv_crt, FMyFormat,Pointer(Result), @Vsrv_pem_size));
    SetLength(Result, Vsrv_pem_size);
  except
    gnutls_x509_privkey_deinit(Vsrv_key);
    gnutls_x509_privkey_deinit(Vca_key);
    gnutls_x509_crt_deinit(Vca_crt);
    gnutls_x509_crt_deinit(Vsrv_crt);
    raise;
  end;
end;

constructor TGNUTLSX509Certificate.create;
begin
  FMyFormat:=GNUTLS_X509_FMT_PEM;
end;

function TGNUTLSX509Certificate.CreateCertificateAndKey: TCertAndKey;

Var
  PK,cacert : TBytes;

begin
  Result:=Default(TCertAndKey);
  PK:=GenPrivKey;
  CaCErt:=GenCACert(PK,Self.HostName,IntToStr(Serial),30);
  Result.PrivateKey:=PK;
  Result.Certificate:=GenSrvCert(PK,CaCert,PK,Self.HostName,'',IntToStr(Serial),30);
end;

function TGNUTLSSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TGNUTLSX509Certificate.Create;
end;

procedure TGNUTLSSocketHandler.SetGNUTLSLastErrorString(AValue: Ansistring);
begin
  if FGNUTLSLastErrorString=AValue then Exit;
  FGNUTLSLastErrorString:=AValue;
end;


function TGNUTLSSocketHandler.Connect: Boolean;

begin
  Result:=Inherited Connect;
  // Initsession sets handle
  Result := Result and InitSession(False);
  if Not Result then
     exit;
  if (Socket is TInetSocket) then
    begin
    FCurrentHostName:=(Socket as TInetSocket).NetworkAddress.Address;
    if SendHostAsSNI then
      begin
      Result:=CheckOK(gnutls_server_name_set(FSession, GNUTLS_NAME_DNS,PAnsiChar(FCurrentHostName), length(FCurrentHostName)));
      if not Result then
        exit;
      end;
    gnutls_session_set_verify_cert(Fsession,PAnsiChar(FCurrentHostName),0);
    end;
  if Not Result then
    exit;
  Result:=DoHandShake;
  if Result and VerifyPeerCert then
    Result:=DoVerifyCert;
  if Result then
    SetSSLActive(True);
end;

function TGNUTLSSocketHandler.Close: Boolean;
begin
  Result:=CheckOK(gnutls_bye(FSession,GNUTLS_SHUT_WR));
end;

Function TGNUTLSSocketHandler.FetchErrorInfo : Boolean;

Var
  P : PAnsiChar;

begin
  FGNUTLSLastErrorString:='';
  Result:=(FGNUTLSLastError<>0);
  if Result then
    begin
    P:=gnutls_strerror(FGNUTLSLastError);
    if P<>Nil then
      FGNUTLSLastErrorString:=StrPas(P)
    else
      FGNUTLSLastErrorString:=Format('Unknown error code: %d',[FGNUTLSLastError]);
    end;
end;

function TGNUTLSSocketHandler.CheckOK(aResult: cInt): Boolean;
begin
  Result:=Check(aResult)>=0;
end;

function TGNUTLSSocketHandler.Check(aResult: cInt): cInt;
begin
  Result:=aResult;
  if (Result<>GNUTLS_E_SUCCESS) then
     begin
     FGNUTLSLastError:=aResult;
     FetchErrorInfo;
     end;
end;

function TGNUTLSSocketHandler.GetCipherListString : String;

begin
  //  Set correct cipher list here
  if FCurrentCypherlist='' then
    begin
    // We need it in a field variable, it seems GNUTLS expects it to last?
    FCurrentCypherlist:='NORMAL';
    // RFC 9113 §9.2: when h2 is offered, forbid TLS < 1.2 at negotiation time.
    if ALPNListOffersH2(CertificateData.ALPNProtocols) then
      FCurrentCypherlist:=FCurrentCypherlist+':-VERS-TLS1.1:-VERS-TLS1.0:-VERS-SSL3.0';
    end;
  Result:=FCurrentCypherlist;
end;

function TGNUTLSSocketHandler.InitSession(AsServer: Boolean): Boolean;

Const
  InitFlags : Array[Boolean] of cInt = (GNUTLS_CLIENT,GNUTLS_SERVER);

Var
  flags :Cint;
  errPtr : PAnsiChar;
  A : AnsiString;

begin
  Flags:=InitFlags[AsServer];
  Result:=CheckOK(gnutls_init(@FSession,Flags));
  if not Result then
    exit;
  A:=GetCipherListString;
  Result:=CheckOK(gnutls_priority_set_direct(FSession, PAnsiChar(A), @errptr));
  if not Result then
    FGNUTLSLastErrorString:=FGNUTLSLastErrorString+', error at: '+StrPas(errPtr);
  If AsServer and CertificateData.NeedCertificateData  then
    if Not CreateSelfSignedCertificate then
      begin
      DoneSession;
      Exit(False);
      end;
  Result:=InitSslKeys;
  if not Result then
    begin
    DoneSession;
    Exit;
    end;
{
S:=CertificateData.CipherList;
FCTX.SetVerify(VO[VerifypeerCert],Nil);
FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
FCTX.SetDefaultPasswdCbUserdata(self);
}
  // ALPN offer/select; must be installed before the handshake. Inert when empty.
  Result:=InstallALPNProtocols;
  if not Result then
    begin
    DoneSession;
    Exit;
    end;
  gnutls_transport_set_int2(FSession,Socket.Handle,Socket.Handle);
  gnutls_handshake_set_timeout(FSession,GNUTLS_DEFAULT_HANDSHAKE_TIMEOUT);
end;

function TGNUTLSSocketHandler.DoneSession: Boolean;
begin
  FreeSession;
  FreeCredentials;
  Result:=True;
end;

function TGNUTLSSocketHandler.MaybeAllocateCredentials : Boolean;

begin
  Result:=Assigned(FCred);
  if Result then
    exit;
  Result:=CheckOK(gnutls_certificate_allocate_credentials(@FCred));
  if Result then
    Result:=CheckOK(gnutls_credentials_set(FSession, GNUTLS_CRD_CERTIFICATE, FCred));
end;

function TGNUTLSSocketHandler.LoadCertificate(aData,aKey : TSSLData) : Boolean;
var
  key,cert : tgnutls_datum_t;
  A,B: AnsiString;

begin
  result:=MaybeAllocateCredentials;
  if not Result then exit;
  if (aData.FileName='') then
    begin
    cert.data:=Pointer(aData.Value);
    cert.size:=Length(aData.Value);
    key.data:=Pointer(aKey.Value);
    key.size:=Length(aKey.Value);
    Result:=CheckOK(gnutls_certificate_set_x509_key_mem(FCred,@cert,@key,GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_key_mem(FCred,@cert,@key,GNUTLS_X509_FMT_DER));
    end
  else
  begin
    A:=aData.FileName;
    B:=aKey.FileName;
    Result:=CheckOK(gnutls_certificate_set_x509_key_file (FCred, PAnsiChar(A),PAnsiChar(B),GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_key_file (FCred, PAnsiChar(A),PAnsiChar(B),GNUTLS_X509_FMT_DER));
    end;
  if Result then
    Result:=CheckOK(gnutls_credentials_set(FSession, GNUTLS_CRD_CERTIFICATE, FCred));
end;

function TGNUTLSSocketHandler.LoadTrustedCertificate(aCertData : TSSLData) : Boolean;

var
  ca : tgnutls_datum_t;
  A : AnsiString;
begin
  MaybeAllocateCredentials;
  Result:=False;
  if (aCertData.FileName<>'') then
    begin
    A:=aCertData.FileName;
    Result:=CheckOK(gnutls_certificate_set_x509_trust_file(FCred,PAnsiChar(A),GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_trust_file(FCred,PAnsiChar(A),GNUTLS_X509_FMT_DER));
    end;
  if (Length(aCertData.Value)>0) then
    begin
    ca.data:=Pointer(aCertData.Value);
    ca.size:=Length(aCertData.Value);
    Result:=CheckOK(gnutls_certificate_set_x509_trust_mem(FCred,@ca,GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_trust_mem(FCred,@ca,GNUTLS_X509_FMT_DER));
    end;
end;

function TGNUTLSSocketHandler.SetTrustedCertificateDir(Const aFileName : string) : Boolean;

Var
  P : PAnsiChar;
  FN : AnsiString;
begin
  MaybeAllocateCredentials;
  FN:=aFileName;
  P:=PAnsiChar(FN);
  if DirectoryExists(aFileName) then
    begin
    Result:=CheckOK(gnutls_certificate_set_x509_trust_dir(FCred,P,GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_trust_dir(FCred,P,GNUTLS_X509_FMT_DER));
    end
  else
    begin
    Result:=CheckOK(gnutls_certificate_set_x509_trust_File(FCred,P,GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_trust_File(FCred,P,GNUTLS_X509_FMT_DER));
    end;
end;

function TGNUTLSSocketHandler.InitSslKeys: boolean;

begin
  Result:=(FSession<>Nil);
  if not Result then
    Exit;
  if not (CertificateData.NeedCertificateData) then
    Result:=LoadCertificate(CertificateData.Certificate,CertificateData.PrivateKey);
  if Result and Not CertificateData.TrustedCertificate.Empty then
    Result:=LoadTrustedCertificate(CertificateData.TrustedCertificate);
  if Result and (CertificateData.TrustedCertsDir<>'') then
    Result:=Result and SetTrustedCertificateDir(CertificateData.TrustedCertsDir);
  // If nothing was set, set defaults.
  if not Assigned(FCred) then
    begin
    Result:=MaybeAllocateCredentials;
    if Result then
      Result:=CheckOK(gnutls_certificate_set_x509_system_trust(FCred));
    end;
end;

constructor TGNUTLSSocketHandler.create;
begin
  inherited create;
  MaybeInitGNUTLS;
end;

destructor TGNUTLSSocketHandler.destroy;
begin
  DoneSession;
  FreeSession;
  FreeCredentials;
  inherited destroy;
end;

Procedure TGNUTLSSocketHandler.FreeSession;

begin
  If (FSession<>Nil) then
    begin
    gnutls_deinit(Fsession);
    Fsession:=Nil;
    end;
end;

Procedure TGNUTLSSocketHandler.FreeCredentials;

begin
  If (FCred<>Nil) then
    begin
    gnutls_certificate_free_credentials(FCred);
    FCred:=Nil;
    end;
end;

function TGNUTLSSocketHandler.DoHandShake : Boolean;

Var
  Ret : cInt;

begin
  Repeat
    ret:=Check(gnutls_handshake(FSession));
  until (ret>=0) or (gnutls_error_is_fatal(ret) <> 0);
  Result:=Ret>=0;
end;

function TGNUTLSSocketHandler.InstallALPNProtocols: Boolean;

var
  Spec : AnsiString;
  Toks : array of AnsiString;
  Datums : array of Tgnutls_datum_t;
  I,Start,Cnt : Integer;

  procedure AddToken(const aToken: AnsiString);
  var
    T : AnsiString;
  begin
    T:=Trim(aToken);
    if T='' then
      Exit;
    SetLength(Toks,Cnt+1);
    Toks[Cnt]:=T;
    Inc(Cnt);
  end;

begin
  Result:=True;
  Spec:=CertificateData.ALPNProtocols;
  if Spec='' then
    Exit;                              // no ALPN: leave the session unchanged
  if not Assigned(gnutls_alpn_set_protocols) then
    Exit(False);                       // ALPN requested but unsupported by libgnutls
  Toks:=Nil;
  Cnt:=0;
  Start:=1;
  for I:=1 to Length(Spec) do
    if Spec[I]=',' then
      begin
      AddToken(Copy(Spec,Start,I-Start));
      Start:=I+1;
      end;
  AddToken(Copy(Spec,Start,Length(Spec)-Start+1));   // trailing token
  if Cnt=0 then
    Exit;                              // only empty tokens
  // One datum per protocol; GnuTLS copies the names, so the locals may go out of
  // scope after the call. Server precedence so our offer order wins.
  SetLength(Datums,Cnt);
  for I:=0 to Cnt-1 do
    begin
    Datums[I].data:=PByte(PAnsiChar(Toks[I]));
    Datums[I].size:=Length(Toks[I]);
    end;
  Result:=CheckOK(gnutls_alpn_set_protocols(FSession,@Datums[0],Cnt,GNUTLS_ALPN_SERVER_PRECEDENCE));
end;

function TGNUTLSSocketHandler.GetSelectedALPNProtocol: string;

var
  D : Tgnutls_datum_t;

begin
  Result:='';
  if (FSession=Nil) or not Assigned(gnutls_alpn_get_selected_protocol) then
    Exit;
  D.data:=Nil;
  D.size:=0;
  // Datum points into session-owned memory: copy it, never free it.
  if (gnutls_alpn_get_selected_protocol(FSession,@D)=GNUTLS_E_SUCCESS)
     and (D.size>0) and (D.data<>Nil) then
    SetString(Result,PAnsiChar(D.data),D.size);
end;

procedure TGNUTLSSocketHandler.GetNegotiatedTLSNames(out aVersion, aCipher, aKX: string);

  function NameOf(P : PAnsiChar) : string;
  begin
    if P<>Nil then
      Result:=StrPas(P)
    else
      Result:='';
  end;

begin
  aVersion:='';
  aCipher:='';
  aKX:='';
  if FSession=Nil then
    Exit;
  if Assigned(gnutls_protocol_get_name) and Assigned(gnutls_protocol_get_version) then
    aVersion:=NameOf(gnutls_protocol_get_name(gnutls_protocol_get_version(FSession)));
  if Assigned(gnutls_cipher_get_name) and Assigned(gnutls_cipher_get) then
    aCipher:=NameOf(gnutls_cipher_get_name(gnutls_cipher_get(FSession)));
  if Assigned(gnutls_kx_get_name) and Assigned(gnutls_kx_get) then
    aKX:=NameOf(gnutls_kx_get_name(gnutls_kx_get(FSession)));
end;

function TGNUTLSSocketHandler.NegotiatedH2TLSFloorMet: Boolean;

var
  Ver,Ciph,KX : string;

begin
  GetNegotiatedTLSNames(Ver,Ciph,KX);
  Result:=H2TLSFloorMet(Ver,Ciph,KX);
end;

function TGNUTLSSocketHandler.Accept: Boolean;

var
  Ver,Ciph,KX : string;

begin
  Result:=InitSession(True);
  if Result then
    Result:=DoHandShake;
  // RFC 9113 §9.2: a negotiated h2 connection MUST meet the TLS floor; otherwise
  // refuse it. Only inspected when h2 was actually negotiated, so plain TLS and
  // HTTP/1.1-over-TLS connections are never affected.
  if Result and (GetSelectedALPNProtocol='h2') then
    begin
    GetNegotiatedTLSNames(Ver,Ciph,KX);
    if not H2TLSFloorMet(Ver,Ciph,KX) then
      begin
      SetGNUTLSLastErrorString(Format(SErrH2InadequateSecurity,[Ver,Ciph,KX]));
      DoneSession;
      Result:=False;
      end;
    end;
  SetSSLActive(Result);
end;


function TGNUTLSSocketHandler.Shutdown(BiDirectional : Boolean): boolean;

begin
  Result:=assigned(FSession);
  if Result then
    If Not BiDirectional then
      gnutls_bye(FSession, GNUTLS_SHUT_WR)
    else
      begin
      Result:=CheckOK(gnutls_bye(FSession, GNUTLS_SHUT_RDWR));
      if Result then
        Result:=fpShutdown(Socket.Handle,1)=0;
      end;
  If Result then
    Result:=DoneSession;
end;

function TGNUTLSSocketHandler.Send(Const Buffer; Count: Integer): Integer;

Var
  P : PByte;

begin
  P:=PByte(@Buffer);
  repeat
    Result:=Check(gnutls_record_send(FSession,P,Count));
  until (Result <> GNUTLS_E_AGAIN) and (Result <> GNUTLS_E_INTERRUPTED);
  if Result<0 then
    Result:=-1;
end;

function TGNUTLSSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

Var
  P : PByte;

begin
  P:=PByte(@Buffer);
  repeat
    Result:=Check(gnutls_record_recv(FSession,P,Count));
  until (Result <> GNUTLS_E_AGAIN) and (Result <> GNUTLS_E_INTERRUPTED);
  if Result<0 then
    Result:=-1;
end;

function TGNUTLSSocketHandler.BytesAvailable: Integer;
begin
  Result:=gnutls_record_check_pending(FSession);
end;

Function TGNUTLSSocketHandler.GNUTLSLastError: integer;
begin
  Result:=FGNUTLSLastError;
end;

function TGNUTLSSocketHandler.GetLastSSLErrorString: String;
begin
  Result:=FGNUTLSLastErrorString;
end;

function TGNUTLSSocketHandler.GetLastSSLErrorCode: Integer;
begin
  Result:=FGNUTLSLastError;
end;

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TGNUTLSSocketHandler);
end.

