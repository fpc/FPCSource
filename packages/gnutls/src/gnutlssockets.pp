unit gnutlssockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, ssockets, sslsockets, dateUtils,
  cTypes, sslbase, gnutls;

Const
  DefCertSize = 8192;

Type
  EGnuTLS = Class(Exception);

  { TGNUTLSSocketHandler }

  TGNUTLSSocketHandler = Class(TSSLSocketHandler)
  Private
    FSession : tgnutls_session_t;
    FCred : tgnutls_certificate_credentials_t;
    FGNUTLSLastErrorString: string;
    FGNUTLSLastError : Integer;
    FCurrentHostName : String;
    FCurrentCypherlist : String;
    function LoadTrustedCertificate(aCertData: TSSLData): Boolean;
    function MaybeAllocateCredentials: Boolean;
    procedure SetGNUTLSLastErrorString(AValue: string);
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
    Function GNUTLSLastError: integer;
    property GNUTLSLastErrorString: string read FGNUTLSLastErrorString write SetGNUTLSLastErrorString;
  end;

  { TGNUTLSX509Certificate }

  TGNUTLSX509Certificate = class(TX509Certificate)
  private
    FMyFormat : tgnutls_x509_crt_fmt_t;
    procedure Check(Aret: cint);
    procedure Check(Aexp: Boolean; Aret: cint);
    function GenCACert(const Aca_priv_key: TBytes; const Acommon_name, Aserial: AnsiString; Adays: Word): TBytes;
    function GenPrivKey: TBytes;
    function GenSrvCert(const Aca_priv_key, Aca_pem, Asrv_priv_key: TBytes; const Acommon_name, Aorganization, Aserial: AnsiString;
      Adays: Word): TBytes;
  public
    constructor create;
    function CreateCertificateAndKey: TCertAndKey; override;
  end;

implementation

{ TSocketHandler }

Procedure MaybeInitGNUTLS;

begin
  if not GnuTLSloaded then
     LoadGnuTLS();
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

procedure TGNUTLSSocketHandler.SetGNUTLSLastErrorString(AValue: string);
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
    FCurrentHostName:=(Socket as TInetSocket).Host;
    if SendHostAsSNI then
      begin
      Result:=CheckOK(gnutls_server_name_set(FSession, GNUTLS_NAME_DNS,pchar(FCurrentHostName), length(FCurrentHostName)));
      if not Result then
        exit;
      end;
    gnutls_session_set_verify_cert(Fsession,pchar(FCurrentHostName),0);
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
  P : Pchar;

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
    end;
  Result:=FCurrentCypherlist;
end;

function TGNUTLSSocketHandler.InitSession(AsServer: Boolean): Boolean;

Const
  InitFlags : Array[Boolean] of cInt = (GNUTLS_CLIENT,GNUTLS_SERVER);

Var
  flags :Cint;
  errPtr : Pchar;

begin
  Flags:=InitFlags[AsServer];
  Result:=CheckOK(gnutls_init(@FSession,Flags));
  if not Result then
    exit;
  Result:=CheckOK(gnutls_priority_set_direct(FSession, PChar(GetCipherListString), @errptr));
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
    DoneSession;
{
S:=CertificateData.CipherList;
FCTX.SetVerify(VO[VerifypeerCert],Nil);
FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
FCTX.SetDefaultPasswdCbUserdata(self);
}
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
    Result:=CheckOK(gnutls_certificate_set_x509_key_file (FCred, pChar(aData.FileName),PChar(aKey.FileName),GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_key_file (FCred, pChar(aData.FileName),PChar(aKey.FileName),GNUTLS_X509_FMT_DER));
    end;
  if Result then
    Result:=CheckOK(gnutls_credentials_set(FSession, GNUTLS_CRD_CERTIFICATE, FCred));
end;

function TGNUTLSSocketHandler.LoadTrustedCertificate(aCertData : TSSLData) : Boolean;

var
  ca : tgnutls_datum_t;

begin
  MaybeAllocateCredentials;
  Result:=False;
  if (aCertData.FileName<>'') then
    begin
    Result:=CheckOK(gnutls_certificate_set_x509_trust_file(FCred,PChar(aCertData.FileName),GNUTLS_X509_FMT_PEM));
    if not Result then
      Result:=CheckOK(gnutls_certificate_set_x509_trust_file(FCred,PChar(aCertData.FileName),GNUTLS_X509_FMT_DER));
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
  P : PChar;

begin
  MaybeAllocateCredentials;
  P:=PAnsiChar(aFileName);
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

function TGNUTLSSocketHandler.Accept: Boolean;

begin
  Result:=InitSession(True);
  if Result then
    Result:=DoHandShake;
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
  Result:=Check(gnutls_record_send(Fsession,P,Count));
  if Result<0 then
    Result:=-1;
end;

function TGNUTLSSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

Var
  P : PByte;

begin
  P:=PByte(@Buffer);
  Result:=Check(gnutls_record_recv(FSession,P,Count));
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

