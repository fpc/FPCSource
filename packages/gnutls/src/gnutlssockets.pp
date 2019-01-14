{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Michael Van Canneyt, member of the Free Pascal development team

    FPC SSockets SSL support using GnuTLS library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit gnutlssockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, ssockets, sslsockets, cTypes, sslbase, gnutls;

Type

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

  public
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

function TGNUTLSX509Certificate.CreateCertificateAndKey: TCertAndKey;
begin
  Result:=Default(TCertAndKey);
  Raise ENotImplemented.Create('No certificate generation yet');
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
    Result:=(not DoVerifyCert);
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

Const
   DefaultCerts : PChar =  '/etc/ssl/certs/ca-certificates.crt';

begin
  Result:=(FSession<>Nil);
  if not Result then
    Exit;
  if not (CertificateData.NeedCertificateData) then
    Result:=LoadCertificate(CertificateData.Certificate,CertificateData.PrivateKey);
  if Result and Not CertificateData.TrustedCertificate.Empty then
    Result:=LoadTrustedCertificate(CertificateData.TrustedCertificate);
  if Result and (CertificateData.CertCA.FileName<>'') then
    Result:=Result and SetTrustedCertificateDir(CertificateData.CertCA.FileName);
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
  Ret : Integer;
  P : PByte;

begin
  Result:=0;
  P:=PByte(@Buffer);
  Repeat
    Ret:=Check(gnutls_record_send(Fsession,P,Count));
    if Ret>0 then
      begin
      Result:=Result+Ret;
      Inc(P,Ret);
      end;
  Until (Result=Count) or ((Ret<0) and (gnutls_error_is_fatal(ret)<>0));
  if Result=Count then
    exit;
  if Ret<0 then
    Result:=-1;
end;

function TGNUTLSSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

Var
  Ret : Integer;
  P : PByte;

begin
  Result:=0;
  P:=PByte(@Buffer);
  Repeat
    Ret:=Check(gnutls_record_recv(FSession,P,Count));
    if Ret>0 then
      begin
      Result:=Result+Ret;
      Inc(P,Ret);
      Dec(Count,Ret)
      end;
  Until (Count=0) or ((Ret<0) and (gnutls_error_is_fatal(ret)<>0));
  if Count=0 then
    exit;
  if Ret<0 then
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

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TGNUTLSSocketHandler);
end.

