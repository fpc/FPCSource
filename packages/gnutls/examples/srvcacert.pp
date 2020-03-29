{
  Simple low-level example using the GnuTLS binding for how generate an own CA
  and self-signed certificate for HTTP server and client.

  Author: Silvio Clecio (silvioprog)
  Date: Wed Jan  9 03:10:58 BRT 2019
  GnuTLS version: 3.4+

  Testing the generated files.

  Server side:

    gnutls-serv --port 8080 --http \
      --x509cafile ca.pem \
      --x509keyfile server.key \
      --x509certfile server.pem

  Client side:

    curl -k --key client.key --cert client.pem https://localhost:8080

  or:

    gnutls-cli localhost --port 8080 --insecure \
      --x509keyfile client.key \
      --x509certfile client.pem
}

program srvcacert;

{$MODE OBJFPC}{$H+}
{$ASSERTIONS ON}

uses
  sysutils,
  classes,
  dateutils,
  ctypes,
  gnutls;

const
  CA_KEY = 'ca.key';
  CA_PEM = 'ca.pem';
  SERVER_KEY = 'server.key';
  SERVER_PEM = 'server.pem';
  CLIENT_KEY = 'client.key';
  CLIENT_PEM = 'client.pem';

  CERT_SIZE = 4096;

type
  EGnuTLS = Exception;

procedure Save(const S: AnsiString; const AFileName: TFileName);
begin
  with TStringStream.Create(S) do
  try
    SaveToFile(AFileName);
  finally
    Free;
  end;
end;

procedure TLSCheckRet(Aret: cint); inline;
begin
  if Aret <> GNUTLS_E_SUCCESS then
    raise EGnuTLS.Create(gnutls_strerror(Aret));
end;

procedure TLSCheck(Aexp: Boolean; Aret: cint); inline;
begin
  if Aexp then
    raise EGnuTLS.Create(gnutls_strerror(Aret));
end;

procedure TLSGenPrivKey(out Apriv_key: AnsiString);
var
  Vkey: Tgnutls_x509_privkey_t;
  Vpriv_key_size: cuint;
begin
  try
    TLSCheckRet(gnutls_x509_privkey_init(@Vkey));
    Vpriv_key_size := gnutls_sec_param_to_pk_bits(GNUTLS_PK_RSA,
      GNUTLS_SEC_PARAM_HIGH);
    Apriv_key := '';
    SetLength(Apriv_key, Pred(Vpriv_key_size));
    TLSCheckRet(gnutls_x509_privkey_generate(Vkey, GNUTLS_PK_RSA,
      Vpriv_key_size, 0));
    TLSCheckRet(gnutls_x509_privkey_export(Vkey, GNUTLS_X509_FMT_PEM,
      @Apriv_key[1], @Vpriv_key_size));
    SetLength(Apriv_key, Pred(Vpriv_key_size));
  except
    gnutls_x509_privkey_deinit(Vkey);
    raise;
  end;
end;

procedure TLSGenCACert(const Aca_priv_key: AnsiString; out Aca_pem: AnsiString;
  const Acommon_name, Aserial: AnsiString; Adays: Word);
var
  Vkey: Tgnutls_x509_privkey_t;
  Vcrt: Tgnutls_x509_crt_t = nil;
  Vdata: Tgnutls_datum_t;
  Vkeyid: AnsiString = '';
  Vkeyidsize: csize_t;
  Vactivation: ttime_t;
  Vca_pem_size: csize_t;
  Vret: cint;
begin
  try
    TLSCheckRet(gnutls_x509_privkey_init(@Vkey));
    Vdata.data := @Aca_priv_key[1];
    Vdata.size := Length(Aca_priv_key);
    TLSCheckRet(gnutls_x509_privkey_import(Vkey, @Vdata, GNUTLS_X509_FMT_PEM));
    TLSCheckRet(gnutls_x509_crt_init(@Vcrt));
    TLSCheckRet(gnutls_x509_crt_set_key(Vcrt, Vkey));
    TLSCheckRet(gnutls_x509_crt_set_dn_by_oid(Vcrt, GNUTLS_OID_X520_COMMON_NAME,
      0, @Acommon_name[1], Length(Acommon_name)));
    TLSCheckRet(gnutls_x509_crt_set_version(Vcrt, 3));
    TLSCheckRet(gnutls_x509_crt_set_serial(Vcrt, @Aserial[1], Length(Aserial)));
    Vactivation := DateTimeToUnix(Now,False);
    TLSCheckRet(gnutls_x509_crt_set_activation_time(Vcrt, Vactivation));
    TLSCheckRet(gnutls_x509_crt_set_expiration_time(Vcrt,
      Vactivation + (Adays * 86400)));
    TLSCheckRet(gnutls_x509_crt_set_ca_status(Vcrt, Ord(True)));
    TLSCheckRet(gnutls_x509_crt_set_key_usage(Vcrt, GNUTLS_KEY_KEY_CERT_SIGN));
    Vkeyidsize := 0;
    Vret := gnutls_x509_crt_get_key_id(Vcrt, GNUTLS_KEYID_USE_SHA1, nil,
      @Vkeyidsize);
    TLSCheck((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or (Vkeyidsize < 1), Vret);
    SetLength(Vkeyid, Pred(Vkeyidsize));
    TLSCheckRet(gnutls_x509_crt_get_key_id(Vcrt, GNUTLS_KEYID_USE_SHA1,
      @Vkeyid[1], @Vkeyidsize));
    TLSCheckRet(gnutls_x509_crt_set_subject_key_id(Vcrt, @Vkeyid[1], Vkeyidsize));
    TLSCheckRet(gnutls_x509_crt_sign2(Vcrt, Vcrt, Vkey, GNUTLS_DIG_SHA256, 0));
    Aca_pem := '';
    Vca_pem_size := CERT_SIZE;
    SetLength(Aca_pem, Pred(Vca_pem_size));
    TLSCheckRet(gnutls_x509_crt_export(Vcrt, GNUTLS_X509_FMT_PEM, @Aca_pem[1],
      @Vca_pem_size));
    SetLength(Aca_pem, Pred(Vca_pem_size));
  except
    gnutls_x509_privkey_deinit(Vkey);
    gnutls_x509_crt_deinit(Vcrt);
    raise;
  end;
end;

procedure TLSGenSrvCert(const Aca_priv_key, Aca_pem, Asrv_priv_key: AnsiString;
  out Asrv_pem: AnsiString; const Acommon_name, Aorganization,
  Aserial: AnsiString; Adays: Word);
var
  Vsrv_key: Tgnutls_x509_privkey_t = nil;
  Vca_key: Tgnutls_x509_privkey_t = nil;
  Vca_crt: Tgnutls_x509_crt_t = nil;
  Vsrv_crt: Tgnutls_x509_crt_t = nil;
  Vdata: Tgnutls_datum_t;
  Vkeyid: AnsiString = '';
  Vkeyidsize: csize_t;
  Vactivation: ttime_t;
  Vsrv_pem_size: csize_t;
  Vret: cint;
begin
  try
  TLSCheckRet(gnutls_x509_privkey_init(@Vca_key));
  Vdata.data := @Aca_priv_key[1];
  Vdata.size := Length(Aca_priv_key);
  TLSCheckRet(gnutls_x509_privkey_import(Vca_key, @Vdata, GNUTLS_X509_FMT_PEM));
  TLSCheckRet(gnutls_x509_privkey_init(@Vsrv_key));
  Vdata.data := @Asrv_priv_key[1];
  Vdata.size := Length(Asrv_priv_key);
  TLSCheckRet(gnutls_x509_privkey_import(Vsrv_key, @Vdata, GNUTLS_X509_FMT_PEM));
  TLSCheckRet(gnutls_x509_crt_init(@Vca_crt));
  Vdata.data := @Aca_pem[1];
  Vdata.size := Length(Aca_pem);
  TLSCheckRet(gnutls_x509_crt_import(Vca_crt, @Vdata, GNUTLS_X509_FMT_PEM));
  TLSCheckRet(gnutls_x509_crt_init(@Vsrv_crt));
  TLSCheckRet(gnutls_x509_crt_set_key(Vsrv_crt, Vsrv_key));
  TLSCheckRet(gnutls_x509_crt_set_dn_by_oid(Vsrv_crt,
    GNUTLS_OID_X520_COMMON_NAME, 0, @Acommon_name[1], Length(Acommon_name)));
  TLSCheckRet(gnutls_x509_crt_set_dn_by_oid(Vsrv_crt,
    GNUTLS_OID_X520_ORGANIZATION_NAME, 0, @Aorganization[1],
    Length(Aorganization)));
  TLSCheckRet(gnutls_x509_crt_set_version(Vsrv_crt, 3));
  TLSCheckRet(gnutls_x509_crt_set_serial(Vsrv_crt, @Aserial[1],
    Length(Aserial)));
  Vactivation := DateTimeToUnix(Now,False);
  TLSCheckRet(gnutls_x509_crt_set_activation_time(Vsrv_crt, Vactivation));
  TLSCheckRet(gnutls_x509_crt_set_expiration_time(Vsrv_crt,
    Vactivation + (Adays * 86400)));
  TLSCheckRet(gnutls_x509_crt_set_ca_status(Vsrv_crt, Ord(False)));
  TLSCheckRet(gnutls_x509_crt_set_key_purpose_oid(Vsrv_crt,
    @GNUTLS_KP_TLS_WWW_SERVER[1], Ord(False)));
  Vkeyidsize := 0;
  Vret := gnutls_x509_crt_get_subject_key_id(Vca_crt, nil, @Vkeyidsize, nil);
  TLSCheck((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or  (Vkeyidsize < 1), Vret);
  SetLength(Vkeyid, Pred(Vkeyidsize));
  TLSCheckRet(gnutls_x509_crt_get_subject_key_id(Vca_crt, @Vkeyid[1],
    @Vkeyidsize, nil));
  TLSCheckRet(gnutls_x509_crt_set_subject_key_id(Vsrv_crt, @Vkeyid[1],
    Vkeyidsize));
  Vkeyidsize := 0;
  gnutls_x509_crt_get_key_id(Vsrv_crt, GNUTLS_KEYID_USE_SHA1, nil, @Vkeyidsize);
  TLSCheck((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or (Vkeyidsize < 1), Vret);
  SetLength(Vkeyid, Pred(Vkeyidsize));
  TLSCheckRet(gnutls_x509_crt_get_key_id(Vsrv_crt, GNUTLS_KEYID_USE_SHA1,
    @Vkeyid[1], @Vkeyidsize));
  TLSCheckRet(gnutls_x509_crt_set_authority_key_id(Vsrv_crt,
    @Vkeyid[1], Vkeyidsize));
  TLSCheckRet(gnutls_x509_crt_sign2(Vsrv_crt, Vca_crt, Vca_key,
    GNUTLS_DIG_SHA256, 0));
  Vsrv_pem_size := CERT_SIZE;
  Asrv_pem := '';
  SetLength(Asrv_pem, Pred(Vsrv_pem_size));
  TLSCheckRet(gnutls_x509_crt_export(Vsrv_crt, GNUTLS_X509_FMT_PEM,
    @Asrv_pem[1], @Vsrv_pem_size));
  SetLength(Asrv_pem, Vsrv_pem_size);
  except
    gnutls_x509_privkey_deinit(Vsrv_key);
    gnutls_x509_privkey_deinit(Vca_key);
    gnutls_x509_crt_deinit(Vca_crt);
    gnutls_x509_crt_deinit(Vsrv_crt);
    raise;
  end;
end;

procedure TLSGenCliCert(const Aca_priv_key, Aca_pem, Acli_priv_key: AnsiString;
  out Acli_pem: AnsiString; const Acommon_name, Aserial: AnsiString;
  Adays: Word);
var
  Vcli_key: Tgnutls_x509_privkey_t = nil;
  Vca_key: Tgnutls_x509_privkey_t = nil;
  Vca_crt: Tgnutls_x509_crt_t = nil;
  Vcli_crt: Tgnutls_x509_crt_t = nil;
  Vdata: Tgnutls_datum_t;
  Vkeyid: AnsiString = '';
  Vkeyidsize: csize_t;
  Vactivation: ttime_t;
  Vcli_pem_size: csize_t;
  Vret: cint;
begin
  try
    TLSCheckRet(gnutls_x509_privkey_init(@Vca_key));
    Vdata.data := @Aca_priv_key[1];
    Vdata.size := Length(Aca_priv_key);
    TLSCheckRet(gnutls_x509_privkey_import(Vca_key, @Vdata,
      GNUTLS_X509_FMT_PEM));
    TLSCheckRet(gnutls_x509_privkey_init(@Vcli_key));
    Vdata.data := @Acli_priv_key[1];
    Vdata.size := Length(Acli_priv_key);
    TLSCheckRet(gnutls_x509_privkey_import(Vcli_key, @Vdata,
      GNUTLS_X509_FMT_PEM));
    TLSCheckRet(gnutls_x509_crt_init(@Vca_crt));
    Vdata.data := @Aca_pem[1];
    Vdata.size := Length(Aca_pem);
    TLSCheckRet(gnutls_x509_crt_import(Vca_crt, @Vdata, GNUTLS_X509_FMT_PEM));
    TLSCheckRet(gnutls_x509_crt_init(@Vcli_crt));
    TLSCheckRet(gnutls_x509_crt_set_key(Vcli_crt, Vcli_key));
    TLSCheckRet(gnutls_x509_crt_set_dn_by_oid(Vcli_crt,
      GNUTLS_OID_X520_COMMON_NAME, 0, @Acommon_name[1], Length(Acommon_name)));
    TLSCheckRet(gnutls_x509_crt_set_version(Vcli_crt, 3));
    TLSCheckRet(gnutls_x509_crt_set_serial(Vcli_crt, @Aserial[1],
      Length(Aserial)));
    Vactivation := DateTimeToUnix(Now,False);
    TLSCheckRet(gnutls_x509_crt_set_activation_time(Vcli_crt, Vactivation));
    TLSCheckRet(gnutls_x509_crt_set_expiration_time(Vcli_crt,
      Vactivation + (Adays * 86400)));
    TLSCheckRet(gnutls_x509_crt_set_ca_status(Vcli_crt, Ord(False)));
    TLSCheckRet(gnutls_x509_crt_set_key_purpose_oid(Vcli_crt,
      @GNUTLS_KP_TLS_WWW_CLIENT[1], Ord(False)));
    Vkeyidsize := 0;
    Vret := gnutls_x509_crt_get_subject_key_id(Vca_crt, nil, @Vkeyidsize, nil);
    TLSCheck((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or (Vkeyidsize < 1), Vret);
    SetLength(Vkeyid, Pred(Vkeyidsize));
    TLSCheckRet(gnutls_x509_crt_get_subject_key_id(Vca_crt, @Vkeyid[1],
      @Vkeyidsize, nil));
    TLSCheckRet(gnutls_x509_crt_set_subject_key_id(Vcli_crt, @Vkeyid[1],
      Vkeyidsize));
    Vkeyidsize := 0;
    Vret := gnutls_x509_crt_get_key_id(Vca_crt, GNUTLS_KEYID_USE_SHA1,
      nil, @Vkeyidsize);
    TLSCheck((Vret <> GNUTLS_E_SHORT_MEMORY_BUFFER) or (Vkeyidsize < 1), Vret);
    SetLength(Vkeyid, Vkeyidsize);
    TLSCheckRet(gnutls_x509_crt_get_key_id(Vca_crt, GNUTLS_KEYID_USE_SHA1,
      @Vkeyid[1], @Vkeyidsize));
    TLSCheckRet(gnutls_x509_crt_set_authority_key_id(Vcli_crt, @Vkeyid[1],
      Vkeyidsize));
    TLSCheckRet(gnutls_x509_crt_sign2(Vcli_crt, Vca_crt, Vca_key,
      GNUTLS_DIG_SHA256, 0));
    Vcli_pem_size := CERT_SIZE;
    Acli_pem := '';
    SetLength(Acli_pem, Pred(Vcli_pem_size));
    TLSCheckRet(gnutls_x509_crt_export(Vcli_crt, GNUTLS_X509_FMT_PEM,
      @Acli_pem[1], @Vcli_pem_size));
    SetLength(Acli_pem, Vcli_pem_size);
  except
    gnutls_x509_privkey_deinit(Vcli_key);
    gnutls_x509_privkey_deinit(Vca_key);
    gnutls_x509_crt_deinit(Vca_crt);
    gnutls_x509_crt_deinit(Vcli_crt);
    raise;
  end;
end;

var
  ca_pkey, ca_crt, pkey, crt: AnsiString;
begin
  LoadGnuTLS;
  Assert(GnuTLSLoaded);
  try
    WriteLn('Generating ', CA_KEY);
    TLSGenPrivKey(ca_pkey);
    Save(ca_pkey, CA_KEY);
    WriteLn('Done!');

    WriteLn('Generating ', CA_PEM);
    TLSGenCACert(ca_pkey, ca_crt, 'GnuTLS test CA', '01', 365);
    Save(ca_crt, CA_PEM);
    WriteLn('Done!');

    WriteLn('Generating ', SERVER_KEY);
    TLSGenPrivKey(pkey);
    Save(pkey, SERVER_KEY);
    WriteLn('Done!');

    WriteLn('Generating ', SERVER_PEM);
    TLSGenSrvCert(ca_pkey, ca_crt, pkey, crt, 'test.gnutls.org',
      'GnuTLS test server', '01', 365);
    Save(crt, SERVER_PEM);
    WriteLn('Done!');

    WriteLn('Generating ', CLIENT_KEY);
    TLSGenPrivKey(pkey);
    Save(pkey, CLIENT_KEY);
    WriteLn('Done!');

    WriteLn('Generating ', CLIENT_PEM);
    TLSGenCliCert(ca_pkey, ca_crt, pkey, crt, 'GnuTLS test client', '01', 365);
    Save(crt, CLIENT_PEM);
    WriteLn('Done!');
  finally
    FreeGnuTLS;
  end;
end.
