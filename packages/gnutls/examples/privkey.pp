{
  Simple low-level example for how generate a RSA private key (4096 bytes) using
  the GnuTLS binding.

  Author(s): Silvio Clecio (silvioprog), Michael Van Canneyt
  Date: Mon Jan  7 01:36:18 -03 2019
  GnuTLS version: 3.4+
}

program privkey;

{$MODE OBJFPC}{$H+}
{$ASSERTIONS ON}

uses
  sysutils,
  ctypes,
  gnutls;

type
  EGnuTLS = Exception;

procedure TLSCheckRet(Aret: cint); inline;
begin
  if Aret <> GNUTLS_E_SUCCESS then
    raise EGnuTLS.Create(gnutls_strerror(Aret));
end;

var
  priv_key: AnsiString = '';
  priv_key_size: cuint;
  key: Tgnutls_x509_privkey_t;
begin
  LoadGnuTLS;
  Assert(GnuTLSLoaded);
  key:=nil;
  try
    TLSCheckRet(gnutls_x509_privkey_init(@key));
    priv_key_size := gnutls_sec_param_to_pk_bits(GNUTLS_PK_RSA,GNUTLS_SEC_PARAM_HIGH);
    SetLength(priv_key, Pred(priv_key_size));
    TLSCheckRet(gnutls_x509_privkey_generate(key, GNUTLS_PK_RSA, priv_key_size, 0));
    TLSCheckRet(gnutls_x509_privkey_export(key, GNUTLS_X509_FMT_PEM, @priv_key[1], @priv_key_size));
    SetLength(priv_key, Pred(priv_key_size));
    WriteLn(priv_key);
  finally
    if key<>Nil then
      gnutls_x509_privkey_deinit(key);
    FreeGnuTLS;
  end;
end.
