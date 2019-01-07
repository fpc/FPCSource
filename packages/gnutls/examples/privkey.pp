{
  Simple Low-level example showing how to generate a
  RSA private key (4096 bytes) using the GnuTLS binding.

  Author: Silvio Clecio (silvioprog)
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
{  cchar = Byte;
  Pcchar = PAnsiChar;
  Pcsize_t = PNativeUInt;
  Pcvoid = Pointer;}

  EGnuTLS = Exception;


procedure CheckRet(ret: cint);
var
  P: Pchar;
  S: string;
begin
  if ret = GNUTLS_E_SUCCESS then
    Exit;
  P := gnutls_strerror(ret);
  S:=StrPas(P);// SetString(S, @P[0], Length(Pcchar(@P[0])));
  SetCodePage(RawByteString(S), CP_UTF8, False);
  raise EGnuTLS.Create(S);
end;

var
  priv_key: String;
  priv_key_size: cuint = SizeOf(priv_key);
  key: Tgnutls_x509_privkey_t;
  
begin
  LoadGnuTLS;
  try
    Assert(GnuTLSLoaded);
    try
      CheckRet(gnutls_x509_privkey_init(@key));
      priv_key_size := gnutls_sec_param_to_pk_bits(GNUTLS_PK_RSA, GNUTLS_SEC_PARAM_HIGH);
      setLength(Priv_key,priv_key_size*2);
      CheckRet(gnutls_x509_privkey_generate(key, GNUTLS_PK_RSA, priv_key_size, 0));
      CheckRet(gnutls_x509_privkey_export(key, GNUTLS_X509_FMT_PEM, @priv_key[1], @priv_key_size));
      setLength(Priv_key,priv_key_size);

      WriteLn(priv_key);
    except
      gnutls_x509_privkey_deinit(key);
      raise;
    end;
  finally
    FreeGnuTLS;
  end;
end.
