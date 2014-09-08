{$mode objfpc}
{$h+}
program genkeypair;

uses sysutils, openssl;

// This is normally only used when you specify a cipher for encoding the private key.

function PasswordCallback(buf:PAnsiChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;

begin
  Result:=0;
  Buf^:=#0;
end;

procedure DoKey(Const FNPrivate, FNPublic : String; AKeySize : Integer = 1024);

  Procedure RaiseErr(Const Msg : String);

  Var
    Err : String;

  begin
    SetLength(Err,1024);
    ErrErrorString(ErrGetError,Err,1024);
    Raise Exception.Create(Msg+' : '+Err);
  end;

  Function GetKey(K : pBIO) : String;

  Var
    L : Integer;
    p : pchar;

  begin
    l:=BIO_ctrl(K,BIO_CTRL_INFO,0,PChar(@P));
    setlength(Result,l);
    move(P^,Result[1],l);
  end;

  Procedure WriteKey(Const FN,Key : String);

  Var
    F : Text;

  begin
    Assign(F,FN);
    Rewrite(F);
    try
      Write(F,Key);
    finally
      Close(F);
    end;
  end;


Var
  rsa: PRSA;
  PK :PEVP_PKEY;
  PrivKey, PubKey: pBIO;
  Key : string;

begin
  InitLibeaInterface(true);
  InitSSLEAInterface(true);
  InitSSLInterface(true);
  ERR_load_crypto_strings;
  OpenSSL_add_all_ciphers;
  pk := EvpPkeynew;
  if (pk=Nil) then
    Raise exception.Create('Could not create key structure.');
  rsa:=RsaGenerateKey(AKeySize,$10001,Nil,Nil);
  if rsa=nil then
    Raise exception.Create('Could not create RSA key.');
  if EvpPkeyAssign(pk, EVP_PKEY_RSA, rsa)=0 then
    Raise exception.Create('Could not assign created RSA key to key structure.');
  // Generate private key
  PrivKey:=BIOnew(BIOsmem);
  if PrivKey=Nil then
    Raise exception.Create('Could not allocate BIO structure for private key.');
  try
    if PEM_write_bio_PrivateKey(PrivKey, PK, nil, nil, 0, @PasswordCallBack, Nil)=0 then
      RaiseErr('Could not write private key');
    Key:=GetKey(PrivKey);
    WriteKey(FNPrivate,Key);
  finally
    BioFreeAll(PrivKey);
  end;
  // Get public key
  PubKey:= BIOnew(BIOsmem);
  if PubKey=Nil then
    Raise exception.Create('Could not allocate BIO structure for public key.');
  try
    if PEM_write_bio_PubKey(PubKey,PK)=0 then
      RaiseErr('Could not write public key');
    Key:=GetKey(PubKey);
    WriteKey(FNPublic,Key);
  finally
    BioFreeAll(PubKey);
  end;
end;

begin
  writeln('Writing private/public key of length 1024 to id_rsa/id_rsa.pub');
  DoKey('id_rsa','id_rsa.pub',1024);
end.

