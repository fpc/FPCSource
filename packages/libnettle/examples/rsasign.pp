program rsasign;

uses classes, libnettle, rsautil, libgmp, ctypes;

Procedure Stop(Msg : string);

begin
  Writeln(Msg);
  Halt(1);
end;

Var
  key : Trsa_private_key;
  hash : tsha1_ctx;
  F : TFileStream;
  s : Tmpz_t;
  res : ansistring;

begin
  LoadLibGMP;
  InitializeNettle();
  if ParamCount<2 then
    Stop('Usage : rsasign keyfile file [signaturefile]');
  if not  ReadRSAKey(ParamStr(1),key) then
    Stop('Failed to read key from '+ParamStr(1));
  nettle_sha1_init(@hash);
  HashFile(nettle_sha1,@hash,ParamStr(2));
  mpz_init(@s);
  if (nettle_rsa_sha1_sign(@key, @hash, @s)=0) then
    Stop('RSA key too small');
  Res:=mpzToString(s);
  Writeln('Signature : ',Res);
  if ParamStr(3)<>'' then
    begin
    F:=TFileStream.Create(ParamStr(3),fmCreate);
    try
      F.WriteBuffer(Res[1],Length(Res));
      Writeln('Signature written to file: ',F.FileName);
    finally
      F.Free;
    end;
    end;
  mpz_clear(s);
  nettle_rsa_private_key_clear(@key);
end.

