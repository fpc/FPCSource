program rsaverify;

uses classes, libnettle, rsautil, libgmp, ctypes;

Procedure Stop(Msg : string);

begin
  Writeln(Msg);
  Halt(1);
end;

Var
  pubkey : Trsa_public_key;
  hash : tsha1_ctx;
  s : Tmpz_t;

begin
  LoadLibGMP;
  InitializeNettle;
  if (ParamCount<>3) then
    Stop('Usage : rsasign pubkeyfile file signaturefile'+slinebreak+'Signature in base 16');
  if not  ReadRSAKey(ParamStr(1),pubkey) then
    Stop('Failed to read key from '+ParamStr(1));
  nettle_sha1_init(@hash);
  HashFile(nettle_sha1,@Hash,ParamStr(2));
  if not ReadSignature(paramstr(3),s) then
   Stop('Failed to read key from '+ParamStr(3));
  if (nettle_rsa_sha1_verify(@pubkey, @hash, @s)=0) then
    Stop('Failed to verify signature')
  else
    Stop('Signature verifies OK')
end.

