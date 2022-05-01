unit rsautil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ctypes, libgmp, libnettle;

// Read an RSA key in s-expression format

Function ReadRSAKey(const aFileName : String; out aPublic : trsa_public_key; out aPrivate : trsa_private_key) : Boolean;
Function ReadRSAKey(const aFileName : String; out aPublic : trsa_public_key) : Boolean;
Function ReadRSAKey(const aFileName : String; out aPrivate : trsa_private_key) : Boolean;
function HashFile(aHash : pnettle_hash; Ctx : Pointer; aFileName : string) : Boolean;
function HashStream(aHash : pnettle_hash; Ctx : Pointer; F : TStream) : Boolean;
Function ReadSignature(const aFileName : String; out s : tmpz_t) : Boolean;
Function MpzToString(s : tmpz_t) : string;

implementation

Function MpzToString(s : tmpz_t) : string;

Var
  P : pcchar;

begin
  Result:='';
  SetLength(result,4096);
  P:=mpz_get_str(pcchar(Result),16,@s);
  Setlength(Result,StrLen(Pchar(P)));
end;

function HashFile(aHash : pnettle_hash; Ctx : Pointer; aFileName : string) : Boolean;

Var
  F : TFileStream;

begin
  Writeln('Hashing file ',aFileName);
  F:=TFileStream.Create(aFileName,fmOpenRead);
  try
    Result:=HashStream(aHash,ctx,F);
  Finally
    F.Free;
  end;
end;

Function HashStream(aHash : pnettle_hash; Ctx : Pointer; F : TStream) : Boolean;

Const
   BufSize = 2048;

Type
  TBuffer = Array[0..BufSize-1] of Byte;

Var
  Buf : TBuffer;
  N : Integer;

begin
  Writeln('Hashing stream...');
  Buf:=Default(TBuffer);
  Repeat
    N:=F.Read(Buf,BufSize);
    aHash^.update(ctx, N, @buf);
  until (N<BufSize);
  Result:=True;
end;

Function ReadSignature(const aFileName : String; out s : tmpz_t) : Boolean;

Var
  B : TBytes;
  F : TFileStream;

begin
  Writeln('reading signature string from ',aFileName);
  mpz_init(@s);
  B:=[];
  F:=TFileStream.Create(aFileName,fmOpenRead);
  try
    SetLength(B,F.Size);
    F.ReadBuffer(B[0],Length(B));
    Result:=mpz_set_str(@s,Pcchar(B),16)=0;
  finally
    F.Free;
  end;
end;


Function ReadRSAKey(const aFileName : String; out aPublic : trsa_public_key; out aPrivate : trsa_private_key) : Boolean;

Var
  B : TBytes;
  F : TFileStream;

begin
  Writeln('reading RSA private/public key from ',aFileName);
  nettle_rsa_public_key_init(@aPublic);
  nettle_rsa_private_key_init(@aPrivate);
  B:=[];
  F:=TFileStream.Create(aFileName,fmOpenRead);
  try
    SetLength(B,F.Size);
    F.ReadBuffer(B[0],Length(B));
    Result:=nettle_rsa_keypair_from_sexp(@aPublic,@aPrivate,0,Length(B),PByte(B))<>0;
  finally
    F.Free;
  end;
end;

Function ReadRSAKey(const aFileName : String; out aPublic : trsa_public_key) : Boolean;

Var
  B : TBytes;
  F : TFileStream;

begin
  Writeln('reading RSA public key from ',aFileName);
  nettle_rsa_public_key_init(@aPublic);
  B:=[];
  F:=TFileStream.Create(aFileName,fmOpenRead);
  try
    SetLength(B,F.Size);
    F.ReadBuffer(B[0],Length(B));
    Result:=nettle_rsa_keypair_from_sexp(@aPublic,Nil,0,Length(B),PByte(B))<>0;
  finally
    F.Free;
  end;
end;

Function ReadRSAKey(const aFileName : String; out aPrivate : trsa_private_key) : Boolean;

Var
  B : TBytes;
  F : TFileStream;

begin
  Writeln('reading RSA private key from ',aFileName);
  nettle_rsa_private_key_init(@aPrivate);
  B:=[];
  F:=TFileStream.Create(aFileName,fmOpenRead);
  try
    SetLength(B,F.Size);
    F.ReadBuffer(B[0],Length(B));
    Result:=nettle_rsa_keypair_from_sexp(Nil,@aPrivate,0,Length(B),PByte(B))<>0;
  finally
    F.Free;
  end;
end;


end.

