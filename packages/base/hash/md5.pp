{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements a MD5 digest algorithm.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Implements a MD5 digest algorithm (RFC 1321)
}

unit md5;

{$mode objfpc}
{$h+}

Interface

type
  PCardinal = ^Cardinal;
  TMD5Count = array[0..1] of Cardinal;
  TMD5State = array[0..3] of Cardinal;
  TMD5Block = array[0..15] of Cardinal;
  TMD5CBits = array[0..7] of byte;
  TMD5Digest = array[0..15] of byte;
  TMD5Buffer = array[0..63] of byte;
  TMD5Context = packed record
    State: TMD5State;
    Count: TMD5Count;
    Buffer: TMD5Buffer;
  end;

Const
  DefBufSize : Cardinal = 1024;

{ Raw methods }

procedure MD5Init(var Context: TMD5Context);
procedure MD5Update(var Context: TMD5Context; Var Buf; BufLen: cardinal);
procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);

{ Auxiliary methods }

function MD5String(M: string): TMD5Digest;
function MD5Buffer(Var Buf; BufLen: cardinal): TMD5Digest;
function MD5File(N: string): TMD5Digest;
function MD5File(N: string; Bufsize : Cardinal): TMD5Digest;
function MD5Print(D: TMD5Digest): String;
// based on an implementation by Matthias Fichtner
function MD5Match(D1, D2: TMD5Digest): boolean;

Implementation

Var
  PADDING: TMD5Buffer;

{ Transformations. }

function F(x,y,z: Cardinal): Cardinal;

begin
  Result:=(x and y) or ((not x) and z);
end;


function G(x,y,z: Cardinal): Cardinal;

begin
  Result:=(x and z) or (y and (not z));
end;


function H(x,y,z: Cardinal): Cardinal;

begin
  Result:=x xor y xor z;
end;


function I(x,y,z: Cardinal): Cardinal;

begin
  Result:=y xor (x or (not z));
end;


procedure rot(var x: Cardinal; n: Byte);

begin
  x:=(x shl n) or (x shr (32 - n));
end;


procedure FF(var a: Cardinal;b,c,d,x: Cardinal; s: Byte; ac: Cardinal);
begin
  inc(a,F(b,c,d)+x+ac);
  rot(a,s);
  inc(a,b);
end;


procedure GG(var a: Cardinal;b,c,d,x: Cardinal;s: Byte;ac: Cardinal);

begin
  inc(a,G(b,c,d)+x+ac);
  rot(a,s);
  inc(a,b);
end;


procedure HH(var a: Cardinal;b,c,d,x: Cardinal;s: Byte;ac: Cardinal);

begin
  inc(a,H(b,c,d)+x+ac);
  rot(a,s);
  inc(a,b);
end;


procedure II(var a: Cardinal;b,c,d,x: Cardinal;s: Byte;ac: Cardinal);

begin
  inc(a,I(b,c,d)+x+ac);
  rot(a,s);
  inc(a,b);
end;

// inverts the bytes of (Count div) 4 cardinals from source to target.

procedure Invert(Source,Target: pointer; Count: cardinal);

var
  S: PByte;
  T: PCardinal;
  I: cardinal;

begin
  S := Source;
  T := Target;
  for I := 1 to (Count div 4) do
    begin
    T^:=S[0] or (S[1] shl 8) or (S[2] shl 16) or (S[3] shl 24);
    inc(S,4);
    inc(T);
    end;
end;

procedure Transform(Buffer: pointer; var State: TMD5State);

var
  a, b, c, d: Cardinal;
  Block: TMD5Block;

begin
  Invert(Buffer, @Block, 64);
  a:=State[0];
  b:=State[1];
  c:=State[2];
  d:=State[3];
  FF(a,b,c,d,Block[0] , 7,$d76aa478);
  FF(d,a,b,c,Block[1] ,12,$e8c7b756);
  FF(c,d,a,b,Block[2] ,17,$242070db);
  FF(b,c,d,a,Block[3] ,22,$c1bdceee);
  FF(a,b,c,d,Block[4] , 7,$f57c0faf);
  FF(d,a,b,c,Block[5] ,12,$4787c62a);
  FF(c,d,a,b,Block[6] ,17,$a8304613);
  FF(b,c,d,a,Block[7] ,22,$fd469501);
  FF(a,b,c,d,Block[8] , 7,$698098d8);
  FF(d,a,b,c,Block[9] ,12,$8b44f7af);
  FF(c,d,a,b,Block[10],17,$ffff5bb1);
  FF(b,c,d,a,Block[11],22,$895cd7be);
  FF(a,b,c,d,Block[12], 7,$6b901122);
  FF(d,a,b,c,Block[13],12,$fd987193);
  FF(c,d,a,b,Block[14],17,$a679438e);
  FF(b,c,d,a,Block[15],22,$49b40821);
  GG(a,b,c,d,Block[1] , 5,$f61e2562);
  GG(d,a,b,c,Block[6] , 9,$c040b340);
  GG(c,d,a,b,Block[11],14,$265e5a51);
  GG(b,c,d,a,Block[0] ,20,$e9b6c7aa);
  GG(a,b,c,d,Block[5] , 5,$d62f105d);
  GG(d,a,b,c,Block[10], 9,$02441453);
  GG(c,d,a,b,Block[15],14,$d8a1e681);
  GG(b,c,d,a,Block[4] ,20,$e7d3fbc8);
  GG(a,b,c,d,Block[9] , 5,$21e1cde6);
  GG(d,a,b,c,Block[14], 9,$c33707d6);
  GG(c,d,a,b,Block[3] ,14,$f4d50d87);
  GG(b,c,d,a,Block[8] ,20,$455a14ed);
  GG(a,b,c,d,Block[13], 5,$a9e3e905);
  GG(d,a,b,c,Block[2] , 9,$fcefa3f8);
  GG(c,d,a,b,Block[7] ,14,$676f02d9);
  GG(b,c,d,a,Block[12],20,$8d2a4c8a);
  HH(a,b,c,d,Block[5] , 4,$fffa3942);
  HH(d,a,b,c,Block[8] ,11,$8771f681);
  HH(c,d,a,b,Block[11],16,$6d9d6122);
  HH(b,c,d,a,Block[14],23,$fde5380c);
  HH(a,b,c,d,Block[1] , 4,$a4beea44);
  HH(d,a,b,c,Block[4] ,11,$4bdecfa9);
  HH(c,d,a,b,Block[7] ,16,$f6bb4b60);
  HH(b,c,d,a,Block[10],23,$bebfbc70);
  HH(a,b,c,d,Block[13], 4,$289b7ec6);
  HH(d,a,b,c,Block[0] ,11,$eaa127fa);
  HH(c,d,a,b,Block[3] ,16,$d4ef3085);
  HH(b,c,d,a,Block[6] ,23,$04881d05);
  HH(a,b,c,d,Block[9] , 4,$d9d4d039);
  HH(d,a,b,c,Block[12],11,$e6db99e5);
  HH(c,d,a,b,Block[15],16,$1fa27cf8);
  HH(b,c,d,a,Block[2] ,23,$c4ac5665);
  II(a,b,c,d,Block[0] , 6,$f4292244);
  II(d,a,b,c,Block[7] ,10,$432aff97);
  II(c,d,a,b,Block[14],15,$ab9423a7);
  II(b,c,d,a,Block[5] ,21,$fc93a039);
  II(a,b,c,d,Block[12], 6,$655b59c3);
  II(d,a,b,c,Block[3] ,10,$8f0ccc92);
  II(c,d,a,b,Block[10],15,$ffeff47d);
  II(b,c,d,a,Block[1] ,21,$85845dd1);
  II(a,b,c,d,Block[8] , 6,$6fa87e4f);
  II(d,a,b,c,Block[15],10,$fe2ce6e0);
  II(c,d,a,b,Block[6] ,15,$a3014314);
  II(b,c,d,a,Block[13],21,$4e0811a1);
  II(a,b,c,d,Block[4] , 6,$f7537e82);
  II(d,a,b,c,Block[11],10,$bd3af235);
  II(c,d,a,b,Block[2] ,15,$2ad7d2bb);
  II(b,c,d,a,Block[9] ,21,$eb86d391);
  inc(State[0],a);
  inc(State[1],b);
  inc(State[2],c);
  inc(State[3],d);
end;



procedure MD5Init(var Context: TMD5Context);

begin
  with Context do
    begin
    State[0] := $67452301;
    State[1] := $efcdab89;
    State[2] := $98badcfe;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    FillChar(Buffer, SizeOf(TMD5Buffer),0);
    end;
end;



procedure MD5Update(var Context: TMD5Context; Var Buf; BufLen: cardinal);

var
  Index: cardinal;
  PartLen: cardinal;
  I: cardinal;
  P : PByte;

begin
  P:=PByte(@Buf);
  with Context do
    begin
    Index := (Count[0] shr 3) and $3f;
    inc(Count[0], BufLen shl 3);
    if Count[0] < (BufLen shl 3) then inc(Count[1]);
    inc(Count[1], BufLen shr 29);
    end;
  PartLen := 64 - Index;
  if BufLen >= PartLen then
    begin
    Move(Buf,Context.Buffer[Index], PartLen);
    Transform(@Context.Buffer, Context.State);
    I := PartLen;
    while I+63 < BufLen do
      begin
      Transform(@P[I], Context.State);
      inc(I, 64);
      end;
    Index := 0;
  end
    else I := 0;
  Move(P[I],Context.Buffer[Index], BufLen - I);
end;



procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);

var
  Bits: TMD5CBits;
  I : cardinal;
  Pad : cardinal;

begin
  Invert(@Context.Count, @Bits, 8);
  I:=(Context.Count[0] shr 3) and $3f;
  if I<56 then
    Pad:=56-I
  else
    Pad:=120-I;
  MD5Update(Context, Padding, Pad);
  MD5Update(Context, Bits, 8);
  Invert(@Context.State, @Digest, 16);
  FillChar(Context, SizeOf(TMD5Context),0);
end;


function MD5String(M: string): TMD5Digest;

var
  Context: TMD5Context;

begin
  MD5Init(Context);
  MD5Update(Context, M[1], length(M));
  MD5Final(Context, Result);
end;

function MD5Buffer(var Buf; BufLen: cardinal): TMD5Digest;
var
  Context: TMD5Context;

begin
  MD5Init(Context);
  MD5Update(Context, buf, buflen);
  MD5Final(Context, Result);
end;

function MD5File(N: string): TMD5Digest;

begin
  Result:=MD5File(N,DefBufSize);
end;


function MD5File(N: string; BufSize : Cardinal): TMD5Digest;

var
  F : File;
  Buf : Pchar;
  Context: TMD5Context;
  Count : Longint;
  ofm : Longint;
  
begin
  MD5Init(Context);
  Assign(F,N);
  {$i-}
  ofm:=FileMode;
  FileMode:=0;
  Reset(F,1);
  {$i+}
  if (IOResult=0) then
    begin
    GetMem(Buf,BufSize);
    Repeat
      BlockRead(F,Buf^,Bufsize,Count);
      If (Count>0) then
        MD5Update(Context, Buf^, Count);
    Until (Count<BufSize);
    FreeMem(Buf,BufSize);
    Close(F);
    end;
  MD5Final(Context, Result);
  FileMode:=ofm;
end;


function MD5Print(D: TMD5Digest): string;

var
  I: byte;

begin
  Result := '';
  for I := 0 to 15 do
    Result := Result + HexStr(D[i],2);
  Result:=LowerCase(Result);
end;

function MD5Match(D1, D2: TMD5Digest): boolean;
var
  I: byte;
begin
  I := 0;
  Result := TRUE;
  while Result and (I < 16) do begin
    Result := D1[I] = D2[I];
    inc(I);
  end;
end;

Initialization
  FillChar(Padding,SizeOF(Padding),0);
  Padding[0]:=$80;
end.
