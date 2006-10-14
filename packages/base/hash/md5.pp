{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2006 by the Free Pascal development team

    Implements a MD5 digest algorithm.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Implements a MD4 digest algorithm (RFC 1320)
  Implements a MD5 digest algorithm (RFC 1321)
}

unit md5;

{$mode objfpc}
{$h+}

interface

const
  DefBufSize = 1024;

type
  PMDDigest = ^TMDDigest;
  TMDDigest = array[0..15] of Byte;

  TMDContext = packed record
    Version : Cardinal;
    State   : array[0..3] of Cardinal;
    Length  : PtrUInt;
    BufCnt  : PtrUInt;
    Buffer  : array[0..63] of Byte;
  end;


{ Raw methods }

procedure MDInit(var Context: TMDContext; const Version: Cardinal);
procedure MDUpdate(var Context: TMDContext; var Buf; const BufLen: PtrUInt);
procedure MDFinal(var Context: TMDContext; var Digest: TMDDigest);

{ Auxiliary methods }

function MDString(const S: String; const Version: Cardinal): TMDDigest;
function MDBuffer(var Buf; const BufLen: PtrUInt; const Version: Cardinal): TMDDigest;
function MDFile(const Filename: String; const Version: Cardinal; const Bufsize: PtrUInt = DefBufSize): TMDDigest;
function MDPrint(const Digest: TMDDigest): String;
// based on an implementation by Matthias Fichtner
function MDMatch(const Digest1, Digest2: TMDDigest): Boolean;

implementation


function rol(x: Cardinal; n: Byte): Cardinal;
begin
  Result := (x shl n) or (x shr (32 - n));
end;


// inverts the bytes of (Count div 4) cardinals from source to target.
procedure Invert(Source, Dest: Pointer; Count: PtrUInt);
var
  S: PByte;
  T: PCardinal;
  I: PtrUInt;
begin
  S := Source;
  T := Dest;
  for I := 1 to (Count div 4) do
  begin
    T^ := S[0] or (S[1] shl 8) or (S[2] shl 16) or (S[3] shl 24);
    inc(S,4);
    inc(T);
  end;
end;


procedure MD4Transform(var Context: TMDContext; Buffer: Pointer);

  procedure R1(var a: Cardinal; b,c,d,x: Cardinal; s: Byte);
  // F(x,y,z) = (x and y) or ((not x) and z)
  begin
    a := rol(a + {F(b,c,d)}((b and c) or ((not b) and d)) + x, s);
  end;

  procedure R2(var a: Cardinal; b,c,d,x: Cardinal; s: Byte);
  // G(x,y,z) = (x and y) or (x and z) or (y and z);
  begin
    a := rol(a + {G(b,c,d)}((b and c) or (b and d) or (c and d)) + x + $5A827999, s);
  end;

  procedure R3(var a: Cardinal; b,c,d,x: Cardinal; s: Byte);
  // H(x,y,z) = x xor y xor z
  begin
    a := rol(a + {H(b,c,d)}(b xor c xor d) + x + $6ED9EBA1, s);
  end;

var
  a, b, c, d: Cardinal;
  Block: array[0..15] of Cardinal;
begin
  Invert(Buffer, @Block, 64);
  a := Context.State[0];
  b := Context.State[1];
  c := Context.State[2];
  d := Context.State[3];

  // Round 1
  R1(a,b,c,d,Block[0],  3); R1(d,a,b,c,Block[1],  7); R1(c,d,a,b,Block[2], 11); R1(b,c,d,a,Block[3], 19);
  R1(a,b,c,d,Block[4],  3); R1(d,a,b,c,Block[5],  7); R1(c,d,a,b,Block[6], 11); R1(b,c,d,a,Block[7], 19);
  R1(a,b,c,d,Block[8],  3); R1(d,a,b,c,Block[9],  7); R1(c,d,a,b,Block[10],11); R1(b,c,d,a,Block[11],19);
  R1(a,b,c,d,Block[12], 3); R1(d,a,b,c,Block[13], 7); R1(c,d,a,b,Block[14],11); R1(b,c,d,a,Block[15],19);

  // Round 2
  R2(a,b,c,d,Block[0],  3); R2(d,a,b,c,Block[4],  5); R2(c,d,a,b,Block[8],  9); R2(b,c,d,a,Block[12],13);
  R2(a,b,c,d,Block[1],  3); R2(d,a,b,c,Block[5],  5); R2(c,d,a,b,Block[9],  9); R2(b,c,d,a,Block[13],13);
  R2(a,b,c,d,Block[2],  3); R2(d,a,b,c,Block[6],  5); R2(c,d,a,b,Block[10], 9); R2(b,c,d,a,Block[14],13);
  R2(a,b,c,d,Block[3],  3); R2(d,a,b,c,Block[7],  5); R2(c,d,a,b,Block[11], 9); R2(b,c,d,a,Block[15],13);

  // Round 3
  R3(a,b,c,d,Block[0],  3); R3(d,a,b,c,Block[8],  9); R3(c,d,a,b,Block[4], 11); R3(b,c,d,a,Block[12],15);
  R3(a,b,c,d,Block[2],  3); R3(d,a,b,c,Block[10], 9); R3(c,d,a,b,Block[6], 11); R3(b,c,d,a,Block[14],15);
  R3(a,b,c,d,Block[1],  3); R3(d,a,b,c,Block[9],  9); R3(c,d,a,b,Block[5], 11); R3(b,c,d,a,Block[13],15);
  R3(a,b,c,d,Block[3],  3); R3(d,a,b,c,Block[11], 9); R3(c,d,a,b,Block[7], 11); R3(b,c,d,a,Block[15],15);

  inc(Context.State[0], a);
  inc(Context.State[1], b);
  inc(Context.State[2], c);
  inc(Context.State[3], d);
  inc(Context.Length,64);
end;


procedure MD5Transform(var Context: TMDContext; Buffer: Pointer);

  procedure R1(var a: Cardinal; b,c,d,x: Cardinal; s: Byte; ac: Cardinal);
  // F(x,y,z) = (x and y) or ((not x) and z)
  begin
    a := b + rol(a + {F(b,c,d)}((b and c) or ((not b) and d)) + x + ac, s);
  end;

  procedure R2(var a: Cardinal; b,c,d,x: Cardinal; s: Byte; ac: Cardinal);
  // G(x,y,z) = (x and z) or (y and (not z))
  begin
    a := b + rol(a + {G(b,c,d)}((b and d) or (c and (not d))) + x + ac, s);
  end;

  procedure R3(var a: Cardinal; b,c,d,x: Cardinal; s: Byte; ac: Cardinal);
  // H(x,y,z) = x xor y xor z;
  begin
    a := b + rol(a + {H(b,c,d)}(b xor c xor d) + x + ac, s);
  end;

  procedure R4(var a: Cardinal; b,c,d,x: Cardinal; s: Byte; ac: Cardinal);
  // I(x,y,z) = y xor (x or (not z));
  begin
    a := b + rol(a + {I(b,c,d)}(c xor (b or (not d))) + x + ac, s);
  end;

var
  a, b, c, d: Cardinal;
  Block: array[0..15] of Cardinal;
begin
  Invert(Buffer, @Block, 64);
  a := Context.State[0];
  b := Context.State[1];
  c := Context.State[2];
  d := Context.State[3];

  // Round 1
  R1(a,b,c,d,Block[0] , 7,$d76aa478); R1(d,a,b,c,Block[1] ,12,$e8c7b756); R1(c,d,a,b,Block[2] ,17,$242070db); R1(b,c,d,a,Block[3] ,22,$c1bdceee);
  R1(a,b,c,d,Block[4] , 7,$f57c0faf); R1(d,a,b,c,Block[5] ,12,$4787c62a); R1(c,d,a,b,Block[6] ,17,$a8304613); R1(b,c,d,a,Block[7] ,22,$fd469501);
  R1(a,b,c,d,Block[8] , 7,$698098d8); R1(d,a,b,c,Block[9] ,12,$8b44f7af); R1(c,d,a,b,Block[10],17,$ffff5bb1); R1(b,c,d,a,Block[11],22,$895cd7be);
  R1(a,b,c,d,Block[12], 7,$6b901122); R1(d,a,b,c,Block[13],12,$fd987193); R1(c,d,a,b,Block[14],17,$a679438e); R1(b,c,d,a,Block[15],22,$49b40821);

  // Round 2
  R2(a,b,c,d,Block[1] , 5,$f61e2562); R2(d,a,b,c,Block[6] , 9,$c040b340); R2(c,d,a,b,Block[11],14,$265e5a51); R2(b,c,d,a,Block[0] ,20,$e9b6c7aa);
  R2(a,b,c,d,Block[5] , 5,$d62f105d); R2(d,a,b,c,Block[10], 9,$02441453); R2(c,d,a,b,Block[15],14,$d8a1e681); R2(b,c,d,a,Block[4] ,20,$e7d3fbc8);
  R2(a,b,c,d,Block[9] , 5,$21e1cde6); R2(d,a,b,c,Block[14], 9,$c33707d6); R2(c,d,a,b,Block[3] ,14,$f4d50d87); R2(b,c,d,a,Block[8] ,20,$455a14ed);
  R2(a,b,c,d,Block[13], 5,$a9e3e905); R2(d,a,b,c,Block[2] , 9,$fcefa3f8); R2(c,d,a,b,Block[7] ,14,$676f02d9); R2(b,c,d,a,Block[12],20,$8d2a4c8a);

  // Round 3
  R3(a,b,c,d,Block[5] , 4,$fffa3942); R3(d,a,b,c,Block[8] ,11,$8771f681); R3(c,d,a,b,Block[11],16,$6d9d6122); R3(b,c,d,a,Block[14],23,$fde5380c);
  R3(a,b,c,d,Block[1] , 4,$a4beea44); R3(d,a,b,c,Block[4] ,11,$4bdecfa9); R3(c,d,a,b,Block[7] ,16,$f6bb4b60); R3(b,c,d,a,Block[10],23,$bebfbc70);
  R3(a,b,c,d,Block[13], 4,$289b7ec6); R3(d,a,b,c,Block[0] ,11,$eaa127fa); R3(c,d,a,b,Block[3] ,16,$d4ef3085); R3(b,c,d,a,Block[6] ,23,$04881d05);
  R3(a,b,c,d,Block[9] , 4,$d9d4d039); R3(d,a,b,c,Block[12],11,$e6db99e5); R3(c,d,a,b,Block[15],16,$1fa27cf8); R3(b,c,d,a,Block[2] ,23,$c4ac5665);

  // Round 4
  R4(a,b,c,d,Block[0] , 6,$f4292244); R4(d,a,b,c,Block[7] ,10,$432aff97); R4(c,d,a,b,Block[14],15,$ab9423a7); R4(b,c,d,a,Block[5] ,21,$fc93a039);
  R4(a,b,c,d,Block[12], 6,$655b59c3); R4(d,a,b,c,Block[3] ,10,$8f0ccc92); R4(c,d,a,b,Block[10],15,$ffeff47d); R4(b,c,d,a,Block[1] ,21,$85845dd1);
  R4(a,b,c,d,Block[8] , 6,$6fa87e4f); R4(d,a,b,c,Block[15],10,$fe2ce6e0); R4(c,d,a,b,Block[6] ,15,$a3014314); R4(b,c,d,a,Block[13],21,$4e0811a1);
  R4(a,b,c,d,Block[4] , 6,$f7537e82); R4(d,a,b,c,Block[11],10,$bd3af235); R4(c,d,a,b,Block[2] ,15,$2ad7d2bb); R4(b,c,d,a,Block[9] ,21,$eb86d391);

  inc(Context.State[0],a);
  inc(Context.State[1],b);
  inc(Context.State[2],c);
  inc(Context.State[3],d);
  inc(Context.Length,64);
end;


procedure MDInit(var Context: TMDContext; const Version: Cardinal);
begin
  Context.Version := Version;
  Context.State[0] := $67452301;
  Context.State[1] := $efcdab89;
  Context.State[2] := $98badcfe;
  Context.State[3] := $10325476;
  Context.Length := 0;
  Context.BufCnt := 0;
end;


procedure MDUpdate(var Context: TMDContext; var Buf; const BufLen: PtrUInt);
var
  Src: Pointer;
  Num: PtrUInt;
begin
  Src := @Buf;
  Num := 0;

  // 1. Transform existing data in buffer
  if Context.BufCnt > 0 then
  begin
    // 1.1 Try to fill buffer to 64 bytes
    Num := 64 - Context.BufCnt;
    if Num > BufLen then
      Num := BufLen;

    Move(Src^, Context.Buffer[Context.BufCnt], Num);
    Context.BufCnt := Context.BufCnt + Num;
    Src := Pointer(PtrUInt(Src) + Num);

    // 1.2 If buffer contains 64 bytes, transform it
    if Context.BufCnt = 64 then
    begin
      case Context.Version of
        4: MD4Transform(Context, @Context.Buffer);
        5: MD5Transform(Context, @Context.Buffer);
      end;
      Context.BufCnt := 0;
    end;
  end;

  // 2. Transform 64-Byte blocks of Buf
  Num := BufLen - Num;
  while Num >= 64 do
  begin
    case Context.Version of
      4: MD4Transform(Context, Src);
      5: MD5Transform(Context, Src);
    end;
    Src := Pointer(PtrUInt(Src) + 64);
    Num := Num - 64;
  end;

  // 3. If there's a block smaller than 64 Bytes left, add it to buffer
  if Num > 0 then
  begin
    Context.BufCnt := Num;
    Move(Src^, Context.Buffer, Num);
  end;
end;


procedure MDFinal(var Context: TMDContext; var Digest: TMDDigest);
const
  Padding: array[0..15] of Cardinal = ($80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
var
  Length: QWord;
  Pads: Cardinal;
begin
  // 1. Compute length of the whole stream in bits
  Length := 8 * (Context.Length + Context.BufCnt);

  // 2. Append padding bits
  Pads := (120 - Context.BufCnt) mod 64;
  if Pads > 0 then
    MDUpdate(Context, Padding, Pads) else
    MDUpdate(Context, Padding, 56);

  // 3. Append length of the stream
  Invert(@Length, @Length, 8);
  MDUpdate(Context, Length, 8);

  // 4. Invert state to digest
  Invert(@Context.State, @Digest, 16);
  FillChar(Context, SizeOf(TMDContext), 0);
end;


function MDString(const S: String; const Version: Cardinal): TMDDigest;
var
  Context: TMDContext;
begin
  MDInit(Context, Version);
  MDUpdate(Context, PChar(S)^, length(S));
  MDFinal(Context, Result);
end;


function MDBuffer(var Buf; const BufLen: PtrUInt; const Version: Cardinal): TMDDigest;
var
  Context: TMDContext;
begin
  MDInit(Context, Version);
  MDUpdate(Context, buf, buflen);
  MDFinal(Context, Result);
end;


function MDFile(const Filename: String; const Version: Cardinal; const BufSize: PtrUInt): TMDDigest;
var
  F: File;
  Buf: Pchar;
  Context: TMDContext;
  Count: Cardinal;
  ofm: Longint;
begin
  MDInit(Context, Version);

  Assign(F, Filename);
  {$i-}
  ofm := FileMode;
  FileMode := 0;
  Reset(F, 1);
  {$i+}

  if IOResult = 0 then
  begin
    GetMem(Buf, BufSize);
    repeat
      BlockRead(F, Buf^, Bufsize, Count);
      if Count > 0 then
        MDUpdate(Context, Buf^, Count);
    until Count < BufSize;
    FreeMem(Buf, BufSize);
    Close(F);
  end;

  MDFinal(Context, Result);
  FileMode := ofm;
end;


function MDPrint(const Digest: TMDDigest): String;
var
  I: Byte;
begin
  Result := '';
  for I := 0 to 15 do
    Result := Result + HexStr(Digest[i],2);
  Result := LowerCase(Result);
end;


function MDMatch(const Digest1, Digest2: TMDDigest): Boolean;
var
  I: Byte;
begin
  I := 0;
  Result := True;
  while Result and (I < 16) do
  begin
    Result := Digest1[I] = Digest2[I];
    inc(I);
  end;
end;

end.
