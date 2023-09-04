{
    This file is part of the Free Pascal packages.
    Copyright (c) 2009-2014 by the Free Pascal development team

    Implements a SHA-1 digest algorithm (RFC 3174)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

// Normally, if an optimized version is available for OS/CPU, that will be used
// Define to use existing unoptimized implementation
{ $DEFINE SHA1PASCAL}

{$IFNDEF FPC_DOTTEDUNITS}
unit sha1;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc}{$h+}{$macro+}

interface

type
  TSHA1Digest = array[0..19] of Byte;

  TSHA1Context = record
    State: array[0..4] of Cardinal;
    Buffer: array[0..63] of Byte;
    BufCnt: PtrUInt;   { in current block, i.e. in range of 0..63 }
    Length: QWord;     { total count of bytes processed }
  end;

{ core }
procedure SHA1Init(out ctx: TSHA1Context);
procedure SHA1Update(var ctx: TSHA1Context; const Buf; BufLen: PtrUInt);
procedure SHA1Final(var ctx: TSHA1Context; out Digest: TSHA1Digest);

{ auxiliary }
function SHA1String(const S: RawByteString): TSHA1Digest;
function SHA1Buffer(const Buf; BufLen: PtrUInt): TSHA1Digest;
function SHA1File(const Filename: String; const Bufsize: PtrUInt = 1024): TSHA1Digest;

{ helpers }
function SHA1Print(const Digest: TSHA1Digest): String;
function SHA1Match(const Digest1, Digest2: TSHA1Digest): Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils,System.SysConst;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils,sysconst;
{$ENDIF FPC_DOTTEDUNITS}

procedure SHA1Init(out ctx: TSHA1Context);
begin
  FillChar(ctx, sizeof(TSHA1Context), 0);
  ctx.State[0] := $67452301;
  ctx.State[1] := $efcdab89;
  ctx.State[2] := $98badcfe;
  ctx.State[3] := $10325476;
  ctx.State[4] := $c3d2e1f0;
end;

const
  { typecast to Longint to prevent assembler errors }
  K20 = LongInt($5A827999);
  K40 = LongInt($6ED9EBA1);
  K60 = LongInt($8F1BBCDC);
  K80 = LongInt($CA62C1D6);

// Use assembler version if we have a suitable CPU as well
// Define SHA1PASCAL to force use of original reference code
{$ifndef SHA1PASCAL}
  {$if defined(CPU386)}
    {$if defined(CPUX86_HAS_BSWAP)}
      {$i sha1i386.inc}
      {$define SHA1ASM}
    {$endif CPUX86_HAS_BSWAP}
  {$elseif defined(CPUX64)}
    {$IFDEF MSWINDOWS}
      // Microsoft Windows uses a different calling convention to the System V ABI
      {$i sha1x64_win.inc}
      {$define SHA1ASM}
    {$ELSE}
      {$i sha1x64_sysv.inc}
      {$define SHA1ASM}
    {$ENDIF MSWINDOWS}
  {$endif}
{$endif not SHA1PASCAL}

{$if not defined(SHA1ASM)}
// Use original version if asked for, or when we have no optimized assembler version
procedure SHA1Transform(var ctx: TSHA1Context; Buf: Pointer);
var
  A, B, C, D, E, Blkv: Cardinal;
  Data: array[0..15] of Cardinal;
begin
  A := ctx.State[0];
  B := ctx.State[1];
  C := ctx.State[2];
  D := ctx.State[3];
  E := ctx.State[4];
{$push}
{$r-,q-}

// I is round index (0..79).
{$define Sha1Round :=

// (V, W, X, Y, Z) map to (A, B, C, D, E) rotated right by I mod 5.
{$if I mod 5 = 0}     {$define V := A} {$define W := B} {$define X := C} {$define Y := D} {$define Z := E}
{$elseif I mod 5 = 1} {$define V := E} {$define W := A} {$define X := B} {$define Y := C} {$define Z := D}
{$elseif I mod 5 = 2} {$define V := D} {$define W := E} {$define X := A} {$define Y := B} {$define Z := C}
{$elseif I mod 5 = 3} {$define V := C} {$define W := D} {$define X := E} {$define Y := A} {$define Z := B}
{$else}               {$define V := B} {$define W := C} {$define X := D} {$define Y := E} {$define Z := A}
{$endif}

{$if I < 16}
  Blkv := BEtoN(Unaligned(PCardinal(Buf)[I]));
{$else}
  Blkv := RolDWord(Data[(I + 13) and 15] xor Data[(I + 8) and 15] xor Data[(I + 2) and 15] xor Data[I and 15], 1);
{$endif}

  Z := Z + Blkv; // Handling Blkv first gives a chance to free its register for the following complex expressions added to Z as well.
{$if I < 77} // for rounds 77 .. 79 this is a dead store
  Data[I and 15] := Blkv;
{$endif}
  Z := Z + RolDWord(V, 5) +
{$if I < 20}     ((W and (X xor Y)) xor Y) + K20
{$elseif I < 40} (W xor X xor Y) + K40
{$elseif I < 60} (((W or X) and Y) or (W and X)) + K60
{$else}          (W xor X xor Y) + K80
{$endif};
  W := RorDWord(W, 2);

  {$undef V} {$undef W} {$undef X} {$undef Y} {$undef Z}

  {$undef I}}

{$define I := 0} Sha1Round {$define I := 1} Sha1Round {$define I := 2} Sha1Round {$define I := 3} Sha1Round {$define I := 4} Sha1Round
{$define I := 5} Sha1Round {$define I := 6} Sha1Round {$define I := 7} Sha1Round {$define I := 8} Sha1Round {$define I := 9} Sha1Round
{$define I := 10} Sha1Round {$define I := 11} Sha1Round {$define I := 12} Sha1Round {$define I := 13} Sha1Round {$define I := 14} Sha1Round
{$define I := 15} Sha1Round {$define I := 16} Sha1Round {$define I := 17} Sha1Round {$define I := 18} Sha1Round {$define I := 19} Sha1Round
{$define I := 20} Sha1Round {$define I := 21} Sha1Round {$define I := 22} Sha1Round {$define I := 23} Sha1Round {$define I := 24} Sha1Round
{$define I := 25} Sha1Round {$define I := 26} Sha1Round {$define I := 27} Sha1Round {$define I := 28} Sha1Round {$define I := 29} Sha1Round
{$define I := 30} Sha1Round {$define I := 31} Sha1Round {$define I := 32} Sha1Round {$define I := 33} Sha1Round {$define I := 34} Sha1Round
{$define I := 35} Sha1Round {$define I := 36} Sha1Round {$define I := 37} Sha1Round {$define I := 38} Sha1Round {$define I := 39} Sha1Round
{$define I := 40} Sha1Round {$define I := 41} Sha1Round {$define I := 42} Sha1Round {$define I := 43} Sha1Round {$define I := 44} Sha1Round
{$define I := 45} Sha1Round {$define I := 46} Sha1Round {$define I := 47} Sha1Round {$define I := 48} Sha1Round {$define I := 49} Sha1Round
{$define I := 50} Sha1Round {$define I := 51} Sha1Round {$define I := 52} Sha1Round {$define I := 53} Sha1Round {$define I := 54} Sha1Round
{$define I := 55} Sha1Round {$define I := 56} Sha1Round {$define I := 57} Sha1Round {$define I := 58} Sha1Round {$define I := 59} Sha1Round
{$define I := 60} Sha1Round {$define I := 61} Sha1Round {$define I := 62} Sha1Round {$define I := 63} Sha1Round {$define I := 64} Sha1Round
{$define I := 65} Sha1Round {$define I := 66} Sha1Round {$define I := 67} Sha1Round {$define I := 68} Sha1Round {$define I := 69} Sha1Round
{$define I := 70} Sha1Round {$define I := 71} Sha1Round {$define I := 72} Sha1Round {$define I := 73} Sha1Round {$define I := 74} Sha1Round
{$define I := 75} Sha1Round {$define I := 76} Sha1Round {$define I := 77} Sha1Round {$define I := 78} Sha1Round {$define I := 79} Sha1Round

{$undef Sha1Round}

  Inc(ctx.State[0], A);
  Inc(ctx.State[1], B);
  Inc(ctx.State[2], C);
  Inc(ctx.State[3], D);
  Inc(ctx.State[4], E);
{$pop}
  Inc(ctx.Length,64);
end;
{$endif not defined(SHA1ASM)}

procedure SHA1Update(var ctx: TSHA1Context; const Buf; BufLen: PtrUInt);
var
  Src: PByte;
  Num: PtrUInt;
begin
  if BufLen = 0 then
    Exit;

  Src := @Buf;
  Num := 0;

  // 1. Transform existing data in buffer
  if ctx.BufCnt > 0 then
  begin
    // 1.1 Try to fill buffer up to block size
    Num := 64 - ctx.BufCnt;
    if Num > BufLen then
      Num := BufLen;

    Move(Src^, ctx.Buffer[ctx.BufCnt], Num);
    Inc(ctx.BufCnt, Num);
    Inc(Src, Num);

    // 1.2 If buffer is filled, transform it
    if ctx.BufCnt = 64 then
    begin
      SHA1Transform(ctx, @ctx.Buffer);
      ctx.BufCnt := 0;
    end;
  end;

  // 2. Transform input data in 64-byte blocks
  Num := BufLen - Num;
  while Num >= 64 do
  begin
    SHA1Transform(ctx, Src);
    Inc(Src, 64);
    Dec(Num, 64);
  end;

  // 3. If there's less than 64 bytes left, add it to buffer
  if Num > 0 then
  begin
    ctx.BufCnt := Num;
    Move(Src^, ctx.Buffer, Num);
  end;
end;

const
  PADDING: array[0..63] of Byte =
    ($80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    );

procedure SHA1Final(var ctx: TSHA1Context; out Digest: TSHA1Digest);
var
  Length: QWord;
  Pads, I: SizeUint;
begin
  // 1. Compute length of the whole stream in bits
  Length := 8 * (ctx.Length + ctx.BufCnt);

  // 2. Append padding bits
  if ctx.BufCnt >= 56 then
    Pads := 120 - ctx.BufCnt
  else
    Pads := 56 - ctx.BufCnt;
  SHA1Update(ctx, PADDING, Pads);

  // 3. Append length of the stream (8 bytes)
  Length := NtoBE(Length);
  SHA1Update(ctx, Length, 8);

  // 4. Invert state to digest
  for I := 0 to High(ctx.State) do
    Unaligned(PUint32(PByte(Digest))[I]) := NtoBE(ctx.State[I]);
  FillChar(ctx, sizeof(TSHA1Context), 0);
end;

function SHA1String(const S: RawByteString): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(S)^, length(S));
  SHA1Final(Context, Result);
end;

function SHA1Buffer(const Buf; BufLen: PtrUInt): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, buf, buflen);
  SHA1Final(Context, Result);
end;

procedure RaiseFileNotFoundException(const fn : String);
begin
  raise EFileNotFoundException.Create(SFileNotFound);
end;

function SHA1File(const Filename: String; const Bufsize: PtrUInt): TSHA1Digest;
var
  F: File;
  Buf: PAnsiChar;
  Context: TSHA1Context;
  Count: Cardinal;
  ofm: Longint;
begin
  SHA1Init(Context);

  Assign(F, Filename);
  {$push}{$i-}
  ofm := FileMode;
  FileMode := 0;
  Reset(F, 1);
  {$pop}

  if IOResult = 0 then
  begin
    GetMem(Buf, BufSize);
    repeat
      BlockRead(F, Buf^, Bufsize, Count);
      if Count > 0 then
        SHA1Update(Context, Buf^, Count);
    until Count < BufSize;
    FreeMem(Buf, BufSize);
    Close(F);
  end
  else
    RaiseFileNotFoundException(FileName);

  SHA1Final(Context, Result);
  FileMode := ofm;
end;

const
  HexTbl: array[0..15] of AnsiChar='0123456789abcdef';     // lowercase

function SHA1Print(const Digest: TSHA1Digest): String;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, 40);
  P := Pointer(Result);
  for I := 0 to 19 do
  begin
    P[0] := HexTbl[(Digest[i] shr 4) and 15];
    P[1] := HexTbl[Digest[i] and 15];
    Inc(P,2);
  end;
end;

function SHA1Match(const Digest1, Digest2: TSHA1Digest): Boolean;
var
  A: array[0..4] of Cardinal absolute Digest1;
  B: array[0..4] of Cardinal absolute Digest2;
begin
{$push}
{$B+}
  Result := (A[0] = B[0]) and (A[1] = B[1]) and (A[2] = B[2]) and (A[3] = B[3]) and (A[4] = B[4]);
{$pop}
end;

end.
