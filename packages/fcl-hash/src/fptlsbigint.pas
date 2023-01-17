
{-------------------------------------------------------------------------------
  Notes: refactored from Ultibo Big Integer unit Copyright (C) 2018 - SoftOz Pty Ltd.
  inspired by AXTLS - \crypto\bigint.c - Copyright (c) 2007, Cameron Rich
  Reference: Handbook of Applied Cryptography (Chapter 14) - http://cacr.uwaterloo.ca/hac/about/chap14.pdf
-------------------------------------------------------------------------------}
{$MODE OBJFPC}
{$h+}
{$MODESWITCH advancedrecords}
{$R-}
{$Q-}

unit fpTLSBigInt;

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses SysUtils;

{off $DEFINE BIGINT_DEBUG}         // Enable debug output/functions for BitInt unit

const
  // Maintain a number of precomputed variables when doing reduction
  BIGINT_M_OFFSET = 0;    // Normal modulo offset
  BIGINT_P_OFFSET = 1;    // P modulo offset
  BIGINT_Q_OFFSET = 2;    // Q modulo offset
  BIGINT_NUM_MODS = 3;    // The number of modulus constants used

const
  BIGINT_COMP_RADIX       = UInt64(4294967296);        // Max component + 1
  BIGINT_COMP_MAX         = UInt64($FFFFFFFFFFFFFFFF); // Max dbl component - 1
  BIGINT_COMP_BIT_SIZE    = 32;                        // Number of bits in a component
  BIGINT_COMP_BYTE_SIZE   = 4;                         // Number of bytes in a component
  BIGINT_COMP_NUM_NIBBLES = 8;                         // Used for diagnostics only

  BIGINT_PERMANENT = $7FFF55AA;  {A magic number for permanents}

  BIGINTCONTEXT_CHUNKSIZE = 16*1024;

type
  PBIComponent = ^TBIComponent;
  TBIComponent = LongWord;    // A single precision component

  PBILongComponent = ^TBILongComponent;
  TBILongComponent = UInt64; // A double precision component

  PBISignedLongComponent = ^TBISignedLongComponent;
  TBISignedLongComponent = Int64; // A signed double precision component

type
  PPBigInt = ^PBigInt;
  PBigInt = ^TBigInt;
  TBigInt = record
    Next: PBigInt;           // The next bigint in the cache
    Size: Integer;           // The number of components in this bigint
    MaxComponents: Integer;  // The number of components allocated for this bigint
    References: Integer;     // An internal reference count
    Components: PBIComponent;  // A ptr to the actual component data
    procedure ToString(out S: AnsiString);
  end;

  {Maintains the state of the cache, and a number of variables used in reduction}
  PBigIntContext = ^TBigIntContext;
  TBigIntContext = record
    FreeList: PBigInt;                                        // Bigints not used
    BIRadix: PBigInt;                                         // The radix used
    BIMod: array[0..BIGINT_NUM_MODS-1] of PBigInt;            // Modulus
    BImu: array[0..BIGINT_NUM_MODS-1] of PBigInt;             // Storage for mu
    BIbk1: array[0..BIGINT_NUM_MODS-1] of PBigInt;            // Storage for b(k+1)
    BINormalisedMod: array[0..BIGINT_NUM_MODS-1] of PBigInt;  // Normalised mod storage
    G: PPBigInt;                                              // Used by sliding-window
    Window: Integer;                                          // The size of the sliding window
    ActiveCount: Integer;                                     // Number of active bigints
    FreeCount: Integer;                                       // Number of free bigints
    ModOffset: Byte;                                          // The mod offset we are using
  end;

procedure BIInitialize(out Context: TBigIntContext);
procedure BITerminate(var Context: TBigIntContext);
procedure BIPermanent(BI: PBigInt);
procedure BIDepermanent(BI: PBigInt);
procedure BIRelease(var Context: TBigIntContext; BI: PBigInt);

function BICopy(BI: PBigInt): PBigInt;
function BIClone(var Context: TBigIntContext; const BI: TBigInt): PBigInt;

procedure BIExport(var Context: TBigIntContext; BI: PBigInt; Data: PByte; Size: Integer; Release: boolean = true);
function BIImport(var Context: TBigIntContext; Data: PByte; const Size: Integer): PBigInt; overload;
function BIImport(var Context: TBigIntContext; const Data: TBytes): PBigInt; overload;
function BIImport(var Context: TBigIntContext; const Data: AnsiString): PBigInt; overload;
function IntToBI(var Context: TBigIntContext; I: TBIComponent): PBigInt;
function BIBitCount(BI: PBigInt): integer;

function BIAdd(var Context: TBigIntContext; BIA, BIB: PBigInt): PBigInt;
function BISubtract(var Context: TBigIntContext; BIA, BIB: PBigInt;out IsNegative: Boolean): PBigInt;
function BIDivide(var Context: TBigIntContext; U, V: PBigInt; IsMod: Boolean): PBigInt;
function BIMultiply(var Context: TBigIntContext; BIA, BIB: PBigInt): PBigInt;
function BIModPower(var Context: TBigIntContext; BI, BIExp: PBigInt): PBigInt;
//function BIModPower2(var Context: TBigIntContext; BI,BIM,BIExp:PBigInt):PBigInt;

function BICompare(BIA, BIB: PBigInt): Integer;
procedure BISetMod(var Context: TBigIntContext; BIM: PBigInt; ModOffset: Integer);
procedure BIFreeMod(var Context: TBigIntContext; ModOffset: Integer);

function BIMod(var Context: TBigIntContext; BI: PBigInt): PBigInt; inline;
function BIResidue(var Context: TBigIntContext; BI: PBigInt): PBigInt; inline;
function BIBarrett(var Context: TBigIntContext; BI: PBigInt): PBigInt;

function BISquare(var Context: TBigIntContext; BI: PBigInt): PBigInt; inline;

function BICRT(var Context: TBigIntContext; BI, DP, DQ, P, Q, QInv: PBigInt): PBigInt;

// Convert a bigint to a string of hex characters @Result[BI.Size*2]
function BIToString(BI: PBigInt): AnsiString; overload;
function BIToDbgString(BI: PBigInt): AnsiString; overload;
procedure BIToString(BI: PBigInt; out S: AnsiString); overload;
function StringToBI(var Context: TBigIntContext; const Value: AnsiString): PBigInt;

implementation

// @S[BI.Size*2]
procedure TBigInt.ToString(out S: AnsiString);
begin
  BIToString(@Self, S);
end;

function Min(A, B: LongInt): LongInt; inline;
begin
  if A < B then
    Result := A
  else
    Result:=B;
end;

function Max(A, B: LongInt): LongInt; inline;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

{Internal Functions}

{Perform a sanity check on bi}
function BICheck(const BI: TBigInt): Boolean; inline;
begin
  {$IFDEF BIGINT_DEBUG}
  if BI.References <= 0 then
  begin
    writeln('BICheck - Zero or negative References in TBigInt');
    raise Exception.Create('20220428201452');
  end;
  if BI.Next <> nil then
  begin
    writeln('BICheck - Attempt to use a TBigInt from the free list');
    raise Exception.Create('20220428201508');
  end;
  {$ENDIF}
  Result:=True;
end;

// Delete any leading 0's (and allow for 0)
procedure BITrim(BI: PBigInt);
begin
  if not BICheck(BI^) then
    Exit;
  while (BI^.Components[BI^.Size-1] = 0) and (BI^.Size > 1) do
    Dec(BI^.Size);
end;

procedure BIResizeComponents(BI: PBigInt; N: Integer);
begin
  if N > BI^.MaxComponents then
  begin
    BI^.MaxComponents := Max(BI^.MaxComponents*2, N);
    ReAllocMem(BI^.Components, BI^.MaxComponents*BIGINT_COMP_BYTE_SIZE);
  end;
  if N > BI^.Size then
    FillByte(BI^.Components[BI^.Size], (N-BI^.Size)*BIGINT_COMP_BYTE_SIZE, 0);
  BI^.Size := N;
end;

function BIAllocate(var Context: TBigIntContext; Size: Integer): PBigInt;
begin
  if Context.FreeList <> nil then
  begin
    Result := Context.FreeList;
    if Result^.References <> 0 then
      raise Exception.Create('20220428200026');
    Context.FreeList := Result^.Next;
    Dec(Context.FreeCount);
    BIResizeComponents(Result, Size);
  end else
  begin
    Result := AllocMem(SizeOf(TBigint));
    Result^.Components := AllocMem((Size*2)*BIGINT_COMP_BYTE_SIZE);
    Result^.MaxComponents := Size*2; // Allow space to expand
    Result^.Size := Size;
  end;
  Result^.References := 1;
  Result^.Next := nil;
  Inc(Context.ActiveCount);
end;

// Work out the highest '1' bit in an exponent. Used when doing sliding-window exponentiation
function BIFindMaxExponentIndex(BIExp: PBigInt): Integer;
var
  I: Integer;
  Shift, Test: TBIComponent;
begin
  I := BIGINT_COMP_BIT_SIZE-1;
  Shift:=BIGINT_COMP_RADIX div 2;
  Test:=BIExp^.Components[BIExp^.Size - 1]; {Assume no leading zeroes}
  if not BICheck(BIExp^) then
  begin
    Result:=-1;
    Exit;
  end;
  repeat
    if (Test and Shift) <> 0 then
    begin
      Result := I + (BIExp^.Size-1) * BIGINT_COMP_BIT_SIZE;
      Exit;
    end;
    Shift := Shift shr 1;
    Dec(I);
  until I < 0;
  Result := -1; // Error - must have been a leading 0
end;

// Is a particular bit is an exponent 1 or 0? Used when doing sliding-window exponentiation
function BIExpBitIsOne(BIExp: PBigInt; Offset: Integer): Boolean;
var
  NumShifts: Integer;
  Shift, Test: TBIComponent;
begin
  Test := BIExp^.Components[Offset div BIGINT_COMP_BIT_SIZE];
  NumShifts := Offset mod BIGINT_COMP_BIT_SIZE;
  if not BICheck(BIExp^) then
  begin
    Result := False;
    Exit;
  end;
  Shift := 1 shl NumShifts;
  Result := (Test and Shift) <> 0;
end;

function BIIntMultiply(var Context: TBigIntContext; BIA: PBigInt; B: TBIComponent): PBigInt;
var
  A: PBIComponent;
  Carry: TBIComponent;
  J, N: Integer;
  R: PBIComponent;
  Tmp: TBILongComponent;
begin
  if not BICheck(BIA^) then
  begin
    Result:=nil;
    Exit;
  end;
  J := 0;
  N := BIA^.Size;
  Carry := 0;
  Result := BIAllocate(Context, N + 1);
  R := Result^.Components;
  A := BIA^.Components;
  FillByte(R^, (N+1) * BIGINT_COMP_BYTE_SIZE, 0);
  repeat
    Tmp := TBILongComponent(R^) + TBILongComponent(A[J]) * B + Carry; // Avoid overflow
    R^ := DWord(Tmp and $ffffffff); // Downsize
    Inc(R);
    Carry := Tmp shr BIGINT_COMP_BIT_SIZE;
    Inc(J);
  until J >= N;
  R^ := Carry;
  BIRelease(Context, BIA);
  BITrim(Result);
end;

function BIIntDivide(BIR: PBigInt; Denom: TBIComponent): PBigInt;
var
  I: Integer;
  R: TBILongComponent;
begin
  if not BICheck(BIR^) then
  begin
    Result:=nil;
    Exit;
  end;
  I := BIR^.Size-1;
  R := 0;
  repeat
    R := (R shl BIGINT_COMP_BIT_SIZE) + BIR^.Components[I];
    BIR^.Components[I] := DWord((R div Denom) and $ffffffff);
    R := R mod Denom;
    Dec(I);
  until I < 0;
  BITrim(BIR);
  Result := BIR;
end;

// Take each component and shift down (in terms of components)
function BICompRightShift(BIR: PBigInt; NumShifts: Integer): PBigInt;
var
  I: Integer;
  X, Y: PBIComponent;
begin
  if not BICheck(BIR^) then
  begin
    Result:=nil;
    Exit;
  end;
  I := BIR^.Size-NumShifts;
  X := BIR^.Components;
  Y := @BIR^.Components[NumShifts];
  if I <= 0 then // Have we completely right shifted?
  begin
    BIR^.Components[0]:=0; {Return 0}
    BIR^.Size:=1;
    Result:=BIR;
    Exit;
  end;
  repeat
    X^ := Y^;
    Inc(X);
    Inc(Y);
    Dec(I);
  until I <= 0;
  Dec(BIR^.Size, NumShifts);
  Result := BIR;
end;

// Take each component and shift it up (in terms of components)
function BICompLeftShift(BIR: PBigInt; NumShifts: Integer): PBigInt;
var
 I: Integer;
 X: PBIComponent;
 Y: PBIComponent;
begin
  if not BICheck(BIR^) then
  begin
    Result:=nil;
    Exit;
  end;
  I := BIR^.Size-1;
  if NumShifts <= 0 then
  begin
    Result:=BIR;
    Exit;
  end;
  BIResizeComponents(BIR, BIR^.Size+NumShifts);
  X := @(BIR^.Components[I + NumShifts]);
  Y := @(BIR^.Components[I]);
  repeat
    X^ := Y^;
    Dec(X);
    Dec(Y);
    Dec(I);
  until I < 0;
  FillByte(BIR^.Components^, NumShifts*BIGINT_COMP_BYTE_SIZE, 0); // Zero least significant components
  Result := BIR;
end;

// Perform a standard multiplication between two bigints
function BIRegularMultiply(var Context: TBigIntContext; BIA, BIB: PBigInt; InnerPartial, OuterPartial: Integer): PBigInt;
var
  Carry: TBIComponent;
  BIR: PBigInt;
  I, J, N, RIndex, T: Integer;
  SA, SB, SR:PBIComponent;
  Tmp: TBILongComponent;
begin
  if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
    Result:=nil;
    Exit;
  end;
  I := 0;
  N := BIA^.Size;
  T := BIB^.Size;
  BIR := BIAllocate(Context, N+T);
  SR := BIR^.Components;
  SA := BIA^.Components;
  SB := BIB^.Components;
  FillByte(BIR^.Components^, (N+T) * BIGINT_COMP_BYTE_SIZE, 0);
  repeat
    Carry := 0;
    RIndex := I;
    J := 0;
    if (OuterPartial > 0) and ((OuterPartial-I) > 0) and (OuterPartial < N) then
    begin
      RIndex := OuterPartial-1;
      J := OuterPartial-I-1;
    end;
    repeat
      if (InnerPartial > 0) and (RIndex >= InnerPartial) then
        Break;
      Tmp := TBILongComponent(SR[RIndex]) + TBILongComponent(SA[J]) * SB[I] + Carry; // Avoid overflow
      SR[RIndex] := TBIComponent(Tmp and $ffffffff); // Downsize
      Inc(RIndex);
      Carry := Tmp shr BIGINT_COMP_BIT_SIZE;
      Inc(J);
    until J >= N;
    SR[RIndex] := Carry;
    Inc(I);
  until I >= T;
  BIRelease(Context,BIA);
  BIRelease(Context,BIB);
  BITrim(BIR);
  Result := BIR;
end;

// Perform the actual square operation. It takes into account overflow
function BIRegularSquare(var Context: TBigIntContext; BI: PBigInt): PBigInt;
var
  BIR: PBigInt;
  C: Byte;
  Carry: TBILongComponent;
  I, J, T: Integer;
  Tmp, XX: TBILongComponent;
  W, X: PBIComponent;
begin
  T := BI^.Size;
  I := 0;
  BIR := BIAllocate(Context, T*2 + 1);
  W := BIR^.Components;
  X := BI^.Components;
  FillChar(W^,BIR^.Size * BIGINT_COMP_BYTE_SIZE,0);
  repeat
    Tmp := W[2*I] + TBILongComponent(X[I]) * X[I]; // Avoid overflow
    W[2 * I] := DWord(Tmp and $ffffffff);
    Carry := Tmp shr BIGINT_COMP_BIT_SIZE;
    J := I+1;
    while J < T do
    begin
      C := 0;
      XX := TBILongComponent(X[I]) * X[J]; // Avoid overflow
      if BIGINT_COMP_MAX-XX < XX then
        C := 1;
      Tmp := XX shl 1;
      if BIGINT_COMP_MAX-Tmp < W[I + J] then
        C := 1;
      Tmp := Tmp + W[I + J];
      if BIGINT_COMP_MAX-Tmp < Carry then
        C := 1;
      Tmp := Tmp + Carry;
      W[I + J] := DWord(Tmp and $ffffffff);
      Carry := Tmp shr BIGINT_COMP_BIT_SIZE;
      if C > 0 then
        Carry := Carry + BIGINT_COMP_RADIX;
      Inc(J);
    end;
    Tmp := W[I+T]+Carry;
    W[I+T] := DWord(Tmp and $ffffffff);
    W[I+T+1] := Tmp shr BIGINT_COMP_BIT_SIZE;
    Inc(I);
  until I >= T;
  BIRelease(Context, BI);
  BITrim(BIR);
  Result := BIR;
end;

// Stomp on the most significant components to give the illusion of a "mod base radix" operation
function BICompMod(BI: PBigInt; Modulus: Integer): PBigInt;
begin
  if not BICheck(BI^) then
  begin
    Result := nil;
    Exit;
  end;
  if BI^.Size > Modulus then
    BI^.Size := Modulus;
  Result := BI;
end;

// Work out g1, g3, g5, g7... etc for the sliding-window algorithm
procedure BIPrecomputeSlideWindow(var Context: TBigIntContext; Window: Integer; G1: PBigInt);
var
  I, K: Integer;
  G2: PBigInt;
begin
  K := 1;
  I := 0;
  while I < Window-1 do {Compute 2^(window-1)}
  begin
    K := K shl 1;
    Inc(I);
  end;
  Context.G := AllocMem(K*SizeOf(PBigInt));
  Context.G[0] := BIClone(Context, G1^);
  BIPermanent(Context.G[0]);
  G2 := BIResidue(Context, BISquare(Context, Context.G[0])); {g^2}
  for I := 1 to K-1 do
  begin
    Context.G[I] := BIResidue(Context, BIMultiply(Context, Context.G[I-1], BICopy(G2)));
    BIPermanent(Context.G[I]);
  end;
  BIRelease(Context, G2);
  Context.Window := K;
end;

procedure BIInitialize(out Context: TBigIntContext);
var
  BIRadix: PBigInt;
begin
  Context:=Default(TBigIntContext);
  BIRadix:=BIAllocate(Context, 2);
  Context.BIRadix := BIRadix;
  BIRadix^.Components[0] := 0;
  BIRadix^.Components[1] := 1;
  BIPermanent(BIRadix);
end;

// Close the bigint context and free any resources
procedure BITerminate(var Context: TBigIntContext);

Var
  BI,BNext : PBigInt;

begin
  BIRelease(Context, Context.BIRadix);
  Context.BIRadix := nil;
  BI:=Context.FreeList;
  While BI<>Nil do
    begin
    BNext:=BI^.Next;
    if BI^.Components<>Nil then
      FreeMem(BI^.Components);
    FreeMem(BI);
    BI:=BNext;
    end;
  Context.FreeList:=nil;
end;

// Make a bigint object "unfreeable" if BIFree() is called on it
procedure BIPermanent(BI: PBigInt);
begin
  if not BICheck(BI^) then
    Exit;
  if BI^.References <> 1 then
    raise Exception.Create('20220428195735');
  BI^.References := BIGINT_PERMANENT;
end;

// Take a permanent object and make it freeable
procedure BIDepermanent(BI: PBigInt);
begin
  if not BICheck(BI^) then
    Exit;
  if BI^.References <> BIGINT_PERMANENT then
    raise Exception.Create('20220428203636');
  BI^.References := 1;
end;

// Free a bigint object so it can be used again
procedure BIRelease(var Context: TBigIntContext; BI: PBigInt);
begin
  if not BICheck(BI^) then
    Exit;
  if BI^.References = BIGINT_PERMANENT then
    Exit;
  Dec(BI^.References);
  if BI^.References > 0 then
    Exit;
  BI^.Next := Context.FreeList;
  Context.FreeList := BI;
  Inc(Context.FreeCount);
  Dec(Context.ActiveCount);
  {$IFDEF BIGINT_DEBUG}
  if Context.ActiveCount < 0 then
    raise Exception.Create('20220428203546');
  {$ENDIF}
end;

{==============================================================================}

// Increment the number of references to this object
function BICopy(BI: PBigInt): PBigInt;
begin
  if not BICheck(BI^) then
  begin
    Result := nil;
    Exit;
  end;
  if BI^.References <> BIGINT_PERMANENT then
    Inc(BI^.References);
  Result := BI;
end;

function BIClone(var Context: TBigIntContext; const BI: TBigInt): PBigInt;
begin
  if not BICheck(BI) then
  begin
    Result:=nil;
    Exit;
  end;
  Result := BIAllocate(Context, BI.Size);
  System.Move(BI.Components^, Result^.Components^, BI.Size*BIGINT_COMP_BYTE_SIZE);
end;

procedure BIExport(var Context: TBigIntContext; BI: PBigInt; Data: PByte;
  Size: Integer; Release: boolean);
{Take a bigint and convert it into a byte sequence}
{Context: The bigint session context}
{BI: The bigint to be converted}
{Data: The converted data as a byte stream}
{Size: The maximum size of the byte stream. Unused bytes will be zeroed}
var
  I: Integer;
  J: Integer;
  K: Integer;
begin
  if not BICheck(BI^) then
    Exit;
  FillByte(Data^,Size,0);
  K := Size-1;
  try
    for I := 0 to BI^.Size - 1 do
    begin
      for J := 0 to BIGINT_COMP_BYTE_SIZE - 1 do
      begin
        Data[K]:=(BI^.Components[I] shr (J * 8)) and $ff;
        Dec(K);
        if K < 0 then
          Exit;
      end;
    end;
  finally
    if Release then
      BIRelease(Context,BI);
  end;
end;

// Allow a binary sequence to be imported as a bigint
function BIImport(var Context: TBigIntContext; Data: PByte; const Size: Integer): PBigInt;
var
  I: Integer;
  J: Integer;
  Offset: Integer;
begin
  J := 0;
  Offset := 0;
  Result := BIAllocate(Context, (Size+BIGINT_COMP_BYTE_SIZE-1) div BIGINT_COMP_BYTE_SIZE);
  FillByte(Result^.Components^, Result^.Size*BIGINT_COMP_BYTE_SIZE, 0);
  for I := Size-1 downto 0 do
  begin
    Result^.Components[Offset] := Result^.Components[Offset] + (TBIComponent(Data[I]) shl (J * 8));
    Inc(J);
    if J = BIGINT_COMP_BYTE_SIZE then
    begin
      J := 0;
      Inc(Offset);
    end;
  end;
end;

function BIImport(var Context: TBigIntContext; const Data: TBytes): PBigInt;
begin
  Result := BIImport(Context, @Data[0], Length(Data));
end;

function BIImport(var Context: TBigIntContext; const Data: AnsiString): PBigInt; overload;

begin
  Result:=BIImport(Context,@Data[1],length(Data));
end;


// Convert an (unsigned) integer into a bigint
// I: The (unsigned) integer to be converted
function IntToBI(var Context: TBigIntContext; I: TBIComponent): PBigInt;
begin
  Result := BIAllocate(Context,1);
  Result^.Components[0] := I;
end;

function BIBitCount(BI: PBigInt): integer;
var
  i: Integer;
  c: TBIComponent;
begin
  i:=BI^.Size-1;
  while (i>=0) and (BI^.Components[i]=0) do
    dec(i);
  if i<0 then
    exit(0);
  Result:=i*BIGINT_COMP_BIT_SIZE;
  c:=BI^.Components[i];
  while (c>0) do
  begin
    inc(Result);
    c:=c shr 1;
  end;
end;

function BIAdd(var Context: TBigIntContext; BIA, BIB: PBigInt): PBigInt;
var
  N: Integer;
  PA: PBIComponent;
  PB: PBIComponent;
  Carry: TBIComponent;
  SL: TBIComponent;
  RL: TBIComponent;
  CY1: TBIComponent;
begin
  Carry := 0;
  if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
    Result:=nil;
    Exit;
  end;
  N := Max(BIA^.Size, BIB^.Size);
  BIResizeComponents(BIA, N+1);
  BIResizeComponents(BIB, N);
  PA := BIA^.Components;
  PB := BIB^.Components;
  repeat
    SL := PA^ + PB^;
    Inc(PB);
    RL := SL + Carry;
    if SL < PA^ then
      CY1 := 1
    else
      CY1 := 0;
    if RL < SL then
      Carry := CY1 or 1
    else
      Carry := CY1 or 0;
    PA^ := RL;
    Inc(PA);
    Dec(N);
  until N = 0;
  PA^ := Carry; // Do overflow
  BIRelease(Context, BIB);
  BITrim(BIA);
  Result := BIA;
end;

// @IsNegative: Indicates that the result was negative
// Return: The result of the subtraction. The result is always positive
function BISubtract(var Context: TBigIntContext; BIA, BIB: PBigInt; out IsNegative: Boolean): PBigInt;
var
  Carry: TBIComponent;
  CY1: TBIComponent;
  N: Integer;
  PA: PBIComponent;
  PB: PBIComponent;
  SL: TBIComponent;
  RL: TBIComponent;
begin
  Carry := 0;
  if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
    Result:=nil;
    Exit;
  end;
  N := BIA^.Size;
  BIResizeComponents(BIB, N);
  PA := BIA^.Components;
  PB := BIB^.Components;
  repeat
    SL := PA^-PB^;
    Inc(PB);
    RL := SL-Carry;
    if SL > PA^ then
      CY1 := 1
    else
      CY1 :=0 ;
    if RL > SL then
      Carry := CY1 or 1
    else
      Carry:=CY1 or 0;
    PA^ := RL;
    Inc(PA);
    Dec(N);
  until N = 0;
  IsNegative := Carry <> 0;
  BITrim(BIB); // Put BIB back to the way it was
  BIRelease(Context, BIB);
  BITrim(BIA);
  Result := BIA;
end;

// Does both division and modulo calculations
// @U: A bigint which is the numerator
// @V: Either the denominator or the modulus depending on the mode
// @IsMod: Determines if this is a normal division (False) or a reduction (True)}
function BIDivide(var Context: TBigIntContext; U, V: PBigInt; IsMod: Boolean): PBigInt;

  function BIDivide_V1(V: PBigInt): TBIComponent; inline;
  begin // V1 for division
    Result := V^.Components[V^.Size-1];
  end;

  function BIDivide_V2(V: PBigInt): TBIComponent; inline;
  begin  // V2 for division
    Result := V^.Components[V^.Size-2];
  end;

  function BIDivide_U(TmpU: PBigInt; J: Integer): TBIComponent; inline;
  begin // U(J) for division
    Result := TmpU^.Components[TmpU^.Size-J-1];
  end;

var
  D, Inner: TBIComponent;
  IsNegative: Boolean;
  J, M, N: Integer;
  ModOffset: Byte;
  OrigUSize: Integer;
  QDash: TBIComponent;
  Quotient, TmpU: PBigInt;
begin
  N := V^.Size;
  M := U^.Size - N;
  J := 0;
  OrigUSize := U^.Size;
  ModOffset := Context.ModOffset;
  if not BICheck(U^) or not BICheck(V^) then
  begin
    Result:=nil;
    Exit;
  end;
  // If doing reduction and we are < mod, then return mod
  if IsMod and (BICompare(V, U) > 0) then
  begin
    BIRelease(Context, V);
    Result := U;
    Exit;
  end;
  Quotient := BIAllocate(Context, M+1);
  TmpU := BIAllocate(Context, N+1);
  BITrim(V); // Make sure we have no leading 0's
  D := BIGINT_COMP_RADIX div (TBILongComponent(BIDivide_V1(V)) + 1);
  FillByte(Quotient^.Components^, Quotient^.Size * BIGINT_COMP_BYTE_SIZE, 0);
  if D > 1 then
  begin // Normalize
    U := BIIntMultiply(Context,U,D);
    if IsMod then
      V := Context.BINormalisedMod[ModOffset]
    else
      V:=BIIntMultiply(Context,V,D);
  end;
  if OrigUSize = U^.Size then
    BIResizeComponents(U, OrigUSize+1); // New digit position u0
  repeat
    // Get a temporary short version of u
    System.Move(U^.Components[U^.Size-N-1-J], TmpU^.Components^, (N+1) * BIGINT_COMP_BYTE_SIZE);
    // Calculate q'
    if BIDivide_U(TmpU, 0) = BIDivide_V1(V) then
    begin
      QDash := BIGINT_COMP_RADIX-1;
    end else
    begin
      QDash := (TBILongComponent(BIDivide_U(TmpU, 0)) * BIGINT_COMP_RADIX + BIDivide_U(TmpU, 1)) div BIDivide_V1(V);
      if (V^.Size > 1) and (BIDivide_V2(V) > 0) then
      begin
        // We are implementing the following: if (V2*q_dash > (((U(0)*COMP_RADIX + U(1) - q_dash*V1)*COMP_RADIX) + U(2))) ...
        Inner := (BIGINT_COMP_RADIX * BIDivide_U(TmpU, 0) + BIDivide_U(TmpU, 1)
                  - TBILongComponent(QDash) * BIDivide_V1(V))
                  and $ffffffff; {Avoid overflow}
        if (TBILongComponent(BIDivide_V2(V)) * QDash) >
            (TBILongComponent(Inner) * BIGINT_COMP_RADIX + BIDivide_U(TmpU, 2)) then {Avoid overflow}
          Dec(QDash);
      end;
    end;
    if QDash > 0 then
    begin // Multiply and subtract
      TmpU := BISubtract(Context, TmpU, BIIntMultiply(Context, BICopy(V), QDash), IsNegative);
      BIResizeComponents(TmpU, N+1);
      Quotient^.Components[Quotient^.Size-J-1] := QDash;
      if IsNegative then
      begin // Add back
        Dec(Quotient^.Components[Quotient^.Size-J-1]);
        TmpU := BIAdd(Context, TmpU, BICopy(V));
        // Lop off the carry
        Dec(TmpU^.Size);
        Dec(V^.Size);
      end;
    end else
    begin
      Quotient^.Components[Quotient^.Size-J-1] := 0;
    end;
    // Copy back to U
    System.Move(TmpU^.Components^, U^.Components[U^.Size-N-1-J], (N+1) * BIGINT_COMP_BYTE_SIZE);
    Inc(J);
  until J > M;
  BIRelease(Context, TmpU);
  BIRelease(Context, V);
  if IsMod then
  begin // Get the remainder
    BIRelease(Context, Quotient);
    BITrim(U);
    Result := BIIntDivide(U, D);
  end else
  begin // Get the quotient
    BIRelease(Context, U);
    BITrim(Quotient);
    Result := Quotient;
  end
end;

function BIMultiply(var Context: TBigIntContext; BIA, BIB: PBigInt): PBigInt;
begin
  if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
    Result:=nil;
    Exit;
  end;
  Result := BIRegularMultiply(Context, BIA, BIB, 0, 0);
end;

// Perform a modular exponentiation.
// it requires BISetMod() to have been called previously
// This is one of the optimisations used for performance
// @BI: The bigint on which to perform the mod power operation
function BIModPower(var Context: TBigIntContext; BI, BIExp: PBigInt): PBigInt;
var
  BIR: PBigInt;
  I, J, L: Int32;
  PartExp: Int32;
  WindowSize: Integer;
begin
  I := BIFindMaxExponentIndex(BIExp);
  BIR := IntToBI(Context, 1);
  WindowSize := 1;
  if not BICheck(BI^) or not BICheck(BIExp^) then
  begin
    Result:=nil;
    Exit;
  end;
  // Work out an optimum size
  J := I;
  while J > 32 do
  begin
    Inc(WindowSize);
    J := J div 5;
  end;
  BIPrecomputeSlideWindow(Context, WindowSize, BI); // Work out the sliding window constants
  repeat // If sliding-window is off, then only one bit will be done at a time and will reduce to standard left-to-right exponentiation
    if BIExpBitIsOne(BIExp,I) then
    begin
      L := I-WindowSize+1;
      PartExp := 0;
      if L < 0 then // LSB of exponent will always be 1
      begin
        L := 0;
      end else
      begin
        while not BIExpBitIsOne(BIExp,L) do
          Inc(L); // Go back up
      end;
      // Build up the section of the exponent
      J := I;
      while J >= L do
      begin
        BIR := BIResidue(Context, BISquare(Context,BIR));
        if BIExpBitIsOne(BIExp, J) then
          Inc(PartExp);
        if J <> L then
          PartExp := PartExp shl 1;
        Dec(J);
      end;
      PartExp := (PartExp-1) div 2; // Adjust for array
      BIR := BIResidue(Context, BIMultiply(Context, BIR, Context.G[PartExp]));
      I := L-1;
    end else
    begin // Square it
      BIR := BIResidue(Context, BISquare(Context, BIR));
      Dec(I);
    end;
  until I < 0;
  for I := 0 to Context.Window-1 do
  begin // Cleanup
    BIDepermanent(Context.G[I]);
    BIRelease(Context, Context.G[I]);
  end;
  BIRelease(Context, BI);
  BIRelease(Context, BIExp);
  Result := BIR;
end;

(*
// Perform a modular exponentiation using a temporary modulus.
// Useful to check the signatures of certificates. The modulus of this function is temporary as it's just used for authentication
// @BI: The bigint to perform the exp/mod
// @BIM: The temporary modulus
function BIModPower2(var Context: TBigIntContext; BI, BIM, BIExp: PBigInt): PBigInt;
var
  BIR: PBigInt;
  TmpBIR: PBigInt;
  TmpContext: TBigIntContext;
begin
  // Set up a temporary bigint context and transfer what we need between them.
  // We need to do this since we want to keep the original modulus which is already in this context.
  // This operation is only called when doing peer verification, and so is not expensive
  BIInitialize(TmpContext, CurrentScratch);
  BISetMod(TmpContext, BIClone(TmpContext, BIM^), BIGINT_M_OFFSET);
  TmpBIR := BIModPower(TmpContext, BIClone(TmpContext, BI^), BIClone(TmpContext, BIExp^));
  BIR := BIClone(Context, TmpBIR^);
  BIRelease(TmpContext, TmpBIR);
  BIFreeMod(TmpContext, BIGINT_M_OFFSET);
  BITerminate(TmpContext);
  BIRelease(Context, BI);
  BIRelease(Context, BIM);
  BIRelease(Context, BIExp);
  Result := BIR;
end;
*)

function BICompare(BIA, BIB: PBigInt): Integer;
var
  A, B: PBIComponent;
  I: Integer;
begin
  if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
    Result := 0;
    Exit;
  end;
  if BIA^.Size > BIB^.Size then
  begin
    Result := 1;
    Exit;
  end;
  if BIA^.Size < BIB^.Size then
  begin
    Result := -1;
    Exit;
  end;
  // Same number of components. Compare starting from the high end and working down
  A := BIA^.Components;
  B := BIB^.Components;
  for I := BIA^.Size-1 downto 0 do
  begin
    if A[I] > B[I] then
      Exit(1)
    else if A[I] < B[I] then
      Exit(-1);
  end;
  Result := 0;
end;

// Pre-compute some of the expensive steps in reduction
// This function should only be called once (normally when a session starts).
// When the session is over, BIFreeMod() should be called. BIModPower() and BIMod() rely on this function being called
// BIM: The bigint modulus that will be used
// ModOffset: There are three moduluii that can be stored - the  standard modulus, and its two primes p and q. This offset refers to which modulus we are referring to
procedure BISetMod(var Context: TBigIntContext; BIM: PBigInt; ModOffset: Integer);
var
  D: TBIComponent;
  K: Integer;
begin
  K := BIM^.Size;
  D := BIGINT_COMP_RADIX div (TBILongComponent(BIM^.Components[K-1]) + 1);
  Context.BIMod[ModOffset] := BIM;
  BIPermanent(Context.BIMod[ModOffset]);
  Context.BINormalisedMod[ModOffset] := BIIntMultiply(Context, BIM, D);
  BIPermanent(Context.BINormalisedMod[ModOffset]);
  Context.BImu[ModOffset] := BIDivide(Context, BICompLeftShift(BIClone(Context, Context.BIRadix^), K*2-1), Context.BIMod[ModOffset], False);
  BIPermanent(Context.BImu[ModOffset]);
  Context.BIbk1[ModOffset] := BICompLeftShift(IntToBI(Context,1), K+1);
  BIPermanent(Context.BIbk1[ModOffset]);
end;

// Used when cleaning various bigints at the end of a session
procedure BIFreeMod(var Context: TBigIntContext; ModOffset: Integer);
begin
  BIDepermanent(Context.BIMod[ModOffset]);
  BIRelease(Context, Context.BIMod[ModOffset]);
  BIDepermanent(Context.BImu[ModOffset]);
  BIRelease(Context, Context.BImu[ModOffset]);
  BIDepermanent(Context.BIbk1[ModOffset]);
  BIRelease(Context, Context.BIbk1[ModOffset]);
  BIDepermanent(Context.BINormalisedMod[ModOffset]);
  BIRelease(Context, Context.BINormalisedMod[ModOffset]);
end;

// Find the residue of BI. BISetMod() must be called beforehand
function BIMod(var Context: TBigIntContext; BI: PBigInt): PBigInt; inline;
begin
  Result := BIDivide(Context, BI, Context.BIMod[Context.ModOffset], True);
end;

// BIResidue is simply an alias for BIBarrett
function BIResidue(var Context: TBigIntContext; BI: PBigInt): PBigInt; inline;
begin
  Result := BIBarrett(Context, BI);
end;

// Perform a single Barrett reduction
function BIBarrett(var Context: TBigIntContext; BI: PBigInt): PBigInt;
var
  Q1:PBigInt;
  Q2:PBigInt;
  Q3:PBigInt;
  R1:PBigInt;
  R2:PBigInt;
  R:PBigInt;
  ModOffset:Byte;
  BIM:PBigInt;
  K:Integer;
  IsNegative:Boolean;
begin
  ModOffset := Context.ModOffset;
  BIM := Context.BIMod[ModOffset];
  K := BIM^.Size;
  if not BICheck(BI^) or not BICheck(BIM^) then
  begin
    Result := nil;
    Exit;
  end;
  if BI^.Size > K*2 then
  begin // Use Classical method instead  - Barrett cannot help here
    Result := BIMod(Context,BI);
    Exit;
  end;
  // q1 = [x / b**(k-1)]
  Q1 := BICompRightshift(BIClone(Context,BI^), K-1);
  // Do outer partial multiply
  // q2 = q1 * mu
  Q2 := BIRegularMultiply(Context, Q1, Context.BImu[ModOffset], 0, K-1);
  // q3 = [q2 / b**(k+1)]
  Q3 := BICompRightShift(Q2, K+1);
  // r1 = x mod b**(k+1)
  R1 := BICompMod(BI, K+1);
  // Do inner partial multiply
  // r2 = q3 * m mod b**(k+1)
  R2 := BICompMod(BIRegularMultiply(Context, Q3, BIM, K+1, 0), K+1);
  // if (r1 < r2) r1 = r1 + b**(k+1)
  if BICompare(R1,R2) < 0 then
    R1 := BIAdd(Context,R1,Context.BIbk1[ModOffset]);
  // r = r1-r2
  R := BISubtract(Context,R1,R2,IsNegative);
  // while (r >= m) do r = r-m
  while BICompare(R,BIM) >= 0 do
    R := BISubtract(Context,R,BIM,IsNegative);
  Result:=R;
end;

// Perform a square operation on a bigint
function BISquare(var Context: TBigIntContext; BI: PBigInt): PBigInt;
begin
  {$IFDEF BIGINT_SQUARE}
    if not BICheck(BI^) then
    begin
      Result := nil;
      Exit;
    end;
    Result := BIRegularSquare(Context, BI);
  {$ELSE}
    Result := BIMultiply(Context, BICopy(BI), BI);
  {$ENDIF}
end;

// Chinese Remainder Theorem to quickly perform RSA decrypts
// @BI: The bigint to perform the exp/mod
// @DP: CRT's dP bigint, @Q: CRT's dQ bigint, @: CRT's p bigint, @: CRT's q bigint, @Inv: CRT's qInv bigint
function BICRT(var Context: TBigIntContext; BI, DP, DQ, P, Q, QInv: PBigInt): PBigInt;
var
  H, M1, M2: PBigInt;
  IsNegative: Boolean;
begin
  Context.ModOffset := BIGINT_P_OFFSET;
  M1 := BIModPower(Context, BICopy(BI), DP);
  Context.ModOffset := BIGINT_Q_OFFSET;
  M2 := BIModPower(Context, BI, DQ);
  H := BISubtract(Context, BIAdd(Context, M1, P), BICopy(M2), IsNegative);
  H := BIMultiply(Context, H, QInv);
  Context.ModOffset := BIGINT_P_OFFSET;
  H := BIResidue(Context, H);
  Result := BIAdd(Context, M2, BIMultiply(Context, Q, H));
end;

function BIToString(BI: PBigInt): AnsiString;
const
  Digits: Array[0..15] of AnsiChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  I,J,K: Integer;
  Num: TBIComponent;
begin
  Result:='';
  if BI = nil then
    Exit;
  SetLength(Result,BI^.Size*BIGINT_COMP_NUM_NIBBLES);
  K:=1;
  for I := BI^.Size-1 downto 0 do
  begin
    for J := BIGINT_COMP_NUM_NIBBLES-1 downto 0 do
    begin
      Num := (BI^.Components[I] shr (J*4)) and $F;
      Result[K]:=Digits[Num];
      inc(K);
    end;
  end;
end;

function BIToDbgString(BI: PBigInt): AnsiString;
var
  Num, I, J: Integer;
begin
  Result:='';
  if BI=nil then
    exit;
  Num:=0;
  for I := BI^.Size-1 downto 0 do
    for J := BIGINT_COMP_NUM_NIBBLES-1 downto 0 do
      inc(Num, (BI^.Components[I] shr (J*4)) and $F);
  Result:='{'+IntToStr(Num)+'}'+BIToString(BI);
end;

procedure BIToString(BI: PBigInt; out S: AnsiString);
begin
  S:=BIToString(BI);
end;

// Convert a string of hex characters to a bigint
function StringToBI(var Context: TBigIntContext; const Value: AnsiString): PBigInt;
const
  Convert: array[Ord('0')..Ord('f')] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  Num: Byte;
  I, J, Offset, Size: Integer;
  BIR: PBigInt;
begin
  J := 0;
  Offset := 0;
  Size := Value.Length;
  BIR := BIAllocate(Context, (Size+BIGINT_COMP_NUM_NIBBLES-1) div BIGINT_COMP_NUM_NIBBLES);
  FillChar(BIR^.Components^, BIR^.Size*BIGINT_COMP_BYTE_SIZE, 0);
  for I := Size downto 1 do
  begin
    Num := Byte(Convert[Ord(Value[I])]);
    if Num = 255 then
    begin
      Result := nil;
      Exit;
    end;
    BIR^.Components[Offset] := BIR^.Components[Offset] + (LongWord(Num) shl (J * 4));
    Inc(J);
    if J = BIGINT_COMP_NUM_NIBBLES then
    begin
      J := 0;
      Inc(Offset);
    end;
  end;
  Result := BIR;
end;

initialization

finalization

end.

