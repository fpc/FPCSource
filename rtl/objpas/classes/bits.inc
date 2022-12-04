{%MainUnit classes.pp}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                               TBits                                      *}
{****************************************************************************}

const
   TBITS_SHIFT = round(ln(bitsizeof(TBitsBase)) / ln(2));
   TBITS_MASK = 1 shl TBITS_SHIFT - 1;

Procedure BitsErrorFmt (const Msg : string; const Args : array of const);
begin
  Raise EBitsError.CreateFmt(Msg,args) at get_caller_addr(get_frame), get_caller_frame(get_frame);
end;

procedure TBits.CheckBitIndex (Bit : SizeInt;CurrentSize : Boolean);

begin
 if (bit<0) or (CurrentSize and (Bit >= FBSize)) then
   BitsErrorFmt(SErrInvalidBitIndex,[bit]);
 if (bit>=MaxBitFlags) then
   BitsErrorFmt(SErrIndexTooLarge,[bit])
end;

{ ************* functions to match TBits class ************* }

procedure TBits.setSize(value: SizeInt);
var
  newSize: SizeInt;
begin
  CheckBitIndex(value, false);
  newSize := value shr TBITS_SHIFT + ord(value and TBITS_MASK <> 0);
  if newSize <> FSize then
  begin
    ReAllocMem(FBits, newSize * SizeOf(TBitsBase));
    if newSize > FSize then
      FillChar(FBits[FSize], (newSize - FSize) * sizeof(TBitsBase), 0);
    FSize := newSize;
  end;

  { If the new size is in the middle of the last chunk, zero its upper bits, so they won't reappear on resizing back. }
  if value and TBITS_MASK <> 0 then
    FBits[value shr TBITS_SHIFT] := FBits[value shr TBITS_SHIFT] and (TBitsBase(1) shl (value and TBITS_MASK) - 1);

  FBSize := value;
end;

function TBits.ScanFor1(start : SizeInt; xorMask : TBitsBase) : SizeInt;
var
   cell: TBitsBase;
begin
   result := start;
   while result < FBSize do
   begin
      { On first iteration, position ('result') is arbitrary.
        On subsequent iterations, position is always 0 modulo bitsizeof(TBitsBase) - points to the start of the next FBits item,
        and (result and TBITS_MASK) becomes 0 (number of lower bits to skip). }
      cell := (xorMask xor FBits[result shr TBITS_SHIFT]) shr (result and TBITS_MASK);
      if cell <> 0 then
      begin
         result := result + integer(
{$if sizeof(TBitsBase) = sizeof(byte)}
            BsfByte
{$elseif sizeof(TBitsBase) = sizeof(word)}
            BsfWord
{$elseif sizeof(TBitsBase) = sizeof(dword)}
            BsfDWord
{$elseif sizeof(TBitsBase) = sizeof(qword)}
            BsfQWord
{$else} {$error unknown TBitsBase} {$endif}
               (cell));
         if result >= FBSize then
            result := -1;
         exit;
      end;
      result := (result + bitsizeof(TBitsBase)) and not SizeInt(TBITS_MASK);
   end;
   result := -1;
end;

function TBits.ScanFor1Rev(start : SizeInt; xorMask : TBitsBase) : SizeInt;
var
   cell: TBitsBase;
begin
   result := start;
   while result >= 0 do
   begin
      { On first iteration, position ('result') is arbitrary.
        On subsequent iterations, position is always -1 modulo bitsizeof(TBitsBase) - points to the end of the previous FBits item,
        and ((-result - 1) and TBITS_MASK) becomes 0 (number of upper bits to skip). }
      cell := TBitsBase((xorMask xor FBits[result shr TBITS_SHIFT]) shl ((-result - 1) and TBITS_MASK));
      if cell <> 0 then
         exit(result - TBITS_MASK + integer(
{$if sizeof(TBitsBase) = sizeof(byte)}
            BsrByte
{$elseif sizeof(TBitsBase) = sizeof(word)}
            BsrWord
{$elseif sizeof(TBitsBase) = sizeof(dword)}
            BsrDWord
{$elseif sizeof(TBitsBase) = sizeof(qword)}
            BsrQWord
{$else} {$error unknown TBitsBase} {$endif}
               (cell)));
      result := (result - bitsizeof(TBitsBase)) or TBITS_MASK;
   end;
   result := -1;
end;

procedure TBits.SetBit(bit : SizeInt; value : Boolean);
var
   cell: PBitsBase;
   mask: TBitsBase;
begin
   grow(bit+1);
   cell := FBits + bit shr TBITS_SHIFT;
   mask := TBitsBase(1) shl (bit and TBITS_MASK);
   if value then
      cell^ := cell^ or mask
   else
      cell^ := cell^ and not mask;
end;

function TBits.OpenBit : SizeInt;
begin
   result := ScanFor1(0, High(TBitsBase));
   if result < 0 then
      result := FBSize;
end;

{ ******************** TBits ***************************** }

constructor TBits.Create(theSize : longint = 0 );
begin
   findIndex := -1;
   if TheSize > 0 then grow(theSize);
end;

destructor TBits.Destroy;
begin
   FreeMem(FBits);
   inherited Destroy;
end;

procedure TBits.grow(nbit: SizeInt);
begin
  if nbit > FBSize then
    SetSize(nbit);
end;

function TBits.getFSize : SizeInt;
begin
   result := FSize;
end;

procedure TBits.seton(bit : SizeInt);
begin
  grow(bit+1);
  FBits[bit shr TBITS_SHIFT] := FBits[bit shr TBITS_SHIFT] or TBitsBase(1) shl (bit and TBITS_MASK);
end;

procedure TBits.clear(bit : SizeInt);
begin
  grow(bit+1);
  FBits[bit shr TBITS_SHIFT] := FBits[bit shr TBITS_SHIFT] and not (TBitsBase(1) shl (bit and TBITS_MASK));
end;

procedure TBits.clearall;
begin
   FillChar(FBits^, FSize * sizeof(TBitsBase), 0);
{ don't clear FBSize here, it will cause exceptions on subsequent reading bit values }
{ use 'Size := 0' to reset everything and deallocate storage }
end;

function TBits.get(bit : SizeInt) : Boolean;
begin
   CheckBitIndex(bit,true);
   result := FBits[bit shr TBITS_SHIFT] shr (bit and TBITS_MASK) and 1 <> 0;
end;

procedure TBits.CopyBits(BitSet : TBits);
begin
  setSize(bitset.Size);
  Move(bitset.FBits^,FBits^,FSize*SizeOf(TBitsBase));
end;

procedure TBits.andbits(bitset : TBits);
var
   n, loop : SizeInt;
begin
   n := FSize;
   if bitset.FSize < n then
      n := bitset.FSize;

   for loop := 0 to n - 1 do
      FBits[loop] := FBits[loop] and bitset.FBits[loop];

   if FSize > n then
      FillChar(FBits[n], (FSize - n) * sizeof(TBitsBase), 0);
end;

procedure TBits.notbits(bitset : TBits);
var
   n, loop : SizeInt;
begin
   n := FSize;
   if bitset.FSize < n then
      n := bitset.FSize;

   for loop := 0 to n - 1 do
      FBits[loop] := FBits[loop] xor bitset.FBits[loop];

   { Zero upper bits, for similar reason as in SetSize. }
   if FBSize and TBITS_MASK <> 0 then
      FBits[FBSize shr TBITS_SHIFT] := FBits[FBSize shr TBITS_SHIFT] and (TBitsBase(1) shl (FBSize and TBITS_MASK) - 1);
end;

procedure TBits.orbits(bitset : TBits);
var
   loop : SizeInt;
begin
   grow(bitset.Size);

   for loop := 0 to bitset.FSize - 1 do
      FBits[loop] := FBits[loop] or bitset.FBits[loop];
end;

procedure TBits.xorbits(bitset : TBits);
var
   loop : SizeInt;
begin
   grow(bitset.Size);

   for loop := 0 to bitset.FSize - 1 do
      FBits[loop] := FBits[loop] xor bitset.FBits[loop];
end;

function TBits.Equals(Obj : TObject): Boolean;
begin
  if Obj is TBits then
    Result := Equals(TBits(Obj))
  else
    Result := inherited Equals(Obj);
end;

function TBits.equals(bitset : TBits) : Boolean;
var
   smallest, largest : TBits;
begin
   if FBSize < bitset.FBSize then
   begin
      smallest := self;
      largest := bitset;
   end else
   begin
      smallest := bitset;
      largest := self;
   end;

   result :=
      (CompareByte(smallest.FBits^, largest.FBits^, smallest.FSize * sizeof(TBitsBase)) = 0) and
      (
         { First smallest.FSize TBitsBases were equal, so scan can start from the next. }
         (largest.FSize = smallest.FSize) or
         (largest.ScanFor1(smallest.FSize shl TBITS_SHIFT, 0) < 0)
      );
end;


{ us this in place of calling FindFirstBit. It sets the current }
{ index used by FindNextBit and FindPrevBit                     }

procedure TBits.SetIndex(index : SizeInt);
begin
   CheckBitIndex(index,true);
   findIndex := index;
end;


{ When state is set to True it looks for bits that are turned On (1) }
{ and when it is set to False it looks for bits that are turned      }
{ off (0).                                                           }

function TBits.FindFirstBit(state : boolean) : SizeInt;
begin
   { -TBitsBaseSigned(not state) is 0 for true or -1 for false, making following ScanFor1s search for 'state'. }
   findXorMask := TBitsBase(-TBitsBaseSigned(not state));
   result := ScanFor1(0, findXorMask);
   findIndex := result;
end;

function TBits.FindNextBit : SizeInt;
begin
   result := findIndex;
   if result >= 0 then
   begin
      result := ScanFor1(result + 1, findXorMask);
      findIndex := result;
   end;
end;

function TBits.FindPrevBit : SizeInt;
begin
   result := findIndex;
   if result >= 0 then
   begin
      result := ScanFor1Rev(result - 1, findXorMask);
      findIndex := result;
   end;
end;

function TBits.Find(State: boolean; StartIndex: SizeInt): SizeInt;
begin
   if SizeUint(StartIndex) < SizeUint(FBSize) then
     result := ScanFor1(StartIndex, TBitsBase(-TBitsBaseSigned(not State)))
   else
     result := -1;
end;

function TBits.FindRev(State: boolean; StartIndex: SizeInt): SizeInt;
begin
   if SizeUint(StartIndex) < SizeUint(FBSize) then
     result := ScanFor1Rev(StartIndex, TBitsBase(-TBitsBaseSigned(not State)))
   else
     result := -1;
end;

function TBits.Find0(StartIndex: SizeInt): SizeInt;
begin
   if SizeUint(StartIndex) < SizeUint(FBSize) then
     result := ScanFor1(StartIndex, High(TBitsBase))
   else
     result := -1;
end;

function TBits.Find0Rev(StartIndex: SizeInt): SizeInt;
begin
   if SizeUint(StartIndex) < SizeUint(FBSize) then
     result := ScanFor1Rev(StartIndex, High(TBitsBase))
   else
     result := -1;
end;

function TBits.Find1(StartIndex: SizeInt): SizeInt;
begin
   if SizeUint(StartIndex) < SizeUint(FBSize) then
     result := ScanFor1(StartIndex, 0)
   else
     result := -1;
end;

function TBits.Find1Rev(StartIndex: SizeInt): SizeInt;
begin
   if SizeUint(StartIndex) < SizeUint(FBSize) then
     result := ScanFor1Rev(StartIndex, 0)
   else
     result := -1;
end;

class procedure TBits.MoveRange(SrcBf: TBits; SrcPos: SizeInt; DstBf: TBits; DstPos, Count: SizeInt);
var
  Src, Dst: PBitsBase;
  N: SizeInt;
begin
  SrcBf.ValidateRange(SrcPos, Count);
  DstBf.FitRange(DstPos, Count);
  Src := SrcBf.FBits;
  Dst := DstBf.FBits;

  { Can move left-to-right? }
  if (Src <> Dst) or (SrcPos >= DstPos) or (SrcPos + Count <= DstPos) then
  begin
    {
      For illustration, let TBitsBase be 8-bit, and:

            0       1       2       3       4       5       6
      Src = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
                         ^SrcPos=13               ^SrcPos+Count=38
      Dst = ....................................................
            0 ^DstPos=2     2       3       4       5       6

      First, align DstPos to the cell boundary by reading bitsizeof(TBitsBase) - DstPos mod bitsizeof(TBitsBase) bits with ReadSmall
      and writing them with Write1.
      Result will be:

            0       1       2       3       4       5       6
      Src = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
                               ^SrcPos=19         ^SrcPos+Count=38
      Dst = ..NOPQRS............................................
            0       1       2       3       4       5       6
                    ^DstPos=8
    }

    N := DstPos and TBITS_MASK;
    if N <> 0 then
    begin
      N := bitsizeof(TBitsBase) - N;
      if Count < N then
        if Count = 0 then exit else N := Count;
      Write1(Dst, DstPos, N, ReadSmall(Src, SrcPos, N));
      Inc(SrcPos, N); Inc(DstPos, N); Dec(Count, N);
    end;

    {
      Fill Dst in whole cells while possible. Result will be:

      Drc = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
                                               ^SrcPos=35
                                                  ^SrcPos+Count=38
      Dst = ..NOPQRSTUVWXYZabcdefghi............................
            0       1       2       3       4       5       6
                                    ^DstPos=24
    }

    N := Count shr TBITS_SHIFT;
    if N > 0 then
    begin
      if SrcPos and TBITS_MASK = 0 then
        { Not even an optimization, as the second branch requires nonzero shift. }
        Move(Src[SrcPos shr TBITS_SHIFT], Dst[DstPos shr TBITS_SHIFT], N * sizeof(TBitsBase))
      else
        MoveUnalignedSrcToAlignedDest(Src + SrcPos shr TBITS_SHIFT, Dst + DstPos shr TBITS_SHIFT, N, SrcPos and TBITS_MASK);
      N := N shl TBITS_SHIFT;
      Inc(SrcPos, N); Inc(DstPos, N); Dec(Count, N);
    end;

    { Append the tail. }
    if Count > 0 then
    begin
      Inc(Dst, DstPos shr TBITS_SHIFT);
      Dst^ := Dst^ shr Count shl Count or ReadSmall(Src, SrcPos, Count);
    end;
  end else
  begin
    { Same as above but right-to-left. }
    Inc(SrcPos, Count); Inc(DstPos, Count);
    N := DstPos and TBITS_MASK;
    if N <> 0 then
    begin
      if Count < N then
        if Count = 0 then exit else N := Count;
      Dec(SrcPos, N); Dec(DstPos, N); Dec(Count, N);
      Write1(Dst, DstPos, N, ReadSmall(Src, SrcPos, N));
    end;

    N := Count shr TBITS_SHIFT;
    if N > 0 then
    begin
      if SrcPos and TBITS_MASK = 0 then
        Move(Src[SrcPos shr TBITS_SHIFT - N], Dst[DstPos shr TBITS_SHIFT - N], N * sizeof(TBitsBase))
      else
        MoveUnalignedSrcToAlignedDestRightToLeft(Src + SrcPos shr TBITS_SHIFT, Dst + DstPos shr TBITS_SHIFT, n, srcPos and TBITS_MASK);
      N := N shl TBITS_SHIFT;
      Dec(SrcPos, N); Dec(DstPos, N); Dec(Count, N);
    end;

    if Count > 0 then
    begin
      Inc(Dst, (DstPos - Count) shr TBITS_SHIFT);
      Dst^ := TBitsBase(Dst^ shl Count) shr Count or ReadSmall(Src, SrcPos - Count, Count) shl (bitsizeof(TBitsBase) - count);
    end;
  end;
end;

procedure TBits.MoveRange(SrcPos, DstPos, Count: SizeInt);
begin
  MoveRange(Self, SrcPos, Self, DstPos, Count);
end;

procedure TBits.ValidateRange(Pos, Count: SizeInt);
begin
  if (SizeUint(Pos) > SizeUint(FBSize)) or (SizeUint(Count) > SizeUint(FBSize - Pos)) then
    ThrowBadRange(Pos, Count);
end;

procedure TBits.FitRange(Pos, Count: SizeInt);
begin
  if (SizeUint(Pos) > SizeUint(FBSize)) or (SizeUint(Count) > SizeUint(FBSize - Pos)) then
    if (SizeUint(Pos) <= MaxBitFlags) and (SizeUint(Count) <= SizeUint(MaxBitFlags - Pos)) then
      SetSize(Pos + Count)
    else
      ThrowBadRange(Pos, Count);
end;

procedure TBits.ThrowBadRange(Pos, Count: SizeInt);
begin
  BitsErrorFmt(SOutOfRange + ' | Pos=%d, Count=%d', [0, FBSize - 1, Pos, Count]);
end;

class procedure TBits.MoveUnalignedSrcToAlignedDest(Src, Dst: PBitsBase; Count, Src1Bits: SizeInt);
var
  Src0Bits: SizeInt;
  X, X1: TBitsBase;
  SrcE: PBitsBase;
begin
  { Prerequisites: Count > 0, 0 < Src1Bits < bitsizeof(TBitsBase). }
  {
    Example from MoveBits after aligning DstPos to the cell boundary becomes:
                              v Src
              0       1       2       3       4       5       6
    OrigSrc = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
                                 ^srcPos=19         ^srcPos+count=38
    OrigDst = ..NOPQRS............................................
              0       1       2       3       4       5       6
                      ^Dst

    Src1Bits = SrcPos mod 8 = 3, Src0Bits = 8 - SrcPos mod 8 = 5.
    Dst[0] = TUVWXYZa = Src0Bits upper bits of Src[0] + Src1Bits lower bits of Src[1], and so on.
  }
  X := Src[0];
  Src0Bits := bitsizeof(TBitsBase) - Src1Bits;

  { Unrolled loop by 2. Not a big complication because loop by 1 would require explicit X := X1, while loop by 2 ping-pongs between X and X1. }
  SrcE := Src + Count shr 1 shl 1;
  while Src < SrcE do
  begin
    X1 := Src[1];
    Dst^ := TBitsBase(X shr Src1Bits or X1 shl Src0Bits);
    X := Src[2];
    Dst[1] := TBitsBase(X1 shr Src1Bits or X shl Src0Bits);
    Inc(Src, 2); Inc(Dst, 2);
  end;

  if Count and 1 <> 0 then
    Dst^ := TBitsBase(X shr Src1Bits or Src[1] shl Src0Bits);
end;

class procedure TBits.MoveUnalignedSrcToAlignedDestRightToLeft(SrcR, DstR: PBitsBase; Count, Src0Bits: SizeInt);
var
  Src1Bits: SizeInt;
  X, X1: TBitsBase;
  SrcRE: PBitsBase;
begin
  { Prerequisites: Count > 0, 0 < Src0Bits < bitsizeof(TBitsBase). }
  X := SrcR[0];
  Src1Bits := bitsizeof(TBitsBase) - Src0Bits;

  SrcRE := SrcR - Count shr 1 shl 1;
  while SrcR > SrcRE do
  begin
    Dec(SrcR, 2); Dec(DstR, 2);
    X1 := SrcR[1];
    DstR[1] := TBitsBase(X shl Src1Bits or X1 shr Src0Bits);
    X := SrcR[0];
    DstR[0] := TBitsBase(X1 shl Src1Bits or X shr Src0Bits);
  end;

  if Count and 1 <> 0 then
    DstR[-1] := TBitsBase(X shl Src1Bits or SrcR[-1] shr Src0Bits);
end;

{ Read 0 < Count < bitsizeof(TBitsBase) bits, possibly across cell boundary. }
class function TBits.ReadSmall(Src: PBitsBase; Pos, Count: SizeInt): TBitsBase;
var
  NRes: SizeInt;
begin
  {
          0       1       2       3       4       5       6
    src = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
               ^pos = 5
                     ^count = 6, pos + count = 11
  }
  Inc(Src, Pos shr TBITS_SHIFT);
  Pos := Pos and TBITS_MASK;
  Result := Src^ shr Pos; { FGH }
  NRes := bitsizeof(TBitsBase) - Pos; { 8 − 5 = 3 = len(FGH) }
  if Count > NRes then Result := TBitsBase(Result or Src[1] shl NRes); { FGH | 000IJKLMNOP }
  Result := Result and (TBitsBase(1) shl Count - 1); { FGHIJK }
end;

{ Write 0 < Count < bitsizeof(TBitsBase) bits that don't cross cell boundary. }
class procedure TBits.Write1(Dst: PBitsBase; Pos, Count: SizeInt; Value: TBitsBase);
begin
  {
    Value = qwer
          0       1       2       3       4       5       6
    Dst = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
             ^Pos = 3
                 ^Count = 4, Pos + Count = 7
  }
  Inc(Dst, Pos shr TBITS_SHIFT);
  Pos := Pos and TBITS_MASK;
  Dst^ :=
    Dst^ { ABCDEFGH }
    and not ((TBitsBase(1) shl Count - 1) shl Pos) { ABC0000H }
    or (Value shl Pos); { ABCqwerH }
end;

function TBits.GetUint(Pos, Count: SizeInt): Cardinal;
var
  Src: PBitsBase;
  Rd: SizeInt;
begin
  if (SizeUint(Pos) > SizeUint(FBSize)) or (SizeUint(Count) > SizeUint(FBSize - Pos)) or not ((Count >= 1) and (Count <= bitsizeof(Cardinal))) then
    ThrowBadRange(Pos, Count);

  Src := FBits + Pos shr TBITS_SHIFT;
  Pos := Pos and TBITS_MASK;
  Result := Cardinal(Src^ shr pos);
  Rd := bitsizeof(TBitsBase) - Pos;
{$if sizeof(Cardinal) > sizeof(TBitsBase)}
  while Rd < Count do
  begin
    Inc(Src);
    Result := Result or Cardinal(Src^) shl Rd;
    Inc(Rd, bitsizeof(TBitsBase));
  end;
{$else}
  if Rd < Count then
    Result := Result or Cardinal(Src[1]) shl Rd;
{$endif}
  if Count < bitsizeof(Cardinal) then
    Result := Result and (Cardinal(1) shl Count - 1);
end;

procedure TBits.SetUint(Pos, Count: SizeInt; Value: Cardinal);
var
  Wr: SizeInt;
  Dst: PBitsBase;
begin
  if not ((Count >= 1) and (Count <= bitsizeof(Cardinal))) then
    ThrowBadRange(Pos, Count);
  if (Count < bitsizeof(Cardinal)) and (Value shr Count <> 0) then
    BitsErrorFmt(SOutOfRange, [0, Cardinal(1) shl Count - 1]);
  FitRange(Pos, Count);
  Dst := FBits + Pos shr TBITS_SHIFT;
  Wr := -Pos and TBITS_MASK;
  if Wr >= Count then
    Dst^ := Dst^ and not ((TBitsBase(1) shl Count - 1) shl (Pos and TBITS_MASK)) or TBitsBase(Value) shl (pos and TBITS_MASK)
  else
  begin
{$if sizeof(Cardinal) > sizeof(TBitsBase)}
    if Wr > 0 then
    begin
      Dst^ := TBitsBase(TBitsBase(Dst^ shl Wr) shr Wr or TBitsBase(Value) shl (bitsizeof(TBitsBase) - Wr));
      Inc(Dst);
    end;
    while Wr + bitsizeof(TBitsBase) <= Count do
    begin
      Dst^ := TBitsBase(Value shr wr);
      Inc(Dst);
      Inc(Wr, bitsizeof(TBitsBase));
    end;
    if Wr < Count then
      Dst^ := Dst^ shr (Count - Wr) shl (Count - Wr) or Value shr Wr;
{$else}
    if Wr > 0 then
    begin
      Dst^ := TBitsBase(TBitsBase(Dst^ shl Wr) shr Wr or TBitsBase(Value) shl (bitsizeof(TBitsBase) - wr));
      Dst[1] := Dst[1] shr (Count - Wr) shl (Count - Wr) or Value shr Wr;
    end
{$if sizeof(Cardinal) < sizeof(TBitsBase)}
    else
      Dst^ := Dst^ shr Count shl Count or Value;
{$else}
    else if Count < bitsizeof(TBitsBase) then
      Dst^ := Dst^ shr Count shl Count or Value
    else
      Dst^ := Value;
{$endif sizeof(Cardinal) < sizeof(TBitsBase)}
{$endif sizeof(Cardinal) > sizeof(TBitsBase)}
  end;
end;

procedure TBits.NotRange(Pos, Count: SizeInt);
var
  B, BE: PBitsBase;
begin
  ValidateRange(Pos, Count);
  B := FBits + pos shr TBITS_SHIFT;
  Pos := Pos and TBITS_MASK;
  if Pos <> 0 then
    if Pos + Count <= bitsizeof(TBitsBase) then
    begin
      if Count > 0 then
        B^ := B^ xor (TBitsBase(1) shl Count - 1) shl Pos;
      exit;
    end else
    begin
      B^ := TBitsBase(B^ xor High(TBitsBase) shl Pos);
      Inc(B); Dec(Count, bitsizeof(TBitsBase) - Pos);
    end;

  BE := B + Count shr TBITS_SHIFT;
  while B < Be do
  begin
    B^ := not B^;
    Inc(B);
  end;

  Count := Count and TBITS_MASK;
  if Count <> 0 then
    B^ := B^ xor (TBitsBase(1) shl Count - 1);
end;

procedure TBits.AndRange(Pos: SizeInt; B: TBits; BPos, Count: SizeInt);
begin
  OpRange(Pos, B, BPos, Count, 8 {%1000});
end;

procedure TBits.OrRange(Pos: SizeInt; B: TBits; BPos, Count: SizeInt);
begin
  OpRange(Pos, B, BPos, Count, 14 {%1110});
end;

procedure TBits.XorRange(Pos: SizeInt; B: TBits; BPos, Count: SizeInt);
begin
  OpRange(Pos, B, BPos, Count, 6 {%0110});
end;

procedure TBits.AndNotRange(Pos: SizeInt; B: TBits; BPos, Count: SizeInt);
begin
  OpRange(Pos, B, BPos, Count, 4 {%0100});
end;

procedure TBits.OpRange(Pos: SizeInt; BBf: TBits; BPos, Count: SizeInt; OpTruth: TBitsOpTruth);
const
  Callbacks: array[TBitsOpTruth] of TBitsOpCallback =
  (
    nil, { %0000 = 0 }
    @_0001_Nor,
    @_0010_AInhibitsB,
    nil, { %0011 = not A }
    @_0100_BInhibitsA,
    nil, { %0101 = not B }
    @_0110_Xor,
    @_0111_Nand,
    @_1000_And,
    @_1001_Equiv,
    nil, // %1010 = B
    @_1011_AImpliesB,
    nil, // %1100 = A
    @_1101_BImpliesA,
    @_1110_Or,
    nil // %1111 = 1
  );
var
  Op: TBitsOpCallback;
  N: SizeInt;
  A, B: PBitsBase;
begin
  Op := Callbacks[OpTruth];
  if not Assigned(Op) then
  begin
    case OpTruth of
      0 {%0000} {0}: ClearRange(Pos, Count);
      3 {%0011} {not A}: NotRange(Pos, Count);
      5 {%0101} {not B}: begin MoveRange(BBf, BPos, Self, Pos, Count); NotRange(Pos, Count); end;
      10 {%1010} {B}: MoveRange(BBf, BPos, Self, Pos, Count);
      12 {%1100} {A}: ;
      15 {%1111} {1}: SetRange(Pos, Count);
      else {impossible};
    end;
    exit;
  end;

  BBf.ValidateRange(BPos, Count);
  FitRange(Pos, Count);

  A := FBits;
  B := BBf.FBits;

  if (A <> B) or (BPos >= Pos) or (BPos + Count <= Pos) then
  begin
    N := Pos and TBITS_MASK;
    if N > 0 then
    begin
      N := bitsizeof(TBitsBase) - N;
      if Count < N then
        if Count = 0 then exit else N := Count;
      Write1(A, Pos, N, Op(A[Pos shr TBITS_SHIFT] shr (Pos and TBITS_MASK), ReadSmall(B, BPos, N)) and (TBitsBase(1) shl N - 1));
      Inc(Pos, N); Inc(BPos, N); Dec(Count, N);
    end;

    N := Count shr TBITS_SHIFT;
    if N > 0 then
    begin
      if BPos and TBITS_MASK = 0 then
        OpAlignedAB(A + Pos shr TBITS_SHIFT, B + BPos shr TBITS_SHIFT, N, Op)
      else
        OpAlignedAUnalignedB(A + Pos shr TBITS_SHIFT, B + BPos shr TBITS_SHIFT, N, BPos and TBITS_MASK, Op);
      N := N shl TBITS_SHIFT;
      Inc(Pos, N); Inc(BPos, N); Dec(Count, N);
    end;

    if Count > 0 then
    begin
      Inc(A, Pos shr TBITS_SHIFT);
      A^ := A^ shr Count shl Count or Op(A^, ReadSmall(B, BPos, Count)) and (TBitsBase(1) shl Count - 1);
    end;
  end else
  begin
    Inc(Pos, Count); Inc(BPos, Count);
    N := Pos and TBITS_MASK;
    if N <> 0 then
    begin
      if Count < N then
        if Count = 0 then exit else N := Count;
      Dec(Pos, N); Dec(BPos, N); Dec(Count, N);
      Write1(A, Pos, N, Op(A[Pos shr TBITS_SHIFT] shr (Pos and TBITS_MASK), ReadSmall(B, BPos, N)) and (TBitsBase(1) shl N - 1));
    end;

    N := Count shr TBITS_SHIFT;
    if N > 0 then
    begin
      if BPos and TBITS_MASK = 0 then
        OpAlignedABRightToLeft(A + Pos shr TBITS_SHIFT, B + BPos shr TBITS_SHIFT, N, Op)
      else
        OpAlignedAUnalignedBRightToLeft(A + Pos shr TBITS_SHIFT, B + BPos shr TBITS_SHIFT, N, BPos and TBITS_MASK, Op);
      N := N shl TBITS_SHIFT;
      Dec(Pos, N); Dec(BPos, N); Dec(Count, N);
    end;

    if Count > 0 then
    begin
      Inc(A, SizeUint(Pos - Count) shr TBITS_SHIFT);
      N := bitsizeof(TBitsBase) - Count;
      A^ := TBitsBase(A^ shl Count) shr Count or Op(A^ shr N, ReadSmall(B, BPos - Count, Count)) and (TBitsBase(1) shl Count - 1) shl N;
    end;
  end;
end;

class procedure TBits.OpAlignedAB(A, B: PBitsBase; Count: SizeInt; Op: TBitsOpCallback);
var
  AE: PBitsBase;
begin
  { Count > 0. }
  AE := A + Count;
  repeat
    A^ := Op(A^, B^);
    Inc(A); Inc(B);
  until A = AE;
end;

class procedure TBits.OpAlignedAUnalignedB(A, B: PBitsBase; Count, B1Bits: SizeInt; Op: TBitsOpCallback);
var
  B0Bits: SizeUint;
  X, X1: TBitsBase;
  BE: PBitsBase;
begin
  { Count > 0, 0 < B1Bits < bitsizeof(TBitsBase). }
  X := B[0];
  B0Bits := bitsizeof(TBitsBase) - B1Bits;

  BE := B + Count shr 1 shl 1;
  while B < BE do
  begin
    X1 := B[1];
    A^ := TBitsBase(Op(A^, TBitsBase(X shr B1Bits or X1 shl B0Bits)));
    X := B[2];
    A[1] := TBitsBase(Op(A[1], TBitsBase(X1 shr B1Bits or X shl B0Bits)));
    Inc(A, 2); Inc(B, 2);
  end;

  if Count and 1 <> 0 then
    A^ := TBitsBase(Op(A^, TBitsBase(X shr B1Bits or B[1] shl B0Bits)));
end;

class procedure TBits.OpAlignedABRightToLeft(AR, BR: PBitsBase; Count: SizeInt; Op: TBitsOpCallback); static;
var
  ARE: PBitsBase;
begin
  { Count > 0. }
  ARE := AR - Count;
  repeat
    Dec(AR); Dec(BR);
    AR^ := Op(AR^, BR^);
  until AR = ARE;
end;

class procedure TBits.OpAlignedAUnalignedBRightToLeft(AR, BR: PBitsBase; Count, B0Bits: SizeInt; Op: TBitsOpCallback); static;
var
  B1Bits: SizeUint;
  X, X1: TBitsBase;
  BRE: PBitsBase;
begin
  { Count > 0, 0 < B0Bits < bitsizeof(TBitsBase). }
  X := BR[0];
  B1Bits := bitsizeof(TBitsBase) - B0Bits;

  BRE := BR - Count div 2 * 2;
  while BR > BRE do
  begin
    Dec(AR, 2); Dec(BR, 2);
    X1 := BR[1];
    AR[1] := TBitsBase(Op(AR[1], TBitsBase(X shl B1Bits or X1 shr B0Bits)));
    X := BR[0];
    AR[0] := TBitsBase(Op(AR[0], TBitsBase(X1 shl B1Bits or X shr B0Bits)));
  end;

  if Count and 1 <> 0 then
    AR[-1] := TBitsBase(Op(AR[-1], TBitsBase(X shl B1Bits or BR[-1] shr B0Bits)));
end;

class function TBits._0001_Nor(A, B: TBitsBase): TBitsBase;
begin
  result := not (A or B);
end;

class function TBits._0010_AInhibitsB(A, B: TBitsBase): TBitsBase;
begin
  result := not A and B;
end;

class function TBits._0100_BInhibitsA(A, B: TBitsBase): TBitsBase;
begin
  result := A and not B;
end;

class function TBits._0110_Xor(A, B: TBitsBase): TBitsBase;
begin
  result := A xor B;
end;

class function TBits._0111_Nand(A, B: TBitsBase): TBitsBase;
begin
  result := not (A and B);
end;

class function TBits._1000_And(A, B: TBitsBase): TBitsBase;
begin
  result := A and B;
end;

class function TBits._1001_Equiv(A, B: TBitsBase): TBitsBase;
begin
  result := not (A xor B);
end;

class function TBits._1011_AImpliesB(A, B: TBitsBase): TBitsBase;
begin
  result := not A or B;
end;

class function TBits._1101_BImpliesA(A, B: TBitsBase): TBitsBase;
begin
  result := A or not B;
end;

class function TBits._1110_Or(A, B: TBitsBase): TBitsBase;
begin
  result := A or B;
end;

procedure TBits.ClearRange(Pos, Count: SizeInt);
var
  B: PBitsBase;
begin
  FitRange(Pos, Count);
  B := FBits + Pos shr TBITS_SHIFT;
  Pos := Pos and TBITS_MASK;
  if Pos <> 0 then
    if Pos + Count <= bitsizeof(TBitsBase) then
    begin
      if Count <> 0 then B^ := B^ and not ((TBitsBase(1) shl Count - 1) shl Pos);
      exit;
    end else
    begin
      B^ := B^ and (TBitsBase(1) shl Pos - 1);
      Inc(B);
      Dec(Count, bitsizeof(TBitsBase) - Pos);
    end;
  FillChar(B^, Count shr TBITS_SHIFT * sizeof(TBitsBase), 0);
  Inc(B, Count shr TBITS_SHIFT);
  Count := Count and TBITS_MASK;
  if Count <> 0 then
    B^ := B^ shr Count shl Count;
end;

procedure TBits.SetRange(Pos, Count: SizeInt);
var
  B: PBitsBase;
begin
  FitRange(Pos, Count);
  B := FBits + Pos shr TBITS_SHIFT;
  Pos := Pos and TBITS_MASK;
  if Pos <> 0 then
    if Pos + Count <= bitsizeof(TBitsBase) then
    begin
      if Count <> 0 then B^ := B^ or (TBitsBase(1) shl Count - 1) shl Pos;
      exit;
    end else
    begin
      B^ := B^ or TBitsBase(High(TBitsBase) shl Pos);
      Inc(B);
      Dec(Count, bitsizeof(TBitsBase) - Pos);
    end;
  FillChar(B^, Count shr TBITS_SHIFT * sizeof(TBitsBase), High(Byte));
  Inc(B, Count shr TBITS_SHIFT);
  Count := Count and TBITS_MASK;
  if Count <> 0 then
    B^ := B^ or (TBitsBase(1) shl Count - 1);
end;

function TBits.PopCount(Pos, Count: SizeInt; Lim: SizeInt = High(SizeInt)): SizeInt;
var
  N: SizeInt;
  B, BE: PBitsBase;
begin
  ValidateRange(Pos, Count);
  Result := 0;
  if Lim <= 0 then exit;

  B := FBits + Pos shr TBITS_SHIFT;
  if Pos and TBITS_MASK <> 0 then
  begin
    N := bitsizeof(TBitsBase) - Pos and TBITS_MASK;
    if Count <= N then
      if Count = 0 then exit else N := Count;
    Result := PopCnt(TBitsBase(b^ shr (Pos and TBITS_MASK) and (TBitsBase(1) shl N - 1)));
    Inc(B); Dec(Count, N);
  end;

  { Unrolling by 4 is faster by itself, and also dilutes checks against 'Lim'. }
  N := Count shr TBITS_SHIFT;
  BE := B + N shr 2 shl 2;
  while B < BE do
  begin
    Inc(Result, SizeInt(PopCnt(B^) + PopCnt(B[1]) + PopCnt(B[2]) + PopCnt(B[3])));
    if Result >= Lim then exit(Lim);
    Inc(B, 4);
  end;

  BE := B + N and (4 - 1);
  while B < BE do
  begin
    Inc(Result, SizeInt(PopCnt(b^)));
    Inc(B);
  end;

  Count := Count and TBITS_MASK;
  if Count > 0 then
    Inc(Result, SizeInt(PopCnt(TBitsBase(B^ and (TBitsBase(1) shl Count - 1)))));

  if Result > Lim then
    Result := Lim;
end;

function TBits.IntersectionPopCount(Pos: SizeInt; BBf: TBits; BPos, Count: SizeInt; Lim: SizeInt = High(SizeInt)): SizeInt;
var
  N: SizeInt;
  A, B: PBitsBase;
begin
  ValidateRange(Pos, Count);
  BBf.ValidateRange(BPos, Count);
  Result := 0;
  if Lim <= 0 then exit;

  A := FBits;
  B := BBf.FBits;

  if Pos and TBITS_MASK <> 0 then
  begin
    N := bitsizeof(TBitsBase) - Pos and TBITS_MASK;
    if Count <= N then
      if Count = 0 then exit else n := count;
    Result := PopCnt(TBitsBase(A[Pos shr TBITS_SHIFT] shr (Pos and TBITS_MASK) and ReadSmall(b, bPos, n)));
    Inc(Pos, N); Inc(BPos, N); Dec(Count, N);
  end;

  if Count >= bitsizeof(TBitsBase) then
  begin
    if BPos and TBITS_MASK = 0 then
      Inc(Result, IntersectionPopCountAlignedAB(A + Pos shr TBITS_SHIFT, B + BPos shr TBITS_SHIFT, Count shr TBITS_SHIFT, Lim - Result))
    else
      Inc(Result, IntersectionPopCountAlignedAUnalignedB(A + Pos shr TBITS_SHIFT, B + BPos shr TBITS_SHIFT, Count shr TBITS_SHIFT, BPos and TBITS_MASK, Lim - Result));
    N := Count shr TBITS_SHIFT shl TBITS_SHIFT;
    Inc(Pos, N); Inc(BPos, N); Dec(Count, N);
  end;

  if Count > 0 then
    Inc(Result, SizeInt(PopCnt(TBitsBase(A[Pos shr TBITS_SHIFT] and ReadSmall(B, BPos, Count)))));

  if Result > Lim then
    Result := Lim;
end;

function TBits.HasIntersection(Pos: SizeInt; B: TBits; BPos, Count: SizeInt): boolean;
begin
  result := IntersectionPopCount(Pos, B, BPos, Count, 1) > 0;
end;

class function TBits.IntersectionPopCountAlignedAB(A, B: PBitsBase; Count, Lim: SizeInt): SizeInt;
var
  AE: PBitsBase;
begin
  Result := 0;

  AE := A + Count shr 2 shl 2;
  while A < AE do
  begin
    Inc(Result, SizeInt(PopCnt(TBitsBase(A^ and B^)) + PopCnt(TBitsBase(A[1] and B[1])) + PopCnt(TBitsBase(A[2] and B[2])) + PopCnt(TBitsBase(A[3] and B[3]))));
    if Result >= Lim then exit;
    Inc(A, 4); Inc(B, 4);
  end;

  AE := A + Count and (4 - 1);
  while A < AE do
  begin
    Inc(Result, SizeInt(PopCnt(TBitsBase(A^ and B^))));
    Inc(A); Inc(B);
  end;
end;

class function TBits.IntersectionPopCountAlignedAUnalignedB(A, B: PBitsBase; Count, B1Bits, Lim: SizeInt): SizeInt;
var
  B0Bits: SizeInt;
  X, X1: TBitsBase;
  AE: PBitsBase;
begin
  result := 0;
  x := b[0];
  B0Bits := bitsizeof(TBitsBase) - B1Bits;

  AE := A + Count shr 1 shl 1;
  while A < AE do
  begin
    X1 := B[1];
    Inc(Result, SizeInt(PopCnt(TBitsBase(A^ and (X shr B1Bits or X1 shl B0Bits)))));
    X := B[2];
    Inc(Result, SizeInt(PopCnt(TBitsBase(A[1] and (X1 shr B1Bits or X shl B0Bits)))));
    if Result >= Lim then exit;
    Inc(A, 2); Inc(B, 2);
  end;

  if Count and 1 <> 0 then
    Inc(Result, SizeInt(PopCnt(TBitsBase(A^ and (X shr B1Bits or B[1] shl B0Bits)))));
end;

function TBits.DumpBinary(Pos, Count: SizeInt; Bin: PByte; BinAvail: SizeInt): SizeInt;
var
  N: SizeInt;
  Tail: TBitsBase;
  B: PBitsBase;
begin
  Result := SizeUint(Count + (bitsizeof(byte) - 1)) div bitsizeof(byte);
  if Result > BinAvail then
    ThrowInsufficientBuffer(Result, BinAvail);

  ValidateRange(Pos, Count);
  B := FBits + Pos shr TBITS_SHIFT;
  Pos := Pos and TBITS_MASK;
  N := Count shr TBITS_SHIFT;
  if N > 0 then
  begin
    if Pos = 0 then
{$ifdef endian_little}
      Move(B^, Bin^, N * sizeof(TBitsBase))
{$else}
      DumpBinaryAligned(B, N, Bin)
{$endif}
    else
      DumpBinaryUnaligned(B, Pos, N, Bin);
    Inc(B, N); Inc(Bin, N * sizeof(TBitsBase));
  end;

  Count := Count and TBITS_MASK;
  if Count > 0 then
  begin
    Tail := ReadSmall(B, Pos, Count);
    repeat
      Bin^ := byte(Tail);
      Tail := Tail shr bitsizeof(byte);
      Inc(Bin); Dec(Count, bitsizeof(byte));
    until count <= 0;
  end;
end;

{$ifndef endian_little}
class procedure TBits.DumpBinaryAligned(B: PBitsBase; Count: SizeInt; Bin: PByte);
var
  BE: PBitsBase;
begin
  BE := B + Count;
  repeat
    unaligned(PBitsBase(Bin)^) := LEtoN(B^);
    Inc(B); Inc(Bin, sizeof(TBitsBase));
  until B = BE;
end;
{$endif}

class procedure TBits.DumpBinaryUnaligned(B: PBitsBase; B1Bits, Count: SizeInt; Bin: PByte);
var
  B0Bits: SizeInt;
  X, X1: TBitsBase;
  BE: PBitsBase;
begin
  { Count > 0, 0 < B1Bits < bitsizeof(TBitsBase). }
  X := B[0];
  B0Bits := bitsizeof(TBitsBase) - B1Bits;

  BE := B + Count shr 1 shl 1;
  while B < BE do
  begin
    X1 := B[1];
    unaligned(PBitsBase(Bin)^) := NtoLE(TBitsBase(X shr B1Bits or X1 shl B0Bits));
    X := B[2];
    unaligned(PBitsBase(Bin)[1]) := NtoLE(TBitsBase(X1 shr B1Bits or X shl B0Bits));
    Inc(B, 2); Inc(Bin, 2 * sizeof(TBitsBase));
  end;

  if Count and 1 <> 0 then
    unaligned(PBitsBase(Bin)^) := NtoLE(TBitsBase(X shr B1Bits or B[1] shl B0Bits));
end;

class procedure TBits.ThrowInsufficientBuffer(Required, Avail: SizeInt);
begin
  BitsErrorFmt('Insufficient buffer: %d / %d.', [Required, Avail]);
end;

function TBits.LoadBinary(Pos, Count: SizeInt; Bin: PByte; BinAvail: SizeInt): SizeInt;
var
  N, BinPos: SizeInt;
  B: PBitsBase;
begin
  Result := SizeUint(Count + (bitsizeof(byte) - 1)) div bitsizeof(byte);
  if Result > BinAvail then
    ThrowInsufficientBuffer(Result, BinAvail);

  FitRange(Pos, Count);
  B := FBits;
  BinPos := 0;
  N := Pos and TBITS_MASK;
  if N > 0 then
  begin
    N := bitsizeof(TBitsBase) - N;
    if Count <= N then
      if Count = 0 then exit else N := Count;
    Write1(B, Pos, n, ReadSmallBinary(bin, 0, n));
    Inc(Pos, N); Inc(BinPos, N); Dec(Count, N);
  end;

  N := Count shr TBITS_SHIFT;
  if N > 0 then
  begin
    if SizeUint(BinPos) mod bitsizeof(byte) = 0 then
{$ifdef endian_little}
      Move(Bin[SizeUint(BinPos) div bitsizeof(byte)], B[Pos shr TBITS_SHIFT], N * sizeof(TBitsBase))
{$else}
      LoadBinaryAligned(B + Pos shr TBITS_SHIFT, N, Bin + SizeUint(BinPos) div bitsizeof(byte))
{$endif}
    else
      LoadBinaryUnaligned(B + Pos shr TBITS_SHIFT, N, Bin + SizeUint(BinPos) div bitsizeof(byte), SizeUint(BinPos) mod bitsizeof(byte));
    N := N shl TBITS_SHIFT;
    Inc(Pos, N); Inc(BinPos, N); Dec(Count, N);
  end;

  if Count > 0 then
    Write1(B, Pos, Count, ReadSmallBinary(Bin, BinPos, Count));
end;

{$ifndef endian_little}
class procedure TBits.LoadBinaryAligned(B: PBitsBase; Count: SizeInt; Bin: PByte);
var
  BE: PBitsBase;
begin
  BE := B + Count;
  repeat
    B^ := LEtoN(unaligned(PBitsBase(Bin)^));
    Inc(B); Inc(Bin, sizeof(TBitsBase));
  until B = BE;
end;
{$endif}

class procedure TBits.LoadBinaryUnaligned(B: PBitsBase; Count: SizeInt; Bin: PByte; Bin1Bits: SizeInt);
var
  Bin0Bits: SizeInt;
  X, X1: TBitsBase;
  BE2, BE: PBitsBase;
begin
  Bin0Bits := bitsizeof(TBitsBase) - Bin1Bits;
  BE := B + Count;
  { Loop by 2 reinterprets 'Bin' as a sequence of little-endian 'TBitsBase's that must be shifted by 0 < Bin1Bits < bitsizeof(byte),
    and assumes PBitsBase(Bin)[2] is accessible: its lower Bin1Bits bits become upper bits of B[1].

    Unlike MoveUnalignedSrcToAlignedDest that works with actual 'TBitsBases', here this assumption if safe only for Count - 1 elements:
    if Bin is 9 bytes and TBitsBase is 4-byte, Bin has two whole 'TBitsBase's (so Count = 2), but PBitsBase(Bin)[2] can't be read. }
  if Count >= 3 then
  begin
    X := LEtoN(unaligned(PBitsBase(Bin)^));
    BE2 := B + (Count - 1) shr 1 shl 1;
    repeat
      X1 := LEtoN(unaligned(PBitsBase(Bin)[1]));
      B^ := TBitsBase(X shr Bin1Bits or X1 shl Bin0Bits);
      X := LEtoN(unaligned(PBitsBase(Bin)[2]));
      B[1] := TBitsBase(X1 shr Bin1Bits or X shl Bin0Bits);
      Inc(B, 2); Inc(Bin, 2 * sizeof(TBitsBase));
    until B = BE2;
  end;

  { The rest is handled by reading only 1 required byte instead of over-reading PBitsBase(Bin)[2]. }
  repeat
    B^ := TBitsBase(LEtoN(unaligned(PBitsBase(Bin)^)) shr Bin1Bits or TBitsBase(Bin[sizeof(TBitsBase)]) shl Bin0Bits);
    Inc(B); Inc(Bin, sizeof(TBitsBase));
  until B = BE;
end;

class function TBits.ReadSmallBinary(Bin: PByte; Pos, Count: SizeInt): TBitsBase;
var
  N: SizeInt;
begin
  { 0 < Count < bitsizeof(TBitsBase). }
  Inc(Bin, SizeUint(Pos) div bitsizeof(byte));
  Pos := SizeUint(Pos) mod bitsizeof(byte);
  Result := Bin^ shr Pos;
  Inc(Bin);
  N := bitsizeof(byte) - Pos;
  while N < Count do
  begin
    Result := Result or TBitsBase(bin^) shl N;
    Inc(Bin); Inc(N, bitsizeof(byte));
  end;
  Result := Result and (TBitsBase(1) shl Count - 1);
end;

function TBits.DumpText(Pos, Count: SizeInt; const S0, S1: string; Buf: PChar; BufAvail: SizeInt): SizeInt;
type
  { These things track strings corresponding to longest sequences of 0s and 1s built so far,
    to Move() in bigger chunks than starting S0 and S1 if possible. }
  TStringSource = record
    P: PChar;
    OneLen, TotalLen: SizeInt;
    Xm: TBitsBase;
  end;
var
  EndPos, Ones, Next, WantedChars, CharsToMove: SizeInt;
  SS: array[0 .. 1] of TStringSource;
  Cs: ^TStringSource;
begin
  ValidateRange(Pos, Count);
  SS[0].OneLen := length(S0);
  SS[1].OneLen := length(S1);
  if (SS[0].OneLen = 1) and (SS[1].OneLen = 1) then
    exit(DumpTextCC(Pos, Count, S0[1], S1[1], Buf, BufAvail));

  Ones := PopCount(Pos, Count);
  Result := (Count - Ones) * SS[0].OneLen + Ones * SS[1].OneLen;
  if Result > BufAvail then
    ThrowInsufficientBuffer(Result, BufAvail);

  SS[0].P := PChar(Pointer(S0));
  SS[0].TotalLen := SS[0].OneLen;
  SS[0].Xm := 0;
  SS[1].P := PChar(Pointer(S1));
  SS[1].TotalLen := SS[1].OneLen;
  SS[1].Xm := High(TBitsBase);
  Cs := @SS[0];
  EndPos := Pos + Count;

  repeat
    Next := ScanFor1(Pos, Cs^.Xm);
    if SizeUint(Next) >= SizeUint(EndPos) then
    begin
      if Pos = EndPos then break;
      Next := EndPos;
    end;
    WantedChars := (Next - Pos) * Cs^.OneLen;
    Pos := Next;
    if WantedChars <= Cs^.TotalLen then
    begin
      { Will not become new longest chunk, and can be filled in one call. }
      Move(Cs^.P^, Buf^, WantedChars * sizeof(char));
      Inc(Buf, WantedChars);
    end else
    begin
      { Will become new longest chunk, filled with the data of the previous and then logarithmically. }
      Move(Cs^.P^, Buf^, Cs^.TotalLen * sizeof(char));
      Cs^.P := Buf;
      Inc(Buf, Cs^.TotalLen);
      Dec(WantedChars, Cs^.TotalLen);
      repeat
        CharsToMove := Cs^.TotalLen;
        if WantedChars < CharsToMove then
          CharsToMove := WantedChars;
        Move(Cs^.P^, Buf^, CharsToMove * sizeof(char));
        Inc(Buf, CharsToMove);
        Inc(Cs^.TotalLen, CharsToMove);
        Dec(WantedChars, CharsToMove);
      until WantedChars = 0;
    end;
    if Cs = @SS[0] then Cs := @SS[1] else Cs := @SS[0];
  until false;
end;

function TBits.DumpText(Pos, Count: SizeInt; const S0, S1: string): string;
var
  Ones, NR: SizeInt;
begin
  Ones := PopCount(Pos, Count);
  NR := (Count - Ones) * length(S0) + Ones * length(S1);
  Result := '';
  SetLength(Result, NR);
  DumpText(Pos, Count, S0, S1, PChar(pointer(Result)), NR);
end;

function TBits.DumpTextCC(Pos, Count: SizeInt; C0, C1: Char; Buf: PChar; BufAvail: SizeInt): SizeInt;
var
  EndPos, NextPos, WantedChars, IC: SizeInt;
  Xm: TBitsBase;
  C: array[0 .. 1] of char;
begin
  Result := Count;
  if Result > BufAvail then
    ThrowInsufficientBuffer(Result, BufAvail);
  C[0] := C0;
  C[1] := C1;
  IC := 0;
  Xm := 0;
  EndPos := Pos + Count;
  repeat
    NextPos := ScanFor1(Pos, Xm);
    if SizeUint(NextPos) >= SizeUint(EndPos) then
    begin
      if Pos = EndPos then break;
      NextPos := EndPos;
    end;
    WantedChars := NextPos - Pos;
    Pos := NextPos;
    FillChar(Buf^, WantedChars, C[IC]);
    Inc(Buf, WantedChars);
    IC := 1 - IC;
    Xm := not Xm;
  until false;
end;

function TBits.LoadText(Pos, Count: SizeInt; const S0, S1: string; Buf: PChar; BufAvail: SizeInt): SizeInt;
var
  NS0, NS1, NS: SizeInt;
  Cell: PBitsBase;
  Mask: TBitsBase;
  StartingBuf: PChar;
begin
  if (S0 = '') or (S1 = '') or (S0 = S1) then
    BitsErrorFmt('Bad strings for 0/1: %s/%s.', [S0, S1]);
  FitRange(Pos, Count);
  NS0 := length(S0);
  NS1 := length(S1);
  if (NS0 = 1) and (NS1 = 1) then
    exit(LoadTextCC(Pos, Count, S0[1], S1[1], Buf, BufAvail));

  StartingBuf := Buf;
  Cell := FBits + Pos shr TBITS_SHIFT;
  Mask := TBitsBase(1) shl (Pos and TBITS_MASK);
  while Count > 0 do
  begin
    if (BufAvail >= NS0) and (CompareChar(Buf^, PChar(Pointer(S0))^, NS0) = 0) then
    begin
      Cell^ := Cell^ and not Mask;
      NS := NS0;
    end else if (BufAvail >= NS1) and (CompareChar(Buf^, PChar(Pointer(S1))^, NS1) = 0) then
    begin
      Cell^ := Cell^ or Mask;
      NS := NS1;
    end else
      ThrowLoadTextError(BufAvail, S0, S1);
    Inc(Buf, NS);
    Dec(BufAvail, NS);
    Mask := TBitsBase(Mask shl 1);
    if Mask = 0 then
    begin
      Inc(Cell);
      Mask := 1;
    end;
    Dec(Count);
  end;
  Result := Buf - StartingBuf;
end;

procedure TBits.LoadText(Pos, Count: SizeInt; const S0, S1, Src: string);
var
  NSrc, Rd: SizeInt;
begin
  NSrc := length(Src);
  Rd := LoadText(Pos, Count, S0, S1, PChar(Pointer(Src)), NSrc);
  if Rd <> NSrc then
    BitsErrorFmt('Too much data at position %d.', [1 + Rd]);
end;

function TBits.LoadTextCC(Pos, Count: SizeInt; C0, C1: Char; Buf: PChar; BufAvail: SizeInt): SizeInt;
var
  Cell: PBitsBase;
  Mask: TBitsBase;
begin
  Result := Count;
  if Result > BufAvail then
    ThrowInsufficientBuffer(Result, BufAvail);
  Cell := FBits + Pos shr TBITS_SHIFT;
  Mask := TBitsBase(1) shl (Pos and TBITS_MASK);
  while Count > 0 do
  begin
    if Buf^ = C0 then
      Cell^ := Cell^ and not Mask
    else if Buf^ = C1 then
      Cell^ := Cell^ or Mask
    else
      ThrowLoadTextError(BufAvail, C0, C1);
    Inc(Buf);
    Mask := TBitsBase(Mask shl 1);
    if Mask = 0 then
    begin
      Inc(Cell);
      Mask := 1;
    end;
    Dec(Count);
  end;
end;

class procedure TBits.ThrowLoadTextError(BufAvail: SizeInt; const S0, S1: string);
begin
  if BufAvail <= 0 then
    BitsErrorFmt('Not enough data in the buffer.', [])
  else
    BitsErrorFmt('Data in the buffer does not match "%s" nor "%s".', [S0, S1]);
end;
