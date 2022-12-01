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
   TBITS_SHIFT =
{$if sizeof(TBitsBase) = sizeof(word)}
            4
{$elseif sizeof(TBitsBase) = sizeof(dword)}
            5
{$elseif sizeof(TBitsBase) = sizeof(qword)}
            6
{$else}
{$error unknown TBitsBase}
{$endif}
            ;
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
{$if sizeof(TBitsBase) = sizeof(word)}
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
{$if sizeof(TBitsBase) = sizeof(word)}
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

