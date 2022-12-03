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
    FBits[value shr TBITS_SHIFT] := FBits[value shr TBITS_SHIFT] and TBitsBase(TBitsBaseUnsigned(1) shl (value and TBITS_MASK) - 1);

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
               (TBitsBaseUnsigned(cell)));
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
               (TBitsBaseUnsigned(cell))));
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
   mask := TBitsBase(TBitsBaseUnsigned(1) shl (bit and TBITS_MASK));
   if value then
      cell^ := cell^ or mask
   else
      cell^ := cell^ and not mask;
end;

function TBits.OpenBit : SizeInt;
begin
   result := ScanFor1(0, -1);
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
  FBits[bit shr TBITS_SHIFT] := FBits[bit shr TBITS_SHIFT] or TBitsBase(TBitsBaseUnsigned(1) shl (bit and TBITS_MASK))
end;

procedure TBits.clear(bit : SizeInt);
begin
  grow(bit+1);
  FBits[bit shr TBITS_SHIFT] := FBits[bit shr TBITS_SHIFT] and not TBitsBase(TBitsBaseUnsigned(1) shl (bit and TBITS_MASK));
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
      FBits[FBSize shr TBITS_SHIFT] := FBits[FBSize shr TBITS_SHIFT] and TBitsBase(TBitsBaseUnsigned(1) shl (FBSize and TBITS_MASK) - 1);
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
   { -TBitsBase(not state) is 0 for true or -1 for false, making following ScanFor1s search for 'state'. }
   result := ScanFor1(0, -TBitsBase(not state));
   findXorMask := -TBitsBase(not state);
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


