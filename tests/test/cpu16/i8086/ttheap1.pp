{ %cpu=i8086 }

{ Test for TP7 compatibility of the tiny heap free list in the i8086 far data
  memory models.

  This test is TP7 compatible. }

{$IFDEF FPC}
  {$DEFINE SKIP_TEST}
  {$IFDEF FPC_MM_COMPACT}
    {$UNDEF SKIP_TEST}
  {$ENDIF not FPC_MM_COMPACT}
  {$IFDEF FPC_MM_LARGE}
    {$UNDEF SKIP_TEST}
  {$ENDIF not FPC_MM_LARGE}
  {$IFDEF FPC_MM_HUGE}
    {$UNDEF SKIP_TEST}
  {$ENDIF not FPC_MM_HUGE}
{$ENDIF FPC}

{$IFDEF SKIP_TEST}
program ttheap1;
begin
  Writeln('Test compiled for the wrong memory model. Goodbye!');
end
{$ELSE SKIP_TEST}

program ttheap1;

type
  PHeapBlock = ^THeapBlock;
  THeapBlock = record
    Next: PHeapBlock;
    Size: Pointer;
  end;

function HexStr(L: LongInt; digits: Integer): string;
const
  D: array [0..15] of char = '0123456789ABCDEF';
var
  res: string;
  I: Integer;
begin
  res := '';
  for I := 1 to digits do
  begin
    res := D[L and 15] + res;
    L := L shr 4;
  end;
  HexStr := res;
end;

function PtrStr(P: Pointer): string;
begin
  PtrStr := '$' + HexStr(Seg(P^), 4) + ':' + HexStr(Ofs(P^), 4);
end;

procedure CheckNormalization(P: Pointer);
begin
  if Ofs(P^) > 15 then
  begin
    Writeln('Pointer not normalized! ', PtrStr(P));
    Halt(1);
  end;
end;

procedure CheckAlignment(P: Pointer);
begin
  if (Ofs(P^) mod 8) <> 0 then
  begin
    Writeln('Pointer not aligned! ', PtrStr(P));
    Halt(1);
  end;
end;

procedure CheckSequence(P1, P2: Pointer);
begin
  if ((LongInt(Seg(P1^)) shl 4) + Ofs(P1^)) >=
     ((LongInt(Seg(P2^)) shl 4) + Ofs(P2^)) then
  begin
    Writeln('Pointer sequence broken: ', PtrStr(P1) , '>=', PtrStr(P2), ' (should be <)');
    Halt(1);
  end;
end;

procedure CheckOverlap(P1, P1Size, P2: Pointer);
begin
  if (((LongInt(Seg(P1^)) shl 4) + Ofs(P1^)) +
      ((LongInt(Seg(P1Size^)) shl 4) + Ofs(P1Size^))) >=
     ((LongInt(Seg(P2^)) shl 4) + Ofs(P2^)) then
  begin
    Writeln('Free list overlap: ', PtrStr(P1), '+', PtrStr(P1Size) , '>=', PtrStr(P2), ' (should be <)');
    Halt(1);
  end;
end;

procedure WalkFreeList;
var
  Block: PHeapBlock;
begin
  Block := FreeList;
  repeat
    CheckNormalization(Block);
    CheckAlignment(Block);
    Write(PtrStr(Block), ' : Next=', PtrStr(Block^.Next), ' Size=', PtrStr(Block^.Size));
    if Block = HeapPtr then
    begin
      Writeln(', Reached HeapPtr!');
      break;
    end;
    Writeln;
    CheckNormalization(Block^.Size);
    CheckAlignment(Block^.Size);
    CheckSequence(Block, Block^.Next);
    CheckOverlap(Block, Block^.Size, Block^.Next);
    Block := Block^.Next;
  until false;
end;

procedure DumpHeap;
begin
  Writeln('HeapOrg = ', PtrStr(HeapOrg));
  Writeln('HeapEnd = ', PtrStr(HeapEnd));
  Writeln('HeapPtr = ', PtrStr(HeapPtr));
  Writeln('FreeList = ', PtrStr(FreeList));
  WalkFreeList;
end;

procedure RandomMem;
var
  I: Integer;
  Q: array [0..1000] of record
    p: Pointer;
    Size: Word;
  end;
begin
  FillChar(Q, SizeOf(Q), 0);
  for I := 1 to 10000 do
  begin
    with Q[Random(1001)] do
    begin
      if p = nil then
      begin
        Size := Random(100);
        GetMem(p, Size);
      end
      else
      begin
        FreeMem(p, Size);
        p := nil;
        Size := 0;
      end;
    end;
  end;
end;

begin
  Randomize;
  DumpHeap;
  RandomMem;
  DumpHeap;
  Writeln('Ok!');
end
{$ENDIF SKIP_TEST}
.
