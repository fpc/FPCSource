{ lz_compress must not write outside lentab, prevtab or the block buffer }
program tw41941;

{$mode objfpc}

uses
  paslznonslide;

const
  WindowSize = 1024;
  GuardByte = $A5;

var
  gData: array of Byte;
  gDataPos: Integer;

// Copy the next aCount bytes of gData to aBuf.
function GetChars(aLzi: plz_info; aCount: longint; aBuf: pu_char): longint; cdecl;

begin
  Result := Length(gData) - gDataPos;
  if Result > aCount then
    Result := aCount;
  if Result > 0 then
    Move(gData[gDataPos], aBuf^, Result);
  Inc(gDataPos, Result);
end;


// Accept every match.
function OutputMatch(aLzi: plz_info; aPos: longint; aLen: longint): longint; cdecl;

begin
  Result := 0;
end;


// Ignore literals.
procedure OutputLiteral(aLzi: plz_info; aCh: u_char); cdecl;

begin
end;


// Allocate aSize bytes followed by aSize guard bytes.
function AllocGuarded(aSize: PtrInt): Pointer;

begin
  Result := GetMem(2 * aSize);
  FillChar(Result^, aSize, 0);
  FillChar(PByte(Result)[aSize], aSize, GuardByte);
end;


// Exit with aCode if a byte after the first aSize bytes of aBuf changed.
procedure CheckGuard(aBuf: Pointer; aSize: PtrInt; const aName: string; aCode: Integer);

var
  i: PtrInt;

begin
  for i := aSize to 2 * aSize - 1 do
    if PByte(aBuf)[i] <> GuardByte then
    begin
      Writeln('Error: ', aName, ' written past its end at offset ', i - aSize);
      Halt(aCode);
    end;
end;


var
  lLzi: lz_info;
  lLenSize, lPrevSize: PtrInt;
  i: Integer;

begin
  lz_init(@lLzi, WindowSize, WindowSize, 253, 3, 0, @GetChars, @OutputMatch, @OutputLiteral, nil);
  SetLength(gData, lLzi.block_buf_size);
  RandSeed := 41941;
  for i := 0 to High(gData) do
    gData[i] := Ord('a') + Random(4);
  gDataPos := 0;
  lLenSize := SizeOf(longint) * lLzi.block_buf_size;
  lPrevSize := SizeOf(pu_char) * lLzi.block_buf_size;
  FreeMem(lLzi.lentab);
  FreeMem(lLzi.prevtab);
  lLzi.lentab := AllocGuarded(lLenSize);
  lLzi.prevtab := AllocGuarded(lPrevSize);
  lz_compress(@lLzi, 2 * lLzi.block_buf_size);
  if gDataPos <> Length(gData) then
  begin
    Writeln('Error: only ', gDataPos, ' of ', Length(gData), ' bytes read');
    Halt(1);
  end;
  CheckGuard(lLzi.lentab, lLenSize, 'lentab', 2);
  CheckGuard(lLzi.prevtab, lPrevSize, 'prevtab', 3);
  lz_release(@lLzi);
  Writeln('ok');
end.
