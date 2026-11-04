{ %target=go32v2,msdos }
{ Issue #40147: SetMouseGraphCursor sets the graphics mouse cursor shape and hot spot (VGA mode 12h) }
program tw40147;

{$mode objfpc}

uses
  dos, msmouse;

const
  cBack = 4;
  cX = 100;
  cY = 100;
  cHotX = 3;
  cHotY = 5;

var
  gErrors: Integer = 0;
  gMask: TMouseGraphCursorMask;
  gLog: array[0..39] of string[80];
  gLogCount: Integer = 0;

// Append aLine to the messages printed after returning to text mode.
procedure Log(const aLine: string);

begin
  if gLogCount <= High(gLog) then
  begin
    gLog[gLogCount] := aLine;
    Inc(gLogCount);
  end;
end;


// Return aValue as decimal text.
function IntText(aValue: Integer): string;

begin
  Str(aValue, Result);
end;


// Set BIOS video mode aMode.
procedure SetVideoMode(aMode: Byte);

var
  lRegs: Registers;

begin
  FillChar(lRegs, SizeOf(lRegs), 0);
  lRegs.ax := aMode;
  Intr($10, lRegs);
end;


// Write pixel aX, aY with colour aColor through the BIOS.
procedure PutPixel(aX, aY: Integer; aColor: Byte);

var
  lRegs: Registers;

begin
  FillChar(lRegs, SizeOf(lRegs), 0);
  lRegs.ax := $0C00 or aColor;
  lRegs.bx := 0;
  lRegs.cx := aX;
  lRegs.dx := aY;
  Intr($10, lRegs);
end;


// Return the colour of pixel aX, aY read through the BIOS.
function GetPixel(aX, aY: Integer): Byte;

var
  lRegs: Registers;

begin
  FillChar(lRegs, SizeOf(lRegs), 0);
  lRegs.ax := $0D00;
  lRegs.bx := 0;
  lRegs.cx := aX;
  lRegs.dx := aY;
  Intr($10, lRegs);
  Result := lRegs.ax and $FF;
end;


// Report aMsg as a failure when aOK is false.
procedure Check(aOK: Boolean; const aMsg: string);

begin
  if aOK then
    Log('ok:   ' + aMsg)
  else
  begin
    Log('FAIL: ' + aMsg);
    Inc(gErrors);
  end;
end;


// Return the expected colour at row aRow, column aCol of the cursor drawn on background cBack.
function Expected(aRow, aCol: Integer): Byte;

var
  lBit: Word;

begin
  lBit := $8000 shr aCol;
  Result := cBack;
  if gMask.ScreenMask[aRow] and lBit = 0 then
    Result := 0;
  if gMask.CursorMask[aRow] and lBit <> 0 then
    Result := Result xor $0F;
end;


// Compare the 16x16 cursor area at its upper left corner aLeft, aTop with the masks; return the mismatch count.
function CompareCursor(aLeft, aTop: Integer): Integer;

var
  lRow, lCol: Integer;
  lLine: string;
  lGot: Byte;

begin
  Result := 0;
  for lRow := 0 to 15 do
  begin
    lLine := '';
    for lCol := 0 to 15 do
    begin
      lGot := GetPixel(aLeft + lCol, aTop + lRow);
      lLine := lLine + HexStr(lGot, 1);
      if lGot <> Expected(lRow, lCol) then
        Inc(Result);
    end;
    Log('      ' + lLine);
  end;
end;


// Return the number of pixels in the 40x40 area around the cursor position that differ from cBack.
function CountNonBack: Integer;

var
  lX, lY: Integer;

begin
  Result := 0;
  for lY := cY - 20 to cY + 19 do
    for lX := cX - 20 to cX + 19 do
      if GetPixel(lX, lY) <> cBack then
        Inc(Result);
end;


var
  lRow, lX, lY, lBad: Integer;

begin
  if not MouseFound then
  begin
    Writeln('No mouse driver installed, test skipped.');
    Halt(0);
  end;
  for lRow := 0 to 15 do
  begin
    gMask.ScreenMask[lRow] := Word($FFFF shr lRow);
    gMask.CursorMask[lRow] := (Word($AAAA shr (lRow and 1)) and not gMask.ScreenMask[lRow]) or 1;
  end;
  gMask.ScreenMask[0] := $0FF0;
  gMask.CursorMask[0] := $F00F;

  SetVideoMode($12);
  Check(InitMouse, 'mouse driver present after switching to mode 12h');
  for lY := cY - 20 to cY + 19 do
    for lX := cX - 20 to cX + 19 do
      PutPixel(lX, lY, cBack);
  Check(CountNonBack = 0, 'background is filled');

  SetMouseGraphCursor(cHotX, cHotY, gMask);
  SetMousePos(cX, cY);
  ShowMouse;
  Log('      cursor area at hot spot offset (' + IntText(cHotX) + ',' + IntText(cHotY) + '):');
  lBad := CompareCursor(cX - cHotX, cY - cHotY);
  Log('      mismatches: ' + IntText(lBad));
  Check(lBad = 0, 'cursor pixels match the masks at the hot spot');
  HideMouse;
  Check(CountNonBack = 0, 'hiding the cursor restores the background');

  SetVideoMode($03);
  for lRow := 0 to gLogCount - 1 do
    Writeln(gLog[lRow]);
  if gErrors = 0 then
    Writeln('All checks passed.')
  else
    Writeln(gErrors, ' check(s) failed.');
  if gErrors > 0 then
    Halt(1);
end.
