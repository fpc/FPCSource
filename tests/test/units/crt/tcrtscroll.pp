{ %TARGET=win32,win64 }
{ crt output past the bottom line moves the top line into the console scrollback, issue #28378 }
program tcrtscroll;

{$mode objfpc}{$h+}

uses
  Windows, Crt;


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln(StdErr);
    writeln(StdErr,'check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns aValue as a decimal string
function IntToStr(aValue: Integer): string;

begin
  Str(aValue,Result);
end;


// Writes aText followed by 10 blanks at the start of absolute buffer row aY (1-based)
procedure WriteRow(aY: Integer; const aText: UnicodeString);

var
  lPos: TCoord;
  lWritten: DWord;
  lText: UnicodeString;

begin
  lText:=aText+'          ';
  lPos.X:=0;
  lPos.Y:=aY-1;
  WriteConsoleOutputCharacterW(GetStdHandle(STD_OUTPUT_HANDLE),PWideChar(lText),Length(lText),lPos,lWritten);
end;


// Returns the first aCount cells of absolute buffer row aY (1-based)
function ReadRow(aY, aCount: Integer): UnicodeString;

var
  lPos: TCoord;
  lRead: DWord;

begin
  SetLength(Result,aCount);
  lPos.X:=0;
  lPos.Y:=aY-1;
  if not ReadConsoleOutputCharacterW(GetStdHandle(STD_OUTPUT_HANDLE),Pointer(Result),aCount,lPos,lRead) then
    lRead:=0;
  SetLength(Result,lRead);
end;


// Returns True when absolute buffer row aY starts with aText
function RowStartsWith(aY: Integer; const aText: UnicodeString): Boolean;

begin
  Result:=ReadRow(aY,Length(aText)+1)=aText+' ';
end;


var
  lInfo: TConsoleScreenBufferInfo;
  lHeight, lCount, lStart, lExpected, lRow, lChecked, lTop, lEnd, i: Integer;

begin
  if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),lInfo) then
    begin
    writeln('skipped: no console');
    halt(0);
    end;
  lHeight:=WindMaxY-WindMinY+1;
  lCount:=2*lHeight+5;
  writeln;
  lStart:=WindMinY+WhereY-1;
  for i:=1 to lCount do
    writeln('line ',i);

  lRow:=WindMinY+WhereY-2;
  lExpected:=lStart+lCount-1;
  if lExpected>lInfo.dwSize.Y-1 then
    lExpected:=lInfo.dwSize.Y-1;
  Check(1,lRow=lExpected,'last line in buffer row '+IntToStr(lExpected)+', found in row '+IntToStr(lRow));
  lChecked:=0;
  while (lChecked<lCount) and (lRow-lChecked>=1) do
    begin
    Check(2,RowStartsWith(lRow-lChecked,'line '+IntToStr(lCount-lChecked)),
      'buffer row '+IntToStr(lRow-lChecked)+' holds line '+IntToStr(lCount-lChecked));
    Inc(lChecked);
    end;
  if lInfo.dwSize.Y<=lHeight then
    writeln('note: buffer has no rows outside the window, check the terminal scrollback by hand');
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),lInfo);
  Check(3,(WindMinY+WhereY-1>=lInfo.srWindow.Top+1) and (WindMinY+WhereY-1<=lInfo.srWindow.Bottom+1),
    'cursor inside the visible console window');
  Check(4,(WhereY>=1) and (WhereY<=lHeight),'WhereY inside the CRT window');

  if lHeight<6 then
    begin
    writeln('skipped window check: console window lower than 6 lines');
    halt(0);
    end;
  lEnd:=WindMinY+WhereY-1;
  lTop:=WindMinY;
  WriteRow(lTop,'outside');
  WriteRow(lTop+4,'below');
  Window32(1,lTop+1,WindMaxX,lTop+3);
  ClrScr;
  for i:=1 to 6 do
    writeln('w',i);
  Check(5,RowStartsWith(lTop,'outside'),'row above a user window is not scrolled, found: '+ReadRow(lTop,10));
  Check(6,RowStartsWith(lTop+1,'w5'),'user window row 1 after scrolling, found: '+ReadRow(lTop+1,10));
  Check(7,RowStartsWith(lTop+2,'w6'),'user window row 2 after scrolling, found: '+ReadRow(lTop+2,10));
  Check(8,RowStartsWith(lTop+4,'below'),'row below a user window is not scrolled, found: '+ReadRow(lTop+4,10));
  Window32(1,lEnd,WindMaxX,lEnd);
  write('ok');
end.
