{ %TARGET=win32,win64 }
{ crt writes a DBCS character as one character in 2 cells, issue #20598 }
program tcrtdbcs;

{$mode objfpc}{$h+}

uses
  Windows, Crt;

const
  { U+4E2D U+6587 in code page 936 }
  CJK = #$D6#$D0#$CE#$C4;


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    GotoXY(1,1);
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns aCount UTF-16 cells of row aY starting at column aX (1-based)
function ReadCells(aX, aY, aCount: Integer): UnicodeString;

var
  lPos: TCoord;
  lRead: DWord;

begin
  SetLength(Result,aCount);
  lPos.X:=aX-1;
  lPos.Y:=aY-1;
  if not ReadConsoleOutputCharacterW(GetStdHandle(STD_OUTPUT_HANDLE),Pointer(Result),aCount,lPos,lRead) then
    lRead:=0;
  SetLength(Result,lRead);
end;


var
  lInfo: TConsoleScreenBufferInfo;
  lCells: UnicodeString;
  lX, lY, i: Integer;

begin
  if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),lInfo) then
    begin
    writeln('skipped: no console');
    halt(0);
    end;
  SetUseACP(False);
  if not SetConsoleOutputCP(936) then
    begin
    writeln('skipped: code page 936 not available');
    halt(0);
    end;
  ClrScr;
  GotoXY(1,2);
  write('a',CJK,'b');
  lX:=WhereX;
  lCells:=ReadCells(1,2,6);
  GotoXY(1,4);
  Check(1,lX=7,'WhereX after a, 2 DBCS characters and b');
  Check(2,(Copy(lCells,1,2)='a'#$4E2D) and (Pos(WideChar(#$6587),lCells)>0) and (Pos('b',lCells)>0),'cells of row 2');
  GotoXY(1,6);
  write('x');
  for i:=1 to 150 do
    write(CJK);
  lY:=WhereY;
  lCells:=ReadCells(2,6,(lY-5)*(WindMaxX-WindMinX+1)-1);
  GotoXY(1,lY+1);
  Check(3,Pos(WideChar(#$4E2D),lCells)>0,'DBCS characters in the long write');
  for i:=1 to Length(lCells) do
    Check(4,(lCells[i]=#$4E2D) or (lCells[i]=#$6587) or (lCells[i]=' '),
      'only DBCS characters and blanks in the long write, found U+'+HexStr(Ord(lCells[i]),4));
  GotoXY(1,lY+2);
  writeln('ok');
end.
