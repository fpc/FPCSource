{ %TARGET=win32,win64 }
{ crt shows Greek, CJK and UTF-8 text by writing UTF-16 to the console, issue #40743.
  Each line shows the text, then OK or FAIL after reading the console cells back. Exit code: number of failures.
  An optional file name parameter receives the results as text. }
program tcrtunicode;

{$mode objfpc}{$h+}
{$codepage utf8}

uses
  Windows, Crt;

const
  ResultCol = 62;
  TextCol = 34;
  Greek: UnicodeString = 'Καλημέρα κόσμε';
  CJK: UnicodeString = '中文 日本語 한국어';
  Box: UnicodeString = '┌─┐│└┘';

var
  Failures: Integer = 0;
  Row: Integer = 3;
  CurrentLabel: AnsiString;
  LastRaw: UnicodeString;
  LogFile: Text;
  Logging: Boolean = False;


// True if code point aCode takes 2 console cells
function IsWide(aCode: Cardinal): Boolean;

begin
  case aCode of
    $1100..$115F, $2E80..$303E, $3041..$33FF, $3400..$4DBF, $4E00..$9FFF,
    $A000..$A4CF, $AC00..$D7A3, $F900..$FAFF, $FE30..$FE4F, $FF00..$FF60, $FFE0..$FFE6:
      Result:=True;
  else
    Result:=False;
  end;
end;


// Text shown in cells aX.. of row aY for the characters of aExpected. The cells are read in one call; a wide
// character comes back as one UTF-16 unit, or as two cells of which one may be blank.
function ReadBack(const aExpected: UnicodeString; aX, aY: Integer): UnicodeString;

var
  lPos: TCoord;
  lRaw: UnicodeString;
  lRead: DWord;
  i, j, lCells: Integer;
  lPerCell: Boolean;

begin
  Result:='';
  lCells:=0;
  for i:=1 to Length(aExpected) do
    if IsWide(Ord(aExpected[i])) then
      Inc(lCells,2)
    else
      Inc(lCells);
  SetLength(lRaw,lCells);
  lPos.X:=aX-1;
  lPos.Y:=aY-1;
  if not ReadConsoleOutputCharacterW(GetStdHandle(STD_OUTPUT_HANDLE),Pointer(lRaw),lCells,lPos,lRead) then
    lRead:=0;
  SetLength(lRaw,lRead);
  LastRaw:=lRaw;
  lPerCell:=Integer(lRead)>=lCells;
  j:=1;
  for i:=1 to Length(aExpected) do
    begin
    if j>Length(lRaw) then
      Result:=Result+'?'
    else if IsWide(Ord(aExpected[i])) and lPerCell then
      begin
      if (lRaw[j]=' ') and (j<Length(lRaw)) then
        Result:=Result+lRaw[j+1]
      else
        Result:=Result+lRaw[j];
      Inc(j,2);
      end
    else
      begin
      Result:=Result+lRaw[j];
      Inc(j);
      end;
    end;
end;


// Code points of s, for a FAIL message
function CodePoints(const s: UnicodeString): AnsiString;

var
  i: Integer;

begin
  Result:='';
  for i:=1 to Length(s) do
    Result:=Result+' '+HexStr(Ord(s[i]),4);
end;


// Writes aLine to the log file when one was given
procedure Log(const aLine: AnsiString);

begin
  if Logging then
    Writeln(LogFile,aLine);
end;


// Compares the cells at TextCol of the current row with aExpected and writes OK or FAIL
procedure Verify(const aExpected: UnicodeString);

var
  lGot: UnicodeString;

begin
  lGot:=ReadBack(aExpected,TextCol,Row);
  if lGot=aExpected then
    Log(CurrentLabel+': OK')
  else
    Log(CurrentLabel+': FAIL, expected'+CodePoints(aExpected)+', got'+CodePoints(lGot)+', cells'+CodePoints(LastRaw));
  GotoXY(ResultCol,Row);
  if lGot=aExpected then
    Write('OK')
  else
    begin
    Inc(Failures);
    Write('FAIL');
    GotoXY(4,Row+1);
    Write('got:',CodePoints(lGot));
    Inc(Row);
    end;
  Inc(Row);
end;


// Writes aLabel at the start of the current row and moves to TextCol
procedure StartLine(const aLabel: AnsiString);

begin
  CurrentLabel:=aLabel;
  GotoXY(1,Row);
  Write(aLabel);
  GotoXY(TextCol,Row);
end;


// aText converted to the ANSI code page and back: what an ANSI code page console shows for it
function ViaACP(const aText: UnicodeString): UnicodeString;

var
  lAnsi: RawByteString;
  lLen: Integer;

begin
  lLen:=WideCharToMultiByte(CP_ACP,0,PWideChar(aText),Length(aText),nil,0,nil,nil);
  SetLength(lAnsi,lLen);
  WideCharToMultiByte(CP_ACP,0,PWideChar(aText),Length(aText),PAnsiChar(lAnsi),lLen,nil,nil);
  lLen:=MultiByteToWideChar(CP_ACP,0,PAnsiChar(lAnsi),Length(lAnsi),nil,0);
  SetLength(Result,lLen);
  MultiByteToWideChar(CP_ACP,0,PAnsiChar(lAnsi),Length(lAnsi),PWideChar(Result),lLen);
end;


// UTF-8 bytes of aText in a string that claims to be in the ANSI code page, as a literal in a source without {$codepage}
function RawUTF8(const aText: UnicodeString): RawByteString;

begin
  Result:=UTF8Encode(aText);
  SetCodePage(Result,CP_ACP,False);
end;


var
  lInfo: TConsoleScreenBufferInfo;
  lOriginalCP: UINT;
  lBytes: RawByteString;
  lText: UnicodeString;
  i, lWidth: Integer;

begin
  if not GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),lInfo) then
    begin
    writeln('skipped: no console');
    halt(0);
    end;
  if ParamCount>0 then
    begin
    Assign(LogFile,ParamStr(1));
    Rewrite(LogFile);
    Logging:=True;
    end;
  lOriginalCP:=GetConsoleOutputCP;
  Log('ANSI code page '+HexStr(GetACP,4)+', console code page '+HexStr(lOriginalCP,4));
  ClrScr;
  Write('Crt Unicode output, ANSI code page ',GetACP,', console code page ',lOriginalCP,'. OK/FAIL in column ',ResultCol,'.');

  { Defaults: UseACP=True, Output in the ANSI code page }
  StartLine('Defaults, Greek');
  Write(Greek);
  Verify(ViaACP(Greek));

  { Output declared UTF-8: any text, whatever UseACP says }
  SetTextCodePage(Output,CP_UTF8);
  StartLine('UTF-8 Output, Greek');
  Write(Greek);
  Verify(Greek);
  StartLine('UTF-8 Output, CJK (wide)');
  Write(CJK);
  Verify(CJK);
  SetUseACP(False);
  StartLine('UTF-8 Output, UseACP off, Greek');
  Write(Greek);
  Verify(Greek);
  StartLine('UTF-8 Output, UseACP off, CJK');
  Write(CJK);
  Verify(CJK);

  { one byte per Write: UTF-8 sequences split over flushes }
  StartLine('UTF-8 one byte per Write');
  lBytes:=UTF8Encode(Greek+' '+CJK);
  for i:=1 to Length(lBytes) do
    Write(lBytes[i]);
  Verify(Greek+' '+CJK);
  SetTextCodePage(Output,GetACP);

  { the report: raw UTF-8 bytes, UseACP off, console in code page 65001 }
  if SetConsoleOutputCP(CP_UTF8) then
    begin
    StartLine('Raw UTF-8, console 65001');
    Write(RawUTF8(Greek));
    Verify(Greek);
    end;

  { OEM box drawing bytes with UseACP off still show in the console code page }
  if SetConsoleOutputCP(437) then
    begin
    StartLine('OEM 437 box bytes, UseACP off');
    Write(#$DA#$C4#$BF#$B3#$C0#$D9);
    Verify(Box);
    end;
  SetConsoleOutputCP(lOriginalCP);

  { a wide character does not straddle the right edge of the window }
  SetTextCodePage(Output,CP_UTF8);
  lWidth:=WindMaxX-WindMinX+1;
  GotoXY(1,Row);
  Write('Wrap: wide character at the last column moves to the next line');
  Inc(Row);
  GotoXY(lWidth,Row);
  lText:='中';
  Write(lText);
  GotoXY(ResultCol,Row);
  if (ReadBack(lText,1,Row+1)=lText) then
    begin
    Log('Wrap: OK');
    Write('OK');
    end
  else
    begin
    Inc(Failures);
    Log('Wrap: FAIL, got'+CodePoints(ReadBack(lText,1,Row+1))+', cells'+CodePoints(LastRaw));
    Write('FAIL');
    end;
  Inc(Row,2);
  SetTextCodePage(Output,GetACP);
  SetUseACP(True);

  if Logging then
    begin
    Writeln(LogFile,Failures,' failure(s)');
    Close(LogFile);
    end;
  GotoXY(1,Row+1);
  if Failures=0 then
    writeln('All checks passed.')
  else
    writeln(Failures,' check(s) failed.');
  halt(Failures);
end.
