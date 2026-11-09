{ %target=win32,win64 }
{ an AnsiString converts to a WideString and to a UnicodeString in the code
  pages MultiByteToWideChar refuses MB_PRECOMPOSED for, where Windows has them
  installed: with known results in ISO-2022-JP (50220), GB18030 (54936) and HZ
  (52936), and in the others as MultiByteToWideChar without flags converts }
program tcpstr29;

{$mode objfpc}{$h+}

uses
  windows;

function Bytes(const b: array of byte; cp: TSystemCodePage): RawByteString;
var
  i: longint;
begin
  result:='';
  setlength(result,length(b));
  for i:=0 to high(b) do
    result[i+1]:=AnsiChar(b[i]);
  SetCodePage(result,cp,false);
end;

procedure Check(const s: RawByteString; const expected: unicodestring; code: longint);
var
  w: widestring;
  u: unicodestring;
begin
  w:=s;
  if w<>expected then
    begin
      writeln('error ',code,': widestring of length ',length(w));
      halt(code);
    end;
  u:=s;
  if u<>expected then
    begin
      writeln('error ',code+1,': unicodestring of length ',length(u));
      halt(code+1);
    end;
end;

{ text in a page converts as Windows converts it when asked without flags }
procedure CheckAsWindows(cp: TSystemCodePage; code: longint);
var
  src: RawByteString;
  expected: unicodestring;
  n: longint;
begin
  if not IsValidCodePage(cp) then
    exit;
  { two high bytes as well: the fallback conversion also gives 'abc' }
  src:=Bytes([$61,$62,$63,$c1,$e9],cp);
  n:=MultiByteToWideChar(cp,0,PAnsiChar(src),length(src),nil,0);
  if n=0 then
    exit;
  setlength(expected,n);
  MultiByteToWideChar(cp,0,PAnsiChar(src),length(src),PWideChar(expected),n);
  Check(src,expected,code);
end;

const
  OtherPages: array[0..20] of TSystemCodePage = (50221,50222,50225,50227,50229,
    52936,55000,55001,55002,55003,55004,
    57002,57003,57004,57005,57006,57007,57008,57009,57010,57011);
var
  i: longint;
begin
  if IsValidCodePage(52936) then
    { ~{ switches to GB2312 and ~} back; the two bytes between are one character }
    Check(Bytes([$7e,$7b,$3c,$3a,$7e,$7d],52936),unicodestring(widechar($5df1)),9);
  for i:=low(OtherPages) to high(OtherPages) do
    CheckAsWindows(OtherPages[i],11+2*i);
  if IsValidCodePage(50220) then
    begin
      { two kanji between the escapes that switch to JIS X 0208 and back }
      Check(Bytes([$1b,$24,$42,$4e,$61,$4f,$42,$1b,$28,$42],50220),
        unicodestring(widechar($4ee4))+unicodestring(widechar($548c)),1);
      Check(Bytes([$61,$62,$63],50220),'abc',3);
    end
  else
    writeln('code page 50220 is not installed');
  if IsValidCodePage(54936) then
    begin
      { a two-byte and a four-byte sequence }
      Check(Bytes([$81,$64,$81,$30,$81,$30],54936),
        unicodestring(widechar($4e68))+unicodestring(widechar($0080)),5);
      Check(Bytes([$61,$62,$63],54936),'abc',7);
    end
  else
    writeln('code page 54936 is not installed');
  writeln('ok');
end.
