{ %target=win32,win64 }
{ case-insensitive compare keeps Hebrew final letter forms distinct, issue #20339 }
program tcomptextling;

{$mode objfpc}{$h+}

uses
  SysUtils;

const
  cKaf = UnicodeString(#$05DB);
  cFinalKaf = UnicodeString(#$05DA);
  cMem = UnicodeString(#$05DE);
  cFinalMem = UnicodeString(#$05DD);

var
  lW1, lW2: WideString;

begin
  if UnicodeCompareText(cKaf,cFinalKaf)=0 then
    halt(1);
  if UnicodeCompareText(cMem,cFinalMem)=0 then
    halt(2);
  if UnicodeSameText(cKaf,cFinalKaf) then
    halt(3);
  if UnicodeCompareText('abc','ABC')<>0 then
    halt(4);
  if not UnicodeSameText('abc','ABC') then
    halt(5);
  if UnicodeCompareText('abc','ABD')>=0 then
    halt(6);

  lW1:=cKaf;
  lW2:=cFinalKaf;
  if WideCompareText(lW1,lW2)=0 then
    halt(11);
  if WideSameText(lW1,lW2) then
    halt(12);
  if WideCompareText('abc','ABC')<>0 then
    halt(13);

  if widestringmanager.CompareUnicodeStringProc(cKaf,cFinalKaf,[coLingIgnoreCase])=0 then
    halt(21);
  if widestringmanager.CompareUnicodeStringProc('abc','ABC',[coLingIgnoreCase])<>0 then
    halt(22);
  if widestringmanager.CompareUnicodeStringProc('abc','ABC',[coIgnoreCase])<>0 then
    halt(23);
  if widestringmanager.CompareUnicodeStringProc('abc','ABC',[])=0 then
    halt(24);

  if AnsiCompareText('abc','ABC')<>0 then
    halt(31);
  if AnsiStrIComp(PAnsiChar('abc'),PAnsiChar('ABC'))<>0 then
    halt(32);
  if AnsiStrLIComp(PAnsiChar('abcx'),PAnsiChar('ABCy'),3)<>0 then
    halt(33);
  writeln('ok');
end.
