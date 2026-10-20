{ CodePageNameToCodePage with the dashless and little-endian UTF names }
program tcodepagename;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  lErrors: Integer;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Checks that aName maps to aExpected }
procedure CheckName(const aName: string; aExpected: TSystemCodePage);

var
  lCp: TSystemCodePage;

begin
  lCp:=CodePageNameToCodePage(aName);
  Check(lCp=aExpected, 'CodePageNameToCodePage('''+aName+''') = '+IntToStr(aExpected)+', got '+IntToStr(lCp));
end;


{ Checks that the name of every code page is found }
procedure CheckAllNamesFound;

var
  lCp: Integer;
  lName: string;

begin
  for lCp:=0 to $FFFE do
    begin
      lName:=CodePageToCodePageName(lCp);
      if lName<>'' then
        Check(CodePageNameToCodePage(lName)<>$FFFF, 'Name '''+lName+''' of code page '+IntToStr(lCp)+' is found');
    end;
end;


begin
  lErrors:=0;
  CheckName('utf8', CP_UTF8);
  CheckName('utf16', CP_UTF16);
  CheckName('utf16le', CP_UTF16);
  CheckName('utf16be', CP_UTF16BE);
  CheckName('utf32', 12000);
  CheckName('utf32le', 12000);
  CheckName('utf32be', 12001);
  CheckName('utf-32le', 12000);
  CheckName('UTF8', CP_UTF8);
  CheckName('Utf16LE', CP_UTF16);
  CheckName('UTF-32LE', 12000);
  CheckName('utf-8', CP_UTF8);
  CheckName('utf-16', CP_UTF16);
  CheckName('utf-16le', CP_UTF16);
  CheckName('utf-16be', CP_UTF16BE);
  CheckName('utf-32', 12000);
  CheckName('utf-32be', 12001);
  CheckName('sjis', 932);
  CheckName('cyrillic', 28595);
  CheckName('iso-8859-8-i', 38598);
  CheckName('l1', 28591);
  CheckName('utf', $FFFF);
  CheckName('utf64', $FFFF);
  CheckName('oem', $FFFF);
  CheckName('', $FFFF);
  CheckAllNamesFound;
  if lErrors>0 then
    begin
      writeln(lErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
