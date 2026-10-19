{ %target=win32,win64 }

{ Expands file names against UNC and drive base paths }
program tw40834;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  gErrors: Integer = 0;

// Checks the RawByteString and UnicodeString ExpandFileName against the expected result
procedure Check(const aFileName, aBasePath, aExpected: string);

var
  lRaw: RawByteString;
  lUni: UnicodeString;

begin
  lRaw:=ExpandFileName(RawByteString(aFileName), RawByteString(aBasePath));
  lUni:=ExpandFileName(UnicodeString(aFileName), UnicodeString(aBasePath));
  writeln('ExpandFileName(''', aFileName, ''', ''', aBasePath, ''') = ''', lRaw, '''');
  if lRaw<>aExpected then
    begin
      writeln('  RawByteString: expected ''', aExpected, ''', got ''', lRaw, '''');
      Inc(gErrors);
    end;
  if lUni<>UnicodeString(aExpected) then
    begin
      writeln('  UnicodeString: expected ''', aExpected, ''', got ''', string(lUni), '''');
      Inc(gErrors);
    end;
end;


begin
  Check('..\test.xyz', '\\pc\share', '\\pc\share\test.xyz');
  Check('\test.xyz', '\\pc\share', '\\pc\share\test.xyz');
  Check('\test.xyz', '\\pc\share\def', '\\pc\share\test.xyz');
  Check('\test.xyz', '\\pc\share\def\', '\\pc\share\test.xyz');
  Check('test.xyz', '\\pc\share\def', '\\pc\share\def\test.xyz');
  Check('..\test.xyz', '\\pc\share\def', '\\pc\share\test.xyz');
  Check('..\..\test.xyz', '\\pc\share\a\b', '\\pc\share\test.xyz');
  Check('..\..\..\test.xyz', '\\pc\share\a\b', '\\pc\share\test.xyz');
  Check('a\.\b\..\test.xyz', '\\pc\share', '\\pc\share\a\test.xyz');
  Check('\a\..\test.xyz', '\\pc\share\def', '\\pc\share\test.xyz');
  Check('\\other\data\test.xyz', '\\pc\share', '\\other\data\test.xyz');
  Check('\\other\data\x\..\..\test.xyz', '\\pc\share', '\\other\data\test.xyz');
  Check('\test.xyz', 'C:\a\b', 'C:\test.xyz');
  Check('..\test.xyz', 'C:\a\b', 'C:\a\test.xyz');
  Check('..\..\..\test.xyz', 'C:\a\b', 'C:\test.xyz');
  if gErrors>0 then
    begin
      writeln(gErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
