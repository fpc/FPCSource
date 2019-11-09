{$mode objfpc}
{$h+}
unit utsysutils;

Interface

Function CheckMaxDateTime : String;

Implementation

uses sysutils, punit, utrtl;

Function CheckMaxDateTime : String;


var
  y,d,h,m,s,z : Word;
    
begin
  Result:='';
  DecodeTime(MaxDateTime, h, m, s, z);
  if not AssertEquals('Hours correct',23,h) then exit;
  if not AssertEquals('Minutes correct',59,m) then exit;
  if not AssertEquals('Seconds correct',59,s) then exit;
  if not AssertEquals('Milliseconds correct',999,z) then exit;
  DecodeDate(MaxDateTime, y, m,d);
  if not AssertEquals('Year correct',9999,y) then exit;
  if not AssertEquals('Month correct',12,m) then exit;
  if not AssertEquals('Day correct',31,d) then exit;
end;

Function CheckIsValidIdent : string;

begin
  Result:='';
  if not AssertTrue('Normal',isValidIdent('abc')) then exit;
  if not AssertTrue('Normal with dot',isValidIdent('abc',true)) then exit;
  if not AssertTrue('Normal underscore',isValidIdent('_abc')) then exit;
  if not AssertTrue('Normal underscore with dot',isValidIdent('_abc',true)) then exit;
  if not AssertTrue('Normal last underscore',isValidIdent('abc_')) then exit;
  if not AssertTrue('Normal last underscore with dot',isValidIdent('abc_',true)) then exit;
  if not AssertTrue('Normal number',isValidIdent('abc0')) then exit;
  if not AssertTrue('Normal number',isValidIdent('abc0',true)) then exit;
  if not AssertFalse('Normal number first',isValidIdent('9abc')) then exit;
  if not AssertFalse('Normal number first',isValidIdent('9abc',True)) then exit;
  if not AssertTrue('Containing dot, allowed',IsValidIdent('a.b',True)) then exit;
  if not AssertFalse('Containing dot, not allowed',IsValidIdent('a.b')) then exit;
  if not AssertFalse('Containing dot pos 1, allowed',IsValidIdent('.b',true)) then exit;
end;

Function CheckAnsiDequotedString : string;

begin
  Result:='';
  if Not AssertEquals('Nothing between quotes','',AnsiDequotedStr('""', '"')) then exit;
  if Not AssertEquals('empty string','',AnsiDequotedStr('', '"')) then exit;
  if Not AssertEquals('Non-quoted string','abc',AnsiDequotedStr('abc', '"')) then exit;
end;

Function CheckFileOpenDirFails : String;

begin
  Result:='';
  If Not AssertEquals('Cannot open directory with fileOpen',-1,FileOpen('.',fmOpenRead)) then exit;
end;

Function CheckStringReplace : String;

Var
  C : integer;

begin
  Result:='';
  If not AssertEquals('StringReplace 1 Result','ABA',StringReplace('ACA','C','B',[],C)) then exit;
  If not AssertEquals('StringReplace 1 count Result',1,C) then exit;
  If not AssertEquals('StringReplace 2 Result','ABAC',StringReplace('ACAC','C','B',[],C)) then exit;
  If not AssertEquals('StringReplace 2 count Result',1,C) then exit;
  If not AssertEquals('StringReplace 3 Result','ABAB',StringReplace('ACAC','C','B',[rfReplaceAll],C)) then exit;
  If not AssertEquals('StringReplace 3 count Result',2,C) then exit;
  If not AssertEquals('StringReplace 4 Result','ACAC',StringReplace('ACAC','D','B',[rfReplaceAll],C)) then exit;
  If not AssertEquals('StringReplace 4 count Result',0,C) then exit;
end;

Function CheckUnicodeStringReplace : String;

Var
  C : integer;

begin
  Result:='';
  If not AssertEquals('UnicodeStringReplace 1 Result','ABA',UnicodeStringReplace('ACA','C','B',[],C)) then exit;
  If not AssertEquals('UnicodeStringReplace 1 count Result',1,C) then exit;
  If not AssertEquals('UnicodeStringReplace 2 Result','ABAC',UnicodeStringReplace('ACAC','C','B',[],C)) then exit;
  If not AssertEquals('UnicodeStringReplace 2 count Result',1,C) then exit;
  If not AssertEquals('UnicodeStringReplace 3 Result','ABAB',UnicodeStringReplace('ACAC','C','B',[rfReplaceAll],C)) then exit;
  If not AssertEquals('UnicodeStringReplace 3 count Result',2,C) then exit;
  If not AssertEquals('UnicodeStringReplace 4 Result','ACAC',UnicodeStringReplace('ACAC','D','B',[rfReplaceAll],C)) then exit;
  If not AssertEquals('UnicodeStringReplace 4 count Result',0,C) then exit;
end;

Function CheckWideStringReplace : String;

Var
  C : integer;

begin
  Result:='';
  If not AssertEquals('WideStringReplace 1 Result','ABA',WideStringReplace('ACA','C','B',[],C)) then exit;
  If not AssertEquals('WideStringReplace 1 count Result',1,C) then exit;
  If not AssertEquals('WideStringReplace 2 Result','ABAC',WideStringReplace('ACAC','C','B',[],C)) then exit;
  If not AssertEquals('WideStringReplace 2 count Result',1,C) then exit;
  If not AssertEquals('WideStringReplace 3 Result','ABAB',WideStringReplace('ACAC','C','B',[rfReplaceAll],C)) then exit;
  If not AssertEquals('WideStringReplace 3 count Result',2,C) then exit;
  If not AssertEquals('WideStringReplace 4 Result','ACAC',WideStringReplace('ACAC','D','B',[rfReplaceAll],C)) then exit;
  If not AssertEquals('WideStringReplace 4 count Result',0,C) then exit;
end;

Function CheckWrapText : String;

begin
  Result:='';
  If not AssertEquals('Default','hello hello',WrapText('hello hello',7)) then exit;
end;


begin
  SysutilsTest('CheckMaxDateTime',@CheckMaxDateTime);
  SysutilsTest('CheckIsValidIdent',@CheckIsValidIdent);
  SysutilsTest('CheckAnsiDequotedString',@CheckAnsiDequotedString);
  SysutilsTest('CheckFileOpenDirFails',@CheckFileOpenDirFails);
  SysutilsTest('CheckStringReplace',@CheckStringReplace);
  SysutilsTest('CheckUnicodeStringReplace',@CheckUnicodeStringReplace);
  SysutilsTest('CheckWideStringReplace',@CheckWideStringReplace);
  SysutilsTest('CheckWrapText',@CheckWrapText);
end.

