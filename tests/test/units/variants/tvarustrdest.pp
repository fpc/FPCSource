{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ VarCast to varUString cleared the destination, set its type, and then assigned the string
  over what was left there: a number in the destination was taken for a string to release, and
  when destination and source were one variant, the source was read after that. }
program tvarustrdest;

{$mode objfpc}{$H+}

uses
  SysUtils, Variants;

var
  gFailed: Integer = 0;

procedure Fail(const aWhat: string);
begin
  WriteLn('FAILED: ', aWhat);
  Inc(gFailed);
end;

procedure CheckDest(const aWhat: string; const aDest, aSource: Variant; const aWant: UnicodeString);
var
  lDest: Variant;
begin
  lDest := aDest;
  try
    VarCast(lDest, aSource, varUString);
    if VarType(lDest) <> varUString then
      Fail(aWhat + ': type $' + IntToHex(VarType(lDest), 4))
    else if UnicodeString(lDest) <> aWant then
      Fail(aWhat + ': gave ' + string(UnicodeString(lDest)));
  except
    on E: Exception do
      Fail(aWhat + ': raised ' + E.ClassName);
  end;
end;

procedure CheckSelf(const aWhat: string; const aValue: Variant; const aWant: UnicodeString);
var
  lDest: Variant;
begin
  lDest := aValue;
  try
    VarCast(lDest, lDest, varUString);
    if VarType(lDest) <> varUString then
      Fail(aWhat + ': type $' + IntToHex(VarType(lDest), 4))
    else if UnicodeString(lDest) <> aWant then
      Fail(aWhat + ': gave ' + string(UnicodeString(lDest)));
  except
    on E: Exception do
      Fail(aWhat + ': raised ' + E.ClassName);
  end;
end;

{ A variant that really is a varUString: assigning a unicodestring makes a varOleStr }
function UStr(const aText: UnicodeString): Variant;
begin
  VarClear(Result);
  TVarData(Result).vType := varUString;
  TVarData(Result).vustring := nil;
  UnicodeString(TVarData(Result).vustring) := aText;
end;

{ In a routine of its own, so that no temporary of the caller keeps a reference }
procedure SetUStr(var aDest: Variant; const aText: UnicodeString);
begin
  aDest := UStr(aText);
end;

var
  lDest, lBad: Variant;
  lRaw: TVarData;
  lOld: AnsiString;
  lOldU: UnicodeString;
  lRefs: SizeInt;
  lDestType: TVarType;
begin
  { a destination holding a number, a value of each width, from each kind of source }
  CheckDest('0 from an integer', 0, 5, '5');
  CheckDest('a shortint from an integer', ShortInt(7), 5, '5');
  CheckDest('a byte from an integer', Byte(7), 5, '5');
  CheckDest('a smallint from an integer', SmallInt(7), 5, '5');
  CheckDest('a word from an integer', Word(7), 5, '5');
  CheckDest('a longint from an integer', LongInt($12345678), 5, '5');
  CheckDest('a longword from an integer', LongWord($87654321), 5, '5');
  CheckDest('an int64 from an integer', Int64($123456789ABC), 5, '5');
  CheckDest('a qword from an integer', QWord($123456789ABC), 5, '5');
  CheckDest('a boolean from an integer', True, 5, '5');
  CheckDest('7 from a string', 7, 'abc', 'abc');
  CheckDest('7 from a widestring', 7, WideString('abc'), 'abc');
  CheckDest('7 from a unicodestring', 7, UStr('abc'), 'abc');
{$ifndef FPUNONE}
  { 1.1, not 1.5: the low half of 1.5's bits is zero, which a 32-bit target reads as nil }
  CheckDest('a double from an integer', 1.1, 5, '5');
{$endif}

  { a destination holding a string }
  CheckDest('a string from an integer', 'old', 5, '5');
  CheckDest('a widestring from an integer', WideString('old'), 5, '5');
  CheckDest('a unicodestring from an integer', UStr('old'), 5, '5');
  CheckDest('a unicodestring from a unicodestring', UStr('old'), UStr('abc'), 'abc');

  { the same variant as destination and source }
  CheckSelf('an integer onto itself', 5, '5');
  CheckSelf('a boolean onto itself', True, 'True');
  CheckSelf('a string onto itself', 'abc', 'abc');
  CheckSelf('a widestring onto itself', WideString('abc'), 'abc');
  CheckSelf('a unicodestring onto itself', UStr('abc'), 'abc');

  { a conversion that fails leaves the destination as it was, and usable }
  FillChar(lRaw, SizeOf(lRaw), 0);
  lRaw.vType := varError;
  lBad := Variant(lRaw);
  lRaw.vType := varEmpty;
  lDest := 'old';
  try
    VarCast(lDest, lBad, varUString);
    Fail('a source that cannot convert: no exception');
  except
    on E: EVariantError do
      ;
  end;
  if (VarType(lDest) <> varString) or (string(lDest) <> 'old') then
    Fail('a source that cannot convert: the destination changed');
  lDest := 7;
  lDestType := VarType(lDest);
  try
    VarCast(lDest, lBad, varUString);
    Fail('a source that cannot convert over a number: no exception');
  except
    on E: EVariantError do
      ;
  end;
  if (VarType(lDest) <> lDestType) or (Integer(lDest) <> 7) then
    Fail('a source that cannot convert over a number: the destination changed');
  lDest := Unassigned;

  { the string the destination held is released, an ansistring and a unicodestring }
  lOld := StringOfChar('x', 1000);
  lRefs := StringRefCount(lOld);
  lDest := lOld;
  VarCast(lDest, 5, varUString);
  if StringRefCount(lOld) <> lRefs then
    Fail('the string in the destination was not released');
  lDest := Unassigned;
  lOldU := UnicodeString(StringOfChar('y', 1000));
  lRefs := StringRefCount(lOldU);
  SetUStr(lDest, lOldU);
  if StringRefCount(lOldU) <> lRefs + 1 then
    Fail('the unicodestring was not put in the destination');
  VarCast(lDest, 5, varUString);
  if StringRefCount(lOldU) <> lRefs then
    Fail('the unicodestring in the destination was not released');
  lDest := Unassigned;

  if gFailed <> 0 then
    Halt(1);
  WriteLn('ok');
end.
