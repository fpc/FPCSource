unit tests.rtti.invoke;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

{.$define debug}

interface

uses
{$IFDEF FPC}
  fpcunit,testregistry, testutils,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  sysutils, typinfo, Rtti;

type
  TTestInvoke = class(TTestCase)
  private type
    TInvokeFlag = (
      ifStatic,
      ifConstructor
    );
    TInvokeFlags = set of TInvokeFlag;
  private
    function DoInvoke(aCodeAddress: CodePointer; aArgs: TValueArray; aCallConv: TCallConv; aResultType: PTypeInfo; aFlags: TInvokeFlags; out aValid: Boolean): TValue;
    procedure DoStaticInvokeTestOrdinalCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: Int64);
    procedure DoStaticInvokeTestAnsiStringCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: AnsiString);
    procedure DoStaticInvokeTestUnicodeStringCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: UnicodeString);
{$ifdef fpc}
    procedure Status(const aMsg: String);
{$endif}
  published
    procedure TestShortString;
    procedure TestAnsiString;
    procedure TestWideString;
    procedure TestUnicodeString;

    procedure TestLongInt;
    procedure TestInt64;

    procedure TestTObject;
  end;

{$ifndef fpc}
  TValueHelper = record helper for TValue
    function AsUnicodeString: UnicodeString;
    function AsAnsiString: AnsiString;
  end;
{$endif}

implementation

{$ifndef fpc}
function TValueHelper.AsUnicodeString: UnicodeString;
begin
  Result := UnicodeString(AsString);
end;

function TValueHelper.AsAnsiString: AnsiString;
begin
  Result := AnsiString(AsString);
end;
{$endif}

function TTestInvoke.DoInvoke(aCodeAddress: CodePointer; aArgs: TValueArray;
  aCallConv: TCallConv; aResultType: PTypeInfo; aFlags: TInvokeFlags; out aValid: Boolean): TValue;
begin
  try
    Result := Rtti.Invoke(aCodeAddress, aArgs, aCallConv, aResultType, ifStatic in aFlags, ifConstructor in aFlags);
    aValid := True;
  except
    on e: ENotImplemented do begin
      Status('Ignoring unimplemented functionality of test');
      aValid := False;
    end else
      raise;
  end;
end;

procedure TTestInvoke.DoStaticInvokeTestOrdinalCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: Int64);
var
  resval: TValue;
  valid: Boolean;
begin
  resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
  if valid and Assigned(aReturnType) and (resval.AsOrdinal <> aResult) then begin
    Fail('Result of test "%s" is unexpected; expected: %s, got: %s', [aTestName, IntToStr(aResult), IntToStr(resval.AsOrdinal)]);
  end;
end;

procedure TTestInvoke.DoStaticInvokeTestAnsiStringCompare(
  const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv;
  aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: AnsiString);
var
  resval: TValue;
  valid: Boolean;
begin
  resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
  if valid and Assigned(aReturnType) and (resval.AsAnsiString <> aResult) then begin
    Fail('Result of test "%s" is unexpected; expected: "%s", got: "%s"', [aTestName, aResult, resval.AsString]);
  end;
end;

procedure TTestInvoke.DoStaticInvokeTestUnicodeStringCompare(
  const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv;
  aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: UnicodeString
  );
var
  resval: TValue;
  valid: Boolean;
begin
  resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
  if valid and Assigned(aReturnType) and (resval.AsUnicodeString <> aResult) then begin
    Fail('Result of test "%s" is unexpected; expected: "%s", got: "%s"', [aTestName, aResult, resval.AsString]);
  end;
end;

{$ifdef fpc}
procedure TTestInvoke.Status(const aMsg: String);
begin
{$ifdef debug}
  Writeln(aMsg);
{$endif}
end;
{$endif}

function TestShortStringRegister(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: ShortString): ShortString; register;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestShortStringCdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: ShortString): ShortString; cdecl;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestShortStringStdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: ShortString): ShortString; stdcall;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestShortStringPascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: ShortString): ShortString; pascal;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

procedure TTestInvoke.TestShortString;
const
  strs: array[0..5] of ShortString = (
    'This ',
    'is a ',
    'test ',
    'of ',
    'shortstring ',
    'concatenation'
  );

var
  values: TValueArray;
  resstr: ShortString;
  i: LongInt;
begin
  SetLength(values, Length(strs));
  resstr := '';
  for i := Low(values) to High(values) do begin
    TValue.Make(@strs[i], TypeInfo(ShortString), values[i]);
    resstr := resstr + strs[i];
  end;

  DoStaticInvokeTestAnsiStringCompare('ShortString Register', @TestShortStringRegister, ccReg, values, TypeInfo(ShortString), resstr);
  DoStaticInvokeTestAnsiStringCompare('ShortString Cdecl', @TestShortStringCdecl, ccCdecl, values, TypeInfo(ShortString), resstr);
  DoStaticInvokeTestAnsiStringCompare('ShortString StdCall', @TestShortStringStdCall, ccStdCall, values, TypeInfo(ShortString), resstr);
  DoStaticInvokeTestAnsiStringCompare('ShortString Pascal', @TestShortStringPascal, ccPascal, values, TypeInfo(ShortString), resstr);
end;

function TestAnsiStringRegister(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: AnsiString): AnsiString; register;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestAnsiStringCdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: AnsiString): AnsiString; cdecl;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestAnsiStringStdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: AnsiString): AnsiString; stdcall;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestAnsiStringPascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: AnsiString): AnsiString; pascal;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

procedure TTestInvoke.TestAnsiString;
const
  strs: array[0..5] of AnsiString = (
    'This ',
    'is a ',
    'test ',
    'of ',
    'AnsiString ',
    'concatenation'
  );

var
  values: TValueArray;
  resstr: AnsiString;
  i: LongInt;
begin
  SetLength(values, Length(strs));
  resstr := '';
  for i := Low(values) to High(values) do begin
    TValue.Make(@strs[i], TypeInfo(AnsiString), values[i]);
    resstr := resstr + strs[i];
  end;

  DoStaticInvokeTestAnsiStringCompare('AnsiString Register', @TestAnsiStringRegister, ccReg, values, TypeInfo(AnsiString), resstr);
  DoStaticInvokeTestAnsiStringCompare('AnsiString Cdecl', @TestAnsiStringCdecl, ccCdecl, values, TypeInfo(AnsiString), resstr);
  DoStaticInvokeTestAnsiStringCompare('AnsiString StdCall', @TestAnsiStringStdCall, ccStdCall, values, TypeInfo(AnsiString), resstr);
  DoStaticInvokeTestAnsiStringCompare('AnsiString Pascal', @TestAnsiStringPascal, ccPascal, values, TypeInfo(AnsiString), resstr);
end;

function TestWideStringRegister(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: WideString): WideString; register;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestWideStringCdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: WideString): WideString; cdecl;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestWideStringStdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: WideString): WideString; stdcall;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestWideStringPascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: WideString): WideString; pascal;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

procedure TTestInvoke.TestWideString;
const
  strs: array[0..5] of WideString = (
    'This ',
    'is a ',
    'test ',
    'of ',
    'WideString ',
    'concatenation'
  );

var
  values: TValueArray;
  resstr: WideString;
  i: LongInt;
begin
  SetLength(values, Length(strs));
  resstr := '';
  for i := Low(values) to High(values) do begin
    TValue.Make(@strs[i], TypeInfo(WideString), values[i]);
    resstr := resstr + strs[i];
  end;

  DoStaticInvokeTestUnicodeStringCompare('WideString Register', @TestWideStringRegister, ccReg, values, TypeInfo(WideString), resstr);
  DoStaticInvokeTestUnicodeStringCompare('WideString Cdecl', @TestWideStringCdecl, ccCdecl, values, TypeInfo(WideString), resstr);
  DoStaticInvokeTestUnicodeStringCompare('WideString StdCall', @TestWideStringStdCall, ccStdCall, values, TypeInfo(WideString), resstr);
  DoStaticInvokeTestUnicodeStringCompare('WideString Pascal', @TestWideStringPascal, ccPascal, values, TypeInfo(WideString), resstr);
end;

function TestUnicodeStringRegister(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: UnicodeString): UnicodeString; register;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestUnicodeStringCdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: UnicodeString): UnicodeString; cdecl;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestUnicodeStringStdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: UnicodeString): UnicodeString; stdcall;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

function TestUnicodeStringPascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: UnicodeString): UnicodeString; pascal;
begin
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6;
end;

procedure TTestInvoke.TestUnicodeString;
const
  strs: array[0..5] of UnicodeString = (
    'This ',
    'is a ',
    'test ',
    'of ',
    'UnicodeString ',
    'concatenation'
  );

var
  values: TValueArray;
  resstr: UnicodeString;
  i: LongInt;
begin
  SetLength(values, Length(strs));
  resstr := '';
  for i := Low(values) to High(values) do begin
    TValue.Make(@strs[i], TypeInfo(UnicodeString), values[i]);
    resstr := resstr + strs[i];
  end;

  DoStaticInvokeTestUnicodeStringCompare('UnicodeString Register', @TestUnicodeStringRegister, ccReg, values, TypeInfo(UnicodeString), resstr);
  DoStaticInvokeTestUnicodeStringCompare('UnicodeString Cdecl', @TestUnicodeStringCdecl, ccCdecl, values, TypeInfo(UnicodeString), resstr);
  DoStaticInvokeTestUnicodeStringCompare('UnicodeString StdCall', @TestUnicodeStringStdCall, ccStdCall, values, TypeInfo(UnicodeString), resstr);
  DoStaticInvokeTestUnicodeStringCompare('UnicodeString Pascal', @TestUnicodeStringPascal, ccPascal, values, TypeInfo(UnicodeString), resstr);
end;

function TestLongIntRegister(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: LongInt): LongInt; register;
begin
  Result := aArg1 + aArg2 * 10 + aArg3 * 100 + aArg4 * 1000 + aArg5 * 10000 + aArg6 * 100000;
end;

function TestLongIntCdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: LongInt): LongInt; cdecl;
begin
  Result := aArg1 + aArg2 * 10 + aArg3 * 100 + aArg4 * 1000 + aArg5 * 10000 + aArg6 * 100000;
end;

function TestLongIntStdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: LongInt): LongInt; stdcall;
begin
  Result := aArg1 + aArg2 * 10 + aArg3 * 100 + aArg4 * 1000 + aArg5 * 10000 + aArg6 * 100000;
end;

function TestLongIntPascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: LongInt): LongInt; pascal;
begin
  Result := aArg1 + aArg2 * 10 + aArg3 * 100 + aArg4 * 1000 + aArg5 * 10000 + aArg6 * 100000;
end;

procedure TTestInvoke.TestLongInt;
const
  vals: array[0..5] of LongInt = (
    8,
    4,
    7,
    3,
    6,
    1
  );

var
  values: TValueArray;
  resval, factor: LongInt;
  i: LongInt;
begin
  SetLength(values, Length(vals));
  resval := 0;
  factor := 1;
  for i := Low(values) to High(values) do begin
    TValue.Make(@vals[i], TypeInfo(LongInt), values[i]);
    resval := resval + vals[i] * factor;
    factor := factor * 10;
  end;

  DoStaticInvokeTestOrdinalCompare('LongInt Register', @TestLongIntRegister, ccReg, values, TypeInfo(LongInt), resval);
  DoStaticInvokeTestOrdinalCompare('LongInt Cdecl', @TestLongIntCdecl, ccCdecl, values, TypeInfo(LongInt), resval);
  DoStaticInvokeTestOrdinalCompare('LongInt StdCall', @TestLongIntStdCall, ccStdCall, values, TypeInfo(LongInt), resval);
  DoStaticInvokeTestOrdinalCompare('LongInt Pascal', @TestLongIntPascal, ccPascal, values, TypeInfo(LongInt), resval);
end;

function TestInt64Register(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: Int64): Int64; register;
begin
  Result := aArg1 + aArg2 * 100 + aArg3 * 10000 + aArg4 * 1000000 + aArg5 * 100000000 + aArg6 * 10000000000;
end;

function TestInt64Cdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: Int64): Int64; cdecl;
begin
  Result := aArg1 + aArg2 * 100 + aArg3 * 10000 + aArg4 * 1000000 + aArg5 * 100000000 + aArg6 * 10000000000;
end;

function TestInt64StdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: Int64): Int64; stdcall;
begin
  Result := aArg1 + aArg2 * 100 + aArg3 * 10000 + aArg4 * 1000000 + aArg5 * 100000000 + aArg6 * 10000000000;
end;

function TestInt64Pascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: Int64): Int64; pascal;
begin
  Result := aArg1 + aArg2 * 100 + aArg3 * 10000 + aArg4 * 1000000 + aArg5 * 100000000 + aArg6 * 10000000000;
end;

procedure TTestInvoke.TestInt64;
const
  vals: array[0..5] of Int64 = (
    8,
    4,
    7,
    3,
    6,
    1
  );

var
  values: TValueArray;
  resval, factor: Int64;
  i: LongInt;
begin
  SetLength(values, Length(vals));
  resval := 0;
  factor := 1;
  for i := Low(values) to High(values) do begin
    TValue.Make(@vals[i], TypeInfo(Int64), values[i]);
    resval := resval + vals[i] * factor;
    factor := factor * 100;
  end;

  DoStaticInvokeTestOrdinalCompare('Int64 Register', @TestInt64Register, ccReg, values, TypeInfo(Int64), resval);
  DoStaticInvokeTestOrdinalCompare('Int64 Cdecl', @TestInt64Cdecl, ccCdecl, values, TypeInfo(Int64), resval);
  DoStaticInvokeTestOrdinalCompare('Int64 StdCall', @TestInt64StdCall, ccStdCall, values, TypeInfo(Int64), resval);
  DoStaticInvokeTestOrdinalCompare('Int64 Pascal', @TestInt64Pascal, ccPascal, values, TypeInfo(Int64), resval);
end;

type
  TTestClass = class
    fString: String;
    fValue: LongInt;
  end;

function TestTTestClassRegister(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: TTestClass): TTestClass; register;
begin
  Result := TTestClass.Create;
  Result.fString := aArg1.fString + aArg2.fString + aArg3.fString + aArg4.fString + aArg5.fString + aArg6.fString;
  Result.fValue := aArg1.fValue + aArg2.fValue * 10 + aArg3.fValue * 100 + aArg4.fValue * 1000 + aArg5.fValue * 10000 + aArg6.fValue * 100000;
end;

function TestTTestClassCdecl(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: TTestClass): TTestClass; cdecl;
begin
  Result := TTestClass.Create;
  Result.fString := aArg1.fString + aArg2.fString + aArg3.fString + aArg4.fString + aArg5.fString + aArg6.fString;
  Result.fValue := aArg1.fValue + aArg2.fValue * 10 + aArg3.fValue * 100 + aArg4.fValue * 1000 + aArg5.fValue * 10000 + aArg6.fValue * 100000;
end;

function TestTTestClassStdCall(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: TTestClass): TTestClass; stdcall;
begin
  Result := TTestClass.Create;
  Result.fString := aArg1.fString + aArg2.fString + aArg3.fString + aArg4.fString + aArg5.fString + aArg6.fString;
  Result.fValue := aArg1.fValue + aArg2.fValue * 10 + aArg3.fValue * 100 + aArg4.fValue * 1000 + aArg5.fValue * 10000 + aArg6.fValue * 100000;
end;

function TestTTestClassPascal(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6: TTestClass): TTestClass; pascal;
begin
  Result := TTestClass.Create;
  Result.fString := aArg1.fString + aArg2.fString + aArg3.fString + aArg4.fString + aArg5.fString + aArg6.fString;
  Result.fValue := aArg1.fValue + aArg2.fValue * 10 + aArg3.fValue * 100 + aArg4.fValue * 1000 + aArg5.fValue * 10000 + aArg6.fValue * 100000;
end;

procedure TTestInvoke.TestTObject;

  procedure DoStaticInvokeTestClassCompare(
    const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv;
    aValues: TValueArray; aReturnType: PTypeInfo; aResult: TTestClass
    );
  var
    resval: TValue;
    rescls: TTestClass;
    valid: Boolean;
  begin
    resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
    if valid and Assigned(aReturnType) then begin
      rescls := TTestClass(PPointer(resval.GetReferenceToRawData)^);
      if (rescls.fString <> aResult.fString) or (rescls.fValue <> aResult.fValue) then
        Fail('Result of test "%s" is unexpected; expected: "%s"/%s, got: "%s"/%s', [aTestName, aResult.fString, IntToStr(aResult.fValue), rescls.fString, IntToStr(rescls.fValue)]);
    end;
  end;

const
  strs: array[0..5] of AnsiString = (
    'This ',
    'is a ',
    'test ',
    'of ',
    'AnsiString ',
    'concatenation'
  );

  vals: array[0..5] of Int64 = (
    8,
    4,
    7,
    3,
    6,
    1
  );

var
  values: TValueArray;
  t, rescls: TTestClass;
  i, factor: LongInt;
begin
  SetLength(values, Length(vals));
  factor := 1;
  rescls := TTestClass.Create;
  for i := Low(values) to High(values) do begin
    t := TTestClass.Create;
    t.fString := strs[i];
    t.fValue := vals[i];
    TValue.Make(@t, TypeInfo(TTestClass), values[i]);
    rescls.fValue := rescls.fValue + vals[i] * factor;
    rescls.fString := rescls.fString + strs[i];
    factor := factor * 10;
  end;

  DoStaticInvokeTestClassCompare('TTestClass Register', @TestTTestClassRegister, ccReg, values, TypeInfo(TTestClass), rescls);
  DoStaticInvokeTestClassCompare('TTestClass Cdecl', @TestTTestClassCdecl, ccCdecl, values, TypeInfo(TTestClass), rescls);
  DoStaticInvokeTestClassCompare('TTestClass StdCall', @TestTTestClassStdCall, ccStdCall, values, TypeInfo(TTestClass), rescls);
  DoStaticInvokeTestClassCompare('TTestClass Pascal', @TestTTestClassPascal, ccPascal, values, TypeInfo(TTestClass), rescls);
end;

begin
{$ifdef fpc}
  RegisterTest(TTestInvoke);
{$else fpc}
  RegisterTest(TTestInvoke.Suite);
{$endif fpc}
end.

