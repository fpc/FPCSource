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
  sysutils, typinfo, Rtti,
  Tests.Rtti.Util;

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
    procedure DoIntfInvoke(aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoMethodInvoke(aInst: TObject; aMethod: TMethod; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoProcVarInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoProcInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoStaticInvokeTestVariant(const aTestName: String;  aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: String);
    procedure DoUntypedInvoke(aInst: TObject; aProc: CodePointer; aMethod: TMethod; aTypeInfo: PTypeInfo; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
{$ifndef InLazIDE}
    {$ifdef fpc}generic{$endif} procedure GenDoMethodInvoke<T>(aInst: TObject; aMethod: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    {$ifdef fpc}generic{$endif} procedure GenDoProcvarInvoke<T>(aInst: TObject; aProc: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    {$ifdef fpc}generic{$endif} procedure GenDoProcInvoke<T>(aInst: TObject; aProc: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    {$ifdef fpc}generic{$endif} function GetRecValue<T>(aReverse: Boolean): TValue;
{$endif}
{$ifdef fpc}
    procedure Status(const aMsg: String);
{$endif}
  published
    procedure TestShortString;
    procedure TestAnsiString;
    procedure TestWideString;
    procedure TestUnicodeString;
    procedure TestVariant;

    procedure TestLongInt;
    procedure TestInt64;
    procedure TestIntfVariant;

    procedure TestTObject;

    procedure TestIntfMethods;
    procedure TestIntfMethodsRecs;
    procedure TestIntfMethodsVariant;

    procedure TestMethodVars;
    procedure TestMethodVarsRecs;

    procedure TestProcVars;
    procedure TestProcVarsRecs;

    procedure TestProc;
    procedure TestProcRecs;

    procedure TestUntyped;
  end;

implementation

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

procedure TTestInvoke.DoStaticInvokeTestVariant(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: String);
var
  resval: TValue;
  valid: Boolean;
begin
  resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
  if valid and (resval.AsAnsiString <> aResult) then begin
    Fail('Result of test "%s" is unexpected; expected: %s, got: %s', [aTestName, aResult, String(resval.AsAnsiString)]);
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

function TestVariantRegister(aArg1 : variant): string; register;

begin
  Result:=aArg1;
end;

function TestVariantCdecl(aArg1 : variant): string; cdecl;

begin
  Result:=aArg1;
end;

function TestVariantPascal(aArg1 : variant): string; pascal;

begin
  Result:=aArg1;
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


procedure TTestInvoke.TestVariant;

var
  values: TValueArray;
  aValue : variant;
  S : AnsiString;
begin
  SetLength(Values,1);
  S:='A nice string';
  aValue:=S;
  TValue.Make(@aValue, TypeInfo(Variant), Values[0]);
  DoStaticInvokeTestVariant('Test register',@TestVariantRegister,ccReg,values,TypeInfo(AnsiString),S);
  DoStaticInvokeTestVariant('Test cdecl',@TestVariantCdecl,ccCdecl,values,TypeInfo(AnsiString),S);
  DoStaticInvokeTestVariant('Test pascal',@TestVariantPascal,ccCdecl,values,TypeInfo(AnsiString),S);
end;

procedure TTestInvoke.TestIntfVariant;

var
  values,aOutput: TValueArray;
  aValue : variant;
  aResult : TValue;
  S : AnsiString;
begin
  SetLength(Values,1);
  S:='A nice string';
  UniqueString(S);
  aValue:=S;
  aResult:=Default(TValue);
  TValue.Make(@S, TypeInfo(AnsiString), aResult);
  TValue.Make(@aValue, TypeInfo(Variant), Values[0]);
  DoIntfInvoke(23,Values,aOutput,aResult);
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

const
  SingleArg1: Single = 1.23;
  SingleArg2In: Single = 3.21;
  SingleArg2Out: Single = 2.34;
  SingleArg3Out: Single = 9.87;
  SingleArg4: Single = 7.89;
  SingleRes: Single = 4.32;
  SingleAddArg1 = Single(1.23);
  SingleAddArg2 = Single(2.34);
  SingleAddArg3 = Single(3.45);
  SingleAddArg4 = Single(4.56);
  SingleAddArg5 = Single(5.67);
  SingleAddArg6 = Single(9.87);
  SingleAddArg7 = Single(8.76);
  SingleAddArg8 = Single(7.65);
  SingleAddArg9 = Single(6.54);
  SingleAddArg10 = Single(5.43);
  SingleAddRes = SingleAddArg1 + SingleAddArg2 + SingleAddArg3 + SingleAddArg4 + SingleAddArg5 +
                 SingleAddArg6 + SingleAddArg7 + SingleAddArg8 + SingleAddArg9 + SingleAddArg10;

  DoubleArg1: Double = 1.23;
  DoubleArg2In: Double = 3.21;
  DoubleArg2Out: Double = 2.34;
  DoubleArg3Out: Double = 9.87;
  DoubleArg4: Double = 7.89;
  DoubleRes: Double = 4.32;
  DoubleAddArg1 = Double(1.23);
  DoubleAddArg2 = Double(2.34);
  DoubleAddArg3 = Double(3.45);
  DoubleAddArg4 = Double(4.56);
  DoubleAddArg5 = Double(5.67);
  DoubleAddArg6 = Double(9.87);
  DoubleAddArg7 = Double(8.76);
  DoubleAddArg8 = Double(7.65);
  DoubleAddArg9 = Double(6.54);
  DoubleAddArg10 = Double(5.43);
  DoubleAddRes = DoubleAddArg1 + DoubleAddArg2 + DoubleAddArg3 + DoubleAddArg4 + DoubleAddArg5 +
                 DoubleAddArg6 + DoubleAddArg7 + DoubleAddArg8 + DoubleAddArg9 + DoubleAddArg10;

  ExtendedArg1: Extended = 1.23;
  ExtendedArg2In: Extended = 3.21;
  ExtendedArg2Out: Extended = 2.34;
  ExtendedArg3Out: Extended = 9.87;
  ExtendedArg4: Extended = 7.89;
  ExtendedRes: Extended = 4.32;
  ExtendedAddArg1 = Extended(1.23);
  ExtendedAddArg2 = Extended(2.34);
  ExtendedAddArg3 = Extended(3.45);
  ExtendedAddArg4 = Extended(4.56);
  ExtendedAddArg5 = Extended(5.67);
  ExtendedAddArg6 = Extended(9.87);
  ExtendedAddArg7 = Extended(8.76);
  ExtendedAddArg8 = Extended(7.65);
  ExtendedAddArg9 = Extended(6.54);
  ExtendedAddArg10 = Extended(5.43);
  ExtendedAddRes = ExtendedAddArg1 + ExtendedAddArg2 + ExtendedAddArg3 + ExtendedAddArg4 + ExtendedAddArg5 +
                 ExtendedAddArg6 + ExtendedAddArg7 + ExtendedAddArg8 + ExtendedAddArg9 + ExtendedAddArg10;

  CurrencyArg1: Currency = 1.23;
  CurrencyArg2In: Currency = 3.21;
  CurrencyArg2Out: Currency = 2.34;
  CurrencyArg3Out: Currency = 9.87;
  CurrencyArg4: Currency = 7.89;
  CurrencyRes: Currency = 4.32;
  CurrencyAddArg1 = Currency(1.23);
  CurrencyAddArg2 = Currency(2.34);
  CurrencyAddArg3 = Currency(3.45);
  CurrencyAddArg4 = Currency(4.56);
  CurrencyAddArg5 = Currency(5.67);
  CurrencyAddArg6 = Currency(9.87);
  CurrencyAddArg7 = Currency(8.76);
  CurrencyAddArg8 = Currency(7.65);
  CurrencyAddArg9 = Currency(6.54);
  CurrencyAddArg10 = Currency(5.43);
  CurrencyAddRes = CurrencyAddArg1 + CurrencyAddArg2 + CurrencyAddArg3 + CurrencyAddArg4 + CurrencyAddArg5 +
                 CurrencyAddArg6 + CurrencyAddArg7 + CurrencyAddArg8 + CurrencyAddArg9 + CurrencyAddArg10;

  CompArg1: Comp = 123;
  CompArg2In: Comp = 321;
  CompArg2Out: Comp = 234;
  CompArg3Out: Comp = 987;
  CompArg4: Comp = 789;
  CompRes: Comp = 432;
  CompAddArg1 = Comp(123);
  CompAddArg2 = Comp(234);
  CompAddArg3 = Comp(345);
  CompAddArg4 = Comp(456);
  CompAddArg5 = Comp(567);
  CompAddArg6 = Comp(987);
  CompAddArg7 = Comp(876);
  CompAddArg8 = Comp(765);
  CompAddArg9 = Comp(654);
  CompAddArg10 = Comp(543);
  CompAddRes = CompAddArg1 + CompAddArg2 + CompAddArg3 + CompAddArg4 + CompAddArg5 +
                 CompAddArg6 + CompAddArg7 + CompAddArg8 + CompAddArg9 + CompAddArg10;

type
  TTestRecord1 = packed record
    b: array[0..0] of Byte;
  end;

  TTestRecord2 = packed record
    b: array[0..1] of Byte;
  end;

  TTestRecord3 = packed record
    b: array[0..2] of Byte;
  end;

  TTestRecord4 = packed record
    b: array[0..3] of Byte;
  end;

  TTestRecord5 = packed record
    b: array[0..4] of Byte;
  end;

  TTestRecord6 = packed record
    b: array[0..5] of Byte;
  end;

  TTestRecord7 = packed record
    b: array[0..6] of Byte;
  end;

  TTestRecord8 = packed record
    b: array[0..7] of Byte;
  end;

  TTestRecord9 = packed record
    b: array[0..8] of Byte;
  end;

  TTestRecord10 = packed record
    b: array[0..9] of Byte;
  end;

  {$M+}
  ITestInterface = interface
    procedure Test1;
    function Test2: SizeInt;
    function Test3(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
    procedure Test4(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString);
    function Test5: AnsiString;
    function Test6: UnicodeString;
    function Test7: WideString;
    function Test8: ShortString;
    procedure Test9(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt);
    procedure Test10(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString);
    procedure Test11(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString);
    procedure Test12(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt);
    function Test13(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single;
    function Test14(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double;
    function Test15(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended;
    function Test16(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp;
    function Test17(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency;
    function Test18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
    function Test19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
    function Test20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
    function Test21(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
    function Test22(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
    function Test23(aArg1 : Variant) : AnsiString;

    function TestRecSize1(aArg1: TTestRecord1): TTestRecord1;
    function TestRecSize2(aArg1: TTestRecord2): TTestRecord2;
    function TestRecSize3(aArg1: TTestRecord3): TTestRecord3;
    function TestRecSize4(aArg1: TTestRecord4): TTestRecord4;
    function TestRecSize5(aArg1: TTestRecord5): TTestRecord5;
    function TestRecSize6(aArg1: TTestRecord6): TTestRecord6;
    function TestRecSize7(aArg1: TTestRecord7): TTestRecord7;
    function TestRecSize8(aArg1: TTestRecord8): TTestRecord8;
    function TestRecSize9(aArg1: TTestRecord9): TTestRecord9;
    function TestRecSize10(aArg1: TTestRecord10): TTestRecord10;

    procedure TestUntyped(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);
  end;
  {$M-}

  TTestInterfaceClass = class(TInterfacedObject, ITestInterface)
  private
    procedure Test1;
    function Test2: SizeInt;
    function Test3(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
    procedure Test4(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString);
    function Test5: AnsiString;
    function Test6: UnicodeString;
    function Test7: WideString;
    function Test8: ShortString;
    procedure Test9(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt);
    procedure Test10(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString);
    procedure Test11(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString);
    procedure Test12(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt);
    function Test13(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single;
    function Test14(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double;
    function Test15(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended;
    function Test16(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp;
    function Test17(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency;
    function Test18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
    function Test19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
    function Test20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
    function Test21(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
    function Test22(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
    function Test23(aArg1 : Variant) : AnsiString;

    function TestRecSize1(aArg1: TTestRecord1): TTestRecord1;
    function TestRecSize2(aArg1: TTestRecord2): TTestRecord2;
    function TestRecSize3(aArg1: TTestRecord3): TTestRecord3;
    function TestRecSize4(aArg1: TTestRecord4): TTestRecord4;
    function TestRecSize5(aArg1: TTestRecord5): TTestRecord5;
    function TestRecSize6(aArg1: TTestRecord6): TTestRecord6;
    function TestRecSize7(aArg1: TTestRecord7): TTestRecord7;
    function TestRecSize8(aArg1: TTestRecord8): TTestRecord8;
    function TestRecSize9(aArg1: TTestRecord9): TTestRecord9;
    function TestRecSize10(aArg1: TTestRecord10): TTestRecord10;

    procedure TestUntyped(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);
  public
    InputArgs: array of TValue;
    OutputArgs: array of TValue;
    ExpectedArgs: array of TValue;
    OutArgs: array of TValue;
    ResultValue: TValue;
    CalledMethod: SizeInt;
    InOutMapping: array of SizeInt;
    procedure Reset;
  public class var
    ProcVarInst: TTestInterfaceClass;
    ProcVarRecInst: TTestInterfaceClass;
  public const
    RecSizeMarker = SizeInt($80000000);
  end;

  TMethodTest1 = procedure of object;
  TMethodTest2 = function: SizeInt of object;
  TMethodTest3 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt of object;
  TMethodTest4 = procedure(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString) of object;
  TMethodTest5 = function: AnsiString of object;
  TMethodTest6 = function: UnicodeString of object;
  TMethodTest7 = function: WideString of object;
  TMethodTest8 = function: ShortString of object;
  TMethodTest9 = procedure(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt) of object;
  TMethodTest10 = procedure(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString) of object;
  TMethodTest11 = procedure(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString) of object;
  TMethodTest12 = procedure(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt) of object;
  TMethodTest13 = function(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single of object;
  TMethodTest14 = function(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double of object;
  TMethodTest15 = function(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended of object;
  TMethodTest16 = function(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp of object;
  TMethodTest17 = function(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency of object;
  TMethodTest18 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single of object;
  TMethodTest19 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double of object;
  TMethodTest20 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended of object;
  TMethodTest21 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp of object;
  TMethodTest22 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency of object;

  TMethodTestRecSize1 = function(aArg1: TTestRecord1): TTestRecord1 of object;
  TMethodTestRecSize2 = function(aArg1: TTestRecord2): TTestRecord2 of object;
  TMethodTestRecSize3 = function(aArg1: TTestRecord3): TTestRecord3 of object;
  TMethodTestRecSize4 = function(aArg1: TTestRecord4): TTestRecord4 of object;
  TMethodTestRecSize5 = function(aArg1: TTestRecord5): TTestRecord5 of object;
  TMethodTestRecSize6 = function(aArg1: TTestRecord6): TTestRecord6 of object;
  TMethodTestRecSize7 = function(aArg1: TTestRecord7): TTestRecord7 of object;
  TMethodTestRecSize8 = function(aArg1: TTestRecord8): TTestRecord8 of object;
  TMethodTestRecSize9 = function(aArg1: TTestRecord9): TTestRecord9 of object;
  TMethodTestRecSize10 = function(aArg1: TTestRecord10): TTestRecord10 of object;

  TMethodTestUntyped = procedure(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4) of object;

  TProcVarTest1 = procedure;
  TProcVarTest2 = function: SizeInt;
  TProcVarTest3 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
  TProcVarTest4 = procedure(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString);
  TProcVarTest5 = function: AnsiString;
  TProcVarTest6 = function: UnicodeString;
  TProcVarTest7 = function: WideString;
  TProcVarTest8 = function: ShortString;
  TProcVarTest9 = procedure(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt);
  TProcVarTest10 = procedure(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString);
  TProcVarTest11 = procedure(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString);
  TProcVarTest12 = procedure(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt);
  TProcVarTest13 = function(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single;
  TProcVarTest14 = function(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double;
  TProcVarTest15 = function(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended;
  TProcVarTest16 = function(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp;
  TProcVarTest17 = function(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency;
  TProcVarTest18 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
  TProcVarTest19 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
  TProcVarTest20 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
  TProcVarTest21 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
  TProcVarTest22 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;

  TProcVarTestRecSize1 = function(aArg1: TTestRecord1): TTestRecord1;
  TProcVarTestRecSize2 = function(aArg1: TTestRecord2): TTestRecord2;
  TProcVarTestRecSize3 = function(aArg1: TTestRecord3): TTestRecord3;
  TProcVarTestRecSize4 = function(aArg1: TTestRecord4): TTestRecord4;
  TProcVarTestRecSize5 = function(aArg1: TTestRecord5): TTestRecord5;
  TProcVarTestRecSize6 = function(aArg1: TTestRecord6): TTestRecord6;
  TProcVarTestRecSize7 = function(aArg1: TTestRecord7): TTestRecord7;
  TProcVarTestRecSize8 = function(aArg1: TTestRecord8): TTestRecord8;
  TProcVarTestRecSize9 = function(aArg1: TTestRecord9): TTestRecord9;
  TProcVarTestRecSize10 = function(aArg1: TTestRecord10): TTestRecord10;

  TProcVarTestUntyped = procedure(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);

procedure TTestInterfaceClass.Test1;
begin
  SetLength(InputArgs, 0);
  SetLength(OutputArgs, 0);
  ResultValue := TValue.Empty;
  CalledMethod := 1;
end;

function TTestInterfaceClass.Test2: SizeInt;
begin
  SetLength(InputArgs, 0);
  SetLength(OutputArgs, 0);
  Result := 42;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 2;
end;

function TTestInterfaceClass.Test3(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
begin
  SetLength(InputArgs, 10);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  TValue.Make(@aArg5, TypeInfo(aArg5), InputArgs[4]);
  TValue.Make(@aArg6, TypeInfo(aArg6), InputArgs[5]);
  TValue.Make(@aArg7, TypeInfo(aArg7), InputArgs[6]);
  TValue.Make(@aArg8, TypeInfo(aArg8), InputArgs[7]);
  TValue.Make(@aArg9, TypeInfo(aArg9), InputArgs[8]);
  TValue.Make(@aArg10, TypeInfo(aArg10), InputArgs[9]);
  SetLength(OutputArgs, 0);
  Result := 42;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 3;
end;

procedure TTestInterfaceClass.Test4(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString);
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  SetLength(OutputArgs, 0);
  ResultValue := TValue.Empty;
  CalledMethod := 4;
end;

function TTestInterfaceClass.Test5: AnsiString;
begin
  SetLength(InputArgs, 0);
  SetLength(OutputArgs, 0);
  Result := 'Hello World';
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 5;
end;

function TTestInterfaceClass.Test6: UnicodeString;
begin
  SetLength(InputArgs, 0);
  SetLength(OutputArgs, 0);
  Result := 'Hello World';
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 6;
end;

function TTestInterfaceClass.Test7: WideString;
begin
  SetLength(InputArgs, 0);
  SetLength(OutputArgs, 0);
  Result := 'Hello World';
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 7;
end;

function TTestInterfaceClass.Test8: ShortString;
begin
  SetLength(InputArgs, 0);
  SetLength(OutputArgs, 0);
  Result := 'Hello World';
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 8;
end;

procedure TTestInterfaceClass.Test9(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt);
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := $1234;
  aArg3 := $5678;
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  ResultValue := TValue.Empty;
  CalledMethod := 9;
end;

procedure TTestInterfaceClass.Test10(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString);
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := 'Foo';
  aArg3 := 'Bar';
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  ResultValue := TValue.Empty;
  CalledMethod := 10;
end;

procedure TTestInterfaceClass.Test11(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString);
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := 'Foo';
  aArg3 := 'Bar';
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  ResultValue := TValue.Empty;
  CalledMethod := 11;
end;

procedure TTestInterfaceClass.Test12(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt);
{$ifdef fpc}
var
  i: SizeInt;
  start: SizeInt;
{$endif}
begin
{$ifdef fpc}
  SetLength(InputArgs, 4);
  InputArgs[0] := specialize OpenArrayToDynArrayValue<SizeInt>(aArg1);
  InputArgs[1] := specialize OpenArrayToDynArrayValue<SizeInt>(aArg2);
  InputArgs[2] := specialize OpenArrayToDynArrayValue<SizeInt>(aArg3);
  InputArgs[3] := specialize OpenArrayToDynArrayValue<SizeInt>(aArg4);
  SetLength(OutputArgs, 2);
  start := $4321;
  for i := 0 to High(aArg2) do
    aArg2[i] := start + i;
  start := $9876;
  for i := 0 to High(aArg3) do
    aArg3[i] := start + i;
  OutputArgs[0] := specialize OpenArrayToDynArrayValue<SizeInt>(aArg2);
  OutputArgs[1] := specialize OpenArrayToDynArrayValue<SizeInt>(aArg3);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  ResultValue := TValue.Empty;
  CalledMethod := 12;
{$endif}
end;

function TTestInterfaceClass.Test13(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single;
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := SingleArg2Out;
  aArg3 := SingleArg3Out;
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  Result := SingleRes;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 13;
end;

function TTestInterfaceClass.Test14(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double;
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := DoubleArg2Out;
  aArg3 := DoubleArg3Out;
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  Result := DoubleRes;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 14;
end;

function TTestInterfaceClass.Test15(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended;
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := ExtendedArg2Out;
  aArg3 := ExtendedArg3Out;
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  Result := ExtendedRes;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 15;
end;

function TTestInterfaceClass.Test16(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp;
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := CompArg2Out;
  aArg3 := CompArg3Out;
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  Result := CompRes;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 16;
end;

function TTestInterfaceClass.Test17(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency;
begin
  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  aArg2 := CurrencyArg2Out;
  aArg3 := CurrencyArg3Out;
  SetLength(OutputArgs, 2);
  TValue.Make(@aArg2, TypeInfo(aArg2), OutputArgs[0]);
  TValue.Make(@aArg3, TypeInfo(aArg3), OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 1;
  InOutMapping[1] := 2;
  Result := CurrencyRes;
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 17;
end;

function TTestInterfaceClass.Test18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
begin
  SetLength(InputArgs, 10);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  TValue.Make(@aArg5, TypeInfo(aArg5), InputArgs[4]);
  TValue.Make(@aArg6, TypeInfo(aArg6), InputArgs[5]);
  TValue.Make(@aArg7, TypeInfo(aArg7), InputArgs[6]);
  TValue.Make(@aArg8, TypeInfo(aArg8), InputArgs[7]);
  TValue.Make(@aArg9, TypeInfo(aArg9), InputArgs[8]);
  TValue.Make(@aArg10, TypeInfo(aArg10), InputArgs[9]);
  SetLength(OutputArgs, 0);
  SetLength(InOutMapping, 0);
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6 + aArg7 + aArg8 + aArg9 + aArg10;
  TValue.Make(@Result ,TypeInfo(Result), ResultValue);
  CalledMethod := 18;
end;

function TTestInterfaceClass.Test19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
begin
  SetLength(InputArgs, 10);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  TValue.Make(@aArg5, TypeInfo(aArg5), InputArgs[4]);
  TValue.Make(@aArg6, TypeInfo(aArg6), InputArgs[5]);
  TValue.Make(@aArg7, TypeInfo(aArg7), InputArgs[6]);
  TValue.Make(@aArg8, TypeInfo(aArg8), InputArgs[7]);
  TValue.Make(@aArg9, TypeInfo(aArg9), InputArgs[8]);
  TValue.Make(@aArg10, TypeInfo(aArg10), InputArgs[9]);
  SetLength(OutputArgs, 0);
  SetLength(InOutMapping, 0);
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6 + aArg7 + aArg8 + aArg9 + aArg10;
  TValue.Make(@Result ,TypeInfo(Result), ResultValue);
  CalledMethod := 19;
end;

function TTestInterfaceClass.Test20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
begin
  SetLength(InputArgs, 10);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  TValue.Make(@aArg5, TypeInfo(aArg5), InputArgs[4]);
  TValue.Make(@aArg6, TypeInfo(aArg6), InputArgs[5]);
  TValue.Make(@aArg7, TypeInfo(aArg7), InputArgs[6]);
  TValue.Make(@aArg8, TypeInfo(aArg8), InputArgs[7]);
  TValue.Make(@aArg9, TypeInfo(aArg9), InputArgs[8]);
  TValue.Make(@aArg10, TypeInfo(aArg10), InputArgs[9]);
  SetLength(OutputArgs, 0);
  SetLength(InOutMapping, 0);
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6 + aArg7 + aArg8 + aArg9 + aArg10;
  TValue.Make(@Result ,TypeInfo(Result), ResultValue);
  CalledMethod := 20;
end;

function TTestInterfaceClass.Test21(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
begin
  SetLength(InputArgs, 10);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  TValue.Make(@aArg5, TypeInfo(aArg5), InputArgs[4]);
  TValue.Make(@aArg6, TypeInfo(aArg6), InputArgs[5]);
  TValue.Make(@aArg7, TypeInfo(aArg7), InputArgs[6]);
  TValue.Make(@aArg8, TypeInfo(aArg8), InputArgs[7]);
  TValue.Make(@aArg9, TypeInfo(aArg9), InputArgs[8]);
  TValue.Make(@aArg10, TypeInfo(aArg10), InputArgs[9]);
  SetLength(OutputArgs, 0);
  SetLength(InOutMapping, 0);
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6 + aArg7 + aArg8 + aArg9 + aArg10;
  TValue.Make(@Result ,TypeInfo(Result), ResultValue);
  CalledMethod := 21;
end;

function TTestInterfaceClass.Test22(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
begin
  SetLength(InputArgs, 10);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  TValue.Make(@aArg2, TypeInfo(aArg2), InputArgs[1]);
  TValue.Make(@aArg3, TypeInfo(aArg3), InputArgs[2]);
  TValue.Make(@aArg4, TypeInfo(aArg4), InputArgs[3]);
  TValue.Make(@aArg5, TypeInfo(aArg5), InputArgs[4]);
  TValue.Make(@aArg6, TypeInfo(aArg6), InputArgs[5]);
  TValue.Make(@aArg7, TypeInfo(aArg7), InputArgs[6]);
  TValue.Make(@aArg8, TypeInfo(aArg8), InputArgs[7]);
  TValue.Make(@aArg9, TypeInfo(aArg9), InputArgs[8]);
  TValue.Make(@aArg10, TypeInfo(aArg10), InputArgs[9]);
  SetLength(OutputArgs, 0);
  SetLength(InOutMapping, 0);
  Result := aArg1 + aArg2 + aArg3 + aArg4 + aArg5 + aArg6 + aArg7 + aArg8 + aArg9 + aArg10;
  TValue.Make(@Result ,TypeInfo(Result), ResultValue);
  CalledMethod := 22;
end;

function TTestInterfaceClass.TestRecSize1(aArg1: TTestRecord1): TTestRecord1;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 1 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize2(aArg1: TTestRecord2): TTestRecord2;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 2 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize3(aArg1: TTestRecord3): TTestRecord3;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 3 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize4(aArg1: TTestRecord4): TTestRecord4;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 4 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize5(aArg1: TTestRecord5): TTestRecord5;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 5 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize6(aArg1: TTestRecord6): TTestRecord6;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 6 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize7(aArg1: TTestRecord7): TTestRecord7;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 7 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize8(aArg1: TTestRecord8): TTestRecord8;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 8 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize9(aArg1: TTestRecord9): TTestRecord9;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 9 or RecSizeMarker;
end;

function TTestInterfaceClass.TestRecSize10(aArg1: TTestRecord10): TTestRecord10;
var
  i: LongInt;
begin
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  SetLength(OutputArgs, 0);
  for i := 0 to High(aArg1.b) do
    Result.b[High(Result.b) - i] := aArg1.b[i];
  TValue.Make(@Result, TypeInfo(Result), ResultValue);
  CalledMethod := 10 or RecSizeMarker;
end;

function TTestInterfaceClass.Test23(aArg1: Variant): AnsiString;

begin
  SetLength(OutputArgs, 0);
  SetLength(InOutMapping, 0);
  SetLength(InputArgs, 1);
  TValue.Make(@aArg1, TypeInfo(aArg1), InputArgs[0]);
  Result:=AnsiString(aArg1);
  TValue.Make(@Result ,TypeInfo(Result), ResultValue);
  CalledMethod:=23;
end;

procedure TTestInterfaceClass.TestUntyped(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);
begin
  if Length(ExpectedArgs) <> 4 then
    Exit;
  if Length(OutArgs) <> 2 then
    Exit;

  SetLength(InputArgs, 4);
  TValue.Make(@aArg1, ExpectedArgs[0].TypeInfo, InputArgs[0]);
  TValue.Make(@aArg2, ExpectedArgs[1].TypeInfo, InputArgs[1]);
  TValue.Make(@aArg3, ExpectedArgs[2].TypeInfo, InputArgs[2]);
  TValue.Make(@aArg4, ExpectedArgs[3].TypeInfo, InputArgs[3]);

  Move(PPointer(OutArgs[0].GetReferenceToRawData)^, aArg1, OutArgs[0].DataSize);
  Move(PPointer(OutArgs[1].GetReferenceToRawData)^, aArg2, OutArgs[1].DataSize);

  SetLength(OutputArgs, 2);
  TValue.Make(@aArg1, ExpectedArgs[0].TypeInfo, OutputArgs[0]);
  TValue.Make(@aArg2, ExpectedArgs[1].TypeInfo, OutputArgs[1]);
  SetLength(InOutMapping, 2);
  InOutMapping[0] := 0;
  InOutMapping[1] := 1;

  CalledMethod := -1;
end;

procedure TTestInterfaceClass.Reset;
begin
  InputArgs := Nil;
  OutputArgs := Nil;
  ExpectedArgs := Nil;
  OutArgs := Nil;
  InOutMapping := Nil;
  ResultValue := TValue.Empty;
  CalledMethod := 0;
end;

procedure ProcTest1;
begin
  TTestInterfaceClass.ProcVarInst.Test1;
end;

function ProcTest2: SizeInt;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test2;
end;

function ProcTest3(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test3(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10);
end;

procedure ProcTest4(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString);
begin
  TTestInterfaceClass.ProcVarInst.Test4(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest5: AnsiString;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test5;
end;

function ProcTest6: UnicodeString;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test6;
end;

function ProcTest7: WideString;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test7;
end;

function ProcTest8: ShortString;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test8;
end;

procedure ProcTest9(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt);
begin
  TTestInterfaceClass.ProcVarInst.Test9(aArg1, aArg2, aArg3, aArg4);
end;

procedure ProcTest10(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString);
begin
  TTestInterfaceClass.ProcVarInst.Test10(aArg1, aArg2, aArg3, aArg4);
end;

procedure ProcTest11(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString);
begin
  TTestInterfaceClass.ProcVarInst.Test11(aArg1, aArg2, aArg3, aArg4);
end;

procedure ProcTest12(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt);
begin
  TTestInterfaceClass.ProcVarInst.Test12(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest13(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test13(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest14(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test14(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest15(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test15(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest16(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test16(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest17(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test17(aArg1, aArg2, aArg3, aArg4);
end;

function ProcTest18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10);
end;

function ProcTest19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10);
end;

function ProcTest20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10);
end;

function ProcTest21(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test21(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10);
end;

function ProcTest22(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
begin
  Result := TTestInterfaceClass.ProcVarInst.Test22(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10);
end;

function ProcTestRecSize1(aArg1: TTestRecord1): TTestRecord1;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize1(aArg1);
end;

function ProcTestRecSize2(aArg1: TTestRecord2): TTestRecord2;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize2(aArg1);
end;

function ProcTestRecSize3(aArg1: TTestRecord3): TTestRecord3;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize3(aArg1);
end;

function ProcTestRecSize4(aArg1: TTestRecord4): TTestRecord4;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize4(aArg1);
end;

function ProcTestRecSize5(aArg1: TTestRecord5): TTestRecord5;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize5(aArg1);
end;

function ProcTestRecSize6(aArg1: TTestRecord6): TTestRecord6;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize6(aArg1);
end;

function ProcTestRecSize7(aArg1: TTestRecord7): TTestRecord7;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize7(aArg1);
end;

function ProcTestRecSize8(aArg1: TTestRecord8): TTestRecord8;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize8(aArg1);
end;

function ProcTestRecSize9(aArg1: TTestRecord9): TTestRecord9;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize9(aArg1);
end;

function ProcTestRecSize10(aArg1: TTestRecord10): TTestRecord10;
begin
  Result := TTestInterfaceClass.ProcVarRecInst.TestRecSize10(aArg1);
end;

procedure ProcTestUntyped(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);
begin
  TTestInterfaceClass.ProcVarInst.TestUntyped(aArg1, aArg2, aArg3, aArg4);
end;

procedure TTestInvoke.DoIntfInvoke(aIndex: SizeInt; aInputArgs,
  aOutputArgs: TValueArray; aResult: TValue);
var
  cls: TTestInterfaceClass;
  intf: ITestInterface;
  name: String;
  context: TRttiContext;
  t: TRttiType;
  inst, res: TValue;
  method: TRttiMethod;
  i: SizeInt;
  input: array of TValue;
  S : String;
begin
  cls := TTestInterfaceClass.Create;
  intf := cls;

  TValue.Make(@intf, TypeInfo(intf), inst);

  if aIndex and TTestInterfaceClass.RecSizeMarker <> 0 then
    name := 'TestRecSize' + IntToStr(aIndex and not TTestInterfaceClass.RecSizeMarker)
  else
    name := 'Test' + IntToStr(aIndex);

  context := TRttiContext.Create;
  try
    t := context.GetType(TypeInfo(ITestInterface));
    method := t.GetMethod(name);
    Check(Assigned(method), 'Method not found: ' + name);

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs));
    for i := 0 to High(input) do
      input[i] := CopyValue(aInputArgs[i]);

    try
    res := method.Invoke(inst, aInputArgs);
    except
      DumpExceptionBacktrace(output);
      raise;
    end;
    CheckEquals(aIndex, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aOutputArgs[i], aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

procedure TTestInvoke.DoMethodInvoke(aInst: TObject; aMethod: TMethod;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
var
  cls: TTestInterfaceClass;
  name: String;
  context: TRttiContext;
  t: TRttiType;
  callable, res: TValue;
  method: TRttiMethodType;
  i: SizeInt;
  input: array of TValue;
begin
  cls := aInst as TTestInterfaceClass;
  cls.Reset;

  if aIndex and TTestInterfaceClass.RecSizeMarker <> 0 then
    name := 'TestRecSize' + IntToStr(aIndex and not TTestInterfaceClass.RecSizeMarker)
  else
    name := 'Test' + IntToStr(aIndex);

  TValue.Make(@aMethod, aTypeInfo, callable);

  context := TRttiContext.Create;
  try
    t := context.GetType(aTypeInfo);
    Check(t is TRttiMethodType, 'Not a method variable: ' + aTypeInfo^.Name);
    method := t as TRttiMethodType;

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs));
    for i := 0 to High(input) do
      input[i] := CopyValue(aInputArgs[i]);

    res := method.Invoke(callable, aInputArgs);
    CheckEquals(aIndex, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aOutputArgs[i], aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

procedure TTestInvoke.DoProcVarInvoke(aInst: TObject; aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
var
  cls: TTestInterfaceClass;
  name: String;
  context: TRttiContext;
  t: TRttiType;
  callable, res: TValue;
  proc: TRttiProcedureType;
  i: SizeInt;
  input: array of TValue;
begin
  cls := aInst as TTestInterfaceClass;
  cls.Reset;

  if aIndex and TTestInterfaceClass.RecSizeMarker <> 0 then begin
    name := 'TestRecSize' + IntToStr(aIndex and not TTestInterfaceClass.RecSizeMarker);
    TTestInterfaceClass.ProcVarRecInst := cls;
  end else begin
    name := 'Test' + IntToStr(aIndex);
    TTestInterfaceClass.ProcVarInst := cls;
  end;

  TValue.Make(@aProc, aTypeInfo, callable);

  context := TRttiContext.Create;
  try
    t := context.GetType(aTypeInfo);
    Check(t is TRttiProcedureType, 'Not a procedure variable: ' + aTypeInfo^.Name);
    proc := t as TRttiProcedureType;

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs));
    for i := 0 to High(input) do
      input[i] := CopyValue(aInputArgs[i]);

    res := proc.Invoke(callable, aInputArgs);
    CheckEquals(aIndex, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aOutputArgs[i], aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

procedure TTestInvoke.DoProcInvoke(aInst: TObject; aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray;
  aResult: TValue);
var
  cls: TTestInterfaceClass;
  name: String;
  context: TRttiContext;
  t: TRttiType;
  callable, res: TValue;
  proc: TRttiProcedureType;
  i: SizeInt;
  input: array of TValue;
  restype: PTypeInfo;
begin
  cls := aInst as TTestInterfaceClass;
  cls.Reset;

  if aIndex and TTestInterfaceClass.RecSizeMarker <> 0 then begin
    name := 'TestRecSize' + IntToStr(aIndex and not TTestInterfaceClass.RecSizeMarker);
    TTestInterfaceClass.ProcVarRecInst := cls;
  end else begin
    name := 'Test' + IntToStr(aIndex);
    TTestInterfaceClass.ProcVarInst := cls;
  end;

  TValue.Make(@aProc, aTypeInfo, callable);

  context := TRttiContext.Create;
  try
    t := context.GetType(aTypeInfo);
    Check(t is TRttiProcedureType, 'Not a procedure variable: ' + aTypeInfo^.Name);
    proc := t as TRttiProcedureType;

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs));
    for i := 0 to High(input) do
      input[i] := CopyValue(aInputArgs[i]);

    if Assigned(proc.ReturnType) then
      restype := PTypeInfo(proc.ReturnType.Handle)
    else
      restype := Nil;

    res := Rtti.Invoke(aProc, aInputArgs, proc.CallingConvention, restype, True, False);
    CheckEquals(aIndex, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aOutputArgs[i], aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

procedure TTestInvoke.DoUntypedInvoke(aInst: TObject; aProc: CodePointer;
  aMethod: TMethod; aTypeInfo: PTypeInfo; aInputArgs, aOutputArgs: TValueArray;
  aResult: TValue);
var
  cls: TTestInterfaceClass;
  intf: ITestInterface;
  name: String;
  context: TRttiContext;
  t: TRttiType;
  callable, res: TValue;
  proc: TRttiInvokableType;
  method: TRttiMethod;
  i: SizeInt;
  input: array of TValue;
begin
  cls := aInst as TTestInterfaceClass;
  cls.Reset;

  name := 'TestUntyped';
  TTestInterfaceClass.ProcVarInst := cls;

  context := TRttiContext.Create;
  try
    method := Nil;
    proc := Nil;
    if Assigned(aProc) then begin
      TValue.Make(@aProc, aTypeInfo, callable);

      t := context.GetType(aTypeInfo);
      Check(t is TRttiProcedureType, 'Not a procedure variable: ' + aTypeInfo^.Name);
      proc := t as TRttiProcedureType;
    end else if Assigned(aMethod.Code) then begin
      TValue.Make(@aMethod, aTypeInfo, callable);

      t := context.GetType(aTypeInfo);
      Check(t is TRttiMethodType, 'Not a method variable: ' + aTypeInfo^.Name);
      proc := t as TRttiMethodType;
    end else begin
      intf := cls;

      TValue.Make(@intf, TypeInfo(intf), callable);

      t := context.GetType(TypeInfo(ITestInterface));
      method := t.GetMethod(name);
      Check(Assigned(method), 'Method not found: ' + name);
    end;

    { arguments might be modified by Invoke (Note: Copy() does not uniquify the
      IValueData of managed types) }
    SetLength(input, Length(aInputArgs));
    SetLength(cls.ExpectedArgs, Length(aInputArgs));
    for i := 0 to High(input) do begin
      input[i] := CopyValue(aInputArgs[i]);
      cls.ExpectedArgs[i] := CopyValue(aInputArgs[i]);
    end;
    SetLength(cls.OutArgs, Length(aOutputArgs));
    for i := 0 to High(cls.OutArgs) do begin
      cls.OutArgs[i] := CopyValue(aOutputArgs[i]);
    end;

    if Assigned(proc) then
      res := proc.Invoke(callable, aInputArgs)
    else
      res := method.Invoke(callable, aInputArgs);

    CheckEquals(-1, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aOutputArgs) do begin
      Check(EqualValues(aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aOutputArgs[i], aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

{$ifndef InLazIDE}
{$ifdef fpc}generic{$endif} procedure TTestInvoke.GenDoMethodInvoke<T>(aInst: TObject; aMethod: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
begin
  DoMethodInvoke(aInst, TMethod(aMethod), TypeInfo(T), aIndex, aInputArgs, aOutputArgs, aResult);
end;

{$ifdef fpc}generic{$endif} procedure TTestInvoke.GenDoProcVarInvoke<T>(aInst: TObject; aProc: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
begin
  DoProcVarInvoke(aInst, CodePointer(aProc), TypeInfo(T), aIndex, aInputArgs, aOutputArgs, aResult);
end;

{$ifdef fpc}generic{$endif} procedure TTestInvoke.GenDoProcInvoke<T>(aInst: TObject; aProc: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
begin
  DoProcInvoke(aInst, CodePointer(aProc), TypeInfo(T), aIndex, aInputArgs, aOutputArgs, aResult);
end;

{$ifdef fpc}generic{$endif} function TTestInvoke.GetRecValue<T>(aReverse: Boolean): TValue;
var
  i: LongInt;
  arr: array of Byte;
begin
  SetLength(arr, SizeOf(T));
  RandSeed := $54827982;
  if not aReverse then begin
    for i := 0 to High(arr) do
      arr[i] := Random($ff);
  end else begin
    for i := High(arr) downto 0 do
      arr[i] := Random($ff);
  end;
  TValue.Make(@arr[0], PTypeInfo(TypeInfo(T)), Result);
end;
{$endif}

procedure TTestInvoke.TestIntfMethods;
begin
  DoIntfInvoke(1, [], [], TValue.Empty);
  DoIntfInvoke(2, [], [], TValue.{$ifdef fpc}specialize{$endif}From<SizeInt>(42));

  DoIntfInvoke(3, [
    GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
    ], [], GetIntValue(42));

  DoIntfInvoke(4, [
    TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Alpha'),
    TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Beta'),
    TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Gamma'),
    TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Delta')
    ], [], TValue.Empty);

  DoIntfInvoke(5, [], [], TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Hello World'));
  DoIntfInvoke(6, [], [], TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Hello World'));
  DoIntfInvoke(7, [], [], TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Hello World'));
  DoIntfInvoke(8, [], [], TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Hello World'));

  DoIntfInvoke(9, [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($1234), GetIntValue($5678)
    ], TValue.Empty);

  DoIntfInvoke(10, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
    ], [
    GetAnsiString('Foo'), GetAnsiString('Bar')
    ], TValue.Empty);

  DoIntfInvoke(11, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
    ], [
    GetShortString('Foo'), GetShortString('Bar')
    ], TValue.Empty);

{$ifdef fpc}
  DoIntfInvoke(12, [
    GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
    ], [
    GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
    ], TValue.Empty);
{$endif}

  DoIntfInvoke(13, [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
    ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
    ], GetSingleValue(SingleRes));

  DoIntfInvoke(14, [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
    ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
    ], GetDoubleValue(DoubleRes));

  DoIntfInvoke(15, [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
    ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
    ], GetExtendedValue(ExtendedRes));

  DoIntfInvoke(16, [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
    ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
    ], GetCompValue(CompRes));

  DoIntfInvoke(17, [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
    ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
    ], GetCurrencyValue(CurrencyRes));

  DoIntfInvoke(18, [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
    ], [], GetSingleValue(SingleAddRes));

  DoIntfInvoke(19, [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
    ], [], GetDoubleValue(DoubleAddRes));

  DoIntfInvoke(20, [
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
    ], [], GetExtendedValue(ExtendedAddRes));

  DoIntfInvoke(21, [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
    ], [], GetCompValue(CompAddRes));

  DoIntfInvoke(22, [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
    ], [], GetCurrencyValue(CurrencyAddRes));
end;

procedure TTestInvoke.TestIntfMethodsVariant;
begin
  DoIntfInvoke(1 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(True));

end;

procedure TTestInvoke.TestIntfMethodsRecs;
begin
  DoIntfInvoke(1 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(True));

  DoIntfInvoke(2 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(True));

  DoIntfInvoke(3 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(True));

  DoIntfInvoke(4 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(True));

  DoIntfInvoke(5 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(True));

  DoIntfInvoke(6 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(True));

  DoIntfInvoke(7 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(True));

  DoIntfInvoke(8 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(True));

  DoIntfInvoke(9 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(True));

  DoIntfInvoke(10 or TTestInterfaceClass.RecSizeMarker,
    [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(False)], [],
    {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(True));
end;

procedure TTestInvoke.TestMethodVars;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest1>(cls, {$ifdef fpc}@{$endif}cls.Test1, 1, [], [], TValue.Empty);
    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest2>(cls, {$ifdef fpc}@{$endif}cls.Test2, 2, [], [], TValue.{$ifdef fpc}{$ifdef fpc}specialize{$endif}{$endif}From<SizeInt>(42));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest3>(cls, {$ifdef fpc}@{$endif}cls.Test3, 3, [
      GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
      ], [], GetIntValue(42));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest4>(cls, {$ifdef fpc}@{$endif}cls.Test4, 4, [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Alpha'),
      TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Beta'),
      TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Gamma'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Delta')
      ], [], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest5>(cls, {$ifdef fpc}@{$endif}cls.Test5, 5, [], [], TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest6>(cls, {$ifdef fpc}@{$endif}cls.Test6, 6, [], [], TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest7>(cls, {$ifdef fpc}@{$endif}cls.Test7, 7, [], [], TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest8>(cls, {$ifdef fpc}@{$endif}cls.Test8, 8, [], [], TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Hello World'));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest9>(cls, {$ifdef fpc}@{$endif}cls.Test9, 9, [
      GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
      ], [
      GetIntValue($1234), GetIntValue($5678)
      ], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest10>(cls, {$ifdef fpc}@{$endif}cls.Test10, 10, [
      GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
      ], [
      GetAnsiString('Foo'), GetAnsiString('Bar')
      ], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest11>(cls, {$ifdef fpc}@{$endif}cls.Test11, 11, [
      GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
      ], [
      GetShortString('Foo'), GetShortString('Bar')
      ], TValue.Empty);

  {$ifdef fpc}
    specialize GenDoMethodInvoke<TMethodTest12>(cls, {$ifdef fpc}@{$endif}cls.Test12, 12, [
      GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
      ], [
      GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
      ], TValue.Empty);
  {$endif}

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest13>(cls, {$ifdef fpc}@{$endif}cls.Test13, 13, [
      GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
      ], [
      GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
      ], GetSingleValue(SingleRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest14>(cls, {$ifdef fpc}@{$endif}cls.Test14, 14, [
      GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
      ], [
      GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
      ], GetDoubleValue(DoubleRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest15>(cls, {$ifdef fpc}@{$endif}cls.Test15, 15, [
      GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
      ], [
      GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
      ], GetExtendedValue(ExtendedRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest16>(cls, {$ifdef fpc}@{$endif}cls.Test16, 16, [
      GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
      ], [
      GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
      ], GetCompValue(CompRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest17>(cls, {$ifdef fpc}@{$endif}cls.Test17, 17, [
      GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
      ], [
      GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
      ], GetCurrencyValue(CurrencyRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest18>(cls, {$ifdef fpc}@{$endif}cls.Test18, 18, [
      GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
      GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
      ], [], GetSingleValue(SingleAddRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest19>(cls, {$ifdef fpc}@{$endif}cls.Test19, 19, [
      GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
      GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
      ], [], GetDoubleValue(DoubleAddRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest20>(cls, {$ifdef fpc}@{$endif}cls.Test20, 20, [
      GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
      GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
      ], [], GetExtendedValue(ExtendedAddRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest21>(cls, {$ifdef fpc}@{$endif}cls.Test21, 21, [
      GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
      GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
      ], [], GetCompValue(CompAddRes));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTest22>(cls, {$ifdef fpc}@{$endif}cls.Test22, 22, [
      GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
      GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
      ], [], GetCurrencyValue(CurrencyAddRes));
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestMethodVarsRecs;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize1>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize1, 1 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize2>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize2, 2 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize3>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize3, 3 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize4>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize4, 4 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize5>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize5, 5 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize6>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize6, 6 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize7>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize7, 7 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize8>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize8, 8 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize9>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize9, 9 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(True));

    {$ifdef fpc}specialize{$endif} GenDoMethodInvoke<TMethodTestRecSize10>(cls, {$ifdef fpc}@{$endif}cls.TestRecSize10, 10 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(True));
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestProcVars;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest1>(cls, {$ifdef fpc}@{$endif}ProcTest1, 1, [], [], TValue.Empty);
    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest2>(cls, {$ifdef fpc}@{$endif}ProcTest2, 2, [], [], TValue.{$ifdef fpc}{$ifdef fpc}specialize{$endif}{$endif}From<SizeInt>(42));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest3>(cls, {$ifdef fpc}@{$endif}ProcTest3, 3, [
      GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
      ], [], GetIntValue(42));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest4>(cls, {$ifdef fpc}@{$endif}ProcTest4, 4, [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Alpha'),
      TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Beta'),
      TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Gamma'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Delta')
      ], [], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest5>(cls, {$ifdef fpc}@{$endif}ProcTest5, 5, [], [], TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest6>(cls, {$ifdef fpc}@{$endif}ProcTest6, 6, [], [], TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest7>(cls, {$ifdef fpc}@{$endif}ProcTest7, 7, [], [], TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest8>(cls, {$ifdef fpc}@{$endif}ProcTest8, 8, [], [], TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Hello World'));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest9>(cls, {$ifdef fpc}@{$endif}ProcTest9, 9, [
      GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
      ], [
      GetIntValue($1234), GetIntValue($5678)
      ], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest10>(cls, {$ifdef fpc}@{$endif}ProcTest10, 10, [
      GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
      ], [
      GetAnsiString('Foo'), GetAnsiString('Bar')
      ], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTest11>(cls, {$ifdef fpc}@{$endif}ProcTest11, 11, [
      GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
      ], [
      GetShortString('Foo'), GetShortString('Bar')
      ], TValue.Empty);

  {$ifdef fpc}
    specialize GenDoProcVarInvoke<TProcVarTest12>(cls, {$ifdef fpc}@{$endif}ProcTest12, 12, [
      GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
      ], [
      GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
      ], TValue.Empty);
  {$endif}

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest13>(cls, {$ifdef fpc}@{$endif}ProcTest13, 13, [
      GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
      ], [
      GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
      ], GetSingleValue(SingleRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest14>(cls, {$ifdef fpc}@{$endif}ProcTest14, 14, [
      GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
      ], [
      GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
      ], GetDoubleValue(DoubleRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest15>(cls, {$ifdef fpc}@{$endif}ProcTest15, 15, [
      GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
      ], [
      GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
      ], GetExtendedValue(ExtendedRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest16>(cls, {$ifdef fpc}@{$endif}ProcTest16, 16, [
      GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
      ], [
      GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
      ], GetCompValue(CompRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest17>(cls, {$ifdef fpc}@{$endif}ProcTest17, 17, [
      GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
      ], [
      GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
      ], GetCurrencyValue(CurrencyRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest18>(cls, {$ifdef fpc}@{$endif}ProcTest18, 18, [
      GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
      GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
      ], [], GetSingleValue(SingleAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest19>(cls, {$ifdef fpc}@{$endif}ProcTest19, 19, [
      GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
      GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
      ], [], GetDoubleValue(DoubleAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest20>(cls, {$ifdef fpc}@{$endif}ProcTest20, 20, [
      GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
      GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
      ], [], GetExtendedValue(ExtendedAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest21>(cls, {$ifdef fpc}@{$endif}ProcTest21, 21, [
      GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
      GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
      ], [], GetCompValue(CompAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcvarInvoke<TProcVarTest22>(cls, {$ifdef fpc}@{$endif}ProcTest22, 22, [
      GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
      GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
      ], [], GetCurrencyValue(CurrencyAddRes));
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestProcVarsRecs;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize1>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize1, 1 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize2>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize2, 2 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize3>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize3, 3 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize4>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize4, 4 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize5>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize5, 5 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize6>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize6, 6 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize7>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize7, 7 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize8>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize8, 8 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize9>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize9, 9 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcVarInvoke<TProcVarTestRecSize10>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize10, 10 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(True));
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestProc;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest1>(cls, {$ifdef fpc}@{$endif}ProcTest1, 1, [], [], TValue.Empty);
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest2>(cls, {$ifdef fpc}@{$endif}ProcTest2, 2, [], [], TValue.{$ifdef fpc}{$ifdef fpc}specialize{$endif}{$endif}From<SizeInt>(42));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest3>(cls, {$ifdef fpc}@{$endif}ProcTest3, 3, [
      GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
      ], [], GetIntValue(42));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest4>(cls, {$ifdef fpc}@{$endif}ProcTest4, 4, [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Alpha'),
      TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Beta'),
      TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Gamma'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Delta')
      ], [], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest5>(cls, {$ifdef fpc}@{$endif}ProcTest5, 5, [], [], TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest6>(cls, {$ifdef fpc}@{$endif}ProcTest6, 6, [], [], TValue.{$ifdef fpc}specialize{$endif}From<UnicodeString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest7>(cls, {$ifdef fpc}@{$endif}ProcTest7, 7, [], [], TValue.{$ifdef fpc}specialize{$endif}From<WideString>('Hello World'));
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest8>(cls, {$ifdef fpc}@{$endif}ProcTest8, 8, [], [], TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Hello World'));

{$ifdef NEEDS_POINTER_HELPER}
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest9>(cls, {$ifdef fpc}@{$endif}ProcTest9, 9, [
      GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
      ], [
      GetIntValue($1234), GetIntValue($5678)
      ], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest10>(cls, {$ifdef fpc}@{$endif}ProcTest10, 10, [
      GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
      ], [
      GetAnsiString('Foo'), GetAnsiString('Bar')
      ], TValue.Empty);

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest11>(cls, {$ifdef fpc}@{$endif}ProcTest11, 11, [
      GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
      ], [
      GetShortString('Foo'), GetShortString('Bar')
      ], TValue.Empty);

  {$ifdef fpc}
    specialize GenDoProcInvoke<TProcVarTest12>(cls, {$ifdef fpc}@{$endif}ProcTest12, 12, [
      GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
      ], [
      GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
      ], TValue.Empty);
  {$endif}

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest13>(cls, {$ifdef fpc}@{$endif}ProcTest13, 13, [
      GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
      ], [
      GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
      ], GetSingleValue(SingleRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest14>(cls, {$ifdef fpc}@{$endif}ProcTest14, 14, [
      GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
      ], [
      GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
      ], GetDoubleValue(DoubleRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest15>(cls, {$ifdef fpc}@{$endif}ProcTest15, 15, [
      GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
      ], [
      GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
      ], GetExtendedValue(ExtendedRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest16>(cls, {$ifdef fpc}@{$endif}ProcTest16, 16, [
      GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
      ], [
      GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
      ], GetCompValue(CompRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest17>(cls, {$ifdef fpc}@{$endif}ProcTest17, 17, [
      GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
      ], [
      GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
      ], GetCurrencyValue(CurrencyRes));
{$endif NEEDS_POINTER_HELPER}

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest18>(cls, {$ifdef fpc}@{$endif}ProcTest18, 18, [
      GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
      GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
      ], [], GetSingleValue(SingleAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest19>(cls, {$ifdef fpc}@{$endif}ProcTest19, 19, [
      GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
      GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
      ], [], GetDoubleValue(DoubleAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest20>(cls, {$ifdef fpc}@{$endif}ProcTest20, 20, [
      GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
      GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
      ], [], GetExtendedValue(ExtendedAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest21>(cls, {$ifdef fpc}@{$endif}ProcTest21, 21, [
      GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
      GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
      ], [], GetCompValue(CompAddRes));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTest22>(cls, {$ifdef fpc}@{$endif}ProcTest22, 22, [
      GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
      GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
      ], [], GetCurrencyValue(CurrencyAddRes));
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestProcRecs;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize1>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize1, 1 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord1>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize2>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize2, 2 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord2>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize3>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize3, 3 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord3>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize4>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize4, 4 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord4>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize5>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize5, 5 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord5>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize6>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize6, 6 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord6>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize7>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize7, 7 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord7>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize8>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize8, 8 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord8>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize9>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize9, 9 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord9>(True));

    {$ifdef fpc}specialize{$endif} GenDoProcInvoke<TProcVarTestRecSize10>(cls, {$ifdef fpc}@{$endif}ProcTestRecSize10, 10 or TTestInterfaceClass.RecSizeMarker,
      [{$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(False)], [],
      {$ifdef fpc}specialize{$endif} GetRecValue<TTestRecord10>(True));
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestUntyped;
var
  cls: TTestInterfaceClass;
begin
  cls := TTestInterfaceClass.Create;
  try
    cls._AddRef;

    DoUntypedInvoke(cls, Nil, Default(TMethod), Nil, [
      GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
      ], [
      GetIntValue($4321), GetIntValue($5678)
      ], TValue.Empty);

    DoUntypedInvoke(cls, Nil, Default(TMethod), Nil, [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str1'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str2'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str3'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str4')
      ], [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('StrVar'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('StrOut')
      ], TValue.Empty);

    DoUntypedInvoke(cls, Nil, Default(TMethod), Nil, [
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str1'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str2'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str3'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str4')
      ], [
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('StrVar'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('StrOut')
      ], TValue.Empty);

    DoUntypedInvoke(cls, Nil, TMethod({$ifdef fpc}@{$endif}cls.TestUntyped), TypeInfo(TMethodTestUntyped), [
      GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
      ], [
      GetIntValue($4321), GetIntValue($5678)
      ], TValue.Empty);

    DoUntypedInvoke(cls, Nil, TMethod({$ifdef fpc}@{$endif}cls.TestUntyped), TypeInfo(TMethodTestUntyped), [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str1'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str2'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str3'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str4')
      ], [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('StrVar'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('StrOut')
      ], TValue.Empty);

    DoUntypedInvoke(cls, Nil, TMethod({$ifdef fpc}@{$endif}cls.TestUntyped), TypeInfo(TMethodTestUntyped), [
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str1'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str2'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str3'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str4')
      ], [
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('StrVar'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('StrOut')
      ], TValue.Empty);

    DoUntypedInvoke(cls, {$ifdef fpc}@{$endif}ProcTestUntyped, Default(TMethod), TypeInfo(TProcVarTestUntyped), [
      GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
      ], [
      GetIntValue($4321), GetIntValue($5678)
      ], TValue.Empty);

    DoUntypedInvoke(cls, {$ifdef fpc}@{$endif}ProcTestUntyped, Default(TMethod), TypeInfo(TProcVarTestUntyped), [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str1'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str2'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str3'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('Str4')
      ], [
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('StrVar'),
      TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>('StrOut')
      ], TValue.Empty);

    DoUntypedInvoke(cls, {$ifdef fpc}@{$endif}ProcTestUntyped, Default(TMethod), TypeInfo(TProcVarTestUntyped), [
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str1'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str2'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str3'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('Str4')
      ], [
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('StrVar'),
      TValue.{$ifdef fpc}specialize{$endif}From<ShortString>('StrOut')
      ], TValue.Empty);
  finally
    cls._Release;
  end;
end;

begin
{$ifdef fpc}
  RegisterTest(TTestInvoke);
{$else fpc}
  RegisterTest(TTestInvoke.Suite);
{$endif fpc}
end.

