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
{$ifndef fpc}
  CodePointer = Pointer;
{$endif}

  TTestInvoke = class(TTestCase)
  private type
    TInvokeFlag = (
      ifStatic,
      ifConstructor
    );
    TInvokeFlags = set of TInvokeFlag;
  private
    function EqualValues(aValue1, aValue2: TValue): Boolean;

    function DoInvoke(aCodeAddress: CodePointer; aArgs: TValueArray; aCallConv: TCallConv; aResultType: PTypeInfo; aFlags: TInvokeFlags; out aValid: Boolean): TValue;
    procedure DoStaticInvokeTestOrdinalCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: Int64);
    procedure DoStaticInvokeTestAnsiStringCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: AnsiString);
    procedure DoStaticInvokeTestUnicodeStringCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: UnicodeString);
    procedure DoIntfInvoke(aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoMethodInvoke(aInst: TObject; aMethod: TMethod; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoProcVarInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
{$ifndef InLazIDE}
    {$ifdef fpc}generic{$endif} procedure GenDoMethodInvoke<T>(aInst: TObject; aMethod: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    {$ifdef fpc}generic{$endif} procedure GenDoProcvarInvoke<T>(aInst: TObject; aProc: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
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

    procedure TestLongInt;
    procedure TestInt64;

    procedure TestTObject;

    procedure TestIntfMethods;
    procedure TestIntfMethodsRecs;

    procedure TestMethodVars;
    procedure TestMethodVarsRecs;

    procedure TestProcVars;
    procedure TestProcVarsRecs;
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

function TTestInvoke.EqualValues(aValue1, aValue2: TValue): Boolean;
var
  td1, td2: PTypeData;
  i: SizeInt;
begin
{$ifdef debug}
  Writeln('Empty: ', aValue1.IsEmpty, ' ', aValue2.IsEmpty);
  Writeln('Kind: ', aValue1.Kind, ' ', aValue2.Kind);
  Writeln('Array: ', aValue1.IsArray, ' ', aValue2.IsArray);
{$endif}
  if aValue1.IsEmpty and aValue2.IsEmpty then
    Result := True
  else if aValue1.IsEmpty and not aValue2.IsEmpty then
    Result := False
  else if not aValue1.IsEmpty and aValue2.IsEmpty then
    Result := False
  else if aValue1.IsArray and aValue2.IsArray then begin
    if aValue1.GetArrayLength = aValue2.GetArrayLength then begin
      Result := True;
      for i := 0 to aValue1.GetArrayLength - 1 do
        if not EqualValues(aValue1.GetArrayElement(i), aValue2.GetArrayElement(i)) then begin
          Writeln('Element ', i, ' differs: ', HexStr(aValue1.GetArrayElement(i).AsOrdinal, 4), ' ', HexStr(aValue2.GetArrayElement(i).AsOrdinal, 4));
          Result := False;
          Break;
        end;
    end else
      Result := False;
  end else if aValue1.Kind = aValue2.Kind then begin
    td1 := aValue1.TypeData;
    td2 := aValue2.TypeData;
    case aValue1.Kind of
      tkBool:
        Result := aValue1.AsBoolean xor not aValue2.AsBoolean;
      tkSet:
        if td1^.SetSize = td2^.SetSize then
          if td1^.SetSize < SizeOf(SizeInt) then
            Result := aValue1.AsOrdinal = aValue2.AsOrdinal
          else
            Result := CompareMem(aValue1.GetReferenceToRawData, aValue2.GetReferenceToRawData, td1^.SetSize)
        else
          Result := False;
      tkEnumeration,
      tkChar,
      tkWChar,
      tkUChar,
      tkInt64,
      tkInteger:
        Result := aValue1.AsOrdinal = aValue2.AsOrdinal;
      tkQWord:
        Result := aValue1.AsUInt64 = aValue2.AsUInt64;
      tkSString,
      tkUString,
      tkAString,
      tkWString:
        Result := aValue1.AsString = aValue2.AsString;
      tkDynArray,
      tkArray:
        if aValue1.GetArrayLength = aValue2.GetArrayLength then begin
          Result := True;
          for i := 0 to aValue1.GetArrayLength - 1 do
            if not EqualValues(aValue1.GetArrayElement(i), aValue2.GetArrayElement(i)) then begin
              Result := False;
              Break;
            end;
        end else
          Result := False;
      tkClass,
      tkClassRef,
      tkInterface,
      tkInterfaceRaw,
      tkPointer:
        Result := PPointer(aValue1.GetReferenceToRawData)^ = PPointer(aValue2.GetReferenceToRawData)^;
      tkProcVar:
        Result := PCodePointer(aValue1.GetReferenceToRawData)^ = PCodePointer(aValue2.GetReferenceToRawData)^;
      tkRecord,
      tkObject,
      tkMethod,
      tkVariant: begin
        if aValue1.DataSize = aValue2.DataSize then
          Result := CompareMem(aValue1.GetReferenceToRawData, aValue2.GetReferenceToRawData, aValue1.DataSize)
        else
          Result := False;
      end
      else
        Result := False;
    end;
  end else
    Result := False;
end;

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
  public
    InputArgs: array of TValue;
    OutputArgs: array of TValue;
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

procedure TTestInterfaceClass.Reset;
begin
  InputArgs := Nil;
  OutputArgs := Nil;
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
  Writeln('Result @ ', HexStr(@Result));
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

function CopyValue({$ifdef fpc}constref{$else}const [ref]{$endif} aValue: TValue): TValue;
var
  arrptr: Pointer;
  len, i: SizeInt;
begin
  if aValue.Kind = tkDynArray then begin
    { we need to decouple the source reference, so we're going to be a bit
      cheeky here }
    len := aValue.GetArrayLength;
    arrptr := Nil;
    DynArraySetLength(arrptr, aValue.TypeInfo, 1, @len);
    TValue.Make(@arrptr, aValue.TypeInfo, Result);
    for i := 0 to len - 1 do
      Result.SetArrayElement(i, aValue.GetArrayElement(i));
  end else
    TValue.Make(aValue.GetReferenceToRawData, aValue.TypeInfo, Result);
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

    writeln('calling ', name);
    res := proc.Invoke(callable, aInputArgs);
    writeln('called ', name);
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

{$ifndef InLazIDE}
{$ifdef fpc}generic{$endif} procedure TTestInvoke.GenDoMethodInvoke<T>(aInst: TObject; aMethod: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
begin
  DoMethodInvoke(aInst, TMethod(aMethod), TypeInfo(T), aIndex, aInputArgs, aOutputArgs, aResult);
end;

{$ifdef fpc}generic{$endif} procedure TTestInvoke.GenDoProcVarInvoke<T>(aInst: TObject; aProc: T; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
begin
  DoProcVarInvoke(aInst, CodePointer(aProc), TypeInfo(T), aIndex, aInputArgs, aOutputArgs, aResult);
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

function GetIntValue(aValue: SizeInt): TValue;
begin
  Result := TValue.{$ifdef fpc}specialize{$endif}From<SizeInt>(aValue);
end;

function GetAnsiString(const aValue: AnsiString): TValue;
begin
  Result := TValue.{$ifdef fpc}specialize{$endif}From<AnsiString>(aValue);
end;

function GetShortString(const aValue: ShortString): TValue;
begin
  Result := TValue.{$ifdef fpc}specialize{$endif}From<ShortString>(aValue);
end;

{$ifdef fpc}
function GetArray(const aArg: array of SizeInt): TValue;
begin
  Result := specialize OpenArrayToDynArrayValue<SizeInt>(aArg);
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
  finally
    cls.Free;
  end;
end;

procedure TTestInvoke.TestProcVarsRecs;
var
  cls: TTestInterfaceClass;
begin
  try
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
  except
    DumpExceptionBacktrace(output);
    raise;
  end;
end;

begin
{$ifdef fpc}
  RegisterTest(TTestInvoke);
{$else fpc}
  RegisterTest(TTestInvoke.Suite);
{$endif fpc}
end.

