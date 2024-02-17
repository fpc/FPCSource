unit tests.rtti.invoke;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

{.$define debug}

interface

uses
{$IFDEF FPC}
  fpcunit,testregistry,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  sysutils, typinfo, Rtti,
  tests.rtti.invoketypes,
  Tests.Rtti.Util;

type

  TProcArgs = record
    aInputArgs,
    aOutputArgs: TValueArray;
    aResult: TValue;
  end;

  { TTestInvokeBase }

  TTestInvokeBase = class(TTestCase)
  private type
    TInvokeFlag = (
      ifStatic,
      ifConstructor
    );
    TInvokeFlags = set of TInvokeFlag;
  private
    function DoInvoke(aCodeAddress: CodePointer; aArgs: TValueArray; aCallConv: TCallConv; aResultType: PTypeInfo; aFlags: TInvokeFlags; out aValid: Boolean): TValue;
    procedure DoStaticInvokeTestVariant(const aTestName: String;  aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: String);
    procedure DoStaticInvokeTestOrdinalCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: Int64);
    procedure DoStaticInvokeTestAnsiStringCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: AnsiString);
    procedure DoStaticInvokeTestUnicodeStringCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; constref aResult: UnicodeString);
    procedure DoIntfInvoke(aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoMethodInvoke(aInst: TObject; aMethod: TMethod; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
    procedure DoProcVarInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; const aInputArgs, aOutputArgs: TValueArray; aResult: TValue);overload;
    procedure DoProcVarInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; aData : TProcArgs);overload;
    procedure DoProcInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; aData : TProcArgs); overload;
    procedure DoProcInvoke(aInst: TObject; aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; aInputArgs, aOutputArgs: TValueArray; aResult: TValue);overload;
    procedure DoUntypedInvoke(aInst: TObject; aProc: CodePointer; aMethod: TMethod; aTypeInfo: PTypeInfo; aData : TProcArgs); overload;
    procedure DoUntypedInvoke(aInst: TObject; aProc: CodePointer; aMethod: TMethod; aTypeInfo: PTypeInfo; aInputArgs, aOutputArgs: TValueArray; aResult: TValue); overload;

    function GetRecValue(aTypeInfo : PTypeInfo; aSize : integer; aReverse: Boolean): TValue;
  end;

  { TTestInvoke }

  TTestInvoke = class(TTestInvokeBase)
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
  end;

  { TTestInvokeIntfMethods }

  TTestInvokeIntfMethods = class(TTestInvokeBase)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
    Procedure Test11;
    Procedure Test12;
    Procedure Test13;
    Procedure Test14;
    Procedure Test15;
    Procedure Test16;
    Procedure Test17;
    Procedure Test18;
    Procedure Test19;
    Procedure Test20;
    Procedure Test21;
    Procedure Test22;
  end;

  { TTestInvokeIntfMethodsRecs }

  TTestInvokeIntfMethodsRecs = class(TTestInvokeBase)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
  end;

  { TTestInvokeMethodVars }

  TTestInvokeMethodTests = class(TTestInvokeBase)
  protected
    cls: TTestInterfaceClass;
    procedure DoProcVarInvoke(aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; const aInputArgs, aOutputArgs: TValueArray; aResult: TValue); overload;
    procedure DoProcInvoke(aProc: CodePointer; aTypeInfo: PTypeInfo; aIndex: SizeInt; const aInputArgs, aOutputArgs: TValueArray; aResult: TValue); overload;
    procedure DoUntypedInvoke(aProc: CodePointer; aMethod: TMethod; aTypeInfo: PTypeInfo; const aInputArgs, aOutputArgs: TValueArray); overload;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
  end;


  TTestInvokeMethodVars = class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
    Procedure Test11;
    Procedure Test12;
    Procedure Test13;
    Procedure Test14;
    Procedure Test15;
    Procedure Test16;
    Procedure Test17;
    Procedure Test18;
    Procedure Test19;
    Procedure Test20;
    Procedure Test21;
    Procedure Test22;
  end;

  { TTestInvokeMethodVarsRecs }

  TTestInvokeMethodVarsRecs = class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
  end;

  { TTestInvokeProcVars }

  TTestInvokeProcVars = class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
    Procedure Test11;
    Procedure Test12;
    Procedure Test13;
    Procedure Test14;
    Procedure Test15;
    Procedure Test16;
    Procedure Test17;
    Procedure Test18;
    Procedure Test19;
    Procedure Test20;
    Procedure Test21;
    Procedure Test22;
  end;

  { TTestInvokeProcVarRecs }

  TTestInvokeProcVarRecs = class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
  end;
  { TTestInvokeTestProc }

  TTestInvokeTestProc = Class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    {$ifdef NEEDS_POINTER_HELPER}
    Procedure Test9;
    Procedure Test10;
    Procedure Test11;
    Procedure Test12;
    Procedure Test13;
    Procedure Test14;
    Procedure Test15;
    Procedure Test16;
    Procedure Test17;
    {$ENDIF}
    Procedure Test18;
    Procedure Test19;
    Procedure Test20;
    Procedure Test21;
    Procedure Test22;
  end;

  { TTestInvokeTestProcRecs }

  TTestInvokeTestProcRecs = Class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
    Procedure Test10;
  end;

  { TTestInvokeUntyped }

  TTestInvokeUntyped = Class(TTestInvokeMethodTests)
  Published
    Procedure Test1;
    Procedure Test2;
    Procedure Test3;
    Procedure Test4;
    Procedure Test5;
    Procedure Test6;
    Procedure Test7;
    Procedure Test8;
    Procedure Test9;
  end;

implementation

{ ----------------------------------------------------------------------
  Auxiliary methods to test
  ----------------------------------------------------------------------}

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

// Shortstring parameters

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

// Ansistring parameters

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

// Widestring parameters

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

// Unicode parameters

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

// Longint parameters

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

// class parameters

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

// Variant parameters

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

// Int64 parameters

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


{ ----------------------------------------------------------------------
  TTestInvokeBase
  ----------------------------------------------------------------------}


function TTestInvokeBase.DoInvoke(aCodeAddress: CodePointer; aArgs: TValueArray;
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

procedure TTestInvokeBase.DoStaticInvokeTestOrdinalCompare(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: Int64);
var
  resval: TValue;
  valid: Boolean;
begin
  resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
  if valid and Assigned(aReturnType) and (resval.AsOrdinal <> aResult) then begin
    Fail('Result of test "%s" is unexpected; expected: %s, got: %s', [aTestName, IntToStr(aResult), IntToStr(resval.AsOrdinal)]);
  end;
end;


procedure TTestInvokeBase.DoStaticInvokeTestVariant(const aTestName: String; aAddress: CodePointer; aCallConv: TCallConv; aValues: TValueArray; aReturnType: PTypeInfo; aResult: String);
var
  resval: TValue;
  valid: Boolean;
begin
  resval := DoInvoke(aAddress, aValues, aCallConv, aReturnType, [ifStatic], valid);
  if valid and (resval.AsAnsiString <> aResult) then begin
    Fail('Result of test "%s" is unexpected; expected: %s, got: %s', [aTestName, aResult, String(resval.AsAnsiString)]);
  end;
end;


procedure TTestInvokeBase.DoStaticInvokeTestAnsiStringCompare(
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

procedure TTestInvokeBase.DoStaticInvokeTestUnicodeStringCompare(
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




procedure TTestInvokeBase.DoIntfInvoke(aIndex: SizeInt; aInputArgs,
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
  input:=Nil;
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

procedure TTestInvokeBase.DoMethodInvoke(aInst: TObject; aMethod: TMethod;
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
  input:=Nil;
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

procedure TTestInvokeBase.DoProcVarInvoke(aInst: TObject; aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; const aInputArgs, aOutputArgs: TValueArray; aResult: TValue);
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
  input:=Nil;
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


procedure TTestInvokeBase.DoProcVarInvoke(aInst: TObject; aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; aData: TProcArgs);
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
  input:=Nil;
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
    SetLength(input, Length(aData.aInputArgs));
    for i := 0 to High(input) do
      input[i] := CopyValue(aData.aInputArgs[i]);

    res := proc.Invoke(callable, aData.aInputArgs);
    CheckEquals(aIndex, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aData.aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aData.aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aData.aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aData.aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aData.aOutputArgs) do begin
      Check(EqualValues(aData.aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aData.aOutputArgs[i], aData.aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;

procedure TTestInvokeBase.DoProcInvoke(aInst: TObject; aProc: CodePointer;
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
  input:=Nil;
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

procedure TTestInvokeBase.DoProcInvoke(aInst: TObject; aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; aData : TProcArgs);
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
  input:=Nil;
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
    SetLength(input, Length(aData.aInputArgs));
    for i := 0 to High(input) do
      input[i] := CopyValue(aData.aInputArgs[i]);

    if Assigned(proc.ReturnType) then
      restype := PTypeInfo(proc.ReturnType.Handle)
    else
      restype := Nil;

    res := Rtti.Invoke(aProc, aData.aInputArgs, proc.CallingConvention, restype, True, False);
    CheckEquals(aIndex, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aData.aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aData.aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aData.aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aData.aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aData.aOutputArgs) do begin
      Check(EqualValues(aData.aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aData.aOutputArgs[i], aData.aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;


procedure TTestInvokeBase.DoUntypedInvoke(aInst: TObject; aProc: CodePointer;
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
  input:=Nil;
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

procedure TTestInvokeBase.DoUntypedInvoke(aInst: TObject; aProc: CodePointer;
  aMethod: TMethod; aTypeInfo: PTypeInfo; aData : TProcArgs);
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
  input:=Nil;
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
    SetLength(input, Length(aData.aInputArgs));
    SetLength(cls.ExpectedArgs, Length(aData.aInputArgs));
    for i := 0 to High(input) do begin
      input[i] := CopyValue(aData.aInputArgs[i]);
      cls.ExpectedArgs[i] := CopyValue(aData.aInputArgs[i]);
    end;
    SetLength(cls.OutArgs, Length(aData.aOutputArgs));
    for i := 0 to High(cls.OutArgs) do begin
      cls.OutArgs[i] := CopyValue(aData.aOutputArgs[i]);
    end;

    if Assigned(proc) then
      res := proc.Invoke(callable, aData.aInputArgs)
    else
      res := method.Invoke(callable, aData.aInputArgs);

    CheckEquals(-1, cls.CalledMethod, 'Wrong method called for ' + name);
    Check(EqualValues(cls.ResultValue, res), 'Reported result value differs from returned for ' + name);
    Check(EqualValues(aData.aResult, res), 'Expected result value differs from returned for ' + name);
    CheckEquals(Length(aData.aInputArgs), Length(cls.InputArgs), 'Count of input args differs for ' + name);
    CheckEquals(Length(cls.OutputArgs), Length(cls.InOutMapping), 'Count of output args and in-out-mapping differs for ' + name);
    CheckEquals(Length(aData.aOutputArgs), Length(cls.OutputArgs), 'Count of output args differs for ' + name);
    for i := 0 to High(aData.aInputArgs) do begin
      Check(EqualValues(input[i], cls.InputArgs[i]), Format('Input argument %d differs for %s', [i + 1, name]));
    end;
    for i := 0 to High(aData.aOutputArgs) do begin
      Check(EqualValues(aData.aOutputArgs[i], cls.OutputArgs[i]), Format('Output argument %d differs for %s', [i + 1, name]));
      Check(EqualValues(aData.aOutputArgs[i], aData.aInputArgs[cls.InOutMapping[i]]), Format('New output argument %d differs from expected output for %s', [i + 1, name]));
    end;
  finally
    context.Free;
  end;
end;


function TTestInvokeBase.GetRecValue(aTypeInfo: PTypeInfo; aSize: integer;
  aReverse: Boolean): TValue;
var
  i: LongInt;
  arr: array of Byte;
begin
  Arr:=nil;
  SetLength(arr, aSize);
  RandSeed := $54827982;
  if not aReverse then begin
    for i := 0 to High(arr) do
      arr[i] := Random($ff);
  end else begin
    for i := High(arr) downto 0 do
      arr[i] := Random($ff);
  end;
  TValue.Make(@arr[0], aTypeInfo, Result);
end;

{ ----------------------------------------------------------------------
  TTestInvoke
  ----------------------------------------------------------------------}

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
  values:=Nil;
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
  values:=nil;
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
  values:=nil;
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
  values:=nil;
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
  values:=nil;
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
  values:=nil;
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
  Values:=[];
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
  Values:=[];
  SetLength(Values,1);
  S:='A nice string';
  UniqueString(S);
  aValue:=S;
  aResult:=Default(TValue);
  TValue.Make(@S, TypeInfo(AnsiString), aResult);
  TValue.Make(@aValue, TypeInfo(Variant), Values[0]);
  DoIntfInvoke(23,Values,aOutput,aResult);
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
  values:=nil;
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

{ ----------------------------------------------------------------------
  TTestInvokeMethodTests
  ----------------------------------------------------------------------}


procedure TTestInvokeMethodTests.DoProcVarInvoke(aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; const aInputArgs,
  aOutputArgs: TValueArray; aResult: TValue);
begin
  CheckNotNull(Cls,'Have class');
  DoProcVarInvoke(cls,aProc,aTypeInfo,aIndex,aInputArgs,aOutputArgs,aResult);
end;

procedure TTestInvokeMethodTests.DoProcInvoke(aProc: CodePointer;
  aTypeInfo: PTypeInfo; aIndex: SizeInt; const aInputArgs,
  aOutputArgs: TValueArray; aResult: TValue);

begin
  CheckNotNull(Cls,'Have class');
  DoProcVarInvoke(cls,aProc,aTypeInfo,aIndex,aInputArgs,aOutputArgs,aResult);
end;

procedure TTestInvokeMethodTests.DoUntypedInvoke(aProc: CodePointer;
  aMethod: TMethod; aTypeInfo: PTypeInfo; const aInputArgs,
  aOutputArgs: TValueArray);
begin
  CheckNotNull(Cls,'Have class');
  DoUntypedInvoke(cls,aProc,aMethod,aTypeInfo,aInputArgs,aOutputArgs,TValue.Empty);
end;

procedure TTestInvokeMethodTests.SetUp;
begin
  inherited SetUp;
  cls := TTestInterfaceClass.Create;
  cls.DoAddRef;
end;

procedure TTestInvokeMethodTests.TearDown;
begin
  cls.DoRelease;
  inherited TearDown;
end;

{ ----------------------------------------------------------------------
  TTestInvokeIntfMethods
  ----------------------------------------------------------------------}


procedure TTestInvokeIntfMethods.Test1;
 begin
   DoIntfInvoke(1, [], [], TValue.Empty);
end;

procedure TTestInvokeIntfMethods.Test2;
begin
  DoIntfInvoke(2, [], [], GetIntValue(42));
end;

procedure TTestInvokeIntfMethods.Test3;
begin
   DoIntfInvoke(3, [
     GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
     ], [], GetIntValue(42));
end;

procedure TTestInvokeIntfMethods.Test4;
begin
   DoIntfInvoke(4, [
    GetAnsiString('Alpha'),
    GetUnicodeString('Beta'),
    GetWideString('Gamma'),
    GetShortString('Delta')
     ], [], TValue.Empty);
end;

procedure TTestInvokeIntfMethods.Test5;
begin
  DoIntfInvoke(5, [], [], GetAnsiString('Hello World'));
end;

procedure TTestInvokeIntfMethods.Test6;
begin
  DoIntfInvoke(6, [], [], GetUnicodeString('Hello World'));
end;

procedure TTestInvokeIntfMethods.Test7;
begin
  DoIntfInvoke(7, [], [], GetWideString('Hello World'));
end;

procedure TTestInvokeIntfMethods.Test8;
begin
  DoIntfInvoke(8, [], [], GetShortString('Hello World'));
end;

procedure TTestInvokeIntfMethods.Test9;
begin
   DoIntfInvoke(9, [
     GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
     ], [
     GetIntValue($1234), GetIntValue($5678)
     ], TValue.Empty);
end;

procedure TTestInvokeIntfMethods.Test10;
begin
   DoIntfInvoke(10, [
     GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
     ], [
     GetAnsiString('Foo'), GetAnsiString('Bar')
     ], TValue.Empty);
end;

procedure TTestInvokeIntfMethods.Test11;
begin
   DoIntfInvoke(11, [
     GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
     ], [
     GetShortString('Foo'), GetShortString('Bar')
     ], TValue.Empty);
end;

procedure TTestInvokeIntfMethods.Test12;
begin
  {$ifdef fpc}
    DoIntfInvoke(12, [
      GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
      ], [
      GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
      ], TValue.Empty);
  {$endif}
end;

procedure TTestInvokeIntfMethods.Test13;
begin
   DoIntfInvoke(13, [
     GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
     ], [
     GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
     ], GetSingleValue(SingleRes));
end;

procedure TTestInvokeIntfMethods.Test14;
begin
   DoIntfInvoke(14, [
     GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
     ], [
     GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
     ], GetDoubleValue(DoubleRes));
end;

procedure TTestInvokeIntfMethods.Test15;
begin
   DoIntfInvoke(15, [
     GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
     ], [
     GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
     ], GetExtendedValue(ExtendedRes));
end;

procedure TTestInvokeIntfMethods.Test16;
begin
   DoIntfInvoke(16, [
     GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
     ], [
     GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
     ], GetCompValue(CompRes));
end;

procedure TTestInvokeIntfMethods.Test17;
begin
   DoIntfInvoke(17, [
     GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
     ], [
     GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
     ], GetCurrencyValue(CurrencyRes));
end;

procedure TTestInvokeIntfMethods.Test18;
begin
   DoIntfInvoke(18, [
     GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
     GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
     ], [], GetSingleValue(SingleAddRes));
end;

procedure TTestInvokeIntfMethods.Test19;
begin
   DoIntfInvoke(19, [
     GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
     GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
     ], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestInvokeIntfMethods.Test20;
begin
   DoIntfInvoke(20, [
     GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
     GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
     ], [], GetExtendedValue(ExtendedAddRes));
end;

procedure TTestInvokeIntfMethods.Test21;
begin
   DoIntfInvoke(21, [
     GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
     GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
     ], [], GetCompValue(CompAddRes));
end;

procedure TTestInvokeIntfMethods.Test22;
begin
   DoIntfInvoke(22, [
     GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
     GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
  ], [], GetCurrencyValue(CurrencyAddRes));
end;



{ ----------------------------------------------------------------------
  TTestInvokeIntfMethodsRecs
  ----------------------------------------------------------------------}


procedure TTestInvokeIntfMethodsRecs.Test1;
 begin
   DoIntfInvoke(1 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),False)], [],
     GetRecValue(TypeInfo(TTestRecord1),Sizeof(TTestrecord1),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test2;
begin
   DoIntfInvoke(2 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),False)], [],
     GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test3;
begin
   DoIntfInvoke(3 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),False)], [],
     GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test4;
begin
   DoIntfInvoke(4 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),False)], [],
     GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test5;
begin
   DoIntfInvoke(5 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),False)], [],
     GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test6;
begin
   DoIntfInvoke(6 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),False)], [],
     GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test7;
begin
   DoIntfInvoke(7 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),False)], [],
      GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test8;
begin
   DoIntfInvoke(8 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),False)], [],
     GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test9;
begin
   DoIntfInvoke(9 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),False)], [],
     GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),True));
end;

procedure TTestInvokeIntfMethodsRecs.Test10;
begin
   DoIntfInvoke(10 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),False)], [],
     GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),True));
end;


{ ----------------------------------------------------------------------
  TTestInvokeMethodVars
  ----------------------------------------------------------------------}

procedure TTestInvokeMethodVars.Test1;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test1), TypeInfo(TMethodTest1),1,[], [], TValue.empty);
end;

procedure TTestInvokeMethodVars.Test2;
 begin
  DoMethodInvoke(cls,TMethod({$ifdef fpc}@{$endif}cls.Test2), TypeInfo(TMethodTest2), 2, [], [], GetIntValue(42));
end;

procedure TTestInvokeMethodVars.Test3;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test3), TypeInfo(TMethodTest3), 3, [
    GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
    ], [], GetIntValue(42));
end;

procedure TTestInvokeMethodVars.Test4;
begin
  DoMethodInvoke(cls,
    TMethod({$ifdef fpc}@{$endif}cls.Test4),TypeInfo(TMethodTest4), 4, [
    GetAnsiString('Alpha'),
    GetUnicodeString('Beta'),
    GetWideString('Gamma'),
    GetShortString('Delta')
    ], [], TValue.Empty);
end;

procedure TTestInvokeMethodVars.Test5;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test5),TYpeInfo(TMethodTest5), 5, [], [], GetAnsiString('Hello World'));
end;

procedure TTestInvokeMethodVars.Test6;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test6),TypeInfo(TMethodTest6), 6, [], [], GetUnicodeString('Hello World'));
end;

procedure TTestInvokeMethodVars.Test7;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test7),TypeInfo(TMethodTest7), 7, [], [], GetWideString('Hello World'));
end;

procedure TTestInvokeMethodVars.Test8;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test8),TypeInfo(TMethodTest8), 8, [], [], GetShortString('Hello World'));
end;

procedure TTestInvokeMethodVars.Test9;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test9),TypeInfo(TMethodTest9), 9, [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($1234), GetIntValue($5678)
    ], TValue.Empty);
end;

procedure TTestInvokeMethodVars.Test10;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test10),TypeInfo(TMethodTest10), 10, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
    ], [
    GetAnsiString('Foo'), GetAnsiString('Bar')
    ], TValue.Empty);
end;

procedure TTestInvokeMethodVars.Test11;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test11),TypeInfo(TMethodTest11), 11, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
    ], [
    GetShortString('Foo'), GetShortString('Bar')
    ], TValue.Empty);
end;

procedure TTestInvokeMethodVars.Test12;
begin
   {$ifdef fpc}
    DoMethodInvoke(cls,TMethod(@cls.Test12),TypeInfo(TMethodTest12), 12, [
       GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
       ], [
       GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
       ], TValue.Empty);
   {$endif}
end;


procedure TTestInvokeMethodVars.Test13;
begin
   DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test13), Typeinfo(TMethodTest13), 13, [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
    ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
    ], GetSingleValue(SingleRes));
end;

procedure TTestInvokeMethodVars.Test14;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test14), TypeInfo(TMethodTest14), 14, [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
    ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
    ], GetDoubleValue(DoubleRes));
end;

procedure TTestInvokeMethodVars.Test15;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test15), TypeInfo(TMethodTest15),15, [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
    ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
    ], GetExtendedValue(ExtendedRes));
end;


procedure TTestInvokeMethodVars.Test16;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test16),TypeInfo(TMethodTest16), 16, [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
    ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
    ], GetCompValue(CompRes));
end;

procedure TTestInvokeMethodVars.Test17;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test17),TypeInfo(TMethodTest17), 17, [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
    ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
    ], GetCurrencyValue(CurrencyRes));
end;

procedure TTestInvokeMethodVars.Test18;
begin
  DoMethodInvoke(cls,TMethod({$ifdef fpc}@{$endif}cls.Test18),TypeInfo(TMethodTest18), 18, [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
    ], [], GetSingleValue(SingleAddRes));
end;

procedure TTestInvokeMethodVars.Test19;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test19), TypeInfo(TMethodTest19), 19, [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
    ], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestInvokeMethodVars.Test20;
begin
  DoMethodInvoke(cls, TMethod( {$ifdef fpc}@{$endif}cls.Test20),TypeInfo(TMethodTest20), 20, [
       GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
       GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
       ], [], GetExtendedValue(ExtendedAddRes));
end;

procedure TTestInvokeMethodVars.Test21;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test21),TypeInfo(TMethodTest21), 21, [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
    ], [], GetCompValue(CompAddRes));
end;

procedure TTestInvokeMethodVars.Test22;

begin
   DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.Test22),TypeInfo(TMethodTest22), 22, [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
    ], [], GetCurrencyValue(CurrencyAddRes));
end;

{ ----------------------------------------------------------------------
  TTestInvokeMethodVarsRecs
  ----------------------------------------------------------------------}


procedure TTestInvokeMethodVarsRecs.Test1;
begin
  DoMethodInvoke(cls,TMethod({$ifdef fpc}@{$endif}cls.TestRecSize1), TypeInfo(TMethodTestRecSize1), 1 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),False)], [],
    GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),True));
end;

procedure TTestInvokeMethodVarsRecs.Test2;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize2),TypeInfo(TMethodTestRecSize2), 2 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord2),SizeOF(TTestrecord2),False)], [],
    GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),True));
end;

procedure TTestInvokeMethodVarsRecs.Test3;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize3), TypeInfo(TMethodTestRecSize3), 3 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),False)], [],
    GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),True));
end;

procedure TTestInvokeMethodVarsRecs.Test4;

begin
  DoMethodInvoke(cls,TMethod({$ifdef fpc}@{$endif}cls.TestRecSize4), TypeInfo(TMethodTestRecSize4), 4 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),False)], [],
    GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),True));
end;

procedure TTestInvokeMethodVarsRecs.Test5;

begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize5),TypeInfo(TMethodTestRecSize5), 5 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),False)], [],
    GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),True));
end;

procedure TTestInvokeMethodVarsRecs.Test6;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize6), TypeInfo(TMethodTestRecSize6), 6 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),False)], [],
    GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),True));
end;

procedure TTestInvokeMethodVarsRecs.Test7;

begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize7),TypeInfo(TMethodTestRecSize7), 7 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),False)], [],
    GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),True));
end;

procedure TTestInvokeMethodVarsRecs.Test8;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize8), TypeInfo(TMethodTestRecSize8), 8 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),False)], [],
    GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),True));
end;

procedure TTestInvokeMethodVarsRecs.Test9;
begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize9),TypeInfo(TMethodTestRecSize9), 9 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),False)], [],
    GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),True));
end;

procedure TTestInvokeMethodVarsRecs.Test10;

begin
  DoMethodInvoke(cls, TMethod({$ifdef fpc}@{$endif}cls.TestRecSize10),TypeInfo(TMethodTestRecSize10), 10 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),False)], [],
    GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),True));
end;

{ ----------------------------------------------------------------------
  TTestInvokeProcVars
  ----------------------------------------------------------------------}

procedure TTestInvokeProcVars.Test1;
begin
  DoProcVarInvoke(CodePointer({$ifdef fpc}@{$endif}ProcTest1),TypeInfo(TProcVarTest1), 1, [], [], TValue.Empty);
end;

procedure TTestInvokeProcVars.Test2;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest2),TypeInfo(TProcVarTest2), 2, [], [], GetIntValue(42));
end;

procedure TTestInvokeProcVars.Test3;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest3),TypeInfo(TProcVarTest3), 3, [
    GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
    ], [], GetIntValue(42));
end;

procedure TTestInvokeProcVars.Test4;
begin

  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest4), TypeInfo(TProcVarTest4), 4, [
    GetAnsiString('Alpha'),
    GetUnicodeString('Beta'),
    GetWideString('Gamma'),
    GetShortString('Delta')
    ], [], TValue.Empty);
end;

procedure TTestInvokeProcVars.Test5;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest5), TypeInfo(TProcVarTest5), 5, [], [], GetAnsiString('Hello World'));
end;

procedure TTestInvokeProcVars.Test6;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest6), TypeInfo(TProcVarTest6), 6, [], [], GetUnicodeString('Hello World'));
end;

procedure TTestInvokeProcVars.Test7;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest7), TypeInfo(TProcVarTest7), 7, [], [], GetWideString('Hello World'));
end;

procedure TTestInvokeProcVars.Test8;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest8), TypeInfo(TProcVarTest8), 8, [], [], GetShortString('Hello World'));
end;

procedure TTestInvokeProcVars.Test9;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest9), TypeInfo(TProcVarTest9) , 9, [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($1234), GetIntValue($5678)
    ], TValue.Empty);
end;

procedure TTestInvokeProcVars.Test10;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest10), TypeInfo(TProcVarTest10), 10, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
    ], [
    GetAnsiString('Foo'), GetAnsiString('Bar')
    ], TValue.Empty);
end;
procedure TTestInvokeProcVars.Test11;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest11), TypeInfo(TProcVarTest11), 11, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
    ], [
    GetShortString('Foo'), GetShortString('Bar')
    ], TValue.Empty);
end;

procedure TTestInvokeProcVars.Test12;
begin
{$ifdef fpc}
  DoProcVarInvoke(CodePointer(@ProcTest12), TypeInfo(TProcVarTest12), 12, [
    GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
    ], [
    GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
    ], TValue.Empty);
{$endif}
end;

procedure TTestInvokeProcVars.Test13;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest13),TypeInfo(TProcVarTest13), 13, [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
    ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
    ], GetSingleValue(SingleRes));
end;

procedure TTestInvokeProcVars.Test14;
begin
  DoProcVarInvoke(CodePointer({$ifdef fpc}@{$endif}ProcTest14), TypeInfo(TProcVarTest14), 14, [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
    ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
    ], GetDoubleValue(DoubleRes));
end;

procedure TTestInvokeProcVars.Test15;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest15), TypeInfo(TProcVarTest15), 15, [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
    ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
    ], GetExtendedValue(ExtendedRes));
end;

procedure TTestInvokeProcVars.Test16;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest16), TypeInfo(TProcVarTest16), 16, [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
    ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
    ], GetCompValue(CompRes));
end;

procedure TTestInvokeProcVars.Test17;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest17), TypeInfo(TProcVarTest17), 17, [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
    ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
    ], GetCurrencyValue(CurrencyRes));
end;

procedure TTestInvokeProcVars.Test18;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest18), TypeInfo(TProcVarTest18), 18, [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
    ], [], GetSingleValue(SingleAddRes));

end;

procedure TTestInvokeProcVars.Test19;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest19), TypeInfo(TProcVarTest19), 19, [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
    ], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestInvokeProcVars.Test20;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest20), TypeInfo(TProcVarTest20), 20, [
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
    ], [], GetExtendedValue(ExtendedAddRes));

end;

procedure TTestInvokeProcVars.Test21;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest21), TypeInfo(TProcVarTest21), 21, [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
    ], [], GetCompValue(CompAddRes));
end;

procedure TTestInvokeProcVars.Test22;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest22), TypeInfo(TProcVarTest22), 22, [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
    ], [], GetCurrencyValue(CurrencyAddRes));

end;

{ ----------------------------------------------------------------------
  TTestInvokeProcVarRecs
  ----------------------------------------------------------------------}

procedure TTestInvokeProcVarRecs.Test1;

begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize1), TypeInfo(TProcVarTestRecSize1), 1 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),False)], [],
    GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),True));
end;

procedure TTestInvokeProcVarRecs.Test2;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize2), TypeInfo(TProcVarTestRecSize2), 2 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),False)], [],
    GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),True));
end;

procedure TTestInvokeProcVarRecs.Test3;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize3), TypeInfo(TProcVarTestRecSize3), 3 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),False)], [],
    GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),True));
end;

procedure TTestInvokeProcVarRecs.Test4;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize4), TypeInfo(TProcVarTestRecSize4), 4 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),False)], [],
    GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),True));
end;

procedure TTestInvokeProcVarRecs.Test5;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize5), TypeInfo(TProcVarTestRecSize5), 5 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),False)], [],
    GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),True));
end;

procedure TTestInvokeProcVarRecs.Test6;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize6), TypeInfo(TProcVarTestRecSize6), 6 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),False)], [],
    GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),True));
end;

procedure TTestInvokeProcVarRecs.Test7;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize7), TypeInfo(TProcVarTestRecSize7), 7 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),False)], [],
    GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),True));
end;

procedure TTestInvokeProcVarRecs.Test8;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize8), TypeInfo(TProcVarTestRecSize8), 8 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),False)], [],
    GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),True));
end;

procedure TTestInvokeProcVarRecs.Test9;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize9), TypeInfo(TProcVarTestRecSize9), 9 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),False)], [],
    GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),True));
end;

procedure TTestInvokeProcVarRecs.Test10;
begin
  DoProcVarInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize10), TypeInfo(TProcVarTestRecSize10), 10 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),False)], [],
    GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),True));
end;

{ TTestInvokeTestProc }

procedure TTestInvokeTestProc.Test1;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest1),TypeInfo(TProcVarTest1), 1, [], [], TValue.Empty);
end;

procedure TTestInvokeTestProc.Test2;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest2),TypeInfo(TProcVarTest2), 2, [], [], GetIntValue(42));
end;

procedure TTestInvokeTestProc.Test3;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest3),TypeInfo(TProcVarTest3), 3, [
    GetIntValue(7), GetIntValue(2), GetIntValue(5), GetIntValue(1), GetIntValue(10), GetIntValue(8), GetIntValue(6), GetIntValue(3), GetIntValue(9), GetIntValue(3)
    ], [], GetIntValue(42));
end;

procedure TTestInvokeTestProc.Test4;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest4),TypeInfo(TProcVarTest4), 4, [
    GetAnsiString('Alpha'),
    GetUnicodeString('Beta'),
    GetWideString('Gamma'),
    GetShortString('Delta')
    ], [], TValue.Empty);
end;

procedure TTestInvokeTestProc.Test5;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest5),TypeInfo(TProcVarTest5), 5, [], [], GetAnsiString('Hello World'));
end;

procedure TTestInvokeTestProc.Test6;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest6),TypeInfo(TProcVarTest6), 6, [], [], GetUnicodeString('Hello World'));
end;

procedure TTestInvokeTestProc.Test7;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest7),TypeInfo(TProcVarTest7), 7, [], [], GetWideString('Hello World'));
end;

procedure TTestInvokeTestProc.Test8;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest8),TypeInfo(TProcVarTest8), 8, [], [], GetShortString('Hello World'));
end;

{$ifdef NEEDS_POINTER_HELPER}
procedure TTestInvokeTestProc.Test9;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest9),TypeInfo(TProcVarTest9), 9, [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($1234), GetIntValue($5678)
    ], TValue.Empty);
end;

procedure TTestInvokeTestProc.Test10;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest10),TypeInfo(TProcVarTest10), 10, [
    GetAnsiString('Alpha'), GetAnsiString('Beta'), GetAnsiString(''), GetAnsiString('Delta')
    ], [
    GetAnsiString('Foo'), GetAnsiString('Bar')
    ], TValue.Empty);
end;

procedure TTestInvokeTestProc.Test11;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest11),TypeInfo(TProcVarTest11), 11, [
    GetShortString('Alpha'), GetShortString('Beta'), GetShortString(''), GetShortString('Delta')
    ], [
    GetShortString('Foo'), GetShortString('Bar')
    ], TValue.Empty);
end;

procedure TTestInvokeTestProc.Test12;
begin
  {$ifdef fpc}
    DoProcInvoke(CodePointer({$ifdef fpc}@{$endif}ProcTest12),TypeInfo(TProcVarTest12), 12, [
      GetArray([$1234, $2345, $3456, $4567]), GetArray([$4321, $5431, $6543, $7654]), GetArray([$5678, $6789, $7890, $8901]), GetArray([$8765, $7654, $6543, $5432])
      ], [
      GetArray([$4321, $4322, $4323, $4324]), GetArray([$9876, $9877, $9878, $9879])
      ], TValue.Empty);
  {$endif}
end;

procedure TTestInvokeTestProc.Test13;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest13),TypeInfo(TProcVarTest13), 13, [
    GetSingleValue(SingleArg1), GetSingleValue(SingleArg2In), GetSingleValue(0), GetSingleValue(SingleArg4)
    ], [
    GetSingleValue(SingleArg2Out), GetSingleValue(SingleArg3Out)
    ], GetSingleValue(SingleRes));
end;

procedure TTestInvokeTestProc.Test14;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest14),TypeInfo(TProcVarTest14), 14, [
    GetDoubleValue(DoubleArg1), GetDoubleValue(DoubleArg2In), GetDoubleValue(0), GetDoubleValue(DoubleArg4)
    ], [
    GetDoubleValue(DoubleArg2Out), GetDoubleValue(DoubleArg3Out)
    ], GetDoubleValue(DoubleRes));
end;

procedure TTestInvokeTestProc.Test15;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest15),TypeInfo(TProcVarTest15), 15, [
    GetExtendedValue(ExtendedArg1), GetExtendedValue(ExtendedArg2In), GetExtendedValue(0), GetExtendedValue(ExtendedArg4)
    ], [
    GetExtendedValue(ExtendedArg2Out), GetExtendedValue(ExtendedArg3Out)
    ], GetExtendedValue(ExtendedRes));
end;

procedure TTestInvokeTestProc.Test16;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest16),TypeInfo(TProcVarTest16), 16, [
    GetCompValue(CompArg1), GetCompValue(CompArg2In), GetCompValue(0), GetCompValue(CompArg4)
    ], [
    GetCompValue(CompArg2Out), GetCompValue(CompArg3Out)
    ], GetCompValue(CompRes));
end;

procedure TTestInvokeTestProc.Test17;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest17),TypeInfo(TProcVarTest17), 17, [
    GetCurrencyValue(CurrencyArg1), GetCurrencyValue(CurrencyArg2In), GetCurrencyValue(0), GetCurrencyValue(CurrencyArg4)
    ], [
    GetCurrencyValue(CurrencyArg2Out), GetCurrencyValue(CurrencyArg3Out)
    ], GetCurrencyValue(CurrencyRes));
end;
{$endif NEEDS_POINTER_HELPER}

procedure TTestInvokeTestProc.Test18;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest18),TypeInfo(TProcVarTest18), 18, [
    GetSingleValue(SingleAddArg1), GetSingleValue(SingleAddArg2), GetSingleValue(SingleAddArg3), GetSingleValue(SingleAddArg4), GetSingleValue(SingleAddArg5),
    GetSingleValue(SingleAddArg6), GetSingleValue(SingleAddArg7), GetSingleValue(SingleAddArg8), GetSingleValue(SingleAddArg9), GetSingleValue(SingleAddArg10)
    ], [], GetSingleValue(SingleAddRes));
end;

procedure TTestInvokeTestProc.Test19;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest19),TypeInfo(TProcVarTest19), 19, [
    GetDoubleValue(DoubleAddArg1), GetDoubleValue(DoubleAddArg2), GetDoubleValue(DoubleAddArg3), GetDoubleValue(DoubleAddArg4), GetDoubleValue(DoubleAddArg5),
    GetDoubleValue(DoubleAddArg6), GetDoubleValue(DoubleAddArg7), GetDoubleValue(DoubleAddArg8), GetDoubleValue(DoubleAddArg9), GetDoubleValue(DoubleAddArg10)
    ], [], GetDoubleValue(DoubleAddRes));
end;

procedure TTestInvokeTestProc.Test20;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest20),TypeInfo(TProcVarTest20), 20, [
    GetExtendedValue(ExtendedAddArg1), GetExtendedValue(ExtendedAddArg2), GetExtendedValue(ExtendedAddArg3), GetExtendedValue(ExtendedAddArg4), GetExtendedValue(ExtendedAddArg5),
    GetExtendedValue(ExtendedAddArg6), GetExtendedValue(ExtendedAddArg7), GetExtendedValue(ExtendedAddArg8), GetExtendedValue(ExtendedAddArg9), GetExtendedValue(ExtendedAddArg10)
    ], [], GetExtendedValue(ExtendedAddRes));
end;

procedure TTestInvokeTestProc.Test21;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest21),TypeInfo(TProcVarTest21), 21, [
    GetCompValue(CompAddArg1), GetCompValue(CompAddArg2), GetCompValue(CompAddArg3), GetCompValue(CompAddArg4), GetCompValue(CompAddArg5),
    GetCompValue(CompAddArg6), GetCompValue(CompAddArg7), GetCompValue(CompAddArg8), GetCompValue(CompAddArg9), GetCompValue(CompAddArg10)
    ], [], GetCompValue(CompAddRes));
end;

procedure TTestInvokeTestProc.Test22;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTest22),TypeInfo(TProcVarTest22), 22, [
    GetCurrencyValue(CurrencyAddArg1), GetCurrencyValue(CurrencyAddArg2), GetCurrencyValue(CurrencyAddArg3), GetCurrencyValue(CurrencyAddArg4), GetCurrencyValue(CurrencyAddArg5),
    GetCurrencyValue(CurrencyAddArg6), GetCurrencyValue(CurrencyAddArg7), GetCurrencyValue(CurrencyAddArg8), GetCurrencyValue(CurrencyAddArg9), GetCurrencyValue(CurrencyAddArg10)
    ], [], GetCurrencyValue(CurrencyAddRes));
end;

{ TTestInvokeTestProcRecs }

procedure TTestInvokeTestProcRecs.Test1;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize1),TypeInfo(TProcVarTestRecSize1), 1 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),False)], [],
    GetRecValue(TypeInfo(TTestRecord1),SizeOf(TTestRecord1),True));
end;

procedure TTestInvokeTestProcRecs.Test2;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize2),TypeInfo(TProcVarTestRecSize2), 2 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),False)], [],
    GetRecValue(TypeInfo(TTestRecord2),SizeOf(TTestRecord2),True));
end;

procedure TTestInvokeTestProcRecs.Test3;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize3),TypeInfo(TProcVarTestRecSize3), 3 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),False)], [],
    GetRecValue(TypeInfo(TTestRecord3),SizeOf(TTestRecord3),True));
end;

procedure TTestInvokeTestProcRecs.Test4;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize4),TypeInfo(TProcVarTestRecSize4), 4 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),False)], [],
    GetRecValue(TypeInfo(TTestRecord4),SizeOf(TTestRecord4),True));
end;

procedure TTestInvokeTestProcRecs.Test5;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize5),TypeInfo(TProcVarTestRecSize5), 5 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),False)], [],
    GetRecValue(TypeInfo(TTestRecord5),SizeOf(TTestRecord5),True));
end;

procedure TTestInvokeTestProcRecs.Test6;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize6),TypeInfo(TProcVarTestRecSize6), 6 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),False)], [],
    GetRecValue(TypeInfo(TTestRecord6),SizeOf(TTestRecord6),True));
end;

procedure TTestInvokeTestProcRecs.Test7;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize7),TypeInfo(TProcVarTestRecSize7), 7 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),False)], [],
    GetRecValue(TypeInfo(TTestRecord7),SizeOf(TTestRecord7),True));
end;

procedure TTestInvokeTestProcRecs.Test8;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize8),TypeInfo(TProcVarTestRecSize8), 8 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),False)], [],
    GetRecValue(TypeInfo(TTestRecord8),SizeOf(TTestRecord8),True));
end;

procedure TTestInvokeTestProcRecs.Test9;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize9),TypeInfo(TProcVarTestRecSize9), 9 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),False)], [],
    GetRecValue(TypeInfo(TTestRecord9),SizeOf(TTestRecord9),True));
end;

procedure TTestInvokeTestProcRecs.Test10;
begin
  DoProcInvoke(CodePointer( {$ifdef fpc}@{$endif}ProcTestRecSize10),TypeInfo(TProcVarTestRecSize10), 10 or TTestInterfaceClass.RecSizeMarker,
    [GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),False)], [],
    GetRecValue(TypeInfo(TTestRecord10),SizeOf(TTestRecord10),True));
end;

{ TTestInvokeUntyped }

procedure TTestInvokeUntyped.Test1;
begin
  DoUntypedInvoke(CodePointer(Nil), Default(TMethod), PTypeInfo(Nil), [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($4321), GetIntValue($5678)
    ]);
end;

procedure TTestInvokeUntyped.Test2;
begin
  DoUntypedInvoke(CodePointer(Nil), Default(TMethod), PTypeInfo(Nil), [
    GetAnsiString('Str1'),
    GetAnsiString('Str2'),
    GetAnsiString('Str3'),
    GetAnsiString('Str4')
    ], [
    GetAnsiString('StrVar'),
    GetAnsiString('StrOut')
    ]);
end;

procedure TTestInvokeUntyped.Test3;
begin
  DoUntypedInvoke(CodePointer(Nil), Default(TMethod), PTypeInfo(Nil), [
    GetShortString('Str1'),
    GetShortString('Str2'),
    GetShortString('Str3'),
    GetShortString('Str4')
    ], [
    GetShortString('StrVar'),
    GetShortString('StrOut')
    ]);
end;

procedure TTestInvokeUntyped.Test4;
begin
  DoUntypedInvoke(Nil, TMethod({$ifdef fpc}@{$endif}cls.TestUntyped), TypeInfo(TMethodTestUntyped), [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($4321), GetIntValue($5678)
    ]);
end;

procedure TTestInvokeUntyped.Test5;
begin
  DoUntypedInvoke(Nil, TMethod({$ifdef fpc}@{$endif}cls.TestUntyped), TypeInfo(TMethodTestUntyped), [
    GetAnsiString('Str1'),
    GetAnsiString('Str2'),
    GetAnsiString('Str3'),
    GetAnsiString('Str4')
    ], [
    GetAnsiString('StrVar'),
    GetAnsiString('StrOut')
    ]);
end;

procedure TTestInvokeUntyped.Test6;
begin
  DoUntypedInvoke(Nil, TMethod({$ifdef fpc}@{$endif}cls.TestUntyped), TypeInfo(TMethodTestUntyped), [
    GetShortString('Str1'),
    GetShortString('Str2'),
    GetShortString('Str3'),
    GetShortString('Str4')
    ], [
    GetShortString('StrVar'),
    GetShortString('StrOut')
    ]);
end;

procedure TTestInvokeUntyped.Test7;
begin
  DoUntypedInvoke({$ifdef fpc}@{$endif}ProcTestUntyped, Default(TMethod), TypeInfo(TProcVarTestUntyped), [
    GetIntValue($1234), GetIntValue($4321), GetIntValue($8765), GetIntValue($5678)
    ], [
    GetIntValue($4321), GetIntValue($5678)
    ]);
end;

procedure TTestInvokeUntyped.Test8;
begin
  DoUntypedInvoke({$ifdef fpc}@{$endif}ProcTestUntyped, Default(TMethod), TypeInfo(TProcVarTestUntyped), [
    GetAnsiString('Str1'),
    GetAnsiString('Str2'),
    GetAnsiString('Str3'),
    GetAnsiString('Str4')
    ], [
    GetAnsiString('StrVar'),
    GetAnsiString('StrOut')
    ]);
end;

procedure TTestInvokeUntyped.Test9;
begin
  DoUntypedInvoke({$ifdef fpc}@{$endif}ProcTestUntyped, Default(TMethod), TypeInfo(TProcVarTestUntyped), [
    GetShortString('Str1'),
    GetShortString('Str2'),
    GetShortString('Str3'),
    GetShortString('Str4')
    ], [
    GetShortString('StrVar'),
    GetShortString('StrOut')
    ]);
end;



begin
{$ifdef fpc}
  RegisterTest(TTestInvoke);
  RegisterTest(TTestInvokeIntfMethods);
  RegisterTest(TTestInvokeIntfMethodsRecs);
  RegisterTest(TTestInvokeMethodVars);
  RegisterTest(TTestInvokeMethodVarsRecs);
  RegisterTest(TTestInvokeProcVars);
  RegisterTest(TTestInvokeProcVarRecs);
  RegisterTest(TTestInvokeTestProc);
  RegisterTest(TTestInvokeTestProcRecs);
  RegisterTest(TTestInvokeUntyped);

{$else fpc}
  RegisterTest(TTestInvoke.Suite);
  RegisterTest(TTestInvokeIntfMethods.Suite);
  RegisterTest(TTestInvokeIntfMethodsRecs.Suite);
  RegisterTest(TTestInvokeMethodVars.Suite);
  RegisterTest(TTestInvokeMethodVarsRecs.Suite);
  RegisterTest(TTestInvokeProcVars.Suite);
  RegisterTest(TTestInvokeProcVarRecs.Suite);
  RegisterTest(TTestInvokeTestProc.Suite);
  RegisterTest(TTestInvokeTestProcRecs.Suite);
  RegisterTest(TTestInvokeUntyped.Suite);
{$endif fpc}
end.

