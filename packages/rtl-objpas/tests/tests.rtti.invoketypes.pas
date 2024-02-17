unit tests.rtti.invoketypes;

{$ifdef fpc}
{$mode ObjFPC}{$H+}
{$endif}
interface

uses
  Classes, SysUtils, RTTI;

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

  { TTestInterfaceClass }

  TTestInterfaceClass = class(TInterfacedObject, ITestInterface)
  public
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
    function DoAddRef : longint;
    function DoRelease : longint;
    Destructor Destroy; override;
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


procedure ProcTest1;
function ProcTest2: SizeInt;
function ProcTest3(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
procedure ProcTest4(aArg1: AnsiString; aArg2: UnicodeString; aArg3: WideString; aArg4: ShortString);
function ProcTest5: AnsiString;
function ProcTest6: UnicodeString;
function ProcTest7: WideString;
function ProcTest8: ShortString;
procedure ProcTest9(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: SizeInt);
procedure ProcTest10(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: AnsiString);
procedure ProcTest11(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: ShortString);
procedure ProcTest12(aArg1: array of SizeInt; var aArg2: array of SizeInt; out aArg3: array of SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: array of SizeInt);
function ProcTest13(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Single): Single;
function ProcTest14(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Double): Double;
function ProcTest15(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Extended): Extended;
function ProcTest16(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Comp): Comp;
function ProcTest17(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4: Currency): Currency;
function ProcTest18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
function ProcTest19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
function ProcTest20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
function ProcTest21(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
function ProcTest22(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
function ProcTestRecSize1(aArg1: TTestRecord1): TTestRecord1;
function ProcTestRecSize2(aArg1: TTestRecord2): TTestRecord2;
function ProcTestRecSize3(aArg1: TTestRecord3): TTestRecord3;
function ProcTestRecSize4(aArg1: TTestRecord4): TTestRecord4;
function ProcTestRecSize5(aArg1: TTestRecord5): TTestRecord5;
function ProcTestRecSize6(aArg1: TTestRecord6): TTestRecord6;
function ProcTestRecSize7(aArg1: TTestRecord7): TTestRecord7;
function ProcTestRecSize8(aArg1: TTestRecord8): TTestRecord8;
function ProcTestRecSize9(aArg1: TTestRecord9): TTestRecord9;
function ProcTestRecSize10(aArg1: TTestRecord10): TTestRecord10;
procedure ProcTestUntyped(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);


implementation

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

function TTestInterfaceClass.DoAddRef: longint;
begin
  Result:=_AddRef;
end;

function TTestInterfaceClass.DoRelease: longint;
begin
  Result:=_Release
end;

destructor TTestInterfaceClass.Destroy;
begin
  // Empty, for debugging purposes
  inherited Destroy;
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



end.

