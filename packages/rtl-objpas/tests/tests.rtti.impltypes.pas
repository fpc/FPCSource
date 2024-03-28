unit tests.rtti.impltypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, rtti;


type
  {$push}
  {$M+}
  ITestInterface = interface
    ['{1DE799BB-BEE9-405F-9AF3-D55DE978C793}']
    procedure TestMethod1;
    function  TestMethod2(aArg1: SizeInt): SizeInt;
    procedure TestMethod3(aArg1: AnsiString);
    procedure TestMethod4(aArg1: ShortString);
    function  TestMethod5: AnsiString;
    function  TestMethod6: ShortString;
    procedure TestMethod7(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: SizeInt);
    procedure TestMethod8(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: AnsiString);
    procedure TestMethod9(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: ShortString);
    procedure TestMethod10(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Single);
    procedure TestMethod11(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Double);
    procedure TestMethod12(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Extended);
    procedure TestMethod13(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Comp);
    procedure TestMethod14(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Currency);
    function  TestMethod15(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
    function  TestMethod16(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
    function  TestMethod17(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
    function  TestMethod18(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
    function  TestMethod19(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
    function  TestMethod20(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
    procedure TestMethod21(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);
  end;
  {$pop}

  TTestMethod1 = procedure of object;
  TTestMethod2 = function(aArg1: SizeInt): SizeInt of object;
  TTestMethod3 = procedure(aArg1: AnsiString) of object;
  TTestMethod4 = procedure(aArg1: ShortString) of object;
  TTestMethod5 = function: AnsiString of object;
  TTestMethod6 = function: ShortString of object;
  TTestMethod7 = procedure(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: SizeInt) of object;
  TTestMethod8 = procedure(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: AnsiString) of object;
  TTestMethod9 = procedure(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: ShortString) of object;
  TTestMethod10 = procedure(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Single) of object;
  TTestMethod11 = procedure(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Double) of object;
  TTestMethod12 = procedure(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Extended) of object;
  TTestMethod13 = procedure(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Comp) of object;
  TTestMethod14 = procedure(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Currency) of object;
  TTestMethod15 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt of object;
  TTestMethod16 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single of object;
  TTestMethod17 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double of object;
  TTestMethod18 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended of object;
  TTestMethod19 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp of object;
  TTestMethod20 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency of object;
  TTestMethod21 = procedure(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4) of object;

  TTestProc1 = procedure;
  TTestProc2 = function(aArg1: SizeInt): SizeInt;
  TTestProc3 = procedure(aArg1: AnsiString);
  TTestProc4 = procedure(aArg1: ShortString);
  TTestProc5 = function: AnsiString;
  TTestProc6 = function: ShortString;
  TTestProc7 = procedure(aArg1: SizeInt; var aArg2: SizeInt; out aArg3: SizeInt; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: SizeInt);
  TTestProc8 = procedure(aArg1: AnsiString; var aArg2: AnsiString; out aArg3: AnsiString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: AnsiString);
  TTestProc9 = procedure(aArg1: ShortString; var aArg2: ShortString; out aArg3: ShortString; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: ShortString);
  TTestProc10 = procedure(aArg1: Single; var aArg2: Single; out aArg3: Single; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Single);
  TTestProc11 = procedure(aArg1: Double; var aArg2: Double; out aArg3: Double; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Double);
  TTestProc12 = procedure(aArg1: Extended; var aArg2: Extended; out aArg3: Extended; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Extended);
  TTestProc13 = procedure(aArg1: Comp; var aArg2: Comp; out aArg3: Comp; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Comp);
  TTestProc14 = procedure(aArg1: Currency; var aArg2: Currency; out aArg3: Currency; {$ifdef fpc}constref{$else}const [ref]{$endif}aArg4: Currency);
  TTestProc15 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: SizeInt): SizeInt;
  TTestProc16 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Single): Single;
  TTestProc17 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Double): Double;
  TTestProc18 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Extended): Extended;
  TTestProc19 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Comp): Comp;
  TTestProc20 = function(aArg1, aArg2, aArg3, aArg4, aArg5, aArg6, aArg7, aArg8, aArg9, aArg10: Currency): Currency;
  TTestProc21 = procedure(var aArg1; out aArg2; const aArg3; {$ifdef fpc}constref{$else}const [ref]{$endif} aArg4);

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
implementation

end.

