{ %CPU=wasm32 }
Unit uthintf;

{$mode objfpc}
{$h+}

interface


type
  TArgdata = record
    toto : string;
  end;
  
  {$M+}
  TMyInterface = Interface ['{76DC0D03-376C-45AA-9E0C-B3546B0C7208}']
    Procedure DoA(a : Integer);
    Procedure DoA;
    function doB : Integer;
    function doc(a : integer) : integer;
    procedure DoD(var p);
    procedure DoE(data : TargData);
  end;

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

 ITestInterface2 = interface
    procedure Test;
    function Test2: LongInt;
    procedure Test3(aArg1: LongInt; const aArg2: AnsiString; var aArg3: Boolean; out aArg4: Word);
    function Test4(aArg1: array of LongInt; aArg2: array of const): AnsiString;
  end;

  
implementation

end.  
  
  
  