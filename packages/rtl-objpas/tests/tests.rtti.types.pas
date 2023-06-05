unit tests.rtti.types;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch prefixedattributes}
{$endif}

interface

uses
  Classes, SysUtils;

Type
  {$M+}
  TGetClassProperties = class
  private
    FPubPropRO: integer;
    FPubPropRW: integer;
  published
    property PubPropRO: integer read FPubPropRO;
    property PubPropRW: integer read FPubPropRW write FPubPropRW;
    property PubPropSetRO: integer read FPubPropRO;
    property PubPropSetRW: integer read FPubPropRW write FPubPropRW;
  end;

  TGetClassPropertiesSub = class(TGetClassProperties)

  end;



  {$M+}
  ITestInterface = interface
    procedure Test;
    function Test2: LongInt;
    procedure Test3(aArg1: LongInt; const aArg2: AnsiString; var aArg3: Boolean; out aArg4: Word);
    function Test4(aArg1: array of LongInt; aArg2: array of const): AnsiString;
  end;
  {$M-}

  TManagedRec = record
    s: string;
  end;

{$ifdef fpc}
  TManagedRecOp = record
    class operator AddRef(var a: TManagedRecOp);
  end;
{$endif}

  TNonManagedRec = record
    i: Integer;
  end;

  TManagedObj = object
    i: IInterface;
  end;

  TNonManagedObj = object
    d: double;
  end;

  TTestEnum = (te1, te2, te3, te4, te5);
  TTestSet = set of TTestEnum;

  TTestProc = procedure;
  TTestFunc1 = function: LongInt;
  TTestFunc2 = function(aArg1: LongInt; aArg2: array of LongInt): String;
  TTestMethod = procedure of object;
  TTestMethod1 = function: LongInt of object;
  TTestMethod2 = function(aArg1: LongInt; aArg2: array of LongInt): String of object;
  TTestHelper = class helper for TObject
  end;


  TTestRecord = record
    Value1: LongInt;
    Value2: String;
  end;
  PTestRecord = ^TTestRecord;

  TArrayOfString = array[0..0] of string;
  TArrayOfManagedRec = array[0..0] of TManagedRec;
  TArrayOfNonManagedRec = array[0..0] of TNonManagedRec;
  TArrayOfByte = array[0..0] of byte;

  TArrayOfLongintDyn = array of LongInt;
  TArrayOfLongintStatic = array[0..3] of LongInt;
  TArrayOfLongint2DStatic = array[0..3, 2..4] of LongInt;


  TTestDynArray = array of Integer;
  TTestEnumeration = (en1, en2, en3, en4);
  {$M-}

  { TTestValueClass }

  {$M+}
  TTestValueClass = class
  private
    FAArray: TTestDynArray;
    FAChar: AnsiChar;
    FAComp: Comp;
    FACurrency: Currency;
    FADouble: Double;
    FAEnumeration: TTestEnumeration;
    FAExtended: Extended;
    FAInteger: integer;
    FAObject: TObject;
    FASingle: Single;
    FAString: string;
    FABoolean: boolean;
    FAShortString: ShortString;
    FAUnknown: IUnknown;
    FAWideChar: WideChar;
    function GetAInteger: integer;
    function GetAString: string;
    function GetABoolean: boolean;
    function GetAShortString: ShortString;
    procedure SetWriteOnly(AValue: integer);
  published
    property AArray: TTestDynArray read FAArray write FAArray;
    property AEnumeration: TTestEnumeration read FAEnumeration write FAEnumeration;
    property AInteger: Integer read FAInteger write FAInteger;
    property AString: string read FAString write FAString;
    property ASingle: Single read FASingle write FASingle;
    property ADouble: Double read FADouble write FADouble;
    property AExtended: Extended read FAExtended write FAExtended;
    property ACurrency: Currency read FACurrency write FACurrency;
    property AObject: TObject read FAObject write FAObject;
    property AUnknown: IUnknown read FAUnknown write FAUnknown;
    property AComp: Comp read FAComp write FAComp;
    property ABoolean: boolean read FABoolean write FABoolean;
    property AShortString: ShortString read FAShortString write FAShortString;
    property AGetInteger: Integer read GetAInteger;
    property AGetString: string read GetAString;
    property AGetBoolean: boolean read GetABoolean;
    property AGetShortString: ShortString read GetAShortString;
    property AWriteOnly: integer write SetWriteOnly;
    property AChar: AnsiChar read FAChar write FAChar;
    property AWideChar: WideChar read FAWideChar write FAWideChar;
  end;
  {$M-}

  {$ifdef fpc}
  {$PUSH}
  {$INTERFACES CORBA}

    ICORBATest = interface
    end;

  {$POP}
  {$endif}

  { TMyAttribute }

  TMyAttribute = class(TCustomAttribute)
  private
    FValue: string;
  public
    constructor create(const avalue : string);
    property value : string read FValue;
  end;


  { TMyAnnotatedClass }

  [TMyAttribute('something')]
  TMyAnnotatedClass = class
  private
    FSomething: String;
  Published
    Property Something : String Read FSomething Write FSomeThing;
  end;

implementation

{ TTestValueClass }

function TTestValueClass.GetAInteger: integer;
begin
  result := FAInteger;
end;

function TTestValueClass.GetAString: string;
begin
  result := FAString;
end;

function TTestValueClass.GetABoolean: boolean;
begin
  result := FABoolean;
end;

function TTestValueClass.GetAShortString: ShortString;
begin
  Result := FAShortString;
end;

procedure TTestValueClass.SetWriteOnly(AValue: integer);
begin
  // Do nothing
end;

{ TMyAttribute }

constructor TMyAttribute.create(const avalue: string);
begin
  FValue:=aValue;
end;

{$ifdef fpc}
class operator TManagedRecOp.AddRef(var  a: TManagedRecOp);
begin
end;
{$endif}


end.

