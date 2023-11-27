{
    This file is part of the Free Component Library
    Copyright (c) 2023 by Michael Van Canneyt michael@freepascal.org

    Delphi-compatible JSON unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.JSON;

interface

{$mode objfpc}
{$h+}
{$SCOPEDENUMS ON}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch }

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, System.Classes, System.Rtti, System.TypInfo, System.Generics.Collections, FpJson.Data;
{$ELSE}
  Types, SysUtils, Classes, Rtti, TypInfo, Generics.Collections, fpjson;
{$ENDIF}

type
  TJSONByteReader = class;
  TJSONValue = class;
  TJSONObject = class;

  EJSONException = class(Exception);

  EJSONPathException = class(EJSONException);

  { EJSONParseException }

  EJSONParseException = class(EJSONException)
  private
    FPath: UnicodeString;
    FOffset: Integer;
    FLine, FPosition: Integer;
    constructor Create(aOffset: Integer; aReader: TJSONByteReader; aValue: TJSONValue; aIndent: PResStringRec; const aArgs: array of const); overload;
    constructor Create(aOffset: Integer; aReader: TJSONByteReader; aValue: TJSONValue); overload;
  public
    property Path: UnicodeString read FPath;
    property Offset: Integer read FOffset;
    property Line: Integer read FLine;
    property Position: Integer read FPosition;
  end;

  { TJSONPathParser }

  TJSONPathParser = record
  type
    TToken = (Undefined, Name, ArrayIndex, Eof, Error);
  private
    FToken: TToken;
    FTokenArrayIndex: Integer;
    FTokenName: UnicodeString;
    FPath : PWideChar;
    FCurrent : PWideChar;
    FEnd : PWideChar;
    function GetIsEof: Boolean;
  public
  public
    constructor Create(const aPath: UnicodeString); overload;
    constructor Create(const aPath: PWideChar; aLen: Integer); overload;
    function NextToken: TToken;
    property IsEof: Boolean read GetIsEof;
    property Token: TToken read FToken;
    property TokenName: UnicodeString read FTokenName;
    property TokenArrayIndex: Integer read FTokenArrayIndex;
  end;
  TJSONPathToken = TJSONPathParser.TToken;

  { TJSONAncestor }

  TJSONAncestor = class abstract
  public
  type
    TJSONOutputOption = (EncodeBelow32, EncodeAbove127);
    TJSONOutputOptions = set of TJSONOutputOption;
  const
    DefaultToBytesOptions = [TJSONOutputOption.EncodeBelow32, TJSONOutputOption.EncodeAbove127];
  private
    FOwned: Boolean;
  protected
    function IsNull: Boolean; virtual;
    procedure AddDescendant(const aDescendent: TJSONAncestor); virtual;
    procedure Format(aBuilder: TUnicodeStringBuilder; const aParentIndent, aIndent: UnicodeString); overload; virtual;
  public
    constructor Create; overload; virtual;
    function Value: UnicodeString; virtual;
    function EstimatedByteSize: Integer; virtual; abstract;
    function ToBytes(const aData: TBytes; aOffset: Integer): Integer; virtual;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; virtual; abstract;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONOutputOptions); overload; virtual; abstract;
    function ToJSON(aOptions: TJSONOutputOptions): UnicodeString; overload;
    function ToJSON: UnicodeString; overload; inline;
    function ToString: RTLString; override;
    function Format(aIndentation: Integer = 4): UnicodeString; overload;
    function Clone: TJSONAncestor; virtual; abstract;
    property Null: Boolean read IsNull;
    property Owned: Boolean read FOwned write FOwned;
  end;

  { TJSONByteReader }

  TJSONByteReader = class
  private
    FData: PByte;
    FCurrent : PByte;
    FEnd : PByte;
    FIsUTF8: Boolean;
    FString : Array of WideChar;
    FStringLen : Integer;
    function GetOffset : Integer;
  public
    constructor Create(const aData: PByte; const aOffset: Integer; const aRange: Integer); overload;
    constructor Create(const aData: PByte; const aOffset: Integer; const aRange: Integer; const aIsUTF8: Boolean); overload;
    destructor Destroy; override;
    function ConsumeByte: Byte;
    function PeekRawByte: Byte; inline;
    function PeekByte: Byte;
    procedure SkipByte; inline;
    procedure SkipWhitespaces;
    function IsEof: Boolean; inline;
    function HasMore(const aSize: Integer): Boolean;
    procedure AddChar(aCh: WideChar);
    procedure ResetString; inline;
    procedure FlushString(out aDest: UnicodeString; aCache: Boolean);
    procedure OffsetToPos(aOffset: Integer; var aLine, APos: Integer);
    property Offset: Integer read GetOffset;
  end;

  { TJSONValue }

  TJSONValue = class abstract(TJSONAncestor)
  public
    type
      TJSONParseOption = (IsUTF8, UseBool, RaiseExc);
      TJSONParseOptions = set of TJSONParseOption;
    const
      DefaultBytesOptions = [TJSONOutputOption.EncodeBelow32,
                             TJSONOutputOption.EncodeAbove127];
  private
    function GetValueP(const aPath: UnicodeString): TJSONValue;
    function GetValueA(const aIndex: Integer): TJSONValue; virtual;
  protected
    function AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean; virtual;
  public
    class function ParseJSONValue(const aData: PByte; const aOffset: Integer; const aLength: Integer; Options: TJSONParseOptions): TJSONValue; overload; static;
    class function ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; aIsUTF8: Boolean = True): TJSONValue; overload; inline; static;
    class function ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; aOptions: TJSONParseOptions): TJSONValue; overload; inline; static;
    class function ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; const aLength: Integer; aIsUTF8: Boolean = True): TJSONValue; overload; inline; static;
    class function ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; const aLength: Integer; aOptions: TJSONParseOptions): TJSONValue; overload; inline; static;
    class function ParseJSONValue(const aData: Unicodestring; aUseBool: Boolean = False; aRaiseExc: Boolean = False): TJSONValue; overload; static;
    class function ParseJSONValue(const aData: UTF8String; aUseBool: Boolean = False; aRaiseExc: Boolean = False): TJSONValue; overload; static;
    class function ParseJSONFragment(const aData: PByte; var aOffset: Integer; const aLength: Integer; aOptions: TJSONParseOptions): TJSONValue; overload; static;
    class function ParseJSONFragment(const aData: TByteDynArray; var aOffset: Integer; aOptions: TJSONParseOptions): TJSONValue; overload; static;
    class function ParseJSONFragment(const aData: Unicodestring; var aOffset: Integer; aOptions: TJSONParseOptions): TJSONValue; overload; static;
    function FindValue(const aPath: UnicodeString): TJSONValue;
    generic function TryGetValue<T>(out aValue: T): Boolean; overload;
    generic function TryGetValue<T>(const aPath: UnicodeString; out aValue: T): Boolean; overload;
    generic function GetValue<T>(const aPath: UnicodeString = ''): T; overload;
    generic function GetValue<T>(const aPath: UnicodeString; aDefaultValue: T): T; overload;
    generic function AsType<T>: T; overload;
    property P[const aPath: UnicodeString]: TJSONValue read GetValueP;{ default;}
    property A[const aIndex: Integer]: TJSONValue read GetValueA;
    class function ParseJSONValueUTF8(const aData: TByteDynArray; const aOffset: Integer;
                                      const aCount: Integer): TJSONValue; overload; static;
    class function ParseJSONValueUTF8(const aData: TByteDynArray;
                                      const aOffset: Integer): TJSONValue; overload; static; 
  end;
  TJSONValueClass = Class of TJSONValue;
  TJSONValueList = specialize TList<TJSONValue>;

  { TJSONString }

  TJSONString = class(TJSONValue)
  protected
    FValue: UnicodeString;
    FIsNull: Boolean;
    function IsNull: Boolean; override;
    function AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean; override;
  public
    constructor Create; overload; override;
    constructor Create(const aValue: UnicodeString); overload;
    constructor Create(const aValue: UTF8String); overload;
    procedure AddChar(const aChar: WideChar);
    function Equals(const aValue: UnicodeString): Boolean; reintroduce; inline;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  end;

  { TJSONNumber }

  TJSONNumber = class sealed(TJSONString)
  protected
    function GetAsDouble: Double;
    function GetAsInt: Integer;
    function GetAsInt64: Int64;
  public
    constructor Create(const aValue: UnicodeString); overload;
    constructor Create(const aValue: Double); overload;
    constructor Create(const aValue: Integer); overload;
    constructor Create(const aValue: Int64); overload;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Clone: TJSONAncestor; override;
    property AsDouble: Double read GetAsDouble;
    property AsInt: Integer read GetAsInt;
    property AsInt64: Int64 read GetAsInt64;
  end;

  { TJSONNull }

  TJSONNull = class sealed(TJSONValue)
  strict protected const
    NULLString: UnicodeString = 'null';
  protected
    function AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean; override;
    function IsNull: Boolean; override;
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  end;

  { TJSONBool }

  TJSONBool = class(TJSONValue)
  private
    FValue: Boolean;
  strict protected const
    FalseString: UnicodeString = 'false';
    TrueString: UnicodeString = 'true';
    FalseBytesLen = 5;
    FalseBytes: array[0..FalseBytesLen-1] of Byte = (Ord('f'), Ord('a'), Ord('l'), Ord('s'), Ord('e'));
    TrueBytesLen = 4;
    TrueBytes: array[0..TruebytesLen-1] of Byte = (Ord('t'), Ord('r'), Ord('u'), Ord('e'));
    BoolStrSizes : Array[Boolean] of Integer = (FalseBytesLen,TrueBytesLen);
  protected
    function AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean; override;
  public
    constructor Create(aValue: Boolean); overload;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
    property AsBoolean: Boolean read FValue;
  end;

  { TJSONTrue }

  TJSONTrue = class sealed(TJSONBool)
  public
    constructor Create; override;
    function Clone: TJSONAncestor; override;
  end;

  { TJSONFalse }

  TJSONFalse = class sealed(TJSONBool)
  public
    constructor Create; override;
    function Clone: TJSONAncestor; override;
  end;

  { TJSONPair }

  TJSONPair = class sealed(TJSONAncestor)
  private
    FJsonString: TJSONString;
    FJsonValue: TJSONValue;
  protected
    procedure AddDescendant(const aDescendant: TJSONAncestor); override;
    procedure SetJsonString(const aValue: TJSONString);
    procedure SetJsonValue(const aValue: TJSONValue);
    function HasName(const aName: UnicodeString): Boolean; inline;
  public
    constructor Create(const aStr: TJSONString; const aValue: TJSONValue); overload;
    constructor Create(const aStr: UnicodeString; const aValue: TJSONValue); overload;
    constructor Create(const aStr: UnicodeString; const aValue: UnicodeString); overload;
    constructor Create(const aStr: UnicodeString; const aValue: Int64); overload;
    constructor Create(const aStr: UnicodeString; const aValue: Integer); overload;
    constructor Create(const aStr: UnicodeString; const aValue: Double); overload;
    constructor Create(const aStr: UnicodeString; const aValue: Boolean); overload;
    constructor Create; overload; override;
    destructor Destroy; override;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Clone: TJSONAncestor; override;
    property JsonString: TJSONString read FJsonString write SetJsonString;
    property JsonValue: TJSONValue read FJsonValue write SetJsonValue;
  end;
  TJSONPairList = specialize TList<TJSONPair>;

  { TJSONObject }

  TJSONObject = class sealed(TJSONValue)
  public type

    { TEnumerator }

    TEnumerator = class
    private
      FCurrent: Integer;
      FObject: TJSONObject;
    public
      constructor Create(const aObject: TJSONObject);
      function GetCurrent: TJSONPair; inline;
      function MoveNext: Boolean; inline;
      property Current: TJSONPair read GetCurrent;
    end;

  private
    FList: TFPList;
  protected
    function IndexOfPairByName(const aPairName: UnicodeString): Integer;
    procedure AddDescendant(const aDescendant: TJSONAncestor); override;
    function GetCount: Integer; inline;
    function GetPair(const aIndex: Integer): TJSONPair; inline;
    function GetPairByName(const aPairName: UnicodeString): TJSONPair;
    procedure Format(aBuilder: TUnicodeStringBuilder; const aParentIndent, aIndent: UnicodeString); overload; override;
  public
    constructor Create; overload; override;
    constructor Create(const aPair: TJSONPair); overload;
    procedure Clear;

    function GetEnumerator: TEnumerator; inline;
    function GetValue(const aName: UnicodeString): TJSONValue; overload;
    destructor Destroy; override;
    function AddPair(const aPair: TJSONPair): TJSONObject; overload;
    function AddPair(const aStr: TJSONString; const aVal: TJSONValue): TJSONObject; overload;
    function AddPair(const aStr: UnicodeString; const aVal: TJSONValue): TJSONObject; overload;
    function AddPair(const aStr: UnicodeString; const aVal: UnicodeString): TJSONObject; overload;
    function AddPair(const aStr: UnicodeString; const aVal: Int64): TJSONObject; overload;
    function AddPair(const aStr: UnicodeString; const aVal: Integer): TJSONObject; overload;
    function AddPair(const aStr: UnicodeString; const aVal: Double): TJSONObject; overload;
    function AddPair(const aStr: UnicodeString; const aVal: Boolean): TJSONObject; overload;
    function RemovePair(const aPairName: UnicodeString): TJSONPair;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Clone: TJSONAncestor; override;
    function Parse(const aData: TByteDynArray; const aPos: Integer; aUseBool: Boolean = False): Integer; overload;
    function Parse(const aData: TByteDynArray; const aPos: Integer; const aCount: Integer; aUseBool: Boolean = False): Integer; overload;
    procedure SetPairs(const aList: TJSONPairList);
    property Count: Integer read GetCount;
    property Pairs[const aIndex: Integer]: TJSONPair read GetPair;
    property Values[const aName: UnicodeString]: TJSONValue read GetValue;
    function Size: Integer; inline; 
    function Get(const aIndex: Integer): TJSONPair; overload; inline; 
    function Get(const aName: UnicodeString): TJSONPair; overload; inline; 
  end;

  TJSONPairEnumerator = class(TJSONObject.TEnumerator)
  end;

  { TJSONArray }

  TJSONArray = class sealed(TJSONValue)
  public
    type

      { TEnumerator }

      TEnumerator = class
      private
        FCurrent: Integer;
        FArray: TJSONArray;
      public
        constructor Create(const aArray: TJSONArray);
        function GetCurrent: TJSONValue; inline;
        function MoveNext: Boolean; inline;
        property Current: TJSONValue read GetCurrent;
      end;

  private
    FList: TFPList;
    function GetValueA(const aIndex: Integer): TJSONValue; override;

  protected
    function AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean; override;
    procedure AddDescendant(const aDescendant: TJSONAncestor); override;
    function Pop: TJSONValue; inline;
    function GetValue(const aIndex: Integer): TJSONValue; overload; inline;
    function GetCount: Integer; inline;
    procedure Format(aBuilder: TUnicodeStringBuilder; const aParentIndent, aIndent: UnicodeString); overload; override;
  public
    Procedure Clear;
    constructor Create; overload; override;
    constructor Create(const aFirstElem: TJSONValue); overload;
    constructor Create(const aFirstElem: TJSONValue; const aSecondElem: TJSONValue); overload;
    constructor Create(const aFirstElem: UnicodeString; const aSecondElem: UnicodeString); overload;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const aIndex: Integer]: TJSONValue read GetValue; default;
    function Remove(aIndex: Integer): TJSONValue;
    procedure AddElement(const aElement: TJSONValue); inline;
    function Add(const aElement: UnicodeString): TJSONArray; overload;
    function Add(const aElement: Integer): TJSONArray; overload;
    function Add(const aElement: Double): TJSONArray; overload;
    function Add(const aElement: Boolean): TJSONArray; overload;
    function Add(const aElement: TJSONObject): TJSONArray; overload;
    function Add(const aElement: TJSONArray): TJSONArray; overload;
    function EstimatedByteSize: Integer; override;
    procedure SetElements(const aList: TJSONValueList);
    function ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer; override;
    procedure ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions); override;
    function Clone: TJSONAncestor; override;
    function GetEnumerator: TEnumerator; inline;
    function Size: Integer; inline; 
    function Get(const Index: Integer): TJSONValue; inline;
  end;

function GetJSONFormat: TFormatSettings;
function FloatToJson(const aValue: Double): UnicodeString;
function JsonToFloat(const aDotValue: UnicodeString): Double;
function TryJsonToFloat(const aDotValue: UnicodeString; var aValue: Double): Boolean;
function HexToDecimal(const aHex: Byte): Byte; inline;
function DecimalToHex(const aDecimal: Byte): Byte; inline;

const
  DefaultJSONBufSize = 4096;
  MaximumNestingLevel: Integer = 512;
  DecimalToHexMap: UnicodeString = '0123456789ABCDEF';
  HexToDecimalMap: array[Byte] of Byte = (
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {00-0F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {10 0F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {20-2F}
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0, {30-3F}
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0, {40-4F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {50-5F}
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0, {60-6F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {70-7F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {80-8F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {90-9F}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {A0-AF}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {B0-BF}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {C0-CF}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {D0-DF}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, {E0-EF}
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0); {F0-FF}

implementation


uses
{$IFDEF FPC_DOTTEDUNITS}
  Fcl.Streams.Extra, FpJson.Scanner, FpJson.Reader;
{$ELSE FPC_DOTTEDUNITS}
  streamex, jsonscanner, jsonreader;
{$ENDIF FPC_DOTTEDUNITS}

var
  _JSONSettings : TFormatSettings;
  NullByte : Byte = 0;
  NullWideChar : WideChar = #0;

Resourcestring
  SErrPathNotFound = 'No value found at path "%s"';
  SErrIndexNotFound = 'No value found at index %d';
  SErrInvalidFloatingPointValue = 'Invalid floating point value: %s';
  SErrJSONSyntaxError = 'Syntax error at ';
  SErrLocation = 'Path: %s, Line: %d, Position: %d, Offset: %d';
  SErrEmptyNameNotAllowed = 'Empty name not allowed in path at pos %d.';
  SErrInvalidIndexAt = 'Invalid index in path at %d';
  SErrCannotAddJSONValue = '%s: Cannot add descendent of type %s';
  SErrStructure = 'Structural error';

function GetJSONFormat: TFormatSettings;

begin
  Result:=_JSONSettings;
end;


function FloatToJson(const aValue: Double): UnicodeString;

begin

  Result:=FloatToStr(aValue,_JSONSettings);
end;


function JsonToFloat(const aDotValue: UnicodeString): Double;

begin
  Result:=StrToFloat(aDotValue,_JSONSettings);
end;


function TryJsonToFloat(const aDotValue: UnicodeString; var aValue: Double): Boolean;

begin
  Result:=TryStrToFloat(aDotValue,aValue,_JSONSettings);
end;


function HexToDecimal(const aHex: Byte): Byte; inline;

begin
  Result:=HexToDecimalMap[aHex];
end;


function DecimalToHex(const aDecimal: Byte): Byte; inline;

begin
  Result:=Byte(DecimalToHexMap[aDecimal+1]);
end;

{ TJSONParser }
Type
  TJSONParser = Class(TBaseJSONReader)
  private
    FStack : Array of TJSONValue;
    FStackPos : integer;
    FStream: TStream;
    FStructType : TJSONType;
    FStruct : TJSONValue;
    FUseBooleans: Boolean;
    FUseExceptions: Boolean;
    FValue : TJSONValue;
    FKey: UnicodeString;
    procedure Pop(aType: TJSONValueClass);
    Procedure Push(AValue : TJSONValue);
    Function NewValue(AValue : TJSONValue) : TJSONValue;
  Protected
    Procedure KeyValue(Const AKey : TJSONStringType); override;
    Procedure StringValue(Const AValue : TJSONStringType);override;
    Procedure NullValue; override;
    Procedure FloatValue(Const AValue : Double); override;
    Procedure BooleanValue(Const AValue : Boolean); override;
    Procedure NumberValue(Const AValue : TJSONStringType); override;
    Procedure IntegerValue(Const AValue : integer); override;
    Procedure Int64Value(Const AValue : int64); override;
    Procedure QWordValue(Const AValue : QWord); override;
    Procedure StartArray; override;
    Procedure StartObject; override;
    Procedure EndArray; override;
    Procedure EndObject; override;
  Public
    Constructor Create(aStream : TStream; aOptions: TJSONValue.TJSONParseOptions); reintroduce;
    Destructor Destroy; override;
    function Parse: TJSONValue;
    function ParseSingle : TJSONValue;
    Property UseBooleans : Boolean Read FUseBooleans;
    Property UseExceptions : Boolean Read FUseExceptions;
    Property Stream : TStream Read FStream; // Will be freed.
  end;

  { TPointerStream }

  TPointerStream = Class(TCustomMemoryStream)
    Constructor Create(aBytes : PByte; aPos,aSize : Integer);
  end;


{ TPointerStream }

constructor TPointerStream.Create(aBytes: PByte; aPos, aSize: Integer);
begin
  SetPointer(aBytes,aSize);
  SizeBoundsSeek:=True;
  Seek(aPos,soFromBeginning);
end;

Function CreateParser(aBytes : PByte; aOffset, aSize : Integer; Options: TJSONValue.TJSONParseOptions) : TJSONParser;

Var
  PS : TPointerStream;
  UseBools,Utf8 : Boolean;

begin
  PS:=TPointerStream.Create(aBytes,aOffset,aSize);
  Result:=TJSONParser.Create(PS,Options);
end;


{ TJSONByteReader }


function TJSONByteReader.GetOffset: Integer;
begin
  Result:=FCurrent-FData;
end;

constructor TJSONByteReader.Create(const aData: PByte; const aOffset: Integer; const aRange: Integer);
begin
  Create(aData,aOffset,aRange,True);
end;

constructor TJSONByteReader.Create(const aData: PByte; const aOffset: Integer; const aRange: Integer; const aIsUTF8: Boolean);
begin
  if (aData=nil) or (aOffset<0) or (aOffset>=aRange) then
    begin
    FData:=@NullByte;
    FCurrent:=@NullByte;
    FEnd:=@NullByte;
    end
  else
  begin
    FData:=aData;
    FCurrent:=FData+aOffset;
    FEnd:=FData+aRange-1;
  end;
  FIsUTF8:=aIsUTF8;
  // Check
  if (FCurrent<=FEnd-2) then
    begin
    if (FCurrent[0]=239) and (FCurrent[1]=187) and (FCurrent[2]=191) then
      Inc(FCurrent,3);
    end;
end;

destructor TJSONByteReader.Destroy;
begin
  inherited Destroy;
end;

function TJSONByteReader.ConsumeByte: Byte;
begin
  Result:=PeekByte;
  SkipByte;
end;

function TJSONByteReader.PeekRawByte: Byte;
begin
  Exit(FCurrent^);
end;

function TJSONByteReader.PeekByte: Byte;
begin
  Result:=PeekRawByte;
end;

procedure TJSONByteReader.SkipByte;
begin
  Inc(FCurrent);
end;

procedure TJSONByteReader.SkipWhitespaces;

Const
  WhiteSpaceBytes = [9,10,13,12,32];

begin
  while not IsEof and (PeekRawByte in WhiteSpaceBytes) do
    SkipByte;
end;

function TJSONByteReader.IsEof: Boolean;
begin
  Result:=(FCurrent>FEnd) or (FCurrent=@NullByte);
end;

function TJSONByteReader.HasMore(const aSize: Integer): Boolean;
begin
  Result:=(FEnd-FCurrent)>=aSize;
end;

procedure TJSONByteReader.AddChar(aCh: WideChar);
begin
  if (FStringLen=Length(FString)) then
    SetLength(FString,2*Length(FString));
  FString[FStringLen]:=aCh;
  Inc(FStringLen);
end;

procedure TJSONByteReader.ResetString;
begin
  FStringLen:=0;
end;

procedure TJSONByteReader.FlushString(out aDest: UnicodeString; aCache: Boolean);
begin
  SetLength(aDest,FStringLen);
  if FStringLen>0 then
    Move(FString[0],aDest[1],FStringLen);
  aCache:=Not aCache;
end;

procedure TJSONByteReader.OffsetToPos(aOffset: Integer; var aLine, APos: Integer);

var
  aDelta : Integer;
  B : Byte;
  PB : PByte;

begin
  PB:=FData;
  aLine:=1;
  aPos:=1;
  while (PB<=FEnd) and (aOffset>0) do
    begin
    B:=PB[0];
    aDelta:=1;
    if (B<128) then
      begin
      Case B of
      10:
        begin
        Inc(aLine);
        Inc(aDelta);
        end;
      13:
        begin
        Inc(aLine);
        Inc(aDelta);
        APos:=0;
        if (PB<FEnd) and (PB[1]=10) then
          Inc(aDelta);
        end;
      end;
      end
    else if (B and 224)=192 then
      Inc(aDelta)
    else if (B and 240)=224 then
      Inc(aDelta,2)
    else if (B and 248)=240 then
      Inc(aDelta,3);
    Inc(PB,aDelta);
    Inc(aPos);
    Dec(aOffset,aDelta);
    end;
end;

{ TJSONValue }

function TJSONValue.GetValueP(const aPath: UnicodeString): TJSONValue;
begin
  Result:=FindValue(aPath);
  if Result = nil then
    raise EJSONException.CreateFmt(SErrPathNotFound, [aPath]);
end;

function TJSONValue.GetValueA(const aIndex: Integer): TJSONValue;

begin
  Result:=FindValue({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.Format('[%s]',[aIndex]));
  if Result = nil then
    raise EJSONException.CreateFmt(SErrIndexNotFound,[aIndex]);
end;

function TJSONValue.AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean;
begin
  Result := True;
  case ATypeInfo^.Kind of
    tkClass:
      TValue.Make(@Self, ATypeInfo, AValue);
    else
      Result := False;
  end;
end;

class function TJSONValue.ParseJSONValue(const aData: PByte; const aOffset: Integer; const aLength: Integer;
  Options: TJSONParseOptions): TJSONValue;

Var
  JP : TJSONParser;

begin
  JP:=CreateParser(aData,aOffset,aLength,Options);
  try
    Result:=JP.Parse;
  finally
    JP.Free;
  end;
end;

class function TJSONValue.ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; aIsUTF8: Boolean): TJSONValue;
begin
  if aIsUTF8 then
    Result:=ParseJSONValue(PByte(aData),aOffset,Length(aData),[TJSONParseOption.IsUTF8])
  else
    Result:=ParseJSONValue(PByte(aData),aOffset,Length(aData),[]);
end;

class function TJSONValue.ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; aOptions: TJSONParseOptions
  ): TJSONValue;
begin
  Result:=ParseJSONValue(PByte(aData),aOffset,Length(aData),aOptions);
end;

class function TJSONValue.ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; const aLength: Integer;
  aIsUTF8: Boolean): TJSONValue;

begin
  if aIsUTF8 then
    Result:=ParseJSONValue(PByte(aData),aOffset,aLength,[TJSONParseOption.IsUTF8])
  else
    Result:=ParseJSONValue(PByte(aData),aOffset,aLength,[]);
end;

class function TJSONValue.ParseJSONValue(const aData: TByteDynArray; const aOffset: Integer; const aLength: Integer;
  aOptions: TJSONParseOptions): TJSONValue;
begin
  Result:=ParseJSONValue(PByte(aData),aOffset,aLength,aOptions)
end;

class function TJSONValue.ParseJSONValue(const aData: Unicodestring; aUseBool: Boolean; aRaiseExc: Boolean): TJSONValue;

var
  B : TBytes;
  O : TJSONParseOptions;
begin
  B:=TEncoding.UTF8.GetBytes(aData);
  O:=[];
  If aUseBool then
    Include(O,TJSONParseOPtion.UseBool);
  If aRaiseExc then
    Include(O,TJSONParseOPtion.RaiseExc);
  Result := ParseJSONValue(B,0,Length(B),O);
end;

class function TJSONValue.ParseJSONValue(const aData: UTF8String; aUseBool: Boolean; aRaiseExc: Boolean): TJSONValue;
var
  O : TJSONParseOptions;
begin
  O:=[];
  If aUseBool then
    Include(O,TJSONParseOPtion.UseBool);
  If aRaiseExc then
    Include(O,TJSONParseOPtion.RaiseExc);
  Result := ParseJSONValue(PByte(aData),0,Length(aData),O);
end;

class function TJSONValue.ParseJSONFragment(const aData: PByte; var aOffset: Integer; const aLength: Integer;
  aOptions: TJSONParseOptions): TJSONValue;

Var
  JP : TJSONParser;

begin
  JP:=CreateParser(aData,aOffset,aLength,aOptions);
  try
    Result:=JP.ParseSingle;
    aOffset:=JP.Scanner.AbsolutePos;
  finally
    JP.Free;
  end;
end;

class function TJSONValue.ParseJSONFragment(const aData: TByteDynArray; var aOffset: Integer; aOptions: TJSONParseOptions
  ): TJSONValue;
begin
  Result:=ParseJSONFragment(PByte(aData),aOffSet,Length(aData),aOptions);
end;

class function TJSONValue.ParseJSONFragment(const aData: UnicodeString; var aOffset: Integer; aOptions: TJSONParseOptions): TJSONValue;

var
  B : TBytes;

begin
  B:=TEncoding.UTF8.GetBytes(aData);
  Result:=ParseJSONFragment(PByte(B),aOffSet,Length(B),aOptions);
end;

function TJSONValue.FindValue(const aPath: UnicodeString): TJSONValue;

var
  Prs : TJSONPathParser;
  JO : TJSONObject Absolute Result;
  JA : TJSONArray Absolute Result;

begin
  Result:=Self;
  if (aPath='') or (Self=nil) then
    Exit;
  Prs:=TJSONPathParser.Create(aPath);
  while Assigned(Result) and not Prs.IsEof do
    case Prs.NextToken of
        TJSONPathToken.Eof:
          ;
        TJSONPathToken.Undefined,
        TJSONPathToken.Error:
          Result:=nil;
        TJSONPathToken.Name:
        // TJSONObject and TJSONArray are declared as sealed, so we don't need is/Inheritsfrom
        begin
        if (Result.ClassType=TJSONObject) then
          Result:=JO.Values[Prs.TokenName]
        else
          Result:=nil;
        end;
      TJSONPathToken.ArrayIndex:
        begin
        if (Result.ClassType=TJSONArray)
           and (Prs.TokenArrayIndex>=0)
           and (Prs.TokenArrayIndex<JA.Count) then
          Result:=JA.Items[Prs.TokenArrayIndex]
        else
          Result:=nil;
        end;
    end;
end;

generic function TJSONValue.TryGetValue<T>(out aValue: T): Boolean;

begin

end;

generic function TJSONValue.TryGetValue<T>(const aPath: UnicodeString; out aValue: T): Boolean; overload;

begin

end;

generic function TJSONValue.GetValue<T>(const aPath: UnicodeString = ''): T; overload;

begin

end;

generic function TJSONValue.GetValue<T>(const aPath: UnicodeString; aDefaultValue: T): T; overload;

begin

end;

generic function TJSONValue.AsType<T> : T;
begin

end;

class function TJSONValue.ParseJSONValueUTF8(const aData: TByteDynArray; const aOffset: Integer; const aCount: Integer): TJSONValue;

var
  O : TJSONParseOptions;
begin
  O:=[TJSONParseOption.IsUTF8];
  Result := ParseJSONValue(PByte(aData),aOffset,aCount,O);
end;

class function TJSONValue.ParseJSONValueUTF8(const aData: TByteDynArray; const aOffset: Integer): TJSONValue;
var
  O : TJSONParseOptions;
begin
  O:=[TJSONParseOption.IsUTF8];
  Result := ParseJSONValue(PByte(aData),aOffset,Length(aData),O);
end;

{ EJSONParseException }

constructor EJSONParseException.Create(aOffset: Integer; aReader: TJSONByteReader; aValue: TJSONValue; aIndent: PResStringRec;
  const aArgs: array of const);

var
  V : TJSONValue;
  VO : TJSONObject absolute V;
  VA : TJSONArray absolute V;
  Pair : TJSONPair;
  lPath : String;

  Procedure AddToPath(S : String); inline;
  begin
    if lPath<>'' then
      lPath:=lPath+'.';
    lPath:=lPath+S;
  end;

begin
  lPath:='';
  FOffset:=aOffset;
  aReader.OffsetToPos(Abs(aOffset),FLine,FPosition);
  V:=aValue;
  While Assigned(V) do
    begin
    if (V is TJSONObject) and (VO.Count>0) then
      begin
      Pair:=VO.Pairs[VO.Count-1];
      AddToPath(Pair.JsonString.Value);
      V:=Pair.JsonValue;
      end
    else if (V is TJSONArray) and (VA.Count>0) then
      begin
      AddToPath(Format('[%d]',[VA.Count-1]));
      V:=Va[Va.Count-1];
      end
    else
      V:=nil;
    end;
  Create(SafeFormat(aIndent^,aArgs)+SafeFormat(SErrLocation,[lPath,FLine,FPosition,FOffset]));
end;

constructor EJSONParseException.Create(aOffset: Integer; aReader: TJSONByteReader; aValue: TJSONValue);
begin
  Create(aOffset,aReader,AValue,@SErrJSONSyntaxError,[]);
end;

{ TJSONPathParser }

function TJSONPathParser.GetIsEof: Boolean;
begin
  Result:=(FCurrent^=#0)
end;

constructor TJSONPathParser.Create(const aPath: UnicodeString);
begin
  Create(PWideChar(aPath),Length(aPath))
end;


constructor TJSONPathParser.Create(const aPath: PWideChar; aLen: Integer);
begin
  FPath:=aPath;
  FCurrent:=FPath;
  FEnd:=@NullWideChar
end;

function TJSONPathParser.NextToken: TToken;

  Procedure Error(const Fmt : UnicodeString; Args : Array of const);

  begin
    FToken:=TToken.Error;
    raise EJSONException.CreateFmt(Fmt,Args);
  end;

  Procedure SkipWhiteSpace;
  begin
    While (Ord(FCurrent^)<>0) and (Ord(FCurrent^)<=32) do
      Inc(FCurrent)
  end;

  Procedure MoveToName(PStart : PWideChar; aLen : Integer);

  var
    PName : PWideChar;

  begin
    SetLength(FTokenName,aLen);
    PName:=PWideChar(FTokenName);
    // Only called with aLen>0
    Move(PStart^,PName^,aLen*SizeOf(UnicodeChar));
  end;

  Procedure ParseName(EndAt : TSysCharSet; SkipFirst : Boolean = True; IsQuoted : Boolean = False);
  var
    PStart : PWideChar;
    Len : Integer;

  begin
    if SkipFirst then
      Inc(FCurrent); // Skip dot or quote
    SkipWhiteSpace;
    PStart:=FCurrent;
    // Endat always contains #0
    While not CharInSet(FCurrent^,EndAt) do
      Inc(FCurrent);
    Len:=FCurrent-PStart;
    if IsQuoted then
      Inc(FCurrent); // Move to character after quote
    if Len=0 then
      Error(SErrEmptyNameNotAllowed, [PStart-FPath]);

    MoveToName(PStart,Len);
    FToken:=TToken.Name;
  end;

  Procedure ParseIndex();
  var
    PStart : PWideChar;
    Len : Integer;

  begin
    PStart:=FCurrent;
    if FCurrent^='-' then
      Inc(FCurrent);
    While (FCurrent^ in ['0'..'9']) do
      Inc(FCurrent);
    if FCurrent^<>']' then
      Error(SErrInvalidIndexAt, [PStart-FPath]);
    Len:=FCurrent-PStart;
    if Len=0 then
      Error(SErrEmptyNameNotAllowed, [PStart-FPath]);
    MoveToName(PStart,Len);
    FToken:=TToken.ArrayIndex;
    FTokenArrayIndex:=StrToInt(FTokenName);
    Inc(FCurrent);
  end;


begin
  SkipWhiteSpace;
  FToken:=TToken.EOF;
  if IsEof then exit(FToken);
  If FCurrent^='.' then
    ParseName([#0,'.'])
  else if FCurrent^='[' then
    begin
    Inc(FCurrent);
    SkipWhiteSpace;
    if FCurrent^ in [#39,'"'] then
      begin
      ParseName([#0,AnsiChar(Ord(FCurrent^))],True,True);
      SkipWhiteSpace;
      if (FCurrent^<>']') then
        Error(SErrInvalidIndexAt, [FCurrent-FPath]);
      Inc(FCurrent);
      end
    else
      ParseIndex;
    end
  else
    ParseName([#0,'.'],False);
  Result:=FToken;
end;

{ TJSONAncestor }

function TJSONAncestor.IsNull: Boolean;
begin
  Result:=False;
end;

procedure TJSONAncestor.AddDescendant(const aDescendent: TJSONAncestor);
begin
  raise EJSONException.CreateFmt(SErrCannotAddJSONValue,[ClassName,aDescendent.ClassName]);
end;

function TJSONAncestor.Format(aIndentation: Integer): UnicodeString;

var
  Bld: TUnicodeStringBuilder;
begin
  Bld:=TUnicodeStringBuilder.Create(DefaultJSONBufSize);
  try
    Format(Bld,'',StringOfChar(' ',aIndentation));
    Result:=Bld.ToString;
  finally
    Bld.Free;
  end;
end;

procedure TJSONAncestor.Format(aBuilder: TUnicodeStringBuilder; const aParentIndent, aIndent: UnicodeString);


begin
  ToChars(aBuilder,[]);
end;

constructor TJSONAncestor.Create;
begin
  FOwned:=True;
end;

function TJSONAncestor.Value: UnicodeString;
begin
  Result:='';
end;

function TJSONAncestor.ToBytes(const aData: TBytes; aOffset: Integer): Integer;
begin
  Result:=ToBytes(aData,aOffset,DefaultToBytesOptions)
end;

function TJSONAncestor.ToJSON(aOptions: TJSONOutputOptions): UnicodeString;

var
  Bld: TUnicodeStringBuilder;

begin
  Bld:=TUnicodeStringBuilder.Create(DefaultJSONBufSize);
  try
    ToChars(Bld,aOptions);
    Result:=Bld.ToString;
  finally
    Bld.Free;
  end;
end;

function TJSONAncestor.ToJSON: UnicodeString;
begin
  Result:=ToJSON(DefaultToBytesOptions);
end;

function TJSONAncestor.ToString: RTLString;

begin
{$IF SIZEOF(CHAR)=1}
  Result:=UTF8Encode(ToJSON([]));
{$ELSE}
  Result:=ToJSON([]);
{$ENDIF}
end;


{ TJSONString }

function TJSONString.IsNull: Boolean;
begin
  Result:=FIsNull;
end;

function TJSONString.AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean;
begin
  Result:=inherited AsTValue(aTypeInfo, aValue);
end;

constructor TJSONString.Create;
begin
  inherited Create;
  FIsNull:=True;
end;

constructor TJSONString.Create(const aValue: UnicodeString);
begin
  inherited Create;
  FValue:=aValue;
  FIsNull:=False;
end;

constructor TJSONString.Create(const aValue: UTF8String);
begin
  Create(UTF8Decode(aValue));
end;

procedure TJSONString.AddChar(const aChar: WideChar);
begin
  FValue:=FValue+aChar;
  FIsNull:=False;
end;

function TJSONString.Equals(const aValue: UnicodeString): Boolean;
begin
  Result:=(aValue=FValue)
end;

function TJSONString.EstimatedByteSize: Integer;
begin
  if FIsNull then
    Result:=4
  else
    Result:=6*Length(FValue)+2;
end;

function MoveAnsiChar(const aChar : AnsiChar; aData : TByteDynArray; var aOffset :Integer): Integer; inline;
begin
  Result:=Sizeof(AnsiChar);
  Move(aChar,aData[aOffset],Result);
  Inc(aOffset,Result);
end;

function MoveRawString(const aString : RawByteString; aData : TByteDynArray; var aOffset :Integer) : Integer; inline;


begin
  Result:=Length(aString);
  if Result>0 then
    Move(aString[1],aData[aOffset],Result);
  Inc(aOffset,Result);
end;

function TJSONString.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;

  procedure Escape(aChar : ansichar); inline;
  begin
    MoveAnsiChar('\',aData,Result);
    MoveAnsiChar(aChar,aData,Result);
  end;

var
  C : UnicodeChar;
  W : Word absolute C;

begin
  Result:=aOffset;
  if FisNull then
    begin
    MoveRawString('null',aData,Result);
    Exit;
    end;
  MoveAnsiChar('"',aData,Result);
  for C in FValue do
    Case C of
      '"','\','/' : Escape(AnsiChar(W));
      #8 : Escape('t');
      #9 : Escape('t');
      #10 : Escape('n');
      #12 : Escape('f');
      #13 : Escape('r');
    else
      if (W>=32) and (W<=127) then
        begin
        aData[Result]:=Ord(W);
        Inc(Result);
        end
      else if (TJSONOutputOption.EncodeBelow32 in aOptions) and (W<32) then
        MoveAnsiChar(AnsiChar(W),aData,Result)
      else if (TJSONOutputOption.EncodeAbove127 in aOptions) and (W>127) then
        MoveAnsiChar(AnsiChar(W),aData,Result)
      else
        begin
        Escape('u');
        MoveRawString(BinStr(W,4),aData,Result);
        end;
    end;
  MoveAnsiChar('"',aData,Result);
end;

procedure TJSONString.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);

var
  Len : Integer;
  B : TBytes;
  S : UTF8String;

begin
  Len:=EstimatedByteSize;
  SetLength(B,Len);
  Len:=ToBytes(B,0,aOptions);
  S:=TEncoding.UTF8.GetAnsiString(B,0,Len);
  aBuilder.Append(S);
end;

function TJSONString.Value: UnicodeString;
begin
  Result:=FValue;
end;

function TJSONString.Clone: TJSONAncestor;
begin
  if IsNull then
    Result:=TJSONString.Create
  else
    Result:=TJSONString.Create(Self.Value);
end;

{ TJSONNumber }

function TJSONNumber.GetAsDouble: Double;
begin
  Result:=JsonToFloat(Value);
end;

function TJSONNumber.GetAsInt: Integer;
begin
  {$IFDEF SIZEOF(Char)=2}
  Result:=StrToInt(Value);
  {$ELSE}
  Result:=StrToInt(UTF8Encode(Value));
  {$ENDIF}
end;

function TJSONNumber.GetAsInt64: Int64;
begin
  {$IFDEF SIZEOF(Char)=2}
  Result:=StrToInt64(Value);
  {$ELSE}
  Result:=StrToInt(UTF8Encode(Value));
  {$ENDIF}
end;

constructor TJSONNumber.Create(const aValue: UnicodeString);

var
  v : UnicodeString;
  D : Double;
begin
  D:=0;
  V:=trim(aValue);
  if (V<>'') and TryJsonToFloat(V,D) then
    Inherited Create(V)
  else
    raise EJSONException.CreateFmt(SErrInvalidFloatingPointValue, [aValue]);
end;

constructor TJSONNumber.Create(const aValue: Double);
begin
  Inherited Create(FloatToJson(aValue))
end;

constructor TJSONNumber.Create(const aValue: Integer);
begin
  Inherited Create(IntToStr(aValue));
end;

constructor TJSONNumber.Create(const aValue: Int64);
begin
  Inherited Create(IntToStr(aValue));
end;

function TJSONNumber.EstimatedByteSize: Integer;
begin
  Result:=Length(FValue);
end;

function TJSONNumber.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;

var
  C : UnicodeChar;

begin
  Result:=aOffset;
  For C in Value do
    MoveAnsiChar(AnsiChar(Ord(C)),aData,Result);
end;

procedure TJSONNumber.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);
begin
  inherited ToChars(aBuilder, aOptions);
end;

function TJSONNumber.Clone: TJSONAncestor;
begin
  Result:=inherited Clone;
end;

{ TJSONNull }

function TJSONNull.AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean;
begin
  if ATypeInfo^.Kind in [system.tkAString,system.tkLString,system.tkUString,system.tkWString] then
    begin
    AValue:='';
    Result := True;
    end
  else
    Result:=inherited AsTValue(aTypeInfo,aValue);
end;

function TJSONNull.IsNull: Boolean;
begin
  Result:=True;
end;

function TJSONNull.EstimatedByteSize: Integer;
begin
  Result:=4;
end;

function TJSONNull.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;
begin
  MoveRawString(NullString,aData,aOffset);
  Result:=aOffset;
end;

procedure TJSONNull.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);
begin
  aBuilder.Append(NULLString);
end;

function TJSONNull.Value: UnicodeString;
begin
  Result:=NullString;
end;

function TJSONNull.Clone: TJSONAncestor;
begin
  Result:=TJSONNull.Create;
end;

{ TJSONBool }

function TJSONBool.AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean;
begin
  Result:=inherited AsTValue(aTypeInfo, aValue);
end;

constructor TJSONBool.Create(aValue: Boolean);
begin
  inherited Create;
  FValue:=aValue;
end;


function TJSONBool.EstimatedByteSize: Integer;

begin
  Result:=BoolStrSizes[FValue];
end;

function TJSONBool.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;

var
  B : PByte;
  L : Integer;

begin
  L:=BoolStrSizes[FValue];
  if FValue then
    B:=@TrueBytes
  else
    B:=@FalseBytes;
  Move(B^,aData[aOffset],L);
  Result:=aOffset+L;
end;

procedure TJSONBool.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);
begin
  aBuilder.Append(Value);
end;

function TJSONBool.Value: UnicodeString;
begin
  if FValue then
    Result:=TrueString
  else
    Result:=FalseString;
end;

function TJSONBool.Clone: TJSONAncestor;
begin
  Result:=TJSONBool.Create(Self.FValue);
end;

{ TJSONTrue }

constructor TJSONTrue.Create;
begin
  inherited Create(True);
end;

function TJSONTrue.Clone: TJSONAncestor;
begin
  Result:=TJSONTrue.Create;
end;

{ TJSONFalse }

constructor TJSONFalse.Create;
begin
  inherited Create(False);
end;

function TJSONFalse.Clone: TJSONAncestor;
begin
  Result:=TJSONFalse.Create;
end;

{ TJSONPair }

procedure TJSONPair.AddDescendant(const aDescendant: TJSONAncestor);

begin
  if not Assigned(FJsonString) then
    FJsonString:=aDescendant as TJSONString
  else if not Assigned(FJsonValue) then
    FJsonValue:=aDescendant as TJSONValue
  else
    inherited;
end;

procedure TJSONPair.SetJsonString(const aValue: TJSONString);
begin
  if aValue=Nil then
    exit;
  if Assigned(FJsonString) and (FJsonString.Owned) then
    FreeAndNil(FJsonString);
  FJsonString:=aValue;
end;

procedure TJSONPair.SetJsonValue(const aValue: TJSONValue);
begin
  if aValue=Nil then
    exit;
  if Assigned(FJsonValue) and (FJsonValue.Owned) then
    FreeAndNil(FJsonValue);
  FJsonValue:=aValue;
end;

function TJSONPair.HasName(const aName: UnicodeString): Boolean;
begin
  Result:=Assigned(FJsonString) and (Not FJsonString.FIsNull) and (FJsonString.Value=aName);
end;

constructor TJSONPair.Create(const aStr: TJSONString; const aValue: TJSONValue);
begin
  Inherited Create;
  FJsonString:=aStr;
  FJsonValue:=aValue;
end;

constructor TJSONPair.Create(const aStr: UnicodeString; const aValue: TJSONValue);
begin
  Create(TJSONString.Create(aStr),aValue);
end;

constructor TJSONPair.Create(const aStr: UnicodeString; const aValue: UnicodeString);
begin
  Create(TJSONString.Create(aStr),TJSONString.Create(aValue));
end;

constructor TJSONPair.Create(const aStr: UnicodeString; const aValue: Int64);
begin
  Create(TJSONString.Create(aStr),TJSONNumber.Create(aValue));
end;

constructor TJSONPair.Create(const aStr: UnicodeString; const aValue: Integer);
begin
  Create(TJSONString.Create(aStr),TJSONNumber.Create(aValue));
end;

constructor TJSONPair.Create(const aStr: UnicodeString; const aValue: Double);
begin
  Create(TJSONString.Create(aStr),TJSONNumber.Create(aValue));
end;

constructor TJSONPair.Create(const aStr: UnicodeString; const aValue: Boolean);
begin
  Create(TJSONString.Create(aStr),TJSONBool.Create(aValue));
end;

constructor TJSONPair.Create;
begin
  inherited Create;
end;

destructor TJSONPair.Destroy;
begin
  JSonString:=nil;
  JsonValue:=nil;
  inherited Destroy;
end;

function TJSONPair.EstimatedByteSize: Integer;
begin
  // name:value
  Result:=FJsonString.EstimatedByteSize+1+FJsonValue.EstimatedByteSize;
end;

function TJSONPair.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;
begin
  if assigned(JSONString) then
    aOffset:=JsonString.ToBytes(aData,aOffset,aOptions);
  aData[aOffset]:=Ord(':');
  inc(aOffset);
  Result:=FJsonValue.ToBytes(aData,aOffset,aOptions);
end;

procedure TJSONPair.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);
begin
  if Assigned(FJsonString) then
    FJsonString.ToChars(aBuilder,aOptions);
  aBuilder.Append(':');
  if Assigned(FJsonValue) then
    FJsonValue.ToChars(aBuilder,aOptions);
end;

function TJSONPair.Clone: TJSONAncestor;

var
  S : TJSONString;
  V : TJSONValue;

begin
  S:=Nil;
  V:=Nil;
  if Assigned(FJsonString) then
    S:=FJsonString.Clone as TJSONString;
  if Assigned(FJsonValue) then
    V:=FJsonValue.Clone as TJSONValue;
  Result:=TJSONPair.Create(S,V);
end;

{ TJSONObject }


function TJSONObject.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TJSONObject.Get(const aIndex: Integer): TJSONPair;
begin
  Result:=TJSONPair(Flist[aIndex]);
end;

function TJSONObject.IndexOfPairByName(const aPairName: UnicodeString): Integer;

var
  I: Integer;
  JP : TJSONPair;
begin
  for i:=0 to Count-1 do
    begin
    JP:=Get(I);
    if JP.HasName(aPairName) then
      Exit(I);
    end;
  Result:=-1
end;

procedure TJSONObject.AddDescendant(const aDescendant: TJSONAncestor);
begin
  if (aDescendant is TJSONPair) then
    AddPair(TJSONPair(aDescendant));
end;


function TJSONObject.GetPair(const aIndex: Integer): TJSONPair;
begin
  Result:=TJSONPair(Flist[aIndex]);
end;


function TJSONObject.GetPairByName(const aPairName: UnicodeString): TJSONPair;

var
  Idx: Integer;

begin
  Idx:=IndexOfPairByName(aPairName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=Get(Idx);
end;


procedure TJSONObject.Format(aBuilder: TUnicodeStringBuilder; const aParentIndent, aIndent: UnicodeString);
var
  S: UnicodeString;
  i,Len: Integer;

begin
  aBuilder.Append('{').Append(sLineBreak);
  S:=aParentIndent+aIndent;
  Len:=Count-1;
  for i:=0 to Len do
    begin
    if I>0 then
      begin
      aBuilder.Append(',');
      aBuilder.Append(sLineBreak);
      end;
    aBuilder.Append(S);
    Get(i).Format(aBuilder,S,aIndent);
    end;
  if Len>=0 then
    aBuilder.Append(sLineBreak);
  aBuilder.Append(aParentIndent);
  aBuilder.Append('}');
end;

constructor TJSONObject.Create;

begin
  inherited Create;
  FList:=TFPList.Create;
end;


constructor TJSONObject.Create(const aPair: TJSONPair);

begin
  Create;
  AddPair(aPair);
end;


procedure TJSONObject.Clear;

var
  I : Integer;
  JP : TJSONPair;

begin
  For I:=0 to FList.Count-1 do
    begin
    JP:=TJSONPair(FList[I]);
    if JP.Owned then
      JP.Free;
    FList[I]:=Nil;
    end;
  FList.Clear;
end;


function TJSONObject.GetEnumerator: TEnumerator;

begin
  Result:=TEnumerator.Create(Self);
end;


function TJSONObject.GetValue(const aName: UnicodeString): TJSONValue;

var
  JP : TJSONPair;

begin
  JP:=GetPairByName(aName);
  if Assigned(JP) then
    Result:=JP.FJsonValue
  else
    Result:=Nil;
end;


destructor TJSONObject.Destroy;

begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;


function TJSONObject.AddPair(const aPair: TJSONPair): TJSONObject;

begin
  if aPair<>Nil then
    FList.Add(aPair);
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: TJSONString; const aVal: TJSONValue): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: UnicodeString; const aVal: TJSONValue): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: UnicodeString; const aVal: UnicodeString): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: UnicodeString; const aVal: Int64): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: UnicodeString; const aVal: Integer): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: UnicodeString; const aVal: Double): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.AddPair(const aStr: UnicodeString; const aVal: Boolean): TJSONObject;

begin
  AddPair(TJSONPair.Create(aStr,aVal));
  Result:=Self;
end;


function TJSONObject.RemovePair(const aPairName: UnicodeString): TJSONPair;

var
  Idx : Integer;

begin
  Idx:=IndexOfPairByName(aPairName);
  if Idx=-1 then
    Result:=nil
  else
    begin
    Result:=Get(Idx);
    FList.Delete(Idx);
    end;
end;


function TJSONObject.EstimatedByteSize: Integer;

var
  I: Integer;

begin
  Result:=2;
  for i:=0 to Count-1 do
    Inc(Result,1+TJSONAncestor(FList[I]).EstimatedByteSize);
  if Count>0 then
    Dec(Result);
end;


function TJSONObject.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;

var
  I: Integer;

begin
  Result:=aOffSet;
  MoveAnsiChar('{',aData,Result);
  for i:=0 to Count-1 do
    begin
    if I>0 then
      MoveAnsiChar(',',aData,Result);
    Get(I).ToBytes(aData,Result,aOptions);
    end;
  MoveAnsiChar('}',aData,Result);
end;


procedure TJSONObject.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);

var
  I: Integer;

begin
  aBuilder.Append('{');
  for i:=0 to Count-1 do
    begin
    if I>0 then
      aBuilder.Append(',');
    Get(I).ToChars(aBuilder,aOptions);
    end;
  aBuilder.Append('}');
end;


function TJSONObject.Clone: TJSONAncestor;

var
  i : Integer;

begin
  Result:=TJSONObject.Create;
  For I:=0 to FList.Count-1 do
    Result.AddDescendant(TJSONAncestor(Flist[i]).Clone);
end;


function TJSONObject.Parse(const aData: TByteDynArray; const aPos: Integer; aUseBool: Boolean): Integer;

begin
  Result:=Parse(aData,aPos,Length(aData),aUseBool);
end;


function TJSONObject.Parse(const aData: TByteDynArray; const aPos: Integer; const aCount: Integer; aUseBool: Boolean): Integer;

var
  V : TJSONValue;
  O : TJSONObject absolute v;

begin
  V:=TJSONValue.ParseJSONValue(aData,aPos,aCount,[TJSONParseOption.UseBool]);
  if not (V is TJSONObject) then
    begin
    V.Free;
    Raise EJSONParseException.Create('JSON is not an object');
    end;
  FreeAndNil(FList);
  FList:=O.FList;
  O.FList:=TFPList.Create;
  V.Free;
end;


procedure TJSONObject.SetPairs(const aList: TJSONPairList);

var
  I : Integer;

begin
  Clear;
  For I:=0 to aList.Count-1 do
    AddPair(aList[i]);
  aList.Free;
end;


function TJSONObject.Size: Integer;

begin
  Result:=Count;
end;


function TJSONObject.Get(const aName: UnicodeString): TJSONPair;

begin
  Result:=GetPairByName(aName);
end;


{ TJSONObject.TEnumerator }

constructor TJSONObject.TEnumerator.Create(const aObject: TJSONObject);

begin
  FObject:=aObject;
  FCurrent:=-1;
end;


function TJSONObject.TEnumerator.GetCurrent: TJSONPair;

begin
  Result:=FObject.Get(FCurrent);
end;


function TJSONObject.TEnumerator.MoveNext: Boolean;

begin
  Inc(FCurrent);
  Result:=FCurrent<FObject.Count;
end;


{ TJSONArray }

function TJSONArray.GetValueA(const aIndex: Integer): TJSONValue;

begin
  Result:=inherited GetValueA(aIndex);
end;


function TJSONArray.AsTValue(aTypeInfo: PTypeInfo; var aValue: TValue): Boolean;

begin
  Result:=inherited AsTValue(aTypeInfo, aValue);
end;


function TJSONArray.Get(const Index: Integer): TJSONValue;

begin
  Result:=TJSONValue(FList[Index]);
end;


function TJSONArray.GetCount: Integer;

begin
  Result:=FList.Count;
end;


function TJSONArray.Size: Integer;

begin
  Result:=FList.Count;
end;


function TJSONArray.Pop: TJSONValue;

begin
  if Count>0 then
    Result:=Remove(0)
  else
    Result:=Nil;
end;


function TJSONArray.GetValue(const aIndex: Integer): TJSONValue;

begin
  if (aIndex<0) or (aIndex>=Count) then
    Result:=TJSONNull.Create
  else
    Result:=TJSONValue(FList[aIndex])
end;


procedure TJSONArray.Format(aBuilder: TUnicodeStringBuilder; const aParentIndent, aIndent: UnicodeString);

var
  S: UnicodeString;
  i,Len: Integer;

begin
  aBuilder.Append('[').Append(sLineBreak);
  S:=aParentIndent+aIndent;
  Len:=Count-1;
  for i:=0 to Len do
    begin
    if I>0 then
      begin
      aBuilder.Append(',');
      aBuilder.Append(sLineBreak);
      end;
    aBuilder.Append(S);
    Items[i].Format(aBuilder,S,aIndent);
    end;
  if Len>=0 then
    aBuilder.Append(sLineBreak);
  aBuilder.Append(aParentIndent);
  aBuilder.Append(']');
end;


procedure TJSONArray.Clear;

var
  I : Integer;
  JA : TJSONAncestor;

begin
  For I:=0 to FList.Count-1 do
    begin
    JA:=TJSONAncestor(FList[I]);
    if JA.Owned then
      JA.Free;
    FList[I]:=Nil;
    end;
  FList.Clear;
end;


constructor TJSONArray.Create;

begin
  inherited Create;
  FList:=TFPList.Create;
end;


procedure TJSONArray.AddElement(const aElement: TJSONValue);

var
  E : TJSONValue;
begin
  E:=aElement;
  if E=Nil then
    E:=TJSONNull.Create;
  FList.Add(aElement);
end;


constructor TJSONArray.Create(const aFirstElem: TJSONValue);

begin
  Create;
  AddElement(aFirstElem);
end;


constructor TJSONArray.Create(const aFirstElem: TJSONValue; const aSecondElem: TJSONValue);

begin
  Create;
  AddElement(aFirstElem);
  AddElement(aSecondElem);
end;

constructor TJSONArray.Create(const aFirstElem: UnicodeString; const aSecondElem: UnicodeString);

begin
  Create;
  AddElement(TJSONString.Create(aFirstElem));
  AddElement(TJSONString.Create(aSecondElem));
end;

destructor TJSONArray.Destroy;

begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;


function TJSONArray.Remove(aIndex: Integer): TJSONValue;

begin
  Result:=TJSONValue(FList[aIndex]);
  FList.Delete(aIndex);
end;


procedure TJSONArray.AddDescendant(const aDescendant: TJSONAncestor);

begin
  if (aDescendant is TJSONValue) then
    AddElement(TJSONValue(aDescendant));
end;


function TJSONArray.Add(const aElement: UnicodeString): TJSONArray;

begin
  AddElement(TJSONString.Create(aElement));
  Result:=Self;
end;


function TJSONArray.Add(const aElement: Integer): TJSONArray;

begin
  AddElement(TJSONNumber.Create(aElement));
  Result:=Self;
end;


function TJSONArray.Add(const aElement: Double): TJSONArray;

begin
  AddElement(TJSONNumber.Create(aElement));
  Result:=Self;
end;


function TJSONArray.Add(const aElement: Boolean): TJSONArray;

begin
  AddElement(TJSONBool.Create(aElement));
  Result:=Self;
end;


function TJSONArray.Add(const aElement: TJSONObject): TJSONArray;

begin
  AddElement(aElement);
  Result:=Self;
end;


function TJSONArray.Add(const aElement: TJSONArray): TJSONArray;

begin
  AddElement(aElement);
  Result:=Self;
end;


function TJSONArray.EstimatedByteSize: Integer;

var
  I : Integer;

begin
  Result:=2;
  For I:=0 to Count-1 do
    Result:=Result+(1+Get(I).EstimatedByteSize);
  if Count>0 then
    Dec(Result);
end;


procedure TJSONArray.SetElements(const aList: TJSONValueList);

var
  I : Integer;

begin
  Clear;
  For I:=0 to aList.Count-1 do
    AddElement(aList[i]);
  aList.Free;
end;


function TJSONArray.ToBytes(const aData: TByteDynArray; aOffset: Integer; aOptions: TJSONAncestor.TJSONOutputOptions): Integer;

var
  I: Integer;

begin
  MoveAnsiChar('[',aData,aOffset);
  for I:=0 to FList.Count-1 do
    begin
    if I>0 then
      MoveAnsiChar(',',aData,aOffset);
    aOffset:=Get(i).ToBytes(aData,aOffset,aOptions);
    end;
  MoveAnsiChar(']',aData,aOffset);
  Result:=aOffset;
end;


procedure TJSONArray.ToChars(aBuilder: TUnicodeStringBuilder; aOptions: TJSONAncestor.TJSONOutputOptions);

var
  I: Integer;

begin
  aBuilder.Append('[');
  for I:=0 to FList.Count-1 do
    begin
    if I>0 then
      aBuilder.Append(',');
    Get(i).ToChars(aBuilder,aOptions);
    end;
  aBuilder.Append(']');
end;


function TJSONArray.Clone: TJSONAncestor;

var
  I : Integer;

begin
  Result:=TJSONArray.create;
  For I:=0 to Size-1 do
    Result.AddDescendant(Get(I).Clone);
end;


function TJSONArray.GetEnumerator: TEnumerator;

begin
  Result:=TEnumerator.Create(Self);
end;



{ TJSONArray.TEnumerator }

constructor TJSONArray.TEnumerator.Create(const aArray: TJSONArray);

begin
  FArray:=aArray;
  FCurrent:=-1;
end;


function TJSONArray.TEnumerator.GetCurrent: TJSONValue;

begin
  Result:=FArray.Items[FCurrent];
end;


function TJSONArray.TEnumerator.MoveNext: Boolean;

begin
  Inc(FCurrent);
  Result:=FCurrent<FArray.Count;
end;


{ TJSONParser }


procedure TJSONParser.Pop(aType: TJSONValueClass);

begin
  if (FStackPos=0) then
    DoError(SErrStructure);
  If (FStruct.Classtype<>aType) then
    DoError(SErrStructure);
  Dec(FStackPos);
  FStruct:=FStack[FStackPos];
end;

procedure TJSONParser.Push(AValue: TJSONValue);

begin
  if (FStackPos=Length(FStack)) then
    SetLength(FStack,FStackPos+10);
  FStack[FStackPos]:=FStruct;
  Inc(FStackPos);
  FStruct:=AValue;
end;

function TJSONParser.NewValue(AValue: TJSONValue): TJSONValue;
begin
  Result:=AValue;
  aValue.Owned:=True;
  // Add to existing structural type
  if (FStruct is TJSONObject) then
    begin
    try
      TJSONObject(FStruct).AddPair(FKey,AValue);
    except
      AValue.Free;
      Raise;
    end;
    FKey:='';
    end
  else if (FStruct is TJSONArray) then
    TJSONArray(FStruct).AddElement(aValue);
  // The first actual value is our result
  if (FValue=Nil) then
    FValue:=AValue;
end;

procedure TJSONParser.KeyValue(const AKey: TJSONStringType);
begin
  if (FStruct is TJSONObject) and (FKey='') then
    FKey:=UTF8Decode(Akey)
  else
    DoError('Duplicatekey or no object');
end;

procedure TJSONParser.StringValue(const AValue: TJSONStringType);
begin
  NewValue(TJSONString.Create(AValue));
end;

procedure TJSONParser.NullValue;
begin
  NewValue(TJSONNull.Create);
end;

procedure TJSONParser.FloatValue(const AValue: Double);
begin
  NewValue(TJSONNumber.Create(AValue));
end;

procedure TJSONParser.BooleanValue(const AValue: Boolean);

var
  B : TJSONBool;
begin
  if not UseBooleans then
    B:=TJSONBool.Create(aValue)
  else if aValue then
    B:=TJSONTrue.Create
  else
    B:=TJSONFalse.Create;
  NewValue(B);
end;

procedure TJSONParser.NumberValue(const AValue: TJSONStringType);
begin
  // Do nothing
  if AValue='' then ;
end;

procedure TJSONParser.IntegerValue(const AValue: integer);
begin
  NewValue(TJSONNumber.Create(AValue));
end;

procedure TJSONParser.Int64Value(const AValue: int64);
begin
  NewValue(TJSONNumber.Create(AValue));
end;

procedure TJSONParser.QWordValue(const AValue: QWord);
begin
  NewValue(TJSONNumber.Create(Int64(AValue)));
end;

procedure TJSONParser.StartArray;
begin
  Push(NewValue(TJSONArray.Create))
end;


procedure TJSONParser.StartObject;
begin
  Push(NewValue(TJSONObject.Create));
end;

procedure TJSONParser.EndArray;
begin
  Pop(TJSONArray);
end;

procedure TJSONParser.EndObject;
begin
  Pop(TJSONObject);
end;

constructor TJSONParser.Create(aStream: TStream; aOptions: TJSONValue.TJSONParseOptions);

var
  JO : TJSONOptions;

begin
  JO:=[joStrict];
  if TJSONValue.TJSONParseOption.IsUTF8 in aOptions then
    Include(JO,joUTF8);
  Inherited Create(aStream,Jo);
  FStream:=aStream;
  FUseBooleans:=TJSONValue.TJSONParseOption.UseBool in aOptions;
  FUseExceptions:=TJSONValue.TJSONParseOption.RaiseExc in aOptions;
end;

destructor TJSONParser.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TJSONParser.Parse: TJSONValue;

begin
  SetLength(FStack,0);
  FStackPos:=0;
  FValue:=Nil;
  FStruct:=Nil;
  try
    DoExecute;
    Result:=FValue;
  except
    On E : exception do
      begin
      FreeAndNil(FValue);
      FStackPos:=0;
      SetLength(FStack,0);
      if FUseExceptions then
        Raise EJSONParseException.Create(E.Message);
      end;
  end;
end;

function TJSONParser.ParseSingle: TJSONValue;

begin
   Scanner.Options:=Scanner.Options+[joSingle];
   Result:=Parse;
end;


initialization
  _JSONSettings:=TFormatSettings.Invariant;
end.
