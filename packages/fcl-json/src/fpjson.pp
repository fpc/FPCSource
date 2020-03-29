{
    This file is part of the Free Component Library

    JSON Data structures
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit fpjson;

interface

uses
  {$ifdef fpc}
  variants,
  {$endif}
  {$ifdef pas2js}
  JS, RTLConsts, Types,
  {$endif}
  SysUtils,
  classes,
  contnrs;

type
  TJSONtype = (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject);
  TJSONInstanceType = (
    jitUnknown,
    jitNumberInteger,
    {$ifdef fpc}
    jitNumberInt64,
    jitNumberQWord,
    {$endif}
    jitNumberFloat,
    jitString,
    jitBoolean,
    jitNull,
    jitArray,
    jitObject);
  TJSONFloat = Double;
  TJSONStringType = {$ifdef fpc}UTF8String{$else}string{$endif};
  TJSONUnicodeStringType = Unicodestring;
  {$ifdef fpc}
  TJSONCharType = AnsiChar;
  PJSONCharType = ^TJSONCharType;
  TJSONVariant = variant;
  TFPJSStream = TMemoryStream;
  {$else}
  TJSONCharType = char;
  TJSONVariant = jsvalue;
  TFPJSStream = TJSArray;
  {$endif}
  TFormatOption = (foSingleLineArray,   // Array without CR/LF : all on one line
                   foSingleLineObject,  // Object without CR/LF : all on one line
                   foDoNotQuoteMembers, // Do not quote object member names.
                   foUseTabchar,        // Use tab characters instead of spaces.
                   foSkipWhiteSpace,    // Do not use whitespace at all
                   foSkipWhiteSpaceOnlyLeading   //  When foSkipWhiteSpace is active, skip whitespace for object members only before :
                   );
  TFormatOptions = set of TFormatOption;

Const
  DefaultIndentSize = 2;
  DefaultFormat     = [];
  AsJSONFormat      = [foSingleLineArray,foSingleLineObject]; // These options make FormatJSON behave as AsJSON
  AsCompressedJSON  = [foSingleLineArray,foSingleLineObject,foskipWhiteSpace]; // These options make FormatJSON behave as AsJSON with TJSONData.CompressedJSON=True
  AsCompactJSON     = [foSingleLineArray,foSingleLineObject,foskipWhiteSpace,foDoNotQuoteMembers]; // These options make FormatJSON behave as AsJSON with TJSONData.CompressedJSON=True and TJSONObject.UnquotedMemberNames=True
  ValueJSONTypes    = [jtNumber, jtString, jtBoolean, jtNull];
  ActualValueJSONTypes = ValueJSONTypes - [jtNull];
  StructuredJSONTypes  = [jtArray,jtObject];

Type
  TJSONData = Class;

  { TBaseJSONEnumerator }

  TJSONEnum = Record
    Key : TJSONStringType;
    KeyNum : Integer;
    Value : TJSONData;
  end;

  TBaseJSONEnumerator = class
  public
    function GetCurrent: TJSONEnum; virtual; abstract;
    function MoveNext : Boolean; virtual; abstract;
    property Current: TJSONEnum read GetCurrent;
  end;

  { TJSONData }
  
  TJSONData = class(TObject)
  private
    Const
      ElementSeps  : Array[Boolean] of TJSONStringType = (', ',',');
    Class Var FCompressedJSON : Boolean;
    Class Var FElementSep : TJSONStringType;
    class procedure DetermineElementSeparators;
    class function GetCompressedJSON: Boolean; {$ifdef fpc}static;{$endif}
    class procedure SetCompressedJSON(AValue: Boolean); {$ifdef fpc}static;{$endif}
  protected
    Class Procedure DoError(Const Msg : String);
    Class Procedure DoError(Const Fmt : String; const Args : Array of {$ifdef pas2js}jsvalue{$else}Const{$endif});
    Function DoFindPath(Const APath : TJSONStringType; Out NotFound : TJSONStringType) : TJSONdata; virtual;
    function GetAsBoolean: Boolean; virtual; abstract;
    function GetAsFloat: TJSONFloat; virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    {$ifdef fpc}
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsQWord: QWord; virtual; abstract;
    {$endif}
    function GetIsNull: Boolean; virtual;
    procedure SetAsBoolean(const AValue: Boolean); virtual; abstract;
    procedure SetAsFloat(const AValue: TJSONFloat); virtual; abstract;
    procedure SetAsInteger(const AValue: Integer); virtual; abstract;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); virtual; abstract;
    procedure SetAsQword(const AValue: QWord); virtual; abstract;
    {$endif}
    function GetAsJSON: TJSONStringType; virtual; abstract;
    function GetAsString: TJSONStringType; virtual; abstract;
    procedure SetAsString(const AValue: TJSONStringType); virtual; abstract;
    {$ifdef fpc}
    function GetAsUnicodeString: TJSONUnicodeStringType; virtual; 
    procedure SetAsUnicodeString(const AValue: TJSONUnicodeStringType); virtual;
    {$endif}
    function GetValue: TJSONVariant; virtual; abstract;
    procedure SetValue(const AValue: TJSONVariant); virtual; abstract;
    function GetItem(Index : Integer): TJSONData; virtual;
    procedure SetItem(Index : Integer; const AValue: TJSONData); virtual;
    Function DoFormatJSON(Options : TFormatOptions; CurrentIndent, Indent : Integer) : TJSONStringType; virtual;
    function GetCount: Integer; virtual;
  Public
    Class function JSONType: TJSONType; virtual;
    Class Property CompressedJSON : Boolean Read GetCompressedJSON Write SetCompressedJSON;
  public
    Constructor Create; virtual;
    Procedure Clear;  virtual; Abstract;
    Procedure DumpJSON(S : TFPJSStream);
    // Get enumerator
    function GetEnumerator: TBaseJSONEnumerator; virtual;
    Function FindPath(Const APath : TJSONStringType) : TJSONdata;
    Function GetPath(Const APath : TJSONStringType) : TJSONdata;
    Function Clone : TJSONData; virtual; abstract;
    Function FormatJSON(Options : TFormatOptions = DefaultFormat; Indentsize : Integer = DefaultIndentSize) : TJSONStringType; 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJSONData read GetItem write SetItem;
    property Value: TJSONVariant read GetValue write SetValue;
    Property AsString : TJSONStringType Read GetAsString Write SetAsString;
    {$ifdef fpc}
    Property AsUnicodeString : TJSONUnicodeStringType Read GetAsUnicodeString Write SetAsUnicodeString;
    {$endif}
    Property AsFloat : TJSONFloat Read GetAsFloat Write SetAsFloat;
    Property AsInteger : Integer Read GetAsInteger Write SetAsInteger;
    {$ifdef fpc}
    Property AsInt64 : Int64 Read GetAsInt64 Write SetAsInt64;
    Property AsQWord : QWord Read GetAsQWord Write SetAsQword;
    {$endif}
    Property AsBoolean : Boolean Read GetAsBoolean Write SetAsBoolean;
    Property IsNull : Boolean Read GetIsNull;
    Property AsJSON : TJSONStringType Read GetAsJSON;
  end;

  TJSONDataClass = Class of TJSONData;
  TJSONNumberType = (
    ntFloat,
    ntInteger
    {$ifdef fpc}
    ,ntInt64
    ,ntQWord
    {$endif}
    );

  TJSONNumber = class(TJSONData)
  protected
  public
    class function JSONType: TJSONType; override;
    class function NumberType : TJSONNumberType; virtual; abstract;
  end;

  { TJSONFloatNumber }

  TJSONFloatNumber = class(TJSONNumber)
  Private
    FValue : TJSONFloat;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue : TJSONFloat); reintroduce;
    class function NumberType : TJSONNumberType; override;
    Procedure Clear;  override;
    Function Clone : TJSONData; override;
  end;
  TJSONFloatNumberClass = Class of TJSONFloatNumber;

  { TJSONIntegerNumber }

  TJSONIntegerNumber = class(TJSONNumber)
  Private
    FValue : Integer;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue : Integer); reintroduce;
    class function NumberType : TJSONNumberType; override;
    Procedure Clear;  override;
    Function Clone : TJSONData; override;
  end;
  TJSONIntegerNumberClass = Class of TJSONIntegerNumber;

  {$ifdef fpc}
  { TJSONInt64Number }

  TJSONInt64Number = class(TJSONNumber)
  Private
    FValue : Int64;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue : Int64); reintroduce;
    class function NumberType : TJSONNumberType; override;
    Procedure Clear;  override;
    Function Clone : TJSONData; override;
  end;
  TJSONInt64NumberClass = Class of TJSONInt64Number;
  {$endif}

  {$ifdef fpc}
  { TJSONQWordNumber }

  TJSONQWordNumber = class(TJSONNumber)
  Private
    FValue : Qword;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue : QWord); reintroduce;
    class function NumberType : TJSONNumberType; override;
    Procedure Clear;  override;
    Function Clone : TJSONData; override;
  end;
  TJSONQWordNumberClass = Class of TJSONQWordNumber;
  {$endif}

  { TJSONString }

  TJSONString = class(TJSONData)
  Private
    FValue: TJSONStringType;
  protected
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
  Public
    Class var StrictEscaping : Boolean;
  public
    Constructor Create(const AValue : TJSONStringType); reintroduce;
    {$ifdef fpc}
    Constructor Create(const AValue : TJSONUnicodeStringType); reintroduce;
    {$endif}
    class function JSONType: TJSONType; override;
    Procedure Clear;  override;
    Function Clone : TJSONData; override;
  end;
  TJSONStringClass = Class of TJSONString;

  { TJSONBoolean }

  TJSONBoolean = class(TJSONData)
  Private
    FValue: Boolean;
  protected
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
  public
    Constructor Create(AValue : Boolean); reintroduce;
    class function JSONType: TJSONType; override;
    Procedure Clear;  override;
    Function  Clone : TJSONData; override;
  end;
  TJSONBooleanClass = Class of TJSONBoolean;

  { TJSONnull }

  TJSONNull = class(TJSONData)
  protected
    Procedure Converterror(From : Boolean);
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    function GetIsNull: Boolean; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    class function JSONType: TJSONType; override;
    Procedure Clear;  override;
    Function Clone : TJSONData; override;
  end;
  TJSONNullClass = Class of TJSONNull;

  TJSONArrayIterator = procedure(Item: TJSONData; Data: TObject; var Continue: Boolean) of object;

  { TJSONArray }
  TJSONObject = Class;

  TJSONArray = class(TJSONData)
  Private
    FList : TFPObjectList;
    function GetArrays(Index : Integer): TJSONArray;
    function GetBooleans(Index : Integer): Boolean;
    function GetFloats(Index : Integer): TJSONFloat;
    function GetIntegers(Index : Integer): Integer;
    {$ifdef fpc}
    function GetInt64s(Index : Integer): Int64;
    {$endif}
    function GetNulls(Index : Integer): Boolean;
    function GetObjects(Index : Integer): TJSONObject;
    {$ifdef fpc}
    function GetQWords(Index : Integer): QWord;
    {$endif}
    function GetStrings(Index : Integer): TJSONStringType;
    {$ifdef fpc}
    function GetUnicodeStrings(Index : Integer): TJSONUnicodeStringType;
    {$endif}
    function GetTypes(Index : Integer): TJSONType;
    procedure SetArrays(Index : Integer; const AValue: TJSONArray);
    procedure SetBooleans(Index : Integer; const AValue: Boolean);
    procedure SetFloats(Index : Integer; const AValue: TJSONFloat);
    procedure SetIntegers(Index : Integer; const AValue: Integer);
    {$ifdef fpc}
    procedure SetInt64s(Index : Integer; const AValue: Int64);
    {$endif}
    procedure SetObjects(Index : Integer; const AValue: TJSONObject);
    {$ifdef fpc}
    procedure SetQWords(Index : Integer; AValue: QWord);
    {$endif}
    procedure SetStrings(Index : Integer; const AValue: TJSONStringType);
    {$ifdef fpc}
    procedure SetUnicodeStrings(Index : Integer; const AValue: TJSONUnicodeStringType);
    {$endif}
  protected
    Function DoFindPath(Const APath : TJSONStringType; Out NotFound : TJSONStringType) : TJSONdata; override;
    Procedure Converterror(From : Boolean);
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetCount: Integer; override;
    function GetItem(Index : Integer): TJSONData; override;
    procedure SetItem(Index : Integer; const AValue: TJSONData); override;
    Function DoFormatJSON(Options : TFormatOptions; CurrentIndent, Indent : Integer) : TJSONStringType; override;
  public
    Constructor Create; overload; reintroduce;
    Constructor Create(const Elements : Array of {$ifdef pas2js}jsvalue{$else}Const{$endif}); overload;
    Destructor Destroy; override;
    class function JSONType: TJSONType; override;
    Function Clone : TJSONData; override;
    // Examine
    procedure Iterate(Iterator : TJSONArrayIterator; Data: TObject);
    function IndexOf(obj: TJSONData): Integer;
    function GetEnumerator: TBaseJSONEnumerator; override;
    // Manipulate
    Procedure Clear;  override;
    function Add(Item : TJSONData): Integer;
    function Add(I : Integer): Integer;
    {$ifdef fpc}
    function Add(I : Int64): Int64;
    function Add(I : QWord): QWord;
    {$endif}
    function Add(const S : String): Integer;
    {$ifdef fpc}
    function Add(const S : UnicodeString): Integer;
    {$endif}
    function Add: Integer;
    function Add(F : TJSONFloat): Integer;
    function Add(B : Boolean): Integer;
    function Add(AnArray : TJSONArray): Integer;
    function Add(AnObject: TJSONObject): Integer;
    Procedure Delete(Index : Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Item: TJSONData): TJSONData;
    function Extract(Index : Integer): TJSONData;
    procedure Insert(Index: Integer);
    procedure Insert(Index: Integer; Item : TJSONData);
    procedure Insert(Index: Integer; I : Integer);
    {$ifdef fpc}
    procedure Insert(Index: Integer; I : Int64);
    procedure Insert(Index: Integer; I : QWord);
    {$endif}
    procedure Insert(Index: Integer; const S : String);
    {$ifdef fpc}
    procedure Insert(Index: Integer; const S : UnicodeString);
    {$endif}
    procedure Insert(Index: Integer; F : TJSONFloat);
    procedure Insert(Index: Integer; B : Boolean);
    procedure Insert(Index: Integer; AnArray : TJSONArray);
    procedure Insert(Index: Integer; AnObject: TJSONObject);
    procedure Move(CurIndex, NewIndex: Integer);
    Procedure Remove(Item : TJSONData);
    Procedure Sort(Compare: TListSortCompare);
    // Easy Access Properties.
    property Items;default;
    Property Types[Index : Integer] : TJSONType Read GetTypes;
    Property Nulls[Index : Integer] : Boolean Read GetNulls;
    Property Integers[Index : Integer] : Integer Read GetIntegers Write SetIntegers;
    {$ifdef fpc}
    Property Int64s[Index : Integer] : Int64 Read GetInt64s Write SetInt64s;
    Property QWords[Index : Integer] : QWord Read GetQWords Write SetQWords;
    {$endif}
    Property Strings[Index : Integer] : TJSONStringType Read GetStrings Write SetStrings;
    {$ifdef fpc}
    Property UnicodeStrings[Index : Integer] : TJSONUnicodeStringType Read GetUnicodeStrings Write SetUnicodeStrings;
    {$endif}
    Property Floats[Index : Integer] : TJSONFloat Read GetFloats Write SetFloats;
    Property Booleans[Index : Integer] : Boolean Read GetBooleans Write SetBooleans;
    Property Arrays[Index : Integer] : TJSONArray Read GetArrays Write SetArrays;
    Property Objects[Index : Integer] : TJSONObject Read GetObjects Write SetObjects;
  end;
  TJSONArrayClass = Class of TJSONArray;

  TJSONObjectIterator = procedure(Const AName : TJSONStringType; Item: TJSONData; Data: TObject; var Continue: Boolean) of object;

  { TJSONObject }

  TJSONObject = class(TJSONData)
  private
    Const
      ElementStart   : Array[Boolean] of TJSONStringType = ('"','');
      SpacedQuoted   : Array[Boolean] of TJSONStringType = ('" : ',' : ');
      UnSpacedQuoted : Array[Boolean] of TJSONStringType = ('":',':');
      ObjStartSeps   : Array[Boolean] of TJSONStringType = ('{ ','{');
      ObjEndSeps     : Array[Boolean] of TJSONStringType = (' }','}');
    Class var FUnquotedMemberNames: Boolean;
    Class var FObjStartSep,FObjEndSep,FElementEnd,FElementStart : TJSONStringType;
    function DoAdd(const AName: TJSONStringType; AValue: TJSONData; FreeOnError: Boolean=True): Integer;
    Class procedure DetermineElementQuotes;
  Private
    {$ifdef pas2js}
    FCount: integer;
    FHash: TJSObject;
    FNames: TStringDynArray;
    {$else}
    FHash : TFPHashObjectList; // Careful : Names limited to 255 chars.
    {$endif}
    function GetArrays(const AName : String): TJSONArray;
    function GetBooleans(const AName : String): Boolean;
    function GetElements(const AName: string): TJSONData;
    function GetFloats(const AName : String): TJSONFloat;
    function GetIntegers(const AName : String): Integer;
    {$ifdef fpc}
    function GetInt64s(const AName : String): Int64;
    {$endif}
    function GetIsNull(const AName : String): Boolean; reintroduce;
    function GetNameOf(Index : Integer): TJSONStringType;
    function GetObjects(const AName : String): TJSONObject;
    {$ifdef fpc}
    function GetQWords(AName : String): QWord;
    {$endif}
    function GetStrings(const AName : String): TJSONStringType;
    {$ifdef fpc}
    function GetUnicodeStrings(const AName : String): TJSONUnicodeStringType;
    {$endif}
    function GetTypes(const AName : String): TJSONType;
    procedure SetArrays(const AName : String; const AValue: TJSONArray);
    procedure SetBooleans(const AName : String; const AValue: Boolean);
    procedure SetElements(const AName: string; const AValue: TJSONData);
    procedure SetFloats(const AName : String; const AValue: TJSONFloat);
    procedure SetIntegers(const AName : String; const AValue: Integer);
    {$ifdef fpc}
    procedure SetInt64s(const AName : String; const AValue: Int64);
    {$endif}
    procedure SetIsNull(const AName : String; const AValue: Boolean);
    procedure SetObjects(const AName : String; const AValue: TJSONObject);
    {$ifdef fpc}
    procedure SetQWords(AName : String; AValue: QWord);
    {$endif}
    procedure SetStrings(const AName : String; const AValue: TJSONStringType);
    {$ifdef fpc}
    procedure SetUnicodeStrings(const AName : String; const AValue: TJSONUnicodeStringType);
    {$endif}
    class function GetUnquotedMemberNames: Boolean; {$ifdef fpc}static;{$endif}
    class procedure SetUnquotedMemberNames(AValue: Boolean); {$ifdef fpc}static;{$endif}
  protected
    Function DoFindPath(Const APath : TJSONStringType; Out NotFound : TJSONStringType) : TJSONdata; override;
    Procedure Converterror(From : Boolean);
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetCount: Integer; override;
    function GetItem(Index : Integer): TJSONData; override;
    procedure SetItem(Index : Integer; const AValue: TJSONData); override;
    Function DoFormatJSON(Options : TFormatOptions; CurrentIndent, Indent : Integer) : TJSONStringType; override;
  public
    constructor Create; reintroduce;
    Constructor Create(const Elements : Array of {$ifdef pas2js}jsvalue{$else}Const{$endif}); overload;
    destructor Destroy; override;
    class function JSONType: TJSONType; override;
    Class Property UnquotedMemberNames : Boolean Read GetUnquotedMemberNames Write SetUnquotedMemberNames;
    Function Clone : TJSONData; override;
    function GetEnumerator: TBaseJSONEnumerator; override;
    // Examine
    procedure Iterate(Iterator : TJSONObjectIterator; Data: TObject);
    function IndexOf(Item: TJSONData): Integer;
    Function IndexOfName(const AName: TJSONStringType; CaseInsensitive : Boolean = False): Integer;
    Function Find(Const AName : String) : TJSONData; overload;
    Function Find(Const AName : String; AType : TJSONType) : TJSONData; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONData): boolean;
    function Find(const key: TJSONStringType; out AValue: TJSONObject): boolean;
    function Find(const key: TJSONStringType; out AValue: TJSONArray): boolean;
    function Find(const key: TJSONStringType; out AValue: TJSONString): boolean;
    function Find(const key: TJSONStringType; out AValue: TJSONBoolean): boolean;
    function Find(const key: TJSONStringType; out AValue: TJSONNumber): boolean;
    Function Get(Const AName : String) : TJSONVariant;
    Function Get(Const AName : String; ADefault : TJSONFloat) : TJSONFloat;
    Function Get(Const AName : String; ADefault : Integer) : Integer;
    {$ifdef fpc}
    Function Get(Const AName : String; ADefault : Int64) : Int64;
    Function Get(Const AName : String; ADefault : QWord) : QWord;
    {$endif}
    Function Get(Const AName : String; ADefault : Boolean) : Boolean;
    Function Get(Const AName : String; ADefault : TJSONStringType) : TJSONStringType;
    {$ifdef fpc}
    Function Get(Const AName : String; ADefault : TJSONUnicodeStringType) : TJSONUnicodeStringType;
    {$endif}
    Function Get(Const AName : String; ADefault : TJSONArray) : TJSONArray;
    Function Get(Const AName : String; ADefault : TJSONObject) : TJSONObject;
    // Manipulate
    Procedure Clear;  override;
    function Add(const AName: TJSONStringType; AValue: TJSONData): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: Boolean): Integer; overload;
    function Add(const AName: TJSONStringType; AValue: TJSONFloat): Integer; overload;
    function Add(const AName, AValue: TJSONStringType): Integer; overload;
    {$ifdef fpc}
    function Add(const AName : String; AValue: TJSONUnicodeStringType): Integer; overload;
    {$endif}
    function Add(const AName: TJSONStringType; Avalue: Integer): Integer; overload;
    {$ifdef fpc}
    function Add(const AName: TJSONStringType; Avalue: Int64): Integer; overload;
    function Add(const AName: TJSONStringType; Avalue: QWord): Integer; overload;
    {$endif}
    function Add(const AName: TJSONStringType): Integer; overload;
    function Add(const AName: TJSONStringType; AValue : TJSONArray): Integer; overload;
    procedure Delete(Index : Integer);
    procedure Delete(Const AName : string);
    procedure Remove(Item : TJSONData);
    {$ifdef fpc}
    Function Extract(Index : Integer) : TJSONData;
    Function Extract(Const AName : string) : TJSONData;
    {$endif}

    // Easy access properties.
    property Names[Index : Integer] : TJSONStringType read GetNameOf;
    property Elements[AName: string] : TJSONData read GetElements write SetElements; default;

    Property Types[AName : String] : TJSONType Read GetTypes;
    Property Nulls[AName : String] : Boolean Read GetIsNull Write SetIsNull;
    Property Floats[AName : String] : TJSONFloat Read GetFloats Write SetFloats;
    Property Integers[AName : String] : Integer Read GetIntegers Write SetIntegers;
    {$ifdef fpc}
    Property Int64s[AName : String] : Int64 Read GetInt64s Write SetInt64s;
    Property QWords[AName : String] : QWord Read GetQWords Write SetQWords;
    {$endif}
    Property Strings[AName : String] : TJSONStringType Read GetStrings Write SetStrings;
    {$ifdef fpc}
    Property UnicodeStrings[AName : String] : TJSONUnicodeStringType Read GetUnicodeStrings Write SetUnicodeStrings;
    {$endif}
    Property Booleans[AName : String] : Boolean Read GetBooleans Write SetBooleans;
    Property Arrays[AName : String] : TJSONArray Read GetArrays Write SetArrays;
    Property Objects[AName : String] : TJSONObject Read GetObjects Write SetObjects;
  end;
  TJSONObjectClass = Class of TJSONObject;

  EJSON = Class(Exception);

  {$ifdef fpc}
  TJSONParserHandler = Procedure(AStream : TStream; Const AUseUTF8 : Boolean; Out Data : TJSONData);
  TJSONStringParserHandler = Procedure(Const aJSON : TJSONStringType; Const AUseUTF8 : Boolean; Out Data : TJSONData);
  {$endif}

Function SetJSONInstanceType(AType : TJSONInstanceType; AClass : TJSONDataClass) : TJSONDataClass;
Function GetJSONInstanceType(AType : TJSONInstanceType) : TJSONDataClass;

Function StringToJSONString(const S : TJSONStringType; Strict : Boolean = False) : TJSONStringType;
Function JSONStringToString(const S : TJSONStringType) : TJSONStringType;
Function JSONTypeName(JSONType : TJSONType) : String;

// These functions create JSONData structures, taking into account the instance types
Function CreateJSON : TJSONNull;
Function CreateJSON(Data : Boolean) : TJSONBoolean;
Function CreateJSON(Data : Integer) : TJSONIntegerNumber;
{$ifdef fpc}
Function CreateJSON(Data : Int64) : TJSONInt64Number;
Function CreateJSON(Data : QWord) : TJSONQWordNumber;
{$endif}
Function CreateJSON(Data : TJSONFloat) : TJSONFloatNumber;
Function CreateJSON(const Data : TJSONStringType) : TJSONString;
{$ifdef fpc}
Function CreateJSON(const Data : TJSONUnicodeStringType) : TJSONString;
{$endif}
Function CreateJSONArray(const Data : Array of {$ifdef pas2js}jsvalue{$else}Const{$endif}) : TJSONArray;
Function CreateJSONObject(const Data : Array of {$ifdef pas2js}jsvalue{$else}Const{$endif}) : TJSONObject;

// These functions rely on a callback. If the callback is not set, they will raise an error.
// When the jsonparser unit is included in the project, the callback is automatically set.
{$ifdef fpc}
Function GetJSON(Const JSON : TJSONStringType; Const UseUTF8 : Boolean = True) : TJSONData;
Function GetJSON(Const JSON : TStream; Const UseUTF8 : Boolean = True) : TJSONData;
Function SetJSONParserHandler(AHandler : TJSONParserHandler) : TJSONParserHandler;
Function SetJSONStringParserHandler(AHandler : TJSONStringParserHandler) : TJSONStringParserHandler;
Function GetJSONParserHandler : TJSONParserHandler;
Function GetJSONStringParserHandler: TJSONStringParserHandler;
{$endif}

implementation

Uses typinfo;

Resourcestring
  SErrCannotConvertFromNull = 'Cannot convert data from Null value';
  SErrCannotConvertToNull = 'Cannot convert data to Null value';
  SErrCannotConvertFromArray = 'Cannot convert data from array value';
  SErrCannotConvertToArray = 'Cannot convert data to array value';
  SErrCannotConvertFromObject = 'Cannot convert data from object value';
  SErrCannotConvertToObject = 'Cannot convert data to object value';
  SErrInvalidFloat = 'Invalid float value : %s';
  SErrCannotSetNotIsNull = 'IsNull cannot be set to False';
  SErrCannotAddArrayTwice = 'Adding an array object to an array twice is not allowed';
  SErrCannotAddObjectTwice = 'Adding an object to an array twice is not allowed';
  SErrUnknownTypeInConstructor = 'Unknown type in JSON%s constructor: %d';
  SErrNotJSONData = 'Cannot add object of type %s to TJSON%s';
  SErrOddNumber = 'TJSONObject must be constructed with name,value pairs';
  SErrNameMustBeString = 'TJSONObject constructor element name at pos %d is not a string';
  SErrNonexistentElement = 'Unknown object member: "%s"';
  SErrDuplicateValue = 'Duplicate object member: "%s"';
  SErrPathElementNotFound = 'Path "%s" invalid: element "%s" not found.';
  SErrWrongInstanceClass = 'Cannot set instance class: %s does not descend from %s.';
  {$ifdef fpc}
  SErrPointerNotNil = 'Cannot add non-nil pointer to JSON%s';
  SErrNoParserHandler = 'No JSON parser handler installed. Recompile your project with the jsonparser unit included';
  {$endif}

Var
  DefaultJSONInstanceTypes :
    Array [TJSONInstanceType] of TJSONDataClass = (
      TJSONData,
      TJSONIntegerNumber,
      {$ifdef fpc}
      TJSONInt64Number,
      TJSONQWordNumber,
      {$endif}
      TJSONFloatNumber,
      TJSONString,
      TJSONBoolean,
      TJSONNull,
      TJSONArray,
      TJSONObject);
Const
  MinJSONInstanceTypes :
    Array [TJSONInstanceType] of TJSONDataClass = (
      TJSONData,
      TJSONIntegerNumber,
      {$ifdef fpc}
      TJSONInt64Number,
      TJSONQWordNumber,
      {$endif}
      TJSONFloatNumber,
      TJSONString,
      TJSONBoolean,
      TJSONNull,
      TJSONArray,
      TJSONObject
      );

function SetJSONInstanceType(AType: TJSONInstanceType; AClass: TJSONDataClass): TJSONDataClass;
begin
  if AClass=Nil then
    TJSONData.DoError(SErrWrongInstanceClass,['Nil',MinJSONInstanceTypes[AType].ClassName]);
  if Not AClass.InheritsFrom(MinJSONINstanceTypes[AType]) then
    TJSONData.DoError(SErrWrongInstanceClass,[AClass.ClassName,MinJSONInstanceTypes[AType].ClassName]);
  Result:=DefaultJSONInstanceTypes[AType];
  DefaultJSONINstanceTypes[AType]:=AClass;
end;

function GetJSONInstanceType(AType: TJSONInstanceType): TJSONDataClass;
begin
  Result:=DefaultJSONInstanceTypes[AType]
end;

function StringToJSONString(const S: TJSONStringType; Strict : Boolean = False): TJSONStringType;

Var
  I,J,L : Integer;
  C : Char;

begin
  I:=1;
  J:=1;
  Result:='';
  L:=Length(S);
  While I<=L do
    begin
    C:=S[I];
    if (C in ['"','/','\',#0..#31]) then
      begin
      Result:=Result+Copy(S,J,I-J);
      Case C of
        '\' : Result:=Result+'\\';
        '/' : if Strict then
                Result:=Result+'\/'
              else
                Result:=Result+'/';
        '"' : Result:=Result+'\"';
        #8  : Result:=Result+'\b';
        #9  : Result:=Result+'\t';
        #10 : Result:=Result+'\n';
        #12 : Result:=Result+'\f';
        #13 : Result:=Result+'\r';
      else
        Result:=Result+'\u'+HexStr(Ord(C),4);
      end;
      J:=I+1;
      end;
    Inc(I);
    end;
  Result:=Result+Copy(S,J,I-1);
end;

function JSONStringToString(const S: TJSONStringType): TJSONStringType;

Var
  I,J,L,U1,U2 : Integer;
  App,W : String;

  Procedure MaybeAppendUnicode;

  Var
    U : String;

  begin
    if (U1<>0) then
      begin
      U:={$IFDEF FPC_HAS_CPSTRING}UTF8Encode(WideChar(U1)){$ELSE}widechar(U1){$ENDIF};
      Result:=Result+U;
      U1:=0;
      end;
  end;

begin
  I:=1;
  J:=1;
  L:=Length(S);
  Result:='';
  U1:=0;
  While (I<=L) do
    begin
    if (S[I]='\') then
      begin
      Result:=Result+Copy(S,J,I-J);
      If I<L then
        begin
        Inc(I);
        App:='';
        Case S[I] of
          '\','"','/'
              : App:=S[I];
          'b' : App:=#8;
          't' : App:=#9;
          'n' : App:=#10;
          'f' : App:=#12;
          'r' : App:=#13;
          'u' : begin
                W:=Copy(S,I+1,4);
                Inc(I,4);
                u2:=StrToInt('$'+W);
                if (U1<>0) then
                  begin
                  App:={$IFDEF FPC_HAS_CPSTRING}UTF8Encode({$ENDIF}WideChar(U1)+WideChar(U2){$IFDEF FPC_HAS_CPSTRING}){$ENDIF};
                  U2:=0;
                  end
                else
                  U1:=U2;
                end;
        end;
        if App<>'' then
          begin
          MaybeAppendUnicode;
          Result:=Result+App;
          end;
        end;
      J:=I+1;
      end
    else
      MaybeAppendUnicode;
    Inc(I);
    end;
  MaybeAppendUnicode;
  Result:=Result+Copy(S,J,I-J+1);
end;

function JSONTypeName(JSONType: TJSONType): String;
begin
  Result:=GetEnumName(TypeInfo(TJSONType),Ord(JSONType));
end;

function CreateJSON: TJSONNull;
begin
  Result:=TJSONNullClass(DefaultJSONInstanceTypes[jitNull]).Create
end;

function CreateJSON(Data: Boolean): TJSONBoolean;
begin
  Result:=TJSONBooleanClass(DefaultJSONInstanceTypes[jitBoolean]).Create(Data);
end;

function CreateJSON(Data: Integer): TJSONIntegerNumber;
begin
  Result:=TJSONIntegerNumberCLass(DefaultJSONInstanceTypes[jitNumberInteger]).Create(Data);
end;

{$ifdef fpc}
function CreateJSON(Data: Int64): TJSONInt64Number;
begin
  Result:=TJSONInt64NumberCLass(DefaultJSONInstanceTypes[jitNumberInt64]).Create(Data);
end;

function CreateJSON(Data: QWord): TJSONQWordNumber;
begin
  Result:=TJSONQWordNumberClass(DefaultJSONInstanceTypes[jitNumberQWord]).Create(Data);
end;
{$endif}

function CreateJSON(Data: TJSONFloat): TJSONFloatNumber;
begin
  Result:=TJSONFloatNumberCLass(DefaultJSONInstanceTypes[jitNumberFloat]).Create(Data);
end;

function CreateJSON(const Data: TJSONStringType): TJSONString;
begin
  Result:=TJSONStringCLass(DefaultJSONInstanceTypes[jitString]).Create(Data);
end;

{$ifdef fpc}
function CreateJSON(const Data: TJSONUnicodeStringType): TJSONString;
begin
  Result:=TJSONStringCLass(DefaultJSONInstanceTypes[jitString]).Create(Data);
end;
{$endif}

function CreateJSONArray(const Data: array of {$ifdef pas2js}jsvalue{$else}Const{$endif}): TJSONArray;
begin
  Result:=TJSONArrayCLass(DefaultJSONInstanceTypes[jitArray]).Create(Data);
end;

function CreateJSONObject(const Data: array of {$ifdef pas2js}jsvalue{$else}Const{$endif}): TJSONObject;
begin
  Result:=TJSONObjectClass(DefaultJSONInstanceTypes[jitObject]).Create(Data);
end;

{$ifdef fpc}
Var
  JPH : TJSONParserHandler;
  JPSH : TJSONStringParserHandler;

function GetJSON(const JSON: TJSONStringType; const UseUTF8: Boolean): TJSONData;

Var
  SS : TStringStream;
begin
  if Assigned(JPSH) then
    JPSH(JSON,UseUTF8,Result)
  else
    begin
    {$IF FPC_FULLVERSION>30300}
    if UseUTF8 then
      SS:=TStringStream.Create(JSON,TEncoding.UTF8)
    else
    {$ENDIF}
      SS:=TStringStream.Create(JSON);
    try
      Result:=GetJSON(SS,UseUTF8);
    finally
      SS.Free;
    end;
    end;
end;
{$endif}

{$ifdef fpc}
function GetJSON(const JSON: TStream; const UseUTF8: Boolean): TJSONData;

Var
  S : TJSONStringType;

begin
  Result:=Nil;
  If (JPH<>Nil) then
    JPH(JSON,UseUTF8,Result)
  else if JPSH=Nil then
    TJSONData.DoError(SErrNoParserHandler)
  else
    begin
    Setlength(S,JSON.Size);
    if Length(S)>0 then
      JSON.ReadBuffer(S[1],Length(S));
    end;
end;


Function SetJSONStringParserHandler(AHandler : TJSONStringParserHandler) : TJSONStringParserHandler;
begin
  Result:=JPSH;
  JPSH:=AHandler;
end;

function SetJSONParserHandler(AHandler: TJSONParserHandler): TJSONParserHandler;
begin
  Result:=JPH;
  JPH:=AHandler;
end;

function GetJSONParserHandler: TJSONParserHandler;
begin
  Result:=JPH;
end;

function GetJSONStringParserHandler: TJSONStringParserHandler;
begin
  Result:=JPSH;
end;
{$endif}

Type
  { TJSONEnumerator }

  TJSONEnumerator = class(TBaseJSONEnumerator)
  Private
    FData : TJSONData;
  public
    Constructor Create(AData : TJSONData);
    function GetCurrent: TJSONEnum; override;
    function MoveNext : Boolean; override;
  end;

  { TJSONArrayEnumerator }

  TJSONArrayEnumerator = class(TBaseJSONEnumerator)
  Private
    FData : TJSONArray;
    FCurrent : Integer;
  public
    Constructor Create(AData : TJSONArray);
    function GetCurrent: TJSONEnum; override;
    function MoveNext : Boolean; override;
  end;

  { TJSONObjectEnumerator }

  TJSONObjectEnumerator = class(TBaseJSONEnumerator)
  Private
    FData : TJSONObject;
    FCurrent : Integer;
  public
    Constructor Create(AData : TJSONObject);
    function GetCurrent: TJSONEnum; override;
    function MoveNext : Boolean; override;
  end;

{$ifdef fpc}
{ TJSONQWordNumber }

function TJSONQWordNumber.GetAsBoolean: Boolean;
begin
  Result:=FValue<>0;
end;

function TJSONQWordNumber.GetAsFloat: TJSONFloat;
begin
  Result:= FValue;
end;

function TJSONQWordNumber.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TJSONQWordNumber.GetAsInt64: Int64;
begin
  Result := FValue;
end;

function TJSONQWordNumber.GetAsQWord: QWord;
begin
  Result := FValue;
end;

procedure TJSONQWordNumber.SetAsBoolean(const AValue: Boolean);
begin
  FValue:=Ord(AValue);
end;

procedure TJSONQWordNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue:=Round(AValue);
end;

procedure TJSONQWordNumber.SetAsInteger(const AValue: Integer);
begin
  FValue:=AValue;
end;

procedure TJSONQWordNumber.SetAsInt64(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONQWordNumber.SetAsQword(const AValue: QWord);
begin
  FValue:=AValue;
end;

function TJSONQWordNumber.GetAsJSON: TJSONStringType;
begin
  Result:=AsString;
end;

function TJSONQWordNumber.GetAsString: TJSONStringType;
begin
  Result:=IntToStr(FValue);
end;

procedure TJSONQWordNumber.SetAsString(const AValue: TJSONStringType);
begin
  FValue:=StrToQWord(AValue);
end;

function TJSONQWordNumber.GetValue: TJSONVariant;
begin
  Result:=FValue;
end;

procedure TJSONQWordNumber.SetValue(const AValue: TJSONVariant);
begin
  FValue:=AValue;
end;

constructor TJSONQWordNumber.Create(AValue: QWord);
begin
  FValue := AValue;
end;

class function TJSONQWordNumber.NumberType: TJSONNumberType;
begin
  Result:=ntQWord;
end;

procedure TJSONQWordNumber.Clear;
begin
  FValue:=0;
end;

function TJSONQWordNumber.Clone: TJSONData;
begin
  Result:=TJSONQWordNumberClass(ClassType).Create(Self.FValue);
end;
{$endif}

{ TJSONObjectEnumerator }

constructor TJSONObjectEnumerator.Create(AData: TJSONObject);
begin
  FData:=AData;
  FCurrent:=-1;
end;

function TJSONObjectEnumerator.GetCurrent: TJSONEnum;
begin
  Result.KeyNum:=FCurrent;
  Result.Key:=FData.Names[FCurrent];
  Result.Value:=FData.Items[FCurrent];
end;

function TJSONObjectEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result:=FCurrent<FData.Count;
end;

{ TJSONArrayEnumerator }

constructor TJSONArrayEnumerator.Create(AData: TJSONArray);
begin
  FData:=AData;
  FCurrent:=-1;
end;

function TJSONArrayEnumerator.GetCurrent: TJSONEnum;
begin
  Result.KeyNum:=FCurrent;
  Result.Key:=IntToStr(FCurrent);
  Result.Value:=FData.Items[FCurrent];
end;

function TJSONArrayEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result:=FCurrent<FData.Count;
end;

  { TJSONEnumerator }

constructor TJSONEnumerator.Create(AData: TJSONData);
begin
  FData:=AData;
end;

function TJSONEnumerator.GetCurrent: TJSONEnum;
begin
  Result.Key:='';
  Result.KeyNum:=0;
  Result.Value:=FData;
  FData:=Nil;
end;

function TJSONEnumerator.MoveNext: Boolean;
begin
  Result:=FData<>Nil;
end;



{ TJSONData }

{$ifdef fpc}
function TJSONData.GetAsUnicodeString: TJSONUnicodeStringType; 
begin
  Result:=UTF8Decode(AsString);
end;

procedure TJSONData.SetAsUnicodeString(const AValue: TJSONUnicodeStringType); 
begin
  AsString:=UTF8Encode(AValue);
end;
{$endif}

function TJSONData.GetItem(Index : Integer): TJSONData;
begin
  Result:=nil;
  if Index>0 then ;
end;

function TJSONData.GetCount: Integer;
begin
  Result:=0;
end;

constructor TJSONData.Create;
begin
  Clear;
end;

procedure TJSONData.DumpJSON(S: TFPJSStream);

  Procedure W(T : String);
  begin
    if T='' then exit;
    {$ifdef pas2js}
    S.push(T);
    {$else}
    S.WriteBuffer(T[1],Length(T)*SizeOf(Char));
    {$endif}
  end;

Var
  I: Integer;
  O : TJSONObject;

begin
  Case JSONType of
    jtObject :
      begin
      O:=TJSONObject(Self);
      W('{');
      For I:=0 to O.Count-1 do
        begin
        if (I>0) then
          W(',');
        W('"');
        W(StringToJSONString(O.Names[i],False));
        W('":');
        O.Items[I].DumpJSON(S);
        end;
      W('}');
      end;
    jtArray :
      begin
      W('[');
      For I:=0 to Count-1 do
        begin
        if (I>0) then
          W(',');
        Items[I].DumpJSON(S);
        end;
      W(']');
      end
  else
    W(AsJSON)
  end;
end;

class function TJSONData.GetCompressedJSON: Boolean; {$ifdef fpc}static;{$endif}
begin
  Result:=FCompressedJSON;
end;

class procedure TJSONData.DetermineElementSeparators;


begin
  FElementSep:=ElementSeps[FCompressedJSON];
end;

class procedure TJSONData.SetCompressedJSON(AValue: Boolean); {$ifdef fpc}static;{$endif}


begin
  if AValue=FCompressedJSON then exit;
  FCompressedJSON:=AValue;
  DetermineElementSeparators;
  TJSONObject.DetermineElementQuotes;
end;

class procedure TJSONData.DoError(const Msg: String);
begin
  Raise EJSON.Create(Msg);
end;

class procedure TJSONData.DoError(const Fmt: String;
  const Args: array of {$ifdef pas2js}jsvalue{$else}Const{$endif});
begin
  Raise EJSON.CreateFmt(Fmt,Args);
end;

function TJSONData.DoFindPath(const APath: TJSONStringType; out
  NotFound: TJSONStringType): TJSONdata;
begin
  If APath<>'' then
    begin
    NotFound:=APath;
    Result:=Nil;
    end
  else
    Result:=Self;
end;

function TJSONData.GetIsNull: Boolean;
begin
  Result:=False;
end;

class function TJSONData.JSONType: TJSONType;
begin
  JSONType:=jtUnknown;
end;

function TJSONData.GetEnumerator: TBaseJSONEnumerator;
begin
  Result:=TJSONEnumerator.Create(Self);
end;

function TJSONData.FindPath(const APath: TJSONStringType): TJSONdata;

Var
  M : TJSONStringType;

begin
  Result:=DoFindPath(APath,M);
end;

function TJSONData.GetPath(const APath: TJSONStringType): TJSONdata;

Var
  M : TJSONStringType;
begin
  Result:=DoFindPath(APath,M);
  If Result=Nil then
    DoError(SErrPathElementNotFound,[APath,M]);
end;

procedure TJSONData.SetItem(Index : Integer; const AValue:
  TJSONData);
begin
  // Do Nothing
  if Index>0 then ;
  if AValue<>nil then ;
end;

function TJSONData.FormatJSON(Options: TFormatOptions; Indentsize: Integer
  ): TJSONStringType;

begin
  Result:=DoFormatJSON(Options,0,IndentSize);
end;

function TJSONData.DoFormatJSON(Options: TFormatOptions; CurrentIndent,
  Indent: Integer): TJSONStringType;

begin
  Result:=AsJSON;
  if Options=[] then ;
  if CurrentIndent=0 then ;
  if Indent>0 then ;
end;

{ TJSONnumber }

class function TJSONnumber.JSONType: TJSONType;
begin
  Result:=jtNumber;
end;


{ TJSONstring }

class function TJSONString.JSONType: TJSONType;
begin
  Result:=jtString;
end;

procedure TJSONString.Clear;
begin
  FValue:='';
end;

function TJSONString.Clone: TJSONData;

begin
  Result:=TJSONStringClass(ClassType).Create(Self.FValue);
end;

function TJSONString.GetValue: TJSONVariant;
begin
  Result:=FValue;
end;

procedure TJSONString.SetValue(const AValue: TJSONVariant);
begin
  FValue:={$ifdef pas2js}TJSONStringType(AValue){$else}AValue{$endif};
end;


function TJSONString.GetAsBoolean: Boolean;
begin
  Result:=StrToBool(FValue);
end;

function TJSONString.GetAsFloat: TJSONFloat;

Var
  C : Integer;

begin
  Val(FValue,Result,C);
  If (C<>0) then
    If Not TryStrToFloat(FValue,Result) then
      Raise EConvertError.CreateFmt(SErrInvalidFloat,[FValue]);
end;

function TJSONString.GetAsInteger: Integer;
begin
  Result:=StrToInt(FValue);
end;

{$ifdef fpc}
function TJSONString.GetAsInt64: Int64;
begin
  Result:=StrToInt64(FValue);
end;

function TJSONString.GetAsQWord: QWord;
begin
  Result:=StrToQWord(FValue);
end;
{$endif}

procedure TJSONString.SetAsBoolean(const AValue: Boolean);
begin
  FValue:=BoolToStr(AValue);
end;

procedure TJSONString.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue:=FloatToStr(AValue);
end;

procedure TJSONString.SetAsInteger(const AValue: Integer);
begin
  FValue:=IntToStr(AValue);
end;

{$ifdef fpc}
procedure TJSONString.SetAsInt64(const AValue: Int64);
begin
  FValue:=IntToStr(AValue);
end;

procedure TJSONString.SetAsQword(const AValue: QWord);
begin
  FValue:=IntToStr(AValue);
end;
{$endif}

function TJSONString.GetAsJSON: TJSONStringType;
begin
  Result:='"'+StringToJSONString(FValue,StrictEscaping)+'"';
end;

function TJSONString.GetAsString: TJSONStringType;
begin
  Result:=FValue;
end;

procedure TJSONString.SetAsString(const AValue: TJSONStringType);
begin
  FValue:=AValue;
end;

constructor TJSONString.Create(const AValue: TJSONStringType);
begin
  FValue:=AValue;
end;

{$ifdef fpc}
constructor TJSONString.Create(const AValue: TJSONUnicodeStringType);
begin
  FValue:=UTF8Encode(AValue);
end;
{$endif}

{ TJSONboolean }


function TJSONBoolean.GetValue: TJSONVariant;
begin
  Result:=FValue;
end;

class function TJSONBoolean.JSONType: TJSONType;
begin
  Result:=jtBoolean;
end;

procedure TJSONBoolean.Clear;
begin
  FValue:=False;
end;

function TJSONBoolean.Clone: TJSONData;
begin
  Result:=TJSONBooleanClass(Self.ClassType).Create(Self.Fvalue);
end;


procedure TJSONBoolean.SetValue(const AValue: TJSONVariant);
begin
  FValue:=boolean(AValue);
end;

function TJSONBoolean.GetAsBoolean: Boolean;
begin
  Result:=FValue;
end;

function TJSONBoolean.GetAsFloat: TJSONFloat;
begin
  Result:=Ord(FValue);
end;

function TJSONBoolean.GetAsInteger: Integer;
begin
  Result:=Ord(FValue);
end;

{$ifdef fpc}
function TJSONBoolean.GetAsInt64: Int64;
begin
  Result:=Ord(FValue);
end;

function TJSONBoolean.GetAsQWord: QWord;
begin
  Result:=Ord(FValue);
end;
{$endif}

procedure TJSONBoolean.SetAsBoolean(const AValue: Boolean);
begin
  FValue:=AValue;
end;

procedure TJSONBoolean.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue:=(AValue<>0)
end;

procedure TJSONBoolean.SetAsInteger(const AValue: Integer);
begin
  FValue:=(AValue<>0)
end;

{$ifdef fpc}
procedure TJSONBoolean.SetAsInt64(const AValue: Int64);
begin
  FValue:=(AValue<>0)
end;

procedure TJSONBoolean.SetAsQword(const AValue: QWord);
begin
  FValue:=(AValue<>0)
end;
{$endif}

function TJSONBoolean.GetAsJSON: TJSONStringType;
begin
  If FValue then
    Result:='true'
  else
    Result:='false';
end;

function TJSONBoolean.GetAsString: TJSONStringType;
begin
  Result:=BoolToStr(FValue, True);
end;

procedure TJSONBoolean.SetAsString(const AValue: TJSONStringType);
begin
  FValue:=StrToBool(AValue);
end;


constructor TJSONBoolean.Create(AValue: Boolean);
begin
  FValue:=AValue;
end;

{ TJSONnull }

procedure TJSONNull.Converterror(From: Boolean);
begin
  If From then
    DoError(SErrCannotConvertFromNull)
  else
    DoError(SErrCannotConvertToNull);
end;

{$warnings off}
function TJSONNull.GetAsBoolean: Boolean;
begin
  ConvertError(True);
  Result:=false;
end;

function TJSONNull.GetAsFloat: TJSONFloat;
begin
  ConvertError(True);
  Result:=0.0;
end;

function TJSONNull.GetAsInteger: Integer;
begin
  ConvertError(True);
  Result:=0;
end;

{$ifdef fpc}
function TJSONNull.GetAsInt64: Int64;
begin
  ConvertError(True);
end;

function TJSONNull.GetAsQWord: QWord;
begin
  ConvertError(True);
end;
{$endif}

function TJSONNull.GetIsNull: Boolean;
begin
  Result:=True;
end;

procedure TJSONNull.SetAsBoolean(const AValue: Boolean);
begin
  ConvertError(False);
  if AValue then ;
end;

procedure TJSONNull.SetAsFloat(const AValue: TJSONFloat);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONNull.SetAsInteger(const AValue: Integer);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

{$ifdef fpc}
procedure TJSONNull.SetAsInt64(const AValue: Int64);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONNull.SetAsQword(const AValue: QWord);
begin
  ConvertError(False);
  if AValue>0 then ;
end;
{$endif}

function TJSONNull.GetAsJSON: TJSONStringType;
begin
  Result:='null';
end;

function TJSONNull.GetAsString: TJSONStringType;
begin
  ConvertError(True);
  Result:='';
end;

procedure TJSONNull.SetAsString(const AValue: TJSONStringType);
begin
  ConvertError(True);
  if AValue='' then ;
end;


function TJSONNull.GetValue: TJSONVariant;
begin
  Result:={$ifdef pas2js}js.Null{$else}variants.Null{$endif};
end;

procedure TJSONNull.SetValue(const AValue: TJSONVariant);
begin
  ConvertError(False);
  {$ifdef pas2js}
  if AValue=0 then ;
  {$else}
  if VarType(AValue)=0 then ;
  {$endif}
end;

class function TJSONNull.JSONType: TJSONType;
begin
  Result:=jtNull;
end;

procedure TJSONNull.Clear;
begin
  // Do nothing
end;

function TJSONNull.Clone: TJSONData;
begin
  Result:=TJSONNullClass(Self.ClassType).Create;
end;

{$warnings on}



{ TJSONFloatNumber }

function TJSONFloatNumber.GetAsBoolean: Boolean;
begin
  Result:=(FValue<>0);
end;

function TJSONFloatNumber.GetAsFloat: TJSONFloat;
begin
  Result:=FValue;
end;

function TJSONFloatNumber.GetAsInteger: Integer;
begin
  Result:=Round(FValue);
end;

{$ifdef fpc}
function TJSONFloatNumber.GetAsInt64: Int64;
begin
  Result:=Round(FValue);
end;

function TJSONFloatNumber.GetAsQWord: QWord;
begin
  Result:=Round(FValue);
end;
{$endif}

procedure TJSONFloatNumber.SetAsBoolean(const AValue: Boolean);
begin
  FValue:=Ord(AValue);
end;

procedure TJSONFloatNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue:=AValue;
end;

procedure TJSONFloatNumber.SetAsInteger(const AValue: Integer);
begin
  FValue:=AValue;
end;

{$ifdef fpc}
procedure TJSONFloatNumber.SetAsInt64(const AValue: Int64);
begin
  FValue:=AValue;
end;

procedure TJSONFloatNumber.SetAsQword(const AValue: QWord);
begin
  FValue:=AValue;
end;
{$endif}

function TJSONFloatNumber.GetAsJSON: TJSONStringType;
begin
  Result:=AsString;
end;

function TJSONFloatNumber.GetAsString: TJSONStringType;
begin
  Str(FValue,Result);
  // Str produces a ' ' in front where the - can go.
  if (Result<>'') and (Result[1]=' ') then
    Delete(Result,1,1);
end;

procedure TJSONFloatNumber.SetAsString(const AValue: TJSONStringType);
Var
  C : Integer;
begin
  Val(AValue,FValue,C);
  If (C<>0) then
    Raise EConvertError.CreateFmt(SErrInvalidFloat,[AValue]);
end;


function TJSONFloatNumber.GetValue: TJSONVariant;
begin
  Result:=FValue;
end;

procedure TJSONFloatNumber.SetValue(const AValue: TJSONVariant);
begin
  FValue:={$ifdef pas2js}TJSONFloat(AValue){$else}AValue{$endif};
end;

constructor TJSONFloatNumber.Create(AValue: TJSONFloat);
begin
  FValue:=AValue;
end;

class function TJSONFloatNumber.NumberType: TJSONNumberType;
begin
  Result:=ntFloat;
end;

procedure TJSONFloatNumber.Clear;
begin
  FValue:=0;
end;

function TJSONFloatNumber.Clone: TJSONData;

begin
  Result:=TJSONFloatNumberClass(ClassType).Create(Self.FValue);
end;

{ TJSONIntegerNumber }

function TJSONIntegerNumber.GetAsBoolean: Boolean;
begin
  Result:=FValue<>0;
end;

function TJSONIntegerNumber.GetAsFloat: TJSONFloat;
begin
  Result:=FValue;
end;

function TJSONIntegerNumber.GetAsInteger: Integer;
begin
  Result:=FValue;
end;

{$ifdef fpc}
function TJSONIntegerNumber.GetAsInt64: Int64;
begin
  Result:=FValue;
end;

function TJSONIntegerNumber.GetAsQWord: QWord;
begin
  result:=FValue;
end;
{$endif}

procedure TJSONIntegerNumber.SetAsBoolean(const AValue: Boolean);
begin
  FValue:=Ord(AValue);
end;

procedure TJSONIntegerNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue:=Round(AValue);
end;

procedure TJSONIntegerNumber.SetAsInteger(const AValue: Integer);
begin
  FValue:=AValue;
end;

{$ifdef fpc}
procedure TJSONIntegerNumber.SetAsInt64(const AValue: Int64);
begin
  FValue:=AValue;
end;

procedure TJSONIntegerNumber.SetAsQword(const AValue: QWord);
begin
  FValue:=AValue;
end;
{$endif}

function TJSONIntegerNumber.GetAsJSON: TJSONStringType;
begin
  Result:=AsString;
end;

function TJSONIntegerNumber.GetAsString: TJSONStringType;
begin
  Result:=IntToStr(FValue)
end;

procedure TJSONIntegerNumber.SetAsString(const AValue: TJSONStringType);
begin
  FValue:=StrToInt(AValue);
end;


function TJSONIntegerNumber.GetValue: TJSONVariant;
begin
  Result:=FValue;
end;

procedure TJSONIntegerNumber.SetValue(const AValue: TJSONVariant);
begin
  FValue:={$ifdef pas2js}Integer(AValue){$else}AValue{$endif};
end;

constructor TJSONIntegerNumber.Create(AValue: Integer);
begin
  FValue:=AValue;
end;

class function TJSONIntegerNumber.NumberType: TJSONNumberType;
begin
  Result:=ntInteger;
end;

procedure TJSONIntegerNumber.Clear;
begin
  FValue:=0;
end;

function TJSONIntegerNumber.Clone: TJSONData;

begin
  Result:=TJSONIntegerNumberClass(ClassType).Create(Self.FValue);
end;

{$ifdef fpc}
{ TJSONInt64Number }

function TJSONInt64Number.GetAsInt64: Int64;
begin
  Result := FValue;
end;

function TJSONInt64Number.GetAsQWord: QWord;
begin
  Result := FValue;
end;

procedure TJSONInt64Number.SetAsInt64(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONInt64Number.SetAsQword(const AValue: QWord);
begin
  FValue := AValue;
end;

function TJSONInt64Number.GetAsBoolean: Boolean;
begin
  Result:=FValue<>0;
end;

function TJSONInt64Number.GetAsFloat: TJSONFloat;
begin
  Result:= FValue;
end;

function TJSONInt64Number.GetAsInteger: Integer;
begin
  Result := FValue;
end;

procedure TJSONInt64Number.SetAsBoolean(const AValue: Boolean);
begin
  FValue:=Ord(AValue);
end;

procedure TJSONInt64Number.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue:=Round(AValue);
end;

procedure TJSONInt64Number.SetAsInteger(const AValue: Integer);
begin
  FValue:=AValue;
end;

function TJSONInt64Number.GetAsJSON: TJSONStringType;
begin
  Result:=AsString;
end;

function TJSONInt64Number.GetAsString: TJSONStringType;
begin
  Result:=IntToStr(FValue)
end;

procedure TJSONInt64Number.SetAsString(const AValue: TJSONStringType);
begin
  FValue:=StrToInt64(AValue);
end;

function TJSONInt64Number.GetValue: TJSONVariant;
begin
  Result:=FValue;
end;

procedure TJSONInt64Number.SetValue(const AValue: TJSONVariant);
begin
  FValue:=AValue;
end;

constructor TJSONInt64Number.Create(AValue: Int64);
begin
  FValue := AValue;
end;

class function TJSONInt64Number.NumberType: TJSONNumberType;
begin
  Result:=ntInt64;
end;

procedure TJSONInt64Number.Clear;
begin
  FValue:=0;
end;

function TJSONInt64Number.Clone: TJSONData;

begin
  Result:=TJSONInt64NumberClass(ClassType).Create(Self.FValue);
end;
{$endif}

{ TJSONArray }

function TJSONArray.GetBooleans(Index : Integer): Boolean;
begin
  Result:=Items[Index].AsBoolean;
end;

function TJSONArray.GetArrays(Index : Integer): TJSONArray;
begin
  Result:=Items[Index] as TJSONArray;
end;

function TJSONArray.GetFloats(Index : Integer): TJSONFloat;
begin
  Result:=Items[Index].AsFloat;
end;

function TJSONArray.GetIntegers(Index : Integer): Integer;
begin
  Result:=Items[Index].AsInteger;
end;

{$ifdef fpc}
function TJSONArray.GetInt64s(Index : Integer): Int64;
begin
  Result:=Items[Index].AsInt64;
end;
{$endif}

function TJSONArray.GetNulls(Index : Integer): Boolean;
begin
  Result:=Items[Index].IsNull;
end;

function TJSONArray.GetObjects(Index : Integer): TJSONObject;
begin
  Result:=Items[Index] as TJSONObject;
end;

{$ifdef fpc}
function TJSONArray.GetQWords(Index : Integer): QWord;
begin
  Result:=Items[Index].AsQWord;
end;
{$endif}

function TJSONArray.GetStrings(Index : Integer): TJSONStringType;
begin
  Result:=Items[Index].AsString;
end;

{$ifdef fpc}
function TJSONArray.GetUnicodeStrings(Index : Integer): TJSONUnicodeStringType;
begin
  Result:=Items[Index].AsUnicodeString;
end;
{$endif}

function TJSONArray.GetTypes(Index : Integer): TJSONType;
begin
  Result:=Items[Index].JSONType;
end;

procedure TJSONArray.SetArrays(Index : Integer; const AValue: TJSONArray);
begin
  Items[Index]:=AValue;
end;

procedure TJSONArray.SetBooleans(Index : Integer; const AValue: Boolean);

begin
  Items[Index]:=CreateJSON(AValue);
end;

procedure TJSONArray.SetFloats(Index : Integer; const AValue: TJSONFloat);
begin
  Items[Index]:=CreateJSON(AValue);
end;

procedure TJSONArray.SetIntegers(Index : Integer; const AValue: Integer);
begin
  Items[Index]:=CreateJSON(AValue);
end;

{$ifdef fpc}
procedure TJSONArray.SetInt64s(Index : Integer; const AValue: Int64);
begin
  Items[Index]:=CreateJSON(AValue);
end;
{$endif}

procedure TJSONArray.SetObjects(Index : Integer; const AValue: TJSONObject);
begin
  Items[Index]:=AValue;
end;

{$ifdef fpc}
procedure TJSONArray.SetQWords(Index : Integer; AValue: QWord);
begin
  Items[Index]:=CreateJSON(AValue);
end;
{$endif}

procedure TJSONArray.SetStrings(Index : Integer; const AValue: TJSONStringType);
begin
  Items[Index]:=CreateJSON(AValue);
end;

{$ifdef fpc}
procedure TJSONArray.SetUnicodeStrings(Index: Integer;
  const AValue: TJSONUnicodeStringType);
begin
  Items[Index]:=CreateJSON(AValue);
end;
{$endif}

function TJSONArray.DoFindPath(const APath: TJSONStringType; out
  NotFound: TJSONStringType): TJSONdata;

Var
  P,I : integer;
  E : String;

begin
  if (APath<>'') and (APath[1]='[') then
    begin
    P:=Pos(']',APath);
    I:=-1;
    If (P>2) then
      I:=StrToIntDef(Copy(APath,2,P-2),-1);
    If (I>=0) and (I<Count) then
       begin
       E:=APath;
       System.Delete(E,1,P);
       Result:=Items[i].DoFindPath(E,NotFound);
       end
    else
       begin
       Result:=Nil;
       If (P>0) then
         NotFound:=Copy(APath,1,P)
       else
         NotFound:=APath;
       end;
    end
  else
    Result:=inherited DoFindPath(APath, NotFound);
end;

procedure TJSONArray.Converterror(From: Boolean);
begin
  If From then
    DoError(SErrCannotConvertFromArray)
  else
    DoError(SErrCannotConvertToArray);
end;

{$warnings off}
function TJSONArray.GetAsBoolean: Boolean;
begin
  ConvertError(True);
  Result:=false;
end;

function TJSONArray.GetAsFloat: TJSONFloat;
begin
  ConvertError(True);
  Result:=0.0;
end;

function TJSONArray.GetAsInteger: Integer;
begin
  ConvertError(True);
  Result:=0;
end;

{$ifdef fpc}
function TJSONArray.GetAsInt64: Int64;
begin
  ConvertError(True);
end;

function TJSONArray.GetAsQWord: QWord;
begin
  ConvertError(True);
end;
{$endif}

procedure TJSONArray.SetAsBoolean(const AValue: Boolean);
begin
  ConvertError(False);
  if AValue then ;
end;

procedure TJSONArray.SetAsFloat(const AValue: TJSONFloat);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONArray.SetAsInteger(const AValue: Integer);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

{$ifdef fpc}
procedure TJSONArray.SetAsInt64(const AValue: Int64);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONArray.SetAsQword(const AValue: QWord);
begin
  ConvertError(False);
  if AValue>0 then ;
end;
{$endif}

{$warnings on}


function TJSONArray.GetAsJSON: TJSONStringType;

Var
  I : Integer;
  Sep : String;
  D : TJSONData;
  V : TJSONStringType;

begin
  Sep:=TJSONData.FElementSep;
  Result:='[';
  For I:=0 to Count-1 do
    begin
    D:=Items[i];
    if D<>Nil then
      V:=D.AsJSON
    else
      V:='null';
    Result:=Result+V;
    If (I<Count-1) then
      Result:=Result+Sep;
    end;
  Result:=Result+']';
end;

Function IndentString(Options : TFormatOptions; Indent : Integer) : TJSONStringType;

begin
  If (foUseTabChar in Options) then
    Result:=StringofChar(#9,Indent)
  else
    Result:=StringOfChar(' ',Indent);  
end;

function TJSONArray.DoFormatJSON(Options: TFormatOptions; CurrentIndent,
  Indent: Integer): TJSONStringType;

Var
  I : Integer;
  MultiLine : Boolean;
  SkipWhiteSpace : Boolean;
  Ind : String;
  
begin
  Result:='[';
  MultiLine:=Not (foSingleLineArray in Options);
  SkipWhiteSpace:=foSkipWhiteSpace in Options;
  Ind:=IndentString(Options, CurrentIndent+Indent);
  if MultiLine then
    Result:=Result+sLineBreak;
  For I:=0 to Count-1 do
    begin
    if MultiLine then
      Result:=Result+Ind;
    if Items[i]=Nil then
      Result:=Result+'null'
    else
      Result:=Result+Items[i].DoFormatJSON(Options,CurrentIndent+Indent,Indent);
    If (I<Count-1) then
      if MultiLine then
        Result:=Result+','
      else
        Result:=Result+ElementSeps[SkipWhiteSpace];
    if MultiLine then
      Result:=Result+sLineBreak
    end;
  if MultiLine then
    Result:=Result+IndentString(Options, CurrentIndent);
  Result:=Result+']';
end;


{$warnings off}
function TJSONArray.GetAsString: TJSONStringType;
begin
  ConvertError(True);
  Result:='';
end;

procedure TJSONArray.SetAsString(const AValue: TJSONStringType);
begin
  ConvertError(False);
  if AValue='' then ;
end;

function TJSONArray.GetValue: TJSONVariant;
begin
  ConvertError(True);
  Result:=0;
end;

procedure TJSONArray.SetValue(const AValue: TJSONVariant);
begin
  ConvertError(False);
  {$ifdef pas2js}
  if AValue=0 then ;
  {$else}
  if VarType(AValue)=0 then ;
  {$endif}
end;
{$warnings on}

function TJSONArray.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TJSONArray.GetItem(Index: Integer): TJSONData;
begin
  Result:=FList[Index] as TJSONData;
end;

procedure TJSONArray.SetItem(Index: Integer; const AValue: TJSONData);
begin
  If (Index=FList.Count) then
    FList.Add(AValue)
  else
    FList[Index]:=AValue;
end;

constructor TJSONArray.Create;
begin
  Flist:=TFPObjectList.Create(True);
end;

{$ifdef pas2js}
Function VarRecToJSON(Const Element : jsvalue; const SourceType : String) : TJSONData;
var
  i: NativeInt;
  VObject: TObject;
begin
  Result:=nil;
  if Element=nil then
    Result:=CreateJSON // TJSONNull
  else if isBoolean(Element) then
    Result:=CreateJSON(boolean(Element))
  else if isString(Element) then
    Result:=CreateJSON(String(Element))
  else if isNumber(Element) then
    begin
    if isInteger(Element) then
      begin
      i:=NativeInt(Element);
      if (i>=low(integer)) and (i<=high(integer)) then
        Result:=CreateJSON(Integer(Element))
      else
        Result:=CreateJSON(TJSONFloat(Element));
      end
    else
      Result:=CreateJSON(TJSONFloat(Element));
    end
  else if isObject(Element) and (Element is TObject) then
    begin
    VObject:=TObject(Element);
    if VObject is TJSONData then
      Result:=TJSONData(VObject)
    else
      TJSONData.DoError(SErrNotJSONData,[VObject.ClassName,SourceType]);
    end
  else
    TJSONData.DoError(SErrUnknownTypeInConstructor,[SourceType,jsTypeOf(Element)]);
end;
{$else}
Function VarRecToJSON(Const Element : TVarRec; const SourceType : String) : TJSONData;

begin
  Result:=Nil;
  With Element do
    case VType of
      vtInteger    : Result:=CreateJSON(VInteger);
      vtBoolean    : Result:=CreateJSON(VBoolean);
      vtChar       : Result:=CreateJSON(VChar);
      vtExtended   : Result:=CreateJSON(VExtended^);
      vtString     : Result:=CreateJSON(vString^);
      vtAnsiString : Result:=CreateJSON(UTF8Decode(StrPas(VPChar)));
      vtUnicodeString: Result:=CreateJSON(UnicodeString(VUnicodeString));
      vtWideString: Result:=CreateJSON(WideString(VWideString));
      vtPChar      : Result:=CreateJSON(StrPas(VPChar));
      vtPointer    : If (VPointer<>Nil) then
                       TJSONData.DoError(SErrPointerNotNil,[SourceType])
                     else
                       Result:=CreateJSON();
      vtCurrency   : Result:=CreateJSON(vCurrency^);
      vtInt64      : Result:=CreateJSON(vInt64^);
      vtObject     : if (VObject is TJSONData) then
                       Result:=TJSONData(VObject)
                     else
                       TJSONData.DoError(SErrNotJSONData,[VObject.ClassName,SourceType]);
      //vtVariant    :
    else
      TJSONData.DoError(SErrUnknownTypeInConstructor,[SourceType,VType])
    end;
end;
{$endif}

constructor TJSONArray.Create(const Elements: array of {$ifdef pas2js}jsvalue{$else}Const{$endif});

Var
  I : integer;
  J : TJSONData;

begin
  Create;
  For I:=Low(Elements) to High(Elements) do
    begin
    J:=VarRecToJSON(Elements[i],'Array');
    Add(J);
    end;
end;

destructor TJSONArray.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

class function TJSONArray.JSONType: TJSONType;
begin
  Result:=jtArray;
end;

function TJSONArray.Clone: TJSONData;

Var
  A : TJSONArray;
  I : Integer;

begin
  A:=TJSONArrayClass(ClassType).Create;
  try
    For I:=0 to Count-1 do
      A.Add(Self.Items[I].Clone);
    Result:=A;
  except
    A.Free;
    Raise;
  end;
end;

procedure TJSONArray.Iterate(Iterator: TJSONArrayIterator; Data: TObject);

Var
  I : Integer;
  Cont : Boolean;
  
begin
  I:=0;
  Cont:=True;
  While (I<FList.Count) and cont do
    begin
    Iterator(Items[i],Data,Cont);
    Inc(I);
    end;
end;

function TJSONArray.IndexOf(obj: TJSONData): Integer;
begin
  Result:=FList.IndexOf(Obj);
end;

function TJSONArray.GetEnumerator: TBaseJSONEnumerator;
begin
  Result:=TJSONArrayEnumerator.Create(Self);
end;

procedure TJSONArray.Clear;
begin
  FList.Clear;
end;

function TJSONArray.Add(Item: TJSONData): Integer;
begin
  Result:=FList.Add(Item);
end;

function TJSONArray.Add(I: Integer): Integer;
begin
  Result:=Add(CreateJSON(I));
end;

{$ifdef fpc}
function TJSONArray.Add(I: Int64): Int64;
begin
  Result:=Add(CreateJSON(I));
end;

function TJSONArray.Add(I: QWord): QWord;
begin
  Result:=Add(CreateJSON(I));
end;
{$endif}

function TJSONArray.Add(const S: String): Integer;
begin
  Result:=Add(CreateJSON(S));
end;

{$ifdef fpc}
function TJSONArray.Add(const S: UnicodeString): Integer;
begin
  Result:=Add(CreateJSON(S));
end;
{$endif}

function TJSONArray.Add: Integer;
begin
  Result:=Add(CreateJSON);
end;

function TJSONArray.Add(F: TJSONFloat): Integer;
begin
  Result:=Add(CreateJSON(F));
end;

function TJSONArray.Add(B: Boolean): Integer;
begin
  Result:=Add(CreateJSON(B));
end;

function TJSONArray.Add(AnArray: TJSONArray): Integer;
begin
  If (IndexOf(AnArray)<>-1) then
    DoError(SErrCannotAddArrayTwice);
  Result:=Add(TJSONData(AnArray));
end;

function TJSONArray.Add(AnObject: TJSONObject): Integer;
begin
  If (IndexOf(AnObject)<>-1) then
    DoError(SErrCannotAddObjectTwice);
  Result:=Add(TJSONData(AnObject));
end;

procedure TJSONArray.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TJSONArray.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TJSONArray.Extract(Item: TJSONData): TJSONData;
begin
  Result := TJSONData(FList.Extract(Item));
end;

function TJSONArray.Extract(Index: Integer): TJSONData;
begin
  Result := TJSONData(FList.Extract(FList.Items[Index]));
end;

procedure TJSONArray.Insert(Index: Integer);
begin
  Insert(Index,CreateJSON);
end;

procedure TJSONArray.Insert(Index: Integer; Item: TJSONData);
begin
  FList.Insert(Index, Item);
end;

procedure TJSONArray.Insert(Index: Integer; I: Integer);
begin
  FList.Insert(Index, CreateJSON(I));
end;

{$ifdef fpc}
procedure TJSONArray.Insert(Index: Integer; I: Int64);
begin
  FList.Insert(Index, CreateJSON(I));
end;

procedure TJSONArray.Insert(Index: Integer; I: QWord);
begin
  FList.Insert(Index, CreateJSON(I));
end;
{$endif}

procedure TJSONArray.Insert(Index: Integer; const S: String);
begin
  FList.Insert(Index, CreateJSON(S));
end;

{$ifdef fpc}
procedure TJSONArray.Insert(Index: Integer; const S: UnicodeString);
begin
  FList.Insert(Index, CreateJSON(S));
end;
{$endif}

procedure TJSONArray.Insert(Index: Integer; F: TJSONFloat);
begin
  FList.Insert(Index, CreateJSON(F));
end;

procedure TJSONArray.Insert(Index: Integer; B: Boolean);
begin
  FList.Insert(Index, CreateJSON(B));
end;

procedure TJSONArray.Insert(Index: Integer; AnArray: TJSONArray);
begin
  if (IndexOf(AnArray)<>-1) then
    DoError(SErrCannotAddArrayTwice);
  FList.Insert(Index, AnArray);
end;

procedure TJSONArray.Insert(Index: Integer; AnObject: TJSONObject);
begin
  if (IndexOf(AnObject)<>-1) then
    DoError(SErrCannotAddObjectTwice);
  FList.Insert(Index, AnObject);
end;

procedure TJSONArray.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TJSONArray.Remove(Item: TJSONData);
begin
  FList.Remove(Item);
end;

procedure TJSONArray.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

{ TJSONObject }

function TJSONObject.GetArrays(const AName: String): TJSONArray;
begin
  Result:=GetElements(AName) as TJSONArray;
end;

function TJSONObject.GetBooleans(const AName: String): Boolean;
begin
  Result:=GetElements(AName).AsBoolean;
end;

function TJSONObject.GetElements(const AName: string): TJSONData;
begin
  {$ifdef pas2js}
  if FHash.hasOwnProperty('%'+AName) then
    Result:=TJSONData(FHash['%'+AName])
  else
    DoError(SErrNonexistentElement,[AName]);
  {$else}
  Result:=TJSONData(FHash.Find(AName));
  If (Result=Nil) then
    DoError(SErrNonexistentElement,[AName]);
  {$endif}
end;

function TJSONObject.GetFloats(const AName: String): TJSONFloat;
begin
  Result:=GetElements(AName).AsFloat;
end;

function TJSONObject.GetIntegers(const AName: String): Integer;
begin
  Result:=GetElements(AName).AsInteger;
end;

{$ifdef fpc}
function TJSONObject.GetInt64s(const AName: String): Int64;
begin
  Result:=GetElements(AName).AsInt64;
end;
{$endif}

function TJSONObject.GetIsNull(const AName: String): Boolean;
begin
  Result:=GetElements(AName).IsNull;
end;

function TJSONObject.GetNameOf(Index: Integer): TJSONStringType;
begin
  {$ifdef pas2js}
  if FNames=nil then
    FNames:=TJSObject.getOwnPropertyNames(FHash);
  if (Index<0) or (Index>=FCount) then
    DoError(SListIndexError,[Index]);
  Result:=copy(FNames[Index],2);
  {$else}
  Result:=FHash.NameOfIndex(Index);
  {$endif}
end;

function TJSONObject.GetObjects(const AName : String): TJSONObject;
begin
  Result:=GetElements(AName) as TJSONObject;
end;

{$ifdef fpc}
function TJSONObject.GetQWords(AName : String): QWord;
begin
  Result:=GetElements(AName).AsQWord;
end;
{$endif}

function TJSONObject.GetStrings(const AName : String): TJSONStringType;
begin
  Result:=GetElements(AName).AsString;
end;

{$ifdef fpc}
function TJSONObject.GetUnicodeStrings(const AName: String
  ): TJSONUnicodeStringType;
begin
  Result:=GetElements(AName).AsUnicodeString;
end;
{$endif}

function TJSONObject.GetTypes(const AName : String): TJSONType;
begin
  Result:=Getelements(Aname).JSONType;
end;

class function TJSONObject.GetUnquotedMemberNames: Boolean; {$ifdef fpc}static;{$endif}
begin
  Result:=FUnquotedMemberNames;
end;

procedure TJSONObject.SetArrays(const AName : String; const AValue: TJSONArray);

begin
  SetElements(AName,AVAlue);
end;

procedure TJSONObject.SetBooleans(const AName : String; const AValue: Boolean);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;

procedure TJSONObject.SetElements(const AName: string; const AValue: TJSONData);
{$ifdef pas2js}
begin
  if not FHash.hasOwnProperty('%'+AName) then
    inc(FCount);
  FHash['%'+AName]:=AValue;
  FNames:=nil;
end;
{$else}
Var
  Index : Integer;

begin
  Index:=FHash.FindIndexOf(AName);
  If (Index=-1) then
    FHash.Add(AName,AValue)
  else
    FHash.Items[Index]:=AValue; // Will free the previous value.
end;
{$endif}

procedure TJSONObject.SetFloats(const AName : String; const AValue: TJSONFloat);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;

procedure TJSONObject.SetIntegers(const AName : String; const AValue: Integer);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;

{$ifdef fpc}
procedure TJSONObject.SetInt64s(const AName : String; const AValue: Int64);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;
{$endif}

procedure TJSONObject.SetIsNull(const AName : String; const AValue: Boolean);
begin
  If Not AValue then
    DoError(SErrCannotSetNotIsNull);
  SetElements(AName,CreateJSON);
end;

procedure TJSONObject.SetObjects(const AName : String; const AValue: TJSONObject);
begin
  SetElements(AName,AValue);
end;

{$ifdef fpc}
procedure TJSONObject.SetQWords(AName : String; AValue: QWord);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;
{$endif}

procedure TJSONObject.SetStrings(const AName : String; const AValue: TJSONStringType);
begin
  SetElements(AName,CreateJSON(AValue));
end;

{$ifdef fpc}
procedure TJSONObject.SetUnicodeStrings(const AName: String;
  const AValue: TJSONUnicodeStringType);
begin
  SetElements(AName,CreateJSON(AValue));
end;
{$endif}

class procedure TJSONObject.DetermineElementQuotes;

begin
  FObjStartSep:=ObjStartSeps[TJSONData.FCompressedJSON];
  FObjEndSep:=ObjEndSeps[TJSONData.FCompressedJSON];
  if TJSONData.FCompressedJSON then
    FElementEnd:=UnSpacedQuoted[FUnquotedMemberNames]
  else
    FElementEnd:=SpacedQuoted[FUnquotedMemberNames];
  FElementStart:=ElementStart[FUnquotedMemberNames]
end;

class procedure TJSONObject.SetUnquotedMemberNames(AValue: Boolean); {$ifdef fpc}static;{$endif}

begin
  if FUnquotedMemberNames=AValue then exit;
  FUnquotedMemberNames:=AValue;
  DetermineElementQuotes;
end;

function TJSONObject.DoFindPath(const APath: TJSONStringType; out
  NotFound: TJSONStringType): TJSONdata;

Var
  N: TJSONStringType;
  L,P,P2 : Integer;

begin
  If (APath='') then
    Exit(Self);
  N:=APath;
  L:=Length(N);
  P:=1;
  While (P<L) and (N[P]='.') do
    inc(P);
  P2:=P;
  While (P2<=L) and (Not (N[P2] in ['.','['])) do
    inc(P2);
   N:=Copy(APath,P,P2-P);
   If (N='') then
     Result:=Self
   else
     begin
     Result:=Find(N);
     If Result=Nil then
       NotFound:=N+Copy(APath,P2,L-P2)
     else
       begin
       N:=Copy(APath,P2,L-P2+1);
       Result:=Result.DoFindPath(N,NotFound);
       end;
     end;
end;

procedure TJSONObject.Converterror(From: Boolean);
begin
  If From then
    DoError(SErrCannotConvertFromObject)
  else
    DoError(SErrCannotConvertToObject);
end;

{$warnings off}
function TJSONObject.GetAsBoolean: Boolean;
begin
  ConvertError(True);
  Result:=false;
end;

function TJSONObject.GetAsFloat: TJSONFloat;
begin
  ConvertError(True);
  Result:=0.0;
end;

function TJSONObject.GetAsInteger: Integer;
begin
  ConvertError(True);
  Result:=0;
end;

{$ifdef fpc}
function TJSONObject.GetAsInt64: Int64;
begin
  ConvertError(True);
end;

function TJSONObject.GetAsQWord: QWord;
begin
  ConvertError(True);
end;
{$endif}

procedure TJSONObject.SetAsBoolean(const AValue: Boolean);
begin
  ConvertError(False);
  if AValue then ;
end;

procedure TJSONObject.SetAsFloat(const AValue: TJSONFloat);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONObject.SetAsInteger(const AValue: Integer);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

{$ifdef fpc}
procedure TJSONObject.SetAsInt64(const AValue: Int64);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONObject.SetAsQword(const AValue: QWord);
begin
  ConvertError(False);
  if AValue>0 then ;
end;
{$endif}

{$warnings on}

function TJSONObject.GetAsJSON: TJSONStringType;

Var
  I : Integer;
  Sep : String;
  V : TJSONStringType;
  D : TJSONData;

begin
  Sep:=TJSONData.FElementSep;
  Result:='';
  For I:=0 to Count-1 do
    begin
    If (Result<>'') then
      Result:=Result+Sep;
    D:=Items[i];
    if Assigned(D) then
      V:=Items[I].AsJSON
    else
      V:='null';
    Result:=Result+FElementStart+StringToJSONString(Names[i])+FElementEnd+V;
    end;
  If (Result<>'') then
    Result:=FObjStartSep+Result+FObjEndSep
  else
    Result:='{}';
end;

{$warnings off}
function TJSONObject.GetAsString: TJSONStringType;
begin
  ConvertError(True);
  Result:='';
end;

procedure TJSONObject.SetAsString(const AValue: TJSONStringType);
begin
  ConvertError(False);
  if AValue='' then ;
end;

function TJSONObject.GetValue: TJSONVariant;
begin
  ConvertError(True);
  Result:=0;
end;

procedure TJSONObject.SetValue(const AValue: TJSONVariant);
begin
  ConvertError(False);
  {$ifdef pas2js}
  if AValue=0 then ;
  {$else}
  if VarType(AValue)=0 then ;
  {$endif}
end;
{$warnings on}

function TJSONObject.GetCount: Integer;
begin
  {$ifdef pas2js}
  Result:=FCount;
  {$else}
  Result:=FHash.Count;
  {$endif}
end;

function TJSONObject.GetItem(Index: Integer): TJSONData;
begin
  {$ifdef pas2js}
  Result:=GetElements(GetNameOf(Index));
  {$else}
  Result:=TJSONData(FHash.Items[Index]);
  {$endif}
end;

procedure TJSONObject.SetItem(Index: Integer; const AValue: TJSONData);
begin
  {$ifdef pas2js}
  SetElements(GetNameOf(Index),AValue);
  {$else}
  FHash.Items[Index]:=AValue;
  {$endif}
end;

constructor TJSONObject.Create;
begin
  {$ifdef pas2js}
  FHash:=TJSObject.new;
  {$else}
  FHash:=TFPHashObjectList.Create(True);
  {$endif}
end;

constructor TJSONObject.Create(const Elements: array of {$ifdef pas2js}jsvalue{$else}Const{$endif});

Var
  I : integer;
  AName : TJSONUnicodeStringType;
  J : TJSONData;

begin
  Create;
  If ((High(Elements)-Low(Elements)) mod 2)=0 then
    DoError(SErrOddNumber);
  I:=Low(Elements);
  While I<=High(Elements) do
    begin
    {$ifdef pas2js}
    if isString(Elements[I]) then
      AName:=String(Elements[I])
    else
      DoError(SErrNameMustBeString,[I+1]);
    {$else}
    With Elements[i] do
      Case VType of
        vtChar       : AName:=TJSONUnicodeStringType(VChar);
        vtString     : AName:=TJSONUnicodeStringType(vString^);
        vtAnsiString : AName:=UTF8Decode(StrPas(VPChar));
        vtPChar      : AName:=TJSONUnicodeStringType(StrPas(VPChar));
      else
        DoError(SErrNameMustBeString,[I+1]);
      end;
    {$endif}
    If (AName='') then
      DoError(SErrNameMustBeString,[I+1]);
    Inc(I);
    J:=VarRecToJSON(Elements[i],'Object');
    {$IFDEF FPC_HAS_CPSTRING}
    Add(UTF8Encode(AName),J);
    {$ELSE}
    Add(AName,J);
    {$ENDIF}
    Inc(I);
    end;
end;

destructor TJSONObject.Destroy;
begin
  {$ifdef pas2js}
  FHash:=nil;
  {$else}
  FreeAndNil(FHash);
  {$endif}
  inherited Destroy;
end;

class function TJSONObject.JSONType: TJSONType;
begin
  Result:=jtObject;
end;

function TJSONObject.Clone: TJSONData;

Var
  O : TJSONObject;
  I: Integer;

begin
  O:=TJSONObjectClass(ClassType).Create;
  try
    For I:=0 to Count-1 do
      O.Add(Self.Names[I],Self.Items[I].Clone);
    Result:=O;
  except
    FreeAndNil(O);
    Raise;
  end;
end;

function TJSONObject.GetEnumerator: TBaseJSONEnumerator;
begin
  Result:=TJSONObjectEnumerator.Create(Self);
end;


function TJSONObject.DoFormatJSON(Options: TFormatOptions; CurrentIndent,
  Indent: Integer): TJSONStringType;

Var
  i : Integer;
  S : TJSONStringType;
  MultiLine,UseQuotes, SkipWhiteSpace,SkipWhiteSpaceOnlyLeading : Boolean;
  NSep,Sep,Ind : String;
  V : TJSONStringType;
  D : TJSONData;

begin
  Result:='';
  UseQuotes:=Not (foDoNotQuoteMembers in options);
  MultiLine:=Not (foSingleLineObject in Options);
  SkipWhiteSpace:=foSkipWhiteSpace in Options;
  SkipWhiteSpaceOnlyLeading:=foSkipWhiteSpaceOnlyLeading in Options;
  CurrentIndent:=CurrentIndent+Indent;
  Ind:=IndentString(Options, CurrentIndent);
  If SkipWhiteSpace then
    begin
    if SkipWhiteSpaceOnlyLeading then
      NSep:=': '
    else
      NSep:=':'
    end
  else
    NSep:=' : ';
  If MultiLine then
    Sep:=','+SLineBreak+Ind
  else if SkipWhiteSpace then
    Sep:=','
  else
    Sep:=', ';
  For I:=0 to Count-1 do
    begin
    If (I>0) then
      Result:=Result+Sep
    else If MultiLine then
      Result:=Result+Ind;
    S:=StringToJSONString(Names[i]);
    If UseQuotes then
      S:='"'+S+'"';
    D:=Items[i];
    if D=Nil then
      V:='null'
    else
      v:=Items[I].DoFormatJSON(Options,CurrentIndent,Indent);
    Result:=Result+S+NSep+V;
    end;
  If (Result<>'') then
    begin
    if MultiLine then
      Result:='{'+sLineBreak+Result+sLineBreak+indentString(options,CurrentIndent-Indent)+'}'
    else
      Result:=ObjStartSeps[SkipWhiteSpace]+Result+ObjEndSeps[SkipWhiteSpace]
    end
  else
    Result:='{}';
end;

procedure TJSONObject.Iterate(Iterator: TJSONObjectIterator; Data: TObject);
{$ifdef pas2js}
var
  i: Integer;
  Cont: Boolean;
begin
  if FNames=nil then
    FNames:=TJSObject.getOwnPropertyNames(FHash);
  Cont:=True;
  for i:=0 to length(FNames) do
    begin
    Iterator(copy(FNames[I],2),TJSONData(FHash[FNames[i]]),Data,Cont);
    if not Cont then break;
    end;
end;
{$else}
Var
  I : Integer;
  Cont : Boolean;

begin
  I:=0;
  Cont:=True;
  While (I<FHash.Count) and Cont do
    begin
    Iterator(Names[I],Items[i],Data,Cont);
    Inc(I);
    end;
end;
{$endif}

function TJSONObject.IndexOf(Item: TJSONData): Integer;
begin
  {$ifdef pas2js}
  if FNames=nil then
    FNames:=TJSObject.getOwnPropertyNames(FHash);
  for Result:=0 to length(FNames)-1 do
    if TJSONData(FHash[FNames[Result]])=Item then exit;
  Result:=-1;
  {$else}
  Result:=FHash.IndexOf(Item);
  {$endif}
end;

function TJSONObject.IndexOfName(const AName: TJSONStringType; CaseInsensitive : Boolean = False): Integer;
begin
  {$ifdef pas2js}
  if FNames=nil then
    FNames:=TJSObject.getOwnPropertyNames(FHash);
  Result:=TJSArray(FNames).indexOf('%'+AName); // -1 if not found
  {$else}
  Result:=FHash.FindIndexOf(AName);
  {$endif}
  if (Result<0) and CaseInsensitive then
    begin
    Result:=Count-1;
    While (Result>=0) and (CompareText(Names[Result],AName)<>0) do
      Dec(Result);
    end;
end;

procedure TJSONObject.Clear;
begin
  {$ifdef pas2js}
  FCount:=0;
  FHash:=TJSObject.new;
  FNames:=nil;
  {$else}
  FHash.Clear;
  {$endif}
end;

function TJSONObject.DoAdd(const AName: TJSONStringType; AValue: TJSONData; FreeOnError : Boolean = True): Integer;
begin
  if {$ifdef pas2js}FHash.hasOwnProperty('%'+AName){$else}(IndexOfName(aName)<>-1){$endif} then
    begin
    if FreeOnError then
      FreeAndNil(AValue);
    DoError(SErrDuplicateValue,[aName]);
    end;
  {$ifdef pas2js}
  FHash['%'+AName]:=AValue;
  FNames:=nil;
  inc(FCount);
  Result:=FCount;
  {$else}
  Result:=FHash.Add(AName,AValue);
  {$endif}
end;

function TJSONObject.Add(const AName: TJSONStringType; AValue: TJSONData
  ): Integer;
begin
  Result:=DoAdd(aName,AValue,False);
end;

function TJSONObject.Add(const AName: TJSONStringType; AValue: Boolean
  ): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;

function TJSONObject.Add(const AName: TJSONStringType; AValue: TJSONFloat): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;

function TJSONObject.Add(const AName, AValue: TJSONStringType): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;

{$ifdef fpc}
function TJSONObject.Add(const AName: String; AValue: TJSONUnicodeStringType
  ): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;
{$endif}

function TJSONObject.Add(const AName: TJSONStringType; Avalue: Integer): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;

{$ifdef fpc}
function TJSONObject.Add(const AName: TJSONStringType; Avalue: Int64): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;

function TJSONObject.Add(const AName: TJSONStringType; Avalue: QWord): Integer;
begin
  Result:=DoAdd(AName,CreateJSON(AValue));
end;
{$endif}

function TJSONObject.Add(const AName: TJSONStringType): Integer;
begin
  Result:=DoAdd(AName,CreateJSON);
end;

function TJSONObject.Add(const AName: TJSONStringType; AValue: TJSONArray
  ): Integer;
begin
  Result:=DoAdd(AName,TJSONData(AValue),False);
end;

procedure TJSONObject.Delete(Index: Integer);
begin
  {$ifdef pas2js}
  if (Index<0) or (Index>=FCount) then
    DoError(SListIndexError,[Index]);
  JSDelete(FHash,'%'+GetNameOf(Index));
  FNames:=nil;
  dec(FCount);
  {$else}
  FHash.Delete(Index);
  {$endif}
end;

procedure TJSONObject.Delete(const AName: string);
{$ifdef pas2js}
begin
  if not FHash.hasOwnProperty('%'+AName) then exit;
  JSDelete(FHash,'%'+AName);
  FNames:=nil;
  dec(FCount);
end;
{$else}
Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  if (I<>-1) then
    Delete(I);
end;
{$endif}

procedure TJSONObject.Remove(Item: TJSONData);
{$ifdef pas2js}
var AName: String;
begin
  for AName in FHash do
    if FHash.hasOwnProperty(AName) and (FHash[AName]=Item) then
      begin
      JSDelete(FHash,AName);
      FNames:=nil;
      dec(FCount);
      exit;
      end;
end;
{$else}
begin
  FHash.Remove(Item);
end;
{$endif}

{$ifdef fpc}
function TJSONObject.Extract(Index: Integer): TJSONData;
begin
  Result:=Items[Index];
  FHash.Extract(Result);
end;

function TJSONObject.Extract(const AName: string): TJSONData;

Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  if (I<>-1) then
    Result:=Extract(I)
  else
    Result:=Nil
end;
{$endif}

function TJSONObject.Get(const AName: String): TJSONVariant;
{$ifdef pas2js}
begin
  if FHash.hasOwnProperty('%'+AName) then
    Result:=TJSONData(FHash['%'+AName]).Value
  else
    Result:=nil;
end;
{$else}
Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I<>-1) then
    Result:=Items[i].Value
  else
    Result:=Null;
end;
{$endif}

function TJSONObject.Get(const AName: String; ADefault: TJSONFloat
  ): TJSONFloat;

Var
  D : TJSONData;

begin
  D:=Find(AName,jtNumber);
  If D<>Nil then
    Result:=D.AsFloat
  else
    Result:=ADefault;
end;

function TJSONObject.Get(const AName: String; ADefault: Integer
  ): Integer;

Var
  D : TJSONData;

begin
  D:=Find(AName,jtNumber);
  If D<>Nil then
    Result:=D.AsInteger
  else
    Result:=ADefault;
end;

{$ifdef fpc}
function TJSONObject.Get(const AName: String; ADefault: Int64): Int64;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtNumber);
  If D<>Nil then
    Result:=D.AsInt64
  else
    Result:=ADefault;
end;

function TJSONObject.Get(const AName: String; ADefault: QWord): QWord;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtNumber);
  If D<>Nil then
    Result:=D.AsQWord
  else
    Result:=ADefault;
end;
{$endif}

function TJSONObject.Get(const AName: String; ADefault: Boolean
  ): Boolean;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtBoolean);
  If D<>Nil then
    Result:=D.AsBoolean
  else
    Result:=ADefault;
end;

function TJSONObject.Get(const AName: String; ADefault: TJSONStringType
  ): TJSONStringType;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtString);
  If (D<>Nil) then
    Result:=D.AsString
  else
    Result:=ADefault;
end;

{$ifdef fpc}
function TJSONObject.Get(const AName: String; ADefault: TJSONUnicodeStringType
  ): TJSONUnicodeStringType;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtString);
  If (D<>Nil) then
    Result:=D.AsUnicodeString
  else
    Result:=ADefault;
end;
{$endif}

function TJSONObject.Get(const AName: String; ADefault: TJSONArray
  ): TJSONArray;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtArray);
  If (D<>Nil) then
    Result:=TJSONArray(D)
  else
    Result:=ADefault;
end;

function TJSONObject.Get(const AName: String; ADefault: TJSONObject
  ): TJSONObject;
Var
  D : TJSONData;

begin
  D:=Find(AName,jtObject);
  If (D<>Nil) then
    Result:=TJSONObject(D)
  else
    Result:=ADefault;
end;

function TJSONObject.Find(const AName: String): TJSONData;
{$ifdef pas2js}
begin
  if FHash.hasOwnProperty('%'+AName) then
    Result:=TJSONData(FHash['%'+AName])
  else
    Result:=nil;
end;
{$else}
Var
  I : Integer;

begin
  I:=IndexOfName(AName);
  If (I<>-1) then
    Result:=Items[i]
  else
    Result:=Nil;
end;
{$endif}

function TJSONObject.Find(const AName: String; AType: TJSONType): TJSONData;
begin
  Result:=Find(AName);
  If Assigned(Result) and (Result.JSONType<>AType) then
    Result:=Nil;
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONData): boolean;
begin
  AValue := Find(key);
  Result := assigned(AValue);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONObject): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtObject);
  if Result then
    AValue := TJSONObject(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONArray): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtArray);
  if Result then
    AValue := TJSONArray(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONString): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtString);
  if Result then
    AValue := TJSONString(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONBoolean): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtBoolean);
  if Result then
    AValue := TJSONBoolean(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONNumber): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtNumber);
  if Result then
    AValue := TJSONNumber(v);
end;

initialization
  // Need to force initialization;
  TJSONData.DetermineElementSeparators;
  TJSONObject.DetermineElementQuotes;
end.

