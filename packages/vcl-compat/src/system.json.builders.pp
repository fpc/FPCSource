unit System.JSON.Builders;

{$mode objfpc}
{$h+}
{$modeswitch functionreferences}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Rtti, System.SysUtils, System.Classes, System.Types, System.Generics.Collections,
  {$ELSE}
  Rtti, SysUtils, Classes, Types, Generics.Collections,
  {$ENDIF }
  System.JSON.Writers, System.JSON, System.JSON.Types, System.JSON.Readers;

type
  EJSONCollectionBuilderError = class(Exception);
  EJSONIteratorError = class(Exception);

  TJSONCollectionBuilder = class abstract
  public type

    TElements = class;
    TPairs = class;
    TParentCollection = class;

    TBaseCollection = class
    strict private
      FOwner: TJSONCollectionBuilder;
      FRootDepth: Integer;
      FWriter: TJSONWriter;
    private
      procedure addingElement;
      procedure addingPair;
      class procedure ErrorInvalidSetOfItems; static;
      procedure WriteVarRec(const aValue: TVarRec);
      procedure WriteVariant(const aValue: Variant);
      procedure WriteOpenArray(const aItems: array of const);
      function WriteReader(const aReader: TJsonReader; aOnlyEnclosed: Boolean): Boolean;
      procedure WriteBuilder(const aBuilder: TJSONCollectionBuilder);
      procedure WriteJSON(const aJSON: string);

      function add(const aValue: string): TElements; overload;
      function add(const aValue: Int32): TElements; overload;
      function add(const aValue: UInt32): TElements; overload;
      function add(const aValue: Int64): TElements; overload;
      function add(const aValue: UInt64): TElements; overload;
      function add(const aValue: Single): TElements; overload;
      function add(const aValue: Double): TElements; overload;
      function add(const aValue: Extended): TElements; overload;
      function add(const aValue: Boolean): TElements; overload;
      function add(const aValue: Char): TElements; overload;
      function add(const aValue: Byte): TElements; overload;
      function add(const aValue: TDateTime): TElements; overload;
      function add(const aValue: TGUID): TElements; overload;
      function add(const aValue: TBytes;
        aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic): TElements; overload;
      function add(const aValue: TJsonOid): TElements; overload;
      function add(const aValue: TJsonRegEx): TElements; overload;
      function add(const aValue: TJsonDBRef): TElements; overload;
      function add(const aValue: TJsonCodeWScope): TElements; overload;
      function add(const aValue: TJsonDecimal128): TElements; overload;
      function add(const aValue: TValue): TElements; overload;
      function add(const aValue: TVarRec): TElements; overload;
      function add(const aValue: Variant): TElements; overload;
      function addElements(const aElements: array of const): TElements; overload;
      function addElements(const aBuilder: TJSONCollectionBuilder): TElements; overload;
      function addElements(const aJSON: string): TElements; overload;
      function addNull: TElements; overload;
      function addUndefined: TElements; overload;
      function addMinKey: TElements; overload;
      function addMaxKey: TElements; overload;

      function add(const aKey: string; const aValue: string): TPairs; overload;
      function add(const aKey: string; const aValue: Int32): TPairs; overload;
      function add(const aKey: string; const aValue: UInt32): TPairs; overload;
      function add(const aKey: string; const aValue: Int64): TPairs; overload;
      function add(const aKey: string; const aValue: UInt64): TPairs; overload;
      function add(const aKey: string; const aValue: Single): TPairs; overload;
      function add(const aKey: string; const aValue: Double): TPairs; overload;
      function add(const aKey: string; const aValue: Extended): TPairs; overload;
      function add(const aKey: string; const aValue: Boolean): TPairs; overload;
      function add(const aKey: string; const aValue: Char): TPairs; overload;
      function add(const aKey: string; const aValue: Byte): TPairs; overload;
      function add(const aKey: string; const aValue: TDateTime): TPairs; overload;
      function add(const aKey: string; const aValue: TGUID): TPairs; overload;
      function add(const aKey: string; const aValue: TBytes;
        aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic): TPairs; overload;
      function add(const aKey: string; const aValue: TJsonOid): TPairs; overload;
      function add(const aKey: string; const aValue: TJsonRegEx): TPairs; overload;
      function add(const aKey: string; const aValue: TJsonDBRef): TPairs; overload;
      function add(const aKey: string; const aValue: TJsonCodeWScope): TPairs; overload;
      function add(const aKey: string; const aValue: TJsonDecimal128): TPairs; overload;
      function add(const aKey: string; const aValue: TValue): TPairs; overload;
      function add(const aKey: string; const aValue: TVarRec): TPairs; overload;
      function add(const aKey: string; const aValue: Variant): TPairs; overload;
      function addPairs(const aPairs: array of const): TPairs; overload;
      function addPairs(const aBuilder: TJSONCollectionBuilder): TPairs; overload;
      function addPairs(const aJSON: string): TPairs; overload;
      function addNull(const aKey: string): TPairs; overload;
      function addUndefined(const aKey: string): TPairs; overload;
      function addMinKey(const aKey: string): TPairs; overload;
      function addMaxKey(const aKey: string): TPairs; overload;

      function EndArray: TParentCollection;  
      function EndObject: TParentCollection;  

      function BeginObject: TPairs; overload;
      function BeginArray: TElements; overload;
      function BeginObject(const aKey: string): TPairs; overload;
      function BeginArray(const aKey: string): TElements; overload;

      property Owner: TJSONCollectionBuilder read FOwner;
      property Writer: TJSONWriter read FWriter;
      property RootDepth: Integer read FRootDepth;
    public
      constructor Create(const aOwner: TJSONCollectionBuilder; const aRootDepth: Integer);
      procedure EndAll;
      function Ended: Boolean;
    end;

    TElements = class(TBaseCollection)
    public
      function add(const aValue: string): TElements; overload; inline;
      function add(const aValue: Int32): TElements; overload; inline;
      function add(const aValue: UInt32): TElements; overload; inline;
      function add(const aValue: Int64): TElements; overload; inline;
      function add(const aValue: UInt64): TElements; overload; inline;
      function add(const aValue: Single): TElements; overload; inline;
      function add(const aValue: Double): TElements; overload; inline;
      function add(const aValue: Extended): TElements; overload; inline;
      function add(const aValue: Boolean): TElements; overload; inline;
      function add(const aValue: Char): TElements; overload; inline;
      function add(const aValue: Byte): TElements; overload; inline;
      function add(const aValue: TDateTime): TElements; overload; inline;
      function add(const aValue: TGUID): TElements; overload; inline;
      function add(const aValue: TBytes;
        aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic): TElements; overload; inline;
      function add(const aValue: TJsonOid): TElements; overload; inline;
      function add(const aValue: TJsonRegEx): TElements; overload; inline;
      function add(const aValue: TJsonDBRef): TElements; overload; inline;
      function add(const aValue: TJsonCodeWScope): TElements; overload; inline;
      function add(const aValue: TJsonDecimal128): TElements; overload; inline;
      function add(const aValue: TValue): TElements; overload; inline;
      function add(const aValue: TVarRec): TElements; overload; inline;
      function add(const aValue: Variant): TElements; overload; inline;
      function addNull: TElements; inline;
      function addUndefined: TElements; inline;
      function addMinKey: TElements; inline;
      function addMaxKey: TElements; inline;
      function addElements(const aElements: array of const): TElements; overload;
      function addElements(const aBuilder: TJSONCollectionBuilder): TElements; overload; inline;
      function addElements(const aJSON: string): TElements; overload; inline;

      function BeginObject: TPairs; overload; inline;
      function BeginArray: TElements; overload; inline;
      function EndArray: TParentCollection; inline; 

      function asRoot: TElements;
    end;

    TPairs = class(TBaseCollection)
    public
      function add(const aKey: string; const aValue: string): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Int32): TPairs; overload; inline;
      function add(const aKey: string; const aValue: UInt32): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Int64): TPairs; overload; inline;
      function add(const aKey: string; const aValue: UInt64): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Single): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Double): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Extended): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Boolean): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Char): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Byte): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TDateTime): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TGUID): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TBytes;
        aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonOid): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonRegEx): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonDBRef): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonCodeWScope): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonDecimal128): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TValue): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TVarRec): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Variant): TPairs; overload; inline;
      function addNull(const aKey: string): TPairs; inline;
      function addUndefined(const aKey: string): TPairs; inline;
      function addMinKey(const aKey: string): TPairs; inline;
      function addMaxKey(const aKey: string): TPairs; inline;
      function addPairs(const aPairs: array of const): TPairs; overload;
      function addPairs(const aBuilder: TJSONCollectionBuilder): TPairs; overload; inline;
      function addPairs(const aJSON: string): TPairs; overload; inline;

      function BeginObject(const aKey: string): TPairs; overload; inline;
      function BeginArray(const aKey: string): TElements; overload; inline;
      function EndObject: TParentCollection; inline; 

      function asRoot: TPairs;
    end;

    TParentCollection = class(TBaseCollection)
    public
      function add(const aValue: string): TElements; overload; inline;
      function add(const aValue: Int32): TElements; overload; inline;
      function add(const aValue: UInt32): TElements; overload; inline;
      function add(const aValue: Int64): TElements; overload; inline;
      function add(const aValue: UInt64): TElements; overload; inline;
      function add(const aValue: Single): TElements; overload; inline;
      function add(const aValue: Double): TElements; overload; inline;
      function add(const aValue: Extended): TElements; overload; inline;
      function add(const aValue: Boolean): TElements; overload; inline;
      function add(const aValue: Char): TElements; overload; inline;
      function add(const aValue: Byte): TElements; overload; inline;
      function add(const aValue: TDateTime): TElements; overload; inline;
      function add(const aValue: TGUID): TElements; overload; inline;
      function add(const aValue: TBytes;
        aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic): TElements; overload; inline;
      function add(const aValue: TJsonOid): TElements; overload; inline;
      function add(const aValue: TJsonRegEx): TElements; overload; inline;
      function add(const aValue: TJsonDBRef): TElements; overload;
      function add(const aValue: TJsonCodeWScope): TElements; overload;
      function add(const aValue: TJsonDecimal128): TElements; overload;
      function add(const aValue: TValue): TElements; overload; inline;
      function add(const aValue: TVarRec): TElements; overload; inline;
      function add(const aValue: Variant): TElements; overload; inline;
      function addNull: TElements; overload; inline;
      function addUndefined: TElements; overload; inline;
      function addMinKey: TElements; overload; inline;
      function addMaxKey: TElements; overload; inline;
      function addElements(const aElements: array of const): TElements; overload;
      function addElements(const aBuilder: TJSONCollectionBuilder): TElements; overload; inline;
      function addElements(const aJSON: string): TElements; overload; inline;

      function add(const aKey: string; const aValue: string): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Int32): TPairs; overload; inline;
      function add(const aKey: string; const aValue: UInt32): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Int64): TPairs; overload; inline;
      function add(const aKey: string; const aValue: UInt64): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Single): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Double): TPairs; overload; inline;
      function add(const aKey: string; aValue: Extended): TPairs; overload; inline;
      function add(const aKey: string; aValue: Boolean): TPairs; overload; inline;
      function add(const aKey: string; aValue: Char): TPairs; overload; inline;
      function add(const aKey: string; aValue: Byte): TPairs; overload; inline;
      function add(const aKey: string; aValue: TDateTime): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TGUID): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TBytes;
        aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonOid): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonRegEx): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonDBRef): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonCodeWScope): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TJsonDecimal128): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TValue): TPairs; overload; inline;
      function add(const aKey: string; const aValue: TVarRec): TPairs; overload; inline;
      function add(const aKey: string; const aValue: Variant): TPairs; overload; inline;
      function addNull(const aKey: string): TPairs; overload; inline;
      function addUndefined(const aKey: string): TPairs; overload; inline;
      function addMinKey(const aKey: string): TPairs; overload; inline;
      function addMaxKey(const aKey: string): TPairs; overload; inline;
      function addPairs(const aPairs: array of const): TPairs; overload;
      function addPairs(const aBuilder: TJSONCollectionBuilder): TPairs; overload; inline;
      function addPairs(const aJSON: string): TPairs; overload; inline;

      function BeginObject: TPairs; overload; inline;
      function BeginArray: TElements; overload; inline;
      function BeginObject(const aKey: string): TPairs; overload; inline;
      function BeginArray(const aKey: string): TElements; overload; inline;
      function EndArray: TParentCollection; inline;  
      function EndObject: TParentCollection; inline;  

      function asArray: TElements;
      function asObject: TPairs;
    end;

    TParentType = (None, Elements, Pairs);
    TGetReaderProc = reference to function (aWriter: TJsonWriter): TJsonReader;
    TReleaseReaderProc = reference to procedure (aWriter: TJsonWriter; aReader: TJsonReader);
    TResetWriterProc = reference to procedure (aWriter: TJsonWriter);

  strict private
    FEmpty: Boolean;
    FPairs: specialize TObjectDictionary<Integer, TPairs>;
    FElements: specialize TObjectDictionary<Integer, TElements>;
    FParentCollections: specialize TObjectDictionary<Integer, TParentCollection>;
    FParentTypes: specialize TStack<TParentType>;
    FJSONWriter: TJSONWriter;
    FGetReader: TGetReaderProc;
    FReleaseReader: TReleaseReaderProc;
    FResetWriter: TResetWriterProc;
    FDateTimeZoneHandling: TJsonDateTimeZoneHandling;
    FExtendedJsonMode: TJsonExtendedJsonMode;
  private
    procedure CheckParentType(aRootDepth: Integer; aParentType: TParentType);
    procedure CheckEmpty;
    function EndArray(aRootDepth: Integer): TParentCollection;
    function EndObject(aRootDepth: Integer): TParentCollection;
    procedure Complete(const aRootDepth: Integer);
    function GetPairs(aRootDepth: Integer): TPairs;
    function GetElements(aRootDepth: Integer): TElements;
    function GetParentCollection(aRootDepth: Integer): TParentCollection;
    function BeginObject(aRootDepth: Integer): TPairs; overload;
    function BeginArray(aRootDepth: Integer): TElements; overload;
    function PairsAsRoot: TPairs;
    function ElementsAsRoot: TElements;
    function asArray(aRootDepth: Integer): TElements;
    function asObject(aRootDepth: Integer): TPairs;
    function Ended(aRootDepth: Integer): Boolean;
    function GetAsJSON: string;
    procedure ClearContent;
    function GetParentType: TParentType;
    function GetParentArray: TElements;
    function GetParentObject: TPairs;
  protected
    procedure DoResetWriter(aWriter: TJsonWriter); virtual;
    function DoGetReader(aWriter: TJsonWriter): TJsonReader; virtual;
    procedure DoReleaseReader(aWriter: TJsonWriter; aReader: TJsonReader); virtual;
    procedure DoWriteCustomVariant(aWriter: TJsonWriter; const aValue: Variant); virtual;

    function DoBeginArray: TElements;
    function DoBeginObject: TPairs;
  public
    constructor Create(const aJSONWriter: TJSONWriter); overload;
    constructor Create(const aJSONWriter: TJSONWriter;
      aGetReader: TGetReaderProc; aReleaseReader: TReleaseReaderProc;
      aResetWriter: TResetWriterProc); overload;
    destructor Destroy; override;
    property asJSON: string read GetAsJSON;
    property ParentType: TParentType read GetParentType;
    property ParentArray: TElements read GetParentArray;
    property ParentObject: TPairs read GetParentObject;
    property ExtendedJsonMode: TJsonExtendedJsonMode read FExtendedJsonMode
      write FExtendedJsonMode default TJsonExtendedJsonMode.None;
    property DateTimeZoneHandling: TJsonDateTimeZoneHandling read FDateTimeZoneHandling
      write FDateTimeZoneHandling default TJsonDateTimeZoneHandling.Local;
  end;

  TJSONArrayBuilder = class(TJSONCollectionBuilder)
  public
    function BeginArray: TJSONCollectionBuilder.TElements;
    function Clear: TJSONArrayBuilder;
  end;

  TJSONObjectBuilder = class(TJSONCollectionBuilder)
  public
    function BeginObject: TJSONCollectionBuilder.TPairs;
    function Clear: TJSONObjectBuilder;
  end;

  TJSONIterator = class
  private type
    TContext = record
      FToken: TJsonToken;
      FIndex: Integer;
      constructor Create(aToken: TJsonToken);
    end;
  public type
    TRewindReaderProc = reference to procedure (aReader: TJsonReader);
    TIterateFunc = reference to function(aIter: TJSONIterator): Boolean;
  private
    FReader: TJsonReader;
    FStack: specialize TStack<TContext>;
    FKey: String;
    FPath: String;
    FType: TJsonToken;
    FStarting: Boolean;
    FFinished: Boolean;
    FRecursion: Boolean;
    FRewindReader: TRewindReaderProc;
    function GetAsBoolean: Boolean; inline;
    function GetAsString: String; inline;
    function GetAsInteger: Int32; inline;
    function GetAsInt64: Int64; inline;
    function GetAsDouble: Double; inline;
    function GetAsExtended: Extended; inline;
    function GetAsDateTime: TDateTime; inline;
    function GetAsGUID: TGUID; inline;
    function GetAsBytes: TBytes; inline;
    function GetAsOid: TJsonOid; inline;
    function GetAsRegEx: TJsonRegEx; inline;
    function GetAsDBRef: TJsonDBRef; inline;
    function GetAsCodeWScope: TJsonCodeWScope; inline;
    function GetAsDecimal: TJsonDecimal128; inline;
    function GetAsVariant: Variant; inline;
    function GetAsValue: TValue; inline;
    function GetIsNull: Boolean; inline;
    function GetIsUndefined: Boolean; inline;
    function GetIsMinKey: Boolean; inline;
    function GetIsMaxKey: Boolean; inline;
    function GetParentType: TJsonToken; inline;
    function GetIndex: Integer; inline;
    function GetInRecurse: Boolean; inline;
    function GetDepth: Integer; inline;
    function GetPath: String; inline;
  protected
    procedure DoRewindReader(aReader: TJsonReader); virtual;
  public
    constructor Create(aReader: TJsonReader); overload;
    constructor Create(aReader: TJsonReader; aRewindReader: TRewindReaderProc); overload;
    destructor Destroy; override;
    procedure Rewind;
    function Next(const aKey: String = ''): Boolean;
    function Recurse: Boolean;
    procedure Return;
    function Find(const aPath: String): Boolean;
    procedure Iterate(aFunc: TIterateFunc);
    function GetPath(aFromDepth: Integer): String; overload; inline;
    property Reader: TJSONReader read FReader;
    
    property Key: String read FKey;
    property Path: String read GetPath;
    property &Type: TJsonToken read FType;
    property ParentType: TJsonToken read GetParentType;
    property &Index: Integer read GetIndex;
    property InRecurse: Boolean read GetInRecurse;
    property Depth: Integer read GetDepth;
    
    property asString: String read GetAsString;
    property asInteger: Int32 read GetAsInteger;
    property asInt64: Int64 read GetAsInt64;
    property asDouble: Double read GetAsDouble;
    property asExtended: Extended read GetAsExtended;
    property asBoolean: Boolean read GetAsBoolean;
    property asDateTime: TDateTime read GetAsDateTime;
    property asGUID: TGUID read GetAsGUID;
    property asBytes: TBytes read GetAsBytes;
    property asOid: TJsonOid read GetAsOid;
    property asRegEx: TJsonRegEx read GetAsRegEx;
    property asDBRef: TJsonDBRef read GetAsDBRef;
    property asCodeWScope: TJsonCodeWScope read GetAsCodeWScope;
    property asDecimal: TJsonDecimal128 read GetAsDecimal;
    property asVariant: Variant read GetAsVariant;
    property asValue: TValue read GetAsValue;
    property IsNull: Boolean read GetIsNull;
    property IsUndefined: Boolean read GetIsUndefined;
    property IsMinKey: Boolean read GetIsMinKey;
    property IsMaxKey: Boolean read GetIsMaxKey;
  end;

  TJSONObjectBuilderPairs = TJSONObjectBuilder.TPairs;
  TJSONArrayBuilderElements = TJSONArrayBuilder.TElements;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo, System.Variants,
  {$ELSE}
  TypInfo, Variants,
  {$ENDIF}
  System.JSONConsts;

{ TJSONCollectionBuilder.TBaseCollection }

constructor TJSONCollectionBuilder.TBaseCollection.Create(
  const aOwner: TJSONCollectionBuilder; const aRootDepth: Integer);
begin
  FRootDepth := aRootDepth;
  FOwner := aOwner;
  FWriter := aOwner.FJSONWriter;
end;

procedure TJSONCollectionBuilder.TBaseCollection.AddingElement;
begin
  // Called when adding an element to an array
  // No special action needed for basic implementation
end;

procedure TJSONCollectionBuilder.TBaseCollection.AddingPair;
begin
  // Called when adding a pair to an object
  // No special action needed for basic implementation
end;

class procedure TJSONCollectionBuilder.TBaseCollection.ErrorInvalidSetOfItems;
begin
  raise EJSONCollectionBuilderError.Create('Invalid set of items');
end;

procedure TJSONCollectionBuilder.TBaseCollection.WriteVarRec(const aValue: TVarRec);

  procedure Error;
  begin
    raise EJSONCollectionBuilderError.CreateFmt('Unsupported VarRec type: %d', [AValue.VType]);
  end;

begin
  case aValue.VType of
    vtInteger:
      Writer.WriteValue(aValue.VInteger);
    vtBoolean:
      Writer.WriteValue(aValue.VBoolean);
    vtChar:
      Writer.WriteValue(string(aValue.VChar));
    vtWideChar:
      Writer.WriteValue(string(aValue.VWideChar));
    vtExtended:
      Writer.WriteValue(aValue.VExtended^);
    vtString:
      Writer.WriteValue(string(aValue.VString^));
    vtPChar:
      Writer.WriteValue(string(aValue.VPChar));
    vtPWideChar:
      Writer.WriteValue(string(aValue.VPWideChar));
    vtPointer:
      if aValue.VPointer = nil then
        Writer.WriteNull
      else
        Error;
    vtObject:
      if aValue.VObject = nil then
        Writer.WriteNull
      else
        Error;
    vtAnsiString:
      Writer.WriteValue(string(ansiString(aValue.VAnsiString)));
    vtCurrency:
      Writer.WriteValue(aValue.VCurrency^);
    vtVariant:
      WriteVariant(aValue.VVariant^);
    vtWideString:
      Writer.WriteValue(string(aValue.VWideString));
    vtInt64:
      Writer.WriteValue(aValue.VInt64^);
    vtUnicodeString:
      Writer.WriteValue(string(aValue.VUnicodeString));
  else
    Error;
  end;
end;

procedure TJSONCollectionBuilder.TBaseCollection.WriteVariant(const aValue: Variant);

  procedure Error;
  begin
    raise EJSONCollectionBuilderError.CreateFmt('Unsupported variant type: %d', [VarType(aValue)]);
  end;

  procedure addBytes;
  var
    pData: Pointer;
  begin
    pData := VarArrayLock(aValue);
    try
      Writer.WriteValue(TBytes(pData), TJsonBinaryType.Generic);
    finally
      VarArrayUnlock(aValue);
    end;
  end;

  procedure addArray;
  var
    I: Integer;
    LElems: TElements;
  begin
    LElems := BeginArray;
    for I := 0 to VarArrayHighBound(aValue, 1) do
      LElems.Add(aValue[I]);
    LElems.EndArray;
  end;

begin
  if VarIsArray(aValue) then
    if not ((VarArrayDimCount(aValue) = 1) and (VarArrayLowBound(aValue, 1) = 0)) then
      Error
    else if (VarType(aValue) and VarTypeMask) = varByte then
      addBytes
    else
      addArray
  else
    case VarType(aValue) and varTypeMask of
      varEmpty:
        Writer.WriteUndefined;
      varNull:
        Writer.WriteNull;
      varSmallint,
      varShortInt,
      varInteger:
        Writer.WriteValue(Integer(aValue));
      varByte,
      varWord,
      varUInt32:
        Writer.WriteValue(Cardinal(aValue));
      varInt64:
        Writer.WriteValue(Int64(aValue));
      varUInt64:
        Writer.WriteValue(UInt64(aValue));
      varSingle,
      varDouble:
        Writer.WriteValue(Double(aValue));
      varCurrency:
        Writer.WriteValue(Extended(aValue));
      varDate:
        Writer.WriteValue(TDateTime(aValue));
      varOleStr,
      varStrArg,
      varUStrArg,
      varString,
      varUString:
        Writer.WriteValue(string(aValue));
      varBoolean:
        Writer.WriteValue(Boolean(aValue));
      else
        Owner.DoWriteCustomVariant(Writer, aValue);
    end;
end;

procedure TJSONCollectionBuilder.TBaseCollection.WriteOpenArray(const aItems: array of const);
var
  iItem: Integer;
  sKey: string;
  LPriorLevel: Integer;
  LPriorType: TParentType;

  procedure Error;
  begin
    raise EJSONCollectionBuilderError.CreateFmt('Invalid open array item at index %d', [iItem]);
  end;

  function GetStr(aIndex: Integer; out aValue: string): Boolean;
  var
    pRec: PVarRec;
  begin
    pRec := @TVarRec(aItems[AIndex]);
    Result := True;
    case pRec^.VType of
      vtChar:
        aValue := string(pRec^.VChar);
      vtWideChar:
        aValue := pRec^.VWideChar;
      vtString:
        aValue := string(pRec^.VString^);
      vtAnsiString:
        aValue := string(ansiString(pRec^.VAnsiString));
      vtWideString:
        aValue := string(pRec^.VWideString);
      vtPChar:
        aValue := string(pRec^.VPChar);
      vtPWideChar:
        aValue := string(pRec^.VPWideChar);
      vtUnicodeString:
        aValue := string(pRec^.VUnicodeString);
    else
      Result := False;
    end;
  end;

begin
  LPriorType := Owner.GetParentType;
  LPriorLevel := Owner.FParentTypes.Count;
  iItem := 0;

  while iItem < Length(aItems) do
  begin
    if (LPriorType = TParentType.Pairs) and ((iItem and 1) = 0) then
    begin
      if not GetStr(iItem, sKey) then
        Error;
      Inc(iItem);
      if iItem >= Length(aItems) then
        Error;
      Writer.WritePropertyName(sKey);
      WriteVarRec(aItems[iItem]);
    end
    else if (LPriorType = TParentType.Elements) then
    begin
      WriteVarRec(aItems[iItem]);
    end
    else
      Error;

    Inc(iItem);
  end;
end;

function TJSONCollectionBuilder.TBaseCollection.WriteReader(const aReader: TJsonReader; aOnlyEnclosed: Boolean): Boolean;
begin
  try
    Writer.WriteToken(aReader, aOnlyEnclosed);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TJSONCollectionBuilder.TBaseCollection.WriteBuilder(const aBuilder: TJSONCollectionBuilder);
var
  LReader: TJsonReader;
begin
  LReader := Owner.DoGetReader(aBuilder.FJSONWriter);
  try
    Writer.WriteToken(LReader, True);
  finally
    Owner.DoReleaseReader(aBuilder.FJSONWriter, LReader);
  end;
end;

procedure TJSONCollectionBuilder.TBaseCollection.WriteJSON(const aJSON: string);
begin
  // Simple implementation: just write the JSON string as-is
  // TODO: Parse and validate JSON properly
  Writer.WriteRawValue(aJSON);
end;

procedure TJSONCollectionBuilder.TBaseCollection.EndAll;
begin
  Owner.Complete(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Ended: Boolean;
begin
  Result := Owner.Ended(FRootDepth);
end;

{ TJSONCollectionBuilder }

constructor TJSONCollectionBuilder.Create(const aJSONWriter: TJSONWriter);
begin
  inherited Create;
  FEmpty := True;
  FJSONWriter := aJSONWriter;
  FParentTypes := specialize TStack<TParentType>.Create;
  FExtendedJsonMode := TJsonExtendedJsonMode.None;
  FDateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
end;

constructor TJSONCollectionBuilder.Create(const aJSONWriter: TJSONWriter;
  aGetReader: TGetReaderProc; aReleaseReader: TReleaseReaderProc;
  aResetWriter: TResetWriterProc);
begin
  Create(aJSONWriter);
  FGetReader := aGetReader;
  FReleaseReader := aReleaseReader;
  FResetWriter := aResetWriter;
end;

destructor TJSONCollectionBuilder.Destroy;
begin
  FElements.Free;
  FPairs.Free;
  FParentTypes.Free;
  FParentCollections.Free;
  inherited Destroy;
end;

procedure TJSONCollectionBuilder.CheckEmpty;
begin
  if not FEmpty then
    raise EJSONCollectionBuilderError.Create('Builder is not empty');
end;

procedure TJSONCollectionBuilder.CheckParentType(aRootDepth: Integer; aParentType: TParentType);
begin
  if (FParentTypes.Count = 0) or Ended(aRootDepth) or (FParentTypes.Peek <> aParentType) then
    raise EJSONCollectionBuilderError.Create('Invalid parent type');
end;

function TJSONCollectionBuilder.Ended(aRootDepth: Integer): Boolean;
begin
  Result := aRootDepth = MaxInt;
end;

procedure TJSONCollectionBuilder.Complete(const aRootDepth: Integer);
var
  LType: TParentType;
begin
  while FParentTypes.Count > aRootDepth do
  begin
    LType := FParentTypes.Pop;
    case LType of
      TParentType.Elements:
        FJSONWriter.WriteEndArray;
      TParentType.Pairs:
        FJSONWriter.WriteEndObject;
    end;
  end;
end;

function TJSONCollectionBuilder.GetElements(aRootDepth: Integer): TElements;
begin
  if FElements = nil then
    FElements := specialize TObjectDictionary<Integer, TElements>.Create([doOwnsValues]);
  if not FElements.TryGetValue(aRootDepth, Result) then
  begin
    Result := TElements.Create(Self, aRootDepth);
    FElements.Add(aRootDepth, Result);
  end;
end;

function TJSONCollectionBuilder.GetPairs(aRootDepth: Integer): TPairs;
begin
  if FPairs = nil then
    FPairs := specialize TObjectDictionary<Integer, TPairs>.Create([doOwnsValues]);
  if not FPairs.TryGetValue(aRootDepth, Result) then
  begin
    Result := TPairs.Create(Self, aRootDepth);
    FPairs.Add(aRootDepth, Result);
  end;
end;

function TJSONCollectionBuilder.GetParentCollection(aRootDepth: Integer): TParentCollection;
begin
  if FParentCollections = nil then
    FParentCollections := specialize TObjectDictionary<Integer, TParentCollection>.Create([doOwnsValues]);
  if not FParentCollections.TryGetValue(aRootDepth, Result) then
  begin
    Result := TParentCollection.Create(Self, aRootDepth);
    FParentCollections.Add(aRootDepth, Result);
  end;
end;

function TJSONCollectionBuilder.GetAsJSON: string;
begin
  FJSONWriter.Flush;
  // Simple implementation: return empty string for now
  // TODO: Implement proper JSON serialization
  Result := '';
end;

function TJSONCollectionBuilder.GetParentType: TParentType;
begin
  if FParentTypes.Count > 0 then
    Result := FParentTypes.Peek
  else
    Result := TParentType.None;
end;

function TJSONCollectionBuilder.GetParentArray: TElements;
begin
  if (FParentTypes.Count > 0) and (FParentTypes.Peek = TParentType.Elements) then
    Result := GetElements(0)
  else
    raise EJSONCollectionBuilderError.Create('Not in array context');
end;

function TJSONCollectionBuilder.GetParentObject: TPairs;
begin
  if (FParentTypes.Count > 0) and (FParentTypes.Peek = TParentType.Pairs) then
    Result := GetPairs(0)
  else
    raise EJSONCollectionBuilderError.Create('Not in object context');
end;

procedure TJSONCollectionBuilder.DoResetWriter(aWriter: TJsonWriter);
begin
  if not assigned(FResetWriter) then
    raise EJSONCollectionBuilderError.Create('No reset writer callback defined');
  FResetWriter(aWriter);
end;

function TJSONCollectionBuilder.DoGetReader(aWriter: TJsonWriter): TJsonReader;
begin
  if not assigned(FGetReader) then
    raise EJSONCollectionBuilderError.Create('No get reader callback defined');
  Result := FGetReader(aWriter);
end;

procedure TJSONCollectionBuilder.DoReleaseReader(aWriter: TJsonWriter; aReader: TJsonReader);
begin
  if not assigned(FReleaseReader) then
    raise EJSONCollectionBuilderError.Create('No release reader callback defined');
  FReleaseReader(aWriter, aReader);
end;

procedure TJSONCollectionBuilder.DoWriteCustomVariant(aWriter: TJsonWriter; const aValue: Variant);
var
  S: string;
begin
  try
    S := aValue;
  except
    raise EJSONCollectionBuilderError.CreateFmt('Unsupported variant type: %d', [VarType(aValue)]);
  end;
  aWriter.WriteValue(S);
end;

function TJSONCollectionBuilder.DoBeginArray: TElements;
begin
  CheckEmpty;
  FEmpty := False;
  Result := BeginArray(0);
end;

function TJSONCollectionBuilder.DoBeginObject: TPairs;
begin
  CheckEmpty;
  FEmpty := False;
  Result := BeginObject(0);
end;

function TJSONCollectionBuilder.BeginArray(aRootDepth: Integer): TElements;
begin
  FParentTypes.Push(TParentType.Elements);
  FJSONWriter.WriteStartArray;
  Result := GetElements(aRootDepth);
end;

function TJSONCollectionBuilder.BeginObject(aRootDepth: Integer): TPairs;
begin
  FParentTypes.Push(TParentType.Pairs);
  FJSONWriter.WriteStartObject;
  Result := GetPairs(aRootDepth);
end;

function TJSONCollectionBuilder.EndArray(aRootDepth: Integer): TParentCollection;
begin
  if FParentTypes.Count > aRootDepth then
  begin
    case FParentTypes.Peek of
      TParentType.Elements:
      begin
        FParentTypes.Pop;
        FJSONWriter.WriteEndArray;
        if FParentTypes.Count > aRootDepth then
          Result := GetParentCollection(aRootDepth)
        else
          Result := GetParentCollection(MaxInt);
      end;
    else
      raise EJSONCollectionBuilderError.Create('Not in array context');
    end;
  end
  else
    Result := GetParentCollection(MaxInt);
end;

function TJSONCollectionBuilder.EndObject(aRootDepth: Integer): TParentCollection;
begin
  if FParentTypes.Count > aRootDepth then
  begin
    case FParentTypes.Peek of
      TParentType.Pairs:
      begin
        FParentTypes.Pop;
        FJSONWriter.WriteEndObject;
        if FParentTypes.Count > aRootDepth then
          Result := GetParentCollection(aRootDepth)
        else
          Result := GetParentCollection(MaxInt);
      end;
    else
      raise EJSONCollectionBuilderError.Create('Not in object context');
    end;
  end
  else
    Result := GetParentCollection(MaxInt);
end;

function TJSONCollectionBuilder.PairsAsRoot: TPairs;
begin
  Result := GetPairs(FParentTypes.Count);
end;

function TJSONCollectionBuilder.ElementsAsRoot: TElements;
begin
  Result := GetElements(FParentTypes.Count);
end;

function TJSONCollectionBuilder.AsArray(aRootDepth: Integer): TElements;
begin
  Result := nil;
  if FParentTypes.Count > 0 then
    if FParentTypes.Peek = TParentType.Elements then
      Result := GetElements(aRootDepth);
  if Result = nil then
    raise EJSONCollectionBuilderError.Create('Not in array context');
end;

function TJSONCollectionBuilder.AsObject(aRootDepth: Integer): TPairs;
begin
  Result := nil;
  if FParentTypes.Count > 0 then
    if FParentTypes.Peek = TParentType.Pairs then
      Result := GetPairs(aRootDepth);
  if Result = nil then
    raise EJSONCollectionBuilderError.Create('Not in object context');
end;

procedure TJSONCollectionBuilder.ClearContent;
begin
  FreeAndNil(FElements);
  FreeAndNil(FPairs);
  FreeAndNil(FParentCollections);
  FParentTypes.Clear;
  FEmpty := True;
  FJSONWriter.Rewind;
  DoResetWriter(FJSONWriter);
end;

{ TJSONCollectionBuilder.TBaseCollection add methods }

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: string): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Int32): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: UInt32): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Int64): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: UInt64): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Single): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Double): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Extended): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Boolean): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Char): TElements;
begin
  FWriter.WriteValue(string(aValue));
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Byte): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TDateTime): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TGUID): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TBytes; aBinaryType: TJsonBinaryType): TElements;
begin
  FWriter.WriteValue(aValue, aBinaryType);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TJsonOid): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TJsonRegEx): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TJsonDBRef): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TJsonCodeWScope): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TJsonDecimal128): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TValue): TElements;
begin
  FWriter.WriteValue(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: TVarRec): TElements;
begin
  WriteVarRec(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aValue: Variant): TElements;
begin
  WriteVariant(aValue);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddElements(const aElements: array of const): TElements;
begin
  WriteOpenArray(aElements);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddElements(const aBuilder: TJSONCollectionBuilder): TElements;
begin
  WriteBuilder(aBuilder);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddElements(const aJSON: string): TElements;
begin
  WriteJSON(aJSON);
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddNull: TElements;
begin
  FWriter.WriteNull;
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddUndefined: TElements;
begin
  FWriter.WriteUndefined;
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddMinKey: TElements;
begin
  FWriter.WriteMinKey;
  Result := Owner.GetElements(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddMaxKey: TElements;
begin
  FWriter.WriteMaxKey;
  Result := Owner.GetElements(FRootDepth);
end;

// Key-value pair methods
function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: string): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Int32): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: UInt32): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Int64): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: UInt64): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Single): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Double): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Extended): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Boolean): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Char): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(string(aValue));
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Byte): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TDateTime): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TGUID): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TBytes; aBinaryType: TJsonBinaryType): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue, aBinaryType);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TJsonOid): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TJsonRegEx): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TJsonDBRef): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TJsonCodeWScope): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TJsonDecimal128): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TValue): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteValue(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: TVarRec): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  WriteVarRec(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.Add(const aKey: string; const aValue: Variant): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  WriteVariant(aValue);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddPairs(const aPairs: array of const): TPairs;
begin
  WriteOpenArray(aPairs);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddPairs(const aBuilder: TJSONCollectionBuilder): TPairs;
begin
  WriteBuilder(aBuilder);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddPairs(const aJSON: string): TPairs;
begin
  WriteJSON(aJSON);
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddNull(const aKey: string): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteNull;
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddUndefined(const aKey: string): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteUndefined;
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddMinKey(const aKey: string): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteMinKey;
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.AddMaxKey(const aKey: string): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  FWriter.WriteMaxKey;
  Result := Owner.GetPairs(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.BeginObject: TPairs;
begin
  Result := Owner.BeginObject(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.BeginArray: TElements;
begin
  Result := Owner.BeginArray(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.BeginObject(const aKey: string): TPairs;
begin
  FWriter.WritePropertyName(aKey);
  Result := Owner.BeginObject(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.BeginArray(const aKey: string): TElements;
begin
  FWriter.WritePropertyName(aKey);
  Result := Owner.BeginArray(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.EndArray: TParentCollection;
begin
  Result := Owner.EndArray(FRootDepth);
end;

function TJSONCollectionBuilder.TBaseCollection.EndObject: TParentCollection;
begin
  Result := Owner.EndObject(FRootDepth);
end;

{ TJSONArrayBuilder }

function TJSONArrayBuilder.BeginArray: TJSONCollectionBuilder.TElements;
begin
  Result := DoBeginArray;
end;

function TJSONArrayBuilder.Clear: TJSONArrayBuilder;
begin
  ClearContent;
  Result := Self;
end;

{ TJSONObjectBuilder }

function TJSONObjectBuilder.BeginObject: TJSONCollectionBuilder.TPairs;
begin
  Result := DoBeginObject;
end;

function TJSONObjectBuilder.Clear: TJSONObjectBuilder;
begin
  ClearContent;
  Result := Self;
end;

{ TJSONIterator.TContext }

constructor TJSONIterator.TContext.Create(aToken: TJsonToken);
begin
  FToken := aToken;
  FIndex := 0;
end;

{ TJSONIterator }

constructor TJSONIterator.Create(aReader: TJsonReader);
begin
  inherited Create;
  FReader := aReader;
  FStack := specialize TStack<TContext>.Create;
  FType := TJsonToken.None;
  FStarting := True;
  FFinished := False;
  FRecursion := False;
end;

constructor TJSONIterator.Create(aReader: TJsonReader; aRewindReader: TRewindReaderProc);
begin
  Create(aReader);
  FRewindReader := aRewindReader;
end;

destructor TJSONIterator.Destroy;
begin
  FStack.Free;
  inherited Destroy;
end;

procedure TJSONIterator.DoRewindReader(aReader: TJsonReader);
begin
  if assigned(FRewindReader) then
    FRewindReader(aReader);
end;

procedure TJSONIterator.Rewind;
begin
  DoRewindReader(FReader);
  FStack.Clear;
  FKey := '';
  FPath := '';
  FType := TJsonToken.None;
  FStarting := True;
  FFinished := False;
  FRecursion := False;
end;

function TJSONIterator.Next(const aKey: String): Boolean;
var
  Context: TContext;
begin
  Result := False;
  FKey := '';

  if FFinished then
    Exit;

  while FReader.Read do
  begin
    FType := FReader.TokenType;

    case FType of
      TJsonToken.StartObject, TJsonToken.StartArray:
      begin
        Context := TContext.Create(FType);
        FStack.Push(Context);
        Result := True;
        Break;
      end;
      TJsonToken.EndObject, TJsonToken.EndArray:
      begin
        if FStack.Count > 0 then
          FStack.Pop;
      end;
      TJsonToken.PropertyName:
      begin
        FKey := FReader.Value.AsString;
        if FReader.Read then
        begin
          FType := FReader.TokenType;
          // Push to stack if the property value is a nested object or array
          if FType in [TJsonToken.StartObject, TJsonToken.StartArray] then
          begin
            Context := TContext.Create(FType);
            FStack.Push(Context);
          end;
          Result := True;
          Break;
        end;
      end;
    else
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  if not Result then
    FFinished := True;
end;

function TJSONIterator.Recurse: Boolean;
begin
  Result := (FType = TJsonToken.StartObject) or (FType = TJsonToken.StartArray);
  if Result then
  begin
    FRecursion := True;
  end;
end;

procedure TJSONIterator.Return;
var
  lDepth: Integer;
  Context: TContext;
begin
  if FStack.Count = 0 then
    Exit;

  lDepth := FStack.Count;

  while FReader.Read and (FStack.Count >= lDepth) do
  begin
    case FReader.TokenType of
      TJsonToken.StartObject, TJsonToken.StartArray:
      begin
        Context := TContext.Create(FReader.TokenType);
        FStack.Push(Context);
      end;
      TJsonToken.EndObject, TJsonToken.EndArray:
      begin
        if FStack.Count > 0 then
          FStack.Pop;
      end;
    end;
  end;
end;

function TJSONIterator.Find(const aPath: String): Boolean;
var
  PathParts: TStringDynArray;
  PartIndex: Integer;
  CurrentPart: String;
begin
  Result := False;
  PathParts := aPath.Split(['.']);
  PartIndex := 0;

  Rewind;
  while Next do
  begin
    if PartIndex < Length(PathParts) then
    begin
      CurrentPart := PathParts[PartIndex];
      if SameText(FKey, CurrentPart) then
      begin
        Inc(PartIndex);
        if PartIndex >= Length(PathParts) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TJSONIterator.Iterate(aFunc: TIterateFunc);
begin
  Rewind;
  while Next do
  begin
    if not aFunc(Self) then
      Break;
  end;
end;

function TJSONIterator.GetPath(aFromDepth: Integer): String;
var
  PathBuilder: TStringBuilder;
  I: Integer;
  Context: TContext;
  StackArray: Array of TContext;
begin
  PathBuilder := TStringBuilder.Create;
  try
    StackArray := FStack.ToArray;
    for I := High(StackArray) downto Low(StackArray) do
    begin
      if I <= aFromDepth then
        Break;
      Context := StackArray[I];
      if PathBuilder.Length > 0 then
        PathBuilder.Append('.');
      PathBuilder.Append(IntToStr(Context.FIndex));
    end;

    if (FKey <> '') and (FStack.Count > aFromDepth) then
    begin
      if PathBuilder.Length > 0 then
        PathBuilder.Append('.');
      PathBuilder.Append(FKey);
    end;

    Result := PathBuilder.ToString;
  finally
    PathBuilder.Free;
  end;
end;

function TJSONIterator.GetAsBoolean: Boolean;
begin
  Result := FReader.Value.AsBoolean;
end;

function TJSONIterator.GetAsString: String;
begin
  Result := FReader.Value.AsString;
end;

function TJSONIterator.GetAsInteger: Int32;
begin
  Result := FReader.Value.AsInteger;
end;

function TJSONIterator.GetAsInt64: Int64;
begin
  Result := FReader.Value.AsInt64;
end;

function TJSONIterator.GetAsDouble: Double;
begin
  Result := FReader.Value.AsDouble;
end;

function TJSONIterator.GetAsExtended: Extended;
begin
  Result := FReader.Value.AsExtended;
end;

function TJSONIterator.GetAsDateTime: TDateTime;
begin
  Result := FReader.Value.AsDateTime;
end;

function TJSONIterator.GetAsGUID: TGUID;
begin
  // TODO: FReader.Value.AsGUID not available in FPC implementation
  // Result := FReader.Value.AsGUID;
  FillChar(Result, SizeOf(Result), 0);
end;

function TJSONIterator.GetAsBytes: TBytes;
begin
  // TODO: FReader.Value.AsBytes not available in FPC implementation
  // Result := FReader.Value.AsBytes;
  Result := nil;
end;

function TJSONIterator.GetAsOid: TJsonOid;
begin
  // TODO: FReader.Value.AsOid not available in FPC implementation
  // Result := FReader.Value.AsOid;
  FillChar(Result, SizeOf(Result), 0);
end;

function TJSONIterator.GetAsRegEx: TJsonRegEx;
begin
  // TODO: FReader.Value.AsRegEx not available in FPC implementation
  // Result := FReader.Value.AsRegEx;
  FillChar(Result, SizeOf(Result), 0);
end;

function TJSONIterator.GetAsDBRef: TJsonDBRef;
begin
  // TODO: FReader.Value.AsDBRef not available in FPC implementation
  // Result := FReader.Value.AsDBRef;
  FillChar(Result, SizeOf(Result), 0);
end;

function TJSONIterator.GetAsCodeWScope: TJsonCodeWScope;
begin
  // TODO: FReader.Value.AsCodeWScope not available in FPC implementation
  // Result := FReader.Value.AsCodeWScope;
  FillChar(Result, SizeOf(Result), 0);
end;

function TJSONIterator.GetAsDecimal: TJsonDecimal128;
begin
  // TODO: FReader.Value.AsDecimal128 not available in FPC implementation
  // Result := FReader.Value.AsDecimal128;
  FillChar(Result, SizeOf(Result), 0);
end;

function TJSONIterator.GetAsVariant: Variant;
begin
  // Handle TValue to Variant conversion based on token type
  case FType of
    TJsonToken.&String:
      Result := FReader.Value.AsString;
    TJsonToken.Integer:
      Result := FReader.Value.AsInt64;
    TJsonToken.Float:
      Result := FReader.Value.AsExtended;
    TJsonToken.Boolean:
      Result := FReader.Value.AsBoolean;
    TJsonToken.Null:
      Result := Null;
  else
    // Try direct conversion, may fail for complex types
    try
      Result := FReader.Value.AsVariant;
    except
      Result := Null;
    end;
  end;
end;

function TJSONIterator.GetAsValue: TValue;
begin
  // TODO: FReader.Value.AsValue not available in FPC implementation
  // Result := FReader.Value.AsValue;
  Result := TValue.Empty;
end;

function TJSONIterator.GetIsNull: Boolean;
begin
  Result := FType = TJsonToken.Null;
end;

function TJSONIterator.GetIsUndefined: Boolean;
begin
  Result := FType = TJsonToken.Undefined;
end;

function TJSONIterator.GetIsMinKey: Boolean;
begin
  Result := FType = TJsonToken.MinKey;
end;

function TJSONIterator.GetIsMaxKey: Boolean;
begin
  Result := FType = TJsonToken.MaxKey;
end;

function TJSONIterator.GetParentType: TJsonToken;
var
  Context: TContext;
begin
  if FStack.Count > 0 then
  begin
    Context := FStack.Peek;
    Result := Context.FToken;
  end
  else
    Result := TJsonToken.None;
end;

function TJSONIterator.GetIndex: Integer;
var
  Context: TContext;
begin
  if FStack.Count > 0 then
  begin
    Context := FStack.Peek;
    Result := Context.FIndex;
  end
  else
    Result := -1;
end;

function TJSONIterator.GetInRecurse: Boolean;
begin
  Result := FRecursion;
end;

function TJSONIterator.GetDepth: Integer;
begin
  Result := FStack.Count;
end;

function TJSONIterator.GetPath: String;
begin
  Result := GetPath(0);
end;

{ TJSONCollectionBuilder.TParentCollection }

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: string): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Int32): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: UInt32): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Int64): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: UInt64): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Single): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Double): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Extended): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Boolean): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Char): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Byte): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TDateTime): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TGUID): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TBytes; aBinaryType: TJsonBinaryType): TElements;
begin
  Result := inherited add(aValue, aBinaryType);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TJsonOid): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TJsonRegEx): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TJsonDBRef): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TJsonCodeWScope): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TJsonDecimal128): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TValue): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: TVarRec): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aValue: Variant): TElements;
begin
  Result := inherited add(aValue);
end;

function TJSONCollectionBuilder.TParentCollection.AddNull: TElements;
begin
  Result := inherited addNull;
end;

function TJSONCollectionBuilder.TParentCollection.AddUndefined: TElements;
begin
  Result := inherited addUndefined;
end;

function TJSONCollectionBuilder.TParentCollection.AddMinKey: TElements;
begin
  Result := inherited addMinKey;
end;

function TJSONCollectionBuilder.TParentCollection.AddMaxKey: TElements;
begin
  Result := inherited addMaxKey;
end;

function TJSONCollectionBuilder.TParentCollection.AddElements(const aElements: array of const): TElements;
begin
  Result := inherited addElements(aElements);
end;

function TJSONCollectionBuilder.TParentCollection.AddElements(const aBuilder: TJSONCollectionBuilder): TElements;
begin
  Result := inherited addElements(aBuilder);
end;

function TJSONCollectionBuilder.TParentCollection.AddElements(const aJSON: string): TElements;
begin
  Result := inherited addElements(aJSON);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: string): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: Int32): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: UInt32): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: Int64): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: UInt64): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: Single): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: Double): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; aValue: Extended): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; aValue: Boolean): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; aValue: Char): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; aValue: Byte): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; aValue: TDateTime): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TGUID): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TBytes; aBinaryType: TJsonBinaryType): TPairs;
begin
  Result := inherited add(aKey, aValue, aBinaryType);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TJsonOid): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TJsonRegEx): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TJsonDBRef): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TJsonCodeWScope): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TJsonDecimal128): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TValue): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: TVarRec): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.Add(const aKey: string; const aValue: Variant): TPairs;
begin
  Result := inherited add(aKey, aValue);
end;

function TJSONCollectionBuilder.TParentCollection.AddNull(const aKey: string): TPairs;
begin
  Result := inherited addNull(aKey);
end;

function TJSONCollectionBuilder.TParentCollection.AddUndefined(const aKey: string): TPairs;
begin
  Result := inherited addUndefined(aKey);
end;

function TJSONCollectionBuilder.TParentCollection.AddMinKey(const aKey: string): TPairs;
begin
  Result := inherited addMinKey(aKey);
end;

function TJSONCollectionBuilder.TParentCollection.AddMaxKey(const aKey: string): TPairs;
begin
  Result := inherited addMaxKey(aKey);
end;

function TJSONCollectionBuilder.TParentCollection.AddPairs(const aPairs: array of const): TPairs;
begin
  Result := inherited addPairs(aPairs);
end;

function TJSONCollectionBuilder.TParentCollection.AddPairs(const aBuilder: TJSONCollectionBuilder): TPairs;
begin
  Result := inherited addPairs(aBuilder);
end;

function TJSONCollectionBuilder.TParentCollection.AddPairs(const aJSON: string): TPairs;
begin
  Result := inherited addPairs(aJSON);
end;

function TJSONCollectionBuilder.TParentCollection.BeginObject: TPairs;
begin
  Result := inherited BeginObject;
end;

function TJSONCollectionBuilder.TParentCollection.BeginArray: TElements;
begin
  Result := inherited BeginArray;
end;

function TJSONCollectionBuilder.TParentCollection.BeginObject(const aKey: string): TPairs;
begin
  Result := inherited BeginObject(aKey);
end;

function TJSONCollectionBuilder.TParentCollection.BeginArray(const aKey: string): TElements;
begin
  Result := inherited BeginArray(aKey);
end;

function TJSONCollectionBuilder.TParentCollection.EndArray: TParentCollection;
begin
  Result := inherited EndArray;
end;

function TJSONCollectionBuilder.TParentCollection.EndObject: TParentCollection;
begin
  Result := inherited EndObject;
end;

function TJSONCollectionBuilder.TParentCollection.AsArray: TElements;
begin
  Result := Owner.AsArray(RootDepth);
end;

function TJSONCollectionBuilder.TParentCollection.AsObject: TPairs;
begin
  Result := Owner.AsObject(RootDepth);
end;

{ TJSONCollectionBuilder.TElements }

function TJSONCollectionBuilder.TElements.Add(const aValue: string): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Int32): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: UInt32): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Int64): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: UInt64): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Single): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Double): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Extended): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Boolean): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Char): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Byte): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TDateTime): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TGUID): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TBytes; aBinaryType: TJsonBinaryType): TElements;
begin
  inherited add(aValue, aBinaryType);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TJsonOid): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TJsonRegEx): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TJsonDBRef): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TJsonCodeWScope): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TJsonDecimal128): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TValue): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: TVarRec): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.Add(const aValue: Variant): TElements;
begin
  inherited add(aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddNull: TElements;
begin
  inherited addNull;
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddUndefined: TElements;
begin
  inherited addUndefined;
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddMinKey: TElements;
begin
  inherited addMinKey;
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddMaxKey: TElements;
begin
  inherited addMaxKey;
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddElements(const aElements: array of const): TElements;
begin
  inherited addElements(aElements);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddElements(const aBuilder: TJSONCollectionBuilder): TElements;
begin
  inherited addElements(aBuilder);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.AddElements(const aJSON: string): TElements;
begin
  inherited addElements(aJSON);
  Result := Self;
end;

function TJSONCollectionBuilder.TElements.BeginObject: TPairs;
begin
  Result := inherited BeginObject;
end;

function TJSONCollectionBuilder.TElements.BeginArray: TElements;
begin
  Result := inherited BeginArray;
end;

function TJSONCollectionBuilder.TElements.EndArray: TParentCollection;
begin
  Result := inherited EndArray;
end;

function TJSONCollectionBuilder.TElements.AsRoot: TElements;
begin
  Result := Self; // TODO: Implement properly
end;

{ TJSONCollectionBuilder.TPairs }

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: string): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Int32): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: UInt32): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Int64): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: UInt64): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Single): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Double): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Extended): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Boolean): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Char): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Byte): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TDateTime): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TGUID): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TBytes; aBinaryType: TJsonBinaryType): TPairs;
begin
  inherited add(aKey, aValue, aBinaryType);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TJsonOid): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TJsonRegEx): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TJsonDBRef): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TJsonCodeWScope): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TJsonDecimal128): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TValue): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: TVarRec): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.Add(const aKey: string; const aValue: Variant): TPairs;
begin
  inherited add(aKey, aValue);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddNull(const aKey: string): TPairs;
begin
  inherited addNull(aKey);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddUndefined(const aKey: string): TPairs;
begin
  inherited addUndefined(aKey);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddMinKey(const aKey: string): TPairs;
begin
  inherited addMinKey(aKey);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddMaxKey(const aKey: string): TPairs;
begin
  inherited addMaxKey(aKey);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddPairs(const aPairs: array of const): TPairs;
begin
  inherited addPairs(aPairs);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddPairs(const aBuilder: TJSONCollectionBuilder): TPairs;
begin
  inherited addPairs(aBuilder);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.AddPairs(const aJSON: string): TPairs;
begin
  inherited addPairs(aJSON);
  Result := Self;
end;

function TJSONCollectionBuilder.TPairs.BeginObject(const aKey: string): TPairs;
begin
  Result := inherited BeginObject(aKey);
end;

function TJSONCollectionBuilder.TPairs.BeginArray(const aKey: string): TElements;
begin
  Result := inherited BeginArray(aKey);
end;

function TJSONCollectionBuilder.TPairs.EndObject: TParentCollection;
begin
  Result := inherited EndObject;
end;

function TJSONCollectionBuilder.TPairs.AsRoot: TPairs;
begin
  Result := Self; // TODO: Implement properly
end;

end.

