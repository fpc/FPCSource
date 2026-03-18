{
    This file is part of the Free Component Library
    Copyright (c) 2026 by Michael Van Canneyt michael@freepascal.org

    Delphi-compatible JSON serializers unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System.JSON.Serializers;

{$mode objfpc}
{$h+}
{$SCOPEDENUMS ON}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.TypInfo, System.Rtti, System.Classes, System.Generics.Collections, Fcl.Streams.Extra,
  {$ELSE}
  SysUtils, TypInfo, Rtti, Classes, Generics.Collections, StreamEx,
  {$ENDIF}
  System.JSON.Types, System.JSON.Writers, System.JSON.Readers;

type
  EJsonSerializationException = class(Exception) end;
  TJsonMemberSerialization = (Fields, &Public, &In);
  TJsonObjectHandling = (Auto, Reuse, Replace);
  TJsonObjectOwnership = (Auto, Owned, NotOwned);
  TJsonReferenceHandling = (None, Preserve, IgnoreCycles, ErrorOnCycles);
  TJsonValueSerialization = (Always, ExcludeDefault, ExcludeSpecial, ExcludeAll);

  TJsonConverter = class;
  TJsonConverterClass = class of TJsonConverter;

  { JsonConverterAttribute }

  JsonConverterAttribute = class(TCustomAttribute)
  private
    FValue: TJsonConverterClass;
  public
    constructor Create(const aValue: TJsonConverterClass);
    property Value: TJsonConverterClass read FValue;
  end;

  JsonIgnoreAttribute = class(TCustomAttribute) public constructor Create; end;

  JsonNameAttribute = System.JSON.Types.JsonNameAttribute;

  JsonInAttribute = class(TCustomAttribute) public constructor Create; end;

  { JsonObjectHandlingAttribute }

  JsonObjectHandlingAttribute = class(TCustomAttribute)
  private
    FValue: TJsonObjectHandling;
  public
    constructor Create(const aValue: TJsonObjectHandling);
    property Value: TJsonObjectHandling read FValue;
  end;

  { JsonObjectOwnership }

  JsonObjectOwnership = class(TCustomAttribute)
  private
    FValue: TJsonObjectOwnership;
  public
    constructor Create(const aValue: TJsonObjectOwnership);
    property Value: TJsonObjectOwnership read FValue;
  end;

  { JsonSerializeAttribute }

  JsonSerializeAttribute = class(TCustomAttribute)
  private
    FValue: TJsonMemberSerialization;
  public
    constructor Create(aValue: TJsonMemberSerialization);
    property Value: TJsonMemberSerialization read FValue;
  end;

  { JsonValueSerializeAttribute }

  JsonValueSerializeAttribute = class(TCustomAttribute)
  private
    FValue: TJsonValueSerialization;
  public
    constructor Create(aValue: TJsonValueSerialization);
    property Value: TJsonValueSerialization read FValue;
  end;

  TJsonSerializer = class;

  { TJsonConverter }

  TJsonConverter = class
  public
    function CanConvert(ATypeInf: PTypeInfo): Boolean; virtual; abstract;
    function CanRead: Boolean; virtual;
    function CanWrite: Boolean; overload; virtual;
    function CanWrite(const aValue: TValue): Boolean; overload; virtual;
    function ReadJson(const aReader: TJsonReader; aTypeInf: PTypeInfo; const aExistingValue: TValue;
      const aSerializer: TJsonSerializer): TValue; virtual; abstract;
    procedure WriteJson(const aWriter: TJsonWriter; const aValue: TValue;
      const aSerializer: TJsonSerializer); virtual; abstract;
  end;
  TJsonConverterList = specialize TList<TJsonConverter>;

  { TJsonConverterRegistry }

  TJsonConverterRegistry = class
  private
    class var FRegistered: specialize TList<TJsonConverterClass>;
  private
    class procedure EnsureCreated; static;
  public
    class procedure RegisterConverter(AClass: TJsonConverterClass); static;
    class procedure UnregisterConverter(AClass: TJsonConverterClass); static;
    class function IsRegistered(AClass: TJsonConverterClass): Boolean; static;
    class function RegisteredCount: Integer; static;
    class procedure Clear; static;
    class procedure PopulateConverters(AList: TJsonConverterList); static;
    class destructor Destroy;
  end;

  IJsonCreator = interface
    ['{B4E2F3A1-7C8D-4E9F-A1B2-C3D4E5F60002}']
    function Invoke(const Params: array of TValue): TValue;
    procedure Release(var Value: TValue);
  end;

  { TJsonObjectCreator }

  TJsonObjectCreator = class(TInterfacedObject, IJsonCreator)
  private
    FMetaClass: TClass;
    FConstructor: TRttiMethod;
  public
    constructor Create(const aMetaClass: TClass; const aConstructor: TRttiMethod);
    function Invoke(const Params: array of TValue): TValue;
    procedure Release(var Value: TValue);
  end;

  { TJsonRecordCreator }

  TJsonRecordCreator = class(TInterfacedObject, IJsonCreator)
  private
    FTypeInf: PTypeInfo;
    FParameterizedConstructor: TRttiMethod;
  public
    constructor Create(ATypeInf: PTypeInfo; const aParameterizedConstructor: TRttiMethod = nil);
    function Invoke(const Params: array of TValue): TValue;
    procedure Release(var Value: TValue);
  end;

  IJsonAttributeProvider = interface
    ['{B4E2F3A1-7C8D-4E9F-A1B2-C3D4E5F60003}']
    function GetAttribute(const aAttributeClass: TCustomAttributeClass; aInherit: Boolean = False): TCustomAttribute;
  end;

  TAttrKey = specialize TPair<TRttiObject, TCustomAttributeClass>;

  { TJsonInlineAttributes }

  TJsonInlineAttributes = class
  private
    class var FCachedObjects: specialize TDictionary<TRttiObject, Boolean>;
    class var FAttributes: specialize TDictionary<TAttrKey, TCustomAttribute>;
    class procedure LoadAttributes(const aRttiObject: TRttiObject); static;
  public
    class constructor Create;
    class destructor Destroy;
    class function GetAttribute(const aRttiObject: TRttiObject; const aAttributeClass: TCustomAttributeClass; aInherit: Boolean = False): TCustomAttribute; static;
  end;

  { TJsonDynamicAttributes }

  TJsonDynamicAttributes = class
  private
    FAttributes: specialize TDictionary<TAttrKey, TCustomAttribute>;
    FOwnedAttributes: specialize TObjectList<TCustomAttribute>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure addAttribute(const aRttiObject: TRttiObject; const aAttribute: TCustomAttribute);
    function GetAttribute(const aRttiObject: TRttiObject; const aAttributeClass: TCustomAttributeClass; aInherit: Boolean = False): TCustomAttribute;
  end;

  { TJsonInlineAttributeProvider }

  TJsonInlineAttributeProvider = class(TInterfacedObject, IJsonAttributeProvider)
  private
    FRttiObject: TRttiObject;
  public
    constructor Create(const aRttiObject: TRttiObject);
    function GetAttribute(const aAttributeClass: TCustomAttributeClass; aInherit: Boolean): TCustomAttribute;
  end;

  { TJsonDynamicAttributeProvider }

  TJsonDynamicAttributeProvider = class(TInterfacedObject, IJsonAttributeProvider)
  private
    FRttiObject: TRttiObject;
    FDynamicAttributes: TJsonDynamicAttributes;
  public
    constructor Create(const aRttiObject: TRttiObject; const aDynamicAttributes: TJsonDynamicAttributes);
    function GetAttribute(const attrClass: TCustomAttributeClass; aInherit: Boolean = False): TCustomAttribute;
  end;

  IJsonValueProvider = interface
    ['{B4E2F3A1-7C8D-4E9F-A1B2-C3D4E5F60001}']
    function GetValue(const aInstance: TValue): TValue;
    procedure SetValue(const aInstance: TValue; const Value: TValue);
  end;

  { TJsonValueProvider }

  TJsonValueProvider = class(TInterfacedObject, IJsonValueProvider)
  public
    function GetValue(const aInstance: TValue): TValue; virtual;
    procedure SetValue(const aInstance: TValue; const aValue: TValue); virtual;
  end;

  { TJsonFieldValueProvider }

  TJsonFieldValueProvider = class(TInterfacedObject, IJsonValueProvider)
  private
    FRttiField: TRttiField;
  public
    constructor Create(const aRttiField: TRttiField);
    function GetValue(const aInstance: TValue): TValue;
    procedure SetValue(const aInstance: TValue; const aValue: TValue);
  end;

  { TJsonPropertyValueProvider }

  TJsonPropertyValueProvider = class(TInterfacedObject, IJsonValueProvider)
  private
    FRttiProperty: TRttiProperty;
  public
    constructor Create(const aRttiProperty: TRttiProperty);
    function GetValue(const aInstance: TValue): TValue;
    procedure SetValue(const aInstance: TValue; const aValue: TValue);
  end;

  TJsonContractType = (&Object, &Class, &Array, Primitive, Converter);

  { TJsonContract }

  TJsonContract = class abstract
  private
    FAttributeProvider: IJsonAttributeProvider;
    FContractType: TJsonContractType;
    FConverter: TJsonConverter;
    FIgnored: Boolean;
    FIsReference: Boolean;
    FSealed: Boolean;
    FTypeInf: PTypeInfo;
  public
    constructor Create(ATypeInf: PTypeInfo);
    property Converter: TJsonConverter read FConverter write FConverter;
    property ContractType: TJsonContractType read FContractType;
    property Ignored: Boolean read FIgnored write FIgnored;
    property IsReference: Boolean read FIsReference write FIsReference;
    property TypeInf: PTypeInfo read FTypeInf;
    property attributeProvider: IJsonAttributeProvider read FAttributeProvider write FAttributeProvider;
    property &Sealed: Boolean read FSealed write FSealed;
  end;

  TJsonPrimitiveKind = (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, &Single, &Double, &Extended,
    &Comp, &Currency, &String, &Char, &Boolean, Enumeration, &Set, DateTime);

  { TJsonPrimitiveContract }

  TJsonPrimitiveContract = class(TJsonContract)
  private
    FKind: TJsonPrimitiveKind;
  public
    constructor Create(ATypeInf: PTypeInfo; aKind: TJsonPrimitiveKind);
    property Kind: TJsonPrimitiveKind read FKind;
  end;

  TJsonProperty = class;
  TJsonPropertyList = specialize TObjectList<TJsonProperty>;

  { TJsonObjectContract }

  TJsonObjectContract = class(TJsonContract)
  private
    FDefaultCreator: IJsonCreator;
    FMemberSerialization: TJsonMemberSerialization;
    FProperties: TJsonPropertyList;
    FValueSerialization: TJsonValueSerialization;

  public
    constructor Create(ATypeInf: PTypeInfo);
    destructor Destroy; override;
    function GetClosestMatchProperty(const aName: string): TJsonProperty;
    property Properties: TJsonPropertyList read FProperties;
    property MemberSerialization: TJsonMemberSerialization read FMemberSerialization write FMemberSerialization;
    property ValueSerialization: TJsonValueSerialization read FValueSerialization write FValueSerialization;
    property DefaultCreator: IJsonCreator read FDefaultCreator write FDefaultCreator;
    
  end;

  { TJsonArrayContract }

  TJsonArrayContract = class(TJsonContract)
  private
    FItemContract: TJsonContract;
    function GetArrayType: PTypeInfo;
  public
    constructor Create(ATypeInf: PTypeInfo);
    property arrayType: PTypeInfo read GetArrayType;
    property ItemContract: TJsonContract read FItemContract write FItemContract;
  end;

  { TJsonClassContract }

  TJsonClassContract = class(TJsonContract)
  public
    constructor Create(ATypeInf: PTypeInfo);
    function ResolveClassByName(const Name: string): TClass;
  end;

  { TJsonConverterContract }

  TJsonConverterContract = class(TJsonContract)
  public
    constructor Create(ATypeInf: PTypeInfo);
  end;

  { TJsonProperty }

  TJsonProperty = class
  private
    FAttributeProvider: IJsonAttributeProvider;
    FContract: TJsonContract;
    FConverter: TJsonConverter;
    FDefaultValue: Variant;
    FExcludeValue: Variant;
    FIgnored: Boolean;
    FName: string;
    FObjectHandling: TJsonObjectHandling;
    FObjectOwnership: TJsonObjectOwnership;
    FParentType: PTypeInfo;
    FReadable: Boolean;
    FTypeInf: PTypeInfo;
    FUnderlyingName: string;
    FValueProvider: IJsonValueProvider;
    FWritable: Boolean;
  public
    property TypeInf: PTypeInfo read FTypeInf write FTypeInf;
    property Contract: TJsonContract read FContract write FContract;
    property Converter: TJsonConverter read FConverter write FConverter;
    property Ignored: Boolean read FIgnored write FIgnored;
    property Name: string read FName write FName;
    property ParentType: PTypeInfo read FParentType write FParentType;
    property Readable: Boolean read FReadable write FReadable;
    property Writable: Boolean read FWritable write FWritable;
    property ValueProvider: IJsonValueProvider read FValueProvider write FValueProvider;
    property attributeProvider: IJsonAttributeProvider read FAttributeProvider write FAttributeProvider;
    property ObjectHandling: TJsonObjectHandling read FObjectHandling write FObjectHandling;
    property ObjectOwnership: TJsonObjectOwnership read FObjectOwnership write FObjectOwnership;
    property UnderlyingName: string read FUnderlyingName write FUnderlyingName;
    property DefaultValue: Variant read FDefaultValue write FDefaultValue;
    property ExcludeValue: Variant read FExcludeValue write FExcludeValue;
  end;

  IJsonContractResolver = interface
    ['{B4E2F3A1-7C8D-4E9F-A1B2-C3D4E5F60004}']
    function ResolveContract(ATypeInf: PTypeInfo): TJsonContract;
  end;

  { TJsonDefaultContractResolver }
  TRttiMemberList = specialize TList<TRttiMember>;

  TJsonDefaultContractResolver = class(TInterfacedObject, IJsonContractResolver)
  private class var
    FSharedRttiCtx: TRttiContext;
    FSharedRttiCtxInitialized: Boolean;
  private
    class procedure EnsureRttiCtx; static;
  protected
  protected
    FDefaultMemberSerialization: TJsonMemberSerialization;
    FDefaultValueSerialization: TJsonValueSerialization;
    FCachedAttributeConverters: specialize TDictionary<TJsonConverterClass, TJsonConverter>;
    FCachedContracts: specialize TObjectDictionary<PTypeInfo, TJsonContract>;
    FListHelperConverter: TJsonConverter;
    FGUIDConverter: TJsonConverter;
    function CreateArrayContract(ATypeInf: PTypeInfo): TJsonArrayContract; virtual;
    function CreateClassContract(ATypeInf: PTypeInfo): TJsonClassContract; virtual;
    function CreateContract(ATypeInf: PTypeInfo): TJsonContract; virtual;
    function CreateConverterContract(ATypeInf: PTypeInfo): TJsonConverterContract; virtual;
    function CreateObjectContract(ATypeInf: PTypeInfo): TJsonObjectContract; virtual;
    function CreatePrimitiveContract(ATypeInf: PTypeInfo): TJsonPrimitiveContract; virtual;
    function CreateProperty(const aRttiMember: TRttiMember; aMemberSerialization: TJsonMemberSerialization): TJsonProperty; virtual;
    procedure CreateProperties(ATypeInf: PTypeInfo; aMemberSerialization: TJsonMemberSerialization; const aOutProperties: TJsonPropertyList); virtual;
    procedure GetSerializableMembers(ATypeInf: PTypeInfo; const aMembers: TRttiMemberList); virtual;
    function ShouldIncludeMember(const aMember: TRttiMember; aMemberSerialization: TJsonMemberSerialization): Boolean; virtual;
    function GetConverterFromAttribute(const aConverterAttribute: JsonConverterAttribute): TJsonConverter;
    procedure InitializeContract(const aJsonContract: TJsonContract; const aRttiType: TRttiType); virtual;
    procedure SetPropertySettingsFromAttributes(const aProperty: TJsonProperty; const aRttiMember: TRttiMember;
      aMemberSerialization: TJsonMemberSerialization); virtual;
    procedure SetPropertySpecialValues(const aProperty: TJsonProperty; const aRttiMember: TRttiMember;
      aValueSerialization: TJsonValueSerialization); virtual;
    function CreateValueProvider(const aRttiMember: TRttiMember): IJsonValueProvider; virtual;
    function CreateAttributeProvider(const aRttiObject: TRttiObject): IJsonAttributeProvider; virtual;
    function ResolvePropertyName(const aName: string): string; virtual;
  public
    constructor Create(ADefaultMemberSerialization: TJsonMemberSerialization = TJsonMemberSerialization.Fields;
                       aDefaultValueSerialization: TJsonValueSerialization = TJsonValueSerialization.ExcludeSpecial);
    destructor Destroy; override;
    procedure ClearCache; virtual;
    function ResolveContract(ATypeInf: PTypeInfo): TJsonContract;
  end;

  { TJsonDynamicContractResolver }

  TJsonDynamicContractResolver = class(TJsonDefaultContractResolver)
  private
    FDynamicAttributes: TJsonDynamicAttributes;
  protected
    function CreateAttributeProvider(const aRttiObject: TRttiObject): IJsonAttributeProvider; override;
  public
    constructor Create(ADefaultMemberSerialization: TJsonMemberSerialization = TJsonMemberSerialization.Fields;
                       aDefaultValueSerialization: TJsonValueSerialization = TJsonValueSerialization.ExcludeSpecial);
    destructor Destroy; override;
    procedure ClearAttributes;
    procedure SetFieldConverter(ATypeInf: PTypeInfo; const aFieldName: string; const aConverterClass: TJsonConverterClass);
    procedure SetFieldName(ATypeInf: PTypeInfo; const aFieldName: string; const aResolvedName: string);
    procedure SetFieldsIgnored(ATypeInf: PTypeInfo; const aFieldNames: array of string);
    procedure SetFieldsIn(ATypeInf: PTypeInfo; const aFieldNames: array of string);
    procedure SetPropertiesIgnored(ATypeInf: PTypeInfo; const aPropertyNames: array of string);
    procedure SetPropertiesIn(ATypeInf: PTypeInfo; const aPropertyNames: array of string);
    procedure SetPropertyConverter(ATypeInf: PTypeInfo; const aPropertyName: string; const aConverterClass: TJsonConverterClass);
    procedure SetPropertyName(ATypeInf: PTypeInfo; const aPropertyName: string; const aResolvedName: string);
    procedure SetTypeConverter(ATypeInf: PTypeInfo; const aConverterClass: TJsonConverterClass);
    procedure SetTypeMemberSerialization(ATypeInf: PTypeInfo; const aMemberSerialization: TJsonMemberSerialization);
    procedure SetTypeIgnored(ATypeInf: PTypeInfo);
  end;

  { IJsonTypeResolver }

  IJsonTypeResolver = interface
    ['{B4E2F3A1-7C8D-4E9F-A1B2-C3D4E5F60005}']
    function ResolveType(const aTypeName: string): PTypeInfo;
    function ResolveName(const aTypeInf: PTypeInfo): string;
  end;

  { TJsonDefaultTypeResolver }

  TJsonDefaultTypeResolver = class(TInterfacedObject, IJsonTypeResolver)
  public
    function ResolveType(const aTypeName: string): PTypeInfo; virtual;
    function ResolveName(const aTypeInf: PTypeInfo): string; virtual;
  end;

  IJsonReferenceResolver = interface
    ['{B4E2F3A1-7C8D-4E9F-A1B2-C3D4E5F60006}']
    function CreateContext(AWriteJson: Boolean; aMode: TJsonReferenceHandling): TObject;
    procedure addReference(const aContext: TObject; const aReference: string; const aValue: TObject);
    function IsReferenced(const aContext: TObject; aValue: TObject): Boolean;
    function GetReference(const aContext: TObject; aValue: TObject; out aExisting: Boolean): string;
    function ResolveReference(const aContext: TObject; const aReference: string): TObject;
    function PushReference(const aContext: TObject; aValue: TObject): Boolean;
    procedure PopReference(const aContext: TObject; aValue: TObject);
  end;

  { TJsonDefaultReferenceResolver }

  TJsonDefaultReferenceResolver = class(TInterfacedObject, IJsonReferenceResolver)
  public type

    { TContext }

    TContext = class(TObject)
    private
      FMode: TJsonReferenceHandling;
      FRefNameDict: Specialize TDictionary<TObject, string>;
      FNameRefDict: Specialize TDictionary<string, TObject>;
      FReferenceCount: Integer;
      FRefSet: Specialize THashSet<TObject>;
    public
      constructor Create(AWriteJson: Boolean; aMode: TJsonReferenceHandling);
      destructor Destroy; override;
    end;
  public
    function CreateContext(AWriteJson: Boolean; aMode: TJsonReferenceHandling): TObject;
    procedure addReference(const aContext: TObject; const aReference: string; const aValue: TObject);
    function IsReferenced(const aContext: TObject; aValue: TObject): Boolean;
    function GetReference(const aContext: TObject; aValue: TObject; out aExisting: Boolean): string;
    function ResolveReference(const aContext: TObject; const aReference: string): TObject;
    function PushReference(const aContext: TObject; aValue: TObject): Boolean;
    procedure PopReference(const aContext: TObject; aValue: TObject);
  end;

  { TJsonSerializer }

  TJsonSerializer = class
  private
    FContractResolver: IJsonContractResolver;
    FConverters: Specialize TObjectList<TJsonConverter>;
    FDateFormatHandling: TJsonDateFormatHandling;
    FDateParseHandling: TJsonDateParseHandling;
    FDateTimeZoneHandling: TJsonDateTimeZoneHandling;
    FFloatFormatHandling: TJsonFloatFormatHandling;
    FFormatting: TJsonFormatting;
    FMaxDepth: Integer;
    FObjectHandling: TJsonObjectHandling;
    FObjectOwnership: TJsonObjectOwnership;
    FReferenceResolver: IJsonReferenceResolver;
    FReferenceHandling: TJsonReferenceHandling;
    FStringEscapeHandling: TJsonStringEscapeHandling;
    FTypeResolver: IJsonTypeResolver;
    FMemberSerialization: TJsonMemberSerialization;
    FValueSerialization: TJsonValueSerialization;
    procedure SerializePrimitive(const aWriter: TJsonWriter; const aValue: TValue; aContract: TJsonPrimitiveContract);
    procedure SerializeObject(const aWriter: TJsonWriter; const aValue: TValue; aContract: TJsonObjectContract);
    procedure SerializeArray(const aWriter: TJsonWriter; const aValue: TValue; aContract: TJsonArrayContract);
    function DeserializePrimitive(const aReader: TJsonReader; aContract: TJsonPrimitiveContract): TValue;
    function DeserializeObject(const aReader: TJsonReader; aContract: TJsonObjectContract): TValue;
    function DeserializeArray(const aReader: TJsonReader; aContract: TJsonArrayContract): TValue;
  protected
    function GetContractResolver: IJsonContractResolver; virtual;
    function GetConverters: TJsonConverterList; virtual;
    function GetDateFormatHandling: TJsonDateFormatHandling; virtual;
    function GetDateParseHandling: TJsonDateParseHandling; virtual;
    function GetDateTimeZoneHandling: TJsonDateTimeZoneHandling; virtual;
    function GetFloatFormatHandling: TJsonFloatFormatHandling; virtual;
    function GetFormatting: TJsonFormatting; virtual;
    function GetMaxDepth: Integer; virtual;
    function GetReferenceResolver: IJsonReferenceResolver; virtual;
    function GetObjectHandling: TJsonObjectHandling; virtual;
    function GetObjectOwnership: TJsonObjectOwnership; virtual;
    function GetStringEscapeHandling: TJsonStringEscapeHandling; virtual;
    function GetTypeResolver: IJsonTypeResolver; virtual;
    function GetReferenceHandling: TJsonReferenceHandling; virtual;
    function GetValueSerialization: TJsonValueSerialization; virtual;
    function GetMemberSerialization: TJsonMemberSerialization; virtual;
    procedure SetContractResolver(const aValue: IJsonContractResolver); virtual;
    procedure SetDateFormatHandling(aValue: TJsonDateFormatHandling); virtual;
    procedure SetDateParseHandling(aValue: TJsonDateParseHandling); virtual;
    procedure SetDateTimeZoneHandling(aValue: TJsonDateTimeZoneHandling); virtual;
    procedure SetFloatFormatHandling(aValue: TJsonFloatFormatHandling); virtual;
    procedure SetFormatting(aValue: TJsonFormatting); virtual;
    procedure SetMaxDepth(aValue: Integer); virtual;
    procedure SetObjectHandling(const aValue: TJsonObjectHandling); virtual;
    procedure SetObjectOwnership(const aValue: TJsonObjectOwnership); virtual;
    procedure SetReferenceResolver(const aValue: IJsonReferenceResolver); virtual;
    procedure SetStringEscapeHandling(aValue: TJsonStringEscapeHandling); virtual;
    procedure SetTypeResolver(const aValue: IJsonTypeResolver); virtual;
    procedure SetReferenceHandling(const aValue: TJsonReferenceHandling); virtual;
    procedure SetValueSerialization(const aValue: TJsonValueSerialization); virtual;
    procedure SetMemberSerialization(const aValue: TJsonMemberSerialization); virtual;

    procedure InternalSerialize(const aWriter: TJsonWriter; const aValue: TValue); virtual;
    function InternalDeserialize(const aReader: TJsonReader; aTypeInf: PTypeInfo): TValue; virtual;
    procedure InternalPopulate(const Reader: TJsonReader; var aValue: TValue; aUseConverter: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    generic function Serialize<T>(const aValue: T): string; overload;
    generic procedure Serialize<T>(const aWriter: TTextWriter; const aValue: T); overload;
    generic procedure Serialize<T>(const aWriter: TJsonWriter; const aValue: T); overload;
    procedure Serialize(const aWriter: TJsonWriter; const aValue: TValue); overload;
    generic function Deserialize<T>(const aJson: string): T; overload;
    generic function Deserialize<T>(const aReader: TTextReader): T; overload;
    generic function Deserialize<T>(const aReader: TJsonReader): T; overload;
    function Deserialize(const aReader: TJsonReader; aTypeinfo: PTypeInfo): TValue; overload;
    generic procedure Populate<T>(const aJson: string; var aValue: T); overload;
    generic procedure Populate<T>(const aReader: TTextReader; var aValue: T); overload;
    generic procedure Populate<T>(const aReader: TJsonReader; var aValue: T); overload;
    procedure Populate(const aReader: TJsonReader; var aValue: TValue); overload;
    procedure Populate(const aReader: TJsonReader; var aValue: TValue; aUseConverter: Boolean); overload;
    class function MatchConverter(const aConverters:  TJsonConverterList; aTypeInf: PTypeInfo): TJsonConverter; static;
    property ContractResolver: IJsonContractResolver read GetContractResolver write SetContractResolver;
    property Converters:  TJsonConverterList read GetConverters;
    property DateFormatHandling: TJsonDateFormatHandling read GetDateFormatHandling write SetDateFormatHandling;
    property DateParseHandling: TJsonDateParseHandling read GetDateParseHandling write SetDateParseHandling;
    property DateTimeZoneHandling: TJsonDateTimeZoneHandling read GetDateTimeZoneHandling write SetDateTimeZoneHandling;
    property FloatFormatHandling: TJsonFloatFormatHandling read GetFloatFormatHandling write SetFloatFormatHandling;
    property Formatting: TJsonFormatting read GetFormatting write SetFormatting;
    property MaxDepth: Integer read GetMaxDepth write SetMaxDepth;
    property ObjectHandling: TJsonObjectHandling read GetObjectHandling write SetObjectHandling;
    property ObjectOwnership: TJsonObjectOwnership read GetObjectOwnership write SetObjectOwnership;
    property ReferenceResolver: IJsonReferenceResolver read GetReferenceResolver write SetReferenceResolver;
    property StringEscapeHandling: TJsonStringEscapeHandling read GetStringEscapeHandling write SetStringEscapeHandling;
    property TypeResolver: IJsonTypeResolver read GetTypeResolver write SetTypeResolver;
    property ReferenceHandling: TJsonReferenceHandling read GetReferenceHandling write SetReferenceHandling;
    property MemberSerialization: TJsonMemberSerialization read GetMemberSerialization write SetMemberSerialization;
    property ValueSerialization: TJsonValueSerialization read GetValueSerialization write SetValueSerialization;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Variants,
  {$ELSE}
  Variants,
  {$ENDIF}
  System.JSONConsts, System.JSON;

{ JsonIgnoreAttribute }

constructor JsonIgnoreAttribute.Create;
begin
  inherited Create;
end;

{ JsonInAttribute }

constructor JsonInAttribute.Create;
begin
  inherited Create;
end;

{ JsonObjectHandlingAttribute }

constructor JsonObjectHandlingAttribute.Create(const aValue: TJsonObjectHandling);
begin
  FValue:=aValue;
end;

{ JsonObjectOwnership }

constructor JsonObjectOwnership.Create(const aValue: TJsonObjectOwnership);
begin
  FValue := aValue;
end;

{ JsonSerializeAttribute }

constructor JsonSerializeAttribute.Create(aValue: TJsonMemberSerialization);
begin
  FValue := aValue;
end;

{ JsonValueSerializeAttribute }

constructor JsonValueSerializeAttribute.Create(aValue: TJsonValueSerialization);
begin
  FValue := aValue;
end;

{ TJsonConverter }

function TJsonConverter.CanRead: Boolean;
begin
  Result := True;
end;

function TJsonConverter.CanWrite: Boolean;
begin
  Result := True;
end;

function TJsonConverter.CanWrite(const aValue: TValue): Boolean;
begin
  Result := CanWrite;
end;

{ TJsonConverterRegistry }

class procedure TJsonConverterRegistry.EnsureCreated;
begin
  if FRegistered = nil then
    FRegistered := specialize TList<TJsonConverterClass>.Create;
end;

class procedure TJsonConverterRegistry.RegisterConverter(AClass: TJsonConverterClass);
begin
  EnsureCreated;
  if not FRegistered.Contains(AClass) then
    FRegistered.Add(AClass);
end;

class procedure TJsonConverterRegistry.UnregisterConverter(AClass: TJsonConverterClass);
begin
  if FRegistered <> nil then
    FRegistered.Remove(AClass);
end;

class function TJsonConverterRegistry.IsRegistered(AClass: TJsonConverterClass): Boolean;
begin
  Result := (FRegistered <> nil) and FRegistered.Contains(AClass);
end;

class function TJsonConverterRegistry.RegisteredCount: Integer;
begin
  if FRegistered <> nil then
    Result := FRegistered.Count
  else
    Result := 0;
end;

class procedure TJsonConverterRegistry.Clear;
begin
  if FRegistered <> nil then
    FRegistered.Clear;
end;

class procedure TJsonConverterRegistry.PopulateConverters(AList: TJsonConverterList);
var
  I: Integer;
begin
  if FRegistered = nil then
    Exit;
  for I := 0 to FRegistered.Count - 1 do
    AList.Add(FRegistered[I].Create);
end;

class destructor TJsonConverterRegistry.Destroy;
begin
  FreeAndNil(FRegistered);
end;

{ TJsonObjectCreator }

constructor TJsonObjectCreator.Create(const aMetaClass: TClass; const aConstructor: TRttiMethod);
begin
  inherited Create;
  FMetaClass := aMetaClass;
  FConstructor := aConstructor;
end;

function TJsonObjectCreator.Invoke(const Params: array of TValue): TValue;
begin
  if FConstructor <> nil then
    Result := FConstructor.Invoke(TValue.specialize From<TClass>(FMetaClass), Params)
  else
    Result := TValue.specialize From<TObject>(FMetaClass.Create);
end;

procedure TJsonObjectCreator.Release(var Value: TValue);
begin
  if Value.IsObject and (Value.AsObject <> nil) then
    Value.AsObject.Free;
end;

{ TJsonRecordCreator }

constructor TJsonRecordCreator.Create(ATypeInf: PTypeInfo; const aParameterizedConstructor: TRttiMethod);
begin
  inherited Create;
  FTypeInf := ATypeInf;
  FParameterizedConstructor := aParameterizedConstructor;
end;

function TJsonRecordCreator.Invoke(const Params: array of TValue): TValue;
begin
  if FParameterizedConstructor <> nil then
    Result := FParameterizedConstructor.Invoke(TValue.Empty, Params)
  else
    TValue.Make(nil, FTypeInf, Result);
end;

procedure TJsonRecordCreator.Release(var Value: TValue);
begin
  // Records are value types - nothing to free
end;

{ TJsonInlineAttributes }

class procedure TJsonInlineAttributes.LoadAttributes(const aRttiObject: TRttiObject);
var
  Attrs: Specialize TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
  Key: TAttrKey;
begin
  if FCachedObjects.ContainsKey(aRttiObject) then
    Exit;
  Attrs := aRttiObject.GetAttributes;
  for Attr in Attrs do
  begin
    Key := TAttrKey.Create(aRttiObject, TCustomAttributeClass(Attr.ClassType));
    if not FAttributes.ContainsKey(Key) then
      FAttributes.Add(Key, Attr);
  end;
  FCachedObjects.Add(aRttiObject, True);
end;

class constructor TJsonInlineAttributes.Create;
begin
  FCachedObjects := specialize TDictionary<TRttiObject, Boolean>.Create;
  FAttributes := specialize TDictionary<TAttrKey, TCustomAttribute>.Create;
end;

class destructor TJsonInlineAttributes.Destroy;
begin
  FAttributes.Free;
  FCachedObjects.Free;
end;

class function TJsonInlineAttributes.GetAttribute(const aRttiObject: TRttiObject; const aAttributeClass: TCustomAttributeClass;
  aInherit: Boolean): TCustomAttribute;
var
  Key: TAttrKey;
begin
  LoadAttributes(aRttiObject);
  Key := TAttrKey.Create(aRttiObject, aAttributeClass);
  if not FAttributes.TryGetValue(Key, Result) then
    Result := nil;
end;

{ TJsonDynamicAttributes }

constructor TJsonDynamicAttributes.Create;
begin
  inherited Create;
  FAttributes := specialize TDictionary<TAttrKey, TCustomAttribute>.Create;
  FOwnedAttributes := specialize TObjectList<TCustomAttribute>.Create(True);
end;

destructor TJsonDynamicAttributes.Destroy;
begin
  FOwnedAttributes.Free;
  FAttributes.Free;
  inherited Destroy;
end;

procedure TJsonDynamicAttributes.Clear;
begin
  FAttributes.Clear;
  FOwnedAttributes.Clear;
end;

procedure TJsonDynamicAttributes.addAttribute(const aRttiObject: TRttiObject; const aAttribute: TCustomAttribute);
var
  Key: TAttrKey;
begin
  Key := TAttrKey.Create(aRttiObject, TCustomAttributeClass(aAttribute.ClassType));
  FAttributes.AddOrSetValue(Key, aAttribute);
  FOwnedAttributes.Add(aAttribute);
end;

function TJsonDynamicAttributes.GetAttribute(const aRttiObject: TRttiObject; const aAttributeClass: TCustomAttributeClass;
  aInherit: Boolean): TCustomAttribute;
var
  Key: TAttrKey;
begin
  Key := TAttrKey.Create(aRttiObject, aAttributeClass);
  if not FAttributes.TryGetValue(Key, Result) then
    Result := nil;
end;

{ TJsonInlineAttributeProvider }

constructor TJsonInlineAttributeProvider.Create(const aRttiObject: TRttiObject);
begin
  inherited Create;
  FRttiObject := aRttiObject;
end;

function TJsonInlineAttributeProvider.GetAttribute(const aAttributeClass: TCustomAttributeClass; aInherit: Boolean
  ): TCustomAttribute;
begin
  Result := TJsonInlineAttributes.GetAttribute(FRttiObject, aAttributeClass, aInherit);
end;

{ TJsonDynamicAttributeProvider }

constructor TJsonDynamicAttributeProvider.Create(const aRttiObject: TRttiObject; const aDynamicAttributes: TJsonDynamicAttributes);
begin
  inherited Create;
  FRttiObject := aRttiObject;
  FDynamicAttributes := aDynamicAttributes;
end;

function TJsonDynamicAttributeProvider.GetAttribute(const attrClass: TCustomAttributeClass; aInherit: Boolean): TCustomAttribute;
begin
  Result := FDynamicAttributes.GetAttribute(FRttiObject, attrClass, aInherit);
  if Result = nil then
    Result := TJsonInlineAttributes.GetAttribute(FRttiObject, attrClass, aInherit);
end;

{ TJsonValueProvider }

function TJsonValueProvider.GetValue(const aInstance: TValue): TValue;
begin
  raise EJsonSerializationException.Create('TJsonValueProvider.GetValue should not be called directly');
end;

procedure TJsonValueProvider.SetValue(const aInstance: TValue; const aValue: TValue);
begin
  raise EJsonSerializationException.Create('TJsonValueProvider.SetValue should not be called directly');
end;

{ TJsonFieldValueProvider }

constructor TJsonFieldValueProvider.Create(const aRttiField: TRttiField);
begin
  inherited Create;
  FRttiField := aRttiField;
end;

function TJsonFieldValueProvider.GetValue(const aInstance: TValue): TValue;
begin
  if aInstance.IsObject then
    Result := FRttiField.GetValue(aInstance.AsObject)
  else
    Result := FRttiField.GetValue(aInstance.GetReferenceToRawData);
end;

procedure TJsonFieldValueProvider.SetValue(const aInstance: TValue; const aValue: TValue);
begin
  if aInstance.IsObject then
    FRttiField.SetValue(aInstance.AsObject, aValue)
  else
    FRttiField.SetValue(aInstance.GetReferenceToRawData, aValue);
end;

{ TJsonPropertyValueProvider }

constructor TJsonPropertyValueProvider.Create(const aRttiProperty: TRttiProperty);
begin
  inherited Create;
  FRttiProperty := aRttiProperty;
end;

function TJsonPropertyValueProvider.GetValue(const aInstance: TValue): TValue;
begin
  if aInstance.IsObject then
    Result := FRttiProperty.GetValue(aInstance.AsObject)
  else
    Result := FRttiProperty.GetValue(aInstance.GetReferenceToRawData);
end;

procedure TJsonPropertyValueProvider.SetValue(const aInstance: TValue; const aValue: TValue);
begin
  if aInstance.IsObject then
    FRttiProperty.SetValue(aInstance.AsObject, aValue)
  else
    FRttiProperty.SetValue(aInstance.GetReferenceToRawData, aValue);
end;

{ TJsonContract }

constructor TJsonContract.Create(ATypeInf: PTypeInfo);
begin
  inherited Create;
  FTypeInf := ATypeInf;
end;

{ TJsonPrimitiveContract }

constructor TJsonPrimitiveContract.Create(ATypeInf: PTypeInfo; aKind: TJsonPrimitiveKind);
begin
  inherited Create(ATypeInf);
  FKind := aKind;
  FContractType := TJsonContractType.Primitive;
end;

{ TJsonObjectContract }

constructor TJsonObjectContract.Create(ATypeInf: PTypeInfo);
begin
  inherited Create(ATypeInf);
  FProperties := specialize TObjectList<TJsonProperty>.Create(True);
  FContractType := TJsonContractType.&Object;
end;

destructor TJsonObjectContract.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

function TJsonObjectContract.GetClosestMatchProperty(const aName: string): TJsonProperty;
var
  Prop: TJsonProperty;
begin
  Result := nil;
  for Prop in FProperties do
    if SameText(Prop.Name, aName) then
      Exit(Prop);
end;

{ TJsonArrayContract }

function TJsonArrayContract.GetArrayType: PTypeInfo;
var
  TD: PTypeData;
begin
  TD := GetTypeData(FTypeInf);
  Result := TD^.ElType2;
end;

constructor TJsonArrayContract.Create(ATypeInf: PTypeInfo);
begin
  inherited Create(ATypeInf);
  FContractType := TJsonContractType.&Array;
end;

{ TJsonClassContract }

constructor TJsonClassContract.Create(ATypeInf: PTypeInfo);
begin
  inherited Create(ATypeInf);
  FContractType := TJsonContractType.&Class;
end;

function TJsonClassContract.ResolveClassByName(const Name: string): TClass;
var
  C: TPersistentClass;
begin
  C := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Classes.GetClass(Name);
  if C <> nil then
    Result := C
  else
    Result := nil;
end;

{ TJsonConverterContract }

constructor TJsonConverterContract.Create(ATypeInf: PTypeInfo);
begin
  inherited Create(ATypeInf);
  FContractType := TJsonContractType.Converter;
end;

{ TJsonDefaultContractResolver }

function TJsonDefaultContractResolver.CreateArrayContract(ATypeInf: PTypeInfo): TJsonArrayContract;
begin
  Result := TJsonArrayContract.Create(ATypeInf);
end;

function TJsonDefaultContractResolver.CreateClassContract(ATypeInf: PTypeInfo): TJsonClassContract;
begin
  Result := TJsonClassContract.Create(ATypeInf);
end;

function TJsonDefaultContractResolver.CreateContract(ATypeInf: PTypeInfo): TJsonContract;
begin
  case ATypeInf^.Kind of
    tkInteger, tkInt64, tkQWord, tkFloat, tkBool, tkEnumeration, tkSet,
    tkChar, tkWChar, tkSString, tkAString, tkUString, tkLString, tkWString:
      Result := CreatePrimitiveContract(ATypeInf);
    tkRecord:
      Result := CreateObjectContract(ATypeInf);
    tkClass:
      Result := CreateObjectContract(ATypeInf);
    tkDynArray:
      Result := CreateArrayContract(ATypeInf);
  else
    raise EJsonSerializationException.CreateFmt(SUnsupportedType, [ATypeInf^.Name]);
  end;
end;

function TJsonDefaultContractResolver.CreateConverterContract(ATypeInf: PTypeInfo): TJsonConverterContract;
begin
  Result := TJsonConverterContract.Create(ATypeInf);
end;

function TJsonDefaultContractResolver.CreateObjectContract(ATypeInf: PTypeInfo): TJsonObjectContract;
var
  RttiType: TRttiType;
  MemberSer: TJsonMemberSerialization;
  SerAttr: TCustomAttribute;
begin
  Result := TJsonObjectContract.Create(ATypeInf);
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType <> nil then
  begin
    InitializeContract(Result, RttiType);
    // Get member serialization from attribute or default
    MemberSer := FDefaultMemberSerialization;
    if Result.attributeProvider <> nil then
      SerAttr := Result.attributeProvider.GetAttribute(JsonSerializeAttribute, False)
    else
      SerAttr := TJsonInlineAttributes.GetAttribute(RttiType, JsonSerializeAttribute);
    if SerAttr <> nil then
      MemberSer := JsonSerializeAttribute(SerAttr).Value;
    Result.MemberSerialization := MemberSer;
    Result.ValueSerialization := FDefaultValueSerialization;
    CreateProperties(ATypeInf, MemberSer, Result.Properties);
    // Set default creator
    if ATypeInf^.Kind = tkRecord then
      Result.DefaultCreator := TJsonRecordCreator.Create(ATypeInf)
    else if ATypeInf^.Kind = tkClass then
      Result.DefaultCreator := TJsonObjectCreator.Create(GetTypeData(ATypeInf)^.ClassType, nil);
  end;
end;

function TJsonDefaultContractResolver.CreatePrimitiveContract(ATypeInf: PTypeInfo): TJsonPrimitiveContract;
var
  TD: PTypeData;
  Kind: TJsonPrimitiveKind;
begin
  TD := GetTypeData(ATypeInf);
  case ATypeInf^.Kind of
    tkInteger:
      case TD^.OrdType of
        otSByte: Kind := TJsonPrimitiveKind.Int8;
        otUByte: Kind := TJsonPrimitiveKind.UInt8;
        otSWord: Kind := TJsonPrimitiveKind.Int16;
        otUWord: Kind := TJsonPrimitiveKind.UInt16;
        otSLong: Kind := TJsonPrimitiveKind.Int32;
        otULong: Kind := TJsonPrimitiveKind.UInt32;
      else
        Kind := TJsonPrimitiveKind.Int32;
      end;
    tkInt64:
      Kind := TJsonPrimitiveKind.Int64;
    tkQWord:
      Kind := TJsonPrimitiveKind.UInt64;
    tkFloat:
      case TD^.FloatType of
        ftSingle: Kind := TJsonPrimitiveKind.&Single;
        ftDouble: Kind := TJsonPrimitiveKind.&Double;
        ftExtended: Kind := TJsonPrimitiveKind.&Extended;
        ftComp: Kind := TJsonPrimitiveKind.&Comp;
        ftCurr: Kind := TJsonPrimitiveKind.&Currency;
      else
        Kind := TJsonPrimitiveKind.&Double;
      end;
    tkBool:
      Kind := TJsonPrimitiveKind.&Boolean;
    tkEnumeration:
      if ATypeInf = TypeInfo(Boolean) then
        Kind := TJsonPrimitiveKind.&Boolean
      else
        Kind := TJsonPrimitiveKind.Enumeration;
    tkSet:
      Kind := TJsonPrimitiveKind.&Set;
    tkChar, tkWChar:
      Kind := TJsonPrimitiveKind.&Char;
    tkSString, tkAString, tkUString, tkLString, tkWString:
      Kind := TJsonPrimitiveKind.&String;
  else
    raise EJsonSerializationException.Create(SUnexpectedTypePrimitiveContract);
  end;
  Result := TJsonPrimitiveContract.Create(ATypeInf, Kind);
end;

function TJsonDefaultContractResolver.CreateProperty(const aRttiMember: TRttiMember; aMemberSerialization: TJsonMemberSerialization
  ): TJsonProperty;
begin
  Result := TJsonProperty.Create;
  Result.UnderlyingName := aRttiMember.Name;
  Result.Name := ResolvePropertyName(aRttiMember.Name);
  if aRttiMember is TRttiField then
  begin
    Result.TypeInf := TRttiField(aRttiMember).FieldType.Handle;
    Result.Readable := True;
    Result.Writable := True;
  end
  else if aRttiMember is TRttiProperty then
  begin
    Result.TypeInf := TRttiProperty(aRttiMember).PropertyType.Handle;
    Result.Readable := TRttiProperty(aRttiMember).IsReadable;
    Result.Writable := TRttiProperty(aRttiMember).IsWritable;
  end;
  Result.ValueProvider := CreateValueProvider(aRttiMember);
  Result.attributeProvider := CreateAttributeProvider(aRttiMember);
  SetPropertySettingsFromAttributes(Result, aRttiMember, aMemberSerialization);
end;

procedure TJsonDefaultContractResolver.CreateProperties(ATypeInf: PTypeInfo; aMemberSerialization: TJsonMemberSerialization;
  const aOutProperties: TJsonPropertyList);
var
  Members: Specialize TList<TRttiMember>;
  Member: TRttiMember;
  Prop: TJsonProperty;
begin
  Members := Specialize TList<TRttiMember>.Create;
  try
    GetSerializableMembers(ATypeInf, Members);
    for Member in Members do
    begin
      if ShouldIncludeMember(Member, aMemberSerialization) then
      begin
        Prop := CreateProperty(Member, aMemberSerialization);
        Prop.ParentType := ATypeInf;
        aOutProperties.Add(Prop);
      end;
    end;
  finally
    Members.Free;
  end;
end;

procedure TJsonDefaultContractResolver.GetSerializableMembers(ATypeInf: PTypeInfo; const aMembers: TRttiMemberList);
var
  RttiType: TRttiType;
  Field: TRttiField;
  Prop: TRttiProperty;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then
    Exit;
  for Field in RttiType.GetDeclaredFields do
    aMembers.Add(Field);
  for Prop in RttiType.GetDeclaredProperties do
    aMembers.Add(Prop);
end;

function TJsonDefaultContractResolver.ShouldIncludeMember(const aMember: TRttiMember; aMemberSerialization: TJsonMemberSerialization
  ): Boolean;
var
  HasJsonIn: Boolean;
begin
  HasJsonIn := TJsonInlineAttributes.GetAttribute(aMember, JsonInAttribute) <> nil;
  case aMemberSerialization of
    TJsonMemberSerialization.Fields:
      Result := (aMember is TRttiField) or HasJsonIn;
    TJsonMemberSerialization.&Public:
      begin
        if aMember is TRttiProperty then
          Result := TRttiProperty(aMember).Visibility >= mvPublished
        else
          Result := HasJsonIn;
      end;
    TJsonMemberSerialization.&In:
      Result := HasJsonIn;
  else
    Result := False;
  end;
end;

function TJsonDefaultContractResolver.GetConverterFromAttribute(const aConverterAttribute: JsonConverterAttribute): TJsonConverter;
var
  Conv: TJsonConverter;
begin
  if FCachedAttributeConverters.TryGetValue(aConverterAttribute.Value, Conv) then
    Exit(Conv);
  Conv := aConverterAttribute.Value.Create;
  FCachedAttributeConverters.Add(aConverterAttribute.Value, Conv);
  Result := Conv;
end;

procedure TJsonDefaultContractResolver.InitializeContract(const aJsonContract: TJsonContract; const aRttiType: TRttiType);
var
  Attr: TCustomAttribute;
begin
  aJsonContract.attributeProvider := CreateAttributeProvider(aRttiType);
  if aJsonContract.attributeProvider <> nil then
    Attr := aJsonContract.attributeProvider.GetAttribute(JsonConverterAttribute, False)
  else
    Attr := TJsonInlineAttributes.GetAttribute(aRttiType, JsonConverterAttribute);
  if Attr <> nil then
    aJsonContract.Converter := GetConverterFromAttribute(JsonConverterAttribute(Attr));
end;

procedure TJsonDefaultContractResolver.SetPropertySettingsFromAttributes(const aProperty: TJsonProperty;
  const aRttiMember: TRttiMember; aMemberSerialization: TJsonMemberSerialization);
var
  Attr: TCustomAttribute;
  AP: IJsonAttributeProvider;
begin
  AP := aProperty.attributeProvider;

  // JsonNameAttribute -> override Name
  if AP <> nil then
    Attr := AP.GetAttribute(JsonNameAttribute, False)
  else
    Attr := TJsonInlineAttributes.GetAttribute(aRttiMember, JsonNameAttribute);
  if Attr <> nil then
    aProperty.Name := JsonNameAttribute(Attr).Value;

  // JsonIgnoreAttribute -> Ignored
  if AP <> nil then
    Attr := AP.GetAttribute(JsonIgnoreAttribute, False)
  else
    Attr := TJsonInlineAttributes.GetAttribute(aRttiMember, JsonIgnoreAttribute);
  if Attr <> nil then
    aProperty.Ignored := True;

  // JsonConverterAttribute -> Converter
  if AP <> nil then
    Attr := AP.GetAttribute(JsonConverterAttribute, False)
  else
    Attr := TJsonInlineAttributes.GetAttribute(aRttiMember, JsonConverterAttribute);
  if Attr <> nil then
    aProperty.Converter := GetConverterFromAttribute(JsonConverterAttribute(Attr));

  // JsonObjectHandlingAttribute
  if AP <> nil then
    Attr := AP.GetAttribute(JsonObjectHandlingAttribute, False)
  else
    Attr := TJsonInlineAttributes.GetAttribute(aRttiMember, JsonObjectHandlingAttribute);
  if Attr <> nil then
    aProperty.ObjectHandling := JsonObjectHandlingAttribute(Attr).Value;

  // JsonObjectOwnership
  if AP <> nil then
    Attr := AP.GetAttribute(JsonObjectOwnership, False)
  else
    Attr := TJsonInlineAttributes.GetAttribute(aRttiMember, JsonObjectOwnership);
  if Attr <> nil then
    aProperty.ObjectOwnership := JsonObjectOwnership(Attr).Value;
end;

procedure TJsonDefaultContractResolver.SetPropertySpecialValues(const aProperty: TJsonProperty; const aRttiMember: TRttiMember;
  aValueSerialization: TJsonValueSerialization);
begin
  case aValueSerialization of
    TJsonValueSerialization.ExcludeDefault:
      if aProperty.TypeInf <> nil then
        case aProperty.TypeInf^.Kind of
          tkInteger, tkInt64, tkQWord: aProperty.DefaultValue := 0;
          tkFloat: aProperty.DefaultValue := 0.0;
          tkSString, tkAString, tkUString, tkLString, tkWString: aProperty.DefaultValue := '';
          tkBool, tkEnumeration: aProperty.DefaultValue := 0;
        end;
  end;
end;

function TJsonDefaultContractResolver.CreateValueProvider(const aRttiMember: TRttiMember): IJsonValueProvider;
begin
  if aRttiMember is TRttiField then
    Result := TJsonFieldValueProvider.Create(TRttiField(aRttiMember))
  else if aRttiMember is TRttiProperty then
    Result := TJsonPropertyValueProvider.Create(TRttiProperty(aRttiMember))
  else
    Result := TJsonValueProvider.Create;
end;

function TJsonDefaultContractResolver.CreateAttributeProvider(const aRttiObject: TRttiObject): IJsonAttributeProvider;
begin
  Result := TJsonInlineAttributeProvider.Create(aRttiObject);
end;

function TJsonDefaultContractResolver.ResolvePropertyName(const aName: string): string;
begin
  Result := aName;
end;

class procedure TJsonDefaultContractResolver.EnsureRttiCtx;
begin
  if not FSharedRttiCtxInitialized then
  begin
    FSharedRttiCtx := TRttiContext.Create;
    FSharedRttiCtxInitialized := True;
  end;
end;

constructor TJsonDefaultContractResolver.Create(ADefaultMemberSerialization: TJsonMemberSerialization;
  aDefaultValueSerialization: TJsonValueSerialization);
begin
  inherited Create;
  EnsureRttiCtx;
  FDefaultMemberSerialization := ADefaultMemberSerialization;
  FDefaultValueSerialization := aDefaultValueSerialization;
  FCachedContracts := specialize TObjectDictionary<PTypeInfo, TJsonContract>.Create([doOwnsValues]);
  FCachedAttributeConverters := specialize TDictionary<TJsonConverterClass, TJsonConverter>.Create;
end;

destructor TJsonDefaultContractResolver.Destroy;
begin
  FCachedAttributeConverters.Free;
  FCachedContracts.Free;
  FListHelperConverter.Free;
  FGUIDConverter.Free;
  inherited Destroy;
end;

procedure TJsonDefaultContractResolver.ClearCache;
begin
  FCachedContracts.Clear;
end;

function TJsonDefaultContractResolver.ResolveContract(ATypeInf: PTypeInfo): TJsonContract;
begin
  if not FCachedContracts.TryGetValue(ATypeInf, Result) then
  begin
    Result := CreateContract(ATypeInf);
    FCachedContracts.Add(ATypeInf, Result);
  end;
end;

{ TJsonDynamicContractResolver }

function TJsonDynamicContractResolver.CreateAttributeProvider(const aRttiObject: TRttiObject): IJsonAttributeProvider;
begin
  Result := TJsonDynamicAttributeProvider.Create(aRttiObject, FDynamicAttributes);
end;

constructor TJsonDynamicContractResolver.Create(ADefaultMemberSerialization: TJsonMemberSerialization;
  aDefaultValueSerialization: TJsonValueSerialization);
begin
  inherited Create(ADefaultMemberSerialization, aDefaultValueSerialization);
  FDynamicAttributes := TJsonDynamicAttributes.Create;
end;

destructor TJsonDynamicContractResolver.Destroy;
begin
  FDynamicAttributes.Free;
  inherited Destroy;
end;

procedure TJsonDynamicContractResolver.ClearAttributes;
begin
  FDynamicAttributes.Clear;
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetFieldConverter(ATypeInf: PTypeInfo; const aFieldName: string;
  const aConverterClass: TJsonConverterClass);
var
  RttiType: TRttiType;
  Field: TRttiField;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  Field := RttiType.GetField(aFieldName);
  if Field = nil then
    raise EJsonSerializationException.CreateFmt(SCannotFindFieldForType, [aFieldName, ATypeInf^.Name]);
  FDynamicAttributes.addAttribute(Field, JsonConverterAttribute.Create(aConverterClass));
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetFieldName(ATypeInf: PTypeInfo; const aFieldName: string; const aResolvedName: string);
var
  RttiType: TRttiType;
  Field: TRttiField;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  Field := RttiType.GetField(aFieldName);
  if Field = nil then
    raise EJsonSerializationException.CreateFmt(SCannotFindFieldForType, [aFieldName, ATypeInf^.Name]);
  FDynamicAttributes.addAttribute(Field, JsonNameAttribute.Create(aResolvedName));
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetFieldsIgnored(ATypeInf: PTypeInfo; const aFieldNames: array of string);
var
  RttiType: TRttiType;
  Field: TRttiField;
  I: Integer;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  for I := 0 to High(aFieldNames) do
  begin
    Field := RttiType.GetField(aFieldNames[I]);
    if Field <> nil then
      FDynamicAttributes.addAttribute(Field, JsonIgnoreAttribute.Create);
  end;
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetFieldsIn(ATypeInf: PTypeInfo; const aFieldNames: array of string);
var
  RttiType: TRttiType;
  Field: TRttiField;
  I: Integer;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  for I := 0 to High(aFieldNames) do
  begin
    Field := RttiType.GetField(aFieldNames[I]);
    if Field <> nil then
      FDynamicAttributes.addAttribute(Field, JsonInAttribute.Create);
  end;
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetPropertiesIgnored(ATypeInf: PTypeInfo; const aPropertyNames: array of string);
var
  RttiType: TRttiType;
  Prop: TRttiProperty;
  I: Integer;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  for I := 0 to High(aPropertyNames) do
  begin
    Prop := RttiType.GetProperty(aPropertyNames[I]);
    if Prop <> nil then
      FDynamicAttributes.addAttribute(Prop, JsonIgnoreAttribute.Create);
  end;
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetPropertiesIn(ATypeInf: PTypeInfo; const aPropertyNames: array of string);
var
  RttiType: TRttiType;
  Prop: TRttiProperty;
  I: Integer;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  for I := 0 to High(aPropertyNames) do
  begin
    Prop := RttiType.GetProperty(aPropertyNames[I]);
    if Prop <> nil then
      FDynamicAttributes.addAttribute(Prop, JsonInAttribute.Create);
  end;
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetPropertyConverter(ATypeInf: PTypeInfo; const aPropertyName: string;
  const aConverterClass: TJsonConverterClass);
var
  RttiType: TRttiType;
  Prop: TRttiProperty;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  Prop := RttiType.GetProperty(aPropertyName);
  if Prop = nil then
    raise EJsonSerializationException.CreateFmt(SCannotFindPropertyForType, [aPropertyName, ATypeInf^.Name]);
  FDynamicAttributes.addAttribute(Prop, JsonConverterAttribute.Create(aConverterClass));
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetPropertyName(ATypeInf: PTypeInfo; const aPropertyName: string; const aResolvedName: string
  );
var
  RttiType: TRttiType;
  Prop: TRttiProperty;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType = nil then Exit;
  Prop := RttiType.GetProperty(aPropertyName);
  if Prop = nil then
    raise EJsonSerializationException.CreateFmt(SCannotFindPropertyForType, [aPropertyName, ATypeInf^.Name]);
  FDynamicAttributes.addAttribute(Prop, JsonNameAttribute.Create(aResolvedName));
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetTypeConverter(ATypeInf: PTypeInfo; const aConverterClass: TJsonConverterClass);
var
  RttiType: TRttiType;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType <> nil then
    FDynamicAttributes.addAttribute(RttiType, JsonConverterAttribute.Create(aConverterClass));
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetTypeMemberSerialization(ATypeInf: PTypeInfo;
  const aMemberSerialization: TJsonMemberSerialization);
var
  RttiType: TRttiType;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType <> nil then
    FDynamicAttributes.addAttribute(RttiType, JsonSerializeAttribute.Create(aMemberSerialization));
  ClearCache;
end;

procedure TJsonDynamicContractResolver.SetTypeIgnored(ATypeInf: PTypeInfo);
var
  RttiType: TRttiType;
begin
  RttiType := FSharedRttiCtx.GetType(ATypeInf);
  if RttiType <> nil then
    FDynamicAttributes.addAttribute(RttiType, JsonIgnoreAttribute.Create);
  ClearCache;
end;

{ TJsonDefaultTypeResolver }

function TJsonDefaultTypeResolver.ResolveType(const aTypeName: string): PTypeInfo;
var
  C: TPersistentClass;
begin
  Result := nil;
  C := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Classes.GetClass(aTypeName);
  if C <> nil then
    Result := C.ClassInfo;
end;

function TJsonDefaultTypeResolver.ResolveName(const aTypeInf: PTypeInfo): string;
begin
  Result := aTypeInf^.Name;
end;

{ TJsonDefaultReferenceResolver }

function TJsonDefaultReferenceResolver.CreateContext(AWriteJson: Boolean; aMode: TJsonReferenceHandling): TObject;
begin
  Result := TContext.Create(AWriteJson, aMode);
end;

procedure TJsonDefaultReferenceResolver.addReference(const aContext: TObject; const aReference: string; const aValue: TObject);
begin
  TContext(aContext).FNameRefDict.Add(aReference, aValue);
end;

function TJsonDefaultReferenceResolver.IsReferenced(const aContext: TObject; aValue: TObject): Boolean;
begin
  Result := TContext(aContext).FRefNameDict.ContainsKey(aValue);
end;

function TJsonDefaultReferenceResolver.GetReference(const aContext: TObject; aValue: TObject; out aExisting: Boolean): string;
var
  Ctx: TContext;
begin
  Ctx := TContext(aContext);
  if Ctx.FRefNameDict.TryGetValue(aValue, Result) then
    aExisting := True
  else
  begin
    Inc(Ctx.FReferenceCount);
    Result := IntToStr(Ctx.FReferenceCount);
    Ctx.FRefNameDict.Add(aValue, Result);
    aExisting := False;
  end;
end;

function TJsonDefaultReferenceResolver.ResolveReference(const aContext: TObject; const aReference: string): TObject;
begin
  TContext(aContext).FNameRefDict.TryGetValue(aReference, Result);
end;

function TJsonDefaultReferenceResolver.PushReference(const aContext: TObject; aValue: TObject): Boolean;
begin
  Result := not TContext(aContext).FRefSet.Add(aValue);
end;

procedure TJsonDefaultReferenceResolver.PopReference(const aContext: TObject; aValue: TObject);
begin
  TContext(aContext).FRefSet.Remove(aValue);
end;

{ TJsonDefaultReferenceResolver.TContext }

constructor TJsonDefaultReferenceResolver.TContext.Create(AWriteJson: Boolean; aMode: TJsonReferenceHandling);
begin
  inherited Create;
  FMode := aMode;
  FReferenceCount := 0;
  FRefSet := specialize THashSet<TObject>.Create;
  if AWriteJson then
    FRefNameDict := specialize TDictionary<TObject, string>.Create
  else
    FNameRefDict := specialize TDictionary<string, TObject>.Create;
end;

destructor TJsonDefaultReferenceResolver.TContext.Destroy;
begin
  FRefSet.Free;
  FRefNameDict.Free;
  FNameRefDict.Free;
  inherited Destroy;
end;

{ TJsonSerializer }

function TJsonSerializer.GetContractResolver: IJsonContractResolver;
begin
  Result := FContractResolver;
end;

function TJsonSerializer.GetConverters: TJsonConverterList;
begin
  Result := FConverters;
end;

function TJsonSerializer.GetDateFormatHandling: TJsonDateFormatHandling;
begin
  Result := FDateFormatHandling;
end;

function TJsonSerializer.GetDateParseHandling: TJsonDateParseHandling;
begin
  Result := FDateParseHandling;
end;

function TJsonSerializer.GetDateTimeZoneHandling: TJsonDateTimeZoneHandling;
begin
  Result := FDateTimeZoneHandling;
end;

function TJsonSerializer.GetFloatFormatHandling: TJsonFloatFormatHandling;
begin
  Result := FFloatFormatHandling;
end;

function TJsonSerializer.GetFormatting: TJsonFormatting;
begin
  Result := FFormatting;
end;

function TJsonSerializer.GetMaxDepth: Integer;
begin
  Result := FMaxDepth;
end;

function TJsonSerializer.GetReferenceResolver: IJsonReferenceResolver;
begin
  Result := FReferenceResolver;
end;

function TJsonSerializer.GetObjectHandling: TJsonObjectHandling;
begin
  Result := FObjectHandling;
end;

function TJsonSerializer.GetObjectOwnership: TJsonObjectOwnership;
begin
  Result := FObjectOwnership;
end;

function TJsonSerializer.GetStringEscapeHandling: TJsonStringEscapeHandling;
begin
  Result := FStringEscapeHandling;
end;

function TJsonSerializer.GetTypeResolver: IJsonTypeResolver;
begin
  Result := FTypeResolver;
end;

function TJsonSerializer.GetReferenceHandling: TJsonReferenceHandling;
begin
  Result := FReferenceHandling;
end;

function TJsonSerializer.GetValueSerialization: TJsonValueSerialization;
begin
  Result := FValueSerialization;
end;

function TJsonSerializer.GetMemberSerialization: TJsonMemberSerialization;
begin
  Result := FMemberSerialization;
end;

procedure TJsonSerializer.SetContractResolver(const aValue: IJsonContractResolver);
begin
  FContractResolver := aValue;
end;

procedure TJsonSerializer.SetDateFormatHandling(aValue: TJsonDateFormatHandling);
begin
  FDateFormatHandling := aValue;
end;

procedure TJsonSerializer.SetDateParseHandling(aValue: TJsonDateParseHandling);
begin
  FDateParseHandling := aValue;
end;

procedure TJsonSerializer.SetDateTimeZoneHandling(aValue: TJsonDateTimeZoneHandling);
begin
  FDateTimeZoneHandling := aValue;
end;

procedure TJsonSerializer.SetFloatFormatHandling(aValue: TJsonFloatFormatHandling);
begin
  FFloatFormatHandling := aValue;
end;

procedure TJsonSerializer.SetFormatting(aValue: TJsonFormatting);
begin
  FFormatting := aValue;
end;

procedure TJsonSerializer.SetMaxDepth(aValue: Integer);
begin
  FMaxDepth := aValue;
end;

procedure TJsonSerializer.SetObjectHandling(const aValue: TJsonObjectHandling);
begin
  FObjectHandling := aValue;
end;

procedure TJsonSerializer.SetObjectOwnership(const aValue: TJsonObjectOwnership);
begin
  FObjectOwnership := aValue;
end;

procedure TJsonSerializer.SetReferenceResolver(const aValue: IJsonReferenceResolver);
begin
  FReferenceResolver := aValue;
end;

procedure TJsonSerializer.SetStringEscapeHandling(aValue: TJsonStringEscapeHandling);
begin
  FStringEscapeHandling := aValue;
end;


procedure TJsonSerializer.SetTypeResolver(const aValue: IJsonTypeResolver);
begin
  FTypeResolver := aValue;
end;

procedure TJsonSerializer.SetReferenceHandling(const aValue: TJsonReferenceHandling);
begin
  FReferenceHandling := aValue;
end;

procedure TJsonSerializer.SetValueSerialization(const aValue: TJsonValueSerialization);
begin
  FValueSerialization := aValue;
end;

procedure TJsonSerializer.SetMemberSerialization(const aValue: TJsonMemberSerialization);
begin
  FMemberSerialization := aValue;
end;

{ TJsonSerializer - Serialize helpers }

procedure TJsonSerializer.SerializePrimitive(const aWriter: TJsonWriter; const aValue: TValue; aContract: TJsonPrimitiveContract);
begin
  case aContract.Kind of
    TJsonPrimitiveKind.Int8, TJsonPrimitiveKind.Int16, TJsonPrimitiveKind.Int32,
    TJsonPrimitiveKind.UInt8, TJsonPrimitiveKind.UInt16, TJsonPrimitiveKind.UInt32:
      aWriter.WriteValue(aValue.AsInteger);
    TJsonPrimitiveKind.Int64:
      aWriter.WriteValue(aValue.AsInt64);
    TJsonPrimitiveKind.UInt64:
      aWriter.WriteValue(aValue.AsUInt64);
    TJsonPrimitiveKind.&Single:
      aWriter.WriteValue(Single(aValue.AsExtended));
    TJsonPrimitiveKind.&Double:
      aWriter.WriteValue(Double(aValue.AsExtended));
    TJsonPrimitiveKind.&Extended:
      aWriter.WriteValue(aValue.AsExtended);
    TJsonPrimitiveKind.&Comp, TJsonPrimitiveKind.&Currency:
      aWriter.WriteValue(aValue.AsExtended);
    TJsonPrimitiveKind.&String:
      aWriter.WriteValue(aValue.AsString);
    TJsonPrimitiveKind.&Char:
      aWriter.WriteValue(string(Chr(aValue.AsOrdinal)));
    TJsonPrimitiveKind.&Boolean:
      aWriter.WriteValue(aValue.AsBoolean);
    TJsonPrimitiveKind.Enumeration:
      aWriter.WriteValue(GetEnumName(aValue.TypeInfo, aValue.AsOrdinal));
    TJsonPrimitiveKind.&Set:
      begin
        // Sets aren't ordinal — extract raw integer from TValue data
        case aValue.DataSize of
          1: aWriter.WriteValue(Integer(PByte(aValue.GetReferenceToRawData)^));
          2: aWriter.WriteValue(Integer(PWord(aValue.GetReferenceToRawData)^));
          4: aWriter.WriteValue(Integer(PLongWord(aValue.GetReferenceToRawData)^));
        else
          aWriter.WriteValue(Int64(PInt64(aValue.GetReferenceToRawData)^));
        end;
      end;
    TJsonPrimitiveKind.DateTime:
      aWriter.WriteValue(TDateTime(aValue.AsExtended));
  end;
end;

procedure TJsonSerializer.SerializeObject(const aWriter: TJsonWriter; const aValue: TValue; aContract: TJsonObjectContract);
var
  Prop: TJsonProperty;
  PropValue: TValue;
begin
  if aValue.IsObject and (aValue.AsObject = nil) then
  begin
    aWriter.WriteNull;
    Exit;
  end;
  aWriter.WriteStartObject;
  for Prop in aContract.Properties do
  begin
    if Prop.Ignored or not Prop.Readable then
      Continue;
    PropValue := Prop.ValueProvider.GetValue(aValue);
    aWriter.WritePropertyName(Prop.Name);
    InternalSerialize(aWriter, PropValue);
  end;
  aWriter.WriteEndObject;
end;

procedure TJsonSerializer.SerializeArray(const aWriter: TJsonWriter; const aValue: TValue; aContract: TJsonArrayContract);
var
  I, Len: Integer;
  ElemValue: TValue;
begin
  aWriter.WriteStartArray;
  Len := aValue.GetArrayLength;
  for I := 0 to Len - 1 do
  begin
    ElemValue := aValue.GetArrayElement(I);
    InternalSerialize(aWriter, ElemValue);
  end;
  aWriter.WriteEndArray;
end;

function TJsonSerializer.DeserializePrimitive(const aReader: TJsonReader; aContract: TJsonPrimitiveContract): TValue;
var
  TmpStr: string;
  SetVal: Integer;
begin
  case aContract.Kind of
    TJsonPrimitiveKind.Int8, TJsonPrimitiveKind.Int16, TJsonPrimitiveKind.Int32,
    TJsonPrimitiveKind.UInt8, TJsonPrimitiveKind.UInt16, TJsonPrimitiveKind.UInt32:
      begin
        TValue.Make(aReader.Value.AsInteger, aContract.TypeInf, Result);
      end;
    TJsonPrimitiveKind.Int64:
      Result := TValue.specialize From<Int64>(aReader.Value.AsInt64);
    TJsonPrimitiveKind.UInt64:
      Result := TValue.specialize From<UInt64>(aReader.Value.AsUInt64);
    TJsonPrimitiveKind.&Single:
      Result := TValue.specialize From<Single>(Single(aReader.Value.AsExtended));
    TJsonPrimitiveKind.&Double:
      Result := TValue.specialize From<Double>(Double(aReader.Value.AsExtended));
    TJsonPrimitiveKind.&Extended:
      Result := TValue.specialize From<Extended>(aReader.Value.AsExtended);
    TJsonPrimitiveKind.&Comp, TJsonPrimitiveKind.&Currency:
      Result := TValue.specialize From<Extended>(aReader.Value.AsExtended);
    TJsonPrimitiveKind.&String:
      Result := TValue.specialize From<string>(aReader.Value.AsString);
    TJsonPrimitiveKind.&Char:
      begin
        TmpStr := aReader.Value.AsString;
        if TmpStr <> '' then
          Result := TValue.specialize From<Char>(TmpStr[1])
        else
          Result := TValue.specialize From<Char>(#0);
      end;
    TJsonPrimitiveKind.&Boolean:
      Result := TValue.specialize From<Boolean>(aReader.Value.AsBoolean);
    TJsonPrimitiveKind.Enumeration:
      begin
        if aReader.TokenType = TJsonToken.&String then
          Result := TValue.FromOrdinal(aContract.TypeInf, GetEnumValue(aContract.TypeInf, aReader.Value.AsString))
        else
          Result := TValue.FromOrdinal(aContract.TypeInf, aReader.Value.AsOrdinal);
      end;
    TJsonPrimitiveKind.&Set:
      begin
        // Sets aren't ordinal — create TValue from raw integer data
        SetVal := aReader.Value.AsInteger;
        TValue.Make(@SetVal, aContract.TypeInf, Result);
      end;
    TJsonPrimitiveKind.DateTime:
      Result := TValue.specialize From<TDateTime>(TDateTime(aReader.Value.AsExtended));
  end;
end;

function TJsonSerializer.DeserializeObject(const aReader: TJsonReader; aContract: TJsonObjectContract): TValue;
var
  Prop: TJsonProperty;
  PropValue: TValue;
  PropName: string;
begin
  if aReader.TokenType = TJsonToken.Null then
  begin
    Result := TValue.Empty;
    Exit;
  end;
  if aContract.DefaultCreator <> nil then
    Result := aContract.DefaultCreator.Invoke([])
  else
    TValue.Make(nil, aContract.TypeInf, Result);

  if aReader.TokenType <> TJsonToken.StartObject then
    raise EJsonSerializationException.Create(SUnexpectedTokenDeserializeObject);

  while aReader.Read do
  begin
    case aReader.TokenType of
      TJsonToken.PropertyName:
        begin
          PropName := aReader.Value.AsString;
          Prop := aContract.GetClosestMatchProperty(PropName);
          if (Prop <> nil) and not Prop.Ignored and Prop.Writable then
          begin
            aReader.Read;
            PropValue := InternalDeserialize(aReader, Prop.TypeInf);
            Prop.ValueProvider.SetValue(Result, PropValue);
          end
          else
          begin
            // Skip unknown property value
            aReader.Read;
            aReader.Skip;
          end;
        end;
      TJsonToken.EndObject:
        Exit;
    else
      raise EJsonSerializationException.CreateFmt(SUnexpectedTokenDeserializeObject, [aReader.TokenType.ToString]);
    end;
  end;
  raise EJsonSerializationException.Create(SUnexpectedEndDeserializeObject);
end;

function TJsonSerializer.DeserializeArray(const aReader: TJsonReader; aContract: TJsonArrayContract): TValue;
var
  ElemList: specialize TList<TValue>;
  ElemTypeInf: PTypeInfo;
  ElemValue: TValue;
  ArrLen, I: Integer;
begin
  if aReader.TokenType <> TJsonToken.StartArray then
    raise EJsonSerializationException.Create(SUnexpectedTokenPopulateArray);

  ElemTypeInf := aContract.arrayType;
  ElemList := specialize TList<TValue>.Create;
  try
    while aReader.Read do
    begin
      if aReader.TokenType = TJsonToken.EndArray then
        Break;
      ElemValue := InternalDeserialize(aReader, ElemTypeInf);
      ElemList.Add(ElemValue);
    end;
    // Build dynamic array TValue
    ArrLen := ElemList.Count;
    Result := TValue.FromArray(aContract.TypeInf, ElemList.ToArray);
  finally
    ElemList.Free;
  end;
end;

{ TJsonSerializer - Internal core }

procedure TJsonSerializer.InternalSerialize(const aWriter: TJsonWriter; const aValue: TValue);
var
  Contract: TJsonContract;
  Conv: TJsonConverter;
begin
  if aValue.IsEmpty then
  begin
    aWriter.WriteNull;
    Exit;
  end;
  Contract := ContractResolver.ResolveContract(aValue.TypeInfo);
  // Check for converter
  Conv := Contract.Converter;
  if Conv = nil then
    Conv := MatchConverter(FConverters, aValue.TypeInfo);
  if (Conv <> nil) and Conv.CanWrite(aValue) then
  begin
    Conv.WriteJson(aWriter, aValue, Self);
    Exit;
  end;
  case Contract.ContractType of
    TJsonContractType.Primitive:
      SerializePrimitive(aWriter, aValue, TJsonPrimitiveContract(Contract));
    TJsonContractType.&Object:
      SerializeObject(aWriter, aValue, TJsonObjectContract(Contract));
    TJsonContractType.&Array:
      SerializeArray(aWriter, aValue, TJsonArrayContract(Contract));
    TJsonContractType.&Class:
      SerializeObject(aWriter, aValue, TJsonObjectContract(Contract));
    TJsonContractType.Converter:
      if Contract.Converter <> nil then
        Contract.Converter.WriteJson(aWriter, aValue, Self)
      else
        aWriter.WriteNull;
  end;
end;

function TJsonSerializer.InternalDeserialize(const aReader: TJsonReader; aTypeInf: PTypeInfo): TValue;
var
  Contract: TJsonContract;
  Conv: TJsonConverter;
begin
  if aReader.TokenType = TJsonToken.None then
    aReader.Read;

  if aReader.TokenType = TJsonToken.Null then
  begin
    Result := TValue.Empty;
    Exit;
  end;

  Contract := ContractResolver.ResolveContract(aTypeInf);
  // Check for converter
  Conv := Contract.Converter;
  if Conv = nil then
    Conv := MatchConverter(FConverters, aTypeInf);
  if (Conv <> nil) and Conv.CanRead then
  begin
    Result := Conv.ReadJson(aReader, aTypeInf, TValue.Empty, Self);
    Exit;
  end;
  case Contract.ContractType of
    TJsonContractType.Primitive:
      Result := DeserializePrimitive(aReader, TJsonPrimitiveContract(Contract));
    TJsonContractType.&Object:
      Result := DeserializeObject(aReader, TJsonObjectContract(Contract));
    TJsonContractType.&Array:
      Result := DeserializeArray(aReader, TJsonArrayContract(Contract));
    TJsonContractType.&Class:
      Result := DeserializeObject(aReader, TJsonObjectContract(Contract));
    TJsonContractType.Converter:
      if Contract.Converter <> nil then
        Result := Contract.Converter.ReadJson(aReader, aTypeInf, TValue.Empty, Self)
      else
        Result := TValue.Empty;
  end;
end;

procedure TJsonSerializer.InternalPopulate(const Reader: TJsonReader; var aValue: TValue; aUseConverter: Boolean);
var
  Contract: TJsonContract;
  ObjContract: TJsonObjectContract;
  Conv: TJsonConverter;
  Prop: TJsonProperty;
  PropValue: TValue;
  PropName: string;
begin
  if aValue.IsEmpty then
    Exit;

  Contract := ContractResolver.ResolveContract(aValue.TypeInfo);

  if aUseConverter then
  begin
    Conv := Contract.Converter;
    if Conv = nil then
      Conv := MatchConverter(FConverters, aValue.TypeInfo);
    if (Conv <> nil) and Conv.CanRead then
    begin
      aValue := Conv.ReadJson(Reader, aValue.TypeInfo, aValue, Self);
      Exit;
    end;
  end;

  if not (Contract is TJsonObjectContract) then
    Exit;
  ObjContract := TJsonObjectContract(Contract);

  if Reader.TokenType = TJsonToken.None then
    Reader.Read;
  if Reader.TokenType <> TJsonToken.StartObject then
    raise EJsonSerializationException.Create(SUnexpectedTokenPopulateObject);

  while Reader.Read do
  begin
    case Reader.TokenType of
      TJsonToken.PropertyName:
        begin
          PropName := Reader.Value.AsString;
          Prop := ObjContract.GetClosestMatchProperty(PropName);
          if (Prop <> nil) and not Prop.Ignored and Prop.Writable then
          begin
            Reader.Read;
            PropValue := InternalDeserialize(Reader, Prop.TypeInf);
            Prop.ValueProvider.SetValue(aValue, PropValue);
          end
          else
          begin
            Reader.Read;
            Reader.Skip;
          end;
        end;
      TJsonToken.EndObject:
        Exit;
    end;
  end;
end;

constructor TJsonSerializer.Create;
begin
  inherited Create;
  FConverters := specialize TObjectList<TJsonConverter>.Create(True);
  TJsonConverterRegistry.PopulateConverters(FConverters);
  FContractResolver := TJsonDefaultContractResolver.Create;
  FMaxDepth := 64;
  FFormatting := TJsonFormatting.None;
  FMemberSerialization := TJsonMemberSerialization.Fields;
  FValueSerialization := TJsonValueSerialization.ExcludeSpecial;
  FDateFormatHandling := TJsonDateFormatHandling.Iso;
  FDateParseHandling := TJsonDateParseHandling.DateTime;
  FDateTimeZoneHandling := TJsonDateTimeZoneHandling.Local;
  FFloatFormatHandling := TJsonFloatFormatHandling.&String;
  FStringEscapeHandling := TJsonStringEscapeHandling.Default;
  FObjectHandling := TJsonObjectHandling.Auto;
  FObjectOwnership := TJsonObjectOwnership.Auto;
  FReferenceHandling := TJsonReferenceHandling.None;
end;

destructor TJsonSerializer.Destroy;
begin
  FConverters.Free;
  inherited Destroy;
end;

generic function TJsonSerializer.Serialize<T>(const aValue: T): string;
var
  SW: TStringWriter;
  JW: TJsonTextWriter;
begin
  SW := TStringWriter.Create;
  try
    JW := TJsonTextWriter.Create(SW, False);
    try
      JW.Formatting := FFormatting;
      JW.StringEscapeHandling := FStringEscapeHandling;
      JW.DateTimeZoneHandling := FDateTimeZoneHandling;
      specialize Serialize<T>(JW, aValue);
      JW.Flush;
      Result := SW.ToString;
    finally
      JW.Free;
    end;
  finally
    SW.Free;
  end;
end;

generic procedure TJsonSerializer.Serialize<T>(const aWriter: TTextWriter; const aValue: T);
var
  JW: TJsonTextWriter;
begin
  JW := TJsonTextWriter.Create(aWriter, False);
  try
    JW.Formatting := FFormatting;
    JW.StringEscapeHandling := FStringEscapeHandling;
    JW.DateTimeZoneHandling := FDateTimeZoneHandling;
    specialize Serialize<T>(JW, aValue);
    JW.Flush;
  finally
    JW.Free;
  end;
end;

generic procedure TJsonSerializer.Serialize<T>(const aWriter: TJsonWriter; const aValue: T);
begin
  Serialize(aWriter, TValue.specialize From<T>(aValue));
end;

procedure TJsonSerializer.Serialize(const aWriter: TJsonWriter; const aValue: TValue);
begin
  InternalSerialize(aWriter, aValue);
end;

generic function TJsonSerializer.Deserialize<T>(const aJson: string): T;
var
  SR: TStringReader;
  JR: TJsonTextReader;
begin
  SR := TStringReader.Create(aJson);
  JR := TJsonTextReader.Create(SR);
  try
    Result := specialize Deserialize<T>(JR);
  finally
    JR.Free; // CloseInput frees SR
  end;
end;

generic function TJsonSerializer.Deserialize<T>(const aReader: TTextReader): T;
var
  JR: TJsonTextReader;
begin
  JR := TJsonTextReader.Create(aReader);
  JR.CloseInput := False; // Caller owns aReader
  try
    Result := specialize Deserialize<T>(JR);
  finally
    JR.Free;
  end;
end;

generic function TJsonSerializer.Deserialize<T>(const aReader: TJsonReader): T;
var
  V: TValue;
begin
  V := InternalDeserialize(aReader, TypeInfo(T));
  if V.IsEmpty then
    Result := Default(T)
  else
    Result := V.specialize AsType<T>;
end;

function TJsonSerializer.Deserialize(const aReader: TJsonReader; aTypeinfo: PTypeInfo): TValue;
begin
  Result := InternalDeserialize(aReader, aTypeinfo);
end;

generic procedure TJsonSerializer.Populate<T>(const aJson: string; var aValue: T);
var
  SR: TStringReader;
  JR: TJsonTextReader;
  V: TValue;
begin
  SR := TStringReader.Create(aJson);
  JR := TJsonTextReader.Create(SR);
  try
    V := TValue.specialize From<T>(aValue);
    InternalPopulate(JR, V, False);
    aValue := V.specialize AsType<T>;
  finally
    JR.Free; // CloseInput frees SR
  end;
end;

generic procedure TJsonSerializer.Populate<T>(const aReader: TTextReader; var aValue: T);
var
  JR: TJsonTextReader;
  V: TValue;
begin
  JR := TJsonTextReader.Create(aReader);
  JR.CloseInput := False; // Caller owns aReader
  try
    V := TValue.specialize From<T>(aValue);
    InternalPopulate(JR, V, False);
    aValue := V.specialize AsType<T>;
  finally
    JR.Free;
  end;
end;

generic procedure TJsonSerializer.Populate<T>(const aReader: TJsonReader; var aValue: T);
var
  V: TValue;
begin
  V := TValue.specialize From<T>(aValue);
  InternalPopulate(aReader, V, False);
  aValue := V.specialize AsType<T>;
end;

procedure TJsonSerializer.Populate(const aReader: TJsonReader; var aValue: TValue);
begin
  InternalPopulate(aReader, aValue, False);
end;

procedure TJsonSerializer.Populate(const aReader: TJsonReader; var aValue: TValue; aUseConverter: Boolean);
begin
  InternalPopulate(aReader, aValue, aUseConverter);
end;

class function TJsonSerializer.MatchConverter(const aConverters: TJsonConverterList; aTypeInf: PTypeInfo): TJsonConverter;
var
  Conv: TJsonConverter;
begin
  Result := nil;
  if aConverters = nil then
    Exit;
  for Conv in aConverters do
    if Conv.CanConvert(aTypeInf) then
      Exit(Conv);
end;

{ JsonConverterAttribute }

constructor JsonConverterAttribute.Create(const aValue: TJsonConverterClass);
begin
  inherited Create;
  FValue := aValue;
end;

end.
