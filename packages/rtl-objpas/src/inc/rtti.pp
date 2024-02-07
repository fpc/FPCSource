{
  This file is part of the Free Pascal run time library.
  Copyright (C) 2013 Joost van der Sluis joost@cnoc.nl
  member of the Free Pascal development team.

  Extended RTTI compatibility unit

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit Rtti;
{$ENDIF}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$goto on}
{$Assertions on}

{ Note: since the Lazarus IDE is not yet capable of correctly handling generic
  functions it is best to define a InLazIDE define inside the IDE that disables
  the generic code for CodeTools. To do this do this:

  - go to Tools -> Codetools Defines Editor
  - go to Edit -> Insert Node Below -> Define Recurse
  - enter the following values:
      Name: InLazIDE
      Description: Define InLazIDE everywhere
      Variable: InLazIDE
      Value from text: 1
}
{$ifdef InLazIDE}
{$define NoGenericMethods}
{$endif}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses
  Types,
  Classes,
  SysUtils,
  typinfo;
{$ENDIF FPC_DOTTEDUNITS}
type
  TRttiObject = class;
  TRttiType = class;
  TRttiMethod = class;
  TRttiProperty = class;
  TRttiInstanceType = class;

  TCustomAttributeClass = class of TCustomAttribute;
  TRttiClass = class of TRttiObject;

  TCustomAttributeArray = specialize TArray<TCustomAttribute>;

  TFunctionCallCallback = class
  protected
    function GetCodeAddress: CodePointer; virtual; abstract;
  public
    property CodeAddress: CodePointer read GetCodeAddress;
  end;

  TFunctionCallFlag = (
    fcfStatic
  );
  TFunctionCallFlags = set of TFunctionCallFlag;

  TFunctionCallParameterInfo = record
    ParamType: PTypeInfo;
    ParamFlags: TParamFlags;
    ParaLocs: PParameterLocations;
  end;

  IValueData = interface
  ['{1338B2F3-2C21-4798-A641-CA2BC5BF2396}']
    procedure ExtractRawData(ABuffer: pointer);
    procedure ExtractRawDataNoCopy(ABuffer: pointer);
    function GetDataSize: SizeInt;
    function GetReferenceToRawData: pointer;
  end;

  TValueData = record
    FTypeInfo: PTypeInfo;
    FValueData: IValueData;
    case integer of
      0:  (FAsUByte: Byte);
      1:  (FAsUWord: Word);
      2:  (FAsULong: LongWord);
      3:  (FAsObject: Pointer);
      4:  (FAsClass: TClass);
      5:  (FAsSByte: Shortint);
      6:  (FAsSWord: Smallint);
      7:  (FAsSLong: LongInt);
      8:  (FAsSingle: Single);
      9:  (FAsDouble: Double);
      10: (FAsExtended: Extended);
      11: (FAsComp: Comp);
      12: (FAsCurr: Currency);
      13: (FAsUInt64: QWord);
      14: (FAsSInt64: Int64);
      15: (FAsMethod: TMethod);
      16: (FAsPointer: Pointer);
      { FPC addition for open arrays }
      17: (FArrLength: SizeInt; FElSize: SizeInt);
  end;

  { TValue }

  TValue = record
  private
    FData: TValueData;
    function GetDataSize: SizeInt;
    function GetTypeDataProp: PTypeData; inline;
    function GetTypeInfo: PTypeInfo; inline;
    function GetTypeKind: TTypeKind; inline;
    function GetIsEmpty: boolean; inline;
    procedure Init; inline;
    // typecast
    procedure CastAssign(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastToVariant(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // from integer
    procedure CastIntegerToFloat(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastIntegerToInteger(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastIntegerToInt64(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastIntegerToQWord(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromInteger(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // from Ansichar
    procedure CastCharToString(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromAnsiChar(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From WideChar
    procedure CastWCharToString(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromWideChar(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From Enumerated
    procedure CastEnumToEnum(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromEnum(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From float
    procedure CastFloatToFloat(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFloatToInteger(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromFloat(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From string
    procedure CastStringToString(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromString(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From class
    procedure CastClassRefToClassRef(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastClassToClass(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastClassToInterface(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromClass(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From Int64
    procedure CastInt64ToFloat(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastInt64ToQWord(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastInt64ToInteger(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromInt64(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From QWord
    procedure CastQWordToFloat(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastQWordToInteger(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastQWordToInt64(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromQWord(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From Interface
    procedure CastInterfaceToInterface(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromInterface(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From Pointer
    procedure CastFromPointer(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From set
    procedure CastSetToSet(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromSet(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // From variant
    procedure CastVariantToVariant(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure CastFromVariant(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    procedure DoCastFromVariant(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
    // Cast entry
    procedure CastFromType(out aRes: Boolean; out ADest: TValue; aDestType: PTypeInfo);
  public
    class function Empty: TValue; static;
    class procedure Make(ABuffer: pointer; ATypeInfo: PTypeInfo; out result: TValue); static;
    class procedure Make(AValue: NativeInt; ATypeInfo: PTypeInfo; out Result: TValue); static; inline;
    { Note: a TValue based on an open array is only valid until the routine having the open array parameter is left! }
    class procedure MakeOpenArray(AArray: Pointer; ALength: SizeInt; ATypeInfo: PTypeInfo; out Result: TValue); static;
{$ifndef NoGenericMethods}
    generic class procedure Make<T>(const AValue: T; out Result: TValue); static; inline;
    generic class function From<T>(constref aValue: T): TValue; static; inline;
    { Note: a TValue based on an open array is only valid until the routine having the open array parameter is left! }
    generic class function FromOpenArray<T>(constref aValue: array of T): TValue; static; inline;
{$endif}
    class function FromOrdinal(aTypeInfo: PTypeInfo; aValue: Int64): TValue; static; {inline;}
    class function FromArray(aArrayTypeInfo: PTypeInfo; const aValues: array of TValue): TValue; static;
    class function FromVarRec(const aValue: TVarRec): TValue; static;
    class function FromVariant(const aValue : Variant) : TValue; static;
    function IsArray: boolean; inline;
    function IsOpenArray: Boolean; inline;
    // Maybe we need to check these now that Cast<> is implemented.
    // OTOH they will probablu be faster.
    function AsString: string; inline;
    function AsUnicodeString: UnicodeString;
    function AsAnsiString: AnsiString;
    function AsExtended: Extended;
    function IsClass: boolean; inline;
    function AsClass: TClass;
    function IsObject: boolean; inline;
    function AsObject: TObject;
    function IsOrdinal: boolean; inline;
    function AsOrdinal: Int64;
    function AsBoolean: boolean;
    function AsCurrency: Currency;
    function AsSingle : Single;
    function AsDateTime : TDateTime;
    function IsDateTime: boolean; inline;
    function AsDouble : Double;
    function AsInteger: Integer;
    function AsError: HRESULT;
    function AsChar: AnsiChar; inline;
    function AsAnsiChar: AnsiChar;
    function AsWideChar: WideChar;
    function AsInt64: Int64;
    function AsUInt64: QWord;
    function AsInterface: IInterface;
    function AsPointer : Pointer;
    function AsVariant : Variant;
    function ToString: String;
    function GetArrayLength: SizeInt;
    function GetArrayElement(AIndex: SizeInt): TValue;
    procedure SetArrayElement(AIndex: SizeInt; constref AValue: TValue);
    function IsType(aTypeInfo: PTypeInfo): boolean; inline;
    function IsInstanceOf(aClass : TClass): boolean; inline;
    function TryCast(aTypeInfo: PTypeInfo; out aResult: TValue; const aEmptyAsAnyType: Boolean = True): Boolean;
    function Cast(aTypeInfo: PTypeInfo; const aEmptyAsAnyType: Boolean = True): TValue; overload;
{$ifndef NoGenericMethods}
    generic function Cast<T>(const aEmptyAsAnyType: Boolean = True): TValue; overload;
    generic function IsType<T>: Boolean; inline;
    generic function AsType<T>(const aEmptyAsAnyType: Boolean = True): T;
    generic function TryAsType<T>(out aResult: T; const aEmptyAsAnyType: Boolean = True): Boolean; inline;
{$endif}
    function TryAsOrdinal(out AResult: int64): boolean;
    function GetReferenceToRawData: Pointer;
    procedure ExtractRawData(ABuffer: Pointer);
    procedure ExtractRawDataNoCopy(ABuffer: Pointer);
    class operator := (const AValue: ShortString): TValue; inline;
    class operator := (const AValue: AnsiString): TValue; inline;
    class operator := (const AValue: UnicodeString): TValue; inline;
    class operator := (const AValue: WideString): TValue; inline;
    class operator := (AValue: LongInt): TValue; inline;
    class operator := (AValue: SmallInt): TValue; inline;
    class operator := (AValue: ShortInt): TValue; inline;
    class operator := (AValue: Byte): TValue; inline;
    class operator := (AValue: Word): TValue; inline;
    class operator := (AValue: Cardinal): TValue; inline;
    class operator := (AValue: Single): TValue; inline;
    class operator := (AValue: Double): TValue; inline;
{$ifdef FPC_HAS_TYPE_EXTENDED}
    class operator := (AValue: Extended): TValue; inline;
{$endif}
    class operator := (AValue: Currency): TValue; inline;
    class operator := (AValue: Comp): TValue; inline;
    class operator := (AValue: Int64): TValue; inline;
    class operator := (AValue: QWord): TValue; inline;
    class operator := (AValue: TObject): TValue; inline;
    class operator := (AValue: TClass): TValue; inline;
    class operator := (AValue: Boolean): TValue; inline;
    class operator := (AValue: IUnknown): TValue; inline;
    class operator := (AValue: TVarRec): TValue; inline;
    property DataSize: SizeInt read GetDataSize;
    property Kind: TTypeKind read GetTypeKind;
    property TypeData: PTypeData read GetTypeDataProp;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property IsEmpty: boolean read GetIsEmpty;
  end;
  PValue = ^TValue;
  TValueArray = specialize TArray<TValue>;

  { TRttiContext }

  TRttiContext = record
  private
    FContextToken: IInterface;
    function GetByHandle(AHandle: Pointer): TRttiObject;
    procedure AddObject(AObject: TRttiObject);
  public
    class function Create: TRttiContext; static;
    procedure  Free;
    function GetType(ATypeInfo: PTypeInfo): TRttiType;
    function GetType(AClass: TClass): TRttiType;
    //function GetTypes: specialize TArray<TRttiType>;
  end;

  { TRttiObject }

  TRttiObject = class abstract
  protected
    function GetHandle: Pointer; virtual; abstract;
  public
    function HasAttribute(aClass: TCustomAttributeClass): Boolean;
    function GetAttribute(aClass: TCustomAttributeClass): TCustomAttribute;
    generic function GetAttribute<T>: T;
    generic function HasAttribute<T>: Boolean;
    function GetAttributes: TCustomAttributeArray; virtual; abstract;
    property Handle: Pointer read GetHandle;
  end;

  { TRttiNamedObject }

  TRttiNamedObject = class(TRttiObject)
  protected
    function GetName: string; virtual;
  public
    function HasName(const aName: string): Boolean;
    property Name: string read GetName;
  end;

  { TRttiType }

  TRttiType = class(TRttiNamedObject)
  private
    FTypeInfo: PTypeInfo;
    FAttributesResolved: boolean;
    FAttributes: TCustomAttributeArray;
    FMethods: specialize TArray<TRttiMethod>;
    function GetAsInstance: TRttiInstanceType;
  protected
    FTypeData: PTypeData;
    function GetName: string; override;
    function GetHandle: Pointer; override;
    function GetIsInstance: boolean; virtual;
    function GetIsManaged: boolean; virtual;
    function GetIsOrdinal: boolean; virtual;
    function GetIsRecord: boolean; virtual;
    function GetIsSet: boolean; virtual;
    function GetTypeKind: TTypeKind; virtual;
    function GetTypeSize: integer; virtual;
    function GetBaseType: TRttiType; virtual;
  public
    constructor Create(ATypeInfo : PTypeInfo);
    destructor Destroy; override;
    function GetAttributes: TCustomAttributeArray; override;
    function GetProperties: specialize TArray<TRttiProperty>; virtual;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    function GetMethods: specialize TArray<TRttiMethod>; virtual;
    function GetMethod(const aName: String): TRttiMethod; virtual;
    function GetDeclaredMethods: specialize TArray<TRttiMethod>; virtual;
    property IsInstance: boolean read GetIsInstance;
    property isManaged: boolean read GetIsManaged;
    property IsOrdinal: boolean read GetIsOrdinal;
    property IsRecord: boolean read GetIsRecord;
    property IsSet: boolean read GetIsSet;
    property BaseType: TRttiType read GetBaseType;
    property Handle: PTypeInfo read FTypeInfo;
    property AsInstance: TRttiInstanceType read GetAsInstance;
    property TypeKind: TTypeKind read GetTypeKind;
    property TypeSize: integer read GetTypeSize;
  end;

  { TRttiFloatType }

  TRttiFloatType = class(TRttiType)
  private
    function GetFloatType: TFloatType; inline;
  protected
    function GetTypeSize: integer; override;
  public
    property FloatType: TFloatType read GetFloatType;
  end;

  TRttiOrdinalType = class(TRttiType)
  private
    function GetMaxValue: LongInt; inline;
    function GetMinValue: LongInt; inline;
    function GetOrdType: TOrdType; inline;
  protected
    function GetTypeSize: Integer; override;
  public
    property OrdType: TOrdType read GetOrdType;
    property MinValue: LongInt read GetMinValue;
    property MaxValue: LongInt read GetMaxValue;
  end;
  
  { TRttiEnumerationType }

  TRttiEnumerationType = class(TRttiOrdinalType)
  private
    function GetUnderlyingType: TRttiType;
  public
    function GetNames: TStringDynArray;
    generic class function GetName<T{: enum}>(AValue: T): string; reintroduce; static;
    generic class function GetValue<T{: enum}>(const AName: string): T; static;
    property UnderlyingType: TRttiType read GetUnderlyingType;
  end;
  

  TRttiInt64Type = class(TRttiType)
  private
    function GetMaxValue: Int64; inline;
    function GetMinValue: Int64; inline;
    function GetUnsigned: Boolean; inline;
  protected
    function GetTypeSize: integer; override;
  public
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
    property Unsigned: Boolean read GetUnsigned;
  end;

  TRttiStringKind = (skShortString, skAnsiString, skWideString, skUnicodeString);

  { TRttiStringType }

  TRttiStringType = class(TRttiType)
  private
    function GetStringKind: TRttiStringKind;
  public
    property StringKind: TRttiStringKind read GetStringKind;
  end;

  TRttiAnsiStringType = class(TRttiStringType)
  private
    function GetCodePage: Word;
  public
    property CodePage: Word read GetCodePage;
  end;

  TRttiPointerType = class(TRttiType)
  private
    function GetReferredType: TRttiType;
  public
    property ReferredType: TRttiType read GetReferredType;
  end;

  TRttiArrayType = class(TRttiType)
  private
    function GetDimensionCount: SizeUInt; inline;
    function GetDimension(aIndex: SizeInt): TRttiType; inline;
    function GetElementType: TRttiType; inline;
    function GetTotalElementCount: SizeInt; inline;
  public
    property DimensionCount: SizeUInt read GetDimensionCount;
    property Dimensions[Index: SizeInt]: TRttiType read GetDimension;
    property ElementType: TRttiType read GetElementType;
    property TotalElementCount: SizeInt read GetTotalElementCount;
  end;

  TRttiDynamicArrayType = class(TRttiType)
  private
    function GetDeclaringUnitName: String; inline;
    function GetElementSize: SizeUInt; inline;
    function GetElementType: TRttiType; inline;
    function GetOleAutoVarType: TVarType; inline;
  public
    property DeclaringUnitName: String read GetDeclaringUnitName;
    property ElementSize: SizeUInt read GetElementSize;
    property ElementType: TRttiType read GetElementType;
    property OleAutoVarType: TVarType read GetOleAutoVarType;
  end;

  { TRttiMember }

  TMemberVisibility=(mvPrivate, mvProtected, mvPublic, mvPublished);

  TRttiMember = class(TRttiNamedObject)
  private
    FParent: TRttiType;
  protected
    function GetVisibility: TMemberVisibility; virtual;
  public
    constructor Create(AParent: TRttiType);
    property Visibility: TMemberVisibility read GetVisibility;
    property Parent: TRttiType read FParent;
  end;

  { TRttiProperty }

  TRttiProperty = class(TRttiMember)
  private
    FPropInfo: PPropInfo;
    FAttributesResolved: boolean;
    FAttributes: TCustomAttributeArray;
    function GetPropertyType: TRttiType;
    function GetIsWritable: boolean;
    function GetIsReadable: boolean;
  protected
    function GetVisibility: TMemberVisibility; override;
    function GetName: string; override;
    function GetHandle: Pointer; override;
  public
    constructor Create(AParent: TRttiType; APropInfo: PPropInfo);
    destructor Destroy; override;
    function GetAttributes: TCustomAttributeArray; override;
    function GetValue(Instance: pointer): TValue;
    procedure SetValue(Instance: pointer; const AValue: TValue);
    property PropertyType: TRttiType read GetPropertyType;
    property IsReadable: boolean read GetIsReadable;
    property IsWritable: boolean read GetIsWritable;
    property Visibility: TMemberVisibility read GetVisibility;
  end;
  TRttiPropertyArray = specialize TArray<TRttiProperty>;

  TRttiParameter = class(TRttiNamedObject)
  private
    FString: String;
  protected
    function GetParamType: TRttiType; virtual; abstract;
    function GetFlags: TParamFlags; virtual; abstract;
  public
    property ParamType: TRttiType read GetParamType;
    property Flags: TParamFlags read GetFlags;
    function ToString: String; override;
  end;
  TRttiParameterArray = specialize TArray<TRttiParameter>;

  TMethodImplementationCallbackMethod = procedure(aUserData: Pointer; const aArgs: TValueArray; out aResult: TValue) of object;
  TMethodImplementationCallbackProc = procedure(aUserData: Pointer; const aArgs: TValueArray; out aResult: TValue);

  TMethodImplementation = class
  private
    fLowLevelCallback: TFunctionCallCallback;
    fCallbackProc: TMethodImplementationCallbackProc;
    fCallbackMethod: TMethodImplementationCallbackMethod;
    fArgs: specialize TArray<TFunctionCallParameterInfo>;
    fArgLen: SizeInt;
    fRefArgs: specialize TArray<SizeInt>;
    fFlags: TFunctionCallFlags;
    fResult: PTypeInfo;
    fCC: TCallConv;
    procedure InitArgs;
    procedure HandleCallback(const aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer);
    constructor Create(aCC: TCallConv; aArgs: specialize TArray<TFunctionCallParameterInfo>; aResult: PTypeInfo; aFlags: TFunctionCallFlags; aUserData: Pointer; aCallback: TMethodImplementationCallbackMethod);
    constructor Create(aCC: TCallConv; aArgs: specialize TArray<TFunctionCallParameterInfo>; aResult: PTypeInfo; aFlags: TFunctionCallFlags; aUserData: Pointer; aCallback: TMethodImplementationCallbackProc);
  Protected
    function GetCodeAddress: CodePointer; inline;
  public
    constructor Create;
    destructor Destroy; override;
    property CodeAddress: CodePointer read GetCodeAddress;
  end;

  TRttiInvokableType = class(TRttiType)
  protected
    function GetParameters(aWithHidden: Boolean): TRttiParameterArray; virtual; abstract;
    function GetCallingConvention: TCallConv; virtual; abstract;
    function GetReturnType: TRttiType; virtual; abstract;
    function GetFlags: TFunctionCallFlags; virtual; abstract;
  public type
    TCallbackMethod = procedure(aInvokable: TRttiInvokableType; const aArgs: TValueArray; out aResult: TValue) of object;
    TCallbackProc = procedure(aInvokable: TRttiInvokableType; const aArgs: TValueArray; out aResult: TValue);
  public
    function GetParameters: TRttiParameterArray; inline;
    property CallingConvention: TCallConv read GetCallingConvention;
    property ReturnType: TRttiType read GetReturnType;
    function Invoke(const aProcOrMeth: TValue; const aArgs: array of TValue): TValue; virtual; abstract;
    { Note: once "reference to" is supported these will be replaced by a single method }
    function CreateImplementation(aCallback: TCallbackMethod): TMethodImplementation;
    function CreateImplementation(aCallback: TCallbackProc): TMethodImplementation;
    function ToString : string; override;
  end;

  TRttiMethodType = class(TRttiInvokableType)
  private
    FCallConv: TCallConv;
    FReturnType: TRttiType;
    FParams, FParamsAll: TRttiParameterArray;
  protected
    function GetParameters(aWithHidden: Boolean): TRttiParameterArray; override;
    function GetCallingConvention: TCallConv; override;
    function GetReturnType: TRttiType; override;
    function GetFlags: TFunctionCallFlags; override;
  public
    function Invoke(const aCallable: TValue; const aArgs: array of TValue): TValue; override;
    function ToString: string; override;
  end;

  TRttiProcedureType = class(TRttiInvokableType)
  private
    FParams, FParamsAll: TRttiParameterArray;
  protected
    function GetParameters(aWithHidden: Boolean): TRttiParameterArray; override;
    function GetCallingConvention: TCallConv; override;
    function GetReturnType: TRttiType; override;
    function GetFlags: TFunctionCallFlags; override;
  public
    function Invoke(const aCallable: TValue; const aArgs: array of TValue): TValue; override;
  end;

  TDispatchKind = (
    dkStatic,
    dkVtable,
    dkDynamic,
    dkMessage,
    dkInterface,
    { the following are FPC-only and will be moved should Delphi add more }
    dkMessageString
  );

  TRttiMethod = class(TRttiMember)
  private
    FString: String;
    function GetFlags: TFunctionCallFlags;
  protected
    function GetCallingConvention: TCallConv; virtual; abstract;
    function GetCodeAddress: CodePointer; virtual; abstract;
    function GetDispatchKind: TDispatchKind; virtual; abstract;
    function GetHasExtendedInfo: Boolean; virtual;
    function GetIsClassMethod: Boolean; virtual; abstract;
    function GetIsConstructor: Boolean; virtual; abstract;
    function GetIsDestructor: Boolean; virtual; abstract;
    function GetIsStatic: Boolean; virtual; abstract;
    function GetMethodKind: TMethodKind; virtual; abstract;
    function GetReturnType: TRttiType; virtual; abstract;
    function GetVirtualIndex: SmallInt; virtual; abstract;
    function GetParameters(aWithHidden: Boolean): TRttiParameterArray; virtual; abstract;
  public
    property CallingConvention: TCallConv read GetCallingConvention;
    property CodeAddress: CodePointer read GetCodeAddress;
    property DispatchKind: TDispatchKind read GetDispatchKind;
    property HasExtendedInfo: Boolean read GetHasExtendedInfo;
    property IsClassMethod: Boolean read GetIsClassMethod;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsDestructor: Boolean read GetIsDestructor;
    property IsStatic: Boolean read GetIsStatic;
    property MethodKind: TMethodKind read GetMethodKind;
    property ReturnType: TRttiType read GetReturnType;
    property VirtualIndex: SmallInt read GetVirtualIndex;
    function ToString: String; override;
    function GetParameters: TRttiParameterArray; inline;
    function Invoke(aInstance: TObject; const aArgs: array of TValue): TValue;
    function Invoke(aInstance: TClass; const aArgs: array of TValue): TValue;
    function Invoke(aInstance: TValue; const aArgs: array of TValue): TValue;
    { Note: once "reference to" is supported these will be replaced by a single method }
    function CreateImplementation(aUserData: Pointer; aCallback: TMethodImplementationCallbackMethod): TMethodImplementation;
    function CreateImplementation(aUserData: Pointer; aCallback: TMethodImplementationCallbackProc): TMethodImplementation;
  end;

  TRttiStructuredType = class(TRttiType)

  end;

  TInterfaceType = (
    itRefCounted, { aka COM interface }
    itRaw         { aka CORBA interface }
  );

  TRttiInterfaceType = class(TRttiType)
  private
    fDeclaredMethods: specialize TArray<TRttiMethod>;
  protected
    function IntfMethodCount: Word;
    function MethodTable: PIntfMethodTable; virtual; abstract;
    function GetBaseType: TRttiType; override;
    function GetIntfBaseType: TRttiInterfaceType; virtual; abstract;
    function GetDeclaringUnitName: String; virtual; abstract;
    function GetGUID: TGUID; virtual; abstract;
    function GetGUIDStr: String; virtual;
    function GetIntfFlags: TIntfFlags; virtual; abstract;
    function GetIntfType: TInterfaceType; virtual; abstract;
  public
    property BaseType: TRttiInterfaceType read GetIntfBaseType;
    property DeclaringUnitName: String read GetDeclaringUnitName;
    property GUID: TGUID read GetGUID;
    property GUIDStr: String read GetGUIDStr;
    property IntfFlags: TIntfFlags read GetIntfFlags;
    property IntfType: TInterfaceType read GetIntfType;
    function GetDeclaredMethods: specialize TArray<TRttiMethod>; override;
  end;

  { TRttiInstanceType }

  TRttiInstanceType = class(TRttiStructuredType)
  private
    FPropertiesResolved: Boolean;
    FProperties: specialize TArray<TRttiProperty>;
    function GetDeclaringUnitName: string;
    function GetMetaClassType: TClass;
  protected
    function GetIsInstance: boolean; override;
    function GetTypeSize: integer; override;
    function GetBaseType: TRttiType; override;
  public
    function GetProperties: specialize TArray<TRttiProperty>; override;
    property MetaClassType: TClass read GetMetaClassType;
    property DeclaringUnitName: string read GetDeclaringUnitName;
  end;

  TVirtualInterfaceInvokeEvent = procedure(aMethod: TRttiMethod; const aArgs: TValueArray; out aResult: TValue) of object;

  TVirtualInterface = class(TInterfacedObject, IInterface)
  private
    fGUID: TGUID;
    fOnInvoke: TVirtualInterfaceInvokeEvent;
    fContext: TRttiContext;
    fThunks: array[0..2] of CodePointer;
    fImpls: array of TMethodImplementation;
    fVmt: PCodePointer;
  protected
    function QueryInterface(constref aIID: TGuid; out aObj): LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; reintroduce; virtual;
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; reintroduce; virtual;
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; reintroduce; virtual;

    procedure HandleUserCallback(aUserData: Pointer; const aArgs: TValueArray; out aResult: TValue);
  public
    constructor Create(aPIID: PTypeInfo);
    constructor Create(aPIID: PTypeInfo; aInvokeEvent: TVirtualInterfaceInvokeEvent);
    destructor Destroy; override;
    property OnInvoke: TVirtualInterfaceInvokeEvent read fOnInvoke write fOnInvoke;
  end;


  ERtti = class(Exception);
  EInsufficientRtti = class(ERtti);
  EInvocationError = class(ERtti);
  ENonPublicType = class(ERtti);

  TFunctionCallParameter = record
    ValueRef: Pointer;
    ValueSize: SizeInt;
    Info: TFunctionCallParameterInfo;
  end;
  TFunctionCallParameterArray = specialize TArray<TFunctionCallParameter>;

  TFunctionCallProc = procedure(const aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer);
  TFunctionCallMethod = procedure(const aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer) of object;

  TFunctionCallManager = record
    Invoke: procedure(CodeAddress: CodePointer; const Args: TFunctionCallParameterArray; CallingConvention: TCallConv;
              ResultType: PTypeInfo; ResultValue: Pointer; Flags: TFunctionCallFlags);
    CreateCallbackProc: function(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
    CreateCallbackMethod: function(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
  end;
  TFunctionCallManagerArray = array[TCallConv] of TFunctionCallManager;

  TCallConvSet = set of TCallConv;

procedure SetFunctionCallManager(aCallConv: TCallConv; constref aFuncCallMgr: TFunctionCallManager; out aOldFuncCallMgr: TFunctionCallManager);
procedure SetFunctionCallManager(aCallConv: TCallConv; constref aFuncCallMgr: TFunctionCallManager);
procedure SetFunctionCallManager(aCallConvs: TCallConvSet; constref aFuncCallMgr: TFunctionCallManager; out aOldFuncCallMgrs: TFunctionCallManagerArray);
procedure SetFunctionCallManager(aCallConvs: TCallConvSet; constref aFuncCallMgr: TFunctionCallManager);
procedure SetFunctionCallManagers(aCallConvs: TCallConvSet; constref aFuncCallMgrs: TFunctionCallManagerArray; out aOldFuncCallMgrs: TFunctionCallManagerArray);
procedure SetFunctionCallManagers(aCallConvs: TCallConvSet; constref aFuncCallMgrs: TFunctionCallManagerArray);
procedure SetFunctionCallManagers(constref aFuncCallMgrs: TFunctionCallManagerArray; out aOldFuncCallMgrs: TFunctionCallManagerArray);
procedure SetFunctionCallManagers(constref aFuncCallMgrs: TFunctionCallManagerArray);
procedure GetFunctionCallManager(aCallConv: TCallConv; out aFuncCallMgr: TFunctionCallManager);
procedure GetFunctionCallManagers(aCallConvs: TCallConvSet; out aFuncCallMgrs: TFunctionCallManagerArray);
procedure GetFunctionCallManagers(out aFuncCallMgrs: TFunctionCallManagerArray);

function Invoke(aCodeAddress: CodePointer; const aArgs: TValueArray; aCallConv: TCallConv;
  aResultType: PTypeInfo; aIsStatic: Boolean; aIsConstructor: Boolean): TValue;

function CreateCallbackProc(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
function CreateCallbackMethod(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;

function IsManaged(TypeInfo: PTypeInfo): boolean;
function IsBoolType(ATypeInfo: PTypeInfo): Boolean;

function ArrayOfConstToTValueArray(const aValues: array of const): TValueArray;
{$ifndef InLazIDE}
generic function OpenArrayToDynArrayValue<T>(constref aArray: array of T): TValue;
{$endif}

{ these resource strings are needed by units implementing function call managers }
resourcestring
  SErrInvokeNotImplemented = 'Invoke functionality is not implemented';
  SErrInvokeResultTypeNoValue = 'Function has a result type, but no result pointer provided';
  SErrInvokeFailed = 'Invoke call failed';
  SErrMethodImplCreateFailed  = 'Failed to create method implementation';
  SErrCallbackNotImplemented = 'Callback functionality is not implemented';
  SErrCallConvNotSupported = 'Calling convention not supported: %s';
  SErrTypeKindNotSupported = 'Type kind is not supported: %s';
  SErrCallbackHandlerNil = 'Callback handler is Nil';
  SErrMissingSelfParam = 'Missing self parameter';
  SErrNotEnumeratedType = '%s is not an enumerated type.';
  
implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Variants,
{$ifdef windows}
  WinApi.Windows,
{$endif}
{$ifdef unix}
  UnixApi.Base,
{$endif}
  System.SysConst,
  System.FGL;
{$ELSE FPC_DOTTEDUNITS}
  Variants,
{$ifdef windows}
  Windows,
{$endif}
{$ifdef unix}
  BaseUnix,
{$endif}
  sysconst,
  fgl;
{$ENDIF FPC_DOTTEDUNITS}

function AlignToPtr(aPtr: Pointer): Pointer; inline;
begin
{$ifdef CPUM68K}
  Result := AlignTypeData(aPtr);
{$else}
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(aPtr, SizeOf(Pointer));
{$else}
  Result := aPtr;
{$endif}
{$endif}
end;

Function IsDateTimeType(aData : PTypeInfo) : Boolean; inline;

begin
  Result:=(aData=TypeInfo(TDateTime))
          or (aData=TypeInfo(TDate))
          or (aData=TypeInfo(TTime));
end;

Function TypeInfoToVarType(aTypeInfo : PTypeInfo; out aType : TVarType) : Boolean;

begin
  aType:=varEmpty;
  case aTypeInfo^.Kind of
    tkChar,
    tkWideChar,
    tkString,
    tkLString:
      aType:=varString;
    tkUString:
      aType:=varUString;
    tkWString:
      aType:=varOleStr;
    tkVariant:
      aType:=varVariant;
    tkInteger:
      case GetTypeData(aTypeInfo)^.OrdType of
        otSByte:  aType:=varShortInt;
        otSWord:  aType:=varSmallint;
        otSLong:  aType:=varInteger;
        otUByte:  aType:=varByte;
        otUWord:  aType:=varWord;
        otULong:  aType:=varLongWord;
        otUQWord: aType:=varQWord;
        otSQWord: aType:=varInt64;
      end;
    tkEnumeration:
      if IsBoolType(aTypeInfo) then
        aType:=varBoolean;
    tkFloat:
      if IsDateTimeType(aTypeInfo) then
        aType:=varDate
      else
        case GetTypeData(aTypeInfo)^.FloatType of
          ftSingle:   aType:=varSingle;
          ftDouble:   aType:=varDouble;
          ftExtended: aType:=varDouble;
          ftComp:     aType:=varInt64;
          ftCurr:     aType:=varCurrency;
        end;
    tkInterface:
      if aTypeInfo=System.TypeInfo(IDispatch) then
        aType:=varDispatch
      else
        aType:=varUnknown;
    tkInt64:
      aType:=varInt64;
    tkQWord:
      aType:=varUInt64
  else
    aType:=varEmpty;
  end;
  Result:=(aType<>varEmpty);
end;

function VarTypeToTypeInfo(aVarType : TVarType; out DataType: PTypeInfo) : Boolean;

begin
  Result:=True;
  DataType:=Nil;
  case aVarType of
    varEmpty,
    varNull:
      ;
    varUnknown:
      DataType:=System.TypeInfo(IInterface);
    varShortInt:
      DataType:=System.TypeInfo(ShortInt);
    varSmallint:
      DataType:=System.TypeInfo(SmallInt);
    varInteger:
      DataType:=System.TypeInfo(Integer);
    varSingle:
      DataType:=System.TypeInfo(Single);
    varCurrency:
      DataType:=System.TypeInfo(Currency);
    varDate:
      DataType:=System.TypeInfo(TDateTime);
    varOleStr:
      DataType:=System.TypeInfo(WideString);
    varUString:
      DataType:=System.TypeInfo(UnicodeString);
    varDispatch:
      DataType:=System.TypeInfo(IDispatch);
    varError:
      DataType:=System.TypeInfo(HRESULT);
    varByte:
      DataType:=System.TypeInfo(Byte);
    varWord:
      DataType:=System.TypeInfo(Word);
    varInt64:
      DataType:=System.TypeInfo(Int64);
    varUInt64:
      DataType:=System.TypeInfo(UInt64);
    varBoolean:
      DataType:=System.TypeInfo(Boolean);
    varDouble:
      DataType:=System.TypeInfo(Double);
    varString:
      DataType:=System.TypeInfo(RawByteString);
  else
    Result:=False;
  end;
end;

Function FloatTypeToTypeInfo(FT : TFloatType) : PTypeInfo;

begin
  Case FT of
    ftSingle: Result:=System.TypeInfo(Single);
    ftDouble: Result:=System.TypeInfo(Double);
    ftExtended: Result:=System.TypeInfo(Extended);
    ftComp: Result:=System.TypeInfo(Comp);
    ftCurr: Result:=System.TypeInfo(Currency);
  end;
end;

type

  { TRttiPool }

  TRttiPool = class
  private type
    TRttiObjectMap = specialize TFPGMap<Pointer, TRttiObject>;
  private
    FObjectMap: TRttiObjectMap;
    FTypesList: specialize TArray<TRttiType>;
    FTypeCount: LongInt;
    FLock: TRTLCriticalSection;
  public
    function GetTypes: specialize TArray<TRttiType>;
    function GetType(ATypeInfo: PTypeInfo): TRttiType;
    function GetByHandle(aHandle: Pointer): TRttiObject;
    procedure AddObject(aObject: TRttiObject);
    constructor Create;
    destructor Destroy; override;
  end;

  IPooltoken = interface
  ['{3CDB3CE9-AB55-CBAA-7B9D-2F3BB1CF5AF8}']
    function RttiPool: TRttiPool;
  end;

  { TPoolToken }

  TPoolToken = class(TInterfacedObject, IPooltoken)
  public
    constructor Create;
    destructor Destroy; override;
    function RttiPool: TRttiPool;
  end;

  { TValueDataIntImpl }

  TValueDataIntImpl = class(TInterfacedObject, IValueData)
  private
    FBuffer: Pointer;
    FDataSize: SizeInt;
    FTypeInfo: PTypeInfo;
    FIsCopy: Boolean;
    FUseAddRef: Boolean;
  public
    constructor CreateCopy(ACopyFromBuffer: Pointer; ALen: SizeInt; ATypeInfo: PTypeInfo; AAddRef: Boolean);
    constructor CreateRef(AData: Pointer; ATypeInfo: PTypeInfo; AAddRef: Boolean);
    destructor Destroy; override;
    procedure ExtractRawData(ABuffer: pointer);
    procedure ExtractRawDataNoCopy(ABuffer: pointer);
    function GetDataSize: SizeInt;
    function GetReferenceToRawData: pointer;
  end;

  TRttiRefCountedInterfaceType = class(TRttiInterfaceType)
  private
    function IntfData: PInterfaceData; inline;
  protected
    function MethodTable: PIntfMethodTable; override;
    function GetIntfBaseType: TRttiInterfaceType; override;
    function GetDeclaringUnitName: String; override;
    function GetGUID: TGUID; override;
    function GetIntfFlags: TIntfFlags; override;
    function GetIntfType: TInterfaceType; override;
  end;

  TRttiRawInterfaceType = class(TRttiInterfaceType)
  private
    function IntfData: PInterfaceRawData; inline;
  protected
    function MethodTable: PIntfMethodTable; override;
    function GetIntfBaseType: TRttiInterfaceType; override;
    function GetDeclaringUnitName: String; override;
    function GetGUID: TGUID; override;
    function GetGUIDStr: String; override;
    function GetIntfFlags: TIntfFlags; override;
    function GetIntfType: TInterfaceType; override;
  end;

  { TRttiVmtMethodParameter }

  TRttiVmtMethodParameter = class(TRttiParameter)
  private
    FVmtMethodParam: PVmtMethodParam;
  protected
    function GetHandle: Pointer; override;
    function GetName: String; override;
    function GetFlags: TParamFlags; override;
    function GetParamType: TRttiType; override;
  public
    constructor Create(AVmtMethodParam: PVmtMethodParam);
    function GetAttributes: TCustomAttributeArray; override;
  end;

  { TRttiMethodTypeParameter }

  TRttiMethodTypeParameter = class(TRttiParameter)
  private
    fHandle: Pointer;
    fName: String;
    fFlags: TParamFlags;
    fType: PTypeInfo;
  protected
    function GetHandle: Pointer; override;
    function GetName: String; override;
    function GetFlags: TParamFlags; override;
    function GetParamType: TRttiType; override;
  public
    constructor Create(aHandle: Pointer; const aName: String; aFlags: TParamFlags; aType: PTypeInfo);
    function GetAttributes: TCustomAttributeArray; override;
  end;

  { TRttiIntfMethod }

  TRttiIntfMethod = class(TRttiMethod)
  private
    FIntfMethodEntry: PIntfMethodEntry;
    FIndex: SmallInt;
    FParams, FParamsAll: TRttiParameterArray;
    FAttributesResolved: boolean;
    FAttributes: TCustomAttributeArray;
  protected
    function GetHandle: Pointer; override;
    function GetName: String; override;
    function GetCallingConvention: TCallConv; override;
    function GetCodeAddress: CodePointer; override;
    function GetDispatchKind: TDispatchKind; override;
    function GetHasExtendedInfo: Boolean; override;
    function GetIsClassMethod: Boolean; override;
    function GetIsConstructor: Boolean; override;
    function GetIsDestructor: Boolean; override;
    function GetIsStatic: Boolean; override;
    function GetMethodKind: TMethodKind; override;
    function GetReturnType: TRttiType; override;
    function GetVirtualIndex: SmallInt; override;
    function GetParameters(aWithHidden: Boolean): TRttiParameterArray; override;
  public
    constructor Create(AParent: TRttiType; AIntfMethodEntry: PIntfMethodEntry; AIndex: SmallInt);
    function GetAttributes: TCustomAttributeArray; override;
  end;

resourcestring
  SErrUnableToGetValueForType = 'Unable to get value for type %s';
  SErrUnableToSetValueForType = 'Unable to set value for type %s';
  SErrDimensionOutOfRange     = 'Dimension index %d is out of range [0, %d[';
  SErrLengthOfArrayMismatch   = 'Length of static array does not match: Got %d, but expected %d';
  SErrInvalidTypecast         = 'Invalid class typecast';
  SErrRttiObjectNoHandle      = 'RTTI object instance has no valid handle property';
  SErrRttiObjectAlreadyRegistered = 'A RTTI object with handle 0x%x is already registered';
  SErrInvokeInsufficientRtti  = 'Insufficient RTTI to invoke function';
  SErrInvokeStaticNoSelf      = 'Static function must not be called with in an instance: %s';
  SErrInvokeNotStaticNeedsSelf = 'Non static function must be called with an instance: %s';
  SErrInvokeClassMethodClassSelf = 'Class method needs to be called with a class type: %s';
  SErrInvokeArrayArgExpected  = 'Array argument expected for parameter %s of method %s';
  SErrInvokeArgInvalidType    = 'Invalid type of argument for parameter %s of method %s';
  SErrInvokeArgCount          = 'Invalid argument count for method %s; expected %d, but got %d';
  SErrInvokeNoCodeAddr        = 'Failed to determine code address for method: %s';
  SErrInvokeRttiDataError     = 'The RTTI data is inconsistent for method: %s';
  SErrInvokeCallableNotProc   = 'The callable value is not a procedure variable for: %s';
  SErrInvokeCallableNotMethod = 'The callable value is not a method variable for: %s';
  SErrMethodImplNoCallback    = 'No callback specified for method implementation';
//  SErrMethodImplInsufficientRtti = 'Insufficient RTTI to create method implementation';
  SErrMethodImplCreateNoArg   = 'TMethodImplementation can not be created this way';
  SErrVirtIntfTypeNil = 'No type information provided for TVirtualInterface';
  SErrVirtIntfTypeMustBeIntf = 'Type ''%s'' is not an interface type';
  SErrVirtIntfTypeNotFound = 'Type ''%s'' is not valid';
  SErrVirtIntfNotAllMethodsRTTI = 'Not all methods of ''%s'' or its parents have the required RTTI';
//  SErrVirtIntfRetrieveIInterface = 'Failed to retrieve IInterface information';
  SErrVirtIntfCreateThunk = 'Failed to create thunks for ''%0:s''';
//  SErrVirtIntfCreateImpl = 'Failed to create implementation for method ''%1:s'' of ''%0:s''';
  SErrVirtIntfInvalidVirtIdx = 'Virtual index %2:d for method ''%1:s'' of ''%0:s'' is invalid';
  SErrVirtIntfMethodNil = 'Method %1:d of ''%0:s'' is Nil';
  SErrVirtIntfCreateVmt = 'Failed to create VMT for ''%s''';
//  SErrVirtIntfIInterface = 'Failed to prepare IInterface method callbacks';

var
  PoolRefCount : integer;
  GRttiPool    : TRttiPool;
  FuncCallMgr: TFunctionCallManagerArray;

function AllocateMemory(aSize: PtrUInt): Pointer;
begin
{$IF DEFINED(WINDOWS)}
  Result := VirtualAlloc(Nil, aSize, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
{$ELSEIF DEFINED(UNIX)}
  Result := fpmmap(Nil, aSize, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, 0, 0);
{$ELSE}
  Result := Nil;
{$ENDIF}
end;

function ProtectMemory(aPtr: Pointer; aSize: PtrUInt; aExecutable: Boolean): Boolean;
{$IF DEFINED(WINDOWS)}
var
  oldprot: DWORD;
{$ENDIF}
begin
{$IF DEFINED(WINDOWS)}
  if aExecutable then
    Result := VirtualProtect(aPtr, aSize, PAGE_EXECUTE_READ, oldprot)
  else
    Result := VirtualProtect(aPtr, aSize, PAGE_READWRITE, oldprot);
{$ELSEIF DEFINED(UNIX)}
  if aExecutable then
    Result := Fpmprotect(aPtr, aSize, PROT_EXEC or PROT_READ) = 0
  else
    Result := Fpmprotect(aPtr, aSize, PROT_READ or PROT_WRITE) = 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure FreeMemory(aPtr: Pointer; aSize: PtrUInt);
begin
{$IF DEFINED(WINDOWS)}
  VirtualFree(aPtr, 0, MEM_RELEASE);
{$ELSEIF DEFINED(UNIX)}
  fpmunmap(aPtr, aSize);
{$ELSE}
  { nothing }
{$ENDIF}
end;

label
  RawThunkEnd;

{$if defined(cpui386)}
const
  RawThunkPlaceholderBytesToPop = $12341234;
  RawThunkPlaceholderProc = $87658765;
  RawThunkPlaceholderContext = $43214321;

type
  TRawThunkBytesToPop = UInt32;
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

{ works for both cdecl and stdcall }
procedure RawThunk; assembler; nostackframe;
asm
  { the stack layout is
      $ReturnAddr <- ESP
      ArgN
      ArgN - 1
      ...
      Arg1
      Arg0

    aBytesToPop is the size of the stack to the Self argument }

  movl RawThunkPlaceholderBytesToPop, %eax
  movl %esp, %ecx
  lea (%ecx,%eax), %eax
  movl RawThunkPlaceholderContext, (%eax)
  movl RawThunkPlaceholderProc, %eax
  jmp %eax
RawThunkEnd:
end;
{$elseif defined(cpux86_64)}
const
  RawThunkPlaceholderProc = PtrUInt($8765876587658765);
  RawThunkPlaceholderContext = PtrUInt($4321432143214321);

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

{$ifdef win64}
procedure RawThunk; assembler; nostackframe;
asm
  { Self is always in register RCX }
  movq RawThunkPlaceholderContext, %rcx
  movq RawThunkPlaceholderProc, %rax
  jmp %rax
RawThunkEnd:
end;
{$else}
procedure RawThunk; assembler; nostackframe;
asm
  { Self is always in register RDI }
  movq RawThunkPlaceholderContext, %rdi
  movq RawThunkPlaceholderProc, %rax
  jmp %rax
RawThunkEnd:
end;
{$endif}
{$elseif defined(cpuarm)}
const
  RawThunkPlaceholderProc = $87658765;
  RawThunkPlaceholderContext = $43214321;

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

procedure RawThunk; assembler; nostackframe;
asm
  (* To be compatible with Thumb we first load the function pointer into R0,
    then move that to R12 which is volatile and then we load the new Self into
    R0 *)
  ldr r0, .LProc
  mov r12, r0
  ldr r0, .LContext
{$ifdef CPUARM_HAS_BX}
  bx r12
{$else}
  mov pc, r12
{$endif}
.LProc:
  .long RawThunkPlaceholderProc
.LContext:
  .long RawThunkPlaceholderContext
RawThunkEnd:
end;
{$elseif defined(cpuaarch64)}
const
  RawThunkPlaceholderProc = $8765876587658765;
  RawThunkPlaceholderContext = $4321432143214321;

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

procedure RawThunk; assembler; nostackframe;
asm
  ldr x16, .LProc
  ldr x0, .LContext
  br x16
.LProc:
  .quad RawThunkPlaceholderProc
.LContext:
  .quad RawThunkPlaceholderContext
RawThunkEnd:
end;
{$elseif defined(cpum68k)}
const
  RawThunkPlaceholderProc = $87658765;
  RawThunkPlaceholderContext = $43214321;

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

procedure RawThunk; assembler; nostackframe;
asm
  lea 4(sp), a0
  move.l #RawThunkPlaceholderContext, (a0)
  move.l #RawThunkPlaceholderProc, a0
  jmp (a0)
RawThunkEnd:
end;
{$elseif defined(cpuriscv64)}
const
  RawThunkPlaceholderProc = $8765876587658765;
  RawThunkPlaceholderContext = $4321432143214321;

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

procedure RawThunk; assembler; nostackframe;
asm
  ld x5, .LProc
  ld x10, .LContext
  jalr x0, x5, 0
.LProc:
  .quad RawThunkPlaceholderProc
.LContext:
  .quad RawThunkPlaceholderContext
RawThunkEnd:
end;
{$elseif defined(cpuriscv32)}
const
  RawThunkPlaceholderProc = $87658765;
  RawThunkPlaceholderContext = $43214321;

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

procedure RawThunk; assembler; nostackframe;
asm
  lw x5, .LProc
  lw x10, .LContext
  jalr x0, x5, 0
.LProc:
  .long RawThunkPlaceholderProc
.LContext:
  .long RawThunkPlaceholderContext
RawThunkEnd:
end;
{$elseif defined(cpuloongarch64)}
const
  RawThunkPlaceholderProc = $8765876587658765;
  RawThunkPlaceholderContext = $4321432143214321;

type
  TRawThunkProc = PtrUInt;
  TRawThunkContext = PtrUInt;

procedure RawThunk; assembler; nostackframe;
asm
  move $t0, $ra
  bl .Lreal
  .quad RawThunkPlaceholderProc
  .quad RawThunkPlaceholderContext
.Lreal:
  ld.d $a0, $ra, 8
  ld.d $t1, $ra, 0
  move $ra, $t0
  jr $t1
RawThunkEnd:
end;
{$endif}

{$if declared(RawThunk)}
const
  RawThunkEndPtr: Pointer = @RawThunkEnd;

type
{$if declared(TRawThunkBytesToPop)}
  PRawThunkBytesToPop = ^TRawThunkBytesToPop;
{$endif}
  PRawThunkContext = ^TRawThunkContext;
  PRawThunkProc = ^TRawThunkProc;
{$endif}

{ Delphi has these as part of TRawVirtualClass.TVTable; until we have that we
  simply leave that here in the implementation }
function AllocateRawThunk(aProc: CodePointer; aContext: Pointer; aBytesToPop: SizeInt): CodePointer;
{$if declared(RawThunk)}
var
  size, i: SizeInt;
{$if declared(TRawThunkBytesToPop)}
  btp: PRawThunkBytesToPop;
  btpdone: Boolean;
{$endif}
  context: PRawThunkContext;
  contextdone: Boolean;
  proc: PRawThunkProc;
  procdone: Boolean;
{$endif}
begin
{$if not declared(RawThunk)}
  { platform dose not have thunk support... :/ }
  Result := Nil;
{$else}
  Size := PtrUInt(RawThunkEndPtr) - PtrUInt(@RawThunk) + 1;
  Result := AllocateMemory(size);
  Move(Pointer(@RawThunk)^, Result^, size);

{$if declared(TRawThunkBytesToPop)}
  btpdone := False;
{$endif}
  contextdone := False;
  procdone := False;

  for i := 0 to Size - 1 do begin
{$if declared(TRawThunkBytesToPop)}
    if not btpdone and (i <= Size - SizeOf(TRawThunkBytesToPop)) then begin
      btp := PRawThunkBytesToPop(PByte(Result) + i);
      if btp^ = TRawThunkBytesToPop(RawThunkPlaceholderBytesToPop) then begin
        btp^ := TRawThunkBytesToPop(aBytesToPop);
        btpdone := True;
      end;
    end;
{$endif}
    if not contextdone and (i <= Size - SizeOf(TRawThunkContext)) then begin
      context := PRawThunkContext(PByte(Result) + i);
      if context^ = TRawThunkContext(RawThunkPlaceholderContext) then begin
        context^ := TRawThunkContext(aContext);
        contextdone := True;
      end;
    end;
    if not procdone and (i <= Size - SizeOf(TRawThunkProc)) then begin
      proc := PRawThunkProc(PByte(Result) + i);
      if proc^ = TRawThunkProc(RawThunkPlaceholderProc) then begin
        proc^ := TRawThunkProc(aProc);
        procdone := True;
      end;
    end;
  end;

  if not contextdone or not procdone
{$if declared(TRawThunkBytesToPop)}
      or not btpdone
{$endif}
      then begin
    FreeMemory(Result, Size);
    Result := Nil;
  end else
    ProtectMemory(Result, Size, True);
{$endif}
end;

procedure FreeRawThunk(aThunk: CodePointer);
begin
{$if declared(RawThunk)}
  FreeMemory(aThunk, PtrUInt(RawThunkEndPtr) - PtrUInt(@RawThunk));
{$endif}
end;

function CCToStr(aCC: TCallConv): String; inline;
begin
  WriteStr(Result, aCC);
end;

procedure NoInvoke(aCodeAddress: CodePointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; aResultValue: Pointer; aFlags: TFunctionCallFlags);
begin
  raise ENotImplemented.Create(SErrInvokeNotImplemented);
end;

function NoCreateCallbackProc(aFunc: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  Result := Nil;
  raise ENotImplemented.Create(SErrCallbackNotImplemented);
end;

function NoCreateCallbackMethod(aFunc: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  Result := Nil;
  raise ENotImplemented.Create(SErrCallbackNotImplemented);
end;

const
  NoFunctionCallManager: TFunctionCallManager = (
    Invoke: @NoInvoke;
    CreateCallbackProc: @NoCreateCallbackProc;
    CreateCallbackMethod: @NoCreateCallbackMethod;
  );

procedure SetFunctionCallManager(aCallConv: TCallConv; constref aFuncCallMgr: TFunctionCallManager;
  out aOldFuncCallMgr: TFunctionCallManager);
begin
  aOldFuncCallMgr := FuncCallMgr[aCallConv];
  FuncCallMgr[aCallConv] := aFuncCallMgr;
end;

procedure SetFunctionCallManager(aCallConv: TCallConv; constref aFuncCallMgr: TFunctionCallManager);
var
  dummy: TFunctionCallManager;
begin
  SetFunctionCallManager(aCallConv, aFuncCallMgr, dummy);
end;

procedure SetFunctionCallManager(aCallConvs: TCallConvSet; constref aFuncCallMgr: TFunctionCallManager;
  out aOldFuncCallMgrs: TFunctionCallManagerArray);
var
  cc: TCallConv;
begin
  for cc := Low(TCallConv) to High(TCallConv) do
    if cc in aCallConvs then begin
      aOldFuncCallMgrs[cc] := FuncCallMgr[cc];
      FuncCallMgr[cc] := aFuncCallMgr;
    end else
      aOldFuncCallMgrs[cc] := Default(TFunctionCallManager);
end;

procedure SetFunctionCallManager(aCallConvs: TCallConvSet; constref aFuncCallMgr: TFunctionCallManager);
var
  dummy: TFunctionCallManagerArray;
begin
  SetFunctionCallManager(aCallConvs, aFuncCallMgr, dummy);
end;

procedure SetFunctionCallManagers(aCallConvs: TCallConvSet; constref aFuncCallMgrs: TFunctionCallManagerArray; out aOldFuncCallMgrs: TFunctionCallManagerArray);
var
  cc: TCallConv;
begin
  for cc := Low(TCallConv) to High(TCallConv) do
    if cc in aCallConvs then begin
      aOldFuncCallMgrs[cc] := FuncCallMgr[cc];
      FuncCallMgr[cc] := aFuncCallMgrs[cc];
    end else
      aOldFuncCallMgrs[cc] := Default(TFunctionCallManager);
end;

procedure SetFunctionCallManagers(aCallConvs: TCallConvSet; constref aFuncCallMgrs: TFunctionCallManagerArray);
var
  dummy: TFunctionCallManagerArray;
begin
  SetFunctionCallManagers(aCallConvs, aFuncCallMgrs, dummy);
end;

procedure SetFunctionCallManagers(constref aFuncCallMgrs: TFunctionCallManagerArray; out aOldFuncCallMgrs: TFunctionCallManagerArray);
begin
  aOldFuncCallMgrs := FuncCallMgr;
  FuncCallMgr := aFuncCallMgrs;
end;

procedure SetFunctionCallManagers(constref aFuncCallMgrs: TFunctionCallManagerArray);
var
  dummy: TFunctionCallManagerArray;
begin
  SetFunctionCallManagers(aFuncCallMgrs, dummy);
end;

procedure GetFunctionCallManager(aCallConv: TCallConv; out aFuncCallMgr: TFunctionCallManager);
begin
  aFuncCallMgr := FuncCallMgr[aCallConv];
end;

procedure GetFunctionCallManagers(aCallConvs: TCallConvSet; out aFuncCallMgrs: TFunctionCallManagerArray);
var
  cc: TCallConv;
begin
  for cc := Low(TCallConv) to High(TCallConv) do
    if cc in aCallConvs then
      aFuncCallMgrs[cc] := FuncCallMgr[cc]
    else
      aFuncCallMgrs[cc] := Default(TFunctionCallManager);
end;

procedure GetFunctionCallManagers(out aFuncCallMgrs: TFunctionCallManagerArray);
begin
  aFuncCallMgrs := FuncCallMgr;
end;

procedure InitDefaultFunctionCallManager;
var
  cc: TCallConv;
begin
  for cc := Low(TCallConv) to High(TCallConv) do
    FuncCallMgr[cc] := NoFunctionCallManager;
end;
{ TRttiPool }

function TRttiPool.GetTypes: specialize TArray<TRttiType>;
begin
  if not Assigned(FTypesList) then
    Exit(Nil);
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalsection(FLock);
  try
{$endif}
    Result := Copy(FTypesList, 0, FTypeCount);
{$ifdef FPC_HAS_FEATURE_THREADING}
  finally
    LeaveCriticalsection(FLock);
  end;
{$endif}
end;

function TRttiPool.GetType(ATypeInfo: PTypeInfo): TRttiType;
var
  obj: TRttiObject;
begin
  if not Assigned(ATypeInfo) then
    Exit(Nil);
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalsection(FLock);
  try
{$endif}
    Result := Nil;
    obj := GetByHandle(ATypeInfo);
    if Assigned(obj) then
      Result := obj as TRttiType;
    if not Assigned(Result) then
      begin
        if FTypeCount = Length(FTypesList) then
          begin
            SetLength(FTypesList, FTypeCount * 2);
          end;
        case ATypeInfo^.Kind of
          tkClass   : Result := TRttiInstanceType.Create(ATypeInfo);
          tkInterface: Result := TRttiRefCountedInterfaceType.Create(ATypeInfo);
          tkInterfaceRaw: Result := TRttiRawInterfaceType.Create(ATypeInfo);
          tkArray: Result := TRttiArrayType.Create(ATypeInfo);
          tkDynArray: Result := TRttiDynamicArrayType.Create(ATypeInfo);
          tkInt64,
          tkQWord: Result := TRttiInt64Type.Create(ATypeInfo);
          tkInteger,
          tkChar,
          tkWChar: Result := TRttiOrdinalType.Create(ATypeInfo);
          tkEnumeration : Result := TRttiEnumerationType.Create(ATypeInfo);
          tkSString,
          tkLString,
          tkAString,
          tkUString,
          tkWString : Result := TRttiStringType.Create(ATypeInfo);
          tkFloat   : Result := TRttiFloatType.Create(ATypeInfo);
          tkPointer : Result := TRttiPointerType.Create(ATypeInfo);
          tkProcVar : Result := TRttiProcedureType.Create(ATypeInfo);
          tkMethod  : Result := TRttiMethodType.Create(ATypeInfo);
        else
          Result := TRttiType.Create(ATypeInfo);
        end;
        FTypesList[FTypeCount] := Result;
        FObjectMap.Add(ATypeInfo, Result);
        Inc(FTypeCount);
      end;
{$ifdef FPC_HAS_FEATURE_THREADING}
  finally
    LeaveCriticalsection(FLock);
  end;
{$endif}
end;

function TRttiPool.GetByHandle(aHandle: Pointer): TRttiObject;
var
  idx: LongInt;
begin
  if not Assigned(aHandle) then
    Exit(Nil);
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalsection(FLock);
  try
{$endif}
    idx := FObjectMap.IndexOf(aHandle);
    if idx < 0 then
      Result := Nil
    else
      Result := FObjectMap.Data[idx];
{$ifdef FPC_HAS_FEATURE_THREADING}
  finally
    LeaveCriticalsection(FLock);
  end;
{$endif}
end;

procedure TRttiPool.AddObject(aObject: TRttiObject);
var
  idx: LongInt;
begin
  if not Assigned(aObject) then
    Exit;
  if not Assigned(aObject.Handle) then
    raise EArgumentException.Create(SErrRttiObjectNoHandle);
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalsection(FLock);
  try
{$endif}
    idx := FObjectMap.IndexOf(aObject.Handle);
    if idx < 0 then
      FObjectMap.Add(aObject.Handle, aObject)
    else if FObjectMap.Data[idx] <> aObject then
      raise EInvalidOpException.CreateFmt(SErrRttiObjectAlreadyRegistered, [aObject.Handle]);
{$ifdef FPC_HAS_FEATURE_THREADING}
  finally
    LeaveCriticalsection(FLock);
  end;
{$endif}
end;

constructor TRttiPool.Create;
begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitCriticalSection(FLock);
{$endif}
  SetLength(FTypesList, 32);
  FObjectMap := TRttiObjectMap.Create;
end;

destructor TRttiPool.Destroy;
var
  i: LongInt;
begin
  for i := 0 to FObjectMap.Count - 1 do
    FObjectMap.Data[i].Free;
  FObjectMap.Free;
{$ifdef FPC_HAS_FEATURE_THREADING}
  DoneCriticalsection(FLock);
{$endif}
  inherited Destroy;
end;

{ TPoolToken }

constructor TPoolToken.Create;
begin
  inherited Create;
  if InterlockedIncrement(PoolRefCount)=1 then
    GRttiPool := TRttiPool.Create;
end;

destructor TPoolToken.Destroy;
begin
  if InterlockedDecrement(PoolRefCount)=0 then
    GRttiPool.Free;
  inherited;
end;

function TPoolToken.RttiPool: TRttiPool;
begin
  result := GRttiPool;
end;

{ TValueDataIntImpl }

procedure IntFinalize(APointer, ATypeInfo: Pointer);
  external name 'FPC_FINALIZE';
procedure IntInitialize(APointer, ATypeInfo: Pointer);
  external name 'FPC_INITIALIZE';
procedure IntAddRef(APointer, ATypeInfo: Pointer);
  external name 'FPC_ADDREF';
function IntCopy(ASource, ADest, ATypeInfo: Pointer): SizeInt;
  external name 'FPC_COPY';

constructor TValueDataIntImpl.CreateCopy(ACopyFromBuffer: Pointer; ALen: SizeInt; ATypeInfo: PTypeInfo; AAddRef: Boolean);
begin
  FTypeInfo := ATypeInfo;
  FDataSize:=ALen;
  if ALen>0 then
    begin
      Getmem(FBuffer,FDataSize);
      if Assigned(ACopyFromBuffer) then
        system.move(ACopyFromBuffer^,FBuffer^,FDataSize)
      else
        FillChar(FBuffer^, FDataSize, 0);
    end;
  FIsCopy := True;
  FUseAddRef := AAddRef;
  if AAddRef and (ALen > 0) then begin
    if Assigned(ACopyFromBuffer) then
      IntAddRef(FBuffer, FTypeInfo)
    else
      IntInitialize(FBuffer, FTypeInfo);
  end;
end;

constructor TValueDataIntImpl.CreateRef(AData: Pointer; ATypeInfo: PTypeInfo; AAddRef: Boolean);
begin
  FTypeInfo := ATypeInfo;
  FDataSize := SizeOf(Pointer);
  if Assigned(AData) then
    FBuffer := PPointer(AData)^
  else
    FBuffer := Nil;
  FIsCopy := False;
  FUseAddRef := AAddRef;
  if AAddRef and Assigned(AData) then
    IntAddRef(@FBuffer, FTypeInfo);
end;

destructor TValueDataIntImpl.Destroy;
begin
  if Assigned(FBuffer) then begin
    if FUseAddRef then
      if FIsCopy then
        IntFinalize(FBuffer, FTypeInfo)
      else
        IntFinalize(@FBuffer, FTypeInfo);
    if FIsCopy then
      Freemem(FBuffer);
  end;
  inherited Destroy;
end;

procedure TValueDataIntImpl.ExtractRawData(ABuffer: pointer);
begin
  if FDataSize = 0 then
    Exit;
  if FIsCopy then
    System.Move(FBuffer^, ABuffer^, FDataSize)
  else
    System.Move(FBuffer{!}, ABuffer^, FDataSize);
  if FUseAddRef then
    IntAddRef(ABuffer, FTypeInfo);
end;

procedure TValueDataIntImpl.ExtractRawDataNoCopy(ABuffer: pointer);
begin
  if FDataSize = 0 then
    Exit;
  if FIsCopy then
    system.move(FBuffer^, ABuffer^, FDataSize)
  else
    System.Move(FBuffer{!}, ABuffer^, FDataSize);
end;

function TValueDataIntImpl.GetDataSize: SizeInt;
begin
  result := FDataSize;
end;

function TValueDataIntImpl.GetReferenceToRawData: pointer;
begin
  if FIsCopy then
    result := FBuffer
  else
    result := @FBuffer;
end;

{ TValue }

function TValue.GetTypeDataProp: PTypeData;
begin
  result := GetTypeData(FData.FTypeInfo);
end;

function TValue.GetTypeInfo: PTypeInfo;
begin
  result := FData.FTypeInfo;
end;

function TValue.GetTypeKind: TTypeKind;
begin
  if not Assigned(FData.FTypeInfo) then
    Result := tkUnknown
  else
    result := FData.FTypeInfo^.Kind;
end;

function TValue.IsObject: boolean;
begin
  result := (Kind = tkClass) or ((Kind = tkUnknown) and not Assigned(FData.FAsObject));
end;

function TValue.IsClass: boolean;
begin
  result := (Kind = tkClassRef) or ((Kind in [tkClass,tkUnknown]) and not Assigned(FData.FAsObject));
end;

function TValue.IsOrdinal: boolean;
begin
  result := (Kind in [tkInteger, tkInt64, tkQWord, tkBool, tkEnumeration, tkChar, tkWChar, tkUChar]) or
            ((Kind in [tkClass, tkClassRef, tkInterfaceRaw, tkUnknown]) and not Assigned(FData.FAsPointer));
end;

function TValue.IsDateTime: boolean;

begin
  Result:=IsDateTimeType(TypeInfo);
end;

function TValue.IsInstanceOf(aClass : TClass): boolean; 

var
  Obj : TObject;

begin
  Result:=IsObject;
  if not Result then
    exit;
  Obj:=AsObject;
  Result:=Assigned(Obj) and Obj.InheritsFrom(aClass);
end;

{$ifndef NoGenericMethods}
generic function TValue.IsType<T>:Boolean;
begin
  Result := IsType(PTypeInfo(System.TypeInfo(T)));
end;

generic class procedure TValue.Make<T>(const AValue: T; out Result: TValue);
begin
  TValue.Make(@AValue, PTypeInfo(System.TypeInfo(T)), Result);
end;

generic class function TValue.From<T>(constref aValue: T): TValue;
begin
  TValue.Make(@aValue, PTypeInfo(System.TypeInfo(T)), Result);
end;

generic class function TValue.FromOpenArray<T>(constref aValue: array of T): TValue;
var
  arrdata: Pointer;
begin
  if Length(aValue) > 0 then
    arrdata := @aValue[0]
  else
    arrdata := Nil;
  TValue.MakeOpenArray(arrdata, Length(aValue), PTypeInfo(System.TypeInfo(aValue)), Result);
end;
{$endif}

function TValue.IsType(ATypeInfo: PTypeInfo): boolean;
begin
  result := ATypeInfo = TypeInfo;
end;

class procedure TValue.Make(AValue: NativeInt; ATypeInfo: PTypeInfo; out Result: TValue);
begin
  TValue.Make(@AValue, ATypeInfo, Result);
end;

class operator TValue.:=(const AValue: ShortString): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(const AValue: AnsiString): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(const AValue: UnicodeString): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;


class operator TValue.:=(const AValue: WideString): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:= (AValue: SmallInt): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:= (AValue: ShortInt): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:= (AValue: Byte): TValue; inline;

begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:= (AValue: Word): TValue; inline;

begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:= (AValue: Cardinal): TValue; inline;

begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: LongInt): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Single): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Double): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

{$ifdef FPC_HAS_TYPE_EXTENDED}
class operator TValue.:=(AValue: Extended): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;
{$endif}

class operator TValue.:=(AValue: Currency): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Comp): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Int64): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: QWord): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: TObject): TValue;
begin
  Make(@AValue, PTypeInfo(AValue.ClassInfo), Result);
end;

class operator TValue.:=(AValue: TClass): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Boolean): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: IUnknown): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:= (AValue: TVarRec): TValue;

begin
  Result:=TValue.FromVarRec(aValue);
end;

function TValue.AsString: string;
begin
  if System.GetTypeKind(String) = tkUString then
    Result := String(AsUnicodeString)
  else
    Result := String(AsAnsiString);
end;

procedure TValue.Init;
begin
  { resets the whole variant part; FValueData is already Nil }
{$if SizeOf(TMethod) > SizeOf(QWord)}
  FData.FAsMethod.Code := Nil;
  FData.FAsMethod.Data := Nil;
{$else}
  FData.FAsUInt64 := 0;
{$endif}
end;

class function TValue.Empty: TValue;
begin
  Result.Init;
  result.FData.FTypeInfo := nil;
end;


function TValue.GetDataSize: SizeInt;
begin
  if Assigned(FData.FValueData) and (Kind <> tkSString) then
    Result := FData.FValueData.GetDataSize
  else begin
    Result := 0;
    case Kind of
      tkEnumeration,
      tkBool,
      tkInt64,
      tkQWord,
      tkInteger:
        case TypeData^.OrdType of
          otSByte,
          otUByte:
            Result := SizeOf(Byte);
          otSWord,
          otUWord:
            Result := SizeOf(Word);
          otSLong,
          otULong:
            Result := SizeOf(LongWord);
          otSQWord,
          otUQWord:
            Result := SizeOf(QWord);
        end;
      tkChar:
        Result := SizeOf(AnsiChar);
      tkFloat:
        case TypeData^.FloatType of
          ftSingle:
            Result := SizeOf(Single);
          ftDouble:
            Result := SizeOf(Double);
          ftExtended:
            Result := SizeOf(Extended);
          ftComp:
            Result := SizeOf(Comp);
          ftCurr:
            Result := SizeOf(Currency);
        end;
      tkSet:
        Result := TypeData^.SetSize;
      tkMethod:
        Result := SizeOf(TMethod);
      tkSString:
        { ShortString can hold max. 254 characters as [0] is Length and [255] is #0 }
        Result := SizeOf(ShortString) - 2;
      tkVariant:
        Result := SizeOf(Variant);
      tkProcVar:
        Result := SizeOf(CodePointer);
      tkWChar:
        Result := SizeOf(WideChar);
      tkUChar:
        Result := SizeOf(UnicodeChar);
      tkFile:
        { ToDo }
        Result := SizeOf(TTextRec);
      tkAString,
      tkWString,
      tkUString,
      tkInterface,
      tkDynArray,
      tkClass,
      tkHelper,
      tkClassRef,
      tkInterfaceRaw,
      tkPointer:
        Result := SizeOf(Pointer);
      tkObject,
      tkRecord:
        Result := TypeData^.RecSize;
      tkArray:
        Result := TypeData^.ArrayData.Size;
      tkUnknown,
      tkLString:
        Assert(False);
    end;
  end;
end;

Procedure TValue.CastAssign(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  aRes:=True;
  aDest:=Self;
end;

Procedure TValue.CastIntegerToInteger(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : Integer;

begin
  with FData do
    case GetTypeData(FTypeInfo)^.OrdType of
      otSByte: Tmp:=FAsSByte;
      otSWord: Tmp:=FAsSWord;
      otSLong: Tmp:=FAsSLong;
    else
      Tmp:=Integer(FAsULong);
    end;
  TValue.Make(@Tmp,aDestType,aDest);
  aRes:=True;
end;


Procedure TValue.CastIntegerToFloat(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : Int64;
  Ti : PtypeInfo;

begin
  Tmp:=AsInt64;
  Ti:=FloatTypeToTypeInfo(GetTypeData(aDestType)^.FloatType);
  TValue.Make(@Tmp,Ti,aDest);
  aRes:=True;
end;

Procedure TValue.CastIntegerToInt64(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: Int64;

begin
  Tmp:=AsInt64;
  TValue.Make(@Tmp,aDestType,aDest);
  aRes:=True;
end;

Procedure TValue.CastIntegerToQWord(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: QWord;

begin
  Tmp:=QWord(AsInt64);
  TValue.Make(@Tmp, aDestType, aDest);
  aRes:=True;
end;


Procedure TValue.CastCharToString(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: AnsiChar;
  S : RawByteString;

begin
  Tmp:=AsAnsiChar;
  aRes:=True;
  case aDestType^.Kind of
    tkChar:
      TValue.Make(NativeInt(Tmp), aDestType, aDest);
    tkString:
      TValue.Make(@Tmp,System.TypeInfo(ShortString),aDest);
    tkWString:
      TValue.Make(@Tmp,System.TypeInfo(WideString),aDest);
    tkUString:
      TValue.Make(@Tmp,System.TypeInfo(UnicodeString),aDest);
    tkLString:
      begin
      SetString(S, PAnsiChar(@Tmp), 1);
      SetCodePage(S,GetTypeData(aDestType)^.CodePage);
      TValue.Make(@S, aDestType, aDest);
      end;
  else
    aRes:=False;
  end;
end;

Procedure TValue.CastWCharToString(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: WideChar;
  RS: RawByteString;
  SS : ShortString;
  WS : WideString;
  US : WideString;

begin
  Tmp:=AsWideChar;
  aRes:=True;
  case aDestType^.Kind of
    tkWChar: TValue.Make(NativeInt(Tmp), aDestType, aDest);
    tkString:
      begin
      SS:=Tmp;
      TValue.Make(@SS,System.TypeInfo(ShortString),aDest);
      end;
    tkWString:
      begin
      WS:=Tmp;
      TValue.Make(@WS,System.TypeInfo(WideString),aDest);
      end;
    tkUString:
      begin
      US:=Tmp;
      TValue.Make(@US,System.TypeInfo(UnicodeString),aDest);
      end;
    tkLString:
      begin
      SetString(RS,PAnsiChar(@Tmp),1);
      SetCodePage(RS,GetTypeData(aDestType)^.CodePage);
      TValue.Make(@RS,aDestType,aDest);
      end;
  else
    aRes:=False;
  end;

end;

Procedure TValue.CastEnumToEnum(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

  Function GetEnumBaseType(aType : PTypeInfo) : PTypeInfo;

  begin
    if aType^.Kind=tkEnumeration then
      Result:=GetTypeData(aType)^.BaseType
    else
      Result:=Nil;
  end;

var
  N : NativeInt;
  BoolType : PTypeInfo;
begin
  N:=AsOrdinal;
  if IsBoolType(FData.FTypeInfo) and IsBoolType(aDestType) then
    begin
    aRes:=True;
    BoolType:=GetEnumBaseType(aDestType);
    if (N<>0) then
      if (BoolType=System.TypeInfo(Boolean)) then
        N:=Ord(True)
      else
        N:=-1;
    TValue.Make(NativeInt(N),aDestType,aDest)
    end
  else
    begin
    aRes:=GetEnumBaseType(FData.FTypeInfo)=GetEnumBaseType(aDestType);
    if aRes then
      TValue.Make(NativeInt(N), aDestType, aDest);
    end;
end;


Procedure TValue.CastFloatToFloat(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Ti : PTypeInfo;
  S : Single;
  D : Double;
  E : Extended;
  Co : Comp;
  Cu : Currency;

begin
  // Destination float type
  ti:=FloatTypeToTypeInfo(GetTypeData(aDestType)^.FloatType);
  case TypeData^.FloatType of
    ftSingle:
      begin
      S:=AsSingle;
      TValue.Make(@S,Ti,aDest);
      end;
    ftDouble:
      begin
      D:=AsDouble;
      TValue.Make(@D,Ti,aDest);
      end;
    ftExtended:
      begin
      E:=AsExtended;
      TValue.Make(@E,Ti,aDest);
      end;
    ftComp:
      begin
      Co:=FData.FAsComp;
      TValue.Make(@Co,Ti,aDest);
      end;
    ftCurr:
      begin
      Cu:=AsCurrency;
      TValue.Make(@Cu,Ti,aDest);
      end;
    end;
  aRes:=True;
  // This is for TDateTime, TDate, TTime
  aDest.FData.FTypeInfo:=aDestType;
end;

Procedure TValue.CastStringToString(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  US : UnicodeString;
  RS : RawByteString;
  WS : WideString;
  SS : ShortString;

begin
  aRes:=False;
  US:=AsUnicodeString;
  case aDestType^.Kind of
  tkUString:
    TValue.Make(@US,aDestType,aDest);
  tkWString:
    begin
    WS:=US;
    TValue.Make(@WS,aDestType,aDest);
    end;
  tkString:
    begin
    RS:=AnsiString(US);
    if Length(RS)>GetTypeData(aDestType)^.MaxLength then
      Exit;
    SS:=RS;
    TValue.Make(@SS,aDestType,aDest);
    end;
  tkChar:
    begin
    RS:=AnsiString(US);
    if Length(RS)<>1 then
      Exit;
    TValue.Make(PAnsiChar(RS),aDestType,aDest);
    end;
  tkLString:
    begin
    SetString(RS,PAnsiChar(US),Length(US));
    TValue.Make(@RS, aDestType, aDest);
    end;
  tkWChar:
    begin
    if Length(US)<>1 then
      Exit;
    TValue.Make(PWideChar(US),aDestType,aDest);
    end;
  else
     // silence compiler warning
  end;
  aRes:=True;
end;

Procedure TValue.CastClassToClass(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : TObject;
  aClass : TClass;

begin
  Tmp:=AsObject;
  aClass:=GetTypeData(aDestType)^.ClassType;
  aRes:=Tmp.InheritsFrom(aClass);
  if aRes then
    TValue.Make(IntPtr(Tmp),aDestType,aDest);
end;

Procedure TValue.CastClassRefToClassRef(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Cfrom,Cto: TClass;

begin
  ExtractRawData(@CFrom);
  Cto:=GetTypeData(GetTypeData(aDestType)^.InstanceType)^.ClassType;
  aRes:=(cFrom=nil) or (Cfrom.InheritsFrom(cTo));
  if aRes then
    TValue.Make(PtrInt(cFrom),aDestType,aDest);
end;

Procedure TValue.CastClassToInterface(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  aGUID : TGUID;
  P : Pointer;

begin
  aRes:=False;
  aGUID:=GetTypeData(aDestType)^.Guid;
  if IsEqualGUID(GUID_NULL,aGUID) then
    Exit;
  aRes:=TObject(AsObject).GetInterface(aGUID,P);
  if aRes then
    TValue.Make(@P,aDestType,aDest);
end;

Procedure TValue.CastInterfaceToInterface(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Parent: PTypeData;
  Tmp : Pointer;

begin
  aRes:=(aDestType=TypeInfo) or (aDestType=System.TypeInfo(IInterface));
  if not aRes then
    begin
    Parent:=GetTypeData(TypeInfo);
    while (not aRes) and Assigned(Parent) and Assigned(Parent^.IntfParent) do
      begin
      aRes:=(Parent^.IntfParent=aDestType);
      if not aRes then
        Parent:=GetTypeData(Parent^.IntfParent);
      end;
    end;
  if not aRes then
    exit;
  ExtractRawDataNoCopy(@Tmp);
  TValue.Make(@Tmp,aDestType,aDest);
end;

Procedure TValue.CastQWordToInteger(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : QWord;
  N : NativeInt;

begin
  aRes:=True;
  Tmp:=FData.FAsUInt64;
  case GetTypeData(aDestType)^.OrdType of
    otSByte: N:=NativeInt(Int8(Tmp));
    otSWord: N:=NativeInt(Int16(Tmp));
    otSLong: N:=NativeInt(Int32(Tmp));
    otUByte: N:=NativeInt(UInt8(Tmp));
    otUWord: N:=NativeInt(UInt16(Tmp));
    otULong: N:=NativeInt(UInt32(Tmp));
  else
    aRes:=False;
  end;
  if aRes then
    TValue.Make(N, aDestType, aDest);
end;

Procedure TValue.CastInt64ToInteger(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: Int64;
  N : NativeInt;

begin
  Tmp:=FData.FAsSInt64;
  aRes:=True;
  case GetTypeData(aDestType)^.OrdType of
    otSByte: N:=NativeInt(Int8(Tmp));
    otSWord: N:=NativeInt(Int16(Tmp));
    otSLong: N:=NativeInt(Int32(Tmp));
    otUByte: N:=NativeInt(UInt8(Tmp));
    otUWord: N:=NativeInt(UInt16(Tmp));
    otULong: N:=NativeInt(UInt32(Tmp));
  else
    aRes:=False;
  end;
  if aRes then
    TValue.Make(N, aDestType, aDest);
end;

Procedure TValue.CastQWordToInt64(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : QWord;

begin
  Tmp:=FData.FAsUInt64;
  TValue.Make(@Tmp,System.TypeInfo(Int64),aDest);
  aRes:=True;
end;


Procedure TValue.CastInt64ToQWord(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : Int64;

begin
  Tmp:=FData.FAsSInt64;
  TValue.Make(@Tmp,System.TypeInfo(QWord),aDest);
  aRes:=True;
end;


Procedure TValue.CastQWordToFloat(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : QWord;
  Ti : PTypeInfo;

begin
  Tmp:=FData.FAsUInt64;
  Ti:=FloatTypeToTypeInfo(GetTypeData(aDestType)^.FloatType);
  TValue.Make(@Tmp,Ti,aDest);
  aRes:=True;
end;

Procedure TValue.CastInt64ToFloat(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : Int64;
  Ti : PTypeInfo;
begin
  Tmp:=AsInt64;
  Ti:=FloatTypeToTypeInfo(GetTypeData(aDestType)^.FloatType);
  TValue.Make(@Tmp,Ti,aDest);
  aRes:=True;
end;

Procedure TValue.CastFloatToInteger(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: Int64;
  DTD : PTypeData;

begin
  aRes:=TypeData^.FloatType=ftComp;
  if not aRes then
    Exit;
  Tmp:=FData.FAsSInt64;
  DTD:=GetTypeData(aDestType);
  Case aDestType^.Kind of
  tkInteger:
    begin
    with DTD^ do
      if MinValue<=MaxValue then
        aRes:=(Tmp>=MinValue) and (Tmp<=MaxValue)
      else
        aRes:=(Tmp>=Cardinal(MinValue)) and (Tmp<=Cardinal(MaxValue))
    end;
  tkInt64:
    With DTD^ do
      aRes:=(Tmp>=MinInt64Value) and (Tmp<=MaxInt64Value);
  tkQWord:
    With DTD^ do
      aRes:=(Tmp>=0) and (QWord(Tmp)>=Qword(MinInt64Value)) and (QWord(Tmp)<=UInt64(MaxInt64Value));
  else
    aRes:=False;
  end;
  if aRes then
    TValue.Make(@Tmp, aDestType, aDest);
end;

Procedure TValue.CastFromVariant(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : Variant;
  tmpBool: Boolean;
  tmpExtended: Extended;
  tmpShortString: ShortString;

  VarType: TVarType;
  DataPtr: Pointer;
  DataType: PTypeInfo;

begin
  aRes:=False;
  Tmp:=AsVariant;
  if VarIsNull(Tmp) and NullStrictConvert then
    Exit;
  if not TypeInfoToVarType(aDestType,VarType) then
    exit;
  try
    Tmp:=VarAsType(Tmp,VarType);
  except
    Exit;
  end;
  DataType:=nil;
  DataPtr:=@TVarData(Tmp).VBoolean;
  if not VarTypeToTypeInfo(TVarData(Tmp).VType,DataType) then
    Exit;
  if DataType=Nil then
    begin
    aDest:=TValue.Empty;
    aRes:=True;
    Exit;
    end;
  // Some special cases
  if (DataType=System.TypeInfo(Boolean)) then
    begin
    tmpBool:=TVarData(Tmp).VBoolean=True;
    DataPtr:=@tmpBool;
    end
  else if (DataType=System.TypeInfo(Double)) then
    begin
    if GetTypeData(aDestType)^.FloatType=ftExtended then
      begin
      tmpExtended:=Extended(TVarData(Tmp).VDouble);
      DataPtr:=@tmpExtended;
      DataType:=System.TypeInfo(Extended);
      end
    end
  else if (DataType=System.TypeInfo(ShortString)) then
    begin
    tmpShortString:=RawByteString(TVarData(tmp).VString);
    DataPtr:=@tmpShortString;
    end;
  TValue.Make(DataPtr,DataType,aDest);
  aRes:=True;
end;


Procedure TValue.CastToVariant(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp: Variant;

begin
  aRes:=False;
  case Self.Kind of
    tkChar:
      Tmp:=Specialize AsType<AnsiChar>;
    tkString,
    tkLString,
    tkWString,
    tkUString:
      Tmp:=AsString;
    tkWChar:
      Tmp:=WideChar(FData.FAsUWord);
    tkClass:
      Tmp:=PtrInt(AsObject);
    tkInterface:
      Tmp:=AsInterface;
    tkInteger:
      begin
        case TypeData^.OrdType of
          otSByte: Tmp:=FData.FAsSByte;
          otUByte: Tmp:=FData.FAsUByte;
          otSWord: Tmp:=FData.FAsSWord;
          otUWord: Tmp:=FData.FAsUWord;
          otSLong: Tmp:=FData.FAsSLong;
          otULong: Tmp:=FData.FAsULong;
          otSQWord: Tmp:=FData.FAsSInt64;
          otUQWord: Tmp:=FData.FAsUInt64;
        end;
      end;
    tkFloat:
      if IsDateTime then
        Tmp:=TDateTime(FData.FAsDouble)
      else
        case TypeData^.FloatType of
          ftSingle,
          ftDouble,
          ftExtended:
            Tmp:=AsExtended;
          ftComp:
            Tmp:=FData.FAsComp;
          ftCurr:
            Tmp:=FData.FAsCurr;
        end;
    tkInt64:
      Tmp:=AsInt64;
    tkQWord:
      Tmp:=AsUInt64;
    tkEnumeration:
      if IsType(System.TypeInfo(Boolean)) then
        Tmp:=AsBoolean
      else
        Tmp:=AsOrdinal;
  else
    Exit;
  end;
  if aDestType=System.TypeInfo(OleVariant) then
    TValue.Make(@Tmp,System.TypeInfo(OleVariant),aDest)
  else
    TValue.Make(@Tmp,System.TypeInfo(Variant),aDest);
  aRes:=True;
end;

Procedure TValue.CastVariantToVariant(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  Tmp : Variant;


begin
  if (TypeInfo=aDestType) then
    aDest:=Self
  else
    begin
    Tmp:=AsVariant;
    if (aDestType=System.TypeInfo(OleVariant)) then
      TValue.Make(@Tmp,System.TypeInfo(OleVariant),aDest)
    else
      TValue.Make(@Tmp,System.TypeInfo(Variant),aDest);
    end;
  aRes:=True;
end;

Procedure TValue.CastSetToSet(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

var
  sMax, dMax, sMin, dMin : Integer;
  TD : PTypeData;

begin
  aRes:=False;
  TD:=TypeData;
  TD:=GetTypeData(TD^.CompType);
  sMin:=TD^.MinValue;
  sMax:=TD^.MaxValue;
  TD:=GetTypeData(aDestType);
  TD:=GetTypeData(TD^.CompType);
  dMin:=TD^.MinValue;
  dMax:=TD^.MaxValue;
  aRes:=(sMin=dMin) and (sMax=dMax);
  if aRes then
    begin
    TValue.Make(GetReferenceToRawData, aDestType, aDest);
    aRes:=true;
    end
end;

Procedure TValue.CastFromInteger(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkChar: CastIntegerToInteger(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
    tkInt64 : CastIntegerToInt64(aRes,aDest,aDestType);
    tkQWord : CastIntegerToQWord(aRes,aDest,aDestType);
    tkFloat : CastIntegerToFloat(aRes,aDest,aDestType);
  else
    aRes:=False
  end;
end;

Procedure TValue.CastFromAnsiChar(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  case aDestType^.Kind of
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString : CastCharToString(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False
  end;
end;

Procedure TValue.CastFromWideChar(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  case aDestType^.Kind of
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString : CastWCharToString(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;


Procedure TValue.CastFromEnum(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  case aDestType^.Kind of
    tkEnumeration : CastEnumToEnum(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=false;
  end;
end;


Procedure TValue.CastFromFloat(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  case aDestType^.Kind of
    tkInt64,
    tkQWord,
    tkInteger : CastFloatToInteger(aRes,aDest,aDestType);
    tkFloat : CastFloatToFloat(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;


Procedure TValue.CastFromString(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString,
    tkChar : CastStringToString(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end
end;

Procedure TValue.CastFromSet(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkSet : CastSetToSet(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;


Procedure TValue.CastFromClass(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkClass : CastClassToClass(aRes,aDest,aDestType);
    tkInterfaceRaw,
    tkInterface : CastClassToInterface(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;


Procedure TValue.CastFromInterface(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkInterfaceRaw,
    tkInterface : CastInterfaceToInterface(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;


Procedure TValue.DoCastFromVariant(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkInteger,
    tkChar,
    tkEnumeration,
    tkFloat,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkInt64,
    tkQWord,
    tkUnicodeString : CastFromVariant(aRes,aDest,aDestType);
    tkVariant : CastVariantToVariant(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;

Procedure TValue.CastFromPointer(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkPointer, tkProcedure: CastAssign(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;

Procedure TValue.CastFromInt64(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkInteger: CastInt64ToInteger(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
    tkInt64 : CastAssign(aRes,aDest,aDestType);
    tkQWord : CastInt64ToQWord(aRes,aDest,aDestType);
    tkFloat : CastInt64ToFloat(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;

Procedure TValue.CastFromQWord(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case aDestType^.Kind of
    tkInteger: CastQWordToInteger(aRes,aDest,aDestType);
    tkVariant : CastToVariant(aRes,aDest,aDestType);
    tkInt64 : CastQWordToInt64(aRes,aDest,aDestType);
    tkQWord : CastAssign(aRes,aDest,aDestType);
    tkFloat : CastQWordToFloat(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;

Procedure TValue.CastFromType(out aRes : Boolean; out ADest: TValue; aDestType: PTypeInfo);

begin
  Case Kind of
    tkInteger : CastFromInteger(aRes,aDest,aDestType);
    tkChar : CastFromAnsiChar(aRes,aDest,aDestType);
    tkEnumeration : CastFromEnum(aRes,aDest,aDestType);
    tkFloat : CastFromFloat(aRes,aDest,aDestType);
    tkLString,
    tkWString,
    tkUstring,
    tkSString : CastFromString(aRes,aDest,aDestType);
    tkSet : CastFromSet(aRes,aDest,aDestType);
    tkWChar : CastFromWideChar(aRes,aDest,aDestType);
    tkInterfaceRaw,
    tkInterface : CastFromInterface(aRes,aDest,aDestType);
    tkVariant : DoCastFromVariant(aRes,aDest,aDestType);
    tkInt64 : CastFromInt64(aRes,aDest,aDestType);
    tkQWord : CastFromQWord(aRes,aDest,aDestType);
    tkClassRef : begin
                 aRes:=(aDestType^.kind=tkClassRef);
                 if aRes then
                   CastClassRefToClassRef(aRes,aDest,aDestType);
                 end;

    tkProcedure,
    tkPointer : CastFromPointer(aRes,aDest,aDestType);
  else
    aRes:=False;
  end;
end;


class procedure TValue.Make(ABuffer: pointer; ATypeInfo: PTypeInfo; out result: TValue);
type
  PMethod = ^TMethod;
var
  td: PTypeData;
begin
  result.Init;
  result.FData.FTypeInfo:=ATypeInfo;
  if not Assigned(ATypeInfo) then
    Exit;
  { first handle those types that need a TValueData implementation }
  case ATypeInfo^.Kind of
    tkSString  : begin
                   td := GetTypeData(ATypeInfo);
                   result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, td^.MaxLength + 1, ATypeInfo, True);
                 end;
    tkWString,
    tkUString,
    tkAString  : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkDynArray : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkArray    : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.ArrayData.Size, ATypeInfo, False);
    tkObject,
    tkRecord   : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.RecSize, ATypeInfo, False);
    tkVariant  : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, SizeOf(Variant), ATypeInfo, False);
    tkInterface: result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
  else
    // Silence compiler warning
  end;
  if not Assigned(ABuffer) then
    Exit;
  { now handle those that are happy with the variant part of FData }
  case ATypeInfo^.Kind of
    tkSString,
    tkWString,
    tkUString,
    tkAString,
    tkDynArray,
    tkArray,
    tkObject,
    tkRecord,
    tkVariant,
    tkInterface:
      { ignore }
      ;
    tkClass    : result.FData.FAsObject := PPointer(ABuffer)^;
    tkClassRef : result.FData.FAsClass := PClass(ABuffer)^;
    tkInterfaceRaw : result.FData.FAsPointer := PPointer(ABuffer)^;
    tkInt64    : result.FData.FAsSInt64 := PInt64(ABuffer)^;
    tkQWord    : result.FData.FAsUInt64 := PQWord(ABuffer)^;
    tkProcVar  : result.FData.FAsMethod.Code := PCodePointer(ABuffer)^;
    tkMethod   : result.FData.FAsMethod := PMethod(ABuffer)^;
    tkPointer  : result.FData.FAsPointer := PPointer(ABuffer)^;
    tkSet      : begin
                   td := GetTypeData(ATypeInfo);
                   case td^.OrdType of
                     otUByte: begin
                       { this can either really be 1 Byte or a set > 32-bit, so
                         check the underlying type }
                       if not (td^.CompType^.Kind in [tkInteger,tkEnumeration]) then
                         raise Exception.CreateFmt(SErrUnableToGetValueForType,[ATypeInfo^.Name]);
                       case td^.SetSize of
                         0, 1:
                           Result.FData.FAsUByte := PByte(ABuffer)^;
                         { these two cases shouldn't happen, but better safe than sorry... }
                         2:
                           Result.FData.FAsUWord := PWord(ABuffer)^;
                         3, 4:
                           Result.FData.FAsULong := PLongWord(ABuffer)^;
                         { maybe we should also allow storage as otUQWord? }
                         5..8:
                           Result.FData.FAsUInt64 := PQWord(ABuffer)^;
                         else
                           Result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, td^.SetSize, ATypeInfo, False);
                       end;
                     end;
                     otUWord:
                       Result.FData.FAsUWord := PWord(ABuffer)^;
                     otULong:
                       Result.FData.FAsULong := PLongWord(ABuffer)^;
                     else
                       { ehm... Panic? }
                       raise Exception.CreateFmt(SErrUnableToGetValueForType,[ATypeInfo^.Name]);
                   end;
                 end;
    tkChar,
    tkWChar,
    tkUChar,
    tkEnumeration,
    tkInteger  : begin
                   case GetTypeData(ATypeInfo)^.OrdType of
                     otSByte: result.FData.FAsSByte := PShortInt(ABuffer)^;
                     otUByte: result.FData.FAsUByte := PByte(ABuffer)^;
                     otSWord: result.FData.FAsSWord := PSmallInt(ABuffer)^;
                     otUWord: result.FData.FAsUWord := PWord(ABuffer)^;
                     otSLong: result.FData.FAsSLong := PLongInt(ABuffer)^;
                     otULong: result.FData.FAsULong := PLongWord(ABuffer)^;
                   else
                     // Silence compiler warning
                   end;
                 end;
    tkBool     : begin
                   case GetTypeData(ATypeInfo)^.OrdType of
                     otUByte: result.FData.FAsUByte := Byte(System.PBoolean(ABuffer)^);
                     otUWord: result.FData.FAsUWord := Word(PBoolean16(ABuffer)^);
                     otULong: result.FData.FAsULong := DWord(PBoolean32(ABuffer)^);
                     otUQWord: result.FData.FAsUInt64 := QWord(PBoolean64(ABuffer)^);
                     otSByte: result.FData.FAsSByte := ShortInt(PByteBool(ABuffer)^);
                     otSWord: result.FData.FAsSWord := SmallInt(PWordBool(ABuffer)^);
                     otSLong: result.FData.FAsSLong := LongInt(PLongBool(ABuffer)^);
                     otSQWord: result.FData.FAsSInt64 := Int64(PQWordBool(ABuffer)^);
                   end;
                 end;
    tkFloat    : begin
                   case GetTypeData(ATypeInfo)^.FloatType of
                     ftCurr   : result.FData.FAsCurr := PCurrency(ABuffer)^;
                     ftSingle : result.FData.FAsSingle := PSingle(ABuffer)^;
                     ftDouble : result.FData.FAsDouble := PDouble(ABuffer)^;
                     ftExtended: result.FData.FAsExtended := PExtended(ABuffer)^;
                     ftComp   : result.FData.FAsComp := PComp(ABuffer)^;
                   end;
                 end;
  else
    raise Exception.CreateFmt(SErrUnableToGetValueForType,[ATypeInfo^.Name]);
  end;
end;


class procedure TValue.MakeOpenArray(AArray: Pointer; ALength: SizeInt; ATypeInfo: PTypeInfo; out Result: TValue);
var
  el: TValue;
begin
  Result.FData.FTypeInfo := ATypeInfo;
  { resets the whole variant part; FValueData is already Nil }
{$if SizeOf(TMethod) > SizeOf(QWord)}
  Result.FData.FAsMethod.Code := Nil;
  Result.FData.FAsMethod.Data := Nil;
{$else}
  Result.FData.FAsUInt64 := 0;
{$endif}
  if not Assigned(ATypeInfo) then
    Exit;
  if ATypeInfo^.Kind <> tkArray then
    Exit;
  if not Assigned(AArray) then
    Exit;
  if ALength < 0 then
    Exit;
  Result.FData.FValueData := TValueDataIntImpl.CreateRef(@AArray, ATypeInfo, False);
  Result.FData.FArrLength := ALength;
  Make(Nil, Result.TypeData^.ArrayData.ElType, el);
  Result.FData.FElSize := el.DataSize;
end;

class function TValue.FromOrdinal(aTypeInfo: PTypeInfo; aValue: Int64): TValue;
{$ifdef ENDIAN_BIG}
var
  p: PByte;
  td: PTypeData;
{$endif}
begin
  if not Assigned(aTypeInfo) or
      not (aTypeInfo^.Kind in [tkInteger, tkInt64, tkQWord, tkEnumeration, tkBool, tkChar, tkWChar, tkUChar]) then
    raise EInvalidCast.Create(SErrInvalidTypecast);

{$ifdef ENDIAN_BIG}
  td := GetTypeData(aTypeInfo);
  p := @aValue;
  case td^.OrdType of
    otSByte,
    otUByte:
      p := p + 7;
    otSWord,
    otUWord:
      p := p + 6;
    otSLong,
    otULong:
      p := p + 4;
    otSQWord,
    otUQWord: ;
  end;
  TValue.Make(p, aTypeInfo, Result);
{$else}
  TValue.Make(@aValue, aTypeInfo, Result);
{$endif}
end;

class function TValue.FromArray(aArrayTypeInfo: PTypeInfo; const aValues: array of TValue): TValue; static;
var
  i, sz: SizeInt;
  data: TValueDataIntImpl;
begin
  Result.Init;
  Result.FData.FTypeInfo := aArrayTypeInfo;
  if not Assigned(aArrayTypeInfo) then
    Exit;
  if aArrayTypeInfo^.Kind = tkDynArray then begin
    data := TValueDataIntImpl.CreateRef(Nil, aArrayTypeInfo, True);
    sz := Length(aValues);
    DynArraySetLength(data.FBuffer, aArrayTypeInfo, 1, @sz);
    Result.FData.FValueData := data;
  end else if aArrayTypeInfo^.Kind = tkArray then begin
    if Result.GetArrayLength <> Length(aValues) then
      raise ERtti.CreateFmt(SErrLengthOfArrayMismatch, [Length(aValues), Result.GetArrayLength]);
    Result.FData.FValueData := TValueDataIntImpl.CreateCopy(Nil, Result.TypeData^.ArrayData.Size, aArrayTypeInfo, False);
  end else
    raise ERtti.CreateFmt(SErrTypeKindNotSupported, [aArrayTypeInfo^.Name]);
  for i := 0 to High(aValues) do
    Result.SetArrayElement(i, aValues[i]);
end;

class function TValue.FromVarRec(const aValue: TVarRec): TValue;

begin
  Result:=Default(TValue);
  case aValue.VType of
    vtInteger: Result:=aValue.VInteger;
    vtBoolean: Result:=aValue.VBoolean;
    vtWideChar: TValue.Make(@aValue.VWideChar,System.TypeInfo(WideChar),Result);
    vtInt64: Result:=aValue.VInt64^;
    vtQWord: Result:=aValue.VQWord^;
    vtChar: TValue.Make(@aValue.VChar,System.TypeInfo(AnsiChar),Result);
    vtPChar: Result:=string(aValue.VPChar);
    vtPWideChar: Result:=widestring(aValue.VPWideChar);
    vtString: Result:=aValue.VString^;
    vtWideString: Result:=WideString(aValue.VWideString);
    vtAnsiString: Result:=AnsiString(aValue.VAnsiString);
    vtUnicodeString: Result:=UnicodeString(aValue.VUnicodeString);
    vtObject: Result:=TObject(aValue.VObject);
    vtPointer: TValue.Make(@aValue.VPointer,System.TypeInfo(Pointer),Result);
    vtInterface: Result:=IInterface(aValue.VInterface);
    vtClass: Result:=aValue.VClass;
    vtVariant: TValue.Make(@aValue.VVariant^,System.TypeInfo(Variant),result);
    vtExtended: Result := aValue.VExtended^;
    vtCurrency: Result := aValue.VCurrency^;
  end;
end;

class function TValue.FromVariant(const aValue : Variant) : TValue;

var
  aType : TVarType;

begin
  Result:=Default(TValue);
  aType:=TVarData(aValue).vtype;
  case aType of
    varEmpty,
    VarNull : TValue.Make(@aValue,System.TypeInfo(Variant),Result);
    varInteger : Result:=Integer(aValue);
    varSmallInt : Result:=SmallInt(aValue);
    varBoolean : Result:=Boolean(aValue);
    varOleStr: Result:=WideString(aValue);
    varInt64: Result:=Int64(aValue);
    varQWord: Result:=QWord(aValue);
    varShortInt: Result:=ShortInt(aValue);
    varByte : Result:=Byte(aValue);
    varWord : Result:=Word(aValue);
    varLongWord : Result:=Cardinal(aValue);
    varSingle : Result:=Single(aValue);
    varDouble : Result:=Double(aValue);
    varDate : TValue.Make(@TVarData(aValue).vDate,System.TypeInfo(TDateTime),Result);
    varDispatch : TValue.Make(@TVarData(aValue).VDispatch,System.TypeInfo(IDispatch),Result);
    varError : TValue.Make(@TVarData(aValue).vDate,System.TypeInfo(HRESULT),Result);
    varUnknown : TValue.Make(@TVarData(aValue).vunknown,System.TypeInfo(IUnknown),Result);
    varCurrency : Result:=Currency(aValue);
    varString : Result:=AnsiString(aValue);
    varUString : Result:=UnicodeString(TVarData(aValue).vustring);
  else
    raise EVariantTypeCastError.CreateFmt('Invalid variant cast from type %d',[aType]);
  end;
end;


function TValue.GetIsEmpty: boolean;
begin
  result := (FData.FTypeInfo=nil) or
            ((Kind in [tkSString, tkObject, tkRecord, tkArray]) and not Assigned(FData.FValueData)) or
            ((Kind in [tkClass, tkClassRef, tkInterfaceRaw]) and not Assigned(FData.FAsPointer));
end;

function TValue.IsArray: boolean;
begin
  result := kind in [tkArray, tkDynArray];
end;

function TValue.IsOpenArray: Boolean;
var
  td: PTypeData;
begin
  td := TypeData;
  Result := (Kind = tkArray) and (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0)
end;


function TValue.AsUnicodeString: UnicodeString;

begin
  if (Kind in [tkSString, tkAString, tkUString, tkWString]) and not Assigned(FData.FValueData) then
    Result := ''
  else
    case Kind of
      tkSString:
        Result := UnicodeString(PShortString(FData.FValueData.GetReferenceToRawData)^);
      tkAString:
        Result := UnicodeString(PAnsiString(FData.FValueData.GetReferenceToRawData)^);
      tkWString:
        Result := UnicodeString(PWideString(FData.FValueData.GetReferenceToRawData)^);
      tkUString:
        Result := UnicodeString(PUnicodeString(FData.FValueData.GetReferenceToRawData)^);
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
end;

function TValue.AsAnsiString: AnsiString;
begin
  if (Kind in [tkSString, tkAString, tkUString, tkWString]) and not Assigned(FData.FValueData) then
    Result := ''
  else
    case Kind of
      tkSString:
        Result := AnsiString(PShortString(FData.FValueData.GetReferenceToRawData)^);
      tkAString:
        Result := AnsiString(PAnsiString(FData.FValueData.GetReferenceToRawData)^);
      tkWString:
        Result := AnsiString(PWideString(FData.FValueData.GetReferenceToRawData)^);
      tkUString:
        Result := AnsiString(PUnicodeString(FData.FValueData.GetReferenceToRawData)^);
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
end;

function TValue.AsExtended: Extended;
begin
  if Kind = tkFloat then
    begin
    case TypeData^.FloatType of
      ftSingle   : result := FData.FAsSingle;
      ftDouble   : result := FData.FAsDouble;
      ftExtended : result := FData.FAsExtended;
      ftCurr     : result := FData.FAsCurr;
      ftComp     : result := FData.FAsComp;
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
    end
  else if Kind in [tkInteger, tkInt64, tkQWord] then
    Result := AsInt64
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;



function TValue.TryCast(aTypeInfo: PTypeInfo; out aResult: TValue; const aEmptyAsAnyType: Boolean = True): Boolean;

begin
  Result:=False;
  if aEmptyAsAnyType and IsEmpty then
    begin
    aResult:=TValue.Empty;
    if (aTypeInfo=nil) then
      exit;
    AResult.FData.FTypeInfo:=aTypeInfo;
    Exit(True);
    end;
  if not aEmptyAsAnyType and (Self.TypeInfo=nil) then
    Exit;
  if (Self.TypeInfo=ATypeInfo) then
    begin
    aResult:=Self;
    Exit(True);
    end;
  if Not Assigned(aTypeInfo) then
    Exit;
  if (aTypeInfo=System.TypeInfo(TValue)) then
    begin
    TValue.Make(@Self,System.TypeInfo(TValue),aResult);
    Exit(True);
    end;
  CastFromType(Result,aResult,ATypeInfo);
end;

function TValue.Cast(aTypeInfo: PTypeInfo; const aEmptyAsAnyType: Boolean = True): TValue; overload;

begin
  if not TryCast(aTypeInfo,Result,aEmptyAsAnyType) then
    raise EInvalidCast.Create(SInvalidCast);
end;

{$ifndef NoGenericMethods}

generic function TValue.AsType<T>(const aEmptyAsAnyType: Boolean = True): T;

begin
  if not (specialize TryAsType<T>(Result,aEmptyAsAnyType)) then
    raise EInvalidCast.Create(SInvalidCast);
end;

generic function TValue.Cast<T>(const aEmptyAsAnyType: Boolean = True): TValue; overload;

var
  Info : PTypeInfo;

begin
  Info:=System.TypeInfo(T);
  if not TryCast(Info,Result,aEmptyAsAnyType) then
    raise EInvalidCast.Create(SInvalidCast);
end;

generic function TValue.TryAsType<T>(out aResult: T; const aEmptyAsAnyType: Boolean = True): Boolean; inline;

var
  Tmp: TValue;
  Info : PTypeInfo;  
begin
  Info:=System.TypeInfo(T);
  Result:=TryCast(Info,Tmp,aEmptyAsAnyType);
  if Result then
    if Assigned(Tmp.TypeInfo) then
      Tmp.ExtractRawData(@aResult)
    else  
      aResult:=Default(T);
end;   
{$endif}

function TValue.AsObject: TObject;
begin
  if IsObject or (IsClass and not Assigned(FData.FAsObject)) then
    result := TObject(FData.FAsObject)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsClass: TClass;
begin
  if IsClass then
    result := FData.FAsClass
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsBoolean: boolean;
begin
  if (Kind = tkBool) then
    case TypeData^.OrdType of
      otSByte:  Result := ByteBool(FData.FAsSByte);
      otUByte:  Result := Boolean(FData.FAsUByte);
      otSWord:  Result := WordBool(FData.FAsSWord);
      otUWord:  Result := Boolean16(FData.FAsUWord);
      otSLong:  Result := LongBool(FData.FAsSLong);
      otULong:  Result := Boolean32(FData.FAsULong);
      otSQWord: Result := QWordBool(FData.FAsSInt64);
      otUQWord: Result := Boolean64(FData.FAsUInt64);
    end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsOrdinal: Int64;
begin
  if IsOrdinal then
    if Kind in [tkClass, tkClassRef, tkInterfaceRaw, tkUnknown] then
      Result := 0
    else
      case TypeData^.OrdType of
        otSByte:  Result := FData.FAsSByte;
        otUByte:  Result := FData.FAsUByte;
        otSWord:  Result := FData.FAsSWord;
        otUWord:  Result := FData.FAsUWord;
        otSLong:  Result := FData.FAsSLong;
        otULong:  Result := FData.FAsULong;
        otSQWord: Result := FData.FAsSInt64;
        otUQWord: Result := FData.FAsUInt64;
      end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;


function TValue.AsCurrency: Currency;
begin
  if (Kind = tkFloat) and (TypeData^.FloatType=ftCurr) then
    result := FData.FAsCurr
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsSingle: Single;

begin
  if Kind = tkFloat then
    begin
    case TypeData^.FloatType of
      ftSingle   : result := FData.FAsSingle;
      ftDouble   : result := FData.FAsDouble;
      ftExtended : result := FData.FAsExtended;
      ftCurr     : result := FData.FAsCurr;
      ftComp     : result := FData.FAsComp;
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
    end
  else if Kind in [tkInteger, tkInt64, tkQWord] then
    Result := AsInt64
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsDateTime: TDateTime;

begin
  if (Kind = tkFloat) and (TypeData^.FloatType=ftDouble) and IsDateTimeType(TypeInfo) then
    result := FData.FAsDouble
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;


function TValue.AsDouble: Double;
begin
  if Kind = tkFloat then
    begin
    case TypeData^.FloatType of
      ftSingle   : result := FData.FAsSingle;
      ftDouble   : result := FData.FAsDouble;
      ftExtended : result := FData.FAsExtended;
      ftCurr     : result := FData.FAsCurr;
      ftComp     : result := FData.FAsComp;
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
    end
  else if Kind in [tkInteger, tkInt64, tkQWord] then
    Result := AsInt64
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;


function TValue.AsError: HRESULT;

begin
  if (Kind = tkInteger) and (TypeInfo=System.TypeInfo(HRESULT)) then
    result := HResult(AsInteger)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInteger: Integer;
begin
  if Kind in [tkInteger, tkInt64, tkQWord] then
    case TypeData^.OrdType of
      otSByte:  Result := FData.FAsSByte;
      otUByte:  Result := FData.FAsUByte;
      otSWord:  Result := FData.FAsSWord;
      otUWord:  Result := FData.FAsUWord;
      otSLong:  Result := FData.FAsSLong;
      otULong:  Result := FData.FAsULong;
      otSQWord: Result := FData.FAsSInt64;
      otUQWord: Result := FData.FAsUInt64;
    end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsAnsiChar: AnsiChar;
begin
  if Kind = tkChar then
    Result := Chr(FData.FAsUByte)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsWideChar: WideChar;
begin
  if Kind = tkWChar then
    Result := WideChar(FData.FAsUWord)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsChar: AnsiChar;
begin
{$if SizeOf(AnsiChar) = 1}
  Result := AsAnsiChar;
{$else}
  Result := AsWideChar;
{$endif}
end;

function TValue.AsPointer : Pointer;

begin
  if Kind in [tkPointer, tkInterface, tkInterfaceRaw, tkClass,tkClassRef,tkAString,tkWideString,tkUnicodeString] then
    Result:=FData.FAsPointer
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsVariant : Variant;

begin
  if (Kind=tkVariant) then
    Result:= PVariant(FData.FValueData.GetReferenceToRawData)^
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInt64: Int64;
begin
  if Kind in [tkInteger, tkInt64, tkQWord] then
    case TypeData^.OrdType of
      otSByte:  Result := FData.FAsSByte;
      otUByte:  Result := FData.FAsUByte;
      otSWord:  Result := FData.FAsSWord;
      otUWord:  Result := FData.FAsUWord;
      otSLong:  Result := FData.FAsSLong;
      otULong:  Result := FData.FAsULong;
      otSQWord: Result := FData.FAsSInt64;
      otUQWord: Result := FData.FAsUInt64;
    end
  else if (Kind = tkFloat) and (TypeData^.FloatType = ftComp) then
    Result := Int64(FData.FAsComp)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsUInt64: QWord;
begin
  if Kind in [tkInteger, tkInt64, tkQWord] then
    case TypeData^.OrdType of
      otSByte:  Result := FData.FAsSByte;
      otUByte:  Result := FData.FAsUByte;
      otSWord:  Result := FData.FAsSWord;
      otUWord:  Result := FData.FAsUWord;
      otSLong:  Result := FData.FAsSLong;
      otULong:  Result := FData.FAsULong;
      otSQWord: Result := FData.FAsSInt64;
      otUQWord: Result := FData.FAsUInt64;
    end
  else if (Kind = tkFloat) and (TypeData^.FloatType = ftComp) then
    Result := QWord(FData.FAsComp)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsInterface: IInterface;
begin
  if Kind = tkInterface then
    Result := PInterface(FData.FValueData.GetReferenceToRawData)^
  else if (Kind in [tkClass, tkClassRef, tkUnknown]) and not Assigned(FData.FAsPointer) then
    Result := Nil
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.ToString: String;
begin
  case Kind of
    tkWString,
    tkUString : result := AsUnicodeString;
    tkSString,
    tkAString : result := AsAnsiString;
    tkInteger : result := IntToStr(AsInteger);
    tkQWord   : result := IntToStr(AsUInt64);
    tkInt64   : result := IntToStr(AsInt64);
    tkBool    : result := BoolToStr(AsBoolean, True);
    tkPointer : result := '(pointer @ ' + HexStr(FData.FAsPointer) + ')';
    tkInterface : result := '(interface @ ' + HexStr(PPointer(FData.FValueData.GetReferenceToRawData)^) + ')';
    tkInterfaceRaw : result := '(raw interface @ ' + HexStr(FData.FAsPointer) + ')';
    tkEnumeration: Result := GetEnumName(TypeInfo, Integer(AsOrdinal));
    tkChar: Result := AnsiChar(FData.FAsUByte);
    tkWChar: Result := UTF8Encode(WideChar(FData.FAsUWord));
  else
    result := '';
  end;
end;

function TValue.GetArrayLength: SizeInt;
var
  td: PTypeData;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then
    Result := DynArraySize(PPointer(FData.FValueData.GetReferenceToRawData)^)
  else begin
    td := TypeData;
    if (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0) then
      Result := FData.FArrLength
    else
      Result := td^.ArrayData.ElCount;
  end;
end;

function TValue.GetArrayElement(AIndex: SizeInt): TValue;
var
  data: Pointer;
  eltype: PTypeInfo;
  elsize: SizeInt;
  td: PTypeData;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then begin
    data := DynArrayIndex(PPointer(FData.FValueData.GetReferenceToRawData)^, [AIndex], FData.FTypeInfo);
    eltype := TypeData^.elType2;
  end else begin
    td := TypeData;
    eltype := td^.ArrayData.ElType;
    { open array? }
    if (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0) then begin
      data := PPointer(FData.FValueData.GetReferenceToRawData)^;
      elsize := FData.FElSize
    end else begin
      data := FData.FValueData.GetReferenceToRawData;
      elsize := td^.ArrayData.Size div td^.ArrayData.ElCount;
    end;
    data := PByte(data) + AIndex * elsize;
  end;
  { MakeWithoutCopy? }
  Make(data, eltype, Result);
end;

procedure TValue.SetArrayElement(AIndex: SizeInt; constref AValue: TValue);
var
  data: Pointer;
  eltype: PTypeInfo;
  elsize: SizeInt;
  td, tdv: PTypeData;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then begin
    data := DynArrayIndex(PPointer(FData.FValueData.GetReferenceToRawData)^, [AIndex], FData.FTypeInfo);
    eltype := TypeData^.elType2;
  end else begin
    td := TypeData;
    eltype := td^.ArrayData.ElType;
    { open array? }
    if (td^.ArrayData.Size = 0) and (td^.ArrayData.ElCount = 0) then begin
      data := PPointer(FData.FValueData.GetReferenceToRawData)^;
      elsize := FData.FElSize
    end else begin
      data := FData.FValueData.GetReferenceToRawData;
      elsize := td^.ArrayData.Size div td^.ArrayData.ElCount;
    end;
    data := PByte(data) + AIndex * elsize;
  end;
  { maybe we'll later on allow some typecasts, but for now be restrictive }
  if eltype^.Kind <> AValue.Kind then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  td := GetTypeData(eltype);
  tdv := AValue.TypeData;
  if ((eltype^.Kind in [tkInteger, tkBool, tkEnumeration, tkSet]) and (td^.OrdType <> tdv^.OrdType)) or
      ((eltype^.Kind = tkFloat) and (td^.FloatType <> tdv^.FloatType)) then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Assigned(AValue.FData.FValueData) and (eltype^.Kind <> tkSString) then
    IntCopy(AValue.FData.FValueData.GetReferenceToRawData, data, eltype)
  else
    Move(AValue.GetReferenceToRawData^, data^, AValue.DataSize);
end;

function TValue.TryAsOrdinal(out AResult: int64): boolean;
begin
  result := IsOrdinal;
  if result then
    AResult := AsOrdinal;
end;

function TValue.GetReferenceToRawData: Pointer;
begin
  if not Assigned(FData.FTypeInfo) then
    Result := Nil
  else if Assigned(FData.FValueData) then
    Result := FData.FValueData.GetReferenceToRawData
  else begin
    Result := Nil;
    case Kind of
      tkInteger,
      tkEnumeration,
      tkInt64,
      tkQWord,
      tkBool:
        case TypeData^.OrdType of
          otSByte:
            Result := @FData.FAsSByte;
          otUByte:
            Result := @FData.FAsUByte;
          otSWord:
            Result := @FData.FAsSWord;
          otUWord:
            Result := @FData.FAsUWord;
          otSLong:
            Result := @FData.FAsSLong;
          otULong:
            Result := @FData.FAsULong;
          otSQWord:
            Result := @FData.FAsSInt64;
          otUQWord:
            Result := @FData.FAsUInt64;
        end;
      tkSet: begin
        case TypeData^.OrdType of
          otUByte: begin
            case TypeData^.SetSize of
              1:
                Result := @FData.FAsUByte;
              2:
                Result := @FData.FAsUWord;
              3, 4:
                Result := @FData.FAsULong;
              5..8:
                Result := @FData.FAsUInt64;
              else
                { this should have gone through FAsValueData :/ }
                Result := Nil;
            end;
          end;
          otUWord:
            Result := @FData.FAsUWord;
          otULong:
            Result := @FData.FAsULong;
          else
            Result := Nil;
        end;
      end;
      tkChar:
        Result := @FData.FAsUByte;
      tkFloat:
        case TypeData^.FloatType of
          ftSingle:
            Result := @FData.FAsSingle;
          ftDouble:
            Result := @FData.FAsDouble;
          ftExtended:
            Result := @FData.FAsExtended;
          ftComp:
            Result := @FData.FAsComp;
          ftCurr:
            Result := @FData.FAsCurr;
        end;
      tkMethod:
        Result := @FData.FAsMethod;
      tkClass:
        Result := @FData.FAsObject;
      tkWChar:
        Result := @FData.FAsUWord;
      tkInterfaceRaw:
        Result := @FData.FAsPointer;
      tkProcVar:
        Result := @FData.FAsMethod.Code;
      tkUChar:
        Result := @FData.FAsUWord;
      tkFile:
        Result := @FData.FAsPointer;
      tkClassRef:
        Result := @FData.FAsClass;
      tkPointer:
        Result := @FData.FAsPointer;
      tkVariant,
      tkDynArray,
      tkArray,
      tkObject,
      tkRecord,
      tkInterface,
      tkSString,
      tkLString,
      tkAString,
      tkUString,
      tkWString:
        Assert(false, 'Managed/complex type not handled through IValueData');
    else
      // Silence compiler warning
    end;
  end;
end;

procedure TValue.ExtractRawData(ABuffer: Pointer);
begin
  if Assigned(FData.FValueData) then
    FData.FValueData.ExtractRawData(ABuffer)
  else if Assigned(FData.FTypeInfo) then
    Move((@FData.FAsPointer)^, ABuffer^, DataSize);
end;

procedure TValue.ExtractRawDataNoCopy(ABuffer: Pointer);
begin
  if Assigned(FData.FValueData) then
    FData.FValueData.ExtractRawDataNoCopy(ABuffer)
  else if Assigned(FData.FTypeInfo) then
    Move((@FData.FAsPointer)^, ABuffer^, DataSize);
end;


function Invoke(aCodeAddress: CodePointer; const aArgs: TValueArray;
  aCallConv: TCallConv; aResultType: PTypeInfo; aIsStatic: Boolean;
  aIsConstructor: Boolean): TValue;
var
  funcargs: TFunctionCallParameterArray;
  i: LongInt;
  flags: TFunctionCallFlags;
begin
  { sanity check }
  if not Assigned(FuncCallMgr[aCallConv].Invoke) then
    raise ENotImplemented.Create(SErrInvokeNotImplemented);

  { ToDo: handle IsConstructor }
  if aIsConstructor then
    raise ENotImplemented.Create(SErrInvokeNotImplemented);

  flags := [];
  if aIsStatic then
    Include(flags, fcfStatic)
  else if Length(aArgs) = 0 then
    raise EInvocationError.Create(SErrMissingSelfParam);
  funcargs:=[];
  SetLength(funcargs, Length(aArgs));
  for i := Low(aArgs) to High(aArgs) do begin
    funcargs[i - Low(aArgs) + Low(funcargs)].ValueRef := aArgs[i].GetReferenceToRawData;
    funcargs[i - Low(aArgs) + Low(funcargs)].ValueSize := aArgs[i].DataSize;
    funcargs[i - Low(aArgs) + Low(funcargs)].Info.ParamType := aArgs[i].TypeInfo;
    funcargs[i - Low(aArgs) + Low(funcargs)].Info.ParamFlags := [];
    funcargs[i - Low(aArgs) + Low(funcargs)].Info.ParaLocs := Nil;
  end;

  if Assigned(aResultType) then
    TValue.Make(Nil, aResultType, Result)
  else
    Result := TValue.Empty;

  FuncCallMgr[aCallConv].Invoke(aCodeAddress, funcargs, aCallConv, aResultType, Result.GetReferenceToRawData, flags);
end;

function Invoke(const aName: String; aCodeAddress: CodePointer; aCallConv: TCallConv; aStatic: Boolean; aInstance: TValue; constref aArgs: array of TValue; const aParams: TRttiParameterArray; aReturnType: TRttiType): TValue;
var
  param: TRttiParameter;
  unhidden, highs, i: SizeInt;
  args: TFunctionCallParameterArray;
  highargs: array of SizeInt;
  restype: PTypeInfo;
  resptr: Pointer;
  mgr: TFunctionCallManager;
  flags: TFunctionCallFlags;
begin
  mgr := FuncCallMgr[aCallConv];
  if not Assigned(mgr.Invoke) then
    raise EInvocationError.CreateFmt(SErrCallConvNotSupported, [CCToStr(aCallConv)]);

  if not Assigned(aCodeAddress) then
    raise EInvocationError.CreateFmt(SErrInvokeNoCodeAddr, [aName]);

  unhidden := 0;
  highs := 0;
  for param in aParams do begin
    if unhidden < Length(aArgs) then begin
      if pfArray in param.Flags then begin
        if Assigned(aArgs[unhidden].TypeInfo) and not aArgs[unhidden].IsArray and (aArgs[unhidden].Kind <> param.ParamType.TypeKind) then
          raise EInvocationError.CreateFmt(SErrInvokeArrayArgExpected, [param.Name, aName]);
      end else if not (pfHidden in param.Flags) then begin
        if Assigned(param.ParamType) and (aArgs[unhidden].Kind <> param.ParamType.TypeKind) then
          raise EInvocationError.CreateFmt(SErrInvokeArgInvalidType, [param.Name, aName]);
      end;
    end;
    if not (pfHidden in param.Flags) then
      Inc(unhidden);
    if pfHigh in param.Flags then
      Inc(highs);
  end;

  if unhidden <> Length(aArgs) then
    raise EInvocationError.CreateFmt(SErrInvokeArgCount, [aName, unhidden, Length(aArgs)]);

  if Assigned(aReturnType) then begin
    TValue.Make(Nil, aReturnType.FTypeInfo, Result);
    resptr := Result.GetReferenceToRawData;
    restype := aReturnType.FTypeInfo;
  end else begin
    Result := TValue.Empty;
    resptr := Nil;
    restype := Nil;
  end;

  highargs:=[];
  args:=[];
  SetLength(highargs, highs);
  SetLength(args, Length(aParams));
  unhidden := 0;
  highs := 0;

  for i := 0 to High(aParams) do begin
    param := aParams[i];
    if Assigned(param.ParamType) then
      args[i].Info.ParamType := param.ParamType.FTypeInfo
    else
      args[i].Info.ParamType := Nil;
    args[i].Info.ParamFlags := param.Flags;
    args[i].Info.ParaLocs := Nil;

    if pfHidden in param.Flags then begin
      if pfSelf in param.Flags then
        args[i].ValueRef := aInstance.GetReferenceToRawData
      else if pfResult in param.Flags then begin
        if not Assigned(restype) then
          raise EInvocationError.CreateFmt(SErrInvokeRttiDataError, [aName]);
        args[i].ValueRef := resptr;
        restype := Nil;
        resptr := Nil;
      end else if pfHigh in param.Flags then begin
        { the corresponding array argument is the *previous* unhidden argument }
        if aArgs[unhidden - 1].IsArray then
          highargs[highs] := aArgs[unhidden - 1].GetArrayLength - 1
        else if not Assigned(aArgs[unhidden - 1].TypeInfo) then
          highargs[highs] := -1
        else
          highargs[highs] := 0;
        args[i].ValueRef := @highargs[highs];
        Inc(highs);
      end;
    end else begin
      if (pfArray in param.Flags) then begin
        if not Assigned(aArgs[unhidden].TypeInfo) then
          args[i].ValueRef := Nil
        else if aArgs[unhidden].Kind = tkDynArray then
          args[i].ValueRef := PPointer(aArgs[unhidden].GetReferenceToRawData)^
        else
          args[i].ValueRef := aArgs[unhidden].GetReferenceToRawData;
      end else
        args[i].ValueRef := aArgs[unhidden].GetReferenceToRawData;

      Inc(unhidden);
    end;
  end;

  flags := [];
  if aStatic then
    Include(flags, fcfStatic);

  mgr.Invoke(aCodeAddress, args, aCallConv, restype, resptr, flags);
end;

function CreateCallbackProc(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  if not Assigned(FuncCallMgr[aCallConv].CreateCallbackProc) then
    raise ENotImplemented.Create(SErrCallbackNotImplemented);

  if not Assigned(aHandler) then
    raise EArgumentNilException.Create(SErrCallbackHandlerNil);

  Result := FuncCallMgr[aCallConv].CreateCallbackProc(aHandler, aCallConv, aArgs, aResultType, aFlags, aContext);
end;

function CreateCallbackMethod(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of TFunctionCallParameterInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  if not Assigned(FuncCallMgr[aCallConv].CreateCallbackMethod) then
    raise ENotImplemented.Create(SErrCallbackNotImplemented);

  if not Assigned(aHandler) then
    raise EArgumentNilException.Create(SErrCallbackHandlerNil);

  Result := FuncCallMgr[aCallConv].CreateCallbackMethod(aHandler, aCallConv, aArgs, aResultType, aFlags, aContext);
end;

function IsManaged(TypeInfo: PTypeInfo): boolean;
begin
  if Assigned(TypeInfo) then
    case TypeInfo^.Kind of
      tkAString,
      tkLString,
      tkWString,
      tkUString,
      tkInterface,
      tkVariant,
      tkDynArray  : Result := true;
      tkArray     : Result := IsManaged(GetTypeData(TypeInfo)^.ArrayData.ElType);
      tkRecord,
      tkObject    :
        with GetTypeData(TypeInfo)^.RecInitData^ do
          Result := (ManagedFieldCount > 0) or Assigned(ManagementOp);
    else
      Result := false;
    end
  else
    Result := false;
end;

function IsBoolType(ATypeInfo: PTypeInfo): Boolean;

begin
  Result:=(ATypeInfo=TypeInfo(Boolean)) or
          (ATypeInfo=TypeInfo(ByteBool)) or
          (ATypeInfo=TypeInfo(WordBool)) or
          (ATypeInfo=TypeInfo(LongBool));
end;


{$ifndef InLazIDE}
generic function OpenArrayToDynArrayValue<T>(constref aArray: array of T): TValue;
var
  arr: specialize TArray<T>;
  i: SizeInt;
begin
  arr:=[];
  SetLength(arr, Length(aArray));
  for i := 0 to High(aArray) do
    arr[i] := aArray[i];
  Result := TValue.specialize From<specialize TArray<T>>(arr);
end;
{$endif}

function ArrayOfConstToTValueArray(const aValues: array of const): TValueArray;

var
  I,Len: Integer;

begin
  Result:=[];
  Len:=Length(aValues);
  SetLength(Result,Len);
  for I:=0 to Len-1 do
    Result[I]:=aValues[I];
end;

{ TRttiPointerType }

function TRttiPointerType.GetReferredType: TRttiType;
begin
  Result := GRttiPool.GetType(FTypeData^.RefType);
end;

{ TRttiArrayType }

function TRttiArrayType.GetDimensionCount: SizeUInt;
begin
  Result := FTypeData^.ArrayData.DimCount;
end;

function TRttiArrayType.GetDimension(aIndex: SizeInt): TRttiType;
begin
  if aIndex >= FTypeData^.ArrayData.DimCount then
    raise ERtti.CreateFmt(SErrDimensionOutOfRange, [aIndex, FTypeData^.ArrayData.DimCount]);
  Result := GRttiPool.GetType(FTypeData^.ArrayData.Dims[Byte(aIndex)]);
end;

function TRttiArrayType.GetElementType: TRttiType;
begin
  Result := GRttiPool.GetType(FTypeData^.ArrayData.ElType);
end;

function TRttiArrayType.GetTotalElementCount: SizeInt;
begin
  Result := FTypeData^.ArrayData.ElCount;
end;

{ TRttiDynamicArrayType }

function TRttiDynamicArrayType.GetDeclaringUnitName: String;
begin
  Result := FTypeData^.DynUnitName;
end;

function TRttiDynamicArrayType.GetElementSize: SizeUInt;
begin
  Result := FTypeData^.elSize;
end;

function TRttiDynamicArrayType.GetElementType: TRttiType;
begin
  Result := GRttiPool.GetType(FTypeData^.ElType2);
end;

function TRttiDynamicArrayType.GetOleAutoVarType: TVarType;
begin
  Result := Word(FTypeData^.varType);
end;

{ TRttiRefCountedInterfaceType }

function TRttiRefCountedInterfaceType.IntfData: PInterfaceData;
begin
  Result := PInterfaceData(FTypeData);
end;

function TRttiRefCountedInterfaceType.MethodTable: PIntfMethodTable;
begin
  Result := IntfData^.MethodTable;
end;

function TRttiRefCountedInterfaceType.GetIntfBaseType: TRttiInterfaceType;
var
  context: TRttiContext;
begin
  if not Assigned(IntfData^.Parent) then
    Exit(Nil);

  context := TRttiContext.Create;
  try
    Result := context.GetType(IntfData^.Parent^) as TRttiInterfaceType;
  finally
    context.Free;
  end;
end;

function TRttiRefCountedInterfaceType.GetDeclaringUnitName: String;
begin
  Result := IntfData^.UnitName;
end;

function TRttiRefCountedInterfaceType.GetGUID: TGUID;
begin
  Result := IntfData^.GUID;
end;

function TRttiRefCountedInterfaceType.GetIntfFlags: TIntfFlags;
begin
  Result := IntfData^.Flags;
end;

function TRttiRefCountedInterfaceType.GetIntfType: TInterfaceType;
begin
  Result := itRefCounted;
end;

{ TRttiRawInterfaceType }

function TRttiRawInterfaceType.IntfData: PInterfaceRawData;
begin
  Result := PInterfaceRawData(FTypeData);
end;

function TRttiRawInterfaceType.MethodTable: PIntfMethodTable;
begin
  { currently there is none! }
  Result := Nil;
end;

function TRttiRawInterfaceType.GetIntfBaseType: TRttiInterfaceType;
var
  context: TRttiContext;
begin
  if not Assigned(IntfData^.Parent) then
    Exit(Nil);

  context := TRttiContext.Create;
  try
    Result := context.GetType(IntfData^.Parent^) as TRttiInterfaceType;
  finally
    context.Free;
  end;
end;

function TRttiRawInterfaceType.GetDeclaringUnitName: String;
begin
  Result := IntfData^.UnitName;
end;

function TRttiRawInterfaceType.GetGUID: TGUID;
begin
  Result := IntfData^.IID;
end;

function TRttiRawInterfaceType.GetGUIDStr: String;
begin
  Result := IntfData^.IIDStr;
end;

function TRttiRawInterfaceType.GetIntfFlags: TIntfFlags;
begin
  Result := IntfData^.Flags;
end;

function TRttiRawInterfaceType.GetIntfType: TInterfaceType;
begin
  Result := itRaw;
end;

{ TRttiVmtMethodParameter }

function TRttiVmtMethodParameter.GetHandle: Pointer;
begin
  Result := FVmtMethodParam;
end;

function TRttiVmtMethodParameter.GetName: String;
begin
  Result := FVmtMethodParam^.Name;
end;

function TRttiVmtMethodParameter.GetFlags: TParamFlags;
begin
  Result := FVmtMethodParam^.Flags;
end;

function TRttiVmtMethodParameter.GetParamType: TRttiType;
var
  context: TRttiContext;
begin
  if not Assigned(FVmtMethodParam^.ParamType) then
    Exit(Nil);

  context := TRttiContext.Create;
  try
    Result := context.GetType(FVmtMethodParam^.ParamType^);
  finally
    context.Free;
  end;
end;

constructor TRttiVmtMethodParameter.Create(AVmtMethodParam: PVmtMethodParam);
begin
  inherited Create;
  FVmtMethodParam := AVmtMethodParam;
end;

function TRttiVmtMethodParameter.GetAttributes: TCustomAttributeArray;
begin
  Result:=Nil;
end;

{ TRttiMethodTypeParameter }

function TRttiMethodTypeParameter.GetHandle: Pointer;
begin
  Result := fHandle;
end;

function TRttiMethodTypeParameter.GetName: String;
begin
  Result := fName;
end;

function TRttiMethodTypeParameter.GetFlags: TParamFlags;
begin
  Result := fFlags;
end;

function TRttiMethodTypeParameter.GetParamType: TRttiType;
var
  context: TRttiContext;
begin
  context := TRttiContext.Create;
  try
    Result := context.GetType(FType);
  finally
    context.Free;
  end;
end;

constructor TRttiMethodTypeParameter.Create(aHandle: Pointer; const aName: String; aFlags: TParamFlags; aType: PTypeInfo);
begin
  fHandle := aHandle;
  fName := aName;
  fFlags := aFlags;
  fType := aType;
end;

function TRttiMethodTypeParameter.GetAttributes: TCustomAttributeArray;
begin
  Result:=Nil;
end;

{ TRttiIntfMethod }

function TRttiIntfMethod.GetHandle: Pointer;
begin
  Result := FIntfMethodEntry;
end;

function TRttiIntfMethod.GetName: String;
begin
  Result := FIntfMethodEntry^.Name;
end;

function TRttiIntfMethod.GetCallingConvention: TCallConv;
begin
  Result := FIntfMethodEntry^.CC;
end;

function TRttiIntfMethod.GetCodeAddress: CodePointer;
begin
  Result := Nil;
end;

function TRttiIntfMethod.GetDispatchKind: TDispatchKind;
begin
  Result := dkInterface;
end;

function TRttiIntfMethod.GetHasExtendedInfo: Boolean;
begin
  Result := True;
end;

function TRttiIntfMethod.GetIsClassMethod: Boolean;
begin
  Result := False;
end;

function TRttiIntfMethod.GetIsConstructor: Boolean;
begin
  Result := False;
end;

function TRttiIntfMethod.GetIsDestructor: Boolean;
begin
  Result := False;
end;

function TRttiIntfMethod.GetIsStatic: Boolean;
begin
  Result := False;
end;

function TRttiIntfMethod.GetMethodKind: TMethodKind;
begin
  Result := FIntfMethodEntry^.Kind;
end;

function TRttiIntfMethod.GetReturnType: TRttiType;
var
  context: TRttiContext;
begin
  if not Assigned(FIntfMethodEntry^.ResultType) then
    Exit(Nil);

  context := TRttiContext.Create;
  try
    Result := context.GetType(FIntfMethodEntry^.ResultType^);
  finally
    context.Free;
  end;
end;

function TRttiIntfMethod.GetVirtualIndex: SmallInt;
begin
  Result := FIndex;
end;

constructor TRttiIntfMethod.Create(AParent: TRttiType; AIntfMethodEntry: PIntfMethodEntry; AIndex: SmallInt);
begin
  inherited Create(AParent);
  FIntfMethodEntry := AIntfMethodEntry;
  FIndex := AIndex;
end;

function TRttiIntfMethod.GetAttributes: TCustomAttributeArray;
{var
  i: SizeInt;
  at: PAttributeTable;}
begin
  FAttributes:=Nil;
  FAttributesResolved:=True;
{ // needs extended RTTI branch
  if not FAttributesResolved then
    begin
      at := FIntfMethodEntry^.Attributes
      if Assigned(at) then
        begin
          SetLength(FAttributes, at^.AttributeCount);
          for i := 0 to High(FAttributes) do
            FAttributes[i] := TCustomAttribute({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}TypInfo.GetAttribute(at, i));
        end;
      FAttributesResolved:=true;
    end;
}
  result := FAttributes;

end;

function TRttiIntfMethod.GetParameters(aWithHidden: Boolean): TRttiParameterArray;
var
  param: PVmtMethodParam;
  total, visible: SizeInt;
  context: TRttiContext;
  obj: TRttiObject;
begin
  if aWithHidden and (Length(FParamsAll) > 0) then
    Exit(FParamsAll);
  if not aWithHidden and (Length(FParams) > 0) then
    Exit(FParams);

  if FIntfMethodEntry^.ParamCount = 0 then
    Exit(Nil);

  SetLength(FParams, FIntfMethodEntry^.ParamCount);
  SetLength(FParamsAll, FIntfMethodEntry^.ParamCount);

  context := TRttiContext.Create;
  try
    total := 0;
    visible := 0;
    param := FIntfMethodEntry^.Param[0];
    while total < FIntfMethodEntry^.ParamCount do begin
      obj := context.GetByHandle(param);
      if Assigned(obj) then
        FParamsAll[total] := obj as TRttiVmtMethodParameter
      else begin
        FParamsAll[total] := TRttiVmtMethodParameter.Create(param);
        context.AddObject(FParamsAll[total]);
      end;

      if not (pfHidden in param^.Flags) then begin
        FParams[visible] := FParamsAll[total];
        Inc(visible);
      end;

      param := param^.Next;
      Inc(total);
    end;

    if visible <> total then
      SetLength(FParams, visible);
  finally
    context.Free;
  end;

  if aWithHidden then
    Result := FParamsAll
  else
    Result := FParams;
end;

{ TRttiInt64Type }

function TRttiInt64Type.GetMaxValue: Int64;
begin
  Result := FTypeData^.MaxInt64Value;
end;

function TRttiInt64Type.GetMinValue: Int64;
begin
  Result := FTypeData^.MinInt64Value;
end;

function TRttiInt64Type.GetUnsigned: Boolean;
begin
  Result := FTypeData^.OrdType = otUQWord;
end;

function TRttiInt64Type.GetTypeSize: integer;
begin
  Result := SizeOf(QWord);
end;

{ TRttiOrdinalType }

function TRttiOrdinalType.GetMaxValue: LongInt;
begin
  Result := FTypeData^.MaxValue;
end;

function TRttiOrdinalType.GetMinValue: LongInt;
begin
  Result := FTypeData^.MinValue;
end;

function TRttiOrdinalType.GetOrdType: TOrdType;
begin
  Result := FTypeData^.OrdType;
end;

function TRttiOrdinalType.GetTypeSize: Integer;
begin
  case OrdType of
    otSByte,
    otUByte:
      Result := SizeOf(Byte);
    otSWord,
    otUWord:
      Result := SizeOf(Word);
    otSLong,
    otULong:
      Result := SizeOf(LongWord);
    otSQWord,
    otUQWord:
      Result := SizeOf(QWord);
  end;
end;

{ TRttiEnumerationType }

function TRttiEnumerationType.GetUnderlyingType: TRttiType;

begin
  Result:=GRttiPool.GetType(GetTypeData(Handle)^.BaseType);
end;


function TRttiEnumerationType.GetNames: TStringDynArray;

var
  I : Integer;

begin
  Result:=[];
  SetLength(Result,GetEnumNameCount(Handle));
  For I:=0 to Length(Result)-1 do
    Result[I]:=GetEnumName(Handle,I);
end;

generic class function TRttiEnumerationType.GetName<T{: enum}>(AValue: T): string;

var
  Info : PTypeInfo;

begin
  Info:=PtypeInfo(TypeInfo(T));
  if Not (Info^.kind in [tkBool,tkEnumeration]) then
    raise EInvalidCast.CreateFmt(SErrNotEnumeratedType,[PtypeInfo(TypeInfo(T))^.name]);
  Result:=GetEnumName(Info,Ord(aValue))
end;

generic class function TRttiEnumerationType.GetValue<T{: enum}>(const AName: string): T;

var
  Info : PTypeInfo;

begin
  Info:=PtypeInfo(TypeInfo(T));
  if Not (Info^.kind in [tkBool,tkEnumeration]) then
    raise EInvalidCast.CreateFmt(SErrNotEnumeratedType,[PtypeInfo(TypeInfo(T))^.name]);
  Result:=T(GetEnumValue(Info,aName))
end;


{ TRttiFloatType }

function TRttiFloatType.GetFloatType: TFloatType;
begin
  result := FTypeData^.FloatType;
end;

function TRttiFloatType.GetTypeSize: integer;
begin
  case FloatType of
    ftSingle:
      Result := SizeOf(Single);
    ftDouble:
      Result := SizeOf(Double);
    ftExtended:
      Result := SizeOf(Extended);
    ftComp:
      Result := SizeOf(Comp);
    ftCurr:
      Result := SizeOf(Currency);
  end;
end;

{ TRttiParameter }

function TRttiParameter.ToString: String;
var
  f: TParamFlags;
  n: String;
  t: TRttiType;
begin
  if FString = '' then begin
    f := Flags;

    if pfVar in f then
      FString := 'var'
    else if pfConst in f then
      FString := 'const'
    else if pfOut in f then
      FString := 'out'
    else if pfConstRef in f then
      FString := 'constref';
    if FString <> '' then
      FString := FString + ' ';

    n := Name;
    if n = '' then
      n := '<unknown>';
    FString := FString + n;

    t := ParamType;
    if Assigned(t) then begin
      FString := FString + ': ';
      if pfArray in flags then
        FString := 'array of ';
      FString := FString + t.Name;
    end;
  end;

  Result := FString;
end;

{ TMethodImplementation }

function TMethodImplementation.GetCodeAddress: CodePointer;
begin
  Result := fLowLevelCallback.CodeAddress;
end;

procedure TMethodImplementation.InitArgs;
var
  i, refargs: SizeInt;
begin
  i := 0;
  refargs := 0;
  SetLength(fRefArgs, Length(fArgs));
  while i < Length(fArgs) do begin
    if (fArgs[i].ParamFlags * [pfVar, pfOut] <> []) and not (pfHidden in fArgs[i].ParamFlags) then begin
      fRefArgs[refargs] := fArgLen;
      Inc(refargs);
    end;

    if pfArray in fArgs[i].ParamFlags then begin
      Inc(i);
      if (i = Length(fArgs)) or not (pfHigh in fArgs[i].ParamFlags) then
        raise EInsufficientRtti.Create(SErrMethodImplCreateFailed);
      Inc(fArgLen);
    end else if not (pfHidden in fArgs[i].ParamFlags) or (pfSelf in fArgs[i].ParamFlags) then
      Inc(fArgLen)
    else if (pfResult in fArgs[i].ParamFlags) then
      fResult := fArgs[i].ParamType;

    Inc(i);
  end;

  SetLength(fRefArgs, refargs);
end;

procedure TMethodImplementation.HandleCallback(const aArgs: specialize TArray<Pointer>; aResult: Pointer; aContext: Pointer);
var
  i, argidx, validx: SizeInt;
  args: TValueArray;
  res: TValue;
begin
  Assert(fArgLen = Length(aArgs), 'Length of arguments does not match');
  args:=[];
  SetLength(args, fArgLen);
  argidx := 0;
  validx := 0;
  i := 0;
  while i < Length(fArgs) do begin
    if pfArray in fArgs[i].ParamFlags then begin
      Inc(validx);
      Inc(i);
      Assert((i < Length(fArgs)) and (pfHigh in fArgs[i].ParamFlags), 'Expected high parameter after open array parameter');
      TValue.MakeOpenArray(aArgs[validx - 1], SizeInt(aArgs[validx]), fArgs[i].ParamType, args[argidx]);
      Inc(argidx);
      Inc(validx);
    end else if not (pfHidden in fArgs[i].ParamFlags) or (pfSelf in fArgs[i].ParamFlags) then begin
      if Assigned(fArgs[i].ParamType) then
        TValue.Make(aArgs[validx], fArgs[i].ParamType, args[argidx])
      else
        TValue.Make(@aArgs[validx], TypeInfo(Pointer), args[argidx]);
      Inc(argidx);
      Inc(validx);
    end;

    Inc(i);
  end;

  if Assigned(fCallbackMethod) then
    fCallbackMethod(aContext, args, res)
  else
    fCallbackProc(aContext, args, res);

  { copy back var/out parameters }
  for i := 0 to High(fRefArgs) do begin
    args[fRefArgs[i]].ExtractRawData(aArgs[fRefArgs[i]]);
  end;

  if Assigned(fResult) then
    res.ExtractRawData(aResult);
end;

constructor TMethodImplementation.Create(aCC: TCallConv; aArgs: specialize TArray<TFunctionCallParameterInfo>; aResult: PTypeInfo; aFlags: TFunctionCallFlags; aUserData: Pointer; aCallback: TMethodImplementationCallbackMethod);
begin
  fCC := aCC;
  fArgs := aArgs;
  fResult := aResult;
  fFlags := aFlags;
  fCallbackMethod := aCallback;
  InitArgs;
  fLowLevelCallback := CreateCallbackMethod(@HandleCallback, fCC, aArgs, aResult, aFlags, aUserData);
  if not Assigned(fLowLevelCallback) then
    raise EInsufficientRtti.Create(SErrMethodImplCreateFailed);
end;

constructor TMethodImplementation.Create(aCC: TCallConv; aArgs: specialize TArray<TFunctionCallParameterInfo>; aResult: PTypeInfo; aFlags: TFunctionCallFlags; aUserData: Pointer; aCallback: TMethodImplementationCallbackProc);
begin
  fCC := aCC;
  fArgs := aArgs;
  fResult := aResult;
  fFlags := aFlags;
  fCallbackProc := aCallback;
  InitArgs;
  fLowLevelCallback := CreateCallbackMethod(@HandleCallback, fCC, aArgs, aResult, aFlags, aUserData);
  if not Assigned(fLowLevelCallback) then
    raise EInsufficientRtti.Create(SErrMethodImplCreateFailed);
end;

constructor TMethodImplementation.Create;
begin
  raise EInvalidOpException.Create(SErrMethodImplCreateNoArg);
end;

destructor TMethodImplementation.Destroy;
begin
  fLowLevelCallback.Free;
  inherited Destroy;
end;

{ TRttiMethod }

function TRttiMethod.GetHasExtendedInfo: Boolean;
begin
  Result := False;
end;

function TRttiMethod.GetFlags: TFunctionCallFlags;
begin
  Result := [];
  if IsStatic then
    Include(Result, fcfStatic);
end;

function TRttiMethod.GetParameters: TRttiParameterArray;
begin
  Result := GetParameters(False);
end;

function TRttiMethod.ToString: String;
var
  ret: TRttiType;
  n: String;
  params: TRttiParameterArray;
  i: LongInt;
begin
  if FString = '' then begin
    n := Name;
    if n = '' then
      n := '<unknown>';
    if not HasExtendedInfo then begin
      FString := 'method ' + n;
    end else begin
      ret := ReturnType;

      if IsClassMethod then
        FString := 'class ';
      if IsConstructor then
        FString := FString + 'constructor'
      else if IsDestructor then
        FString := FString + 'destructor'
      else if Assigned(ret) then
        FString := FString + 'function'
      else
        FString := FString + 'procedure';

      FString := FString + ' ' + n;

      params := GetParameters;
      if Length(params) > 0 then begin
        FString := FString + '(';
        for i := 0 to High(params) do begin
          if i > 0 then
            FString := FString + '; ';
          FString := FString + params[i].ToString;
        end;
        FString := FString + ')';
      end;

      if Assigned(ret) then
        FString := FString + ': ' + ret.Name;

      if IsStatic then
        FString := FString + '; static';
    end;
  end;

  Result := FString;
end;

function TRttiMethod.Invoke(aInstance: TObject; const aArgs: array of TValue): TValue;
var
  instance: TValue;
begin
  TValue.Make(@aInstance, TypeInfo(TObject), instance);
  Result := Invoke(instance, aArgs);
end;

function TRttiMethod.Invoke(aInstance: TClass; const aArgs: array of TValue): TValue;
var
  instance: TValue;
begin
  TValue.Make(@aInstance, TypeInfo(TClass), instance);
  Result := Invoke(instance, aArgs);
end;

function TRttiMethod.Invoke(aInstance: TValue; const aArgs: array of TValue): TValue;
var
  addr: CodePointer;
  vmt: PCodePointer;
begin
  if not HasExtendedInfo then
    raise EInvocationError.Create(SErrInvokeInsufficientRtti);

  if IsStatic and not aInstance.IsEmpty then
    raise EInvocationError.CreateFmt(SErrInvokeStaticNoSelf, [Name]);

  if not IsStatic and aInstance.IsEmpty then
    raise EInvocationError.CreateFmt(SErrInvokeNotStaticNeedsSelf, [Name]);

  if not IsStatic and IsClassMethod and not aInstance.IsClass then
    raise EInvocationError.CreateFmt(SErrInvokeClassMethodClassSelf, [Name]);

  addr := Nil;
  if IsStatic then
    addr := CodeAddress
  else begin
    vmt := Nil;
    if aInstance.Kind in [tkInterface, tkInterfaceRaw] then
      vmt := PCodePointer(PPPointer(aInstance.GetReferenceToRawData)^^);
    { ToDo }
    if Assigned(vmt) then
      addr := vmt[VirtualIndex];
  end;

  Result := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Rtti.Invoke(Name, addr, CallingConvention, IsStatic, aInstance, aArgs, GetParameters(True), ReturnType);
end;

function TRttiMethod.CreateImplementation(aUserData: Pointer; aCallback: TMethodImplementationCallbackMethod): TMethodImplementation;
var
  params: TRttiParameterArray;
  args: specialize TArray<TFunctionCallParameterInfo>;
  res: PTypeInfo;
  restype: TRttiType;
  resinparam: Boolean;
  i: SizeInt;
begin
  if not Assigned(aCallback) then
    raise EArgumentNilException.Create(SErrMethodImplNoCallback);

  resinparam := False;
  params := GetParameters(True);
  args:=[];
  SetLength(args, Length(params));
  for i := 0 to High(params) do begin
    if Assigned(params[i].ParamType) then
      args[i].ParamType := params[i].ParamType.FTypeInfo
    else
      args[i].ParamType := Nil;
    args[i].ParamFlags := params[i].Flags;
    args[i].ParaLocs := Nil;
    if pfResult in params[i].Flags then
      resinparam := True;
  end;

  restype := GetReturnType;
  if Assigned(restype) and not resinparam then
    res := restype.FTypeInfo
  else
    res := Nil;

  Result := TMethodImplementation.Create(GetCallingConvention, args, res, GetFlags, aUserData, aCallback);
end;

function TRttiMethod.CreateImplementation(aUserData: Pointer; aCallback: TMethodImplementationCallbackProc): TMethodImplementation;
var
  params: TRttiParameterArray;
  args: specialize TArray<TFunctionCallParameterInfo>;
  res: PTypeInfo;
  restype: TRttiType;
  resinparam: Boolean;
  i: SizeInt;
begin
  if not Assigned(aCallback) then
    raise EArgumentNilException.Create(SErrMethodImplNoCallback);

  resinparam := False;
  params := GetParameters(True);
  args:=[];
  SetLength(args, Length(params));
  for i := 0 to High(params) do begin
    if Assigned(params[i].ParamType) then
      args[i].ParamType := params[i].ParamType.FTypeInfo
    else
      args[i].ParamType := Nil;
    args[i].ParamFlags := params[i].Flags;
    args[i].ParaLocs := Nil;
    if pfResult in params[i].Flags then
      resinparam := True;
  end;

  restype := GetReturnType;
  if Assigned(restype) and not resinparam then
    res := restype.FTypeInfo
  else
    res := Nil;

  Result := TMethodImplementation.Create(GetCallingConvention, args, res, GetFlags, aUserData, aCallback);
end;

{ TRttiInvokableType }

function TRttiInvokableType.GetParameters: TRttiParameterArray;
begin
  Result := GetParameters(False);
end;

function TRttiInvokableType.CreateImplementation(aCallback: TCallbackMethod): TMethodImplementation;
var
  params: TRttiParameterArray;
  args: specialize TArray<TFunctionCallParameterInfo>;
  res: PTypeInfo;
  restype: TRttiType;
  resinparam: Boolean;
  i: SizeInt;
begin
  if not Assigned(aCallback) then
    raise EArgumentNilException.Create(SErrMethodImplNoCallback);

  resinparam := False;
  params := GetParameters(True);
  args:=[];
  SetLength(args, Length(params));
  for i := 0 to High(params) do begin
    if Assigned(params[i].ParamType) then
      args[i].ParamType := params[i].ParamType.FTypeInfo
    else
      args[i].ParamType := Nil;
    args[i].ParamFlags := params[i].Flags;
    args[i].ParaLocs := Nil;
    if pfResult in params[i].Flags then
      resinparam := True;
  end;

  restype := GetReturnType;
  if Assigned(restype) and not resinparam then
    res := restype.FTypeInfo
  else
    res := Nil;

  Result := TMethodImplementation.Create(GetCallingConvention, args, res, GetFlags, Self, TMethodImplementationCallbackMethod(aCallback));
end;

function TRttiInvokableType.CreateImplementation(aCallback: TCallbackProc): TMethodImplementation;
var
  params: TRttiParameterArray;
  args: specialize TArray<TFunctionCallParameterInfo>;
  res: PTypeInfo;
  restype: TRttiType;
  resinparam: Boolean;
  i: SizeInt;
begin
  if not Assigned(aCallback) then
    raise EArgumentNilException.Create(SErrMethodImplNoCallback);

  resinparam := False;
  params := GetParameters(True);
  args:=[];
  SetLength(args, Length(params));
  for i := 0 to High(params) do begin
    if Assigned(params[i].ParamType) then
      args[i].ParamType := params[i].ParamType.FTypeInfo
    else
      args[i].ParamType := Nil;
    args[i].ParamFlags := params[i].Flags;
    args[i].ParaLocs := Nil;
    if pfResult in params[i].Flags then
      resinparam := True;
  end;

  restype := GetReturnType;
  if Assigned(restype) and not resinparam then
    res := restype.FTypeInfo
  else
    res := Nil;

  Result := TMethodImplementation.Create(GetCallingConvention, args, res, GetFlags, Self, TMethodImplementationCallbackProc(aCallback));
end;

function TRttiInvokableType.ToString: string;

var
  P : TRTTIParameter;
  A : TRTTIParameterArray;
  I : integer;
  RT : TRttiType;

begin
  RT:=GetReturnType;
  if RT=nil then
    Result:=name+' = procedure ('
  else
    Result:=name+' = function (';
  A:=GetParameters(False);
  for I:=0 to Length(a)-1 do
    begin
      P:=A[I];
      if I>0 then
        Result:=Result+'; ';
      Result:=Result+P.Name;
      if Assigned(P.ParamType) then
        Result:=Result+' : '+P.ParamType.Name;
    end;
  result:=Result+')';
  if Assigned(RT) then
    Result:=Result+' : '+RT.Name;
end;



{ TRttiMethodType }

function TRttiMethodType.GetParameters(aWithHidden: Boolean): TRttiParameterArray;
type
  TParamInfo = record
    Handle: Pointer;
    Flags: TParamFlags;
    Name: String;
  end;

  PParamFlags = ^TParamFlags;
  PCallConv = ^TCallConv;
  PPPTypeInfo = ^PPTypeInfo;

var
  infos: array of TParamInfo;
  total, visible, i: SizeInt;
  ptr: PByte;
  paramtypes: PPPTypeInfo;
  paramtype: PTypeInfo;
  context: TRttiContext;
  obj: TRttiObject;
begin
  if aWithHidden and (Length(FParamsAll) > 0) then
    Exit(FParamsAll);
  if not aWithHidden and (Length(FParams) > 0) then
    Exit(FParams);

  ptr := @FTypeData^.ParamList[0];

  visible := 0;
  total := 0;

  if FTypeData^.ParamCount > 0 then begin
    infos:=[];
    SetLength(infos, FTypeData^.ParamCount);
    while total < FTypeData^.ParamCount do begin
      { align }
      ptr := AlignTParamFlags(ptr);
      infos[total].Handle := ptr;
      infos[total].Flags := PParamFlags(ptr)^;
      Inc(ptr, SizeOf(TParamFlags));
      { handle name }
      infos[total].Name := PShortString(ptr)^;
      Inc(ptr, ptr^ + SizeOf(Byte));
      { skip type name }
      Inc(ptr, ptr^ + SizeOf(Byte));

      if not (pfHidden in infos[total].Flags) then
        Inc(visible);
      Inc(total);
    end;
  end;

  if FTypeData^.MethodKind in [mkFunction, mkClassFunction] then begin
    { skip return type name }
    ptr := AlignToPtr(PByte(ptr) + ptr^ + SizeOf(Byte));
    { handle return type }
    FReturnType := GRttiPool.GetType(PPPTypeInfo(ptr)^^);
    Inc(ptr, SizeOf(PPTypeInfo));
  end;

  { handle calling convention }
  FCallConv := PCallConv(ptr)^;
  Inc(ptr, SizeOf(TCallConv));

  SetLength(FParamsAll, FTypeData^.ParamCount);
  SetLength(FParams, visible);

  if FTypeData^.ParamCount > 0 then begin
    context := TRttiContext.Create;
    try
      paramtypes := PPPTypeInfo(AlignTypeData(ptr));
      visible := 0;
      for i := 0 to FTypeData^.ParamCount - 1 do begin
        obj := context.GetByHandle(infos[i].Handle);
        if Assigned(obj) then
          FParamsAll[i] := obj as TRttiMethodTypeParameter
        else begin
          if Assigned(paramtypes[i]) then
            paramtype := paramtypes[i]^
          else
            paramtype := Nil;
          FParamsAll[i] := TRttiMethodTypeParameter.Create(infos[i].Handle, infos[i].Name, infos[i].Flags, paramtype);
          context.AddObject(FParamsAll[i]);
        end;

        if not (pfHidden in infos[i].Flags) then begin
          FParams[visible] := FParamsAll[i];
          Inc(visible);
        end;
      end;
    finally
      context.Free;
    end;
  end;

  if aWithHidden then
    Result := FParamsAll
  else
    Result := FParams;
end;

function TRttiMethodType.GetCallingConvention: TCallConv;
begin
  { the calling convention is located after the parameters, so get the parameters
    which will also initialize the calling convention }
  GetParameters(True);
  Result := FCallConv;
end;

function TRttiMethodType.GetReturnType: TRttiType;
begin
  if FTypeData^.MethodKind in [mkFunction, mkClassFunction] then begin
    { the return type is located after the parameters, so get the parameters
      which will also initialize the return type }
    GetParameters(True);
    Result := FReturnType;
  end else
    Result := Nil;
end;

function TRttiMethodType.GetFlags: TFunctionCallFlags;
begin
  Result := [];
end;

function TRttiMethodType.ToString: string;

begin
  Result:=Inherited ToString;
  Result:=Result+' of object';
end;

function TRttiMethodType.Invoke(const aCallable: TValue; const aArgs: array of TValue): TValue;
var
  method: PMethod;
  inst: TValue;
begin
  if aCallable.Kind <> tkMethod then
    raise EInvocationError.CreateFmt(SErrInvokeCallableNotMethod, [Name]);

  method := PMethod(aCallable.GetReferenceToRawData);

  { by using a pointer we can also use this for non-class instance methods }
  TValue.Make(@method^.Data, PTypeInfo(TypeInfo(Pointer)), inst);

  Result := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Rtti.Invoke(Name, method^.Code, CallingConvention, False, inst, aArgs, GetParameters(True), ReturnType);
end;

{ TRttiProcedureType }

function TRttiProcedureType.GetParameters(aWithHidden: Boolean): TRttiParameterArray;
var
  visible, i: SizeInt;
  param: PProcedureParam;
  obj: TRttiObject;
  context: TRttiContext;
begin
  if aWithHidden and (Length(FParamsAll) > 0) then
    Exit(FParamsAll);
  if not aWithHidden and (Length(FParams) > 0) then
    Exit(FParams);

  if FTypeData^.ProcSig.ParamCount = 0 then
    Exit(Nil);

  SetLength(FParamsAll, FTypeData^.ProcSig.ParamCount);
  SetLength(FParams, FTypeData^.ProcSig.ParamCount);

  context := TRttiContext.Create;
  try
    param := AlignToPtr(PProcedureParam(@FTypeData^.ProcSig.ParamCount + SizeOf(FTypeData^.ProcSig.ParamCount)));
    visible := 0;
    for i := 0 to FTypeData^.ProcSig.ParamCount - 1 do begin
      obj := context.GetByHandle(param);
      if Assigned(obj) then
        FParamsAll[i] := obj as TRttiMethodTypeParameter
      else begin
        FParamsAll[i] := TRttiMethodTypeParameter.Create(param, param^.Name, param^.ParamFlags, param^.ParamType);
        context.AddObject(FParamsAll[i]);
      end;

      if not (pfHidden in param^.ParamFlags) then begin
        FParams[visible] := FParamsAll[i];
        Inc(visible);
      end;

      param := PProcedureParam(AlignToPtr(PByte(@param^.Name) + Length(param^.Name) + SizeOf(param^.Name[0])));
    end;

    SetLength(FParams, visible);
  finally
    context.Free;
  end;

  if aWithHidden then
    Result := FParamsAll
  else
    Result := FParams;
end;

function TRttiProcedureType.GetCallingConvention: TCallConv;
begin
  Result := FTypeData^.ProcSig.CC;
end;

function TRttiProcedureType.GetReturnType: TRttiType;
var
  context: TRttiContext;
begin
  if not Assigned(FTypeData^.ProcSig.ResultTypeRef) then
    Exit(Nil);

  context := TRttiContext.Create;
  try
    Result := context.GetType(FTypeData^.ProcSig.ResultTypeRef^);
  finally
    context.Free;
  end;
end;

function TRttiProcedureType.GetFlags: TFunctionCallFlags;
begin
  Result := [fcfStatic];
end;

function TRttiProcedureType.Invoke(const aCallable: TValue; const aArgs: array of TValue): TValue;
begin
  if aCallable.Kind <> tkProcVar then
    raise EInvocationError.CreateFmt(SErrInvokeCallableNotProc, [Name]);

  Result := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Rtti.Invoke(Name, PCodePointer(aCallable.GetReferenceToRawData)^, CallingConvention, True, TValue.Empty, aArgs, GetParameters(True), ReturnType);
end;

{ TRttiStringType }

function TRttiStringType.GetStringKind: TRttiStringKind;
begin
  case TypeKind of
    tkSString : result := skShortString;
    tkLString : result := skAnsiString;
    tkAString : result := skAnsiString;
    tkUString : result := skUnicodeString;
    tkWString : result := skWideString;
  else
    Raise EConvertError.Create('Not a string type :'+GetEnumName(TypeInfo(TTypeKind),Ord(TypeKind)));
  end;
end;

function TRttiAnsiStringType.GetCodePage: Word;

begin
  Result:=FTypeData^.CodePage;
end;

{ TRttiInterfaceType }

function TRttiInterfaceType.IntfMethodCount: Word;
var
  parent: TRttiInterfaceType;
  table: PIntfMethodTable;
begin
  parent := GetIntfBaseType;
  if Assigned(parent) then
    Result := parent.IntfMethodCount
  else
    Result := 0;

  table := MethodTable;
  if Assigned(table) then
    Inc(Result, table^.Count);
end;

function TRttiInterfaceType.GetBaseType: TRttiType;
begin
  Result := GetIntfBaseType;
end;

function TRttiInterfaceType.GetGUIDStr: String;
begin
  Result := GUIDToString(GUID);
end;

function TRttiInterfaceType.GetDeclaredMethods: specialize TArray<TRttiMethod>;
var
  methtable: PIntfMethodTable;
  count, index: Word;
  method: PIntfMethodEntry;
  context: TRttiContext;
  obj: TRttiObject;
  parent: TRttiInterfaceType;
  parentmethodcount: Word;
begin
  if Assigned(fDeclaredMethods) then
    Exit(fDeclaredMethods);

  methtable := MethodTable;
  if not Assigned(methtable) then
    Exit(Nil);

  if (methtable^.Count = 0) or (methtable^.RTTICount = $ffff) then
    Exit(Nil);

  parent := GetIntfBaseType;
  if Assigned(parent) then
    parentmethodcount := parent.IntfMethodCount
  else
    parentmethodcount := 0;

  SetLength(fDeclaredMethods, methtable^.Count);

  context := TRttiContext.Create;
  try
    method := methtable^.Method[0];
    count := methtable^.Count;
    while count > 0 do begin
      index := methtable^.Count - count;
      obj := context.GetByHandle(method);
      if Assigned(obj) then
        fDeclaredMethods[index] := obj as TRttiMethod
      else begin
        fDeclaredMethods[index] := TRttiIntfMethod.Create(Self, method, parentmethodcount + index);
        context.AddObject(fDeclaredMethods[index]);
      end;

      method := method^.Next;
      Dec(count);
    end;
  finally
    context.Free;
  end;

  Result := fDeclaredMethods;
end;

{ TRttiInstanceType }

function TRttiInstanceType.GetMetaClassType: TClass;
begin
  result := FTypeData^.ClassType;
end;

function TRttiInstanceType.GetDeclaringUnitName: string;
begin
  result := FTypeData^.UnitName;
end;

function TRttiInstanceType.GetBaseType: TRttiType;
var
  AContext: TRttiContext;
begin
  AContext := TRttiContext.Create;
  try
    result := AContext.GetType(FTypeData^.ParentInfo);
  finally
    AContext.Free;
  end;
end;

function TRttiInstanceType.GetIsInstance: boolean;
begin
  Result:=True;
end;

function TRttiInstanceType.GetTypeSize: integer;
begin
  Result:=sizeof(TObject);
end;

function TRttiInstanceType.GetProperties: specialize TArray<TRttiProperty>;
var
  TypeInfo: PTypeInfo;
  TypeRttiType: TRttiType;
  TD: PTypeData;
  PPD: PPropData;
  TP: PPropInfo;
  Count: longint;
  obj: TRttiObject;
begin
  if not FPropertiesResolved then
    begin
      TypeInfo := FTypeInfo;

      // Get the total properties count
      SetLength(FProperties,FTypeData^.PropCount);
      TypeRttiType:= self;
      repeat
        TD:=GetTypeData(TypeInfo);

        // published properties count for this object
        // skip the attribute-info if available
        PPD := PClassData(TD)^.PropertyTable;
        Count:=PPD^.PropCount;
        // Now point TP to first propinfo record.
        TP:=PPropInfo(@PPD^.PropList);
        While Count>0 do
          begin
            // Don't overwrite properties with the same name
            if FProperties[TP^.NameIndex]=nil then begin
              obj := GRttiPool.GetByHandle(TP);
              if Assigned(obj) then
                FProperties[TP^.NameIndex] := obj as TRttiProperty
              else begin
                FProperties[TP^.NameIndex] := TRttiProperty.Create(TypeRttiType, TP);
                GRttiPool.AddObject(FProperties[TP^.NameIndex]);
              end;
            end;

            // Point to TP next propinfo record.
            // Located at Name[Length(Name)+1] !
            TP:=TP^.Next;
            Dec(Count);
          end;
        TypeInfo:=TD^.Parentinfo;
        TypeRttiType:= GRttiPool.GetType(TypeInfo);
      until TypeInfo=nil;
    end;

  result := FProperties;
end;

{ TRttiMember }

function TRttiMember.GetVisibility: TMemberVisibility;
begin
  result := mvPublished;
end;

constructor TRttiMember.Create(AParent: TRttiType);
begin
  inherited Create();
  FParent := AParent;
end;

{ TRttiProperty }

function TRttiProperty.GetPropertyType: TRttiType;
begin
  result := GRttiPool.GetType(FPropInfo^.PropType);
end;

function TRttiProperty.GetIsReadable: boolean;
begin
  result := assigned(FPropInfo^.GetProc);
end;

function TRttiProperty.GetIsWritable: boolean;
begin
  result := assigned(FPropInfo^.SetProc);
end;

function TRttiProperty.GetVisibility: TMemberVisibility;
begin
  // At this moment only pulished rtti-property-info is supported by fpc
  result := mvPublished;
end;

function TRttiProperty.GetName: string;
begin
  Result:=FPropInfo^.Name;
end;

function TRttiProperty.GetHandle: Pointer;
begin
  Result := FPropInfo;
end;

constructor TRttiProperty.Create(AParent: TRttiType; APropInfo: PPropInfo);
begin
  inherited Create(AParent);
  FPropInfo := APropInfo;
end;

destructor TRttiProperty.Destroy;
var
  attr: TCustomAttribute;
begin
  for attr in FAttributes do
    attr.Free;
  inherited Destroy;
end;

function TRttiProperty.GetAttributes: TCustomAttributeArray;
var
  i: SizeInt;
  at: PAttributeTable;
begin
  if not FAttributesResolved then
    begin
      at := FPropInfo^.AttributeTable;
      if Assigned(at) then
        begin
          SetLength(FAttributes, at^.AttributeCount);
          for i := 0 to High(FAttributes) do
            FAttributes[i] := TCustomAttribute({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}TypInfo.GetAttribute(at, i));
        end;
      FAttributesResolved:=true;
    end;
  result := FAttributes;
end;

function TRttiProperty.GetValue(Instance: pointer): TValue;

  procedure ValueFromBool(value: Int64);
  var
    b8: Boolean;
    b16: Boolean16;
    b32: Boolean32;
    bb: ByteBool;
    bw: WordBool;
    bl: LongBool;
    td: PTypeData;
    p: Pointer;
  begin
    td := GetTypeData(FPropInfo^.PropType);
    case td^.OrdType of
      otUByte:
        begin
          b8 := Boolean(value);
          p := @b8;
        end;
      otUWord:
        begin
          b16 := Boolean16(value);
          p := @b16;
        end;
      otULong:
        begin
          b32 := Boolean32(value);
          p := @b32;
        end;
      otSByte:
        begin
          bb := ByteBool(value);
          p := @bb;
        end;
      otSWord:
        begin
          bw := WordBool(value);
          p := @bw;
        end;
      otSLong:
        begin
          bl := LongBool(value);
          p := @bl;
        end;
    else
      // Silence compiler warning
    end;
    TValue.Make(p, FPropInfo^.PropType, result);
  end;

  procedure ValueFromInt(value: Int64);
  var
    i8: UInt8;
    i16: UInt16;
    i32: UInt32;
    td: PTypeData;
    p: Pointer;
  begin
    td := GetTypeData(FPropInfo^.PropType);
    case td^.OrdType of
      otUByte,
      otSByte:
        begin
          i8 := value;
          p := @i8;
        end;
      otUWord,
      otSWord:
        begin
          i16 := value;
          p := @i16;
        end;
      otULong,
      otSLong:
        begin
          i32 := value;
          p := @i32;
        end;
    else
      // Silence compiler warning
    end;
    TValue.Make(p, FPropInfo^.PropType, result);
  end;

var
  Values: record
    case Integer of
      0: (Enum: Int64);
      1: (Bool: Int64);
      2: (Int: Int64);
      3: (Ch: Byte);
      4: (Wch: Word);
      5: (I64: Int64);
      6: (Si: Single);
      7: (Db: Double);
      8: (Ex: Extended);
      9: (Cur: Currency);
     10: (Cp: Comp);
     11: (A: Pointer;)
  end;
  s: String;
  ss: ShortString;
  u : UnicodeString;
  O: TObject;
  Int: IUnknown;
begin
  case FPropinfo^.PropType^.Kind of
    tkSString:
      begin
        ss := ShortString(GetStrProp(TObject(Instance), FPropInfo));
        TValue.Make(@ss, FPropInfo^.PropType, result);
      end;
    tkAString:
      begin
        s := GetStrProp(TObject(Instance), FPropInfo);
        TValue.Make(@s, FPropInfo^.PropType, result);
      end;
    tkUString:
      begin
        U := GetUnicodeStrProp(TObject(Instance), FPropInfo);
        TValue.Make(@U, FPropInfo^.PropType, result);
      end;
    tkWString:
      begin
        U := GetWideStrProp(TObject(Instance), FPropInfo);
        TValue.Make(@U, FPropInfo^.PropType, result);
      end;
    tkEnumeration:
      begin
        Values.Enum := Integer(GetOrdProp(TObject(Instance), FPropInfo));
        ValueFromInt(Values.Enum);
      end;
    tkBool:
      begin
        Values.Bool := GetOrdProp(TObject(Instance), FPropInfo);
        ValueFromBool(Values.Bool);
      end;
    tkInteger:
      begin
        Values.Int := GetOrdProp(TObject(Instance), FPropInfo);
        ValueFromInt(Values.Int);
      end;
    tkChar:
      begin
        Values.Ch := Byte(GetOrdProp(TObject(Instance), FPropInfo));
        TValue.Make(@Values.Ch, FPropInfo^.PropType, result);
      end;
    tkWChar:
      begin
        Values.Wch := Word(GetOrdProp(TObject(Instance), FPropInfo));
        TValue.Make(@Values.Wch, FPropInfo^.PropType, result);
      end;
    tkInt64,
    tkQWord:
      begin
        Values.I64 := GetOrdProp(TObject(Instance), FPropInfo);
        TValue.Make(@Values.I64, FPropInfo^.PropType, result);
      end;
    tkClass:
    begin
      O := GetObjectProp(TObject(Instance), FPropInfo);
      TValue.Make(@O, FPropInfo^.PropType, Result);
    end;
    tkInterface:
    begin
      Int := GetInterfaceProp(TObject(Instance), FPropInfo);
      TValue.Make(@Int, FPropInfo^.PropType, Result);
    end;
    tkFloat:
    begin
      case GetTypeData(FPropInfo^.PropType)^.FloatType of
        ftCurr   :
          begin
            Values.Cur := Currency(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Cur, FPropInfo^.PropType, Result);
          end;
        ftSingle :
          begin
            Values.Si := Single(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Si, FPropInfo^.PropType, Result);
          end;
        ftDouble :
          begin
            Values.Db := Double(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Db, FPropInfo^.PropType, Result);
          end;
        ftExtended:
          begin
            Values.Ex := GetFloatProp(TObject(Instance), FPropInfo);
            TValue.Make(@Values.Ex, FPropInfo^.PropType, Result);
          end;
        ftComp   :
          begin
            Values.Cp := Comp(GetFloatProp(TObject(Instance), FPropInfo));
            TValue.Make(@Values.Cp, FPropInfo^.PropType, Result);
          end;
      end;
    end;
    tkDynArray:
      begin
        Values.A := GetDynArrayProp(TObject(Instance), FPropInfo);
        TValue.Make(@Values.A, FPropInfo^.PropType, Result);
      end
  else
    result := TValue.Empty;
  end
end;

procedure TRttiProperty.SetValue(Instance: pointer; const AValue: TValue);
begin
  case FPropinfo^.PropType^.Kind of
    tkSString,
    tkAString:
      SetStrProp(TObject(Instance), FPropInfo, AValue.AsString);
    tkUString:
      SetUnicodeStrProp(TObject(Instance), FPropInfo, AValue.AsUnicodeString);
    tkWString:
      SetWideStrProp(TObject(Instance), FPropInfo, AValue.AsUnicodeString);
    tkInteger,
    tkInt64,
    tkQWord,
    tkChar,
    tkBool,
    tkWChar,
    tkEnumeration:
      SetOrdProp(TObject(Instance), FPropInfo, AValue.AsOrdinal);
    tkClass:
      SetObjectProp(TObject(Instance), FPropInfo, AValue.AsObject);
    tkInterface:
      SetInterfaceProp(TObject(Instance), FPropInfo, AValue.AsInterface);
    tkFloat:
      SetFloatProp(TObject(Instance), FPropInfo, AValue.AsExtended);
    tkDynArray:
      SetDynArrayProp(TObject(Instance), FPropInfo, PPointer(AValue.GetReferenceToRawData)^);
  else
    raise exception.createFmt(SErrUnableToSetValueForType, [PropertyType.Name]);
  end
end;

function TRttiType.GetIsInstance: boolean;
begin
  result := false;
end;

function TRttiType.GetIsManaged: boolean;
begin
  result := {$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}Rtti.IsManaged(FTypeInfo);
end;

function TRttiType.GetIsOrdinal: boolean;
begin
  result := false;
end;

function TRttiType.GetIsRecord: boolean;
begin
  result := false;
end;
function TRttiType.GetIsSet: boolean;

begin
  result := false;
end;

function TRttiType.GetAsInstance: TRttiInstanceType;
begin
  // This is a ridicoulous design, but Delphi-compatible...
  result := TRttiInstanceType(self);
end;

function TRttiType.GetBaseType: TRttiType;
begin
  result := nil;
end;

function TRttiType.GetTypeKind: TTypeKind;
begin
  result := FTypeInfo^.Kind;
end;

function TRttiType.GetTypeSize: integer;
begin
  result := -1;
end;

function TRttiType.GetName: string;
begin
  Result:=FTypeInfo^.Name;
end;

function TRttiType.GetHandle: Pointer;
begin
  Result := FTypeInfo;
end;

constructor TRttiType.Create(ATypeInfo: PTypeInfo);
begin
  inherited Create();
  FTypeInfo:=ATypeInfo;
  if assigned(FTypeInfo) then
    FTypeData:=GetTypeData(ATypeInfo);
end;

destructor TRttiType.Destroy;
var
  attr: TCustomAttribute;
begin
  for attr in FAttributes do
    attr.Free;
  inherited;
end;

function TRttiType.GetAttributes: TCustomAttributeArray;
var
  i: Integer;
  at: PAttributeTable;
begin
  if not FAttributesResolved then
    begin
    at := GetAttributeTable(FTypeInfo);
    if Assigned(at) then
      begin
      setlength(FAttributes,at^.AttributeCount);
      for i := 0 to at^.AttributeCount-1 do
        FAttributes[i]:={$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}TypInfo.GetAttribute(at,i);
      end;
    FAttributesResolved:=true;
    end;
  result := FAttributes;
end;

function TRttiType.GetProperties: specialize TArray<TRttiProperty>;
begin
  Result := Nil;
end;

function TRttiType.GetProperty(const AName: string): TRttiProperty;
var
  FPropList: specialize TArray<TRttiProperty>;
  i: Integer;
begin
  result := nil;
  FPropList := GetProperties;
  for i := 0 to length(FPropList)-1 do
    if sametext(FPropList[i].Name,AName) then
      begin
        result := FPropList[i];
        break;
      end;
end;

function TRttiType.GetMethods: specialize TArray<TRttiMethod>;
var
  parentmethods, selfmethods: specialize TArray<TRttiMethod>;
  parent: TRttiType;
begin
  if Assigned(fMethods) then
    Exit(fMethods);

  selfmethods := GetDeclaredMethods;

  parent := GetBaseType;
  if Assigned(parent) then begin
    parentmethods := parent.GetMethods;
  end;

  fMethods := Concat(parentmethods, selfmethods);

  Result := fMethods;
end;

function TRttiType.GetMethod(const aName: String): TRttiMethod;
var
  methods: specialize TArray<TRttiMethod>;
  method: TRttiMethod;
begin
  methods := GetMethods;
  for method in methods do
    if SameText(method.Name, AName) then
      Exit(method);
  Result := Nil;
end;

function TRttiType.GetDeclaredMethods: specialize TArray<TRttiMethod>;
begin
  Result := Nil;
end;

{ TRttiNamedObject }

function TRttiNamedObject.GetName: string;
begin
  result := '';
end;

function TRttiNamedObject.HasName(const aName: string): Boolean;
begin
  Result:=SameText(Name,AName);
end;

{ TRttiContext }

class function TRttiContext.Create: TRttiContext;
begin
  result.FContextToken := nil;
end;

procedure TRttiContext.Free;
begin
  FContextToken := nil;
end;

function TRttiContext.GetByHandle(AHandle: Pointer): TRttiObject;
begin
  if not Assigned(FContextToken) then
    FContextToken := TPoolToken.Create;
  Result := (FContextToken as IPooltoken).RttiPool.GetByHandle(AHandle);
end;

procedure TRttiContext.AddObject(AObject: TRttiObject);
begin
  if not Assigned(FContextToken) then
    FContextToken := TPoolToken.Create;
  (FContextToken as IPooltoken).RttiPool.AddObject(AObject);
end;

function TRttiContext.GetType(ATypeInfo: PTypeInfo): TRttiType;
begin
  if not assigned(FContextToken) then
    FContextToken := TPoolToken.Create;
  result := (FContextToken as IPooltoken).RttiPool.GetType(ATypeInfo);
end;


function TRttiContext.GetType(AClass: TClass): TRttiType;
begin
  if assigned(AClass) then
    result := GetType(PTypeInfo(AClass.ClassInfo))
  else
    result := nil;
end;

{function TRttiContext.GetTypes: specialize TArray<TRttiType>;

begin
  if not assigned(FContextToken) then
    FContextToken := TPoolToken.Create;
  result := (FContextToken as IPooltoken).RttiPool.GetTypes;
end;}

{ TVirtualInterface }

{.$define DEBUG_VIRTINTF}

constructor TVirtualInterface.Create(aPIID: PTypeInfo);
const
  BytesToPopQueryInterface =
{$ifdef cpui386}
    3 * SizeOf(Pointer); { aIID + aObj + $RetAddr }
{$else}
    0;
{$endif}
  BytesToPopAddRef =
{$ifdef cpui386}
    1 * SizeOf(Pointer); { $RetAddr }
{$else}
    0;
{$endif}
  BytesToPopRelease =
{$ifdef cpui386}
    1 * SizeOf(Pointer); { $RetAddr }
{$else}
    0;
{$endif}
var
  t: TRttiType;
  ti: PTypeInfo;
  td: PInterfaceData;
  methods: specialize TArray<TRttiMethod>;
  m: TRttiMethod;
  mt: PIntfMethodTable;
  count, i: SizeInt;
begin
  if not Assigned(aPIID) then
    raise EArgumentNilException.Create(SErrVirtIntfTypeNil);
  { ToDo: add support for raw interfaces once they support RTTI }
  if aPIID^.Kind <> tkInterface then
    raise EArgumentException.CreateFmt(SErrVirtIntfTypeMustBeIntf, [aPIID^.Name]);

  fContext := TRttiContext.Create;
  t := fContext.GetType(aPIID);
  if not Assigned(t) then
    raise EInsufficientRtti.CreateFmt(SErrVirtIntfTypeNotFound, [aPIID^.Name]);

  { check whether the interface and all its parents have RTTI enabled (the only
    exception is IInterface as we know the methods of that) }
  td := PInterfaceData(GetTypeData(aPIID));

  fGUID := td^.GUID;

  fThunks[0] := AllocateRawThunk(TMethod(@QueryInterface).Code, Pointer(Self), BytesToPopQueryInterface);
  fThunks[1] := AllocateRawThunk(TMethod(@_AddRef).Code, Pointer(Self), BytesToPopAddRef);
  fThunks[2] := AllocateRawThunk(TMethod(@_Release).Code, Pointer(Self), BytesToPopRelease);

  for i := Low(fThunks) to High(fThunks) do
    if not Assigned(fThunks[i]) then
      raise ENotImplemented.CreateFmt(SErrVirtIntfCreateThunk, [aPIID^.Name]);

  ti := aPIID;
  { ignore the three methods of IInterface }
  count := 0;
  while ti <> TypeInfo(IInterface) do begin
    mt := td^.MethodTable;
    if (mt^.Count > 0) and (mt^.RTTICount <> mt^.Count) then
      raise EInsufficientRtti.CreateFmt(SErrVirtIntfNotAllMethodsRTTI, [aPIID^.Name]);
    Inc(count, mt^.Count);
    ti := td^.Parent^;
    td := PInterfaceData(GetTypeData(ti));
  end;

  SetLength(fImpls, count);

  methods := t.GetMethods;
  for m in methods do begin
    if m.VirtualIndex > High(fImpls) + Length(fThunks) then
      raise ERtti.CreateFmt(SErrVirtIntfInvalidVirtIdx, [aPIID^.Name, m.Name, m.VirtualIndex]);
    if m.VirtualIndex < Length(fThunks) then
      raise ERtti.CreateFmt(SErrVirtIntfInvalidVirtIdx, [aPIID^.Name, m.Name, m.VirtualIndex]);
    { we use the childmost entry, except for the IInterface methods }
    if Assigned(fImpls[m.VirtualIndex - Length(fThunks)]) then begin
      {$IFDEF DEBUG_VIRTINTF}Writeln('Ignoring duplicate implementation for index ', m.VirtualIndex);{$ENDIF}
      Continue;
    end;
    fImpls[m.VirtualIndex - Length(fThunks)] := m.CreateImplementation(m, @HandleUserCallback);
  end;

  for i := 0 to High(fImpls) do
    if not Assigned(fImpls) then
      raise ERtti.CreateFmt(SErrVirtIntfMethodNil, [aPIID^.Name, i]);

  fVmt := GetMem(Length(fImpls) * SizeOf(CodePointer) + Length(fThunks) * SizeOf(CodePointer));
  if not Assigned(fVmt) then
    raise ERtti.CreateFmt(SErrVirtIntfCreateVmt, [aPIID^.Name]);

  for i := 0 to High(fThunks) do begin
    fVmt[i] := fThunks[i];
    {$IFDEF DEBUG_VIRTINTF}Writeln('VMT ', i, ': ', HexStr(fVmt[i]));{$ENDIF}
  end;
  for i := 0 to High(fImpls) do begin
    fVmt[i + Length(fThunks)] := fImpls[i].CodeAddress;
    {$IFDEF DEBUG_VIRTINTF}Writeln('VMT ', i + Length(fThunks), ': ', HexStr(fVmt[i + Length(fThunks)]));{$ENDIF}
  end;
end;

constructor TVirtualInterface.Create(aPIID: PTypeInfo; aInvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(aPIID);
  OnInvoke := aInvokeEvent;
end;

destructor TVirtualInterface.Destroy;
var
  impl: TMethodImplementation;
  thunk: CodePointer;
begin
  {$IFDEF DEBUG_VIRTINTF}Writeln('Freeing implementations');{$ENDIF}
  for impl in fImpls do
    impl.Free;
  {$IFDEF DEBUG_VIRTINTF}Writeln('Freeing thunks');{$ENDIF}
  for thunk in fThunks do
    FreeRawThunk(thunk);
  {$IFDEF DEBUG_VIRTINTF}Writeln('Freeing VMT');{$ENDIF}
  if Assigned(fVmt) then
    FreeMem(fVmt);
  {$IFDEF DEBUG_VIRTINTF}Writeln('Freeing Context');{$ENDIF}
  fContext.Free;
  {$IFDEF DEBUG_VIRTINTF}Writeln('Done');{$ENDIF}
  inherited Destroy;
end;

function TVirtualInterface.QueryInterface(constref aIID: TGuid; out aObj): LongInt;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  {$IFDEF DEBUG_VIRTINTF}Writeln('QueryInterface for ', GUIDToString(aIID));{$ENDIF}
  if IsEqualGUID(aIID, fGUID) then begin
    {$IFDEF DEBUG_VIRTINTF}Writeln('Returning ', HexStr(@fVmt));{$ENDIF}
    Pointer(aObj) := @fVmt;
    { QueryInterface increases the reference count }
    _AddRef;
    Result := S_OK;
  end else
    Result := inherited QueryInterface(aIID, aObj);
end;

function TVirtualInterface._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; 
begin
  Result:=Inherited _AddRef;
end;

function TVirtualInterface._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=Inherited _Release;
end;

procedure TVirtualInterface.HandleUserCallback(aUserData: Pointer; const aArgs: TValueArray; out aResult: TValue);
begin
  {$IFDEF DEBUG_VIRTINTF}Writeln('Call for ', TRttiMethod(aUserData).Name);{$ENDIF}
  if Assigned(fOnInvoke) then
    fOnInvoke(TRttiMethod(aUserData), aArgs, aResult);
end;

function TRttiObject.GetAttribute(aClass: TCustomAttributeClass): TCustomAttribute;

var
  attrarray : TCustomAttributeArray;
  a: TCustomAttribute;

begin
  Result:=nil;
  attrarray:=GetAttributes;
  for a in attrarray do
    if a.InheritsFrom(aClass) then
      Exit(a);
end;

function TRttiObject.HasAttribute(aClass: TCustomAttributeClass): Boolean;
begin
  Result:=Assigned(GetAttribute(aClass));
end;

generic function TRttiObject.GetAttribute<T>: T;
begin
  Result:=T(GetAttribute(T));
end;

generic function TRttiObject.HasAttribute<T>: Boolean;
begin
  Result:=HasAttribute(T);
end;


{$ifndef InLazIDE}
{$if defined(CPUI386) or (defined(CPUX86_64) and defined(WIN64))}
{$I invoke.inc}
{$endif}
{$endif}

initialization
  PoolRefCount := 0;
  InitDefaultFunctionCallManager;
{$ifdef SYSTEM_HAS_INVOKE}
  InitSystemFunctionCallManager;
{$endif}
end.

