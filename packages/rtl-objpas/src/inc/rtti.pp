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
unit Rtti experimental;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

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

interface

uses
  Classes,
  SysUtils,
  typinfo;

type
  TRttiObject = class;
  TRttiType = class;
  TRttiMethod = class;
  TRttiProperty = class;
  TRttiInstanceType = class;

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
  public
    class function Empty: TValue; static;
    class procedure Make(ABuffer: pointer; ATypeInfo: PTypeInfo; out result: TValue); static;
{$ifndef NoGenericMethods}
    generic class function From<T>(constref aValue: T): TValue; static; inline;
{$endif}
    function IsArray: boolean; inline;
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
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsUInt64: QWord;
    function AsInterface: IInterface;
    function ToString: String;
    function GetArrayLength: SizeInt;
    function GetArrayElement(AIndex: SizeInt): TValue;
    procedure SetArrayElement(AIndex: SizeInt; constref AValue: TValue);
    function IsType(ATypeInfo: PTypeInfo): boolean; inline;
    function TryAsOrdinal(out AResult: int64): boolean;
    function GetReferenceToRawData: Pointer;
    class operator := (const AValue: String): TValue; inline;
    class operator := (AValue: LongInt): TValue; inline;
    class operator := (AValue: Single): TValue; inline;
    class operator := (AValue: Double): TValue; inline;
{$ifdef FPC_HAS_TYPE_EXTENDED}
    class operator := (AValue: Extended): TValue; inline;
{$endif}
    class operator := (AValue: Currency): TValue; inline;
    class operator := (AValue: Int64): TValue; inline;
    class operator := (AValue: QWord): TValue; inline;
    class operator := (AValue: TObject): TValue; inline;
    class operator := (AValue: TClass): TValue; inline;
    class operator := (AValue: Boolean): TValue; inline;
    property DataSize: SizeInt read GetDataSize;
    property Kind: TTypeKind read GetTypeKind;
    property TypeData: PTypeData read GetTypeDataProp;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property IsEmpty: boolean read GetIsEmpty;
  end;

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
    property Handle: Pointer read GetHandle;
  end;

  { TRttiNamedObject }

  TRttiNamedObject = class(TRttiObject)
  protected
    function GetName: string; virtual;
  public
    property Name: string read GetName;
  end;

  { TRttiType }

  TRttiType = class(TRttiNamedObject)
  private
    FTypeInfo: PTypeInfo;
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
    constructor create(ATypeInfo : PTypeInfo);
    function GetProperties: specialize TArray<TRttiProperty>; virtual;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    function GetMethods: specialize TArray<TRttiMethod>; virtual;
    function GetDeclaredMethods: specialize TArray<TRttiMethod>; virtual;
    property IsInstance: boolean read GetIsInstance;
    property isManaged: boolean read GetIsManaged;
    property IsOrdinal: boolean read GetIsOrdinal;
    property IsRecord: boolean read GetIsRecord;
    property IsSet: boolean read GetIsSet;
    property BaseType: TRttiType read GetBaseType;
    property AsInstance: TRttiInstanceType read GetAsInstance;
    property TypeKind: TTypeKind read GetTypeKind;
    property TypeSize: integer read GetTypeSize;
  end;

  { TRttiFloatType }

  TRttiFloatType = class(TRttiType)
  private
    function GetFloatType: TFloatType;
  public
    property FloatType: TFloatType read GetFloatType;
  end;


  TRttiStringKind = (skShortString, skAnsiString, skWideString, skUnicodeString);

  { TRttiStringType }

  TRttiStringType = class(TRttiType)
  private
    function GetStringKind: TRttiStringKind;
  public
    property StringKind: TRttiStringKind read GetStringKind;
  end;

  TRttiPointerType = class(TRttiType)
  private
    function GetReferredType: TRttiType;
  public
    property ReferredType: TRttiType read GetReferredType;
  end;

  { TRttiMember }

  TMemberVisibility=(mvPrivate, mvProtected, mvPublic, mvPublished);

  TRttiMember = class(TRttiNamedObject)
  private
    FParent: TRttiType;
  protected
    function GetVisibility: TMemberVisibility; virtual;
  public
    constructor create(AParent: TRttiType);
    property Visibility: TMemberVisibility read GetVisibility;
    property Parent: TRttiType read FParent;
  end;

  { TRttiProperty }

  TRttiProperty = class(TRttiMember)
  private
    FPropInfo: PPropInfo;
    function GetPropertyType: TRttiType;
    function GetIsWritable: boolean;
    function GetIsReadable: boolean;
  protected
    function GetVisibility: TMemberVisibility; override;
    function GetName: string; override;
    function GetHandle: Pointer; override;
  public
    constructor create(AParent: TRttiType; APropInfo: PPropInfo);
    function GetValue(Instance: pointer): TValue;
    procedure SetValue(Instance: pointer; const AValue: TValue);
    property PropertyType: TRttiType read GetPropertyType;
    property IsReadable: boolean read GetIsReadable;
    property IsWritable: boolean read GetIsWritable;
    property Visibility: TMemberVisibility read GetVisibility;
  end;

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
    function GetParameters: specialize TArray<TRttiParameter>; virtual; abstract;
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

  EInsufficientRtti = class(Exception);
  EInvocationError = class(Exception);
  ENonPublicType = class(Exception);

  TFunctionCallParameter = record
    Value: TValue;
    ParamFlags: TParamFlags;
    ParaLocs: PParameterLocations;
  end;
  TFunctionCallParameterArray = specialize TArray<TFunctionCallParameter>;

  TFunctionCallFlag = (
    fcfStatic
  );
  TFunctionCallFlags = set of TFunctionCallFlag;

  TFunctionCallCallback = Pointer;

  TFunctionCallProc = procedure(const aArgs: TValueArray; out aResult: TValue; aContext: Pointer);
  TFunctionCallMethod = procedure(const aArgs: TValueArray; out aResult: TValue; aContext: Pointer) of object;

  TFunctionCallManager = record
    Invoke: procedure(CodeAddress: CodePointer; const Args: TFunctionCallParameterArray; CallingConvention: TCallConv;
              ResultType: PTypeInfo; out ResultValue: TValue; Flags: TFunctionCallFlags);
    CreateCallbackProc: function(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
    CreateCallbackMethod: function(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
    FreeCallback: procedure(aCallback: TFunctionCallCallback; aCallConv: TCallConv);
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

function CreateCallbackProc(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
function CreateCallbackMethod(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
procedure FreeCallback(aCallback: TFunctionCallCallback; aCallConv: TCallConv);

function IsManaged(TypeInfo: PTypeInfo): boolean;

{ these resource strings are needed by units implementing function call managers }
resourcestring
  SErrInvokeNotImplemented = 'Invoke functionality is not implemented';
  SErrInvokeFailed = 'Invoke call failed';
  SErrCallbackNotImplented = 'Callback functionality is not implemented';
  SErrCallConvNotSupported = 'Calling convention not supported: %s';
  SErrTypeKindNotSupported = 'Type kind is not supported: %s';
  SErrCallbackHandlerNil = 'Callback handler is Nil';
  SErrMissingSelfParam = 'Missing self parameter';

implementation

uses
  fgl;

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
  end;

  TRttiIntfMethod = class(TRttiMethod)
  private
    FIntfMethodEntry: PIntfMethodEntry;
    FIndex: SmallInt;
    FParams: specialize TArray<TRttiParameter>;
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
  public
    constructor Create(AParent: TRttiType; AIntfMethodEntry: PIntfMethodEntry; AIndex: SmallInt);
    function GetParameters: specialize TArray<TRttiParameter>; override;
  end;

resourcestring
  SErrUnableToGetValueForType = 'Unable to get value for type %s';
  SErrUnableToSetValueForType = 'Unable to set value for type %s';
  SErrInvalidTypecast         = 'Invalid class typecast';
  SErrRttiObjectNoHandle      = 'RTTI object instance has no valid handle property';
  SErrRttiObjectAlreadyRegistered = 'A RTTI object with handle 0x%x is already registered';

var
  PoolRefCount : integer;
  GRttiPool    : TRttiPool;
  FuncCallMgr: TFunctionCallManagerArray;

procedure NoInvoke(aCodeAddress: CodePointer; const aArgs: TFunctionCallParameterArray; aCallConv: TCallConv;
            aResultType: PTypeInfo; out aResultValue: TValue; aFlags: TFunctionCallFlags);
begin
  raise ENotImplemented.Create(SErrInvokeNotImplemented);
end;

function NoCreateCallbackProc(aFunc: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  Result := Nil;
  raise ENotImplemented.Create(SErrCallbackNotImplented);
end;

function NoCreateCallbackMethod(aFunc: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  Result := Nil;
  raise ENotImplemented.Create(SErrCallbackNotImplented);
end;

procedure NoFreeCallback(aCallback: TFunctionCallCallback; aCallConv: TCallConv);
begin
  raise ENotImplemented.Create(SErrCallbackNotImplented);
end;

const
  NoFunctionCallManager: TFunctionCallManager = (
    Invoke: @NoInvoke;
    CreateCallbackProc: @NoCreateCallbackProc;
    CreateCallbackMethod: @NoCreateCallbackMethod;
    FreeCallback: @NoFreeCallback
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

  SetLength(funcargs, Length(aArgs));
  for i := Low(aArgs) to High(aArgs) do begin
    funcargs[i - Low(aArgs) + Low(funcargs)].Value := aArgs[i];
    funcargs[i - Low(aArgs) + Low(funcargs)].ParamFlags := [];
    funcargs[i - Low(aArgs) + Low(funcargs)].ParaLocs := Nil;
  end;

  FuncCallMgr[aCallConv].Invoke(aCodeAddress, funcargs, aCallConv, aResultType, Result, flags);
end;

function CreateCallbackProc(aHandler: TFunctionCallProc; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  if not Assigned(FuncCallMgr[aCallConv].CreateCallbackProc) then
    raise ENotImplemented.Create(SErrCallbackNotImplented);

  if not Assigned(aHandler) then
    raise EArgumentNilException.Create(SErrCallbackHandlerNil);

  Result := FuncCallMgr[aCallConv].CreateCallbackProc(aHandler, aCallConv, aArgs, aResultType, aFlags, aContext);
end;

function CreateCallbackMethod(aHandler: TFunctionCallMethod; aCallConv: TCallConv; aArgs: array of PTypeInfo; aResultType: PTypeInfo; aFlags: TFunctionCallFlags; aContext: Pointer): TFunctionCallCallback;
begin
  if not Assigned(FuncCallMgr[aCallConv].CreateCallbackMethod) then
    raise ENotImplemented.Create(SErrCallbackNotImplented);

  if not Assigned(aHandler) then
    raise EArgumentNilException.Create(SErrCallbackHandlerNil);

  Result := FuncCallMgr[aCallConv].CreateCallbackMethod(aHandler, aCallConv, aArgs, aResultType, aFlags, aContext);
end;

procedure FreeCallback(aCallback: TFunctionCallCallback; aCallConv: TCallConv);
begin
  if Assigned(FuncCallMgr[aCallConv].FreeCallback) then
    FuncCallMgr[aCallConv].FreeCallback(aCallback, aCallConv);
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

{ TRttiPointerType }

function TRttiPointerType.GetReferredType: TRttiType;
begin
  Result := GRttiPool.GetType(FTypeData^.RefType);
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
          tkSString,
          tkLString,
          tkAString,
          tkUString,
          tkWString : Result := TRttiStringType.Create(ATypeInfo);
          tkFloat   : Result := TRttiFloatType.Create(ATypeInfo);
          tkPointer : Result := TRttiPointerType.Create(ATypeInfo);
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

function TRttiIntfMethod.GetParameters: specialize TArray<TRttiParameter>;
var
  param: PVmtMethodParam;
  total, visible: SizeInt;
  context: TRttiContext;
  obj: TRttiObject;
begin
  if Length(FParams) > 0 then
    Exit(FParams);

  if FIntfMethodEntry^.ParamCount = 0 then
    Exit(Nil);

  SetLength(FParams, FIntfMethodEntry^.ParamCount);

  context := TRttiContext.Create;
  try
    total := 0;
    visible := 0;
    param := FIntfMethodEntry^.Param[0];
    while total < FIntfMethodEntry^.ParamCount do begin
      if not (pfHidden in param^.Flags) then begin
        obj := context.GetByHandle(param);
        if Assigned(obj) then
          FParams[visible] := obj as TRttiVmtMethodParameter
        else begin
          FParams[visible] := TRttiVmtMethodParameter.Create(param);
          context.AddObject(FParams[visible]);
        end;
        Inc(visible);
      end;

      param := param^.Next;
      Inc(total);
    end;

    SetLength(FParams, visible);
  finally
    context.Free;
  end;

  Result := FParams;
end;

{ TRttiFloatType }

function TRttiFloatType.GetFloatType: TFloatType;
begin
  result := FTypeData^.FloatType;
end;

{ TValue }

class function TValue.Empty: TValue;
begin
  result.FData.FTypeInfo := nil;
{$if SizeOf(TMethod) > SizeOf(QWord)}
  Result.FData.FAsMethod.Code := Nil;
  Result.FData.FAsMethod.Data := Nil;
{$else}
  Result.FData.FAsUInt64 := 0;
{$endif}
end;

class procedure TValue.Make(ABuffer: pointer; ATypeInfo: PTypeInfo; out result: TValue);
type
  PBoolean16 = ^Boolean16;
  PBoolean32 = ^Boolean32;
  PBoolean64 = ^Boolean64;
  PByteBool = ^ByteBool;
  PQWordBool = ^QWordBool;
  PMethod = ^TMethod;
var
  td: PTypeData;
  size: SizeInt;
begin
  result.FData.FTypeInfo:=ATypeInfo;
  { resets the whole variant part; FValueData is already Nil }
{$if SizeOf(TMethod) > SizeOf(QWord)}
  Result.FData.FAsMethod.Code := Nil;
  Result.FData.FAsMethod.Data := Nil;
{$else}
  Result.FData.FAsUInt64 := 0;
{$endif}
  if not Assigned(ATypeInfo) then
    Exit;
  { first handle those types that need a TValueData implementation }
  case ATypeInfo^.Kind of
    tkSString  : begin
                   if Assigned(ABuffer) then
                     size := Length(PShortString(ABuffer)^) + 1
                   else
                     size := 256;
                   result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, size, ATypeInfo, True);
                 end;
    tkWString,
    tkUString,
    tkAString  : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkDynArray : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkArray    : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.ArrayData.Size, ATypeInfo, False);
    tkObject,
    tkRecord   : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.RecSize, ATypeInfo, False);
    tkInterface: result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
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
    tkEnumeration,
    tkInteger  : begin
                   case GetTypeData(ATypeInfo)^.OrdType of
                     otSByte: result.FData.FAsSByte := PShortInt(ABuffer)^;
                     otUByte: result.FData.FAsUByte := PByte(ABuffer)^;
                     otSWord: result.FData.FAsSWord := PSmallInt(ABuffer)^;
                     otUWord: result.FData.FAsUWord := PWord(ABuffer)^;
                     otSLong: result.FData.FAsSLong := PLongInt(ABuffer)^;
                     otULong: result.FData.FAsULong := PLongWord(ABuffer)^;
                   end;
                 end;
    tkBool     : begin
                   case GetTypeData(ATypeInfo)^.OrdType of
                     otUByte: result.FData.FAsSByte := ShortInt(PBoolean(ABuffer)^);
                     otUWord: result.FData.FAsUWord := Byte(PBoolean16(ABuffer)^);
                     otULong: result.FData.FAsULong := SmallInt(PBoolean32(ABuffer)^);
                     otUQWord: result.FData.FAsUInt64 := QWord(PBoolean64(ABuffer)^);
                     otSByte: result.FData.FAsSByte := Word(PByteBool(ABuffer)^);
                     otSWord: result.FData.FAsSWord := LongInt(PWordBool(ABuffer)^);
                     otSLong: result.FData.FAsSLong := LongWord(PLongBool(ABuffer)^);
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

{$ifndef NoGenericMethods}
generic class function TValue.From<T>(constref aValue: T): TValue;
begin
  TValue.Make(@aValue, System.TypeInfo(T), Result);
end;
{$endif}

function TValue.GetTypeDataProp: PTypeData;
begin
  result := GetTypeData(FData.FTypeInfo);
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

function TValue.AsString: string;
begin
  if System.GetTypeKind(String) = tkUString then
    Result := String(AsUnicodeString)
  else
    Result := String(AsAnsiString);
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
    else
      raise EInvalidCast.Create(SErrInvalidTypecast);
    end;
    end
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.AsObject: TObject;
begin
  if IsObject or (IsClass and not Assigned(FData.FAsObject)) then
    result := TObject(FData.FAsObject)
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.IsObject: boolean;
begin
  result := (Kind = tkClass) or ((Kind = tkUnknown) and not Assigned(FData.FAsObject));
end;

function TValue.IsClass: boolean;
begin
  result := (Kind = tkClassRef) or ((Kind in [tkClass,tkUnknown]) and not Assigned(FData.FAsObject));
end;

function TValue.AsClass: TClass;
begin
  if IsClass then
    result := FData.FAsClass
  else
    raise EInvalidCast.Create(SErrInvalidTypecast);
end;

function TValue.IsOrdinal: boolean;
begin
  result := (Kind in [tkInteger, tkInt64, tkQWord, tkBool]) or
            ((Kind in [tkClass, tkClassRef, tkInterfaceRaw, tkUnknown]) and not Assigned(FData.FAsPointer));
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
    end;
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
    end;
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
    tkSString,
    tkAString : result := AsString;
    tkInteger : result := IntToStr(AsInteger);
    tkBool    : result := BoolToStr(AsBoolean, True);
  else
    result := '';
  end;
end;

function TValue.GetArrayLength: SizeInt;
begin
  if not IsArray then
    raise EInvalidCast.Create(SErrInvalidTypecast);
  if Kind = tkDynArray then
    Result := DynArraySize(PPointer(FData.FValueData.GetReferenceToRawData)^)
  else
    Result := TypeData^.ArrayData.ElCount;
end;

function TValue.GetArrayElement(AIndex: SizeInt): TValue;
var
  data: Pointer;
  eltype: PTypeInfo;
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
    data := PByte(FData.FValueData.GetReferenceToRawData) + AIndex * (td^.ArrayData.Size div td^.ArrayData.ElCount);
  end;
  { MakeWithoutCopy? }
  Make(data, eltype, Result);
end;

procedure TValue.SetArrayElement(AIndex: SizeInt; constref AValue: TValue);
var
  data: Pointer;
  eltype: PTypeInfo;
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
    data := PByte(FData.FValueData.GetReferenceToRawData) + AIndex * (td^.ArrayData.Size div td^.ArrayData.ElCount);
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

function TValue.IsType(ATypeInfo: PTypeInfo): boolean;
begin
  result := ATypeInfo = TypeInfo;
end;

function TValue.TryAsOrdinal(out AResult: int64): boolean;
begin
  result := IsOrdinal;
  if result then
    AResult := AsOrdinal;
end;

function TValue.GetReferenceToRawData: Pointer;
begin
  if IsEmpty then
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
    end;
  end;
end;

class operator TValue.:=(const AValue: String): TValue;
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
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: TClass): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
end;

class operator TValue.:=(AValue: Boolean): TValue;
begin
  Make(@AValue, System.TypeInfo(AValue), Result);
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

{ TRttiMethod }

function TRttiMethod.GetHasExtendedInfo: Boolean;
begin
  Result := False;
end;

function TRttiMethod.ToString: String;
var
  ret: TRttiType;
  n: String;
  params: specialize TArray<TRttiParameter>;
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

{ TRttiStringType }

function TRttiStringType.GetStringKind: TRttiStringKind;
begin
  case TypeKind of
    tkSString : result := skShortString;
    tkLString : result := skAnsiString;
    tkAString : result := skAnsiString;
    tkUString : result := skUnicodeString;
    tkWString : result := skWideString;
  end;
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

constructor TRttiMember.create(AParent: TRttiType);
begin
  inherited create();
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

constructor TRttiProperty.create(AParent: TRttiType; APropInfo: PPropInfo);
begin
  inherited create(AParent);
  FPropInfo := APropInfo;
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
    end;
    TValue.Make(p, FPropInfo^.PropType, result);
  end;

var
  s: string;
  ss: ShortString;
  i: int64;
  c: Char;
  wc: WideChar;
begin
  case FPropinfo^.PropType^.Kind of
    tkSString:
      begin
        ss := GetStrProp(TObject(Instance), FPropInfo);
        TValue.Make(@ss, FPropInfo^.PropType, result);
      end;
    tkAString:
      begin
        s := GetStrProp(TObject(Instance), FPropInfo);
        TValue.Make(@s, FPropInfo^.PropType, result);
      end;
    tkBool:
      begin
        i := GetOrdProp(TObject(Instance), FPropInfo);
        ValueFromBool(i);
      end;
    tkInteger:
      begin
        i := GetOrdProp(TObject(Instance), FPropInfo);
        ValueFromInt(i);
      end;
    tkChar:
      begin
        c := AnsiChar(GetOrdProp(TObject(Instance), FPropInfo));
        TValue.Make(@c, FPropInfo^.PropType, result);
      end;
    tkWChar:
      begin
        wc := WideChar(GetOrdProp(TObject(Instance), FPropInfo));
        TValue.Make(@wc, FPropInfo^.PropType, result);
      end;
    tkInt64,
    tkQWord:
      begin
        i := GetOrdProp(TObject(Instance), FPropInfo);
        TValue.Make(@i, FPropInfo^.PropType, result);
      end;
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
    tkInteger,
    tkInt64,
    tkQWord,
    tkChar,
    tkBool,
    tkWChar:
      SetOrdProp(TObject(Instance), FPropInfo, AValue.AsOrdinal);
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
  result := Rtti.IsManaged(FTypeInfo);
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

constructor TRttiType.create(ATypeInfo: PTypeInfo);
begin
  inherited create();
  FTypeInfo:=ATypeInfo;
  if assigned(FTypeInfo) then
    FTypeData:=GetTypeData(ATypeInfo);
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

function TRttiType.GetDeclaredMethods: specialize TArray<TRttiMethod>;
begin
  Result := Nil;
end;

{ TRttiNamedObject }

function TRttiNamedObject.GetName: string;
begin
  result := '';
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

initialization
  PoolRefCount := 0;
  InitDefaultFunctionCallManager;
end.

