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

interface

uses
  Classes,
  SysUtils,
  typinfo;

type
  TRttiType = class;
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
    function IsArray: boolean; inline;
    function AsString: string;
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

  { TRttiContext }

  TRttiContext = record
  private
    FContextToken: IInterface;
  public
    class function Create: TRttiContext; static;
    procedure  Free;
    function GetType(ATypeInfo: PTypeInfo): TRttiType;
    function GetType(AClass: TClass): TRttiType;
    //function GetTypes: specialize TArray<TRttiType>;
  end;

  { TRttiObject }

  TRttiObject = class
  public

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
    FPropertiesResolved: boolean;
    FProperties: specialize TArray<TRttiProperty>;
    function GetAsInstance: TRttiInstanceType;
  protected
    FTypeData: PTypeData;
    function GetName: string; override;
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
    function GetProperties: specialize TArray<TRttiProperty>;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    destructor destroy; override;
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

  TRttiStructuredType = class(TRttiType)

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


  { TRttiInstanceType }

  TRttiInstanceType = class(TRttiStructuredType)
  private
    function GetDeclaringUnitName: string;
    function GetMetaClassType: TClass;
  protected
    function GetIsInstance: boolean; override;
    function GetTypeSize: integer; override;
    function GetBaseType: TRttiType; override;
  public
    property MetaClassType: TClass read GetMetaClassType;
    property DeclaringUnitName: string read GetDeclaringUnitName;

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
  public
    constructor create(AParent: TRttiType; APropInfo: PPropInfo);
    function GetValue(Instance: pointer): TValue;
    procedure SetValue(Instance: pointer; const AValue: TValue);
    property PropertyType: TRttiType read GetPropertyType;
    property IsReadable: boolean read GetIsReadable;
    property IsWritable: boolean read GetIsWritable;
    property Visibility: TMemberVisibility read GetVisibility;
  end;

function IsManaged(TypeInfo: PTypeInfo): boolean;

implementation

type

  { TRttiPool }

  TRttiPool = class
  private
    FTypesList: specialize TArray<TRttiType>;
    FTypeCount: LongInt;
    FLock: TRTLCriticalSection;
  public
    function GetTypes: specialize TArray<TRttiType>;
    function GetType(ATypeInfo: PTypeInfo): TRttiType;
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

resourcestring
  SErrUnableToGetValueForType = 'Unable to get value for type %s';
  SErrUnableToSetValueForType = 'Unable to set value for type %s';
  SErrInvalidTypecast         = 'Invalid class typecast';

var
  PoolRefCount : integer;
  GRttiPool    : TRttiPool;

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

{ TRttiPool }

function TRttiPool.GetTypes: specialize TArray<TRttiType>;
begin
  if not Assigned(FTypesList) then
    Exit(Nil);
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalsection(FLock);
{$endif}
  Result := Copy(FTypesList, 0, FTypeCount);
{$ifdef FPC_HAS_FEATURE_THREADING}
  LeaveCriticalsection(FLock);
{$endif}
end;

function TRttiPool.GetType(ATypeInfo: PTypeInfo): TRttiType;
var
  i: integer;
begin
  if not Assigned(ATypeInfo) then
    Exit(Nil);
{$ifdef FPC_HAS_FEATURE_THREADING}
  EnterCriticalsection(FLock);
{$endif}
  Result := Nil;
  for i := 0 to FTypeCount - 1 do
    begin
      if FTypesList[i].FTypeInfo = ATypeInfo then
        begin
          Result := FTypesList[i];
          Break;
        end;
    end;
  if not Assigned(Result) then
    begin
      if FTypeCount = Length(FTypesList) then
        begin
          SetLength(FTypesList, FTypeCount * 2);
        end;
      case ATypeInfo^.Kind of
        tkClass   : Result := TRttiInstanceType.Create(ATypeInfo);
        tkSString,
        tkLString,
        tkAString,
        tkUString,
        tkWString : Result := TRttiStringType.Create(ATypeInfo);
        tkFloat   : Result := TRttiFloatType.Create(ATypeInfo);
      else
        Result := TRttiType.Create(ATypeInfo);
      end;
      FTypesList[FTypeCount] := Result;
      Inc(FTypeCount);
    end;
{$ifdef FPC_HAS_FEATURE_THREADING}
  LeaveCriticalsection(FLock);
{$endif}
end;

constructor TRttiPool.Create;
begin
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitCriticalSection(FLock);
{$endif}
  SetLength(FTypesList, 32);
end;

destructor TRttiPool.Destroy;
var
  i: LongInt;
begin
  for i := 0 to length(FTypesList)-1 do
    FTypesList[i].Free;
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
      system.move(ACopyFromBuffer^,FBuffer^,FDataSize);
    end;
  FIsCopy := True;
  FUseAddRef := AAddRef;
  if AAddRef and (ALen > 0) then
    IntAddRef(FBuffer, FTypeInfo);
end;

constructor TValueDataIntImpl.CreateRef(AData: Pointer; ATypeInfo: PTypeInfo; AAddRef: Boolean);
begin
  FTypeInfo := ATypeInfo;
  FDataSize := SizeOf(Pointer);
  FBuffer := PPointer(AData)^;
  FIsCopy := False;
  FUseAddRef := AAddRef;
  if AAddRef then
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
  if not Assigned(ABuffer) then
    Exit;
  case ATypeInfo^.Kind of
    tkSString  : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Length(PShortString(ABuffer)^) + 1, ATypeInfo, True);
    tkWString,
    tkUString,
    tkAString  : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkDynArray : result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
    tkArray    : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.ArrayData.Size, ATypeInfo, False);
    tkObject,
    tkRecord   : result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, Result.TypeData^.RecSize, ATypeInfo, False);
    tkClass    : result.FData.FAsObject := PPointer(ABuffer)^;
    tkClassRef : result.FData.FAsClass := PClass(ABuffer)^;
    tkInterfaceRaw : result.FData.FAsPointer := PPointer(ABuffer)^;
    tkInterface: result.FData.FValueData := TValueDataIntImpl.CreateRef(ABuffer, ATypeInfo, True);
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
                       td := GetTypeData(td^.CompType);
                       { ToDo: what about PACKSETS? }
                       size := td^.MaxValue div 8;
                       case size of
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
                           Result.FData.FValueData := TValueDataIntImpl.CreateCopy(ABuffer, size, ATypeInfo, False);
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


function TValue.GetTypeDataProp: PTypeData;
begin
  result := GetTypeData(FData.FTypeInfo);
end;

function TValue.GetDataSize: SizeInt;
var
  td: PTypeData;
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
        case TypeData^.OrdType of
          otUByte: begin
            td := GetTypeData(TypeData^.CompType);
            Result := td^.MaxValue div 8;
            case Result of
              0:
                Result := SizeOf(Byte);
              3:
                Result := SizeOf(LongWord);
              5..7:
                Result := SizeOf(QWord);
            end;
          end;
          otUWord:
            Result := SizeOf(Word);
          otULong:
            Result := SizeOf(LongWord);
          else
            Result := 0;
        end;
      tkMethod:
        { ? }
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
  if (Kind in [tkSString, tkAString, tkUString, tkWString]) and not Assigned(FData.FValueData) then
    Result := ''
  else
    case Kind of
      tkSString:
        Result := PShortString(FData.FValueData.GetReferenceToRawData)^;
      tkAString:
        Result := PAnsiString(FData.FValueData.GetReferenceToRawData)^;
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
  result := fdata.FTypeInfo^.Kind = tkClass;
end;

function TValue.IsClass: boolean;
begin
  result := (Kind = tkClassRef) or ((Kind = tkClass) and not Assigned(FData.FAsObject));
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
            ((Kind in [tkClass, tkClassRef, tkInterfaceRaw]) and not Assigned(FData.FAsPointer));
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
    if Kind in [tkClass, tkClassRef, tkInterfaceRaw] then
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
  else if (Kind in [tkClass, tkClassRef]) and not Assigned(FData.FAsPointer) then
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
var
  td: PTypeData;
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
            td := GetTypeData(TypeData^.CompType);
            case td^.MaxValue div 8 of
              0, 1:
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

constructor TRttiType.create(ATypeInfo: PTypeInfo);
begin
  inherited create();
  FTypeInfo:=ATypeInfo;
  if assigned(FTypeInfo) then
    FTypeData:=GetTypeData(ATypeInfo);
end;

function aligntoptr(p : pointer) : pointer;inline;
   begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   end;

function aligntoqword(p : pointer) : pointer;inline;
  type
    TAlignCheck = record
      b : byte;
      q : qword;
    end;
  begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    result:=align(p,PtrInt(@TAlignCheck(nil^).q))
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
    result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  end;


function TRttiType.GetProperties: specialize TArray<TRttiProperty>;
type
  PPropData = ^TPropData;
var
  TypeInfo: PTypeInfo;
  TypeRttiType: TRttiType;
  TD: PTypeData;
  PPD: PPropData;
  TP: PPropInfo;
  Count: longint;
begin
  if not FPropertiesResolved then
    begin
      TypeInfo := FTypeInfo;

      // Get the total properties count
      SetLength(FProperties,FTypeData^.PropCount);
      // Clear list
      FillChar(FProperties[0],FTypeData^.PropCount*sizeof(TRttiProperty),0);
      TypeRttiType:= self;
      repeat
        TD:=GetTypeData(TypeInfo);

        // published properties count for this object
        // skip the attribute-info if available
        PPD := aligntoptr(PPropData(pointer(@TD^.UnitName)+PByte(@TD^.UnitName)^+1));
        Count:=PPD^.PropCount;
        // Now point TP to first propinfo record.
        TP:=PPropInfo(@PPD^.PropList);
        While Count>0 do
          begin
            // Don't overwrite properties with the same name
            if FProperties[TP^.NameIndex]=nil then
              FProperties[TP^.NameIndex]:=TRttiProperty.Create(TypeRttiType, TP);

            // Point to TP next propinfo record.
            // Located at Name[Length(Name)+1] !
            TP:=aligntoptr(PPropInfo(pointer(@TP^.Name)+PByte(@TP^.Name)^+1));
            Dec(Count);
          end;
        TypeInfo:=TD^.Parentinfo;
        TypeRttiType:= GRttiPool.GetType(TypeInfo);
      until TypeInfo=nil;
    end;

  result := FProperties;
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

destructor TRttiType.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(FProperties) do
    FProperties[i].Free;
  inherited destroy;
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
end.

