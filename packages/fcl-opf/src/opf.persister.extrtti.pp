{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Extended RTTI-based persistence (3.3.1 and higher)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Persister.ExtRTTI;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, Data.Db, System.Rtti,
{$ELSE}
  Classes, SysUtils, typinfo, db, rtti,
{$ENDIF}
  Opf.Types, Opf.Registry, Opf.Fields, Opf.Consts,
  Opf.Persister.RTTI;

type

  { TExtRTTIPersister:
    Extends TRTTIPersister with Level 2 TNullable<T>
    support via extended RTTI. Requires FPC 3.3.1+ (trunk). }

  TExtRTTIPersister = class(TRTTIPersister)
  protected
    // Level 2: map the TNullable<T> record fields of aClass
    procedure ScanNullableRecordFields(aClass: TClass; aMap: TOPFieldMapList);
    function AcceptNullableRecordField(aClass: TClass; aRecordField: TRttiField): Boolean; virtual;
    function NullableRecordColumnName(aClass: TClass; aRecordField: TRttiField): UTF8String; virtual;
    function AddNullableRecordMap(aMap: TOPFieldMapList; aClass: TClass; aRecordField: TRttiField;
      const aColName: UTF8String): TOPFieldMap;
    // Class fields (pkFields), accessed through RttiMember
    procedure PersistFieldMember(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam);
    procedure ApplyFieldMember(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
  public
    // Fills the map with the class's persisted properties, including nullable record fields
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    // Sets up object creation through the class's own parameterless constructor
    procedure AfterRegister(aDef: TPersisterDef); override;
    // Copies a database field value into the object's property or field
    function ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean; override;
    // Copies the object's property or field value into a query parameter
    function PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean; override;
  end;

  { TOPDeferredRelation: 
    Collected during attribute scanning, processed after registration }

  TOPDeferredRelation = record
    ParentClassName: UTF8String;
    FKFieldName: UTF8String;
    FKName: UTF8String;
    DBCascade: Boolean;
  end;
  TOPDeferredRelationArray = array of TOPDeferredRelation;

  { TAttributePersister:
    Attribute-driven field mapping.
    Scans class/property/field attributes to build the field map.
    Requires FPC 3.3.1+ (trunk) with {$modeswitch prefixedattributes}. }

  TAttributePersister = class(TExtRTTIPersister)
  private
    type
      { Mapping attributes found on a single property or field }
      TMemberAttributes = record
        Options: TCustomAttribute;
        Sequence: TCustomAttribute;
        Size: TCustomAttribute;
        Unique: TCustomAttribute;
        Default: TCustomAttribute;
        Relation: TCustomAttribute;
      end;
  private
    FDeferredRelations: TOPDeferredRelationArray;
    FIndexes: TOPIndexDefArray;
    FPKConstraintName: UTF8String;
    FHasAttributePK: Boolean;
    function GetPropAttributes(aPropInfo: PPropInfo): TMemberAttributes;
    function GetFieldAttributes(aFieldEntry: PExtendedVmtFieldEntry): TMemberAttributes;
    procedure AddDeferredRelation(aRelAttr: TCustomAttribute; const aColName: UTF8String);
    procedure ApplyDefaultAttribute(aClass: TClass; F: TOPFieldMap; aDefAttr: TCustomAttribute;
      const aMemberKind, aMemberName: String);
    procedure ApplyMemberAttributes(aClass: TClass; F: TOPFieldMap; const aAttrs: TMemberAttributes;
      const aMemberKind, aMemberName: String);
    procedure AddPropertyMapping(aClass: TClass; aMap: TOPFieldMapList; aPropInfo: PPropInfo;
      const aColName: UTF8String);
    procedure ScanProperty(aClass: TClass; aMap: TOPFieldMapList; aPropInfo: PPropInfo);
    procedure ScanProperties(aClass: TClass; aMap: TOPFieldMapList; aVisibilities: TVisibilityClasses);
    procedure ScanField(aClass: TClass; aMap: TOPFieldMapList; aFieldEntry: PExtendedVmtFieldEntry;
      aCheckPropertyDuplicates: Boolean);
    procedure ScanFields(aClass: TClass; aMap: TOPFieldMapList; aVisibilities: TVisibilityClasses;
      aCheckPropertyDuplicates: Boolean);
    procedure ScanExplicitProperty(aClass: TClass; aMap: TOPFieldMapList; aPropInfo: PPropInfo);
    procedure ScanExplicitProperties(aClass: TClass; aMap: TOPFieldMapList);
    procedure ResolvePrimaryKey(aClass: TClass; aMap: TOPFieldMapList);
    procedure CollectIndexes(aClass: TClass; aMap: TOPFieldMapList);
  protected
    function AcceptNullableRecordField(aClass: TClass; aRecordField: TRttiField): Boolean; override;
    function NullableRecordColumnName(aClass: TClass; aRecordField: TRttiField): UTF8String; override;
  public
    // The table name from the class's table-name attribute, or the default name
    class function GetTableName(aClass: TClass): UTF8String; override;
    // Fills the map from the attributes on the class, its properties and fields
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    // Relations found while scanning, to be set up after registration
    property DeferredRelations: TOPDeferredRelationArray read FDeferredRelations;
    // Indexes declared through attributes
    property Indexes: TOPIndexDefArray read FIndexes;
    // Constraint name given in the primary key attribute
    property PKConstraintName: UTF8String read FPKConstraintName;
    // True when the primary key was declared through an attribute
    property HasAttributePK: Boolean read FHasAttributePK;
  end;

{ Code address of the parameterless constructor that creates objects of
  aClass: one declared by the most derived class that has one (Create is
  preferred over other names), not TObject.Create. Nil when there is none
  or the class has no method RTTI. Called as function(aClass, 1): TObject. }
function OPFFindDefaultConstructor(aClass: TClass): CodePointer;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Variants, System.Math,
{$ELSE}
  Variants, Math,
{$ENDIF}
  Opf.Nullable, Opf.Attributes, Opf.Factory, Opf.FieldValue, Opf.Json, Opf.Variants;

resourcestring
  SErrUnknownEnumValue = 'Unknown enumeration value "%s" for property %s';
  SErrLargeSetNeedsAsString = 'Set field "%s" of class "%s" is %d bytes: a set of more than '+
    '32 elements cannot be stored as a number, store it as text (foAsString)';
  SErrInvalidSetElement = 'Invalid set element "%s" for field %s';

var
  { Locale-invariant format settings for parsing OPDefault float values }
  AttrFormatSettings: TFormatSettings;

var
  { Unit-level RTTI context:
    keeps the RTTI pool alive so that cached TRttiField references stored on
    TOPFieldMap.RttiMember remain valid. }
  NullableRttiCtx: TRttiContext;
  NullableRttiCtxCreated: Boolean = False;

function GetNullableRttiCtx: TRttiContext;
begin
  // Created in the initialization section: no race between threads
  Result := NullableRttiCtx;
end;

{ Returns True if aFieldRtti represents a TNullable<T> record field.
  Detection: tkRecord with fields named FValue and FHasValue (Boolean).
  Also checks for the mangled name pattern TNullable$ as a fast pre-filter. }
function IsNullableRecordField(aFieldRtti: TRttiField): Boolean;
var
  RecType: TRttiType;
  FVal, FHas: TRttiField;
begin
  Result := False;
  if aFieldRtti.FieldType.TypeKind <> tkRecord then
    Exit;
  { Fast pre-filter: FPC mangles generic names as TNullable$1$crcXXXX }
  if Pos('TNullable$', aFieldRtti.FieldType.Name) <> 1 then
    Exit;
  RecType := aFieldRtti.FieldType;
  FVal := RecType.GetField('FValue');
  FHas := RecType.GetField('FHasValue');
  Result := (FVal <> nil) and (FHas <> nil) and (FHas.FieldType.TypeKind = tkBool);
end;

{ TNullable read helpers:
  read-modify-write cycle for record fields.
  Pattern: GetValue(instance) -> read inner field from raw record data. }

function ReadNullableHasValue(Instance: TObject; aRecordField: TRttiField): Boolean;
var
  RecVal: TValue;
  HasField: TRttiField;
begin
  RecVal := aRecordField.GetValue(Instance);
  HasField := aRecordField.FieldType.GetField('FHasValue');
  Result := HasField.GetValue(RecVal.GetReferenceToRawData).AsBoolean;
end;

function ReadNullableValue(Instance: TObject; aRecordField: TRttiField): TValue;
var
  RecVal: TValue;
  ValField: TRttiField;
begin
  RecVal := aRecordField.GetValue(Instance);
  ValField := aRecordField.FieldType.GetField('FValue');
  Result := ValField.GetValue(RecVal.GetReferenceToRawData);
end;

{ TNullable write helpers - set inner fields, then write record back to instance. }

procedure WriteNullableValue(Instance: TObject; aRecordField: TRttiField; const aValue: TValue);
var
  RecVal, BoolVal: TValue;
  ValField, HasField: TRttiField;
begin
  RecVal := aRecordField.GetValue(Instance);
  ValField := aRecordField.FieldType.GetField('FValue');
  HasField := aRecordField.FieldType.GetField('FHasValue');
  ValField.SetValue(RecVal.GetReferenceToRawData, aValue);
  TValue.Make(NativeInt(Ord(True)), TypeInfo(Boolean), BoolVal);
  HasField.SetValue(RecVal.GetReferenceToRawData, BoolVal);
  aRecordField.SetValue(Instance, RecVal);
end;

procedure WriteNullableNull(Instance: TObject; aRecordField: TRttiField);
var
  RecVal, BoolVal: TValue;
  HasField: TRttiField;
begin
  RecVal := aRecordField.GetValue(Instance);
  HasField := aRecordField.FieldType.GetField('FHasValue');
  TValue.Make(NativeInt(Ord(False)), TypeInfo(Boolean), BoolVal);
  HasField.SetValue(RecVal.GetReferenceToRawData, BoolVal);
  aRecordField.SetValue(Instance, RecVal);
end;

{ A TValue of the inner float type of a nullable field (TDateTime, Currency,
  Single, ...), so the record field receives a value of its own size. }

procedure MakeFloatValue(aField: TOPFieldMap; const aValue: Extended; out aResult: TValue);
var
  S: Single;
  D: Double;
  E: Extended;
  C: Currency;
  CP: Comp;
begin
  if aField.PropertyTypeInfo=Nil then
  begin
    TValue.specialize Make<Double>(aValue, aResult);
    Exit;
  end;
  // Qualified: Data.Db declares ftSingle as well
  case GetTypeData(aField.PropertyTypeInfo)^.FloatType of
    TFloatType.ftSingle:
      begin
        S:=aValue;
        TValue.Make(@S, aField.PropertyTypeInfo, aResult);
      end;
    TFloatType.ftExtended:
      begin
        E:=aValue;
        TValue.Make(@E, aField.PropertyTypeInfo, aResult);
      end;
    TFloatType.ftCurr:
      begin
        C:=aValue;
        TValue.Make(@C, aField.PropertyTypeInfo, aResult);
      end;
    TFloatType.ftComp:
      begin
        CP:=Round(aValue);
        TValue.Make(@CP, aField.PropertyTypeInfo, aResult);
      end;
  else
    D:=aValue;
    TValue.Make(@D, aField.PropertyTypeInfo, aResult);
  end;
end;

{ Inner types of TNullable<T> that Level 2 can store }

function IsSupportedNullableKind(aKind: TTypeKind): Boolean;
begin
  Result:=aKind in [tkSString, tkAString, tkLString, tkWString, tkUString,
                    tkChar, tkWChar, tkUChar,
                    tkInteger, tkInt64, tkQWord, tkFloat, tkBool, tkEnumeration];
end;

{ The inner value of a Level 2 field from a database field }

procedure MakeInnerValue(aField: TOPFieldMap; aDBField: TField; out aResult: TValue);
var
  C: Currency;
  U: UnicodeString;
  W: WideString;
  S: AnsiString;
  Ord: Int64;
begin
  case aField.PropertyKind of
    tkUString:
      begin
        U:=UTF8Decode(OPFGetUTF8Field(aDBField));
        TValue.Make(@U, aField.PropertyTypeInfo, aResult);
      end;
    tkWString:
      begin
        W:=UTF8Decode(OPFGetUTF8Field(aDBField));
        TValue.Make(@W, aField.PropertyTypeInfo, aResult);
      end;
    tkSString, tkAString, tkLString:
      begin
        S:=aDBField.AsString;
        if aField.PropertyKind=tkSString then
          TValue.specialize Make<String>(S, aResult)
        else
          TValue.Make(@S, aField.PropertyTypeInfo, aResult);
      end;
    tkFloat:
      if OPFFloatKind(aField)=fkCurrency then
      begin
        // Exact, also where Extended is Double
        C:=aDBField.AsCurrency;
        TValue.Make(@C, aField.PropertyTypeInfo, aResult);
      end
      else
        MakeFloatValue(aField, OPFGetFloatFieldValue(aDBField, aField), aResult);
    tkBool:
      TValue.Make(NativeInt(System.Ord(aDBField.AsBoolean)), aField.PropertyTypeInfo, aResult);
  else
    // Ordinal types, with the actual type (Byte, Word, Cardinal, enumerations)
    case aField.PropertyKind of
      tkChar, tkWChar, tkUChar:
        Ord:=OPFTextToChar(aField.PropertyKind, aDBField);
      tkEnumeration:
        if foAsString in aField.Options then
        begin
          Ord:=GetEnumValue(aField.PropertyTypeInfo, aDBField.AsString);
          // As the classic path: an unknown name is an error, not ordinal -1
          if Ord<0 then
            raise EFieldMap.CreateFmt(SErrUnknownEnumValue, [aDBField.AsString, aField.PropertyName]);
        end
        else
          Ord:=aDBField.AsInteger;
      tkQWord:
        Ord:=OPFCheckQWord(aDBField.AsLargeInt);
    else
      Ord:=aDBField.AsLargeInt;
    end;
    aResult:=TValue.FromOrdinal(aField.PropertyTypeInfo, Ord);
  end;
end;

{ Store the inner value of a Level 2 field in a parameter }

procedure SetInnerParam(aField: TOPFieldMap; const aValue: TValue; aParam: TParam);
begin
  case aField.PropertyKind of
    tkUString, tkWString:
      OPFSetUTF8Param(aParam, UTF8Encode(aValue.AsUnicodeString));
    tkSString, tkAString, tkLString:
      aParam.AsString:=aValue.AsString;
    tkChar, tkWChar, tkUChar:
      OPFSetUTF8Param(aParam, OPFCharToText(aField.PropertyKind, aValue.AsOrdinal));
    tkInteger:
      if OPFIsUnsigned32(aField) then
        aParam.AsLargeInt:=aValue.AsOrdinal
      else
        aParam.AsInteger:=aValue.AsOrdinal;
    tkInt64:
      aParam.AsLargeInt:=aValue.AsInt64;
    tkQWord:
      aParam.AsLargeInt:=OPFCheckQWord(Int64(aValue.AsUInt64));
    tkEnumeration:
      if foAsString in aField.Options then
        aParam.AsString:=GetEnumName(aField.PropertyTypeInfo, aValue.AsOrdinal)
      else
        aParam.AsInteger:=aValue.AsOrdinal;
    tkFloat:
      if OPFFloatKind(aField)=fkCurrency then
        OPFSetCurrencyParam(aParam, aValue.AsCurrency)
      else
        OPFSetFloatParam(aParam, aField, aValue.AsExtended);
    tkBool:
      aParam.AsBoolean:=aValue.AsBoolean;
  else
    raise EOPF.CreateFmt(SErrUnsupportedPropertyType, [aField.PropertyName]);
  end;
end;

{ Ordinal value of a set held in a TValue. Only for sets of up to 4 bytes:
  larger sets must be stored as text (checked at registration). }

function SetOrdinal(const aValue: TValue): Int64;
var
  L: LongInt;
begin
  L:=0;
  Move(aValue.GetReferenceToRawData^, L, Min(SizeOf(L), aValue.DataSize));
  Result:=L;
end;

{ True for a set of enumeration values: its elements are written by name }

function IsEnumSet(aTypeInfo: PTypeInfo): Boolean;
var
  Comp: PTypeInfo;
begin
  Comp:=GetTypeData(aTypeInfo)^.CompType;
  Result:=Assigned(Comp) and (Comp^.Kind=tkEnumeration);
end;

{ A set as text, for a set of any size: [Name1,Name2] for enumerations,
  [1,35] for integers }

function SetText(aField: TOPFieldMap; const aValue: TValue): String;
var
  Elements: TBytes;
  I: Integer;
begin
  if IsEnumSet(aField.PropertyTypeInfo) then
    Exit(SetToString(aField.PropertyTypeInfo, aValue.GetReferenceToRawData, True));
  Elements:=SetToArray(aField.PropertyTypeInfo, aValue.GetReferenceToRawData);
  Result:='';
  for I:=0 to High(Elements) do
  begin
    if I>0 then
      Result:=Result+',';
    Result:=Result+IntToStr(Elements[I]);
  end;
  Result:='['+Result+']';
end;

{ The inverse of SetText; aResult has the size of the set type }

procedure TextToSet(aField: TOPFieldMap; const aText: String; aResult: Pointer);
var
  Parts: specialize TArray<String>;
  Elements: array of Byte;
  S: String;
  I, N: Integer;
begin
  if IsEnumSet(aField.PropertyTypeInfo) then
  begin
    StringToSet(aField.PropertyTypeInfo, aText, aResult);
    Exit;
  end;
  S:=Trim(aText);
  if (S<>'') and (S[1]='[') then
    Delete(S, 1, 1);
  if (S<>'') and (S[Length(S)]=']') then
    Delete(S, Length(S), 1);
  Elements:=Nil;
  if Trim(S)<>'' then
  begin
    Parts:=S.Split([',']);
    SetLength(Elements, Length(Parts));
    for I:=0 to High(Parts) do
    begin
      N:=StrToInt(Trim(Parts[I]));
      if (N<0) or (N>=GetTypeData(aField.PropertyTypeInfo)^.SetSize*8) then
        raise EFieldMap.CreateFmt(SErrInvalidSetElement, [Parts[I], aField.PropertyName]);
      Elements[I]:=N;
    end;
  end;
  ArrayToSet(aField.PropertyTypeInfo, Elements, aResult);
end;

{ A set in a TValue, from its text or its ordinal value. The buffer has the
  size of the set type. }

procedure MakeSetValue(aField: TOPFieldMap; aDBField: TField; out aResult: TValue);
var
  Buf: array of Byte;
  L: LongInt;
begin
  Buf:=Nil;
  SetLength(Buf, Max(GetTypeData(aField.PropertyTypeInfo)^.SetSize, SizeOf(L)));
  if foAsString in aField.Options then
    TextToSet(aField, aDBField.AsString, @Buf[0])
  else
  begin
    L:=aDBField.AsInteger;
    Move(L, Buf[0], SizeOf(L));
  end;
  TValue.Make(@Buf[0], aField.PropertyTypeInfo, aResult);
end;

{ A value as the change trackers compare it }

function ValueToVariant(aField: TOPFieldMap; const aValue: TValue; out aResult: Variant): Boolean;
var
  Obj: TObject;
begin
  Result := True;
  case aField.PropertyKind of
    tkSString, tkAString, tkLString:
      aResult := aValue.AsString;
    tkWString, tkUString:
      aResult := aValue.AsUnicodeString;
    tkInteger, tkEnumeration, tkChar, tkWChar, tkUChar:
      aResult := aValue.AsOrdinal;
    tkInt64:
      aResult := aValue.AsInt64;
    tkQWord:
      aResult := aValue.AsUInt64;
    tkFloat:
      if OPFFloatKind(aField) = fkCurrency then
        aResult := aValue.AsCurrency
      else
        aResult := OPFFloatVariant(aField, aValue.AsExtended);
    tkBool:
      aResult := aValue.AsBoolean;
    tkSet:
      if aValue.DataSize > SizeOf(LongInt) then
        aResult := SetText(aField, aValue)
      else
        aResult := SetOrdinal(aValue);
    tkVariant:
      aResult := aValue.AsVariant;
    tkClass:
      begin
        Obj := aValue.AsObject;
        if Obj = nil then
          aResult := Null
        else if Obj is TStrings then
          aResult := TStrings(Obj).Text
        else
          aResult := OPSerializer.Serialize(Obj);
      end;
  else
    Result := False;
  end;
end;

{ Reader for the change trackers (Opf.FieldValue) for members that are not
  published properties: class fields (pkFields) and Level 2 TNullable<T>
  record fields (Null when the record has no value). }

function ExtRTTINullableMemberReader(aInstance: TObject; aField: TOPFieldMap; out aValue: Variant): Boolean;
var
  RecordField: TRttiField;
begin
  aValue := Unassigned;
  RecordField := TRttiField(aField.RttiMember);
  if RecordField = nil then
    Exit(False);
  if aField.MemberKind = omkField then
    Exit(ValueToVariant(aField, RecordField.GetValue(aInstance), aValue));
  if not ReadNullableHasValue(aInstance, RecordField) then
  begin
    aValue := Null;
    Exit(True);
  end;
  Result := ValueToVariant(aField, ReadNullableValue(aInstance, RecordField), aValue);
end;

{ TExtRTTIPersister }

function OPFFindDefaultConstructor(aClass: TClass): CodePointer;
var
  RttiType: TRttiType;
  Methods: TRttiMethodArray;
  M: TRttiMethod;
  Level: TRttiType;
  I: Integer;
begin
  Result := nil;
  if aClass = nil then
    Exit;
  RttiType := GetNullableRttiCtx.GetType(aClass);
  if RttiType = nil then
    Exit;
  // Own methods first, then those of each ancestor in turn
  Methods := RttiType.GetMethods;
  Level := nil;
  for I := 0 to Length(Methods) - 1 do
  begin
    M := Methods[I];
    // A new class level: a constructor found on a more derived class wins
    if M.Parent <> Level then
    begin
      if Result <> nil then
        Exit;
      Level := M.Parent;
    end;
    if not M.IsConstructor then
      Continue;
    if Length(M.GetParameters) > 0 then
      Continue;
    // TObject.Create does nothing: the caller creates the object as usual
    if (M.Parent is TRttiInstanceType) and
       (TRttiInstanceType(M.Parent).MetaclassType = TObject) then
      Continue;
    // Create is preferred over other constructors of the same class
    if SameText(M.Name, 'Create') then
      Exit(M.CodeAddress);
    if Result = nil then
      Result := M.CodeAddress;
  end;
end;

type
  TConstructorFunc = function(aClass: TClass; aFlag: Integer): TObject;

{ The factory registered by TExtRTTIPersister. A plain function, not owned
  by a class definition: it stays valid when a registry is freed, and it runs
  the constructor of the class that is actually created, also for an
  unregistered descendant of the registered class. }

function ExtRTTIConstructorFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
var
  Code: CodePointer;
begin
  Code := OPFFindDefaultConstructor(aClass);
  if Code <> nil then
    Result := TConstructorFunc(Code)(aClass, 1)
  else if aClass.InheritsFrom(TComponent) then
  begin
    if Assigned(aContext) and (aContext.OwnerObject is TComponent) then
      Result := TComponentClass(aClass).Create(TComponent(aContext.OwnerObject))
    else
      Result := TComponentClass(aClass).Create(nil);
  end
  else
    Result := aClass.Create;
end;

procedure TExtRTTIPersister.AfterRegister(aDef: TPersisterDef);
var
  Code: CodePointer;
begin
  inherited AfterRegister(aDef);
  Code := OPFFindDefaultConstructor(aDef.InstanceClass);
  if Code = nil then
    Exit;
  aDef.ConstructorCode := Code;
  // Never replaces a factory the application registered for the class or an
  // ancestor; that factory then creates the objects of this class as well.
  OPFactory.RegisterDefaultFactory(aDef.InstanceClass, @ExtRTTIConstructorFactory);
end;

procedure TExtRTTIPersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
begin
  { Pass 1: Classic RTTI - delegate to parent TRTTIPersister }
  inherited PopulateFieldMap(aClass, aMap);
  { Pass 2: Extended RTTI - TNullable<T> record fields }
  ScanNullableRecordFields(aClass, aMap);
end;

function TExtRTTIPersister.AcceptNullableRecordField(aClass: TClass; aRecordField: TRttiField): Boolean;
begin
  Result := True;
end;

function TExtRTTIPersister.NullableRecordColumnName(aClass: TClass; aRecordField: TRttiField): UTF8String;
begin
  { Column name: the field name without leading F }
  Result := aRecordField.Name;
  if (Length(Result) > 1) and (Result[1] = 'F') and (Result[2] in ['A'..'Z']) then
    Delete(Result, 1, 1);
end;

procedure TExtRTTIPersister.ScanNullableRecordFields(aClass: TClass; aMap: TOPFieldMapList);
var
  Ctx: TRttiContext;
  RttiClassType: TRttiType;
  ClassFields: specialize TArray<TRttiField>;
  RF: TRttiField;
  ColName: String;
begin
  Ctx := GetNullableRttiCtx;
  RttiClassType := Ctx.GetType(aClass);
  if RttiClassType = nil then
    Exit;
  ClassFields := RttiClassType.GetFields;
  for RF in ClassFields do
  begin
    if not IsNullableRecordField(RF) then
      Continue;
    if not AcceptNullableRecordField(aClass, RF) then
      Continue;
    ColName := NullableRecordColumnName(aClass, RF);
    { Skip if already in the field map (from classic RTTI pass) }
    if aMap.IndexOfPropertyName(ColName) >= 0 then
      Continue;
    if aMap.IndexOfFieldName(ColName) >= 0 then
      Continue;
    AddNullableRecordMap(aMap, aClass, RF, ColName);
  end;
end;

procedure TExtRTTIPersister.PersistFieldMember(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam);
var
  V: TValue;
  Obj: TObject;
begin
  V := TRttiField(aFieldMap.RttiMember).GetValue(Instance);
  case aFieldMap.PropertyKind of
    tkSet:
      if foAsString in aFieldMap.Options then
        aParam.AsString := SetText(aFieldMap, V)
      else
        aParam.AsInteger := SetOrdinal(V);
    tkVariant:
      aParam.AsString := VariantToJSON(V.AsVariant);
    tkClass:
      begin
        Obj := V.AsObject;
        if Obj = nil then
          aParam.Clear
        else if Obj is TStrings then
          aParam.AsString := TStrings(Obj).Text
        else
          aParam.AsString := OPSerializer.Serialize(Obj);
      end;
  else
    if not IsSupportedNullableKind(aFieldMap.PropertyKind) then
      RaiseError('PersistProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
    SetInnerParam(aFieldMap, V, aParam);
  end;
end;

procedure TExtRTTIPersister.ApplyFieldMember(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
var
  RF: TRttiField;
  V: TValue;
  Variant_: Variant;
  Obj: TObject;
begin
  RF := TRttiField(aFieldMap.RttiMember);
  if aField.IsNull then
  begin
    // No null state: the default value of the type
    if aFieldMap.PropertyKind <> tkClass then
    begin
      TValue.Make(nil, aFieldMap.PropertyTypeInfo, V);
      RF.SetValue(Instance, V);
    end
    else
    begin
      // As for a class property: an owned list or collection is emptied,
      // other objects are left as they are (their owner is not known)
      Obj := RF.GetValue(Instance).AsObject;
      if Obj is TStrings then
        TStrings(Obj).Clear
      else if Obj is TCollection then
        TCollection(Obj).Clear;
    end;
    Exit;
  end;
  case aFieldMap.PropertyKind of
    tkSet:
      begin
        MakeSetValue(aFieldMap, aField, V);
        RF.SetValue(Instance, V);
      end;
    tkVariant:
      begin
        Variant_ := JSONToVariant(aField.AsString);
        TValue.Make(@Variant_, TypeInfo(Variant), V);
        RF.SetValue(Instance, V);
      end;
    tkClass:
      begin
        // Fill the object the field holds; a nil field is left as is
        Obj := RF.GetValue(Instance).AsObject;
        if Obj is TStrings then
          TStrings(Obj).Text := aField.AsString
        else if Obj <> nil then
          OPSerializer.Deserialize(aField.AsString, Obj);
      end;
  else
    if not IsSupportedNullableKind(aFieldMap.PropertyKind) then
      RaiseError('ApplyProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
    MakeInnerValue(aFieldMap, aField, V);
    RF.SetValue(Instance, V);
  end;
end;

function TExtRTTIPersister.AddNullableRecordMap(aMap: TOPFieldMapList; aClass: TClass;
  aRecordField: TRttiField; const aColName: UTF8String): TOPFieldMap;
var
  InnerValField: TRttiField;
begin
  InnerValField := aRecordField.FieldType.GetField('FValue');
  // Fail at registration, not at the first Save or Load
  if not IsSupportedNullableKind(InnerValField.FieldType.TypeKind) then
    raise EOPF.CreateFmt(SErrUnsupportedNullableType,
      [aClass.ClassName, aRecordField.Name, InnerValField.FieldType.Name]);
  Result := aMap.AddField(aColName, aColName);
  Result.PropertyKind := InnerValField.FieldType.TypeKind;
  // The inner type: distinguishes TDateTime and Currency from plain floats
  Result.PropertyTypeInfo := InnerValField.FieldType.Handle;
  Result.Options := Result.Options + [foNullable];
  // The class-level TRttiField, used by Persist/Apply and the change trackers
  Result.MemberKind := omkNullableRecord;
  Result.RttiMember := aRecordField;
end;

function TExtRTTIPersister.ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean;
var
  RecordField: TRttiField;
  InnerVal: TValue;
begin
  { Level 2: TNullable<T> via extended RTTI (MemberKind omkNullableRecord) }
  if aFieldMap.MemberKind = omkNullableRecord then
  begin
    Result := True;
    RecordField := TRttiField(aFieldMap.RttiMember);
    if aField.IsNull then
      WriteNullableNull(Instance, RecordField)
    else
    begin
      if not IsSupportedNullableKind(aFieldMap.PropertyKind) then
        RaiseError('ApplyProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
      MakeInnerValue(aFieldMap, aField, InnerVal);
      WriteNullableValue(Instance, RecordField, InnerVal);
    end;
    Exit;
  end;

  { A class field (pkFields): through its TRttiField }
  if aFieldMap.MemberKind = omkField then
  begin
    Result := True;
    ApplyFieldMember(Instance, aFieldMap, aField);
    Exit;
  end;

  { Delegate to parent for Level 0/1 and classic RTTI }
  Result := inherited ApplyProperty(Instance, aFieldMap, aField);
end;

function TExtRTTIPersister.PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
var
  RecordField: TRttiField;
  InnerVal: TValue;
begin
  { Level 2: TNullable<T> via extended RTTI (MemberKind omkNullableRecord) }
  if aFieldMap.MemberKind = omkNullableRecord then
  begin
    Result := True;
    RecordField := TRttiField(aFieldMap.RttiMember);
    if not ReadNullableHasValue(Instance, RecordField) then
    begin
      aParam.Clear;
      Exit;
    end;
    InnerVal := ReadNullableValue(Instance, RecordField);
    if not IsSupportedNullableKind(aFieldMap.PropertyKind) then
      RaiseError('PersistProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
    SetInnerParam(aFieldMap, InnerVal, aParam);
    Exit;
  end;

  { A class field (pkFields): through its TRttiField }
  if aFieldMap.MemberKind = omkField then
  begin
    Result := True;
    PersistFieldMember(Instance, aFieldMap, aParam);
    Exit;
  end;

  { Delegate to parent for Level 0/1 and classic RTTI }
  Result := inherited PersistProperty(Instance, aFieldMap, aParam);
end;

{ TAttributePersister }

function AttrIsSupportedTypeKind(aKind: TTypeKind): Boolean;
begin
  Result := aKind in [tkSString, tkAString, tkLString, tkWString, tkUString,
                      tkChar, tkWChar, tkUChar,
                      tkInteger, tkInt64, tkQWord, tkFloat, tkBool, tkEnumeration, tkClass,
                      tkSet, tkVariant];
end;


function TAttributePersister.AcceptNullableRecordField(aClass: TClass; aRecordField: TRttiField): Boolean;
begin
  Result:=not aRecordField.HasAttribute(Opf.Attributes.OPSkip);
end;

function TAttributePersister.NullableRecordColumnName(aClass: TClass; aRecordField: TRttiField): UTF8String;
var
  A: TCustomAttribute;
begin
  A:=aRecordField.GetAttribute(Opf.Attributes.OPFieldName);
  if A<>nil then
    Result:=Opf.Attributes.OPFieldName(A).Name
  else
    Result:=inherited NullableRecordColumnName(aClass, aRecordField);
end;

class function TAttributePersister.GetTableName(aClass: TClass): UTF8String;
var
  Attr: TCustomAttribute;
begin
  Attr:=FindClassAttribute(aClass, Opf.Attributes.OPTableName);
  if Attr<>nil then
    Result:=Opf.Attributes.OPTableName(Attr).Name
  else
    Result:=inherited GetTableName(aClass);
end;

function TAttributePersister.GetPropAttributes(aPropInfo: PPropInfo): TMemberAttributes;
begin
  Result.Options:=FindPropAttribute(aPropInfo, Opf.Attributes.OPFieldOptions);
  Result.Sequence:=FindPropAttribute(aPropInfo, Opf.Attributes.OPSequence);
  Result.Size:=FindPropAttribute(aPropInfo, Opf.Attributes.OPSize);
  Result.Unique:=FindPropAttribute(aPropInfo, Opf.Attributes.OPUnique);
  Result.Default:=FindPropAttribute(aPropInfo, Opf.Attributes.OPDefault);
  Result.Relation:=FindPropAttribute(aPropInfo, Opf.Attributes.OPRelation);
end;

function TAttributePersister.GetFieldAttributes(aFieldEntry: PExtendedVmtFieldEntry): TMemberAttributes;
begin
  Result.Options:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPFieldOptions);
  Result.Sequence:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPSequence);
  Result.Size:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPSize);
  Result.Unique:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPUnique);
  Result.Default:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPDefault);
  Result.Relation:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPRelation);
end;

procedure TAttributePersister.AddDeferredRelation(aRelAttr: TCustomAttribute; const aColName: UTF8String);
var
  DR: TOPDeferredRelation;
begin
  DR.ParentClassName:=Opf.Attributes.OPRelation(aRelAttr).ParentClassName;
  DR.FKFieldName:=aColName;
  DR.FKName:=Opf.Attributes.OPRelation(aRelAttr).FKName;
  DR.DBCascade:=Opf.Attributes.OPRelation(aRelAttr).DBCascade;
  SetLength(FDeferredRelations, Length(FDeferredRelations)+1);
  FDeferredRelations[High(FDeferredRelations)]:=DR;
end;

procedure TAttributePersister.ApplyDefaultAttribute(aClass: TClass; F: TOPFieldMap;
  aDefAttr: TCustomAttribute; const aMemberKind, aMemberName: String);
var
  V: UTF8String;
begin
  V:=Opf.Attributes.OPDefault(aDefAttr).Value;
  try
    case F.PropertyKind of
      tkInteger: F.DefaultValue:=StrToInt(V);
      tkInt64:   F.DefaultValue:=StrToInt64(V);
      tkFloat:   F.DefaultValue:=StrToFloat(V, AttrFormatSettings);
      tkBool:    F.DefaultValue:=StrToBool(V);
      tkSString, tkAString, tkLString, tkUString, tkWString:
                 F.DefaultValue:=V;
    else
      RaiseError('PopulateFieldMap',
        Format('OPDefault on %s "%s" of class "%s": unsupported type kind %s',
        [aMemberKind, aMemberName, aClass.ClassName, GetEnumName(TypeInfo(TTypeKind), Ord(F.PropertyKind))]));
    end;
  except
    on E: EConvertError do
      RaiseError('PopulateFieldMap',
        Format('OPDefault on %s "%s" of class "%s": cannot convert "%s" - %s',
        [aMemberKind, aMemberName, aClass.ClassName, V, E.Message]));
  end;
end;

procedure TAttributePersister.ApplyMemberAttributes(aClass: TClass; F: TOPFieldMap;
  const aAttrs: TMemberAttributes; const aMemberKind, aMemberName: String);
begin
  if aAttrs.Options<>nil then
    F.Options:=F.Options + Opf.Attributes.OPFieldOptions(aAttrs.Options).Options;
  if aAttrs.Sequence<>nil then
    F.SequenceName:=Opf.Attributes.OPSequence(aAttrs.Sequence).SequenceName;
  if aAttrs.Size<>nil then
  begin
    F.MaxSize:=Opf.Attributes.OPSize(aAttrs.Size).MaxSize;
    F.MinSize:=Opf.Attributes.OPSize(aAttrs.Size).MinSize;
  end;
  if aAttrs.Unique<>nil then
    F.Options:=F.Options + [foUnique];
  if aAttrs.Default<>nil then
    ApplyDefaultAttribute(aClass, F, aAttrs.Default, aMemberKind, aMemberName);
  if aAttrs.Relation<>nil then
    AddDeferredRelation(aAttrs.Relation, F.FieldName);
end;

procedure TAttributePersister.AddPropertyMapping(aClass: TClass; aMap: TOPFieldMapList;
  aPropInfo: PPropInfo; const aColName: UTF8String);
var
  F: TOPFieldMap;
begin
  F:=aMap.AddField(aColName, aPropInfo^.Name);
  F.PropertyTypeInfo:=aPropInfo^.PropType;
  F.PropInfo:=aPropInfo;
  // Level 0 (stored field) and Level 1 (IOPFNullable), as TRTTIPersister
  MarkNullable(F, aPropInfo, ClassHasNullableIntf(aClass));
  ApplyMemberAttributes(aClass, F, GetPropAttributes(aPropInfo), 'property', aPropInfo^.Name);
end;

procedure TAttributePersister.ScanProperty(aClass: TClass; aMap: TOPFieldMapList; aPropInfo: PPropInfo);
var
  FnAttr: TCustomAttribute;
  ColName: UTF8String;
begin
  // Check conflict: OPSkip + OPFieldName
  if HasPropAttribute(aPropInfo, Opf.Attributes.OPSkip) and
     HasPropAttribute(aPropInfo, Opf.Attributes.OPFieldName) then
    RaiseError('PopulateFieldMap',
      Format('Conflicting attributes: OPSkip and OPFieldName on property "%s" of class "%s"',
      [aPropInfo^.Name, aClass.ClassName]));
  if HasPropAttribute(aPropInfo, Opf.Attributes.OPSkip) then
    Exit;
  if not AttrIsSupportedTypeKind(aPropInfo^.PropType^.Kind) then
  begin
    if aMap.SkipUnknownTypes then
      Exit;
    RaiseError('PopulateFieldMap',
      Format(SErrUnsupportedPropertyType, [aPropInfo^.Name]));
  end;
  FnAttr:=FindPropAttribute(aPropInfo, Opf.Attributes.OPFieldName);
  if FnAttr<>nil then
    ColName:=Opf.Attributes.OPFieldName(FnAttr).Name
  else
    ColName:=aPropInfo^.Name;
  AddPropertyMapping(aClass, aMap, aPropInfo, ColName);
end;

(* Try extended RTTI first (GetPropListEx) - works when class uses {$RTTI EXPLICIT}.
   Falls back to classic GetPropList for published properties (no property-level attributes). *)

procedure TAttributePersister.ScanProperties(aClass: TClass; aMap: TOPFieldMapList;
  aVisibilities: TVisibilityClasses);
var
  PropListOld: PPropList;
  PropListEx: PPropListEx;
  PropCount, I: Integer;
begin
  PropCount:=GetPropListEx(aClass, PropListEx, aVisibilities);
  if PropCount > 0 then
  begin
    // Extract PPropInfo from PPropInfoEx
    GetMem(PropListOld, PropCount * SizeOf(PPropInfo));
    for I:=0 to PropCount-1 do
      PropListOld^[I]:=PropListEx^[I]^.Info;
    FreeMem(PropListEx);
  end
  else
    // Fallback: classic RTTI (published only, no property attributes)
    PropCount:=GetPropList(aClass, PropListOld);
  if PropCount > 0 then
  try
    for I:=0 to PropCount-1 do
      ScanProperty(aClass, aMap, PropListOld^[I]);
  finally
    FreeMem(PropListOld);
  end;
end;

procedure TAttributePersister.ScanField(aClass: TClass; aMap: TOPFieldMapList;
  aFieldEntry: PExtendedVmtFieldEntry; aCheckPropertyDuplicates: Boolean);
var
  FnAttr: TCustomAttribute;
  FldName, ColName: UTF8String;
  F: TOPFieldMap;
  RF: TRttiField;
begin
  FldName:=aFieldEntry^.Name^;
  // A TNullable<T> record is supported as a field
  if not AttrIsSupportedTypeKind(aFieldEntry^.FieldType^^.Kind)
     and (aFieldEntry^.FieldType^^.Kind<>tkRecord) then
  begin
    if aMap.SkipUnknownTypes then
      Exit;
    RaiseError('PopulateFieldMap', Format(SErrUnsupportedPropertyType, [FldName]));
  end;
  // Check conflict: OPSkip + OPFieldName
  if HasFieldAttribute(aFieldEntry, Opf.Attributes.OPSkip) and
     HasFieldAttribute(aFieldEntry, Opf.Attributes.OPFieldName) then
    RaiseError('PopulateFieldMap',
      Format('Conflicting attributes: OPSkip and OPFieldName on field "%s" of class "%s"',
      [FldName, aClass.ClassName]));
  if HasFieldAttribute(aFieldEntry, Opf.Attributes.OPSkip) then
    Exit;
  ColName:=FieldToColumnName(FldName);
  FnAttr:=FindFieldAttribute(aFieldEntry, Opf.Attributes.OPFieldName);
  if FnAttr<>nil then
    ColName:=Opf.Attributes.OPFieldName(FnAttr).Name;
  // Duplicate detection: property + field both present
  if aCheckPropertyDuplicates and (aMap.IndexOfPropertyName(ColName) >= 0) then
    RaiseError('PopulateFieldMap',
      Format('Duplicate mapping: both property and field map to "%s" in class "%s". Use OPSkip on one.',
      [ColName, aClass.ClassName]));
  if aMap.IndexOfFieldName(ColName) >= 0 then
    Exit; // Already added (e.g. from property pass)
  RF:=GetNullableRttiCtx.GetType(aClass).GetField(FldName);
  if RF=nil then
    RaiseError('PopulateFieldMap', Format(SErrUnsupportedPropertyType, [FldName]));
  if IsNullableRecordField(RF) then
    F:=AddNullableRecordMap(aMap, aClass, RF, ColName)
  else if aFieldEntry^.FieldType^^.Kind=tkRecord then
  begin
    // Only TNullable<T> records can be stored
    if aMap.SkipUnknownTypes then
      Exit;
    RaiseError('PopulateFieldMap', Format(SErrUnsupportedPropertyType, [FldName]));
  end
  else
  begin
    F:=aMap.AddField(ColName, ColName);
    F.PropertyKind:=aFieldEntry^.FieldType^^.Kind;
    F.PropertyTypeInfo:=aFieldEntry^.FieldType^;
    // Read and written through the TRttiField: there is no property
    F.MemberKind:=omkField;
    F.RttiMember:=RF;
  end;
  ApplyMemberAttributes(aClass, F, GetFieldAttributes(aFieldEntry), 'field', FldName);
  // A set of more than 32 elements does not fit in an integer column
  if (F.PropertyKind=tkSet) and not (foAsString in F.Options)
     and (GetTypeData(F.PropertyTypeInfo)^.SetSize>SizeOf(LongInt)) then
    RaiseError('PopulateFieldMap', Format(SErrLargeSetNeedsAsString,
      [FldName, aClass.ClassName, GetTypeData(F.PropertyTypeInfo)^.SetSize]));
end;

procedure TAttributePersister.ScanFields(aClass: TClass; aMap: TOPFieldMapList;
  aVisibilities: TVisibilityClasses; aCheckPropertyDuplicates: Boolean);
var
  FieldInfos: PExtendedFieldInfoTable;
  FieldCount, I: Integer;
begin
  FieldCount:=GetFieldInfos(aClass, nil, aVisibilities);
  if FieldCount <= 0 then
    Exit;
  GetMem(FieldInfos, FieldCount * SizeOf(PExtendedVmtFieldEntry));
  try
    GetFieldInfos(aClass, FieldInfos, aVisibilities);
    for I:=0 to FieldCount-1 do
      if FieldInfos^[I]^.FieldType <> nil then
        ScanField(aClass, aMap, FieldInfos^[I], aCheckPropertyDuplicates);
  finally
    FreeMem(FieldInfos);
  end;
end;

procedure TAttributePersister.ScanExplicitProperty(aClass: TClass; aMap: TOPFieldMapList;
  aPropInfo: PPropInfo);
var
  FnAttr: TCustomAttribute;
  ColName: UTF8String;
begin
  FnAttr:=FindPropAttribute(aPropInfo, Opf.Attributes.OPFieldName);
  if FnAttr=nil then
    Exit;
  ColName:=Opf.Attributes.OPFieldName(FnAttr).Name;
  // Already in map?
  if aMap.IndexOfFieldName(ColName) >= 0 then
    Exit;
  if aMap.IndexOfPropertyName(aPropInfo^.Name) >= 0 then
    Exit;
  if HasPropAttribute(aPropInfo, Opf.Attributes.OPSkip) then
    RaiseError('PopulateFieldMap',
      Format('Conflicting attributes: OPSkip and OPFieldName on property "%s" of class "%s"',
      [aPropInfo^.Name, aClass.ClassName]));
  if not AttrIsSupportedTypeKind(aPropInfo^.PropType^.Kind) then
    Exit;
  AddPropertyMapping(aClass, aMap, aPropInfo, ColName);
end;

(*
  Re-scan published properties for OPFieldName explicit includes
  that may have been added after the initial visibility pass.
  Note: including non-published properties via OPFieldName requires {$RTTI EXPLICIT}
  which is not automatically generated by FPC. For now, this pass only covers
  published properties (same as the property pass).
*)

procedure TAttributePersister.ScanExplicitProperties(aClass: TClass; aMap: TOPFieldMapList);
var
  PropList: PPropList;
  PropCount, I: Integer;
begin
  PropCount:=GetPropList(aClass, PropList);
  if PropCount > 0 then
  try
    for I:=0 to PropCount-1 do
      ScanExplicitProperty(aClass, aMap, PropList^[I]);
  finally
    FreeMem(PropList);
  end;
end;

procedure TAttributePersister.ResolvePrimaryKey(aClass: TClass; aMap: TOPFieldMapList);
var
  PKAttr: TCustomAttribute;
  FieldNames: specialize TArray<String>;
  FldName: UTF8String;
  F: TOPFieldMap;
  I: Integer;
begin
  PKAttr:=FindClassAttribute(aClass, Opf.Attributes.OPPrimaryKey);
  if PKAttr=nil then
    Exit;
  FHasAttributePK:=True;
  FPKConstraintName:=Opf.Attributes.OPPrimaryKey(PKAttr).ConstraintName;
  FieldNames:=String(Opf.Attributes.OPPrimaryKey(PKAttr).Fields).Split([',']);
  for I:=0 to High(FieldNames) do
  begin
    FldName:=Trim(FieldNames[I]);
    if FldName='' then Continue;
    F:=aMap.FindByPropertyName(FldName);
    if F=nil then
      F:=aMap.FindByFieldName(FldName);
    if F=nil then
      RaiseError('PopulateFieldMap',
        Format('OPPrimaryKey references field "%s" not found in field map for class "%s"',
        [FldName, aClass.ClassName]));
    F.Options:=F.Options + [foInKey];
  end;
end;

procedure TAttributePersister.CollectIndexes(aClass: TClass; aMap: TOPFieldMapList);
var
  AT: PAttributeTable;
  Attr: TCustomAttribute;
  FieldNames: specialize TArray<String>;
  FldName: UTF8String;
  IdxDef: TOPIndexDef;
  I, J: Integer;
begin
  AT:=GetAttributeTable(aClass.ClassInfo);
  if AT=nil then
    Exit;
  for I:=0 to AT^.AttributeCount-1 do
  begin
    Attr:=OPFCachedAttribute(AT, I);
    if not (Attr is Opf.Attributes.OPIndex) then
      Continue;
    IdxDef.FieldNames:=Opf.Attributes.OPIndex(Attr).Fields;
    IdxDef.IndexName:=Opf.Attributes.OPIndex(Attr).IndexName;
    // Validate field names exist in map
    FieldNames:=String(IdxDef.FieldNames).Split([',']);
    for J:=0 to High(FieldNames) do
    begin
      FldName:=Trim(FieldNames[J]);
      if FldName='' then Continue;
      if (aMap.IndexOfFieldName(FldName) < 0) and (aMap.IndexOfPropertyName(FldName) < 0) then
        RaiseError('PopulateFieldMap',
          Format('OPIndex "%s" references field "%s" not found in field map for class "%s"',
          [IdxDef.IndexName, FldName, aClass.ClassName]));
    end;
    SetLength(FIndexes, Length(FIndexes)+1);
    FIndexes[High(FIndexes)]:=IdxDef;
  end;
end;

procedure TAttributePersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
var
  VisAttr, PersistAttr: TCustomAttribute;
  Visibilities: TVisibilityClasses;
  PersistKind: TOPPersistKind;
begin
  SetLength(FDeferredRelations, 0);
  SetLength(FIndexes, 0);
  FPKConstraintName:='';
  FHasAttributePK:=False;
  // Determine visibility scope
  VisAttr:=FindClassAttribute(aClass, Opf.Attributes.OPVisibility);
  if VisAttr<>nil then
    Visibilities:=Opf.Attributes.OPVisibility(VisAttr).Visibilities
  else
    Visibilities:=[vcPublished];
  // Determine persist kind
  PersistAttr:=FindClassAttribute(aClass, Opf.Attributes.OPPersistKind);
  if PersistAttr<>nil then
    PersistKind:=Opf.Attributes.OPPersistKind(PersistAttr).Kind
  else
    PersistKind:=pkProperties;
  // Pass 1: properties, pass 2: fields, pass 3: explicit OPFieldName includes
  if PersistKind in [pkProperties, pkBoth] then
    ScanProperties(aClass, aMap, Visibilities);
  if PersistKind in [pkFields, pkBoth] then
    ScanFields(aClass, aMap, Visibilities, PersistKind = pkBoth);
  if PersistKind in [pkProperties, pkBoth] then
    ScanExplicitProperties(aClass, aMap);
  // Level 2 TNullable<T> fields next to properties (the field pass maps them itself)
  if PersistKind = pkProperties then
    ScanNullableRecordFields(aClass, aMap);
  ResolvePrimaryKey(aClass, aMap);
  CollectIndexes(aClass, aMap);
end;

initialization
  AttrFormatSettings:=DefaultFormatSettings;
  AttrFormatSettings.DecimalSeparator:='.';
  OPFNullableMemberReader:=@ExtRTTINullableMemberReader;
  NullableRttiCtx := TRttiContext.Create(False);
  NullableRttiCtxCreated := True;

finalization
  NullableRttiCtx.Free;

end.
