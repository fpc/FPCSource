{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    SQL generation for persisting

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Fields;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Variants, Data.Db,
{$ELSE}
  Classes, SysUtils, typinfo, Variants, db,
{$ENDIF}
  Opf.Types;

Type

  TOPFieldMapList = Class;

  TOPFieldOption = (foInKey, foInSelect, foInInsert, foInUpdate, foRequired, foAutoInc, foUUID, foAsString, foNullable, foUnique,
                    foCreatedAt, foUpdatedAt, foCreatedBy, foUpdatedBy,
                    foSoftDelete,
                    foVersion,
                    // Store a TDateTime/TDate/TTime as a number (legacy schemas)
                    foDateTimeAsFloat);
  TOPFieldOptions = Set of TOPFieldOption;

  // How a mapped value is accessed
  TOPMemberKind = (omkProperty, omkField, omkNullableRecord);

  TOPFieldMap = class;
  TOPValueGenerateEvent = procedure(aInstance: TObject; aField: TOPFieldMap; aConnection: TObject) of object;

  { TOPFieldMap }

  TOPFieldMap = class(TCollectionItem)
  private
    FData: Pointer;
    FDefaultValue: Variant;
    FFieldName: UTF8String;
    FOnGenerateValue: TOPValueGenerateEvent;
    FPropInfo: PPropInfo;
    FPropertyKind: TTypeKind;
    FReferencedPropertyName: UTF8String;
    fSequenceName: String;
    FMaxBound: Integer;
    FMinBound: Integer;
    FOptions: TOPFieldOptions;
    FPropertyName: UTF8String;
    FTableName : UTF8String;
    FTypeInfo: PTypeInfo;
    FMemberKind: TOPMemberKind;
    FRttiMember: TObject;
    function GetReferencedPropertyName: UTF8String;
    procedure SetOptions(AValue: TOPFieldOptions);
    procedure SetPropertyKind(AValue: TTypeKind);
    procedure CheckKindOptions(aKind: TTypeKind; const aOptions: TOPFieldOptions);
  Protected
    procedure SetTypeInfo(AValue: PTypeInfo); virtual;
    function GetFieldName: UTF8String; virtual;
    function GetPropertyName: UTF8String; virtual;
    procedure SetFieldName(AValue: UTF8String); virtual;
    procedure SetPropertyName(AValue: UTF8String); virtual;
    Function GetDisplayName: string; override;
  Public
    // Copies the settings of another field mapping into this one
    Procedure Assign(Source: TPersistent); override;
    // For use by user: the framework does not use it
    Property Data : Pointer Read FData Write FData;
    // Where the value is kept: a published property, a class field or a nullable record field
    Property MemberKind : TOPMemberKind Read FMemberKind Write FMemberKind;
    // The class field or record field that holds the value, when it is not a published property
    Property RttiMember : TObject Read FRttiMember Write FRttiMember;
    // Custom value generator event - called before INSERT to set field value
    Property OnGenerateValue : TOPValueGenerateEvent Read FOnGenerateValue Write FOnGenerateValue;
    // Full property info pointer - for stored-specifier inspection
    Property PropInfo : PPropInfo Read FPropInfo Write FPropInfo;
    // Type information of the property
    Property PropertyTypeInfo : PTypeInfo Read FTypeInfo Write SetTypeInfo;
  Published
    // Table name can be empty, then table name of persister will be used.
    Property TableName : UTF8String Read FTableName Write FTableName;
    // Field name in database
    Property FieldName : UTF8String Read GetFieldName Write SetFieldName;
    // Object property name
    Property PropertyName : UTF8String Read GetPropertyName Write SetPropertyName;
    // Name of the property referred to by this field
    Property ReferencedPropertyName : UTF8String Read GetReferencedPropertyName Write FReferencedPropertyName;
    // Object property type. foVersion and foSoftDelete need a kind of the
    // right type: they are checked when the kind or the options are set (an
    // unknown kind, tkUnknown, is checked once the kind is set).
    Property PropertyKind : TTypeKind Read FPropertyKind Write SetPropertyKind;
    // Sequence name if this field gets a sequence value on insert
    Property SequenceName : String Read fSequenceName Write FSequenceName;
    // Options that control how the field is stored and used
    Property Options : TOPFieldOptions Read FOptions Write SetOptions;
    // The following are only needed when creating table create SQL.
    // Min bound: how this is interpreted depends on the field type.
    Property MinSize : Integer Read FMinBound Write FMinBound;
    // Max bound: how this is interpreted depends on the field type.
    // For string fields this is the maximum length
    Property MaxSize : Integer Read FMaxBound Write FMaxBound;
    // Default value of the column when the table is created; Null means no default
    Property DefaultValue : Variant Read FDefaultValue Write FDefaultValue;
  end;
  TOPFieldMapClass = Class of TOPFieldMap;
  TOPFieldMapArray = Array of TOPFieldMap;

  TUTF8StringArray = Array of UTF8string;

  // Formats a database identifier (quoting/transforming) for use in SQL.
  TOPIdentifierFormatter = function(const aName: UTF8String): UTF8String of object;
  // Escape text for a LIKE pattern so % and _ match themselves (escape character !)
  TOPLikeEscaper = function(const aText: UTF8String): UTF8String of object;

  TOPIndexDef = Record
    FieldNames: UTF8String;
    IndexName: UTF8String;
  end;
  TOPIndexDefArray = Array of TOPIndexDef;

  TOPFieldPair = Record
    DBField : TField;
    OPField :TOPFieldMap;
  end;
  TOPFieldPairArray = Array of TOPFieldPair;

  { TFieldMapEnumerator }

  TFieldMapEnumerator = class
  private
    FMap: TOPFieldMapList;
    FPosition: Integer;
  public
    // Creates an enumerator that walks over the fields of the given list
    constructor Create(AMap: TOPFieldMapList);
    // Returns the field at the current position
    function GetCurrent: TOPFieldMap;
    // Moves to the next field; returns False when there are no more fields
    function MoveNext: Boolean;
    // The field at the current position
    property Current: TOPFieldMap read GetCurrent;
  end;

  { TOPFieldMapList }

  TOPFieldMapList = class(TCollection)
  private
    FArray : TOPFieldMapArray;
    FDefaultFieldOptions: TOPFieldOptions;
    FSkipUnknownTypes: Boolean;
    function GetFields(aIndex : Integer): TOPFieldMap;
    procedure SetFields(aIndex : Integer; AValue: TOPFieldMap);
  Protected
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
  Public
    // Copies all field mappings and settings of another list into this one
    Procedure Assign(Source: TPersistent); override;
    // Returns an enumerator so the list can be used in a for..in loop
    Function GetEnumerator: TFieldMapEnumerator;
    // Returns all field mappings as an array
    Function AsArray : TOPFieldMapArray;
    // Adds a field mapping that uses the default field options
    Function AddField(Const aFieldName : UTF8String; const aPropertyName : String) : TOPFieldMap; overload;
    // Adds a field mapping with the given options
    Function AddField(Const aFieldName : UTF8String; const aPropertyName : String; aOptions : TOPFieldOptions) : TOPFieldMap; overload;
    // With the property kind: needed for foVersion and foSoftDelete, which check it
    Function AddField(Const aFieldName : UTF8String; const aPropertyName : String; aKind : TTypeKind; aOptions : TOPFieldOptions) : TOPFieldMap; overload;
    // Position of the field with the given column name, or -1 if not found
    function indexOfFieldName(const aFieldName: UTF8String) : Integer;
    // The field with the given column name, or Nil if not found
    Function FindByFieldName(const aFieldName: UTF8String) : TOPFieldMap;
    // Position of the field with the given property name, or -1 if not found
    function indexOfPropertyName(const aPropertyName: UTF8String) : Integer;
    // The field with the given property name, or Nil if not found
    Function FindByPropertyName(const aPropertyName: UTF8String) : TOPFieldMap;
    // Raises a field mapping error with the given message
    procedure RaiseError(const aOperation, aReason: string); virtual; overload;
    // Raises a field mapping error with a message built from a format string and arguments
    procedure RaiseError(const aOperation, aFmt: string; const aArgs: array of const); virtual; overload;
    // The first field that is part of the primary key, or Nil if there is none
    Function FindPrimaryKeyField : TOPFieldMap;
    // The field that marks a record as deleted, or Nil if there is none
    Function FindSoftDeleteField : TOPFieldMap;
    // The field that holds the record version number, or Nil if there is none
    Function FindVersionField : TOPFieldMap;
    // The field mappings, by position
    Property Fields[aIndex : Integer] : TOPFieldMap read GetFields write SetFields; default;
    // Options given to fields added without explicit options
    Property DefaultFieldOptions : TOPFieldOptions Read FDefaultFieldOptions Write FDefaultFieldOptions;
    // Skip properties of unsupported types instead of raising an error when mapping a class
    Property SkipUnknownTypes : Boolean Read FSkipUnknownTypes Write FSkipUnknownTypes;
  end;
  TOPFieldMapListClass = Class of TOPFieldMapList;

  { What a tkFloat property really holds }
  TOPFloatKind = (fkUnknown, fkSingle, fkDouble, fkExtended, fkComp, fkCurrency,
                  fkDateTime, fkDate, fkTime);

  { How date/time properties are stored:
    dtsNative: TIMESTAMP/DATE/TIME columns, date/time parameters (default)
    dtsFloat: numeric columns and parameters, as before version 1.0.
    A single field can keep the numeric storage with foDateTimeAsFloat. }
  TOPDateTimeStorage = (dtsNative, dtsFloat);

var
  OPFDateTimeStorage: TOPDateTimeStorage = dtsNative;

{ Text escaped for a LIKE pattern with escape character !: ! % and _ are
  preceded by ! (use with ESCAPE '!') }
function OPFEscapeLikeText(const aText: UTF8String): UTF8String;

// Kind of a tkFloat field, from its type info (fkUnknown without type info)
function OPFFloatKind(aField: TOPFieldMap): TOPFloatKind;
// True if aField is a date/time that is stored natively (not as a number)
function OPFIsNativeDateTime(aField: TOPFieldMap): Boolean;
// Set a parameter for a tkFloat field with the matching data type:
// date/time for native dates, BCD for currency (exact), float otherwise
procedure OPFSetFloatParam(aParam: TParam; aField: TOPFieldMap; const aValue: Extended);
// Read a tkFloat field value from a result field
function OPFGetFloatFieldValue(aDBField: TField; aField: TOPFieldMap): Extended;
// Set a parameter to an exact currency value (BCD, not money)
procedure OPFSetCurrencyParam(aParam: TParam; const aValue: Currency);
// The value of a Currency property, read without a detour through Extended:
// where Extended is Double (Win64, AArch64) that loses digits of large values
function OPFGetCurrencyProp(aInstance: TObject; aPropInfo: PPropInfo): Currency;
// Set a Currency property, without a detour through Extended
procedure OPFSetCurrencyProp(aInstance: TObject; aPropInfo: PPropInfo; const aValue: Currency);

implementation

uses Opf.Consts;

function OPFEscapeLikeText(const aText: UTF8String): UTF8String;
var
  I: Integer;
begin
  Result:='';
  for I:=1 to Length(aText) do
  begin
    if aText[I] in ['!', '%', '_'] then
      Result:=Result+'!';
    Result:=Result+aText[I];
  end;
end;

function OPFFloatKind(aField: TOPFieldMap): TOPFloatKind;
var
  TI: PTypeInfo;
  N: String;
begin
  Result:=fkUnknown;
  if (aField=Nil) or (aField.PropertyKind<>tkFloat) then
    Exit;
  TI:=aField.PropertyTypeInfo;
  if TI=Nil then
    Exit;
  if TI=TypeInfo(TDateTime) then
    Exit(fkDateTime);
  if TI=TypeInfo(TDate) then
    Exit(fkDate);
  if TI=TypeInfo(TTime) then
    Exit(fkTime);
  // Aliases declared elsewhere have their own type info: go by name
  N:=UpperCase(TI^.Name);
  if N='TDATETIME' then
    Exit(fkDateTime);
  if N='TDATE' then
    Exit(fkDate);
  if N='TTIME' then
    Exit(fkTime);
  // Qualified: Data.Db declares ftSingle as well
  case GetTypeData(TI)^.FloatType of
    TFloatType.ftSingle: Result:=fkSingle;
    TFloatType.ftDouble: Result:=fkDouble;
    TFloatType.ftExtended: Result:=fkExtended;
    TFloatType.ftComp: Result:=fkComp;
    TFloatType.ftCurr: Result:=fkCurrency;
  end;
end;

function OPFIsNativeDateTime(aField: TOPFieldMap): Boolean;
begin
  Result:=(OPFFloatKind(aField) in [fkDateTime, fkDate, fkTime])
          and (OPFDateTimeStorage=dtsNative)
          and not (foDateTimeAsFloat in aField.Options);
end;

procedure OPFSetFloatParam(aParam: TParam; aField: TOPFieldMap; const aValue: Extended);
begin
  if OPFIsNativeDateTime(aField) then
    case OPFFloatKind(aField) of
      fkDate: aParam.AsDate:=aValue;
      fkTime: aParam.AsTime:=aValue;
    else
      aParam.AsDateTime:=aValue;
    end
  else if OPFFloatKind(aField)=fkCurrency then
    OPFSetCurrencyParam(aParam, Currency(aValue))
  else
    aParam.AsFloat:=aValue;
end;

procedure OPFSetCurrencyParam(aParam: TParam; const aValue: Currency);
begin
  // Not AsCurrency: PostgreSQL sends ftCurrency as money (2 decimals)
  aParam.AsBCD:=aValue;
end;

type
  TGetCurrencyProc = function: Currency of object;
  TGetCurrencyProcIndex = function(aIndex: Integer): Currency of object;
  TSetCurrencyProc = procedure(const aValue: Currency) of object;
  TSetCurrencyProcIndex = procedure(aIndex: Integer; aValue: Currency) of object;

{ The method of a property accessor (aProc: GetProc or SetProc, aKind: its
  ptStatic or ptVirtual kind) }

function AccessorMethod(aInstance: TObject; aProc: CodePointer; aKind: Byte): TMethod;
begin
  if aKind=ptStatic then
    Result.Code:=aProc
  else
    Result.Code:=PCodePointer(Pointer(aInstance.ClassType)+PtrUInt(aProc))^;
  Result.Data:=aInstance;
end;

function OPFGetCurrencyProp(aInstance: TObject; aPropInfo: PPropInfo): Currency;
var
  M: TMethod;
begin
  if GetTypeData(aPropInfo^.PropType)^.FloatType<>TFloatType.ftCurr then
    Exit(GetFloatProp(aInstance, aPropInfo));
  case aPropInfo^.PropProcs and 3 of
    ptField:
      Result:=PCurrency(Pointer(aInstance)+PtrUInt(aPropInfo^.GetProc))^;
    ptStatic, ptVirtual:
      begin
        M:=AccessorMethod(aInstance, aPropInfo^.GetProc, aPropInfo^.PropProcs and 3);
        if ((aPropInfo^.PropProcs shr 6) and 1)=0 then
          Result:=TGetCurrencyProc(M)()
        else
          Result:=TGetCurrencyProcIndex(M)(aPropInfo^.Index);
      end;
  else
    Result:=GetFloatProp(aInstance, aPropInfo);
  end;
end;

procedure OPFSetCurrencyProp(aInstance: TObject; aPropInfo: PPropInfo; const aValue: Currency);
var
  M: TMethod;
begin
  if GetTypeData(aPropInfo^.PropType)^.FloatType<>TFloatType.ftCurr then
  begin
    SetFloatProp(aInstance, aPropInfo, aValue);
    Exit;
  end;
  case (aPropInfo^.PropProcs shr 2) and 3 of
    ptField:
      PCurrency(Pointer(aInstance)+PtrUInt(aPropInfo^.SetProc))^:=aValue;
    ptStatic, ptVirtual:
      begin
        M:=AccessorMethod(aInstance, aPropInfo^.SetProc, (aPropInfo^.PropProcs shr 2) and 3);
        if ((aPropInfo^.PropProcs shr 6) and 1)=0 then
          TSetCurrencyProc(M)(aValue)
        else
          TSetCurrencyProcIndex(M)(aPropInfo^.Index, aValue);
      end;
  else
    // Read-only: as SetFloatProp
    SetFloatProp(aInstance, aPropInfo, aValue);
  end;
end;

function OPFGetFloatFieldValue(aDBField: TField; aField: TOPFieldMap): Extended;
begin
  // AsDateTime also reads a date/time stored as a number
  if OPFIsNativeDateTime(aField) then
    Result:=aDBField.AsDateTime
  else if OPFFloatKind(aField)=fkCurrency then
    Result:=aDBField.AsCurrency
  else
    Result:=aDBField.AsFloat;
end;

{ TFieldMapEnumerator }

constructor TFieldMapEnumerator.Create(AMap: TOPFieldMapList);
begin
  FMap:=aMap;
  FPosition:=-1;
end;

function TFieldMapEnumerator.GetCurrent: TOPFieldMap;
begin
  Result:=FMap[fPosition]
end;

function TFieldMapEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result:=FPosition<FMap.Count;
end;

{ TOPFieldMap }


function TOPFieldMap.GetPropertyName: UTF8String;
begin
  Result:=FPropertyName
end;

function TOPFieldMap.GetReferencedPropertyName: UTF8String;
begin
  Result:=FReferencedPropertyName;
  if Result='' then
    Result:=PropertyName;
end;

{ The type checks of foSoftDelete and foVersion. tkUnknown: the kind is not
  known yet (AddField without kind); the check is made when it is set. }

procedure TOPFieldMap.CheckKindOptions(aKind: TTypeKind; const aOptions: TOPFieldOptions);
begin
  if aKind=tkUnknown then
    Exit;
  if (foSoftDelete in aOptions) and not (aKind in [tkBool, tkFloat]) then
    raise EFieldMap.CreateFmt(SErrSoftDeleteFieldType, [FPropertyName]);
  if (foVersion in aOptions) and not (aKind in [tkInteger, tkInt64, tkSString, tkAString, tkUString, tkWString]) then
    raise EFieldMap.CreateFmt(SErrVersionFieldType, [FPropertyName]);
end;

procedure TOPFieldMap.SetPropertyKind(AValue: TTypeKind);
begin
  if FPropertyKind=AValue then Exit;
  CheckKindOptions(AValue, FOptions);
  FPropertyKind:=AValue;
end;

procedure TOPFieldMap.SetOptions(AValue: TOPFieldOptions);
var
  I: Integer;
  List: TOPFieldMapList;
begin
  if ([foRequired, foNullable] * AValue) = [foRequired, foNullable] then
    raise EFieldMap.Create(SErrConflictingFieldOptions);
  CheckKindOptions(FPropertyKind, AValue);
  if (foSoftDelete in AValue) then
  begin
    if (Collection is TOPFieldMapList) then
    begin
      List := TOPFieldMapList(Collection);
      for I := 0 to List.Count - 1 do
        if (List.Fields[I] <> Self) and (foSoftDelete in List.Fields[I].Options) then
          raise EFieldMap.Create(SErrDuplicateSoftDeleteField);
    end;
  end;
  if (foVersion in AValue) then
  begin
    if (foInKey in AValue) then
      raise EFieldMap.CreateFmt(SErrVersionFieldInKey, [FPropertyName]);
    if (Collection is TOPFieldMapList) then
    begin
      List := TOPFieldMapList(Collection);
      for I := 0 to List.Count - 1 do
        if (List.Fields[I] <> Self) and (foVersion in List.Fields[I].Options) then
          raise EFieldMap.Create(SErrDuplicateVersionField);
    end;
  end;
  FOptions := AValue;
end;

procedure TOPFieldMap.SetTypeInfo(AValue: PTypeInfo);
begin
  if FTypeInfo=AValue then Exit;
  if Assigned(AValue) then
    SetPropertyKind(AValue^.Kind);
  FTypeInfo:=AValue;
end;

function TOPFieldMap.GetFieldName: UTF8String;
begin
  Result:=FFieldName;
  If (Result='') then
    Result:=FPropertyName;
end;

procedure TOPFieldMap.SetFieldName(AValue: UTF8String);
begin
  if (FFieldName=AValue) then Exit;
  FFieldName:=AValue;
end;

procedure TOPFieldMap.SetPropertyName(AValue: UTF8String);
begin
  FPropertyName:=aValue;
end;


procedure TOPFieldMap.Assign(Source: TPersistent);

Var
  F : TOPFieldMap;

begin
  if (Source is TOPFieldMap) then
    begin
    F:=source as TOPFieldMap;
    FieldName:=F.FieldName;
    FPropertyName:=F.FPropertyName;
    FPropertyKind:=F.FPropertyKind;
    Options:=F.Options;
    MinSize:=F.MinSize;
    MaxSize:=F.MaxSize;
    SequenceName:=F.SequenceName;
    TableName:=F.TableName;
    FData:=F.Data;
    FPropertyKind:=F.FPropertyKind;
    FTypeInfo:=F.FTypeInfo;
    FReferencedPropertyName:=F.FReferencedPropertyName;
    FOnGenerateValue:=F.FOnGenerateValue;
    FPropInfo:=F.FPropInfo;
    FDefaultValue:=F.FDefaultValue;
    FMemberKind:=F.FMemberKind;
    FRttiMember:=F.FRttiMember
    end
  else
    inherited Assign(Source);
end;

function TOPFieldMap.GetDisplayName: string;
begin
  Result:=PropertyName;
end;

{ TOPFieldMapList }

function TOPFieldMapList.GetFields(aIndex: Integer): TOPFieldMap;
begin
  Result:=TOPFieldMap(Items[aIndex])
end;

procedure TOPFieldMapList.SetFields(aIndex : Integer; AValue: TOPFieldMap);
begin
  Items[aIndex]:=aValue;
end;

procedure TOPFieldMapList.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  SetLength(FArray,0);
end;


procedure TOPFieldMapList.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TOPFieldMapList then
    FSkipUnknownTypes := TOPFieldMapList(Source).FSkipUnknownTypes;
end;

function TOPFieldMapList.GetEnumerator: TFieldMapEnumerator;
begin
  Result:=TFieldMapEnumerator.Create(Self);
end;

function TOPFieldMapList.AsArray: TOPFieldMapArray;

Var
  I : Integer;

begin
  // Also rebuilt when items were reordered (Index:=, Exchange), which does
  // not notify the collection
  I:=0;
  if Count=Length(FArray) then
    While (I<Count) and (FArray[I]=Fields[I]) do
      Inc(I);
  if (I<Count) or (Count<>Length(FArray)) then
    begin
    // A new array: a caller may still hold the previous one
    FArray:=Nil;
    SetLength(FArray,Count);
    For I:=0 to Count-1 do
      FArray[i]:=Fields[i];
    end;
  Result:=FArray;
end;

function TOPFieldMapList.AddField(const aFieldName: UTF8String; const aPropertyName: String): TOPFieldMap;
begin
  Result:=AddField(aFieldName,aPropertyName,DefaultFieldOptions);
end;

procedure TOPFieldMapList.RaiseError(const aOperation, aReason: string);
begin
  OPFRaiseError(EFieldMap, ClassName, aOperation, aReason);
end;

procedure TOPFieldMapList.RaiseError(const aOperation, aFmt: string; const aArgs: array of const);
begin
  OPFRaiseError(EFieldMap, ClassName, aOperation, aFmt, aArgs);
end;

function TOPFieldMapList.AddField(const aFieldName: UTF8String; const aPropertyName : String; aOptions: TOPFieldOptions
  ): TOPFieldMap;
begin
  Result:=AddField(aFieldName, aPropertyName, tkUnknown, aOptions);
end;

function TOPFieldMapList.AddField(const aFieldName: UTF8String; const aPropertyName: String;
  aKind: TTypeKind; aOptions: TOPFieldOptions): TOPFieldMap;
var
  ColName: UTF8String;
begin
  // An empty field name stands for the property name (see FieldName)
  ColName:=aFieldName;
  if ColName='' then
    ColName:=aPropertyName;
  if IndexOfFieldName(ColName)<>-1 then
    RaiseError('Add', SDuplicateFieldName, [ColName]);
  if IndexOfPropertyName(aPropertyName)<>-1 then
    RaiseError('Add', SDuplicatePropertyName, [aPropertyName]);
  Result:=Add as TOPFieldMap;
  try
    Result.FieldName:=aFieldName;
    Result.PropertyName:=aPropertyName;
    // The kind first: foVersion and foSoftDelete check it
    Result.PropertyKind:=aKind;
    Result.Options:=aOptions;
  except
    // Options rejected: no half-made field is left in the list
    Result.Free;
    raise;
  end;
end;

function TOPFieldMapList.indexOfFieldName(const aFieldName : UTF8String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aFieldName,GetFields(Result).FieldName) do
    Dec(Result);
end;

function TOPFieldMapList.FindByFieldName(const aFieldName: UTF8String
  ): TOPFieldMap;
Var
  I : Integer;
begin
  I:=indexOfFieldName(aFieldName);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetFields(I);
end;

function TOPFieldMapList.indexOfPropertyName(const aPropertyName : UTF8String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aPropertyName,GetFields(Result).PropertyName) do
    Dec(Result);
end;

function TOPFieldMapList.FindByPropertyName(const aPropertyName: UTF8String): TOPFieldMap;
Var
  I : Integer;
begin
  I:=indexOfPropertyName(aPropertyName);
  if (I=-1) then
    Result:=Nil
  else
    Result:=GetFields(I);
end;

function TOPFieldMapList.FindPrimaryKeyField: TOPFieldMap;
var
  I: Integer;
begin
  for I:=0 to Count-1 do
    if foInKey in Fields[I].Options then
      Exit(Fields[I]);
  Result:=Nil;
end;

function TOPFieldMapList.FindSoftDeleteField: TOPFieldMap;
var
  I: Integer;
begin
  for I:=0 to Count-1 do
    if foSoftDelete in Fields[I].Options then
      Exit(Fields[I]);
  Result:=Nil;
end;

function TOPFieldMapList.FindVersionField: TOPFieldMap;
var
  I: Integer;
begin
  for I:=0 to Count-1 do
    if foVersion in Fields[I].Options then
      Exit(Fields[I]);
  Result:=Nil;
end;

end.

