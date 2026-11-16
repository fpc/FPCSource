{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Memento built from the field map of a persister using extended RTTI (3.3.1 and higher)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Memento.ExtRTTI;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, System.Variants, System.Rtti,
{$ELSE}
  Classes, SysUtils, typinfo, Variants, rtti,
{$ENDIF}
  Opf.Fields, Opf.ChangeTracking, Opf.Memento;

type

  { TOPRttiValue:
    Contains one captured property value and what is needed to read the
    property again. Member is Nil for:
    - a serialized class property
    - a property that could not be resolved or read.
    }

  TOPRttiValue = record
    Name: UTF8String;
    TypeInfo: PTypeInfo;
    Member: TRttiDataMember;
    Serialized: Boolean;
    Compared: Boolean;
    Value: TValue;
    // Level 0/1 nullable property: null state captured beside the value
    Nullable: Boolean;
    PropInfo: PPropInfo;
    IsNull: Boolean;
  end;
  TOPRttiValueArray = array of TOPRttiValue;

  { TOPRttiMemento:
    A memento that builds itself from a field map,
    so a class without a hand-written memento can still be tracked.
    Every property the map sets as foInUpdate or foInKey is captured as a TValue;
    a class-valued property is captured as serialized text.
    Only foInUpdate properties are compared.
    Restore writes every captured entry back, the foInKey ones included;
    a class-valued property is written into instead of replaced.
    An entry that cannot be written is skipped. }

  TOPRttiMemento = class(TOPMemento)
  private
    FSubjectClass: TClass;
    FFieldMap: TOPFieldMapList;
    FValues: TOPRttiValueArray;
    function FindProperty(const aName: UTF8String): TRttiProperty;
    procedure ResolveMember(aField: TOPFieldMap; var aValue: TOPRttiValue);
    function CurrentObject(aSubject: TObject; const aValue: TOPRttiValue): TObject;
    function ReadCurrent(aSubject: TObject; const aValue: TOPRttiValue): TValue;
    procedure RestoreValue(aSubject: TObject; const aValue: TOPRttiValue);
    procedure RestorePlainValue(aSubject: TObject; const aValue: TOPRttiValue);
    function SameRecordValue(const aLeft, aRight: TValue): Boolean;
    function NullableFields(aType: TRttiType; out aHasValue, aValue: TRttiField): Boolean;
    function SameNullableValue(aHasValue, aValue: TRttiField; aLeft, aRight: Pointer): Boolean;
  protected
    // The class of the subject this memento was built from.
    function GetSubjectClass: TClass; override;
    // Copy every mapped property of aSubject into this memento.
    procedure Capture(aSubject: TObject); override;
    // Write the captured values back into aSubject, skipping what cannot be written.
    procedure DoRestore(aSubject: TObject); override;
    // Add the name of every compared property of aSubject that differs to aNames.
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); override;
    // True when both values are considered equal for change detection.
    function SameValue(const aLeft, aRight: TValue): Boolean; virtual;
  public
    // Capture the properties aFieldMap marks foInUpdate or foInKey from aSubject.
    constructor CreateFromFieldMap(aSubject: TObject; aFieldMap: TOPFieldMapList);
  end;

implementation

uses
  Opf.Consts, Opf.Json, Opf.FieldValue;

var
  { Unit-level RTTI contexts:
    keep the RTTI pool alive so that the members stored beside a captured value remain valid.
    There are two:
    - a published property lives in the pool of a context created with True,
    - a property described by extended RTTI only in the pool of one created with False.
    }
  MementoRttiCtx: array[Boolean] of TRttiContext;
  MementoRttiCtxCreated: Boolean = False;

function GetMementoRttiCtx(aPublishedOnly: Boolean): TRttiContext;

begin
  // Created in the initialization section: no race between threads
  Result:=MementoRttiCtx[aPublishedOnly];
end;


{ - Two empty values are equal,
  - two nulls are equal,
  - an empty or a null against a value is not equal. }

function SameVariantValue(const aLeft, aRight: Variant): Boolean;

begin
  // Also: a different variant type is a change, and comparing never raises
  Result:=OPFSameVariant(aLeft,aRight);
end;


{ TOPRttiMemento }

constructor TOPRttiMemento.CreateFromFieldMap(aSubject: TObject; aFieldMap: TOPFieldMapList);

begin
  if aSubject=Nil then
    RaiseError('CreateFromFieldMap',SErrMementoNilSubject);
  FSubjectClass:=aSubject.ClassType;
  FFieldMap:=aFieldMap;
  try
    inherited Create(aSubject);
  finally
    FFieldMap:=Nil;
  end;
end;


function TOPRttiMemento.GetSubjectClass: TClass;

begin
  Result:=FSubjectClass;
end;


function TOPRttiMemento.FindProperty(const aName: UTF8String): TRttiProperty;

var
  lType: TRttiType;

begin
  Result:=Nil;
  lType:=GetMementoRttiCtx(True).GetType(FSubjectClass);
  if Assigned(lType) then
    Result:=lType.GetProperty(aName);
  if Assigned(Result) then
    Exit;
  lType:=GetMementoRttiCtx(False).GetType(FSubjectClass);
  if Assigned(lType) then
    Result:=lType.GetProperty(aName);
end;


{ - A published class property is read by name,
  - A property described by extended RTTI only through the member kept beside it,
    and a name the subject does not have also marks the entry unreadable.
  - A field map entry holding a TRttiField instead of a property info is a nullable record field,
    and is read through that field. }

procedure TOPRttiMemento.ResolveMember(aField: TOPFieldMap; var aValue: TOPRttiValue);

begin
  aValue.Member:=Nil;
  aValue.Serialized:=False;
  if aField.PropertyKind=tkClass then
    begin
    if GetPropInfo(FSubjectClass,aField.PropertyName)<>Nil then
      aValue.Serialized:=True
    else
      begin
      aValue.Member:=FindProperty(aField.PropertyName);
      aValue.Serialized:=Assigned(aValue.Member) and aValue.Member.IsReadable;
      if not aValue.Serialized then
        aValue.Member:=Nil;
      end;
    Exit;
    end;
  if (aField.MemberKind<>omkProperty) and Assigned(aField.RttiMember) then
    aValue.Member:=TRttiField(aField.RttiMember)
  else
    aValue.Member:=FindProperty(aField.PropertyName);
  if Assigned(aValue.Member) and not aValue.Member.IsReadable then
    aValue.Member:=Nil;
end;


{ The child object aSubject holds for a serialized entry right now:
  through the member for a property described by extended RTTI only,
  by name otherwise. }

function TOPRttiMemento.CurrentObject(aSubject: TObject; const aValue: TOPRttiValue): TObject;

begin
  if Assigned(aValue.Member) then
    Result:=aValue.Member.GetValue(Pointer(aSubject)).AsObject
  else
    Result:=GetObjectProp(aSubject,aValue.Name);
end;


{ An empty result means the value could not be read,
  or the class property is Nil.
  NOte that nil is not the same as empty text. }

function TOPRttiMemento.ReadCurrent(aSubject: TObject; const aValue: TOPRttiValue): TValue;

var
  lObject: TObject;

begin
  Result:=TValue.Empty;
  if aValue.Serialized then
    begin
    lObject:=CurrentObject(aSubject,aValue);
    if not Assigned(lObject) then
      Exit;
    if lObject is TStrings then
      Result:=TValue.specialize From<UTF8String>(TStrings(lObject).Text)
    else
      Result:=TValue.specialize From<UTF8String>(OPSerializer.Serialize(lObject));
    end
  else if Assigned(aValue.Member) then
    Result:=aValue.Member.GetValue(Pointer(aSubject));
end;


procedure TOPRttiMemento.Capture(aSubject: TObject);

var
  lIndex: Integer;
  lCount: Integer;
  lField: TOPFieldMap;

begin
  SetLength(FValues,0);
  if not Assigned(FFieldMap) then
    Exit;
  SetLength(FValues,FFieldMap.Count);
  lCount:=0;
  for lIndex:=0 to FFieldMap.Count-1 do
    begin
    lField:=FFieldMap[lIndex];
    if ([foInUpdate,foInKey]*lField.Options)=[] then
      Continue;
    FValues[lCount].Name:=lField.PropertyName;
    FValues[lCount].TypeInfo:=lField.PropertyTypeInfo;
    FValues[lCount].Compared:=foInUpdate in lField.Options;
    // Level 2 nullables are records and compare their own null state
    FValues[lCount].Nullable:=(foNullable in lField.Options) and not OPFIsNullableMember(lField);
    FValues[lCount].PropInfo:=lField.PropInfo;
    FValues[lCount].IsNull:=FValues[lCount].Nullable
      and OPFIsPropNull(aSubject,lField.PropertyName,lField.PropInfo,True);
    ResolveMember(lField,FValues[lCount]);
    FValues[lCount].Value:=ReadCurrent(aSubject,FValues[lCount]);
    if not Assigned(FValues[lCount].TypeInfo) then
      FValues[lCount].TypeInfo:=FValues[lCount].Value.TypeInfo;
    Inc(lCount);
    end;
  SetLength(FValues,lCount);
end;


{ Write one captured value back:
  Write into the child object aSubject holds now for a class-valued property,
  Write over the released old value for a record field. }

procedure TOPRttiMemento.RestoreValue(aSubject: TObject; const aValue: TOPRttiValue);

begin
  try
    RestorePlainValue(aSubject,aValue);
  finally
    // After the value: a setter may change the stored flag of a Level 0 property
    if aValue.Nullable then
      OPFSetPropNull(aSubject,aValue.Name,aValue.PropInfo,aValue.IsNull);
  end;
end;


procedure TOPRttiMemento.RestorePlainValue(aSubject: TObject; const aValue: TOPRttiValue);

var
  lObject: TObject;
  lField: TRttiField;

begin
  if aValue.Value.IsEmpty then
    Exit;
  if aValue.Serialized then
    begin
    lObject:=CurrentObject(aSubject,aValue);
    if not Assigned(lObject) then
      Exit;
    if lObject is TStrings then
      TStrings(lObject).Text:=aValue.Value.AsString
    else
      OPSerializer.Deserialize(aValue.Value.AsString,lObject);
    end
  else if Assigned(aValue.Member) and aValue.Member.IsWritable then
    begin
    lField:=Nil;
    if aValue.Member is TRttiField then
      lField:=TRttiField(aValue.Member);
    if Assigned(lField) and (aValue.Value.TypeInfo=lField.FieldType.Handle) then
      FinalizeArray(PByte(aSubject)+lField.Offset,aValue.Value.TypeInfo,1);
    if (aValue.Value.Kind=tkVariant) and (aValue.Member is TRttiProperty) then
      SetVariantProp(aSubject,PPropInfo(aValue.Member.Handle),aValue.Value.AsVariant)
    else
      aValue.Member.SetValue(Pointer(aSubject),aValue.Value);
    end;
end;


{ An entry that cannot be written is skipped. }

procedure TOPRttiMemento.DoRestore(aSubject: TObject);

var
  lIndex: Integer;

begin
  for lIndex:=0 to Length(FValues)-1 do
    RestoreValue(aSubject,FValues[lIndex]);
end;


{ A value that could not be read at capture time is considered changed. }

procedure TOPRttiMemento.DoCompare(aSubject: TObject; var aNames: TOPChangeNames);

var
  lIndex: Integer;
  lIsNull: Boolean;

begin
  for lIndex:=0 to Length(FValues)-1 do
    begin
    if not FValues[lIndex].Compared then
      Continue;
    if not (FValues[lIndex].Serialized or Assigned(FValues[lIndex].Member)) then
      aNames.Add(FValues[lIndex].Name)
    else if FValues[lIndex].Nullable then
      begin
      // A change of the null state is a change; two nulls are equal
      // whatever value the property holds.
      lIsNull:=OPFIsPropNull(aSubject,FValues[lIndex].Name,FValues[lIndex].PropInfo,True);
      if lIsNull<>FValues[lIndex].IsNull then
        aNames.Add(FValues[lIndex].Name)
      else if not lIsNull then
        aNames.AddIf(not SameValue(FValues[lIndex].Value,ReadCurrent(aSubject,FValues[lIndex])),
                     FValues[lIndex].Name);
      end
    else
      aNames.AddIf(not SameValue(FValues[lIndex].Value,ReadCurrent(aSubject,FValues[lIndex])),
                   FValues[lIndex].Name);
    end;
end;


{ A record is treated as a nullable only when it contains the two fields checked in the nullable type,
  so a record that merely shares the name is walked field by field. }

function TOPRttiMemento.NullableFields(aType: TRttiType; out aHasValue, aValue: TRttiField): Boolean;

begin
  aHasValue:=Nil;
  aValue:=Nil;
  Result:=Pos('TNullable$',aType.Name)=1;
  if not Result then
    Exit;
  aHasValue:=aType.GetField('FHasValue');
  aValue:=aType.GetField('FValue');
  Result:=Assigned(aHasValue) and Assigned(aValue)
          and Assigned(aHasValue.FieldType)
          and (aHasValue.FieldType.TypeKind=tkBool);
end;


{ Two nullables without a value are equal; the payload is compared only when
  both sides have one. }

function TOPRttiMemento.SameNullableValue(aHasValue, aValue: TRttiField; aLeft, aRight: Pointer): Boolean;

var
  lLeftHas: Boolean;

begin
  Result:=False;
  lLeftHas:=aHasValue.GetValue(aLeft).AsBoolean;
  if lLeftHas<>aHasValue.GetValue(aRight).AsBoolean then
    Exit;
  Result:=not lLeftHas;
  if not Result then
    Result:=SameValue(aValue.GetValue(aLeft),aValue.GetValue(aRight));
end;


{ A record without readable fields cannot be compared and counts as changed. }

function TOPRttiMemento.SameRecordValue(const aLeft, aRight: TValue): Boolean;

var
  lType: TRttiType;
  lFields: TRttiFieldArray;
  lHasValue: TRttiField;
  lValue: TRttiField;
  lIndex: Integer;
  lLeft: Pointer;
  lRight: Pointer;

begin
  Result:=False;
  lType:=GetMementoRttiCtx(False).GetType(aLeft.TypeInfo);
  if not Assigned(lType) then
    Exit;
  lLeft:=aLeft.GetReferenceToRawData;
  lRight:=aRight.GetReferenceToRawData;
  if NullableFields(lType,lHasValue,lValue) then
    Exit(SameNullableValue(lHasValue,lValue,lLeft,lRight));
  lFields:=lType.GetFields;
  if Length(lFields)=0 then
    Exit;
  for lIndex:=0 to Length(lFields)-1 do
    if not SameValue(lFields[lIndex].GetValue(lLeft),lFields[lIndex].GetValue(lRight)) then
      Exit;
  Result:=True;
end;


{ A type kind that is not handled counts as changed. }

function TOPRttiMemento.SameValue(const aLeft, aRight: TValue): Boolean;

begin
  if aLeft.IsEmpty or aRight.IsEmpty then
    Exit(aLeft.IsEmpty and aRight.IsEmpty);
  if aLeft.TypeInfo<>aRight.TypeInfo then
    Exit(False);
  case aLeft.Kind of
    tkInteger,tkInt64,tkQWord,tkBool,tkEnumeration,tkChar,tkWChar,tkUChar:
      Result:=aLeft.AsOrdinal=aRight.AsOrdinal;
    tkSString,tkLString,tkAString:
      Result:=aLeft.AsString=aRight.AsString;
    // AsString converts to the system code page, which can lose characters
    tkUString,tkWString:
      Result:=aLeft.AsUnicodeString=aRight.AsUnicodeString;
    tkFloat:
      // Currency exactly, also where Extended is Double
      if GetTypeData(aLeft.TypeInfo)^.FloatType=TFloatType.ftCurr then
        Result:=aLeft.AsCurrency=aRight.AsCurrency
      else
        Result:=aLeft.AsExtended=aRight.AsExtended;
    tkSet:
      Result:=(aLeft.DataSize=aRight.DataSize)
              and CompareMem(aLeft.GetReferenceToRawData,aRight.GetReferenceToRawData,aLeft.DataSize);
    tkVariant:
      Result:=SameVariantValue(aLeft.AsVariant,aRight.AsVariant);
    tkRecord:
      Result:=SameRecordValue(aLeft,aRight);
  else
    Result:=False;
  end;
end;


function OPRttiMementoFactory(aSubject: TObject; aFieldMap: TOPFieldMapList): TOPMemento;

begin
  Result:=TOPRttiMemento.CreateFromFieldMap(aSubject,aFieldMap);
end;


initialization
  OPDefaultMementoFactory:=@OPRttiMementoFactory;
  MementoRttiCtx[False]:=TRttiContext.Create(False);
  MementoRttiCtx[True]:=TRttiContext.Create(True);
  MementoRttiCtxCreated:=True;

finalization
  MementoRttiCtx[False].Free;
  MementoRttiCtx[True].Free;

end.
