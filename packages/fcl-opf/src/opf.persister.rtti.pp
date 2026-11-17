{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Classic RTTI-based persistence

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Persister.RTTI;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, Data.Db,
{$ELSE}
  Classes, SysUtils, typinfo, db,
{$ENDIF}
  Opf.Types, Opf.Registry, Opf.Fields, Opf.Consts;

type

  { TRTTIPersister }

  TRTTIPersister = class(TOPPersister)
  protected
    // True if aClass implements IOPFNullable (Level 1)
    class function ClassHasNullableIntf(aClass: TClass): Boolean;
    // Set foNullable for Level 1 (aHasNullableIntf) or Level 0: a stored
    // specifier that is a Boolean field. A stored method (streaming style)
    // does not make a property nullable: set foNullable explicitly for that.
    procedure MarkNullable(F: TOPFieldMap; aPropInfo: PPropInfo; aHasNullableIntf: Boolean);
    procedure SetNullState(Instance: TObject; aFieldMap: TOPFieldMap; aIsNull: Boolean);
    procedure ApplyNullValue(Instance: TObject; aFieldMap: TOPFieldMap);
    procedure ApplyEnumValue(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
    procedure ApplyObjectValue(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
    procedure FillObject(aObject: TObject; aField: TField);
    procedure ClearObjectProperty(Instance: TObject; aFieldMap: TOPFieldMap);
    procedure ApplyFieldValue(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
  public
    // Fills the map with the class's published properties
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    // Copies a database field value (or null) into the object's property
    function ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean; override;
    // Copies the object's property value (or null) into a query parameter
    function PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean; override;
    // Raises a persister error for the given operation and reason
    procedure RaiseError(const aOperation, aReason: string);
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Variants,
{$ELSE}
  Variants,
{$ENDIF}
  Opf.Json, Opf.Variants, Opf.Nullable, Opf.FieldValue, Opf.Factory;

function IsSupportedTypeKind(aKind: TTypeKind): Boolean;
begin
  Result := aKind in [tkSString, tkAString, tkLString, tkWString, tkUString,
                      tkChar, tkWChar, tkUChar,
                      tkInteger, tkInt64, tkQWord, tkFloat, tkBool, tkEnumeration, tkClass,
                      tkSet, tkVariant];
end;

{ The property info of a mapped property (a manually added map may have none) }

function FieldPropInfo(Instance: TObject; aFieldMap: TOPFieldMap): PPropInfo;
begin
  Result := aFieldMap.PropInfo;
  if Result = nil then
    Result := GetPropInfo(Instance, aFieldMap.PropertyName, [tkFloat]);
  if Result = nil then
    raise EPropertyError.CreateFmt('Unknown property: "%s"', [aFieldMap.PropertyName]);
end;

{ TRTTIPersister }

class function TRTTIPersister.ClassHasNullableIntf(aClass: TClass): Boolean;
begin
  Result:=aClass.GetInterfaceEntryByStr(SGUIDIOPFNullable) <> nil;
end;

procedure TRTTIPersister.MarkNullable(F: TOPFieldMap; aPropInfo: PPropInfo; aHasNullableIntf: Boolean);
begin
  { Level 1: IOPFNullable - all properties are nullable.
    Level 0: the stored specifier, PropProcs bits 4-5:
      0 = field, 1 = static method, 2 = virtual method, 3 = constant.
    Only a field is a null flag; a method is the streaming "is not default"
    test, where false must not mean NULL. }
  if aHasNullableIntf or (((aPropInfo^.PropProcs shr 4) and 3) = 0) then
    F.Options := F.Options + [foNullable];
end;

procedure TRTTIPersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
var
  PropList: PPropList;
  PropCount, I: Integer;
  PropInfo: PPropInfo;
  F: TOPFieldMap;
  HasNullableIntf: Boolean;
begin
  { Classic RTTI - published properties via GetPropList }
  HasNullableIntf := ClassHasNullableIntf(aClass);
  PropCount := GetPropList(aClass, PropList);
  if PropCount > 0 then
  try
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if not IsSupportedTypeKind(PropInfo^.PropType^.Kind) then
      begin
        if aMap.SkipUnknownTypes then
          Continue
        else
          RaiseError('PopulateFieldMap',
            Format(SErrUnsupportedPropertyType, [PropInfo^.Name]));
      end;
      F := aMap.AddField(PropInfo^.Name, PropInfo^.Name);
      F.PropertyTypeInfo := PropInfo^.PropType;
      F.PropInfo := PropInfo;
      MarkNullable(F, PropInfo, HasNullableIntf);
    end;
  finally
    FreeMem(PropList);
  end;
end;

{ Record the null state of a nullable property:
  Level 1 via the IOPFNullable interface, Level 0 via a field-based stored specifier. }

procedure TRTTIPersister.SetNullState(Instance: TObject; aFieldMap: TOPFieldMap; aIsNull: Boolean);
begin
  if not (foNullable in aFieldMap.Options) then
    Exit;
  OPFSetPropNull(Instance, aFieldMap.PropertyName, aFieldMap.PropInfo, aIsNull);
end;

procedure TRTTIPersister.ApplyNullValue(Instance: TObject; aFieldMap: TOPFieldMap);
begin
  case aFieldMap.PropertyKind of
    tkSString, tkAString, tkLString:
      SetStrProp(Instance, aFieldMap.PropertyName, '');
    tkWString, tkUString:
      SetUnicodeStrProp(Instance, aFieldMap.PropertyName, '');
    tkInteger, tkBool, tkEnumeration, tkSet, tkChar, tkWChar, tkUChar:
      SetOrdProp(Instance, aFieldMap.PropertyName, 0);
    tkInt64, tkQWord:
      SetInt64Prop(Instance, aFieldMap.PropertyName, 0);
    tkFloat:
      SetFloatProp(Instance, aFieldMap.PropertyName, 0.0);
    tkVariant:
      SetVariantProp(Instance, aFieldMap.PropertyName, Null);
    tkClass:
      ClearObjectProperty(Instance, aFieldMap);
  else
    RaiseError('ApplyProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
  end;
end;

procedure TRTTIPersister.ApplyEnumValue(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
var
  VInt: Integer;
begin
  if foAsString in aFieldMap.Options then
  begin
    VInt := GetEnumValue(aFieldMap.PropertyTypeInfo, aField.AsString);
    if VInt < 0 then
      RaiseError('ApplyProperty', Format('Unknown enumeration value "%s" for property %s',
        [aField.AsString, aFieldMap.PropertyName]));
    SetOrdProp(Instance, aFieldMap.PropertyName, VInt);
  end
  else
    SetOrdProp(Instance, aFieldMap.PropertyName, aField.AsInteger);
end;

procedure TRTTIPersister.ApplyObjectValue(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
var
  Obj: TObject;
  ObjClass: TClass;
  Ctx: TOPFactoryContext;
begin
  Obj := GetObjectProp(Instance, aFieldMap.PropertyName);
  if Obj <> nil then
  begin
    FillObject(Obj, aField);
    Exit;
  end;
  // No object yet: create one through the factory, so constructors with
  // parameters (TCollection, TComponent owner) can be handled there
  ObjClass := GetObjectPropClass(Instance, aFieldMap.PropertyName);
  // TStrings is abstract
  if (ObjClass = TStrings) and not OPFactory.HasFactory(ObjClass) then
    ObjClass := TStringList;
  if ObjClass.InheritsFrom(TCollection) and not OPFactory.HasFactory(ObjClass) then
    RaiseError('ApplyProperty', Format(SErrCollectionNeedsFactory,
      [Instance.ClassName, aFieldMap.PropertyName, ObjClass.ClassName]));
  Ctx := TOPFactoryContext.Create(frProperty);
  try
    Ctx.OwnerObject := Instance;
    Ctx.PropertyName := aFieldMap.PropertyName;
    Obj := OPFactory.CreateInstance(ObjClass, Ctx);
  finally
    Ctx.Free;
  end;
  try
    FillObject(Obj, aField);
    SetObjectProp(Instance, aFieldMap.PropertyName, Obj);
  except
    Obj.Free;
    raise;
  end;
  // A setter that copies (FObj.Assign(Value)) leaves our object unused
  if GetObjectProp(Instance, aFieldMap.PropertyName) <> Obj then
    Obj.Free;
end;

{ NULL for a class property: empty a string list or collection, so the
  object matches the database. Other objects are left unchanged: their
  owner is not known, so they cannot be freed or replaced. }

procedure TRTTIPersister.ClearObjectProperty(Instance: TObject; aFieldMap: TOPFieldMap);
var
  Obj: TObject;
begin
  Obj := GetObjectProp(Instance, aFieldMap.PropertyName);
  if Obj is TStrings then
    TStrings(Obj).Clear
  else if Obj is TCollection then
    TCollection(Obj).Clear;
end;

procedure TRTTIPersister.FillObject(aObject: TObject; aField: TField);
begin
  if aObject is TStrings then
    TStrings(aObject).Text := aField.AsString
  else
    OPSerializer.Deserialize(aField.AsString, aObject);
end;

procedure TRTTIPersister.ApplyFieldValue(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField);
begin
  case aFieldMap.PropertyKind of
    tkSString, tkAString, tkLString:
      SetStrProp(Instance, aFieldMap.PropertyName, aField.AsString);
    tkWString, tkUString:
      SetUnicodeStrProp(Instance, aFieldMap.PropertyName, UTF8Decode(OPFGetUTF8Field(aField)));
    tkChar, tkWChar, tkUChar:
      SetOrdProp(Instance, aFieldMap.PropertyName, OPFTextToChar(aFieldMap.PropertyKind, aField));
    tkInteger:
      // AsLargeInt: unsigned 32-bit values do not fit in AsInteger
      SetOrdProp(Instance, aFieldMap.PropertyName, aField.AsLargeInt);
    tkInt64:
      SetInt64Prop(Instance, aFieldMap.PropertyName, aField.AsLargeInt);
    tkQWord:
      SetOrdProp(Instance, aFieldMap.PropertyName, OPFCheckQWord(aField.AsLargeInt));
    tkFloat:
      if OPFFloatKind(aFieldMap)=fkCurrency then
        // Exact, also where Extended is Double
        OPFSetCurrencyProp(Instance, FieldPropInfo(Instance, aFieldMap), aField.AsCurrency)
      else
        SetFloatProp(Instance, aFieldMap.PropertyName, OPFGetFloatFieldValue(aField, aFieldMap));
    tkBool:
      SetOrdProp(Instance, aFieldMap.PropertyName, Ord(aField.AsBoolean));
    tkEnumeration:
      ApplyEnumValue(Instance, aFieldMap, aField);
    tkSet:
      if foAsString in aFieldMap.Options then
        SetOrdProp(Instance, aFieldMap.PropertyName,
          StringToSet(aFieldMap.PropertyTypeInfo, aField.AsString))
      else
        SetOrdProp(Instance, aFieldMap.PropertyName, aField.AsInteger);
    tkVariant:
      SetVariantProp(Instance, aFieldMap.PropertyName,
        JSONToVariant(aField.AsString));
    tkClass:
      ApplyObjectValue(Instance, aFieldMap, aField);
  else
    RaiseError('ApplyProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
  end;
end;

function TRTTIPersister.ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  if aField.IsNull then
  begin
    // Null state after the value: a setter may mark a Level 0 property as stored
    ApplyNullValue(Instance, aFieldMap);
    SetNullState(Instance, aFieldMap, True);
  end
  else
  begin
    ApplyFieldValue(Instance, aFieldMap, aField);
    SetNullState(Instance, aFieldMap, False);
  end;
end;

function TRTTIPersister.PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
var
  Obj: TObject;
begin
  Result := True;
  { Level 1 (IOPFNullable, takes precedence) and Level 0 (dynamic stored specifier).
    The change trackers use the same function, so they see the same null state. }
  if OPFIsPropNull(Instance, aFieldMap.PropertyName, aFieldMap.PropInfo,
                   foNullable in aFieldMap.Options) then
  begin
    aParam.Clear;
    Exit;
  end;
  case aFieldMap.PropertyKind of
    tkSString, tkAString, tkLString:
      aParam.AsString := GetStrProp(Instance, aFieldMap.PropertyName);
    tkWString, tkUString:
      OPFSetUTF8Param(aParam, UTF8Encode(GetUnicodeStrProp(Instance, aFieldMap.PropertyName)));
    tkChar, tkWChar, tkUChar:
      OPFSetUTF8Param(aParam, OPFCharToText(aFieldMap.PropertyKind, GetOrdProp(Instance, aFieldMap.PropertyName)));
    tkInteger:
      if OPFIsUnsigned32(aFieldMap) then
        aParam.AsLargeInt := GetOrdProp(Instance, aFieldMap.PropertyName)
      else
        aParam.AsInteger := GetOrdProp(Instance, aFieldMap.PropertyName);
    tkInt64:
      aParam.AsLargeInt := GetInt64Prop(Instance, aFieldMap.PropertyName);
    tkQWord:
      aParam.AsLargeInt := OPFCheckQWord(GetOrdProp(Instance, aFieldMap.PropertyName));
    tkFloat:
      if OPFFloatKind(aFieldMap)=fkCurrency then
        OPFSetCurrencyParam(aParam, OPFGetCurrencyProp(Instance, FieldPropInfo(Instance, aFieldMap)))
      else
        OPFSetFloatParam(aParam, aFieldMap, GetFloatProp(Instance, aFieldMap.PropertyName));
    tkBool:
      aParam.AsBoolean := Boolean(GetOrdProp(Instance, aFieldMap.PropertyName));
    tkEnumeration:
      if foAsString in aFieldMap.Options then
        aParam.AsString := GetEnumName(aFieldMap.PropertyTypeInfo,
          GetOrdProp(Instance, aFieldMap.PropertyName))
      else
        aParam.AsInteger := GetOrdProp(Instance, aFieldMap.PropertyName);
    tkSet:
      if foAsString in aFieldMap.Options then
        aParam.AsString := SetToString(aFieldMap.PropertyTypeInfo,
          GetOrdProp(Instance, aFieldMap.PropertyName), True)
      else
        aParam.AsInteger := GetOrdProp(Instance, aFieldMap.PropertyName);
    tkVariant:
      aParam.AsString := VariantToJSON(
        GetVariantProp(Instance, aFieldMap.PropertyName));
    tkClass:
    begin
      Obj := GetObjectProp(Instance, aFieldMap.PropertyName);
      if Obj = nil then
      begin
        aParam.Clear;
        aParam.DataType := ftString;
      end
      else if Obj is TStrings then
        aParam.AsString := TStrings(Obj).Text
      else
        aParam.AsString := OPSerializer.Serialize(Obj);
    end;
  else
    RaiseError('PersistProperty', Format(SErrUnsupportedPropertyType, [aFieldMap.PropertyName]));
  end;
end;

procedure TRTTIPersister.RaiseError(const aOperation, aReason: string);
begin
  OPFRaiseError(EFieldMap, ClassName, aOperation, aReason);
end;

end.
