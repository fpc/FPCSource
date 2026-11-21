{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Class mappings saved to and loaded from JSON

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Mapping.Json;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data,
{$ELSE}
  Classes, SysUtils, fpjson,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Relations, Opf.M2M;

type
  EOPFMapping = class(EOPF);

  { TOPJSONMapping }

  TOPJSONMapping = class(TComponent, IOPRegistrationListener)
  private
    FRegistry: TOPRegistry;
    FM2MRegistry: TOPManyToManyRegistry;
    FHookedRegistry: TOPRegistry;
    FDocument: TJSONObject;
    FLoading: Boolean;
    function GetRegistry: TOPRegistry;
    function GetM2MRegistry: TOPManyToManyRegistry;
    procedure SetRegistry(aValue: TOPRegistry);
    procedure SetM2MRegistry(aValue: TOPManyToManyRegistry);
    procedure Hook;
    procedure CheckKeys(aObject: TJSONObject; const aAllowed: array of String; const aContext: String);
    function ObjectValue(aData: TJSONData; const aKey, aContext: String): TJSONObject;
    function GetObject(aParent: TJSONObject; const aKey, aContext: String): TJSONObject;
    function GetArray(aParent: TJSONObject; const aKey, aContext: String): TJSONArray;
    function GetString(aParent: TJSONObject; const aKey, aContext: String; out aValue: UTF8String): Boolean;
    function GetRequiredString(aParent: TJSONObject; const aKey, aContext: String): UTF8String;
    function GetInteger(aParent: TJSONObject; const aKey, aContext: String; out aValue: Integer): Boolean;
    function GetBoolean(aParent: TJSONObject; const aKey, aContext: String; out aValue: Boolean): Boolean;
    function ClassesObject: TJSONObject;
    function FindEntry(aClass: TClass): TJSONObject;
    function FindRegisteredClass(const aName: UTF8String): TClass;
    function FindStreamableClass(const aName: UTF8String): TClass;
    function ClassKey(aClass: TClass): UTF8String;
    function ParseOptions(aArray: TJSONArray; const aContext: String): TOPFieldOptions;
    function FindMappedField(aDef: TPersisterDef; const aName: UTF8String): TOPFieldMap;
    procedure ApplyFields(aDef: TPersisterDef; aFields: TJSONObject);
    procedure ApplyIndexes(aDef: TPersisterDef; aIndexes: TJSONArray);
    procedure ApplyClassEntry(aDef: TPersisterDef; aEntry: TJSONObject; aRegistered: Boolean);
    procedure ApplyClasses(aPersisterClass: TOPPersisterClass);
    procedure ApplyRelation(aParent, aChild: TClass; aRelation: TJSONObject; const aContext: String; aCreated: TFPList);
    procedure ApplyManyToMany(aClassA, aClassB: TClass; aLink: TJSONObject; const aContext: String; aCreated: TFPList);
    procedure ApplyRelations(aClass: TClass; aStrict: Boolean; aCreated: TFPList);
    function DefToJSON(aDef: TPersisterDef): TJSONObject;
    function FieldToJSON(aField: TOPFieldMap): TJSONObject;
  protected
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
  public
    // Removes the mapping from the registry and frees it.
    destructor Destroy; override;
    // Loads a mapping, applies it to registered classes and to classes registered later.
    // With aPersisterClass, unregistered classes of the mapping are registered with it.
    procedure LoadFromStream(aStream: TStream; aPersisterClass: TOPPersisterClass = Nil);
    // Loads a mapping from a file, see LoadFromStream.
    procedure LoadFromFile(const aFileName: String; aPersisterClass: TOPPersisterClass = Nil);
    // Writes the mapping of all registered classes and their relations.
    procedure SaveToStream(aStream: TStream);
    // Writes the mapping of all registered classes to a file.
    procedure SaveToFile(const aFileName: String);
    // Stops applying the mapping to classes registered later.
    procedure Unhook;
    // Applies the class settings and fields of the mapping to a new definition.
    procedure ClassRegistering(aDef: TPersisterDef);
    // Applies the relations of the mapping that involve the class of a new definition.
    procedure ClassRegistered(aDef: TPersisterDef);
    // Raises a mapping error with a message built from a format string and arguments.
    procedure RaiseError(const aOperation, aFmt: string; const aArgs: array of const);
    // The registry the mapping is saved from and applied to; the global registry if not set.
    property Registry: TOPRegistry read GetRegistry write SetRegistry;
    // The registry for many-to-many links; the global one when Registry is not set.
    property M2MRegistry: TOPManyToManyRegistry read GetM2MRegistry write SetM2MRegistry;
  end;

// Loads a mapping into the global registry, replacing a mapping loaded before.
procedure LoadMapping(aStream: TStream; aPersisterClass: TOPPersisterClass = Nil); overload;
// Loads a mapping file into the global registry, replacing a mapping loaded before.
procedure LoadMapping(const aFileName: String; aPersisterClass: TOPPersisterClass = Nil); overload;
// Writes the mapping of the global registry.
procedure SaveMapping(aStream: TStream); overload;
// Writes the mapping of the global registry to a file.
procedure SaveMapping(const aFileName: String); overload;
// Frees the mapping loaded with LoadMapping.
procedure ClearMapping;
// Writes the default mapping of aClasses, registered with aPersisterClass in a private registry.
// A class without a primary key gets foInKey on its first field.
procedure SaveDefaultMapping(aStream: TStream; const aClasses: array of TClass; aPersisterClass: TOPPersisterClass); overload;
// Writes the default mapping of aClasses to a file, see SaveDefaultMapping.
procedure SaveDefaultMapping(const aFileName: String; const aClasses: array of TClass; aPersisterClass: TOPPersisterClass); overload;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo, System.Variants, FpJson.Parser;
{$ELSE}
  typinfo, variants, jsonparser;
{$ENDIF}

resourcestring
  SErrMappingInvalidJSON = 'Invalid JSON in mapping: %s';
  SErrMappingNotObject = 'A mapping must be a JSON object';
  SErrMappingVersion = 'Unsupported mapping version: %d';
  SErrMappingKeyType = 'The value of "%s" in %s must be a JSON %s';
  SErrMappingUnknownKey = 'Unknown key "%s" in %s';
  SErrMappingMissingKey = 'Missing key "%s" in %s';
  SErrMappingClassNotFound = 'Class "%s" of the mapping is not registered with RegisterClass';
  SErrMappingAmbiguousClass = 'Class name "%s" of the mapping matches more than one registered class: use UnitName.ClassName';
  SErrMappingUnknownProperty = 'Class "%s" has no mapped property "%s"';
  SErrMappingUnknownOption = 'Unknown field option "%s" in %s';
  SErrMappingUnknownField = 'Unknown field "%s" in %s';
  SErrMappingParentNotFound = 'Parent class "%s" of a relation of class "%s" is not registered';
  SErrMappingNoM2MRegistry = 'The mapping has many-to-many links, but no many-to-many registry is set';
  SErrMappingM2MClass = 'Class "%s" of a many-to-many link is not registered';

const
  MappingVersion = 1;
  STypeObject = 'object';
  STypeArray = 'array';
  STypeString = 'string';
  STypeInteger = 'integer';
  STypeBoolean = 'boolean';

type
  { Puts foInKey on the first field of a definition without a primary key }
  TOPDefaultKeyListener = class(TObject, IOPRegistrationListener)
  public
    // Marks the first field as key when no key is found.
    procedure ClassRegistering(aDef: TPersisterDef);
    // Does nothing.
    procedure ClassRegistered(aDef: TPersisterDef);
  end;

var
  _Mapping: TOPJSONMapping = nil;

{ True if aName is the class name of aClass, optionally prefixed with its unit name }

function NameMatches(const aName: UTF8String; aClass: TClass): Boolean;

begin
  Result:=(CompareText(aName, aClass.ClassName)=0)
          or (CompareText(aName, aClass.UnitName+'.'+aClass.ClassName)=0);
end;


{ The default value of a field from a JSON value }

function JSONToDefault(aData: TJSONData): Variant;

begin
  case aData.JSONType of
    jtNull:
      Result:=Null;
    jtBoolean:
      Result:=aData.AsBoolean;
    jtNumber:
      if TJSONNumber(aData).NumberType=ntFloat then
        Result:=aData.AsFloat
      else
        Result:=aData.AsInt64;
  else
    Result:=UTF8String(aData.AsString);
  end;
end;


{ The JSON value of the default value of a field }

function DefaultToJSON(const aValue: Variant): TJSONData;

begin
  if VarIsNull(aValue) or VarIsEmpty(aValue) then
    Exit(TJSONNull.Create);
  case VarType(aValue) and varTypeMask of
    varBoolean:
      Result:=TJSONBoolean.Create(Boolean(aValue));
    varShortInt, varSmallint, varInteger, varByte, varWord, varLongWord, varInt64, varQWord:
      Result:=TJSONInt64Number.Create(Int64(aValue));
    varSingle, varDouble, varCurrency, varDate:
      Result:=TJSONFloatNumber.Create(Double(aValue));
  else
    Result:=TJSONString.Create(UTF8String(VarToStr(aValue)));
  end;
end;


{ TOPJSONMapping }

function TOPJSONMapping.GetRegistry: TOPRegistry;

begin
  Result:=FRegistry;
  if Result=Nil then
    Result:=TOPRegistry.Instance;
end;


function TOPJSONMapping.GetM2MRegistry: TOPManyToManyRegistry;

begin
  Result:=FM2MRegistry;
  if (Result=Nil) and (FRegistry=Nil) then
    Result:=OPM2MRegistry;
end;


procedure TOPJSONMapping.SetRegistry(aValue: TOPRegistry);

begin
  if FRegistry=aValue then
    Exit;
  Unhook;
  if Assigned(FRegistry) then
    FRegistry.RemoveFreeNotification(Self);
  FRegistry:=aValue;
  if Assigned(FRegistry) then
    FRegistry.FreeNotification(Self);
end;


procedure TOPJSONMapping.SetM2MRegistry(aValue: TOPManyToManyRegistry);

begin
  if FM2MRegistry=aValue then
    Exit;
  if Assigned(FM2MRegistry) then
    FM2MRegistry.RemoveFreeNotification(Self);
  FM2MRegistry:=aValue;
  if Assigned(FM2MRegistry) then
    FM2MRegistry.FreeNotification(Self);
end;


procedure TOPJSONMapping.Notification(aComponent: TComponent; aOperation: TOperation);

begin
  inherited Notification(aComponent, aOperation);
  if aOperation<>opRemove then
    Exit;
  if aComponent=FHookedRegistry then
    FHookedRegistry:=Nil;
  if aComponent=FRegistry then
    FRegistry:=Nil;
  if aComponent=FM2MRegistry then
    FM2MRegistry:=Nil;
end;


destructor TOPJSONMapping.Destroy;

begin
  Unhook;
  FreeAndNil(FDocument);
  inherited Destroy;
end;


procedure TOPJSONMapping.Hook;

begin
  Unhook;
  FHookedRegistry:=Registry;
  FHookedRegistry.RegistrationListener:=Self;
  FHookedRegistry.FreeNotification(Self);
end;


procedure TOPJSONMapping.Unhook;

var
  lMe: IOPRegistrationListener;

begin
  if FHookedRegistry=Nil then
    Exit;
  lMe:=Self;
  if FHookedRegistry.RegistrationListener=lMe then
    FHookedRegistry.RegistrationListener:=Nil;
  if FHookedRegistry<>FRegistry then
    FHookedRegistry.RemoveFreeNotification(Self);
  FHookedRegistry:=Nil;
end;


procedure TOPJSONMapping.RaiseError(const aOperation, aFmt: string; const aArgs: array of const);

begin
  OPFRaiseError(EOPFMapping, ClassName, aOperation, aFmt, aArgs);
end;


procedure TOPJSONMapping.CheckKeys(aObject: TJSONObject; const aAllowed: array of String; const aContext: String);

var
  I, J: Integer;
  lFound: Boolean;

begin
  for I:=0 to aObject.Count-1 do
  begin
    lFound:=False;
    for J:=0 to High(aAllowed) do
      if aObject.Names[I]=aAllowed[J] then
        lFound:=True;
    if not lFound then
      RaiseError('Load', SErrMappingUnknownKey, [aObject.Names[I], aContext]);
  end;
end;


function TOPJSONMapping.ObjectValue(aData: TJSONData; const aKey, aContext: String): TJSONObject;

begin
  if not (aData is TJSONObject) then
    RaiseError('Load', SErrMappingKeyType, [aKey, aContext, STypeObject]);
  Result:=TJSONObject(aData);
end;


function TOPJSONMapping.GetObject(aParent: TJSONObject; const aKey, aContext: String): TJSONObject;

var
  lData: TJSONData;

begin
  lData:=aParent.Find(aKey);
  if lData=Nil then
    Result:=Nil
  else
    Result:=ObjectValue(lData, aKey, aContext);
end;


function TOPJSONMapping.GetArray(aParent: TJSONObject; const aKey, aContext: String): TJSONArray;

var
  lData: TJSONData;

begin
  lData:=aParent.Find(aKey);
  if lData=Nil then
    Exit(Nil);
  if not (lData is TJSONArray) then
    RaiseError('Load', SErrMappingKeyType, [aKey, aContext, STypeArray]);
  Result:=TJSONArray(lData);
end;


function TOPJSONMapping.GetString(aParent: TJSONObject; const aKey, aContext: String; out aValue: UTF8String): Boolean;

var
  lData: TJSONData;

begin
  aValue:='';
  lData:=aParent.Find(aKey);
  Result:=lData<>Nil;
  if not Result then
    Exit;
  if lData.JSONType<>jtString then
    RaiseError('Load', SErrMappingKeyType, [aKey, aContext, STypeString]);
  aValue:=lData.AsString;
end;


function TOPJSONMapping.GetRequiredString(aParent: TJSONObject; const aKey, aContext: String): UTF8String;

begin
  if not GetString(aParent, aKey, aContext, Result) then
    RaiseError('Load', SErrMappingMissingKey, [aKey, aContext]);
end;


function TOPJSONMapping.GetInteger(aParent: TJSONObject; const aKey, aContext: String; out aValue: Integer): Boolean;

var
  lData: TJSONData;

begin
  aValue:=0;
  lData:=aParent.Find(aKey);
  Result:=lData<>Nil;
  if not Result then
    Exit;
  if (lData.JSONType<>jtNumber) or (TJSONNumber(lData).NumberType=ntFloat) then
    RaiseError('Load', SErrMappingKeyType, [aKey, aContext, STypeInteger]);
  aValue:=lData.AsInteger;
end;


function TOPJSONMapping.GetBoolean(aParent: TJSONObject; const aKey, aContext: String; out aValue: Boolean): Boolean;

var
  lData: TJSONData;

begin
  aValue:=False;
  lData:=aParent.Find(aKey);
  Result:=lData<>Nil;
  if not Result then
    Exit;
  if lData.JSONType<>jtBoolean then
    RaiseError('Load', SErrMappingKeyType, [aKey, aContext, STypeBoolean]);
  aValue:=lData.AsBoolean;
end;


function TOPJSONMapping.ClassesObject: TJSONObject;

begin
  if FDocument=Nil then
    Result:=Nil
  else
    Result:=GetObject(FDocument, 'classes', 'the mapping');
end;


function TOPJSONMapping.FindEntry(aClass: TClass): TJSONObject;

var
  lClasses: TJSONObject;
  I: Integer;
  lPlain: TJSONData;

begin
  Result:=Nil;
  lClasses:=ClassesObject;
  if lClasses=Nil then
    Exit;
  lPlain:=Nil;
  for I:=0 to lClasses.Count-1 do
    if CompareText(lClasses.Names[I], aClass.UnitName+'.'+aClass.ClassName)=0 then
      Exit(ObjectValue(lClasses.Items[I], lClasses.Names[I], 'classes'))
    else if CompareText(lClasses.Names[I], aClass.ClassName)=0 then
      lPlain:=lClasses.Items[I];
  if lPlain<>Nil then
    Result:=ObjectValue(lPlain, aClass.ClassName, 'classes');
end;


function TOPJSONMapping.FindRegisteredClass(const aName: UTF8String): TClass;

var
  I: Integer;
  lClass: TClass;

begin
  Result:=Nil;
  for I:=0 to Registry.PersisterCount-1 do
  begin
    lClass:=Registry[I].InstanceClass;
    if not NameMatches(aName, lClass) then
      Continue;
    // An auto-created definition and its replacement have the same class
    if (Result<>Nil) and (Result<>lClass) then
      RaiseError('Load', SErrMappingAmbiguousClass, [aName]);
    Result:=lClass;
  end;
end;


function TOPJSONMapping.FindStreamableClass(const aName: UTF8String): TClass;

var
  lPos: Integer;

begin
  lPos:=LastDelimiter('.', aName);
  Result:=GetClass(Copy(aName, lPos+1, Length(aName)-lPos));
  if (Result<>Nil) and (lPos>0) and (CompareText(Copy(aName, 1, lPos-1), Result.UnitName)<>0) then
    Result:=Nil;
  if Result=Nil then
    RaiseError('Load', SErrMappingClassNotFound, [aName]);
end;


function TOPJSONMapping.ClassKey(aClass: TClass): UTF8String;

var
  I: Integer;
  lOther: TClass;

begin
  Result:=aClass.ClassName;
  for I:=0 to Registry.PersisterCount-1 do
  begin
    lOther:=Registry[I].InstanceClass;
    if (lOther<>aClass) and (CompareText(lOther.ClassName, aClass.ClassName)=0) then
      Exit(aClass.UnitName+'.'+aClass.ClassName);
  end;
end;


function TOPJSONMapping.ParseOptions(aArray: TJSONArray; const aContext: String): TOPFieldOptions;

var
  I, lValue: Integer;

begin
  Result:=[];
  for I:=0 to aArray.Count-1 do
  begin
    if aArray.Items[I].JSONType<>jtString then
      RaiseError('Load', SErrMappingKeyType, ['options', aContext, STypeString]);
    lValue:=GetEnumValue(TypeInfo(TOPFieldOption), aArray.Items[I].AsString);
    if lValue<0 then
      RaiseError('Load', SErrMappingUnknownOption, [aArray.Items[I].AsString, aContext]);
    Include(Result, TOPFieldOption(lValue));
  end;
end;


function TOPJSONMapping.FindMappedField(aDef: TPersisterDef; const aName: UTF8String): TOPFieldMap;

begin
  Result:=aDef.FieldMap.FindByPropertyName(aName);
  if Result=Nil then
    Result:=aDef.FieldMap.FindByFieldName(aName);
end;


procedure TOPJSONMapping.ApplyFields(aDef: TPersisterDef; aFields: TJSONObject);

var
  I: Integer;
  lName, lStr: UTF8String;
  lContext: String;
  lEntry: TJSONObject;
  lField: TOPFieldMap;
  lArray: TJSONArray;
  lInt: Integer;
  lSkip, lKeyDeclared: Boolean;
  lDefault: TJSONData;

begin
  lKeyDeclared:=False;
  for I:=0 to aFields.Count-1 do
  begin
    lName:=aFields.Names[I];
    lContext:=Format('field "%s" of class "%s"', [lName, aDef.InstanceClass.ClassName]);
    lEntry:=ObjectValue(aFields.Items[I], lName, 'fields');
    CheckKeys(lEntry, ['column', 'options', 'sequence', 'minSize', 'maxSize', 'default', 'skip'], lContext);
    lField:=aDef.FieldMap.FindByPropertyName(lName);
    if GetBoolean(lEntry, 'skip', lContext, lSkip) and lSkip then
    begin
      if lField<>Nil then
        lField.Free;
      Continue;
    end;
    if lField=Nil then
      RaiseError('Load', SErrMappingUnknownProperty, [aDef.InstanceClass.ClassName, lName]);
    if GetString(lEntry, 'column', lContext, lStr) then
      lField.FieldName:=lStr;
    lArray:=GetArray(lEntry, 'options', lContext);
    if lArray<>Nil then
    begin
      lField.Options:=ParseOptions(lArray, lContext);
      if foInKey in lField.Options then
        lKeyDeclared:=True;
    end;
    if GetString(lEntry, 'sequence', lContext, lStr) then
      lField.SequenceName:=lStr;
    if GetInteger(lEntry, 'minSize', lContext, lInt) then
      lField.MinSize:=lInt;
    if GetInteger(lEntry, 'maxSize', lContext, lInt) then
      lField.MaxSize:=lInt;
    lDefault:=lEntry.Find('default');
    if lDefault<>Nil then
      lField.DefaultValue:=JSONToDefault(lDefault);
  end;
  if not lKeyDeclared then
    Exit;
  // The fields of the mapping form the whole primary key
  for I:=0 to aDef.FieldMap.Count-1 do
  begin
    lField:=aDef.FieldMap[I];
    if (foInKey in lField.Options) and (aFields.IndexOfName(lField.PropertyName, True)<0) then
      lField.Options:=lField.Options-[foInKey];
  end;
end;


procedure TOPJSONMapping.ApplyIndexes(aDef: TPersisterDef; aIndexes: TJSONArray);

var
  I, J: Integer;
  lContext: String;
  lEntry: TJSONObject;
  lFields: TJSONArray;
  lField: TOPFieldMap;
  lIndexes: TOPIndexDefArray;
  lName: UTF8String;

begin
  lIndexes:=Nil;
  SetLength(lIndexes, aIndexes.Count);
  for I:=0 to aIndexes.Count-1 do
  begin
    lContext:=Format('an index of class "%s"', [aDef.InstanceClass.ClassName]);
    lEntry:=ObjectValue(aIndexes.Items[I], 'indexes', lContext);
    CheckKeys(lEntry, ['name', 'fields'], lContext);
    lIndexes[I].IndexName:=GetRequiredString(lEntry, 'name', lContext);
    lContext:=Format('index "%s" of class "%s"', [lIndexes[I].IndexName, aDef.InstanceClass.ClassName]);
    lFields:=GetArray(lEntry, 'fields', lContext);
    if lFields=Nil then
      RaiseError('Load', SErrMappingMissingKey, ['fields', lContext]);
    lIndexes[I].FieldNames:='';
    for J:=0 to lFields.Count-1 do
    begin
      if lFields.Items[J].JSONType<>jtString then
        RaiseError('Load', SErrMappingKeyType, ['fields', lContext, STypeString]);
      lName:=lFields.Items[J].AsString;
      lField:=FindMappedField(aDef, lName);
      if lField=Nil then
        RaiseError('Load', SErrMappingUnknownField, [lName, lContext]);
      if J>0 then
        lIndexes[I].FieldNames:=lIndexes[I].FieldNames+',';
      lIndexes[I].FieldNames:=lIndexes[I].FieldNames+lField.FieldName;
    end;
  end;
  aDef.Indexes:=lIndexes;
end;


procedure TOPJSONMapping.ApplyClassEntry(aDef: TPersisterDef; aEntry: TJSONObject; aRegistered: Boolean);

var
  lContext: String;
  lStr: UTF8String;
  lFields: TJSONObject;
  lIndexes: TJSONArray;

begin
  lContext:=Format('class "%s"', [aDef.InstanceClass.ClassName]);
  CheckKeys(aEntry, ['table', 'pkConstraint', 'indexes', 'fields', 'relations'], lContext);
  if GetString(aEntry, 'table', lContext, lStr) then
    aDef.TableName:=lStr;
  if GetString(aEntry, 'pkConstraint', lContext, lStr) then
    aDef.PKConstraintName:=lStr;
  lFields:=GetObject(aEntry, 'fields', lContext);
  if lFields<>Nil then
    ApplyFields(aDef, lFields);
  lIndexes:=GetArray(aEntry, 'indexes', lContext);
  if lIndexes<>Nil then
    ApplyIndexes(aDef, lIndexes);
  GetArray(aEntry, 'relations', lContext);
  if aRegistered and (aDef.FieldMap.Count>0) then
    aDef.ResolvePrimaryKey;
end;


procedure TOPJSONMapping.ApplyClasses(aPersisterClass: TOPPersisterClass);

var
  lClasses, lEntry: TJSONObject;
  I: Integer;
  lName: UTF8String;
  lClass: TClass;
  lDef: TPersisterDef;

begin
  lClasses:=ClassesObject;
  if lClasses=Nil then
    Exit;
  for I:=0 to lClasses.Count-1 do
  begin
    lName:=lClasses.Names[I];
    lEntry:=ObjectValue(lClasses.Items[I], lName, 'classes');
    lClass:=FindRegisteredClass(lName);
    if lClass<>Nil then
      ApplyClassEntry(Registry.FindPersister(lClass), lEntry, True)
    else if aPersisterClass<>Nil then
    begin
      lDef:=Registry.RegisterPersister(FindStreamableClass(lName), aPersisterClass);
      lDef.AutoCreated:=True;
    end;
  end;
end;


procedure TOPJSONMapping.ApplyRelation(aParent, aChild: TClass; aRelation: TJSONObject; const aContext: String; aCreated: TFPList);

var
  lRelations: TOPRelationRegistry;
  lRelDef: TOPRelationDef;
  lField: TOPFieldMap;
  lName, lStr: UTF8String;
  lIdx: Integer;
  lCascade: Boolean;

begin
  lName:=GetRequiredString(aRelation, 'foreignKey', aContext);
  lField:=FindMappedField(Registry.FindPersister(aChild), lName);
  if lField=Nil then
    RaiseError('Load', SErrMappingUnknownField, [lName, aContext]);
  lRelations:=TOPRelationRegistry(Registry.RelationRegistry);
  lIdx:=lRelations.IndexOfRelation(aParent, aChild);
  if lIdx<>-1 then
  begin
    lRelDef:=lRelations[lIdx];
    lRelDef.ForeignKeyFieldName:=lField.FieldName;
    if GetBoolean(aRelation, 'dbCascade', aContext, lCascade) then
      lRelDef.DBCascade:=lCascade;
    if GetString(aRelation, 'listProperty', aContext, lStr) then
      lRelDef.ListPropertyName:=lStr;
  end
  else
  begin
    GetBoolean(aRelation, 'dbCascade', aContext, lCascade);
    GetString(aRelation, 'listProperty', aContext, lStr);
    lRelDef:=lRelations.RegisterRelationship(aParent, aChild, lField.FieldName, lCascade, lStr);
    if Assigned(aCreated) then
      aCreated.Add(lRelDef);
  end;
  if GetString(aRelation, 'constraint', aContext, lStr) then
    lRelDef.ConstraintName:=lStr;
end;


procedure TOPJSONMapping.ApplyManyToMany(aClassA, aClassB: TClass; aLink: TJSONObject; const aContext: String; aCreated: TFPList);

var
  lLink: TOPManyToManyDef;
  lJunction, lKeyA, lKeyB, lListA, lListB, lSwap: UTF8String;
  lCascade, lHasCascade, lHasListA, lHasListB: Boolean;

begin
  lJunction:=GetRequiredString(aLink, 'junctionTable', aContext);
  lKeyA:=GetRequiredString(aLink, 'foreignKeyA', aContext);
  lKeyB:=GetRequiredString(aLink, 'foreignKeyB', aContext);
  lHasCascade:=GetBoolean(aLink, 'dbCascade', aContext, lCascade);
  lHasListA:=GetString(aLink, 'listPropertyA', aContext, lListA);
  lHasListB:=GetString(aLink, 'listPropertyB', aContext, lListB);
  lLink:=M2MRegistry.FindDef(aClassA, aClassB);
  if lLink=Nil then
  begin
    lLink:=M2MRegistry.RegisterManyToMany(aClassA, aClassB, lJunction, lKeyA, lKeyB, lCascade, lListA, lListB);
    if Assigned(aCreated) then
      aCreated.Add(lLink);
    Exit;
  end;
  if lLink.ClassA<>aClassA then
  begin
    lSwap:=lLink.ListPropertyNameA;
    lLink.ListPropertyNameA:=lLink.ListPropertyNameB;
    lLink.ListPropertyNameB:=lSwap;
    lLink.ClassA:=aClassA;
    lLink.ClassB:=aClassB;
  end;
  lLink.JunctionTable:=lJunction;
  lLink.ForeignKeyA:=lKeyA;
  lLink.ForeignKeyB:=lKeyB;
  if lHasCascade then
    lLink.DBCascade:=lCascade;
  if lHasListA then
    lLink.ListPropertyNameA:=lListA;
  if lHasListB then
    lLink.ListPropertyNameB:=lListB;
end;


procedure TOPJSONMapping.ApplyRelations(aClass: TClass; aStrict: Boolean; aCreated: TFPList);

var
  lClasses, lEntry, lItem: TJSONObject;
  lArray: TJSONArray;
  I, J: Integer;
  lChild, lParent, lClassA, lClassB: TClass;
  lName, lNameB: UTF8String;
  lContext: String;

begin
  lClasses:=ClassesObject;
  if lClasses<>Nil then
    for I:=0 to lClasses.Count-1 do
    begin
      lChild:=FindRegisteredClass(lClasses.Names[I]);
      if lChild=Nil then
        Continue;
      lEntry:=ObjectValue(lClasses.Items[I], lClasses.Names[I], 'classes');
      lContext:=Format('class "%s"', [lChild.ClassName]);
      lArray:=GetArray(lEntry, 'relations', lContext);
      if lArray=Nil then
        Continue;
      for J:=0 to lArray.Count-1 do
      begin
        lContext:=Format('a relation of class "%s"', [lChild.ClassName]);
        lItem:=ObjectValue(lArray.Items[J], 'relations', lContext);
        CheckKeys(lItem, ['parent', 'foreignKey', 'constraint', 'dbCascade', 'listProperty'], lContext);
        lName:=GetRequiredString(lItem, 'parent', lContext);
        lParent:=FindRegisteredClass(lName);
        if lParent=Nil then
        begin
          if aStrict then
            RaiseError('Load', SErrMappingParentNotFound, [lName, lChild.ClassName]);
          Continue;
        end;
        if (aClass<>Nil) and (aClass<>lChild) and (aClass<>lParent) then
          Continue;
        ApplyRelation(lParent, lChild, lItem, lContext, aCreated);
      end;
    end;
  lArray:=GetArray(FDocument, 'manyToMany', 'the mapping');
  if (lArray=Nil) or (lArray.Count=0) then
    Exit;
  if M2MRegistry=Nil then
    RaiseError('Load', SErrMappingNoM2MRegistry, []);
  for I:=0 to lArray.Count-1 do
  begin
    lContext:='a many-to-many link';
    lItem:=ObjectValue(lArray.Items[I], 'manyToMany', lContext);
    CheckKeys(lItem, ['classA', 'classB', 'junctionTable', 'foreignKeyA', 'foreignKeyB',
                      'dbCascade', 'listPropertyA', 'listPropertyB'], lContext);
    lName:=GetRequiredString(lItem, 'classA', lContext);
    lNameB:=GetRequiredString(lItem, 'classB', lContext);
    lClassA:=FindRegisteredClass(lName);
    lClassB:=FindRegisteredClass(lNameB);
    if (lClassA=Nil) or (lClassB=Nil) then
    begin
      if not aStrict then
        Continue;
      if lClassA=Nil then
        RaiseError('Load', SErrMappingM2MClass, [lName]);
      RaiseError('Load', SErrMappingM2MClass, [lNameB]);
    end;
    if (aClass<>Nil) and (aClass<>lClassA) and (aClass<>lClassB) then
      Continue;
    lContext:=Format('the many-to-many link of "%s" and "%s"', [lName, lNameB]);
    ApplyManyToMany(lClassA, lClassB, lItem, lContext, aCreated);
  end;
end;


procedure TOPJSONMapping.LoadFromStream(aStream: TStream; aPersisterClass: TOPPersisterClass);

var
  lData: TJSONData;
  lVersion: Integer;

begin
  lData:=Nil;
  try
    lData:=GetJSON(aStream);
  except
    on E: Exception do
      RaiseError('LoadFromStream', SErrMappingInvalidJSON, [E.Message]);
  end;
  if not (lData is TJSONObject) then
  begin
    lData.Free;
    RaiseError('LoadFromStream', SErrMappingNotObject, []);
  end;
  Unhook;
  FreeAndNil(FDocument);
  FDocument:=TJSONObject(lData);
  CheckKeys(FDocument, ['version', 'classes', 'manyToMany'], 'the mapping');
  if GetInteger(FDocument, 'version', 'the mapping', lVersion) and (lVersion<>MappingVersion) then
    RaiseError('LoadFromStream', SErrMappingVersion, [lVersion]);
  ClassesObject;
  GetArray(FDocument, 'manyToMany', 'the mapping');
  Hook;
  FLoading:=True;
  try
    ApplyClasses(aPersisterClass);
    FLoading:=False;
    ApplyRelations(Nil, aPersisterClass<>Nil, Nil);
  except
    FLoading:=False;
    Unhook;
    raise;
  end;
end;


procedure TOPJSONMapping.LoadFromFile(const aFileName: String; aPersisterClass: TOPPersisterClass);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(lStream, aPersisterClass);
  finally
    lStream.Free;
  end;
end;


procedure TOPJSONMapping.ClassRegistering(aDef: TPersisterDef);

var
  lEntry: TJSONObject;

begin
  lEntry:=FindEntry(aDef.InstanceClass);
  if lEntry<>Nil then
    ApplyClassEntry(aDef, lEntry, False);
end;


procedure TOPJSONMapping.ClassRegistered(aDef: TPersisterDef);

var
  lCreated: TFPList;
  I: Integer;

begin
  if FLoading or (FDocument=Nil) then
    Exit;
  lCreated:=TFPList.Create;
  try
    try
      ApplyRelations(aDef.InstanceClass, False, lCreated);
    except
      for I:=lCreated.Count-1 downto 0 do
        TObject(lCreated[I]).Free;
      raise;
    end;
  finally
    lCreated.Free;
  end;
end;


function TOPJSONMapping.FieldToJSON(aField: TOPFieldMap): TJSONObject;

var
  lOptions: TJSONArray;
  lOption: TOPFieldOption;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('column', aField.FieldName);
    lOptions:=TJSONArray.Create;
    Result.Add('options', lOptions);
    for lOption:=Low(TOPFieldOption) to High(TOPFieldOption) do
      if lOption in aField.Options then
        lOptions.Add(GetEnumName(TypeInfo(TOPFieldOption), Ord(lOption)));
    Result.Add('sequence', aField.SequenceName);
    Result.Add('minSize', aField.MinSize);
    Result.Add('maxSize', aField.MaxSize);
    Result.Add('default', DefaultToJSON(aField.DefaultValue));
  except
    Result.Free;
    raise;
  end;
end;


function TOPJSONMapping.DefToJSON(aDef: TPersisterDef): TJSONObject;

var
  lIndexes, lIndexFields, lRelations: TJSONArray;
  lFields, lItem: TJSONObject;
  lNames: TStringArray;
  lRelDefs: TOPRelationRegistry;
  lRelDef: TOPRelationDef;
  I, J: Integer;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('table', aDef.TableName);
    Result.Add('pkConstraint', aDef.PKConstraintName);
    lIndexes:=TJSONArray.Create;
    Result.Add('indexes', lIndexes);
    for I:=0 to High(aDef.Indexes) do
    begin
      lItem:=TJSONObject.Create;
      lIndexes.Add(lItem);
      lItem.Add('name', aDef.Indexes[I].IndexName);
      lIndexFields:=TJSONArray.Create;
      lItem.Add('fields', lIndexFields);
      lNames:=String(aDef.Indexes[I].FieldNames).Split([',']);
      for J:=0 to High(lNames) do
        lIndexFields.Add(Trim(lNames[J]));
    end;
    lFields:=TJSONObject.Create;
    Result.Add('fields', lFields);
    for I:=0 to aDef.FieldMap.Count-1 do
      lFields.Add(aDef.FieldMap[I].PropertyName, FieldToJSON(aDef.FieldMap[I]));
    lRelations:=TJSONArray.Create;
    Result.Add('relations', lRelations);
    lRelDefs:=TOPRelationRegistry(Registry.RelationRegistry);
    for I:=0 to lRelDefs.RelationCount-1 do
    begin
      lRelDef:=lRelDefs[I];
      if lRelDef.ChildClass<>aDef.InstanceClass then
        Continue;
      lItem:=TJSONObject.Create;
      lRelations.Add(lItem);
      lItem.Add('parent', ClassKey(lRelDef.ParentClass));
      lItem.Add('foreignKey', lRelDef.ForeignKeyFieldName);
      lItem.Add('constraint', lRelDef.ConstraintName);
      lItem.Add('dbCascade', lRelDef.DBCascade);
      lItem.Add('listProperty', lRelDef.ListPropertyName);
    end;
  except
    Result.Free;
    raise;
  end;
end;


procedure TOPJSONMapping.SaveToStream(aStream: TStream);

var
  lDoc, lClasses, lItem: TJSONObject;
  lLinks: TJSONArray;
  lLink: TOPManyToManyDef;
  lText: TJSONStringType;
  I: Integer;

begin
  lDoc:=TJSONObject.Create;
  try
    lDoc.Add('version', MappingVersion);
    lClasses:=TJSONObject.Create;
    lDoc.Add('classes', lClasses);
    for I:=0 to Registry.PersisterCount-1 do
      lClasses.Add(ClassKey(Registry[I].InstanceClass), DefToJSON(Registry[I]));
    if Assigned(M2MRegistry) and (M2MRegistry.DefCount>0) then
    begin
      lLinks:=TJSONArray.Create;
      lDoc.Add('manyToMany', lLinks);
      for I:=0 to M2MRegistry.DefCount-1 do
      begin
        lLink:=M2MRegistry[I];
        lItem:=TJSONObject.Create;
        lLinks.Add(lItem);
        lItem.Add('classA', ClassKey(lLink.ClassA));
        lItem.Add('classB', ClassKey(lLink.ClassB));
        lItem.Add('junctionTable', lLink.JunctionTable);
        lItem.Add('foreignKeyA', lLink.ForeignKeyA);
        lItem.Add('foreignKeyB', lLink.ForeignKeyB);
        lItem.Add('dbCascade', lLink.DBCascade);
        lItem.Add('listPropertyA', lLink.ListPropertyNameA);
        lItem.Add('listPropertyB', lLink.ListPropertyNameB);
      end;
    end;
    lText:=lDoc.FormatJSON+LineEnding;
    aStream.WriteBuffer(lText[1], Length(lText));
  finally
    lDoc.Free;
  end;
end;


procedure TOPJSONMapping.SaveToFile(const aFileName: String);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(lStream);
  finally
    lStream.Free;
  end;
end;


{ Global mapping }

procedure LoadMapping(aStream: TStream; aPersisterClass: TOPPersisterClass);

begin
  ClearMapping;
  _Mapping:=TOPJSONMapping.Create(Nil);
  try
    _Mapping.LoadFromStream(aStream, aPersisterClass);
  except
    FreeAndNil(_Mapping);
    raise;
  end;
end;


procedure LoadMapping(const aFileName: String; aPersisterClass: TOPPersisterClass);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadMapping(lStream, aPersisterClass);
  finally
    lStream.Free;
  end;
end;


procedure SaveMapping(aStream: TStream);

var
  lMapping: TOPJSONMapping;

begin
  lMapping:=TOPJSONMapping.Create(Nil);
  try
    lMapping.SaveToStream(aStream);
  finally
    lMapping.Free;
  end;
end;


procedure SaveMapping(const aFileName: String);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(aFileName, fmCreate);
  try
    SaveMapping(lStream);
  finally
    lStream.Free;
  end;
end;


procedure ClearMapping;

begin
  FreeAndNil(_Mapping);
end;


{ TOPDefaultKeyListener }

procedure TOPDefaultKeyListener.ClassRegistering(aDef: TPersisterDef);

var
  lField: TOPFieldMap;

begin
  try
    aDef.ResolvePrimaryKey;
  except
    on EOPFRegistry do
    begin
      if aDef.FieldMap.Count=0 then
        raise;
      lField:=aDef.FieldMap[0];
      lField.Options:=lField.Options+[foInKey];
    end;
  end;
end;


procedure TOPDefaultKeyListener.ClassRegistered(aDef: TPersisterDef);

begin
end;


procedure SaveDefaultMapping(aStream: TStream; const aClasses: array of TClass; aPersisterClass: TOPPersisterClass);

var
  lRegistry: TOPRegistry;
  lM2M: TOPManyToManyRegistry;
  lMapping: TOPJSONMapping;
  lListener: TOPDefaultKeyListener;
  I: Integer;

begin
  lListener:=Nil;
  lM2M:=Nil;
  lMapping:=Nil;
  lRegistry:=TOPRegistry.Create(Nil);
  try
    lListener:=TOPDefaultKeyListener.Create;
    lRegistry.RegistrationListener:=lListener;
    for I:=0 to High(aClasses) do
      lRegistry.RegisterPersister(aClasses[I], aPersisterClass);
    lRegistry.RegistrationListener:=Nil;
    lM2M:=TOPManyToManyRegistry.Create(Nil);
    lM2M.Registry:=lRegistry;
    lMapping:=TOPJSONMapping.Create(Nil);
    lMapping.Registry:=lRegistry;
    lMapping.M2MRegistry:=lM2M;
    lMapping.SaveToStream(aStream);
  finally
    lMapping.Free;
    lM2M.Free;
    lRegistry.Free;
    lListener.Free;
  end;
end;


procedure SaveDefaultMapping(const aFileName: String; const aClasses: array of TClass; aPersisterClass: TOPPersisterClass);

var
  lStream: TFileStream;

begin
  lStream:=TFileStream.Create(aFileName, fmCreate);
  try
    SaveDefaultMapping(lStream, aClasses, aPersisterClass);
  finally
    lStream.Free;
  end;
end;


finalization
  ClearMapping;
end.
