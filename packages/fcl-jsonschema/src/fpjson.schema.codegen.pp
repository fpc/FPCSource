{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    JSON Schema - pascal code generator

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpjson.schema.codegen;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.DateUtils, Pascal.CodeGenerator,  System.Contnrs,
  {$ELSE}
  Classes, SysUtils, dateutils, pascodegen, contnrs,
  {$ENDIF}
  fpjson.schema.types,
  fpjson.schema.consts,
  fpjson.schema.Pascaltypes;

Type

  { TJSONSchemaCodeGen }

  { TJSONSchemaCodeGenerator }

  TJSONSchemaCodeGenerator = class(TPascalCodeGenerator)
  private
    FData: TSchemaData;
    FDelphiCode: boolean;
    FTrackChanges: boolean;
    FUseProperties: boolean;
    FVerboseHeader: Boolean;
    FWriteClassType: boolean;
    function GetUseProperties: boolean;
  protected
    procedure GenerateHeader; virtual;
    procedure GenerateFPCDirectives(modeswitches : array of string);
    procedure GenerateFPCDirectives();
    function GetPascalTypeAndDefault(aType: TSchemaSimpleType; out aPasType, aPasDefault: string) : boolean;
    function GetJSONDefault(aType: TPascalType) : String;
    procedure SetTypeData(aData : TSchemaData);
  public
    Property TypeData : TSchemaData Read FData;
    property DelphiCode: boolean read FDelphiCode write FDelphiCode;
    Property VerboseHeader : Boolean Read FVerboseHeader Write FVerboseHeader;
    property WriteClassType: boolean read FWriteClassType write FWriteClassType;
    // Dto classes expose their members as properties with private fields. Implied by TrackChanges.
    property UseProperties: boolean read GetUseProperties write FUseProperties;
    // Dto classes record which properties were assigned; only those are serialized.
    property TrackChanges: boolean read FTrackChanges write FTrackChanges;
  end;

  { TTypeCodeGenerator }

  TTypeCodeGenerator = class(TJSONSchemaCodeGenerator)
  private
    FTypeParentClass: string;
    FGenerated : TFPObjectHashTable;
    procedure GenerateClassForwardTypes(aData: TSchemaData);
    procedure GenerateClassTypes(aData: TSchemaData);
    procedure GenerateIntegerTypes(aData: TSchemaData);
    procedure GeneratePascalArrayTypes(aData: TSchemaData);
    procedure GenerateStringTypes(aData: TSchemaData);
    procedure WriteDtoConstructor(aType: TPascalTypeData); virtual;
    procedure WriteDtoField(aType: TPascalTypeData; aProperty: TPascalPropertyData); virtual;
    procedure WriteDtoProperty(aType: TPascalTypeData; aProperty: TPascalPropertyData); virtual;
    procedure WriteDtoPropertyClassType(aType: TPascalTypeData); virtual;
    procedure WriteDtoTrackingImplementation(aType: TPascalTypeData); virtual;
    procedure WriteDtoType(aType: TPascalTypeData); virtual;
    procedure WriteDtoForwardType(aType: TPascalTypeData); virtual;
    procedure WriteDtoArrayType(aType: TPascalTypeData); virtual;
    procedure WriteDtoArrayRefType(aType: TPascalTypeData); virtual;
    procedure WriteStringArrayType(aType: TPascalTypeData);
    procedure WriteIntegerArrayType(aType: TPascalTypeData);
    procedure WriteStringType(aType: TPascalTypeData); virtual;
    procedure WriteIntegerType(aType: TPascalTypeData); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(aData: TSchemaData);
    property TypeParentClass: string read FTypeParentClass write FTypeParentClass;
  end;

  { TSerializerCodeGen }

  { TSerializerCodeGenerator }

  TSerializerCodeGenerator = class(TJSONSchemaCodeGenerator)
  const
    Bools : Array[Boolean] of String = ('False','True');
  private
    FConvertUTC: Boolean;
    FDataUnitName: string;
    FSkipReadOnly: Boolean;
  protected
    // True if the property schema is marked readOnly
    function IsReadOnly(aProperty: TPascalPropertyData) : boolean; virtual;
    // Name of the local variable used to deserialize a property when UseProperties is set
    function DeserializeLocalName(aProperty: TPascalPropertyData) : string;
    // True if deserializing the property needs a local variable
    function NeedsDeserializeLocal(aProperty: TPascalPropertyData) : boolean;
    // Get qualified type name for deserializer references (handles rtbQualify for reserved types)
    function QualifyTypeName(const aTypeName: string): string; virtual;
    function MustSerializeType(aType : TPascalTypeData) : boolean; virtual;
    // True if the property is a TDateTime with schema format 'date'
    function IsDateOnly(aProperty: TPascalPropertyData) : boolean; virtual;
    function FieldToJSON(aProperty: TPascalPropertyData) : string; virtual;
    function ArrayMemberToField(aType: TPascalType; const aPropertyTypeName: String; const aFieldName: string): string; virtual;
    function FieldToJSON(aType: TPascalType; aFieldName: String): string; virtual;
    procedure GenerateConverters; virtual;
    function JSONToField(aProperty: TPascalPropertyData) : string; virtual;
    function JSONToField(aType: TPascalType; const aPropertyTypeName: string; const aKeyName: string): string; virtual;
    procedure WriteFieldDeSerializer(aType : TPascalTypeData; aProperty: TPascalPropertyData); virtual;
    procedure WriteFieldSerializer(aType : TPascalTypeData; aProperty: TPascalPropertyData; aIndex: Integer); virtual;
    // Dto (object) type helpers
    procedure WriteDtoObjectSerializer(aType: TPascalTypeData); virtual;
    procedure WriteDtoSerializer(aType: TPascalTypeData); virtual;
    procedure WriteDtoObjectDeserializer(aType: TPascalTypeData); virtual;
    procedure WriteDtoDeserializer(aType: TPascalTypeData); virtual;
    procedure WriteDtoHelper(aType: TPascalTypeData); virtual;
    // Array type helpers
    procedure WriteArrayHelper(aType: TPascalTypeData); virtual;
    procedure WriteArrayHelperDeserialize(aType: TPascalTypeData);
    procedure WriteArrayHelperDeSerializeArray(aType: TPascalTypeData);
    procedure WriteArrayHelperImpl(aType: TPascalTypeData);
    procedure WriteArrayHelperSerialize(aType: TPascalTypeData);
    procedure WriteArrayHelperSerializeArray(aType: TPascalTypeData);
  public
    procedure Execute(aData: TSchemaData);
    property DataUnitName: string read FDataUnitName write FDataUnitName;
    property ConvertUTC : Boolean Read FConvertUTC Write FConvertUTC;
    // Do not serialize readOnly properties (client side code)
    property SkipReadOnly : Boolean Read FSkipReadOnly Write FSkipReadOnly;
  end;

implementation

function TJSONSchemaCodeGenerator.GetPascalTypeAndDefault(
  aType: TSchemaSimpleType; out aPasType, aPasDefault: string) : boolean;

begin
  Result := True;
  case aType of
    sstInteger:
    begin
      aPasType := FData.TypeMap['integer'];
      aPasDefault := '0';
    end;
    sstNumber:
    begin
      aPasType := FData.TypeMap['number'];
      aPasDefault := '0';
    end;
    sstBoolean:
    begin
      aPasType := FData.TypeMap['boolean'];
      aPasDefault := 'False';
    end;
    sstString:
    begin
      aPasType := FData.TypeMap['string'];
      aPasDefault := '''''';
    end;
    sstObject:
    begin
      aPasType := 'TJSONObject';
      aPasDefault := 'TJSONObject(Nil)';
    end;
    sstArray:
    begin
      aPasType := 'TJSONArray';
      aPasDefault := 'TJSONArray(Nil)';
    end;
    else
      Result := False;
  end;
end;


function TJSONSchemaCodeGenerator.GetJSONDefault(aType: TPascalType): String;

begin
  case aType of
    ptEnum:
      Result:='''''';
    ptDateTime:
      Result:='''''';
    ptInteger,
    ptInt64:
      Result:='0';
    ptfloat32,
    ptfloat64:
      Result := '0.0';
    ptBoolean:
      Result := 'False';
    ptJSON,
    ptString:
      Result := '''''';
    ptAnonStruct:
      Result := 'TJSONObject(Nil)';
    ptArray:
      Result := 'TJSONArray(Nil)';
  end;
end;


procedure TJSONSchemaCodeGenerator.SetTypeData(aData: TSchemaData);
begin
  FData:=aData;
end;


function TJSONSchemaCodeGenerator.GetUseProperties: boolean;

begin
  Result:=FUseProperties or FTrackChanges;
end;


procedure TJSONSchemaCodeGenerator.GenerateHeader;

begin
  // Do nothing
end;

procedure TJSONSchemaCodeGenerator.GenerateFPCDirectives(modeswitches: array of string);

var
  S : String;

begin
  if DelphiCode then
    begin
    Addln('{$ifdef FPC}');
    AddLn('{$mode delphi}');
    end
  else
    AddLn('{$mode objfpc}');
  AddLn('{$h+}');
  for S in modeswitches do
    AddLn('{$modeswitch %s}',[lowercase(S)]);
  if DelphiCode then
    Addln('{$endif FPC}');
  Addln('');
end;

procedure TJSONSchemaCodeGenerator.GenerateFPCDirectives;
begin
  GenerateFPCDirectives([]);
end;


{ TTypeCodeGenerator }

procedure TTypeCodeGenerator.WriteDtoField(aType: TPascalTypeData; aProperty: TPascalPropertyData);

var
  lFieldName, lTypeName: string;

begin
  lFieldName := aProperty.PascalName;
  lTypeName := aProperty.PascalTypeName;
  if UseProperties and WriteClassType then
    lFieldName:='F'+lFieldName;
  if lTypeName = '' then
    Addln('// Unknown type for field %s...', [lFieldName])
  else
    Addln('%s : %s;', [lFieldName, lTypeName]);
end;


procedure TTypeCodeGenerator.WriteDtoProperty(aType: TPascalTypeData; aProperty: TPascalPropertyData);

var
  lName, lTypeName, lWriter: string;

begin
  lName := aProperty.PascalName;
  lTypeName := aProperty.PascalTypeName;
  if lTypeName = '' then
    exit;
  if TrackChanges then
    lWriter:='Set'+lName
  else
    lWriter:='F'+lName;
  Addln('property %s : %s read F%s write %s;', [lName, lTypeName, lName, lWriter]);
end;


procedure TTypeCodeGenerator.WriteDtoPropertyClassType(aType: TPascalTypeData);

var
  I: integer;
  lProp: TPascalPropertyData;

begin
  Addln('%s = Class(%s)', [aType.PascalName, TypeParentClass]);
  Addln('private');
  indent;
  for I:=0 to aType.PropertyCount-1 do
    WriteDtoField(aType,aType.Properties[I]);
  if TrackChanges then
    begin
    if aType.PropertyCount>0 then
      Addln('FChanged : array[0..%d] of Boolean;', [aType.PropertyCount-1]);
    for I:=0 to aType.PropertyCount-1 do
      begin
      lProp:=aType.Properties[I];
      if lProp.PascalTypeName<>'' then
        Addln('procedure Set%s(const aValue : %s);', [lProp.PascalName, lProp.PascalTypeName]);
      end;
    end;
  undent;
  Addln('public');
  indent;
  if aType.HasObjectProperty(True) then
    Addln('constructor CreateWithMembers;');
  if TrackChanges then
    begin
    Addln('// True if property aIndex (declaration order) was assigned, or an object in it has changes.');
    Addln('function FieldChanged(aIndex : Integer) : Boolean;');
    Addln('// Mark property aIndex (declaration order) as changed.');
    Addln('procedure MarkChanged(aIndex : Integer);');
    Addln('// True if any property was assigned, or an object in it has changes.');
    Addln('function HasChanges : Boolean;');
    Addln('// Forget all changes, also in the objects this object refers to.');
    Addln('procedure ClearChanges;');
    Addln('// Mark all properties as changed, also in the objects this object refers to.');
    Addln('procedure MarkAllChanged;');
    end;
  for I:=0 to aType.PropertyCount-1 do
    WriteDtoProperty(aType,aType.Properties[I]);
  undent;
  Addln('end;');
  Addln('');
end;


procedure TTypeCodeGenerator.WriteDtoTrackingImplementation(aType: TPascalTypeData);

var
  I, lCount: integer;
  lProp: TPascalPropertyData;
  lName, lField: string;
  lHasObjects, lHasObjectArrays: Boolean;

  // Write method aMethod that sets all change flags to aValue, recursively.
  procedure WriteSetAllChanged(const aMethod: string; aValue: Boolean);

  var
    lJ: integer;
    lSubProp: TPascalPropertyData;
    lSubField: string;

  begin
    Addln('procedure %s.%s;', [lName, aMethod]);
    Addln('');
    if lHasObjectArrays then
      begin
      Addln('var');
      indent;
      Addln('lI : Integer;');
      undent;
      Addln('');
      end;
    Addln('begin');
    indent;
    if lCount>0 then
      Addln('FillChar(FChanged,SizeOf(FChanged),Ord(%s));', [BoolToStr(aValue,'True','False')]);
    for lJ:=0 to lCount-1 do
      begin
      lSubProp:=aType.Properties[lJ];
      lSubField:='F'+lSubProp.PascalName;
      if lSubProp.PropertyType in [ptSchemaStruct,ptAnonStruct] then
        begin
        Addln('if Assigned(%s) then', [lSubField]);
        indent;
        Addln('%s.%s;', [lSubField, aMethod]);
        undent;
        end
      else if (lSubProp.PropertyType=ptArray) and (lSubProp.ElementType in [ptSchemaStruct,ptAnonStruct]) then
        begin
        Addln('for lI:=0 to Length(%s)-1 do', [lSubField]);
        indent;
        Addln('if Assigned(%s[lI]) then', [lSubField]);
        indent;
        Addln('%s[lI].%s;', [lSubField, aMethod]);
        undent;
        undent;
        end;
      end;
    undent;
    Addln('end;');
    Addln('');
  end;

begin
  lName:=aType.PascalName;
  lCount:=aType.PropertyCount;
  lHasObjects:=False;
  lHasObjectArrays:=False;
  for I:=0 to lCount-1 do
    begin
    lProp:=aType.Properties[I];
    if lProp.PropertyType in [ptSchemaStruct,ptAnonStruct] then
      lHasObjects:=True
    else if (lProp.PropertyType=ptArray) and (lProp.ElementType in [ptSchemaStruct,ptAnonStruct]) then
      lHasObjectArrays:=True;
    end;
  for I:=0 to lCount-1 do
    begin
    lProp:=aType.Properties[I];
    if lProp.PascalTypeName='' then
      continue;
    Addln('procedure %s.Set%s(const aValue : %s);', [lName, lProp.PascalName, lProp.PascalTypeName]);
    Addln('');
    Addln('begin');
    indent;
    Addln('F%s:=aValue;', [lProp.PascalName]);
    Addln('FChanged[%d]:=True;', [I]);
    undent;
    Addln('end;');
    Addln('');
    end;
  Addln('function %s.FieldChanged(aIndex : Integer) : Boolean;', [lName]);
  Addln('');
  if lHasObjectArrays then
    begin
    Addln('var');
    indent;
    Addln('lI : Integer;');
    undent;
    Addln('');
    end;
  Addln('begin');
  indent;
  if lCount=0 then
    Addln('Result:=False;')
  else
    begin
    Addln('if (aIndex<0) or (aIndex>%d) then', [lCount-1]);
    indent;
    Addln('Exit(False);');
    undent;
    Addln('Result:=FChanged[aIndex];');
    if lHasObjects or lHasObjectArrays then
      begin
      Addln('if Result then');
      indent;
      Addln('Exit;');
      undent;
      Addln('case aIndex of');
      indent;
      for I:=0 to lCount-1 do
        begin
        lProp:=aType.Properties[I];
        lField:='F'+lProp.PascalName;
        if lProp.PropertyType in [ptSchemaStruct,ptAnonStruct] then
          Addln('%d: Result:=Assigned(%s) and %s.HasChanges;', [I, lField, lField])
        else if (lProp.PropertyType=ptArray) and (lProp.ElementType in [ptSchemaStruct,ptAnonStruct]) then
          begin
          Addln('%d:', [I]);
          indent;
          Addln('for lI:=0 to Length(%s)-1 do', [lField]);
          indent;
          Addln('if Assigned(%s[lI]) and %s[lI].HasChanges then', [lField, lField]);
          indent;
          Addln('Exit(True);');
          undent;
          undent;
          undent;
          end;
        end;
      undent;
      Addln('end;');
      end;
    end;
  undent;
  Addln('end;');
  Addln('');
  Addln('procedure %s.MarkChanged(aIndex : Integer);', [lName]);
  Addln('');
  Addln('begin');
  indent;
  if lCount>0 then
    begin
    Addln('if (aIndex>=0) and (aIndex<=%d) then', [lCount-1]);
    indent;
    Addln('FChanged[aIndex]:=True;');
    undent;
    end;
  undent;
  Addln('end;');
  Addln('');
  Addln('function %s.HasChanges : Boolean;', [lName]);
  Addln('');
  Addln('var');
  indent;
  Addln('lI : Integer;');
  undent;
  Addln('');
  Addln('begin');
  indent;
  Addln('Result:=False;');
  Addln('for lI:=0 to %d do', [lCount-1]);
  indent;
  Addln('if FieldChanged(lI) then');
  indent;
  Addln('Exit(True);');
  undent;
  undent;
  undent;
  Addln('end;');
  Addln('');
  WriteSetAllChanged('ClearChanges',False);
  WriteSetAllChanged('MarkAllChanged',True);
end;


procedure TTypeCodeGenerator.WriteDtoConstructor(aType: TPascalTypeData);

var
  I : Integer;
  lProp : TPascalPropertyData;
  lConstructor, lPrefix : String;

begin
  if UseProperties then
    lPrefix:='F'
  else
    lPrefix:='';
  Addln('constructor %s.CreateWithMembers;',[aType.PascalName]);
  Addln('');
  Addln('begin');
  indent;
  For I:=0 to aType.PropertyCount-1 do
    begin
    lProp:=aType.Properties[i];
    if lProp.PropertyType=ptSchemaStruct then
      begin
      if lProp.TypeData.HasObjectProperty(True) then
        lConstructor:='CreateWithMembers'
      else
        lConstructor:='Create';
      AddLn('%s%s := %s.%s;',[lPrefix,lProp.PascalName,lProp.TypeData.PascalName,lConstructor]);
      end;
    end;
  Undent;
  Addln('end;');
  Addln('');
end;


procedure TTypeCodeGenerator.WriteDtoType(aType: TPascalTypeData);

var
  I: integer;

begin
  fGenerated.Add(aType.PascalName,aType);
  if WriteClassType and UseProperties then
    begin
    WriteDtoPropertyClassType(aType);
    exit;
    end;
  if WriteClassType then
    Addln('%s = Class(%s)', [aType.PascalName, TypeParentClass])
  else
    Addln('%s = record', [aType.PascalName]);
  indent;
  for I:=0  to aType.PropertyCount-1 do
    WriteDtoField(aType,aType.Properties[i]);
  if WriteClassType and aType.HasObjectProperty(True) then
    Addln('constructor CreateWithMembers;');
  undent;
  Addln('end;');
  Addln('');
end;

procedure TTypeCodeGenerator.WriteDtoForwardType(aType: TPascalTypeData);
begin
  Addln('%s = class;',[aType.PascalName]);
end;

procedure TTypeCodeGenerator.WriteDtoArrayType(aType: TPascalTypeData);

var
  Fmt : String;

begin
  if FGenerated.Items[aType.PascalName]<>Nil then
    exit;
  FGenerated.Add(aType.PascalName,aType);
  if DelphiCode then
    Fmt:='%s = TArray<%s>;'
  else
    Fmt:='%s = Array of %s;';
  Addln(Fmt,[aType.PascalName,aType.ElementTypeData.PascalName]);
end;

procedure TTypeCodeGenerator.WriteDtoArrayRefType(aType: TPascalTypeData);
var
  Fmt : String;
  lName : string;
begin
  if DelphiCode then
    Fmt:='%s = TArray<%s>;'
  else
    Fmt:='%s = Array of %s;';
  Addln(Fmt,[aType.PascalName,aType.ElementTypeData.PascalName]);

end;

procedure TTypeCodeGenerator.WriteStringArrayType(aType: TPascalTypeData);

begin
  WriteDtoArrayType(aType);
end;

procedure TTypeCodeGenerator.WriteIntegerArrayType(aType: TPascalTypeData);
begin
  WriteDtoArrayType(aType);
end;

procedure TTypeCodeGenerator.WriteStringType(aType: TPascalTypeData);

begin
  FGenerated.Add(aType.PascalName,aType);
  Addln('%s = string;',[aType.PascalName]);
end;

procedure TTypeCodeGenerator.WriteIntegerType(aType: TPascalTypeData);
var
  I,lEl,lMin,lMax : Integer;
  lName: string;
begin
  lMin:=0;
  lMax:=0;
  FGenerated.Add(aType.PascalName,aType);
  if aType.Schema.Validations.HasKeywordData(jskEnum) and
     (aType.Schema.Validations.Enum.Count>0) then
    begin
    lMin:=aType.Schema.Validations.Enum.Items[0].AsInteger;
    lMax:=aType.Schema.Validations.Enum.Items[0].AsInteger;
    for I:=1 to aType.Schema.Validations.Enum.Count-1 do
      begin
      lEl:=aType.Schema.Validations.Enum.Items[i].AsInteger;
      if lEl<lMin then
        lMin:=lEl;
      if lEl>lMax then
        lMax:=lEl;
      end;
    if (lMax-lMin+1)<>aType.Schema.Validations.Enum.Count then
      begin
      lMin:=0;
      lMax:=0;
      end;
    end;
  lName:=aType.PascalName;
  if lMin<>lMax then
    Addln('%s = %d..%d;',[lName,lMin,lMax])
  else
    Addln('%s = Integer;',[lName]);

end;


constructor TTypeCodeGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TypeParentClass := 'TObject';
end;

procedure TTypeCodeGenerator.GenerateStringTypes(aData : TSchemaData);

var
  I,lCount: integer;
  lType,lArray : TPascalTypeData;
begin
  lCount:=0;
  for I := 0 to aData.TypeCount-1 do
    begin
    lType:=aData.Types[I];
    if (lType.PascalType=ptString) then
      begin
      DoLog('Generating string type %s', [lType.PascalName]);
      WriteStringType(lType);
      inc(lCount);
      lArray:=aData.FindSchemaTypeData('['+lType.SchemaName+']');
      if lArray<>Nil then
        begin
        WriteStringArrayType(lArray);
        inc(lCount);
        end;
      end;
    end;
  if lCount>0 then
    AddLn('');
end;

procedure TTypeCodeGenerator.GenerateIntegerTypes(aData : TSchemaData);

var
  I,lCount: integer;
  lType,lArray : TPascalTypeData;
begin
  lCount:=0;
  for I := 0 to aData.TypeCount-1 do
    begin
    lType:=aData.Types[I];
    if (lType.PascalType=ptInteger) then
      begin
      DoLog('Generating integer type %s', [lType.PascalName]);
      WriteIntegerType(lType);
      inc(lCount);
      lArray:=aData.FindSchemaTypeData('['+lType.SchemaName+']');
      if lArray<>Nil then
        begin
        WriteIntegerArrayType(lArray);
        inc(lCount);
        end;
      end;
    end;
  if lCount>0 then
    AddLn('');
end;

procedure TTypeCodeGenerator.GenerateClassForwardTypes(aData: TSchemaData);
var
  I: integer;
  lArray : TPascalTypeData;
  lName : string;
begin
  for I := 0 to aData.TypeCount-1 do
    if aData.Types[I].PascalType in [ptSchemaStruct,ptAnonStruct] then
      begin
        DoLog('Generating DTO class forward type %s', [aData.Types[I].PascalName]);
        lName:=aData.Types[I].PascalName;
        WriteDtoForwardType(aData.Types[I]);
      end

end;

procedure TTypeCodeGenerator.GenerateClassTypes(aData : TSchemaData);

var
  I: integer;
  lArray : TPascalTypeData;
  lName : string;
begin
  for I := 0 to aData.TypeCount-1 do
    if aData.Types[I].PascalType in [ptSchemaStruct,ptAnonStruct] then
      begin
        DoLog('Generating DTO class type %s', [aData.Types[I].PascalName]);
        lName:=aData.Types[I].PascalName;
        WriteDtoType(aData.Types[I]);
        lArray:=aData.FindSchemaTypeData('['+aData.Types[I].SchemaName+']');
        if lArray<>Nil then
          WriteDtoArrayType(lArray);
      end
end;

procedure TTypeCodeGenerator.GeneratePascalArrayTypes(aData : TSchemaData);

// Generate a definition of an array of a standard pascal type.

var
  I, lCount: integer;
  lType : TPascalTypeData;
  lName : string;

begin
  lCount := 0;
  for I := 0 to aData.TypeCount-1 do
    begin
    lType:=aData.Types[I];
    // It is an array
    if (lType.PascalType=ptArray) then
      begin
      if (lType.ElementTypeData.PascalName<>'') then
        begin
        DoLog('Generating array type %s', [lType.PascalName]);
        WriteDtoArrayType(lType);
        inc(lCount);
        end
      end;
    end;
  if lCount>0 then
    AddLn('');
end;

procedure TTypeCodeGenerator.Execute(aData: TSchemaData);

var
  I: integer;

begin
  FData := aData;
  FGenerated:=TFPObjectHashTable.Create(False);
  GenerateHeader;
  try
    Addln('unit %s;', [OutputUnitName]);
    Addln('');
    GenerateFPCDirectives();
    Addln('');
    Addln('interface');
    Addln('');
    if DelphiCode then
      AddLn('uses System.Types;')
    else
      AddLn('uses types;');
    Addln('');
    EnsureSection(csType);
    Addln('');
    indent;
    if WriteClassType then
      GenerateClassForwardTypes(aData);
    GenerateIntegerTypes(aData);
    GenerateStringTypes(aData);
    GeneratePascalArrayTypes(aData);
    GenerateClassTypes(aData);
    undent;
    Addln('implementation');
    Addln('');
    if WriteClassType then
      for I := 0 to aData.TypeCount-1 do
        begin
        if (aData.Types[I].PascalType in [ptSchemaStruct,ptAnonStruct])
           and aData.Types[I].HasObjectProperty(True) then
          begin
          DoLog('Generating type %s constructor', [aData.Types[I].PascalName]);
          WriteDtoConstructor(aData.Types[I]);
          end;
        if TrackChanges and (aData.Types[I].PascalType in [ptSchemaStruct,ptAnonStruct]) then
          WriteDtoTrackingImplementation(aData.Types[I]);
        end;
    Addln('end.');
  finally
    FData := nil;
  end;
end;


{ TSerializerCodeGenerator }

function TSerializerCodeGenerator.QualifyTypeName(const aTypeName: string): string;
begin
  // Only qualify reserved type names to avoid conflicts with standard library types
  if Assigned(TypeData) and TypeData.IsReservedTypeName(aTypeName) then
    Result := TypeData.GetQualifiedTypeName(aTypeName, DataUnitName)
  else
    Result := aTypeName;
end;

function TSerializerCodeGenerator.MustSerializeType(aType: TPascalTypeData): boolean;
begin
  Result:=Assigned(aType);
end;

function TSerializerCodeGenerator.IsReadOnly(aProperty: TPascalPropertyData): boolean;

begin
  Result:=Assigned(aProperty.Schema)
          and Assigned(aProperty.Schema.MetaData)
          and aProperty.Schema.MetaData.ReadOnly;
end;


function TSerializerCodeGenerator.DeserializeLocalName(aProperty: TPascalPropertyData): string;

begin
  Result:='lF'+aProperty.PascalName;
end;


function TSerializerCodeGenerator.NeedsDeserializeLocal(aProperty: TPascalPropertyData): boolean;

begin
  Result:=UseProperties and WriteClassType
          and (aProperty.PropertyType in [ptEnum,ptArray])
          and (aProperty.PascalTypeName<>'');
end;


function TSerializerCodeGenerator.IsDateOnly(aProperty: TPascalPropertyData): boolean;

begin
  Result:=(aProperty.PropertyType=ptDateTime)
          and Assigned(aProperty.Schema)
          and SameText(aProperty.Schema.Validations.Format,SFmtDate);
end;


function TSerializerCodeGenerator.FieldToJSON(aProperty: TPascalPropertyData): string;

begin
  if IsDateOnly(aProperty) then
    Result:=Format('DateOnlyToISO8601(%s)', [aProperty.PascalName])
  else
    Result:=FieldToJSON(aProperty.PropertyType,aProperty.PascalName)
end;


function TSerializerCodeGenerator.FieldToJSON(aType: TPascalType; aFieldName : String): string;

begin
  if aType in [ptAnonStruct,ptSchemaStruct] then
  begin
    Result := Format('%s.SerializeObject', [aFieldName]);
  end
  else
  begin
    case aType of
      ptBoolean:
        if DelphiCode then
          Result := Format('TJSONBool.Create(%s)', [aFieldName])
        else
          Result := aFieldName;
      ptJSON:
        if DelphiCode then
          Result := Format('TJSONObject.ParseJSONValue(%s,True,True)', [aFieldName])
        else
          Result := Format('GetJSON(%s)', [aFieldName]);
      ptDateTime :
        Result := Format('DateToISO8601(%s,%s)', [aFieldName,Bools[Not ConvertUTC]]);
      ptEnum :
        Result := Format('%s.AsString', [aFieldName]);
      ptArray:
        Result := Format('%s.SerializeArray', [aFieldName]);
    else
      Result := aFieldName;
    end;
  end;
end;


function TSerializerCodeGenerator.JSONToField(aProperty : TPascalPropertyData): string;

begin
  Result:=JSONToField(aProperty.PropertyType,aProperty.TypeNames[ntPascal], aProperty.SchemaName);
end;


function TSerializerCodeGenerator.JSONToField(aType: TPascalType; const aPropertyTypeName: string; const aKeyName: string): string;

  function ObjectField(lName: string) : string;
  begin
    if DelphiCode then
      Result := Format('aJSON.GetValue<TJSONObject>(''%s'',Nil)', [lName])
    else
      Result := Format('aJSON.Get(''%s'',TJSONObject(Nil))', [lName]);
  end;

  function ArrayField(lName: string) : string;
  begin
    if DelphiCode then
      Result := Format('aJSON.GetValue<TJSONArray>(''%s'',Nil)', [lName])
    else
      Result := Format('aJSON.Get(''%s'',TJSONArray(Nil))', [lName]);
  end;

var
  lPropType,
  lPasDefault: string;

begin
  if aType in [ptSchemaStruct,ptAnonStruct] then
  begin
    Result := Format('%s.Deserialize(%s)', [QualifyTypeName(aPropertyTypeName), ObjectField(aKeyName)]);
  end
  else if aType = ptArray then
  begin
    Result := Format('%s.Deserialize(%s)', [QualifyTypeName(aPropertyTypeName), ArrayField(aKeyName)]);
  end
  else
  begin
    case aType of
      ptString,
      ptFloat32,
      ptFloat64,
      ptDateTime,
      ptEnum,
      ptInteger,
      ptInt64,
      ptBoolean:
      begin
        if aType=ptDateTime then
          lPropType:='string'
        else
          lPropType:=aPropertyTypeName;
        lPasDefault:=GetJSONDefault(aType);
        if DelphiCode then
          Result := Format('aJSON.GetValue<%s>(''%s'',%s)', [lPropType, aKeyName, lPasDefault])
        else
          Result := Format('aJSON.Get(''%s'',%s)', [aKeyName, lPasDefault]);
      end;
      ptJSON:
        Result := 'JSONDataAsString('+ObjectField(aKeyName)+')';
    else
      Result := aKeyName;
    end;
  end;
end;


function TSerializerCodeGenerator.ArrayMemberToField(aType: TPascalType; const aPropertyTypeName : String; const aFieldName: string): string;

  function getStdType : string;
  begin
    case aType of
      ptFloat32,
      ptFloat64: Result:='Float';
      ptString : Result:='string';
      ptInteger : Result:='Integer';
      ptInt64 : Result:='Int64';
      ptBoolean : Result:='Boolean';
    end;
  end;

var
  lType,lPasDefault: string;

begin
  if aType in [ptAnonStruct,ptSchemaStruct] then
    Result := Format('%s.Deserialize(%s as TJSONObject)', [QualifyTypeName(aPropertyTypeName), aFieldName])
  else if aType = ptArray then
    Result := Format('%s.Deserialize(%s as TJSONArray)', [QualifyTypeName(aPropertyTypeName), aFieldName])
  else
    begin
    case aType of
      ptEnum:
        begin
        lPasDefault:=GetJSONDefault(aType);
        if DelphiCode then
          Result := Format('%s.GetValue<String>('''',%s)', [aFieldName, lPasDefault])
        else
          Result := Format('%s.AsString', [aFieldName]);
        end;
      ptDateTime:
        Result := Format('%s.AsString', [aFieldName]);
      ptFloat32,
      ptFloat64,
      ptString,
      ptInteger,
      ptInt64,
      ptBoolean:
      begin
        lType:=GetStdType;
        lPasDefault:=GetJSONDefault(aType);
        if DelphiCode then
          Result := Format('%s.GetValue<%s>('''',%s)', [aFieldName, lType, lPasDefault])
        else
          Result := Format('%s.As%s', [aFieldName, lType]);
      end;
      ptJSON,
      ptAnonStruct:
      begin
        if DelphiCode then
          Result := Format('%s.ToJSON', [aFieldName])
        else
          Result := Format('%s.AsJSON', [aFieldName]);
      end;
    else
      Result := aFieldName;
    end;
  end;
end;


procedure TSerializerCodeGenerator.WriteFieldSerializer(aType : TPascalTypeData; aProperty: TPascalPropertyData; aIndex: Integer);

var
  lAssign, lValue, lKeyName, lFieldName, lCondition: string;
  lType: TPascalType;

  procedure AddCondition(const aCondition: string);

  begin
    if lCondition='' then
      lCondition:=aCondition
    else
      lCondition:=lCondition+' and '+aCondition;
  end;

begin
  lKeyName := aProperty.SchemaName;
  lFieldName := aProperty.PascalName;
  lValue := FieldToJSON(aProperty);
  lType:=aProperty.PropertyType;
  lCondition:='';
  case lType of
    ptEnum:
      AddCondition(Format('(%s<>%s._empty_)',[lFieldName,aProperty.PascalTypeName]));
    ptDateTime:
      AddCondition(Format('(%s<>0)',[lFieldName]));
    ptJSON:
      if WriteClassType then
        AddCondition(Format('(%s<>'''')',[lFieldName]));
    ptAnonStruct,
    ptSchemaStruct:
      if WriteClassType then
        AddCondition(Format('Assigned(%s)',[lFieldName]));
  end;
  if TrackChanges and WriteClassType then
    AddCondition(Format('FieldChanged(%d)',[aIndex]));
  case lType of
    ptEnum,
    ptDatetime,
    ptInteger,
    ptInt64,
    ptString,
    ptBoolean,
    ptFloat32,
    ptFloat64,
    ptJSON,
    ptAnonStruct,
    ptSchemaStruct:
    begin
      if lCondition<>'' then
        begin
        AddLn('if %s then',[lCondition]);
        indent;
        end;
      if DelphiCode then
        Addln('Result.AddPair(''%s'',%s);', [lKeyName, lValue])
      else
        Addln('Result.Add(''%s'',%s);', [lKeyName, lValue]);
      if lCondition<>'' then
        undent;
    end;
    ptArray:
    begin
      if lCondition<>'' then
        begin
        AddLn('if %s then',[lCondition]);
        indent;
        Addln('begin');
        end;
      Addln('Arr:=TJSONArray.Create;');
      if DelphiCode then
        Addln('Result.AddPair(''%s'',Arr);', [lKeyName])
      else
        Addln('Result.Add(''%s'',Arr);', [lKeyName]);
      lAssign := Format('%s[i]', [lFieldName]);
      lAssign := FieldToJSON(aProperty.ElementType, lAssign);
      Addln('For I:=0 to Length(%s)-1 do', [lFieldName]);
      indent;
      Addln('Arr.Add(%s);', [lAssign]);
      undent;
      if lCondition<>'' then
        begin
        Addln('end;');
        undent;
        end;
    end;
    else
      DoLog('Unknown type for property %s', [aProperty.PascalName]);
  end;
end;


procedure TSerializerCodeGenerator.WriteFieldDeSerializer(aType: TPascalTypeData; aProperty: TPascalPropertyData);

var
  lElName, lValue, lKeyName, lFieldName, lLocal: string;
  lUseLocal: Boolean;

begin
  lKeyName := aProperty.SchemaName;
  lFieldName := aProperty.PascalName;
  lUseLocal := NeedsDeserializeLocal(aProperty);
  lLocal := DeserializeLocalName(aProperty);
  if aProperty.PropertyType<>ptArray then
    lValue := JSONToField(aProperty)
  else
    lValue := ArrayMemberToField(aProperty.ElementType,aProperty.ElementTypeName,'lArr[i]');
  case aProperty.PropertyType of
    ptEnum :
      if lUseLocal then
        begin
        Addln('%s.AsString:=%s;', [lLocal, lValue]);
        Addln('Result.%s:=%s;', [lFieldName, lLocal]);
        end
      else
        Addln('Result.%s.AsString:=%s;', [lFieldName, lValue]);
    ptDateTime:
      if IsDateOnly(aProperty) then
        Addln('Result.%s:=ISO8601ToDateOnlyDef(%s,0);', [lFieldName, lValue])
      else
        Addln('Result.%s:=ISO8601ToDateDef(%s,0,%s);', [lFieldName, lValue, Bools[Not ConvertUTC]]);
    ptInteger,
    ptInt64,
    ptFloat32,
    ptFloat64,
    ptString,
    ptBoolean,
    ptAnonStruct,
    ptJSON,
    ptSchemaStruct:
      Addln('Result.%s:=%s;', [lFieldName, lValue]);
    ptArray:
    begin
      if DelphiCode then
        Addln('lArr:=aJSON.GetValue<TJSONArray>(''%s'',Nil);', [lKeyName])
      else
        Addln('lArr:=aJSON.Get(''%s'',TJSONArray(Nil));', [lKeyName]);
      Addln('if Assigned(lArr) then');
      indent;
      Addln('begin');
      if lUseLocal then
        begin
        Addln('SetLength(%s,lArr.Count);', [lLocal]);
        Addln('For I:=0 to Length(%s)-1 do', [lLocal]);
        indent;
        Addln('%s[i]:=%s;', [lLocal, lValue]);
        undent;
        Addln('Result.%s:=%s;', [lFieldName, lLocal]);
        end
      else
        begin
        Addln('SetLength(Result.%s,lArr.Count);', [lFieldName]);
        lElName := Format('%s[i]', [lFieldName]);
        Addln('For I:=0 to Length(Result.%s)-1 do', [lFieldName]);
        indent;
        Addln('Result.%s:=%s;', [lElName, lValue]);
        undent;
        end;
      Addln('end;');
      undent;
    end;
    else
      DoLog('Unknown type for property %s', [aProperty.PascalName]);
  end;
end;


procedure TSerializerCodeGenerator.WriteDtoObjectSerializer(aType: TPascalTypeData);

var
  I: integer;
  lName: string;

begin
  lName := aType.SerializerName;
  Addln('function %s.SerializeObject : TJSONObject;', [lName]);
  Addln('');
  if aType.HasArrayProperty then
  begin
    Addln('var');
    indent;
    Addln('i : integer;');
    Addln('Arr : TJSONArray;');
    undent;
    Addln('');
  end;
  Addln('begin');
  indent;
  Addln('Result:=TJSONObject.Create;');
  Addln('try');
  indent;
  for I := 0 to aType.PropertyCount-1 do
    if not (SkipReadOnly and IsReadOnly(aType.Properties[I])) then
      WriteFieldSerializer(aType, aType.Properties[I], I);
  undent;
  Addln('except');
  indent;
  Addln('Result.Free;');
  Addln('raise;');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');
end;


procedure TSerializerCodeGenerator.WriteDtoSerializer(aType: TPascalTypeData);

var
  lName: string;

begin
  lName := aType.SerializerName;
  Addln('function %s.Serialize : String;', [lName]);
  Addln('var');
  indent;
  Addln('lObj : TJSONObject;');
  undent;
  Addln('begin');
  indent;
  Addln('lObj:=SerializeObject;');
  Addln('try');
  indent;
  if DelphiCode then
    Addln('Result:=lObj.ToJSON;')
  else
    Addln('Result:=lObj.AsJSON;');
  undent;
  Addln('finally');
  indent;
  Addln('lObj.Free');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');
end;


procedure TSerializerCodeGenerator.WriteDtoObjectDeserializer(aType: TPascalTypeData);

var
  I: integer;
  lHasArray, lHasLocals: boolean;

begin
  Addln('class function %s.Deserialize(aJSON : TJSONObject) : %s;', [aType.SerializerName, QualifyTypeName(aType.PascalName)]);
  Addln('');
  lHasArray := aType.HasArrayProperty;
  lHasLocals := False;
  for I := 0 to aType.PropertyCount-1 do
    if NeedsDeserializeLocal(aType.Properties[I]) then
      lHasLocals := True;
  if lHasArray or lHasLocals then
  begin
    Addln('var');
    indent;
    if lHasArray then
    begin
      Addln('lArr : TJSONArray;');
      Addln('i : Integer;');
    end;
    for I := 0 to aType.PropertyCount-1 do
      if NeedsDeserializeLocal(aType.Properties[I]) then
        Addln('%s : %s;', [DeserializeLocalName(aType.Properties[I]), aType.Properties[I].PascalTypeName]);
    undent;
  end;
  undent;
  Addln('begin');
  indent;
  if WriteClassType then
    Addln('Result := %s.Create;', [QualifyTypeName(aType.PascalName)])
  else
    Addln('Result := Default(%s);', [QualifyTypeName(aType.PascalName)]);
  Addln('If (aJSON=Nil) then');
  indent;
  Addln('exit;');
  undent;
  for I := 0 to aType.PropertyCount-1 do
    WriteFieldDeSerializer(aType, aType.Properties[I]);
  if TrackChanges and WriteClassType then
    Addln('Result.ClearChanges;');
  undent;
  Addln('end;');
  Addln('');
end;


procedure TSerializerCodeGenerator.WriteDtoDeserializer(aType: TPascalTypeData);

begin
  Addln('class function %s.Deserialize(aJSON : String) : %s;', [aType.SerializerName, QualifyTypeName(aType.PascalName)]);
  Addln('');
  Addln('var');
  indent;
  Addln('lObj : TJSONObject;');
  undent;
  Addln('begin');
  indent;
  Addln('Result := Default(%s);', [QualifyTypeName(aType.PascalName)]);
  Addln('if (aJSON='''') then');
  indent;
  Addln('exit;');
  undent;
  if DelphiCode then
    Addln('lObj := TJSONObject.ParseJSONValue(aJSON,True,True) as TJSONObject;')
  else
    Addln('lObj := GetJSON(aJSON) as TJSONObject;');
  Addln('if (lObj = nil) then');
  indent;
  Addln('exit;');
  undent;
  Addln('try');
  indent;
  Addln('Result:=Deserialize(lObj);');
  undent;
  Addln('finally');
  indent;
  Addln('lObj.Free');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');
end;


procedure TSerializerCodeGenerator.WriteDtoHelper(aType: TPascalTypeData);

begin
  if WriteClassType then
    Addln('%s = class helper for %s', [aType.SerializerName, QualifyTypeName(aType.PascalName)])
  else
  if DelphiCode then
    Addln('%s = record helper for %s', [aType.SerializerName, QualifyTypeName(aType.PascalName)])
  else
    Addln('%s = type helper for %s', [aType.SerializerName, QualifyTypeName(aType.PascalName)]);
  indent;
  if stSerialize in aType.SerializeTypes then
  begin
    Addln('function SerializeObject : TJSONObject;');
    Addln('function Serialize : String;');
  end;
  if stDeserialize in aType.SerializeTypes then
  begin
    Addln('class function Deserialize(aJSON : TJSONObject) : %s; overload; static;', [QualifyTypeName(aType.PascalName)]);
    Addln('class function Deserialize(aJSON : String) : %s; overload; static;', [QualifyTypeName(aType.PascalName)]);
  end;
  undent;
  Addln('end;');
end;

procedure TSerializerCodeGenerator.WriteArrayHelper(aType: TPascalTypeData);

begin
  if DelphiCode then
    Addln('%s = record helper for %s', [aType.SerializerName, QualifyTypeName(aType.PascalName)])
  else
    Addln('%s = type helper for %s', [aType.SerializerName, QualifyTypeName(aType.PascalName)]);
  Indent;
  if stSerialize in aType.SerializeTypes then
    begin
    Addln('function SerializeArray : TJSONArray;');
    Addln('function Serialize : String;');
    end;
  if stDeserialize in aType.SerializeTypes then
    begin
    Addln('class function Deserialize(aJSON : TJSONArray) : %s; overload; static;', [QualifyTypeName(aType.PascalName)]);
    Addln('class function Deserialize(aJSON : String) : %s; overload; static;', [QualifyTypeName(aType.PascalName)]);
    end;
  undent;
  Addln('end;');
end;

procedure TSerializerCodeGenerator.WriteArrayHelperSerializeArray(aType: TPascalTypeData);
var
  lSerializeCall : String;
begin
  Addln('');
  Addln('function %s.SerializeArray : TJSONArray;',[aType.SerializerName]);
  Addln('var');
  indent;
  Addln('I : Integer;');
  undent;
  Addln('begin');
  indent;
  Addln('Result:=TJSONArray.Create;');
  Addln('try');
  indent;
  Addln('For I:=0 to length(Self)-1 do');
  Indent;
  if aType.ElementTypeData.Pascaltype in [ptSchemaStruct,ptAnonStruct] then
    lSerializeCall:='.SerializeObject'
  else  if aType.ElementTypeData.Pascaltype=ptArray then
    lSerializeCall:='.SerializeArray'
  else if aType.ElementTypeData.schema=Nil then
    lSerializeCall:=''
  else
    Raise EJSONSchema.CreateFmt('Cannot decide how to serialize %',[aType.ElementTypeData.PascalName]);
  Addln('Result.Add(self[i]%s);',[lSerializeCall]);
  undent;
  undent;
  Addln('except');
  indent;
  Addln('Result.Free;');
  Addln('raise;');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');
end;

procedure TSerializerCodeGenerator.WriteArrayHelperSerialize(aType: TPascalTypeData);
begin
  Addln('');
  Addln('function %s.Serialize : String;',[aType.SerializerName]);
  Addln('var');
  indent;
  Addln('lObj : TJSONArray;');
  undent;
  Addln('begin');
  indent;
  Addln('lObj:=SerializeArray;');
  Addln('try');
  indent;
  if DelphiCode then
    Addln('Result:=lObj.ToJSON;')
  else
    Addln('Result:=lObj.AsJSON;');
  undent;
  Addln('finally');
  indent;
  Addln('lObj.Free');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');
end;

procedure TSerializerCodeGenerator.WriteArrayHelperDeSerializeArray(aType: TPascalTypeData);
var
  lType : string;
begin
  Addln('class function %s.Deserialize(aJSON : TJSONArray) : %s; ', [aType.SerializerName, QualifyTypeName(aType.PascalName)]);
  Addln('');
  Addln('var');
  indent;
  Addln('i : integer;');
  undent;
  Addln('begin');
  indent;
  Addln('SetLength(Result,aJSON.Count);');
  Addln('For i:=0 to aJSON.Count-1 do');
  indent;
  lType:=ArrayMemberToField(aType.ElementTypeData.Pascaltype,aType.ElementTypeData.PascalName,'aJSON[i]');
  Addln('Result[i]:=%s;',[lType]);
  undent;
  undent;
  Addln('end;');
  Addln('');
end;

procedure TSerializerCodeGenerator.WriteArrayHelperDeserialize(aType: TPascalTypeData);
begin
  Addln('class function %s.Deserialize(aJSON : String) : %s; ', [aType.SerializerName, QualifyTypeName(aType.PascalName)]);
  Addln('');
  Addln('var');
  indent;
  Addln('lObj : TJSONData;');
  Addln('lArr : TJSONArray absolute lobj;');
  undent;
  Addln('begin');
  indent;
  Addln('lObj:=GetJSON(aJSON);');
  Addln('try');
  indent;
  Addln('Result:=DeSerialize(lArr);');
  undent;
  Addln('finally');
  indent;
  Addln('lObj.Free;');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');

end;


procedure TSerializerCodeGenerator.WriteArrayHelperImpl(aType: TPascalTypeData);

begin
  if stSerialize in aType.SerializeTypes then
    begin
    WriteArrayHelperSerializeArray(aType);
    WriteArrayHelperSerialize(aType);
    end;
  if stDeserialize in aType.SerializeTypes then
    begin
    WriteArrayHelperDeserializeArray(aType);
    WriteArrayHelperDeserialize(aType);
    end;
end;


procedure TSerializerCodeGenerator.GenerateConverters;

begin
  Addln('function ISO8601ToDateDef(S: String; aDefault : TDateTime; aConvertUTC: Boolean = True) : TDateTime;');
  Addln('');
  Addln('begin');
  indent;
  Addln('if (S='''') then');
  indent;
  Addln('Exit(aDefault);');
  undent;
  Addln('try');
  indent;
  AddLn('Result:=ISO8601ToDate(S,aConvertUTC);');
  undent;
  Addln('except');
  indent;
  Addln('Result:=aDefault;');
  undent;
  Addln('end;');
  undent;
  Addln('end;');
  Addln('');
  Addln('function ISO8601ToDateOnlyDef(S: String; aDefault : TDateTime) : TDateTime;');
  Addln('');
  Addln('begin');
  indent;
  Addln('Result:=DateOf(ISO8601ToDateDef(S,aDefault,True));');
  undent;
  Addln('end;');
  Addln('');
  Addln('function DateOnlyToISO8601(aDate : TDateTime) : String;');
  Addln('');
  Addln('begin');
  indent;
  Addln('Result:=FormatDateTime(''yyyy"-"mm"-"dd'',aDate);');
  undent;
  Addln('end;');
  Addln('');
  if DelphiCode then
    Addln('function JSONDataAsString(aData: TJSONValue) : String;')
  else
    Addln('function JSONDataAsString(aData: TJSONData) : String;');
  Addln('');
  Addln('begin');
  indent;
  Addln('if aData=Nil then');
  indent;
  Addln('Result:=''''');
  undent;
  Addln('else');
  indent;
  if DelphiCode then
    Addln('Result:=aData.ToJSON;')
  else
    Addln('Result:=aData.AsJSON;');
  undent;
  undent;
  Addln('end;');
  Addln('');
end;

procedure TSerializerCodeGenerator.Execute(aData: TSchemaData);

var
  I: integer;
  lType: TPascalTypeData;

begin
  FData := aData;
  GenerateHeader;
  try
    Addln('unit %s;', [OutputUnitName]);
    Addln('');
    Addln('interface');
    Addln('');
    GenerateFPCDirectives(['typehelpers']);
    Addln('');
    Addln('uses');
    indent;
    if DelphiCode then
      begin
      AddLn('System.Types,');
      Addln('System.JSON,')
      end
    else
      begin
      AddLn('Types,');
      Addln('fpJSON,');
      end;
    Addln(DataUnitName+';');
    undent;
    Addln('');
    EnsureSection(csType);
    indent;
    for I := 0 to aData.TypeCount-1 do
    begin
      lType := aData.Types[I];
      if MustSerializeType(lType) then
        with lType do
          if Pascaltype in [ptSchemaStruct,ptAnonStruct] then
            begin
            DoLog('Generating serialization helper type %s for Dto %s', [SerializerName, PascalName]);
            WriteDtoHelper(lType);
            Addln('');
            end
          else if Pascaltype=ptArray then
            begin
            // For arrays of simple types, we need to generate code to read/write the array
            if (ElementTypeData.Pascaltype=ptArray) and (ElementTypeData.Schema=Nil) then
              begin
              WriteArrayHelper(ElementTypeData);
              end;
            WriteArrayHelper(lType);
            end;
    end;
    undent;
    Addln('implementation');
    Addln('');
    if DelphiCode then
      Addln('uses System.Generics.Collections, System.SysUtils, System.DateUtils, System.StrUtils;')
    else
      Addln('uses Generics.Collections, SysUtils, DateUtils, StrUtils;');
    Addln('');
    GenerateConverters;
    for I := 0 to aData.TypeCount-1 do
    begin
      lType := aData.Types[I];
      if MustSerializeType(lType) then
      begin
        if LType.Pascaltype in [ptSchemaStruct,ptAnonStruct] then
          begin
          if stSerialize in lType.SerializeTypes then
          begin
            WriteDtoObjectSerializer(aData.Types[I]);
            WriteDtoSerializer(aData.Types[I]);
          end;
          if stDeserialize in lType.SerializeTypes then
          begin
            WriteDtoObjectDeserializer(aData.Types[I]);
            WriteDtoDeserializer(aData.Types[I]);
          end;
          end
        else if lType.Pascaltype=ptArray then
          begin
          // For arrays of simple types, we need to generate code to read/write the array
          if (lType.ElementTypeData.Pascaltype=ptArray) and (lType.ElementTypeData.Schema=Nil) then
            begin
            WriteArrayHelperImpl(lType.ElementTypeData);
            end;
          WriteArrayHelperImpl(lType);
          end;
      end;
    end;
    Addln('');
    Addln('end.');
  finally
    FData := nil;
  end;
end;

end.

