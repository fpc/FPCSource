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
  fpjson.schema.Pascaltypes;

Type

  { TJSONSchemaCodeGen }

  { TJSONSchemaCodeGenerator }

  TJSONSchemaCodeGenerator = class(TPascalCodeGenerator)
  private
    FData: TSchemaData;
    FDelphiCode: boolean;
    FVerboseHeader: Boolean;
    FWriteClassType: boolean;
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
  protected
    function MustSerializeType(aType : TPascalTypeData) : boolean; virtual;
    function FieldToJSON(aProperty: TPascalPropertyData) : string; virtual;
    function ArrayMemberToField(aType: TPascalType; const aPropertyTypeName: String; const aFieldName: string): string; virtual;
    function FieldToJSON(aType: TPascalType; aFieldName: String): string; virtual;
    procedure GenerateConverters; virtual;
    function JSONToField(aProperty: TPascalPropertyData) : string; virtual;
    function JSONToField(aType: TPascalType; const aPropertyTypeName: string; const aKeyName: string): string; virtual;
    procedure WriteFieldDeSerializer(aType : TPascalTypeData; aProperty: TPascalPropertyData); virtual;
    procedure WriteFieldSerializer(aType : TPascalTypeData; aProperty: TPascalPropertyData); virtual;
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
  if lTypeName = '' then
    Addln('// Unknown type for field %s...', [lFieldName])
  else
    Addln('%s : %s;', [lFieldName, lTypeName]);
end;


procedure TTypeCodeGenerator.WriteDtoConstructor(aType: TPascalTypeData);

var
  I : Integer;
  lProp : TPascalPropertyData;
  lConstructor : String;

begin
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
      AddLn('%s := %s.%s;',[lProp.PascalName,lProp.TypeData.PascalName,lConstructor]);
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
  False: Boolean;

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
        end;
    Addln('end.');
  finally
    FData := nil;
  end;
end;


{ TSerializerCodeGenerator }

function TSerializerCodeGenerator.MustSerializeType(aType: TPascalTypeData): boolean;
begin
  Result:=Assigned(aType);
end;

function TSerializerCodeGenerator.FieldToJSON(aProperty: TPascalPropertyData): string;

begin
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
    Result := Format('%s.Deserialize(%s)', [aPropertyTypeName, ObjectField(aKeyName)]);
  end
  else if aType = ptArray then
  begin
    Result := Format('%s.Deserialize(%s)', [aPropertyTypeName, ArrayField(aKeyName)]);
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
      begin
        if DelphiCode then
          Result := ObjectField(aKeyName)+'.ToJSON'
        else
          Result := ObjectField(aKeyName)+'.AsJSON';
      end;
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
    Result := Format('%s.Deserialize(%s as TJSONObject)', [aPropertyTypeName, aFieldName])
  else if aType = ptArray then
    Result := Format('%s.Deserialize(%s as TJSONArray)', [aPropertyTypeName, aFieldName])
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


procedure TSerializerCodeGenerator.WriteFieldSerializer(aType : TPascalTypeData; aProperty: TPascalPropertyData);

var
  lAssign, lValue, lKeyName, lFieldName: string;
  lType: TPascalType;
  lNilCheck : Boolean;

begin
  lKeyName := aProperty.SchemaName;
  lFieldName := aProperty.PascalName;
  lValue := FieldToJSON(aProperty);
  lType:=aProperty.PropertyType;
  lNilCheck:=WriteClassType and (lType in [ptJSON,ptAnonStruct,ptSchemaStruct]);
  case lType of
    ptEnum:
      begin
      Addln('if (%s<>%s._empty_) then',[lFieldName,aProperty.PascalTypeName]);
      indent;
      if DelphiCode then
        Addln('Result.AddPair(''%s'',%s);', [lKeyName, lValue])
      else
        Addln('Result.Add(''%s'',%s);', [lKeyName, lValue]);
      undent;
      end;
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
      if lNilCheck then
        begin
        if (lType=ptJSON) then
          // JSON string...
          AddLn('if (%s<>'''') then',[lFieldName])
        else
          AddLn('if Assigned(%s) then',[lFieldName]);
        indent;
        end;
      if DelphiCode then
        Addln('Result.AddPair(''%s'',%s);', [lKeyName, lValue])
      else
        Addln('Result.Add(''%s'',%s);', [lKeyName, lValue]);
      if lNilCheck then
        undent;
    end;
    ptArray:
    begin
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
    end;
    else
      DoLog('Unknown type for property %s', [aProperty.PascalName]);
  end;
end;


procedure TSerializerCodeGenerator.WriteFieldDeSerializer(aType: TPascalTypeData; aProperty: TPascalPropertyData);

var
  lElName, lValue, lKeyName, lFieldName: string;

begin
  lKeyName := aProperty.SchemaName;
  lFieldName := aProperty.PascalName;
  if aProperty.PropertyType<>ptArray then
    lValue := JSONToField(aProperty)
  else
    lValue := ArrayMemberToField(aProperty.ElementType,aProperty.ElementTypeName,'lArr[i]');
  case aProperty.PropertyType of
    ptEnum :
      Addln('Result.%s.AsString:=%s;', [lFieldName, lValue]);
    ptDateTime:
      begin
      Addln('Result.%s:=ISO8601ToDateDef(%s,0,%s);', [lFieldName, lValue, Bools[Not ConvertUTC]]);
      end;
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
      Addln('SetLength(Result.%s,lArr.Count);', [lFieldName]);
      lElName := Format('%s[i]', [lFieldName]);

      Addln('For I:=0 to Length(Result.%s)-1 do', [lFieldName]);
      indent;
      Addln('Result.%s:=%s;', [lElName, lValue]);
      undent;
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
    WriteFieldSerializer(aType, aType.Properties[I]);
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
  lHasArray: boolean;

begin
  Addln('class function %s.Deserialize(aJSON : TJSONObject) : %s;', [aType.SerializerName, aType.PascalName]);
  Addln('');
  lHasArray := aType.HasArrayProperty;
  //  lHasObject:=aType.HasObjectProperty(True);
  if lHasArray then
  begin
    Addln('var');
    indent;
    if lHasArray then
    begin
      Addln('lArr : TJSONArray;');
      Addln('i : Integer;');
    end;
    undent;
  end;
  undent;
  Addln('begin');
  indent;
  if WriteClassType then
    Addln('Result := %s.Create;', [aType.PascalName])
  else
    Addln('Result := Default(%s);', [aType.PascalName]);
  Addln('If (aJSON=Nil) then');
  indent;
  Addln('exit;');
  undent;
  for I := 0 to aType.PropertyCount-1 do
    WriteFieldDeSerializer(aType, aType.Properties[I]);
  undent;
  Addln('end;');
  Addln('');
end;


procedure TSerializerCodeGenerator.WriteDtoDeserializer(aType: TPascalTypeData);

begin
  Addln('class function %s.Deserialize(aJSON : String) : %s;', [aType.SerializerName, aType.PascalName]);
  Addln('');
  Addln('var');
  indent;
  Addln('lObj : TJSONObject;');
  undent;
  Addln('begin');
  indent;
  Addln('Result := Default(%s);', [aType.PascalName]);
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
    Addln('%s = class helper for %s', [aType.SerializerName, aType.PascalName])
  else
  if DelphiCode then
    Addln('%s = record helper for %s', [aType.SerializerName, aType.PascalName])
  else
    Addln('%s = type helper for %s', [aType.SerializerName, aType.PascalName]);
  indent;
  if stSerialize in aType.SerializeTypes then
  begin
    Addln('function SerializeObject : TJSONObject;');
    Addln('function Serialize : String;');
  end;
  if stDeserialize in aType.SerializeTypes then
  begin
    Addln('class function Deserialize(aJSON : TJSONObject) : %s; overload; static;', [aType.PascalName]);
    Addln('class function Deserialize(aJSON : String) : %s; overload; static;', [aType.PascalName]);
  end;
  undent;
  Addln('end;');
end;

procedure TSerializerCodeGenerator.WriteArrayHelper(aType: TPascalTypeData);

begin
  if DelphiCode then
    Addln('%s = record helper for %s', [aType.SerializerName, aType.PascalName])
  else
    Addln('%s = type helper for %s', [aType.SerializerName, aType.PascalName]);
  Indent;
  if stSerialize in aType.SerializeTypes then
    begin
    Addln('function SerializeArray : TJSONArray;');
    Addln('function Serialize : String;');
    end;
  if stDeserialize in aType.SerializeTypes then
    begin
    Addln('class function Deserialize(aJSON : TJSONArray) : %s; overload; static;', [aType.PascalName]);
    Addln('class function Deserialize(aJSON : String) : %s; overload; static;', [aType.PascalName]);
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
  Addln('class function %s.Deserialize(aJSON : TJSONArray) : %s; ', [aType.SerializerName, aType.PascalName]);
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
  Addln('class function %s.Deserialize(aJSON : String) : %s; ', [aType.SerializerName,aType.PascalName]);
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

