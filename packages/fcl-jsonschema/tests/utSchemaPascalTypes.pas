unit utSchemaPascalTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, fpjson.schema.types, fpjson.schema.pascaltypes, fpjson.schema.schema;

Type

  { TTestSchemaPascalType }

  TTestSchemaPascalType = class(TTestCase)
  private
    FSchema: TJSONSchema;
    FSchemaData: TSchemaData;
    FType : TPascalTypeData;
    function DefineProperty(aName: string; aType: TSchemaSimpleType; aFormat: String='') : TJSONSchema;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure DoAddProperties;
    procedure AssertEquals(const Msg: string; aExpected, aActual: TPascalType); overload;
    function AssertPascalTypeData(const Msg: string; aType: TPascalTypeData; aPascaltype: TPascalType; const aSchemaName, aPascalName: String) : TPascalTypeData;
    procedure AssertProperty(Msg: String; aType: TPascalTypeData; aIndex: Integer; const aSchemaName, aPascalName, aPascalTypeName: String; aPascalType: TPascalType; aTypeData: TPascalTypeData);
    Property Schema : TJSONSchema Read FSchema;
    Property Data : TSchemaData Read FSchemaData;
  Published
    Procedure TestHookup;
    Procedure TestTypeMap;
    procedure TestCreatePascalType;
    Procedure TestAddType;
    Procedure TestAddTypeAlias;
    Procedure TestStandardTypes;
    Procedure TestAddPropertiesInteger;
    procedure TestAddPropertiesInt64;
    procedure TestAddPropertiesString;
    procedure TestAddPropertiesStringDate;
    procedure TestAddPropertiesStringDateTime;
    procedure TestAddPropertiesStringTime;
    procedure TestAddPropertiesStringNoEnum;
    procedure TestAddPropertiesStringEnum;
    procedure TestAddPropertiesNumber;
    procedure TestAddPropertiesBoolean;
    procedure TestAddPropertiesAny;
    procedure TestAddPropertiesEmptyObject;
    procedure TestAddPropertiesNone;
    procedure TestAddPropertiesNull;
    procedure TestAddPropertiesMulti;
    procedure TestAddPropertiesArray;
    procedure TestAddPropertiesArrayDelphi;
    procedure TestAddPropertiesArrayMultiValue;
    procedure TestAddPropertiesObject;
    procedure TestAddPropertiesObjectExisting;
  end;

implementation

uses TypInfo;

{ TTestSchemaPascalType }

procedure TTestSchemaPascalType.SetUp;
begin
  inherited SetUp;
  FSchemaData:=TSchemaData.Create;
  Fschema:=TJSONSchema.Create;
end;

procedure TTestSchemaPascalType.TearDown;
begin
  FreeAndNil(FSchema);
  FreeAndNil(FSchemaData);
  inherited TearDown;
end;

procedure TTestSchemaPascalType.AssertEquals(const Msg : string; aExpected, aActual : TPascalType);

begin
  AssertEquals(Msg,GetEnumName(Typeinfo(TPascalType),Ord(aExpected)),
                   GetEnumName(Typeinfo(TPascalType),Ord(aActual)));
end;

function TTestSchemaPascalType.AssertPascalTypeData(const Msg: string; aType: TPascalTypeData; aPascaltype: TPascalType;
  const aSchemaName, aPascalName: String): TPascalTypeData;
begin
  AssertNotNull(Msg+': have type',aType);
  AssertEquals(Msg+' Schema name',aSchemaName,aType.SchemaName);
  AssertEquals(Msg+' Pascal name',aPascalName,aType.PascalName);
  AssertEquals(Msg+' Pascal type',aPascalType,aType.PascalType);
  Result:=aType;
end;

procedure TTestSchemaPascalType.TestHookup;
begin
  AssertNotNull('Have schema',Data);
  AssertEquals('No types',0,Data.TypeCount);
end;

procedure TTestSchemaPascalType.TestTypeMap;
begin
  Data.AddAliasToTypeMap(ptInteger,'a','a','b',Nil);
  AssertEquals('Correct existing map','b',Data.TypeMap['a']);
  AssertEquals('Correct nonexisting map','c',Data.TypeMap['c']);
end;

procedure TTestSchemaPascalType.TestCreatePascalType;
var
  aType : TPascalTypeData;
  S : TJSONSchema;
begin
  S:=TJSONSchema.Create;
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',S);
  try
    AssertPascalTypeData('basic',aType,ptSchemaStruct,'a','Ta');
    AssertSame('Schema',S,aType.Schema);
    AssertEquals('Not added to types',0,Data.TypeCount);
  finally
    aType.Free;
    S.Free;
  end;
end;

procedure TTestSchemaPascalType.TestAddType;
var
  aType : TPascalTypeData;
begin
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Nil);
  Data.AddType('a',aType);
  AssertEquals('Not added to types',1,Data.TypeCount);
  AssertSame('Can find',aType,Data.FindSchemaTypeData('a'));
  AssertEquals('Can index',0,Data.IndexOfSchemaType('a'));
end;

procedure TTestSchemaPascalType.TestAddTypeAlias;
begin
  Data.AddAliasToTypeMap(ptInteger,'int','int','integer',Nil);
  AssertNotNull('Can find',Data.FindSchemaTypeData('int'));
  AssertEquals('Cannot index',-1,Data.IndexOfSchemaType('int'));
  AssertEquals('Not added to types',0,Data.Typecount);
end;

procedure TTestSchemaPascalType.TestStandardTypes;
begin
  Data.DefineStandardPascalTypes;
  AssertPascalTypeData('integer',Data.FindSchemaTypeData('integer'),ptInteger,'integer','integer');
  AssertPascalTypeData('int64',Data.FindSchemaTypeData('integer','int64'),ptInt64,'integer','int64');
  AssertPascalTypeData('string',Data.FindSchemaTypeData('string'),ptString,'string','string');
  AssertPascalTypeData('date-time',Data.FindSchemaTypeData('string--date-time'),ptDateTime,'string','TDateTime');
  AssertPascalTypeData('date',Data.FindSchemaTypeData('string--date'),ptDateTime,'string','TDateTime');
  AssertPascalTypeData('time',Data.FindSchemaTypeData('string--time'),ptDateTime,'string','TDateTime');
  AssertPascalTypeData('boolean',Data.FindSchemaTypeData('boolean'),ptBoolean,'boolean','boolean');
  AssertPascalTypeData('number',Data.FindSchemaTypeData('number'),ptFloat64,'number','double');
end;

procedure TTestSchemaPascalType.AssertProperty(Msg : String; aType : TPascalTypeData; aIndex : Integer; const aSchemaName,aPascalName,aPascalTypeName : String; aPascalType : TPascalType; aTypeData : TPascalTypeData);

var
  lProp : TPascalPropertyData;

begin
  AssertNotNull(Msg+': have type',aType);
  AssertTrue(Msg+': have properties',aType.PropertyCount>0);
  AssertTrue(Msg+': have valid index',aIndex<aType.PropertyCount);
  lProp:=aType.Properties[aIndex];
  AssertNotNull(Msg+': have property',lProp);
  AssertEquals(Msg+': schema name',aSchemaName,lProp.SchemaName);
  AssertEquals(Msg+': pascal name',aPascalName,lProp.PascalName);
  AssertEquals(Msg+': Pascal type name',aPascalTypeName,lProp.PascalTypeName);
  AssertEquals(Msg+': Pascal type',aPascalType,lProp.PropertyType);
  AssertSame(Msg+': Type data',aTypeData,lProp.TypeData);
end;

function TTestSchemaPascalType.DefineProperty(aName: string; aType: TSchemaSimpleType; aFormat: String): TJSONSchema;

begin
  Data.DefineStandardPascalTypes;
  Result:=TJSONSchema.Create;
  Result.Name:=aName;
  Result.Validations.Types:=[aType];
  if aFormat<>'' then
    Result.Validations.Format:=aFormat;
  Schema.Properties.Add(Result);
end;

procedure TTestSchemaPascalType.TestAddPropertiesInteger;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstInteger,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Integer',aType,0,'b','b','integer',ptInteger,Data.FindSchemaTypeData('integer'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesInt64;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstInteger,'int64');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Int64',aType,0,'b','b','int64',ptInt64,Data.FindSchemaTypeData('integer','int64'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesString;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstString,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('String',aType,0,'b','b','string',ptString,Data.FindSchemaTypeData('string'));
end;


procedure TTestSchemaPascalType.TestAddPropertiesStringDate;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstString,'date');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Date',aType,0,'b','b','TDateTime',ptDateTime,Data.FindSchemaTypeData('string','date'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesStringDateTime;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstString,'date-time');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Date',aType,0,'b','b','TDateTime',ptDateTime,Data.FindSchemaTypeData('string','date-time'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesStringTime;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstString,'time');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Date',aType,0,'b','b','TDateTime',ptDateTime,Data.FindSchemaTypeData('string','time'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesStringNoEnum;
var
  lProp : TJSONSchema;
  aType : TPascalTypeData;

begin
  lprop:=DefineProperty('b',sstString,'');
  lProp.Validations.Enum:=TJSONArray.Create(['de','fg','hi']);
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Enum',aType,0,'b','b','string',ptString,Data.FindSchemaTypeData('string'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesStringEnum;
var
  lProp : TJSONSchema;
  aType : TPascalTypeData;

begin
  Data.UseEnums:=True;
  lprop:=DefineProperty('b',sstString,'');
  lProp.Validations.Enum:=TJSONArray.Create(['de','fg','hi']);
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Enum',aType,0,'b','b','Ta_b',ptEnum,Data.FindSchemaTypeData('(a_b)'));
  AssertSame('Schema in property def',lProp,aType.Properties[0].Schema);
  AssertSame('Schema in property type def',lProp,Data.FindSchemaTypeData('(a_b)').Schema);
  AssertEquals('Have 2 public types',2,Data.TypeCount);
end;


procedure TTestSchemaPascalType.TestAddPropertiesNumber;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstNumber,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Number',aType,0,'b','b','double',ptFloat64,Data.FindSchemaTypeData('number'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesBoolean;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstBoolean,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertPascalTypeData('Ta',Data.FindSchemaTypeData('a'),ptSchemaStruct,'a','Ta');
  AssertProperty('Boolean',aType,0,'b','b','boolean',ptBoolean,Data.FindSchemaTypeData('boolean'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesAny;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstAny,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertProperty('any',aType,0,'b','b','string',ptJSON,Data.FindSchemaTypeData('any'));
end;

procedure TTestSchemaPascalType.TestAddPropertiesEmptyObject;
var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstObject,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertProperty('any',aType,0,'b','b','string',ptJSON,Data.FindSchemaTypeData('JSON'));
end;


procedure TTestSchemaPascalType.DoAddProperties;

begin
  Data.AddPropertiesToType(FType);
end;

procedure TTestSchemaPascalType.TestAddPropertiesNone;

var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstNone,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  FType:=aType;
  AssertException('type none not allowed', ESchemaData, @DoAddProperties);
end;

procedure TTestSchemaPascalType.TestAddPropertiesNull;
var
  aType : TPascalTypeData;

begin
  DefineProperty('b',sstNull,'');
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  FType:=aType;
  AssertException('type null not allowed', ESchemaData, @DoAddProperties);
end;

procedure TTestSchemaPascalType.TestAddPropertiesMulti;
var
  lProp : TJSONSchema;
  aType : TPascalTypeData;

begin
  lProp:=DefineProperty('b',sstString,'');
  lProp.Validations.Types:=[sstString,sstNumber];
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  FType:=aType;
  AssertException('multiple types not allowed', ESchemaData, @DoAddProperties);
end;

procedure TTestSchemaPascalType.TestAddPropertiesArray;

var
  lProp,lElement : TJSONSchema;
  aType : TPascalTypeData;

begin
  lProp:=DefineProperty('b',sstArray,'');
  lElement:=TJSONSchema.Create;
  lElement.Validations.Types:=[sstString];
  lProp.Items.Add(lElement);
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertProperty('array',aType,0,'b','b','Array of string',ptArray,Data.FindSchemaTypeData('[string]'));
  AssertSame('Schema in property def',lProp,aType.Properties[0].Schema);
  AssertSame('Schema in property type def',lProp,Data.FindSchemaTypeData('[string]').Schema);
  AssertEquals('Have 2 public types',2,Data.TypeCount);
end;

procedure TTestSchemaPascalType.TestAddPropertiesArrayDelphi;
var
  lProp,lElement : TJSONSchema;
  aType : TPascalTypeData;

begin
  Data.DelphiTypes:=True;
  lProp:=DefineProperty('b',sstArray,'');
  lElement:=TJSONSchema.Create;
  lElement.Validations.Types:=[sstString];
  lProp.Items.Add(lElement);
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertProperty('array',aType,0,'b','b','TArray<string>',ptArray,Data.FindSchemaTypeData('[string]'));
  AssertEquals('Have 2 public types',2,Data.TypeCount);
end;

procedure TTestSchemaPascalType.TestAddPropertiesArrayMultiValue;
var
  lProp,lElement : TJSONSchema;
  aType : TPascalTypeData;

begin
  lProp:=DefineProperty('b',sstArray,'');
  lElement:=TJSONSchema.Create;
  lElement.Validations.Types:=[sstString];
  lProp.Items.Add(lElement);
  lElement:=TJSONSchema.Create;
  lElement.Validations.Types:=[sstInteger];
  lProp.Items.Add(lElement);
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  FType:=aType;
  AssertException('multiple value types in array not allowed', ESchemaData, @DoAddProperties);
end;

procedure TTestSchemaPascalType.TestAddPropertiesObject;

var
  lProp,lElement : TJSONSchema;
  aType,aType2 : TPascalTypeData;

begin
  lProp:=DefineProperty('b',sstObject,'');
  lElement:=TJSONSchema.Create;
  lElement.Name:='c';
  lElement.Validations.Types:=[sstString];
  lProp.Properties.Add(lElement);
  lElement:=TJSONSchema.Create;
  lElement.Name:='d';
  lElement.Validations.Types:=[sstInteger];
  lProp.Properties.Add(lElement);
  aType:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType);
  Data.AddPropertiesToType(aType);
  AssertProperty('array',aType,0,'b','b','Ta_b',ptAnonStruct,Data.FindSchemaTypeData('{a_b}'));
  AssertSame('Schema in property def',lProp,aType.Properties[0].Schema);
  AssertSame('Schema in property type def',lProp,Data.FindSchemaTypeData('{a_b}').Schema);
  aType2:=Data.FindSchemaTypeData('{a_b}');
  AssertProperty('sub prop 1',aType2,0,'c','c','string',ptString,Data.FindSchemaTypeData('string'));
  AssertProperty('sub prop 2',aType2,1,'d','d','integer',ptInteger,Data.FindSchemaTypeData('integer'));
  AssertEquals('Have 2 public types',2,Data.TypeCount);
  Data.CheckDependencies;
  AssertEquals('type depends on created subtype',1,aType.DependencyCount);
  AssertSame('type depends on created subtype',aType2,aType.Dependency[0]);
end;

procedure TTestSchemaPascalType.TestAddPropertiesObjectExisting;

var
  lProp : TJSONSchema;
  aType1,aType2 : TPascalTypeData;
begin
  aType1:=Data.CreatePascalType(0,ptSchemaStruct,'a','Ta',Schema);
  Data.AddType('a',aType1);
  aType2:=Data.CreatePascalType(1,ptSchemaStruct,'b','Tb',Nil);
  Data.AddType('b',aType2);
  lProp:=DefineProperty('c',sstObject,'');
  lProp.Ref:='b';
  Data.AddPropertiesToType(aType1);
  AssertProperty('prop 1',aType1,0,'c','c','Tb',ptSchemaStruct,aType2);
end;

initialization
   RegisterTest(TTestSchemaPascalType);
end.

