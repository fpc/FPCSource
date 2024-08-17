{
    This file is part of the Free Component Library

    Testsuite for JSONSchema validator
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utSchemaValidator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, fpjson.schema.types, fpjson.schema.schema, fpjson.schema.validator,
  fpjson.schema.testutils;

Type

  { TTestSchemaValidator }

  TTestSchemaValidator = Class(TSchemaTestCase)
  private
    FSchema: TJSONSchema;
    FValidator: TJSONSchemaValidator;
    function AddPrefixItem(aType: TSchemaSimpleType): TJSONSchema;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Procedure AssertValid(const Msg : String;aJSON : TJSONData);
    Procedure AssertValid(const Msg : String;aJSON : TJSONStringType);
    Procedure AssertInValid(const Msg : String;aJSON : TJSONData);
    Procedure AssertInValid(const Msg : String;aJSON : TJSONStringType);
    Procedure SetSchemaJSON(const aJSON : TJSONStringType);
    Property Validator : TJSONSchemaValidator Read FValidator;
    Property Schema : TJSONSchema Read FSchema;
  Published
    Procedure TestHookup;
    Procedure TestAny;
    Procedure TestNone;
    procedure TestTypeString;
    procedure TestTypeInteger;
    procedure TestTypeBoolean;
    procedure TestTypeNumber;
    procedure TestTypeNull;
    procedure TestTypeObject;
    procedure TestTypeArray;
    procedure TestTypes;
    procedure TestConstString;
    procedure TestConstBoolean;
    procedure TestConstInteger;
    procedure TestConstArray;
    procedure TestConstObject;
    Procedure TestNumericMinimum;
    Procedure TestNumericExclusiveMinimum;
    Procedure TestNumericMaximum;
    Procedure TestNumericExclusiveMaximum;
    Procedure TestNumericMultipleOf;
    Procedure TestStringMinLength;
    Procedure TestStringMaxLength;
    Procedure TestStringPattern;
    procedure TestEnum;
    Procedure TestArrayMinItems;
    Procedure TestArrayMaxItems;
    Procedure TestArrayContains;
    Procedure TestArrayMinContains;
    Procedure TestArrayMaxContains;
    Procedure TestArrayPrefixItems;
    procedure TestArrayItems;
    procedure TestArrayItemsPrefixItems;
    procedure TestArrayUniqueItems;
    procedure TestObjectMinProperties;
    procedure TestObjectMaxProperties;
    procedure TestObjectRequired;
    procedure TestObjectDependentRequired;
    procedure TestObjectProperties;
    Procedure TestIfThen;
    Procedure TestIfElse;
    Procedure TestAnyOf;
    Procedure TestAllOf;
    Procedure TestOneOf;
    Procedure TestNot;
    Procedure TestRef;
  end;

implementation

uses fpjson.schema.reader;

Const
  SJSONNull        = 'null';
  SJSONString1     = '"foo"';
  SJSONString2     = '"bar"';
  SJSONInteger     = '42';
  SJSONFloat       = '3.1415';
  SJSONBool        = 'true';
  SJSONBoolFalse   = 'false';
  SJSONArray1      = '['+SJSONString1+']';
  SJSONArray1a     = '['+SJSONString1+','+SJSONString2+']';
  SJSONArray2      = '['+SJSONString1+','+SJSONInteger+']';
  SJSONArray3      = '['+SJSONString1+','+SJSONInteger+','+SJSONFloat+']';
  SJSONArrayNull   = '['+SJSONString1+','+SJSONInteger+','+SJSONNull+']';
  SJSONArrayOnlyNull3 = '['+SJSONNull+','+SJSONNull+','+SJSONNull+']';
  SJSONArrayOnlyNull2 = '['+SJSONNull+','+SJSONNull+']';
  SJSONArrayOnlyNull1 = '['+SJSONNull+']';
  SJSONArrayEmpty  = '[]';
  SJSONArray2Nulls = '['+SJSONString1+','+SJSONInteger+','+SJSONNull+','+SJSONNull+']';
  SJSONArray3Nulls = '['+SJSONString1+','+SJSONInteger+','+SJSONNull+','+SJSONNull+','+SJSONNull+']';
  SJSONArray2NullsA = '['+SJSONString1+','+SJSONInteger+','+SJSONNull+','+SJSONNull+','+SJSONInteger+']';
  SJSONObjectEmpty = '{}';
  SJSONObject1     = '{"one":"foo"}';
  SJSONObject2     = '{"one":"foo","two":"bar"}';
  SJSONObject3     = '{"one":"foo","two":"bar","count":42}';
  SEmailPattern    = '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';

{ TTestSchemaValidator }

procedure TTestSchemaValidator.SetUp;
begin
  inherited SetUp;
  FValidator:=TJSONSchemaValidator.Create(Nil);
  FSchema:=TJSONSchema.Create;
end;

procedure TTestSchemaValidator.TearDown;
begin
  FreeAndNil(FValidator);
  FreeAndNil(FSchema);
  inherited TearDown;
end;

procedure TTestSchemaValidator.AssertValid(const Msg : String;aJSON: TJSONStringType);
var
  D : TJSONData;

begin
  D:=GetJSON(aJSON,True);
  try
    AssertValid(Msg,D);
  finally
    D.Free;
  end;
end;

procedure TTestSchemaValidator.AssertInValid(const Msg: String; aJSON: TJSONStringType);
var
  D : TJSONData;

begin
  D:=GetJSON(aJSON,True);
  try
    AssertInValid(Msg,D);
  finally
    D.Free;
  end;
end;

procedure TTestSchemaValidator.SetSchemaJSON(const aJSON: TJSONStringType);

var
  aReader : TJsonSchemaReader;

begin
  aReader:=TJsonSchemaReader.Create(Nil);
  try
    aReader.ReadFromString(Schema,aJSON);
  finally
    aReader.Free;
  end;
end;

procedure TTestSchemaValidator.AssertInValid(const Msg: String; aJSON: TJSONData);
begin
  FValidator.Reset;
  AssertFalse(Msg,FValidator.ValidateJSON(aJSON,Schema));
end;

procedure TTestSchemaValidator.TestHookup;
begin
  AssertNotNull('Schema',Schema);
  AssertNotNull('Validator',Validator);
  AssertEquals('Validator empty',0,Validator.Messages.Count);
end;

procedure TTestSchemaValidator.TestAny;
begin
  Schema.MatchType:=smAny;
  AssertValid('String',SJSONString1);
  AssertValid('Integer',SJSONInteger);
  AssertValid('Float',SJSONFloat);
  AssertValid('Boolean',SJSONBool);
  AssertValid('Boolean',SJSONBoolFalse);
  AssertValid('Null',SJSONNull);
  AssertValid('Array',SJSONArray1);
  AssertValid('Array3',SJSONArray3);
  AssertValid('Object',SJSONObject1);
end;

procedure TTestSchemaValidator.TestNone;
begin
  Schema.MatchType:=smNone;
  AssertInvalid('String',SJSONString1);
  AssertInvalid('Integer',SJSONInteger);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Array',SJSONArray2);
  AssertInvalid('Object',SJSONObject2);
end;

procedure TTestSchemaValidator.TestTypeString;
begin
  Schema.Validations.Types:=[sstString];
  AssertValid('String',SJSONString1);
  AssertInvalid('Integer',SJSONInteger);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Array',SJSONArray2);
  AssertInvalid('Object',SJSONObject2);
end;

procedure TTestSchemaValidator.TestTypeInteger;
begin
  Schema.Validations.Types:=[sstInteger];
  Assertvalid('Integer',SJSONInteger);
  AssertInValid('String',SJSONString1);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Array',SJSONArray2);
  AssertInvalid('Object',SJSONObject2);
end;

procedure TTestSchemaValidator.TestTypeBoolean;
begin
  Schema.Validations.Types:=[sstBoolean];
  AssertValid('Boolean',SJSONBool);
  AssertValid('False Boolean',SJSONBoolFalse);
  AssertInvalid('Integer',SJSONInteger);
  AssertInValid('String',SJSONString1);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Array',SJSONArray2);
  AssertInvalid('Object',SJSONObject2);
end;

procedure TTestSchemaValidator.TestTypeNumber;
begin
  Schema.Validations.Types:=[sstNumber];
  AssertValid('Integer',SJSONInteger);
  AssertValid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInValid('String',SJSONString1);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Array',SJSONArray2);
  AssertInvalid('Object',SJSONObject2);
end;

procedure TTestSchemaValidator.TestTypeNull;
begin
  Schema.Validations.Types:=[sstNull];
  AssertValid('Null',SJSONNull);
  AssertInvalid('Integer',SJSONInteger);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInValid('String',SJSONString1);
  AssertInvalid('Array',SJSONArray2);
  AssertInvalid('Object',SJSONObject2);
end;

procedure TTestSchemaValidator.TestTypeObject;
begin
  Schema.Validations.Types:=[sstObject];
  AssertValid('Object',SJSONObject2);
  AssertValid('Object 1',SJSONObject1);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Integer',SJSONInteger);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInValid('String',SJSONString1);
  AssertInvalid('Array',SJSONArray2);
end;

procedure TTestSchemaValidator.TestTypeArray;

begin
  Schema.Validations.Types:=[sstArray];
  AssertValid('Array 1',SJSONArray1);
  AssertValid('Array 2',SJSONArray2);
  AssertInvalid('Object',SJSONObject1);
  AssertInvalid('Null',SJSONNull);
  AssertInvalid('Integer',SJSONInteger);
  AssertInvalid('Float',SJSONFloat);
  AssertInvalid('Boolean',SJSONBool);
  AssertInValid('String',SJSONString1);
end;

procedure TTestSchemaValidator.TestTypes;
begin
  Schema.Validations.Types:=[sstArray,sstObject];
  AssertValid('Array1',SJSONArray1);
  Assertvalid('Object',SJSONObject1);
end;

procedure TTestSchemaValidator.TestConstString;
begin
  Schema.Validations.constValue:=TJSONString.Create('foo');
  AssertValid('Same String',SJSONString1);
  AssertInValid('Different String',SJSONString2);
  AssertInValid('Different type','1');
end;

procedure TTestSchemaValidator.TestConstBoolean;
begin
  Schema.Validations.constValue:=TJSONBoolean.Create(true);
  AssertValid('Same value',SJSONBool);
  AssertInValid('Different value',SJSONBoolFalse);
  AssertInValid('Different type','1');
end;

procedure TTestSchemaValidator.TestConstInteger;
begin
  Schema.Validations.constValue:=TJSONBoolean.Create(true);
  AssertValid('Same value',SJSONBool);
  AssertInValid('Different value',SJSONBoolFalse);
  AssertInValid('Different type','1');
  AssertInValid('String with correct value','"true"');
end;

procedure TTestSchemaValidator.TestConstArray;
begin
  Schema.Validations.constValue:=TJSONArray.Create([1,2,3]);
  AssertValid('Same value','[1,2,3]');
  AssertInValid('Different value','[1,3,2]');
  AssertInValid('Different type','1');
  AssertInValid('String with correct value','"[1,2,3]"');
end;

procedure TTestSchemaValidator.TestConstObject;
begin
  Schema.Validations.constValue:=TJSONObject.Create(['one',1,'two',2,'three',3]);
  AssertValid('Same value',Schema.Validations.constValue.AsJson);
  AssertInValid('Different value','{"one":1,"two":3,"three":2}');
  AssertInValid('Different type','1');
  AssertInValid('String with correct value','"{\"one\":1,\"two\":3,\"three\":2}"');
end;

procedure TTestSchemaValidator.TestNumericMinimum;
begin
  Schema.Validations.Types:=[sstNumber];
  Schema.Validations.Minimum:=1;
  AssertValid('OK',SJSONFloat);
  AssertInValid('NOK','0.5');
  AssertValid('Limit OK','1');
end;

procedure TTestSchemaValidator.TestNumericExclusiveMinimum;
begin
  Schema.Validations.Types:=[sstNumber];
  Schema.Validations.ExclusiveMinimum:=1;
  AssertValid('OK',SJSONFloat);
  AssertInValid('NOK','0.5');
  AssertInValid('Limit NOK','1');
end;

procedure TTestSchemaValidator.TestNumericMaximum;
begin
  Schema.Validations.Types:=[sstNumber];
  Schema.Validations.Maximum:=10;
  AssertValid('OK',SJSONFloat);
  AssertInValid('NOK','15');
  AssertValid('Limit OK','10');
end;

procedure TTestSchemaValidator.TestNumericExclusiveMaximum;
begin
  Schema.Validations.Types:=[sstNumber];
  Schema.Validations.ExclusiveMaximum:=10;
  AssertValid('OK',SJSONFloat);
  AssertInValid('NOK','15');
  AssertInValid('Limit OK','10');
end;

procedure TTestSchemaValidator.TestNumericMultipleOf;
begin
  Schema.Validations.Types:=[sstNumber];
  Schema.Validations.MultipleOf:=10;
  AssertValid('OK','100');
  AssertInValid('NOK','15');
  AssertValid('Limit OK','10');
  AssertValid('0','0');
  AssertValid('Float integer','10.000');
end;

procedure TTestSchemaValidator.TestStringMinLength;
begin
  Schema.Validations.Types:=[sstString];
  Schema.Validations.MinLength:=10;
  AssertValid('Limit OK','"0123456789"');
  AssertInValid('NOK','"123456789"');
  AssertValid('OK','"01234567890"');
end;

procedure TTestSchemaValidator.TestStringMaxLength;
begin
  Schema.Validations.Types:=[sstString];
  Schema.Validations.MaxLength:=10;
  AssertValid('Limit OK','"0123456789"');
  AssertInValid('NOK','"01234567890"');
  AssertValid('OK','"123456789"');
end;

procedure TTestSchemaValidator.TestStringPattern;
begin
  Schema.Validations.Types:=[sstString];
  Schema.Validations.Pattern:=SEmailPattern;
  AssertValid('Email','"michael@freepascal.org"');
  AssertInValid('Email 2','"michael@freepascal"');
end;

procedure TTestSchemaValidator.TestEnum;
begin
  Schema.Validations.Enum:=TJSONArray.Create([True,'something']);
  AssertValid('Boolean','true');
  AssertValid('String','"something"');
  AssertInValid('String with boolean','"true"');
  AssertInValid('Numerical','123');
end;

procedure TTestSchemaValidator.TestArrayMinItems;
begin
  Schema.Validations.Types:=[sstArray];
  Schema.Validations.MinItems:=2;
  AssertValid('2',SJSONArray1a);
  AssertValid('2a',SJSONArray2);
  AssertValid('3',SJSONArray2);
  AssertInValid('1',SJSONArray1);
end;

procedure TTestSchemaValidator.TestArrayMaxItems;
begin
  Schema.Validations.Types:=[sstArray];
  Schema.Validations.MaxItems:=2;
  AssertValid('1',SJSONArray1);
  AssertValid('2',SJSONArray1a);
  AssertValid('2a',SJSONArray2);
  AssertInValid('3',SJSONArray3);
end;

procedure TTestSchemaValidator.TestArrayContains;
begin
  Schema.Validations.Types:=[sstArray];
  Schema.Contains.Validations.Types:=[sstNull];
  AssertValid('1',SJSONArrayNull);
end;

procedure TTestSchemaValidator.TestArrayMinContains;
begin
  Schema.Validations.Types:=[sstArray];
  Schema.Validations.MinContains:=2;
  Schema.Contains.Validations.Types:=[sstNull];
  AssertValid('2',SJSONArray2Nulls);
  AssertValid('3',SJSONArray3Nulls);
  AssertInValid('1',SJSONArrayNull);

end;

procedure TTestSchemaValidator.TestArrayMaxContains;
begin
  Schema.Validations.Types:=[sstArray];
  Schema.Validations.MaxContains:=2;
  Schema.Contains.Validations.Types:=[sstNull];
  AssertValid('2',SJSONArray2Nulls);
  AssertValid('1',SJSONArrayNull);
  AssertInValid('3',SJSONArray3Nulls);
end;

function TTestSchemaValidator.AddPrefixItem(aType : TSchemaSimpleType) : TJSONSchema;

begin
  Result:=Schema.CreateChildSchema;
  Result.Validations.Types:=[aType];
  Schema.PrefixItems.Add(Result);
end;

procedure TTestSchemaValidator.TestArrayPrefixItems;

begin
  Schema.Validations.Types:=[sstArray];
  AddPrefixItem(sstString);
  AddPrefixItem(sstInteger);
  AssertValid('Less elements OK',SJSONArray1);
  AssertValid('Exact elements OK',SJSONArray2);
  AssertValid('More elements OK',SJSONArrayNull);
  AssertInValid('2nd element not OK',SJSONArray1a);
end;

procedure TTestSchemaValidator.TestArrayItems;

var
  SS : TJSONSchema;

begin
  Schema.Validations.Types:=[sstArray];
  SS:=Schema.CreateChildSchema;
  SS.Validations.Types:=[sstNull];
  Schema.Items.Add(SS);
  AssertValid('Empty OK',SJSONArrayEmpty);
  AssertValid('One OK',SJSONArrayOnlyNull1);
  AssertValid('Two OK',SJSONArrayOnlyNull2);
  AssertValid('Three OK',SJSONArrayOnlyNull3);
  AssertInValid('None NOK',SJSONArray1a);
  AssertInValid('One with others NOK',SJSONArrayNull);

end;

procedure TTestSchemaValidator.TestArrayItemsPrefixItems;
var
  SS : TJSONSchema;

begin
  Schema.Validations.Types:=[sstArray];
  AddPrefixItem(sstString);
  AddPrefixItem(sstInteger);
  SS:=Schema.CreateChildSchema;
  SS.Validations.Types:=[sstNull];
  Schema.Items.Add(SS);
  AssertValid('Empty OK',SJSONArrayEmpty);
  AssertValid('Less elements OK',SJSONArray1);
  AssertValid('Exact elements OK',SJSONArray2);
  AssertValid('More Prefix elements OK',SJSONArrayNull);
  AssertInValid('Has some but then again not',SJSONArray2NullsA);
  AssertInValid('Immediate NOK',SJSONArray3);
  AssertInValid('Two OK',SJSONArrayOnlyNull2);
  AssertInValid('Three OK',SJSONArrayOnlyNull3);
  AssertInValid('One OK',SJSONArrayOnlyNull1);
  AssertInValid('None NOK',SJSONArray1a);
end;

procedure TTestSchemaValidator.TestArrayUniqueItems;
begin
  Schema.Validations.Types:=[sstArray];
  Schema.Validations.UniqueItems:=True;
  AssertValid('Empty OK',SJSONArrayEmpty);
  AssertValid('One element OK',SJSONArrayOnlyNull1);
  AssertValid('Different elements OK',SJSONArray3);
  AssertInValid('2 elements OK',SJSONArrayOnlyNull2);
  AssertInValid('3 elements OK',SJSONArrayOnlyNull3);
end;

procedure TTestSchemaValidator.TestObjectMinProperties;
begin
  Schema.Validations.Types:=[sstObject];
  Schema.Validations.MinProperties:=1;
  AssertValid('1 ok',SJSONObject1);
  AssertValid('2 ok',SJSONObject2);
  AssertValid('3 ok',SJSONObject3);
  AssertInvalid('0 not ok',SJSONObjectEmpty);
  Schema.Validations.MinProperties:=3;
  AssertInValid('Min 3, 2 not ok',SJSONObject2);
end;

procedure TTestSchemaValidator.TestObjectMaxProperties;
begin
  Schema.Validations.Types:=[sstObject];
  Schema.Validations.MaxProperties:=1;
  Assertvalid('0 not ok',SJSONObjectEmpty);
  AssertValid('1 ok',SJSONObject1);
  AssertInValid('2 not ok',SJSONObject2);
  AssertInValid('3 not ok',SJSONObject3);
  Schema.Validations.MaxProperties:=2;
  AssertInValid('Max 2, 3 not ok',SJSONObject3);
end;

procedure TTestSchemaValidator.TestObjectRequired;
begin
  Schema.Validations.Types:=[sstObject];
  Schema.Validations.Required.Add('one');
  AssertValid('1 ok',SJSONObject1);
  AssertValid('2 ok',SJSONObject2);
  AssertInValid('0 nok',SJSONObjectEmpty);
  Schema.Validations.Required.Add('two');
  AssertValid('2 ok',SJSONObject2);
  AssertValid('3 ok',SJSONObject3);
  AssertInValid('1 ok',SJSONObject1);
end;

procedure TTestSchemaValidator.TestObjectDependentRequired;
begin
  Schema.Validations.Types:=[sstObject];
  Schema.Validations.DependentRequired.AddDependent('one').Required.Add('two');
  AssertValid('Empty ok',SJSONObjectEmpty);
  AssertValid('Req ok',SJSONObject2);
  AssertValid('Extra ok',SJSONObject3);
  AssertInvalid('Missing not ok',SJSONObject1);
  Schema.Validations.DependentRequired.AddDependent('two').Required.Add('count');
  AssertValid('Extra ok',SJSONObject3);
  AssertInValid('Req missin nok',SJSONObject2);
end;

procedure TTestSchemaValidator.TestObjectProperties;
var
  SS : TJSONSchema;
begin
  Schema.Validations.Types:=[sstObject];
  SS:=Schema.CreateChildSchema('one');
  SS.Validations.types:=[sstString];
  Schema.Properties.Add(SS);
  SS:=Schema.CreateChildSchema('two');
  SS.Validations.types:=[sstString];
  Schema.Properties.Add(SS);
end;

procedure TTestSchemaValidator.TestIfThen;
begin
  Schema.IfSchema.Validations.Types:=[sstString];
  Schema.ThenSchema.Validations.constValue:=TJSONString.Create('something');
  AssertValid('Correct then','"something"');
  AssertValid('no else OK','true');
  AssertInValid('Incorrect then','"solo"');
end;

procedure TTestSchemaValidator.TestIfElse;

begin
  Schema.IfSchema.Validations.Types:=[sstString];
  Schema.ElseSchema.Validations.constValue:=TJSONIntegerNumber.Create(12);
  AssertValid('No then OK','"something"');
  AssertValid('Correct else','12');
  AssertInValid('Incorrect else','24');
end;

procedure TTestSchemaValidator.TestAnyOf;
var
  SS : TJSONSchema;
begin
  SS:=Schema.CreateChildSchema;
  SS.Validations.Types:=[sstString];
  Schema.AnyOf.Add(SS);
  SS:=Schema.CreateChildSchema;
  SS.Validations.Types:=[sstNumber];
  Schema.AnyOf.Add(SS);
  AssertValid('String ','"something"');
  AssertValid('Number','12');
  AssertInValid('Boolean','false');
end;

procedure TTestSchemaValidator.TestAllOf;
var
  SS : TJSONSchema;
begin
  SS:=Schema.CreateChildSchema;
  SS.Validations.Types:=[sstString];
  Schema.AllOf.Add(SS);
  SS:=Schema.CreateChildSchema;
  SS.Validations.constValue:=TJSONString.Create('something');
  Schema.AllOf.Add(SS);
  AssertValid('String ','"something"');
  AssertInvalid('String ','"else"');
  AssertInValid('Number','12');
  AssertInValid('Boolean','false');
end;

procedure TTestSchemaValidator.TestOneOf;
var
  SS : TJSONSchema;
begin
  SS:=Schema.CreateChildSchema;
  SS.Validations.Types:=[sstString];
  Schema.OneOf.Add(SS);
  SS:=Schema.CreateChildSchema;
  SS.Validations.constValue:=TJSONString.Create('something');
  Schema.OneOf.Add(SS);
  AssertValid('Different String ','"else"'); //
  AssertInValid('String ','"something"');
  AssertInValid('Number','12');
  AssertInValid('Boolean','false');
end;

procedure TTestSchemaValidator.TestNot;
begin
  Schema.NotSchema.Validations.Types:=[sstString];
  AssertInValid('String not OK','"something"');
  AssertValid('Number Correct','12');
  AssertValid('Boolean Correct','false');
end;

procedure TTestSchemaValidator.TestRef;
begin
  Schema.Validations.Types:=[sstObject];
  Schema.Properties.Add('productId').Validations.Types:=[sstInteger];
  Schema.Properties.Add('name').Ref:='#/$defs/string';
  with Schema.Defs.Add('string') do
    begin
    id:='string';
    Validations.types:=[sstString];
    end;
  AssertValid('Correct ref','{ "productId": 123, "name" : "Widget" }');
end;



procedure TTestSchemaValidator.AssertValid(const Msg: String; aJSON: TJSONData);
begin
  FValidator.Reset;
  AssertTrue(Msg,FValidator.ValidateJSON(aJSON,Schema));
end;

initialization
  RegisterTest(TTestSchemaValidator);
end.

