{
    This file is part of the Free Component Library

    Testsuite for JSONSchema class
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utSchema;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, fpjson.schema.types, fpjson.schema.schema,
  fpjson.schema.testutils;

type

  { TTestSchemaObject }

  TTestSchemaObject = class(TSchemaTestcase)
  private
    FSchema: TJSONSchema;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    Property Schema : TJSONSchema Read FSchema;
  end;

  TTestJSONSchemaMetadata = class(TJSONSchemaMetadata)
  Public
    Property AVailableKeywords : TJSONSchemaKeywords Read Keywords;
  end;

  { TTestMetadata }

  TTestMetadata = class(TTestSchemaObject)
  private
    FData: TJSONSchemaMetadata;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure TestHavekeyword(aKeyword : TJSONSchemaKeyword);
    property Data : TJSONSchemaMetadata Read FData;
  Published
    Procedure TestHookup;
    Procedure TestTitle;
    Procedure TestKeywords;
    Procedure TestDescription;
    Procedure TestDefaultValue;
    Procedure TestDeprecated;
    Procedure TestExamples;
    Procedure TestReadOnly;
    Procedure TestWriteOnly;
    Procedure TestAssign;
  end;

  { TTestSchemaValidations }

  TTestSchemaValidations = class(TTestSchemaObject)
  private
    FData: TJSONSchemaValidations;
  Public
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure TestHavekeyword(aKeyword : TJSONSchemaKeyword);
    property Data : TJSONSchemaValidations Read FData;
  Published
    Procedure TestHookup;
    Procedure TestTypes;
    Procedure TestconstValue;
    Procedure TestEnum;
    Procedure TestExclusiveMaximum;
    Procedure TestExclusiveMinimum;
    Procedure TestMaximum;
    Procedure TestMinimum;
    Procedure TestMaxItems;
    Procedure TestMinItems;
    Procedure TestRequired;
    Procedure TestMaxLength;
    Procedure TestMinLength;
    Procedure TestMaxProperties;
    Procedure TestMinProperties;
    Procedure TestPattern;
    Procedure TestUniqueItems;
    Procedure TestMinContains;
    Procedure TestMaxContains;
    Procedure TestMultipleOf;
    Procedure TestDependentRequired;
    Procedure TestFormat;
    Procedure TestFormatValidator;
    Procedure TestcontentMediaType;
    Procedure TestcontentEncoding;
    Procedure TestcontentSchema;
    Procedure TestAssign;
  end;

  TTestSchema = Class(TTestSchemaObject)
  private
    procedure CheckKeyword(aKeyword: TJSONSchemaKeyword);
  Published
    Procedure TestHookup;
    Procedure TestCreateChildSchemaName;
    Procedure TestCreateChildSchemaKeyword;
    Procedure TestCreateChildSchemaNoName;
    Procedure TestRootSchema;
    Procedure TestFind;
    Procedure TestIndexOfChild;
    Procedure TestFindChild;
    Procedure TestPath;
    // Vocabulary
    Procedure TestValidation;
    Procedure TestMetaData;
    Procedure TestID;
    Procedure TestSchema;
    Procedure TestRef;
    Procedure TestComment;
    Procedure TestAnchor;
    Procedure TestDefs;
    Procedure TestDynamicAnchor;
    Procedure TestDynamicRef;
    Procedure TestVocabulary;
    Procedure TestAllof;
    Procedure TestAnyOf;
    Procedure TestOneOf;
    Procedure TestNotSchema;
    Procedure TestIfSchema;
    Procedure TestThenSchema;
    Procedure TestElseSchema;
    Procedure TestProperties;
    Procedure TestItems;
    Procedure TestPrefixItems;
    Procedure TestPatternProperties;
    Procedure TestPropertyNames;
    Procedure TestAdditionalProperties;
    Procedure TestDependentSchemas;
    Procedure TestContains;
    Procedure TestUnevaluatedItems;
    Procedure TestUnevaluatedProperties;
  end;


implementation

{ TTestSchemaObject }

procedure TTestSchemaObject.SetUp;
begin
  inherited SetUp;
  FSchema:=TJSONSchema.Create;
end;

procedure TTestSchemaObject.TearDown;
begin
  FreeAndNil(FSchema);
  inherited TearDown;
end;

{ TTestMetadata }

procedure TTestMetadata.SetUp;
begin
  inherited SetUp;
  FData:=TTestJSONSchemaMetadata.Create(Schema);
end;

procedure TTestMetadata.TearDown;
begin
  FreeAndNil(FData);
  inherited TearDown;
end;

procedure TTestMetadata.TestHavekeyword(aKeyword: TJSONSchemaKeyword);
begin
  AssertTrue('Have '+aKeyword.AsString+' data',Data.HasKeywordData(aKeyword));
  AssertTrue('Keywords',Data.KeywordsWithData=[aKeyWord]);
  AssertTrue('Schema constrained',Data.Schema.MatchType=smConstrained);
end;

procedure TTestMetadata.TestHookup;

var
  T : TJSONSchemaKeyword;

begin
  AssertNotNull('Have data',Data);
  AssertSame('Schema',Schema,Data.Schema);
  For T in TJSONSchemaKeyword do
    AssertFalse('No '+t.AsString+' data on init',Data.HasKeywordData(T));
end;

procedure TTestMetadata.TestTitle;
begin
  Data.Title:='Solo';
  TestHavekeyword(jskTitle);
  AssertTrue('Sets constrained',Schema.MatchType=smConstrained);
end;

procedure TTestMetadata.TestKeywords;

const
  KW = [jskTitle,jskDescription,jskDefault,jskDeprecated,
        jskExamples,jskReadOnly,jskWriteOnly];
begin
  AssertTrue('Correct keyword list',Kw=TTestJSONSchemaMetadata(Data).AvailableKeywords)
end;

procedure TTestMetadata.TestDescription;
begin
  Data.Description:='Solo';
  TestHavekeyword(jskDescription);
end;

procedure TTestMetadata.TestDefaultValue;
begin
  Data.DefaultValue:=TJSONIntegerNumber.Create(12);
  TestHavekeyword(jskDefault);
end;

procedure TTestMetadata.TestDeprecated;
begin
  Data.Deprecated:=False;
  TestHavekeyword(jskDeprecated);
end;

procedure TTestMetadata.TestExamples;
begin
  // Needs to be done manually, we do not detect changes to
//  Schema.MatchType:=smConstrained;
  Data.Examples:=TJSONArray.Create(['Example1']);
  TestHavekeyword(jskExamples);
end;

procedure TTestMetadata.TestReadOnly;
begin
  Data.ReadOnly:=True;
  TestHavekeyword(jskReadOnly);
end;

procedure TTestMetadata.TestWriteOnly;
begin
  Data.WriteOnly:=True;
  TestHavekeyword(jskWriteOnly);
end;

procedure TTestMetadata.TestAssign;

var
  M : TJSONSchemaMetadata;

begin
  M:=TJSONSchemaMetadata.Create(Schema);
  try
    Data.Title:='Solo';
    Data.Description:='Solo2';
    Data.DefaultValue:=TJSONIntegerNumber.Create(12);
    Data.Deprecated:=True;
    Data.Examples.Add(TJSONString.Create('Example1'));
    Data.ReadOnly:=True;
    Data.WriteOnly:=True;
    M.Assign(Data);

    AssertEquals('Title',Data.Title,M.Title);
    AssertEquals('Description',Data.Description,m.Description);
    AssertEquals('Deprecated',Data.Deprecated,m.Deprecated);
    AssertEquals('ReadOnly',Data.ReadOnly,m.ReadOnly);
    AssertEquals('WriteOnly',Data.WriteOnly,m.WriteOnly);
    AssertNotNull('Default copied',M.DefaultValue);
    AssertEquals('Correct value',12,m.DefaultValue.AsInteger);
    AssertEquals('Examples copied',Data.Examples.Count,M.Examples.Count);

  finally
    M.Free;
  end;
end;

{ TTestSchemaValidations }

procedure TTestSchemaValidations.SetUp;
begin
  inherited SetUp;
  FData:=TJSONSchemaValidations.Create(Schema);
end;

procedure TTestSchemaValidations.TearDown;
begin
  FreeAndNil(FData);
  inherited TearDown;
end;

procedure TTestSchemaValidations.TestHavekeyword(aKeyword: TJSONSchemaKeyword);
begin
  AssertTrue('Have '+aKeyword.AsString+' data',Data.HasKeywordData(aKeyword));
  AssertTrue('Correct keywords ',Data.KeywordsWithData=[aKeyword]);
  AssertTrue('Schema set to constrained',Schema.MatchType=smConstrained);
end;

procedure TTestSchemaValidations.TestHookup;
var
  T : TJSONSchemaKeyword;

begin
  AssertNotNull('Have data',Data);
  AssertSame('Schema',Schema,Data.Schema);
  For T in TJSONSchemaKeyword do
    AssertFalse('No '+t.AsString+' data on init',Data.HasKeywordData(T));
end;

procedure TTestSchemaValidations.TestTypes;
begin
  Data.Types:=[sstObject,sstString];
  TestHavekeyword(jskType);
  AssertTrue('Sets constrained',Schema.MatchType=smConstrained);
end;

procedure TTestSchemaValidations.TestconstValue;
begin
  Data.ConstValue:=TJSONObject.Create(['me','you']);
  TestHavekeyword(jskConst);
end;

procedure TTestSchemaValidations.TestEnum;
begin
  Data.Enum:=TJSONArray.Create([TJSONObject.Create(['me','you'])]);
  TestHavekeyword(jskEnum);
end;

procedure TTestSchemaValidations.TestExclusiveMaximum;
begin
  Data.ExclusiveMaximum:=10.0;
  TestHavekeyword(jskExclusiveMaximum);
end;

procedure TTestSchemaValidations.TestExclusiveMinimum;
begin
  Data.ExclusiveMinimum:=10.0;
  TestHavekeyword(jskExclusiveMinimum);
end;

procedure TTestSchemaValidations.TestMaximum;
begin
  Data.Maximum:=10.0;
  TestHavekeyword(jskMaximum);
end;

procedure TTestSchemaValidations.TestMinimum;
begin
  Data.Minimum:=10.0;
  TestHavekeyword(jskMinimum);
end;

procedure TTestSchemaValidations.TestMaxItems;
begin
  Data.MaxItems:=10;
  TestHavekeyword(jskMaxItems);
end;

procedure TTestSchemaValidations.TestMinItems;
begin
  Data.MaxItems:=13;
  TestHavekeyword(jskMaxItems);
end;

procedure TTestSchemaValidations.TestRequired;
begin
  Data.Required.Add('type');
  TestHavekeyword(jskRequired);
end;

procedure TTestSchemaValidations.TestMaxLength;
begin
  Data.MaxLength:=10;
  TestHavekeyword(jskMaxLength);
end;

procedure TTestSchemaValidations.TestMinLength;
begin
  Data.MinLength:=10;
  TestHavekeyword(jskMinLength);
end;

procedure TTestSchemaValidations.TestMaxProperties;
begin
  Data.MaxProperties:=10;
  TestHavekeyword(jskMaxProperties);
end;

procedure TTestSchemaValidations.TestMinProperties;
begin
  Data.MinProperties:=10;
  TestHavekeyword(jskMinProperties);
end;

procedure TTestSchemaValidations.TestPattern;
begin
  Data.MinProperties:=10;
  TestHavekeyword(jskMinProperties);
end;

procedure TTestSchemaValidations.TestUniqueItems;
begin
  Data.UniqueItems:=True;
  TestHavekeyword(jskUniqueItems);
end;

procedure TTestSchemaValidations.TestMinContains;
begin
  Data.MinContains:=10;
  TestHavekeyword(jskMinContains);
end;

procedure TTestSchemaValidations.TestMaxContains;
begin
  Data.MaxContains:=10;
  TestHavekeyword(jskMaxContains);
end;

procedure TTestSchemaValidations.TestMultipleOf;
begin
  Data.MultipleOf:=10.1;
  TestHavekeyword(jskMultipleOf);
end;

procedure TTestSchemaValidations.TestDependentRequired;
begin
  Data.DependentRequired.AddDependent('xyz');
  TestHavekeyword(jskDependentRequired);
end;

procedure TTestSchemaValidations.TestFormat;
begin
  Data.Format:='uri';
  TestHavekeyword(jskFormat);
end;

procedure TTestSchemaValidations.TestFormatValidator;
begin
  Data.FormatValidator:=sfvUri;
  TestHavekeyword(jskFormat);
  AssertEquals('String','uri',Data.Format);
end;

procedure TTestSchemaValidations.TestcontentMediaType;
begin
  Data.contentMediaType:='application/json';
  TestHavekeyword(jskContentMediaType);
end;

procedure TTestSchemaValidations.TestcontentEncoding;
begin
  Data.contentEncoding:='utf8';
  TestHavekeyword(jskContentEncoding);
end;

procedure TTestSchemaValidations.TestcontentSchema;
begin
  Data.contentSchema.MatchType:=smAny;
  TestHavekeyword(jskContentSchema);
end;

procedure TTestSchemaValidations.TestAssign;

var
  V : TJSONSchemaValidations;

begin
  Data.UniqueItems:=True;
  Data.Types:=[sstObject];
  Data.Required.Add('x');
  Data.Pattern:='Pattern';
  Data.MultipleOf:=10;
  Data.MinProperties:=11;
  Data.MinLength:=12;
  Data.MinItems:=13;
  Data.Minimum:=14;
  Data.MinContains:=15;
  Data.MaxProperties:=16;
  Data.MaxLength:=17;
  Data.MaxItems:=18;
  Data.Maximum:=19;
  Data.MaxContains:=20;
  Data.Format:='uri';
  Data.ExclusiveMinimum:=12;
  Data.ExclusiveMaximum:=13;
  Data.contentMediaType:='application/json';
  Data.contentEncoding:='utf8';
  Data.constValue:=TJSONBoolean.Create(True);
  V:=TJSONSchemaValidations.Create(Schema);
  try
    V.Assign(data);
    With Data do
      begin
      AssertEquals('UniqueItems',UniqueItems,V.UniqueItems);
      AssertTrue('Types',Types=V.Types);
      AssertEquals('Required',Required.Text,V.Required.Text);
      AssertEquals('Pattern',Pattern,V.Pattern);
      AssertEquals('MultipleOf',MultipleOf,V.MultipleOf);
      AssertEquals('MinProperties',MinProperties,V.MinProperties);
      AssertEquals('MinLength',MinLength,V.MinLength);
      AssertEquals('MinItems',MinItems,V.MinItems);
      AssertEquals('Minimum',Minimum,V.Minimum);
      AssertEquals('MinContains',MinContains,V.MinContains);
      AssertEquals('MaxProperties',MaxProperties,V.MaxProperties);
      AssertEquals('MaxLength',MaxLength,V.MaxLength);
      AssertEquals('MaxItems',MaxItems,V.MaxItems);
      AssertEquals('Maximum',Maximum,V.Maximum);
      AssertEquals('MaxContains',MaxContains,V.MaxContains);
      AssertEquals('Format',Format,V.Format);
      AssertEquals('ExclusiveMinimum',ExclusiveMinimum,V.ExclusiveMinimum);
      AssertEquals('ExclusiveMaximum',ExclusiveMaximum,V.ExclusiveMaximum);
      AssertEquals('contentMediaType',contentMediaType,V.contentMediaType);
      AssertEquals('contentEncoding',contentEncoding,V.contentEncoding);
      AssertEquals('constValue',constValue.asJSON,V.constValue.aSJSON);
      end
  finally
    V.Free
  end;
end;

procedure TTestSchema.TestHookup;

var
  K : TJSONSchemaKeyword;

begin
  AssertNotNull('Have schema',Schema);
  AssertNotNull('Have metadata',Schema.MetaData);
  AssertNotNull('Have validations',Schema.Validations);
  for K in TJSONSchemaKeyword do
    AssertFalse('No keyword '+K.AsString,Schema.HasKeywordData(k));
  AssertSame('Metadata schema',Schema,Schema.Metadata.Schema);
  AssertSame('Validations schema',Schema,Schema.Validations.Schema);
  AssertEquals('No children',0,Schema.ChildSchemaCount);
  AssertSame('root',Schema,Schema.RootSchema);
end;

procedure TTestSchema.TestCreateChildSchemaName;

var
  S : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S:=Schema.CreateChildSchema('ok');
  try
    AssertSame('Schema parent',schema,S.Parent);
    AssertEquals('Name','ok',S.Name);
    AssertEquals('Count',1,Schema.ChildSchemaCount);
    AssertSame('Child',S,Schema.ChildSchemas[0]);
  finally
    S.Free;
  end;
  AssertEquals('Count',0,Schema.ChildSchemaCount);
end;

procedure TTestSchema.TestCreateChildSchemaKeyword;
var
  S : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S:=Schema.CreateChildSchema(jskContentSchema);
  try
    AssertSame('Schema parent',schema,S.Parent);
    AssertEquals('Name','contentSchema',S.Name);
    AssertEquals('Count',1,Schema.ChildSchemaCount);
    AssertSame('Child',S,Schema.ChildSchemas[0]);
  finally
    S.Free;
  end;
  AssertEquals('Count',0,Schema.ChildSchemaCount);
end;

procedure TTestSchema.TestCreateChildSchemaNoName;
var
  S : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S:=Schema.CreateChildSchema();
  try
    AssertSame('Schema parent',schema,S.Parent);
    AssertEquals('Name','',S.Name);
    AssertEquals('Count',1,Schema.ChildSchemaCount);
    AssertSame('Child',S,Schema.ChildSchemas[0]);
  finally
    S.Free;
  end;
  AssertEquals('Count',0,Schema.ChildSchemaCount);
end;

procedure TTestSchema.TestRootSchema;

var
  S,S2 : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S:=Schema.CreateChildSchema();
  try
    AssertSame('Schema parent',schema,S.Parent);
    AssertEquals('Name','',S.Name);
    AssertEquals('Count 1',1,Schema.ChildSchemaCount);
    AssertSame('Child ',S,Schema.ChildSchemas[0]);
    S2:=S.CreateChildSchema();
    AssertEquals('Count 1a',1,Schema.ChildSchemaCount);
    AssertEquals('Count 2',1,S.ChildSchemaCount);
    AssertSame('Child 2',S2,S.ChildSchemas[0]);
    AssertSame('root',Schema,S2.RootSchema);
  finally
    S.Free;
    S2.Free;
  end;
  AssertEquals('Count',0,Schema.ChildSchemaCount);
end;

procedure TTestSchema.TestFind;

  // Find schema using schema-local $Ref URI
var
  S,S2 : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S:=Schema.CreateChildSchema('x');
  try
    S2:=S.CreateChildSchema('y');
    AssertSame('Find #/x/y',S2,Schema.Find('#/x/y'));
  finally
    S.Free;
    S2.Free;
  end;

end;

procedure TTestSchema.TestIndexOfChild;

// Find index of direct child schema with given name
var
  S,S2 : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S2:=nil;
  S:=Schema.CreateChildSchema('x');
  try
    S2:=Schema.CreateChildSchema('y');
    AssertEquals('Count',2,Schema.ChildSchemaCount);
    AssertEquals('Index',0,Schema.IndexOfChild('x'));
    AssertEquals('Index',1,Schema.IndexOfChild('y'));
  finally
    S.Free;
    S2.Free;
  end;
  AssertEquals('Count',0,Schema.ChildSchemaCount);
end;

procedure TTestSchema.TestFindChild;

  // Find direct child schema with given name
  // function FindChild(const aName: String): TJSONSchema;
var
  S,S2 : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S2:=nil;
  S:=Schema.CreateChildSchema('x');
  try
    S2:=Schema.CreateChildSchema('y');
    AssertEquals('Count',2,Schema.ChildSchemaCount);
    AssertSame('Index',S,Schema.FindChild('x'));
    AssertSame('Index',S2,Schema.FindChild('y'));
  finally
    S.Free;
    S2.Free;
  end;
  AssertEquals('Count',0,Schema.ChildSchemaCount);
end;

procedure TTestSchema.TestPath;
  // Path till root schema.

var
  S,S2 : TJSONSchema;

begin
  // Create child schema with given name
  // function CreateChildSchema(aName : string): TJsonSchema; overload;
  S:=Schema.CreateChildSchema('x');
  try
    S2:=S.CreateChildSchema('y');
    AssertEquals('Path','#/x/y',S2.Path);
  finally
    S.Free;
    S2.Free;
  end;
end;

procedure TTestSchema.TestValidation;
begin
  AssertNotNull('Have validation',Schema.Validations);
  AssertSame('Schema correct',Schema,Schema.Validations.Schema);
  Schema.Validations.Format:='uri';
  AssertTrue('Keyword passes through',Schema.HasKeywordData(jskFormat));
end;

procedure TTestSchema.TestMetaData;
begin
  AssertNotNull('Have validation',Schema.Metadata);
  AssertSame('Schema correct',Schema,Schema.Metadata.Schema);
  Schema.Metadata.ReadOnly:=True;
  AssertTrue('Keyword passes through',Schema.HasKeywordData(jskReadOnly));
end;

procedure TTestSchema.CheckKeyword(aKeyword : TJSONSchemaKeyword);

begin
  AssertTrue('Have Keyword '+aKeyword.AsString,Schema.HasKeywordData(aKeyword));
  AssertTrue('Schema constrained',Schema.MatchType=smConstrained);
end;


procedure TTestSchema.TestID;
begin
  // ID of this schema
  Schema.ID:='https://json-schema.org/draft/2020-12/schema';
  CheckKeyword(jskID);
end;

procedure TTestSchema.TestSchema;
begin
  // Identifier of used JSON schema
  Schema.Schema:='https://json-schema.org/draft/2020-12/schema';
  CheckKeyword(jskSchema);
end;

procedure TTestSchema.TestRef;
begin
  // $ref
  Schema.Ref:='#/solo';
  CheckKeyword(jskRef);

end;

procedure TTestSchema.TestComment;
begin
  // $comment
  Schema.Comment:=' A comment';
  CheckKeyword(jskComment);

end;

procedure TTestSchema.TestAnchor;
begin
  // $anchor
  Schema.anchor:='#/ref/two';
  CheckKeyword(jskAnchor);

end;

procedure TTestSchema.TestDefs;
begin
  // $defs
  // property Defs: TJSONSchemaList read FDefs;

end;

procedure TTestSchema.TestDynamicAnchor;
begin
  // $dynamicAnchor
  Schema.DynamicAnchor:='#/ref/two';
  CheckKeyword(jskDynamicAnchor);

end;

procedure TTestSchema.TestDynamicRef;
begin
  // $dynamicRef
  Schema.DynamicRef:='#/ref/two';
  CheckKeyword(jskDynamicRef);
end;

procedure TTestSchema.TestVocabulary;
begin
  // $vocabulary
  Schema.Vocabulary.AddVocabulary('https://www.freepascal.org/').Enabled:=True;
  CheckKeyword(jskVocabulary);
  AssertEquals('Count',1,Schema.Vocabulary.Count);
  AssertEquals('URL','https://www.freepascal.org/',Schema.Vocabulary[0].URL);
  AssertEquals('Enabled',True,Schema.Vocabulary[0].Enabled);
end;

procedure TTestSchema.TestAllof;

var
  S1,S2 : TJSONSchema;
begin
  // allOf keyword
  S1:=Schema.AllOf.Add('x');
  CheckKeyword(jskAllOf);
  S2:=Schema.AllOf.Add('y');
  AssertEquals('Count',2,Schema.AllOf.Count);
  AssertSame('Item 0',S1,Schema.AllOf[0]);
  AssertSame('Item 1',S2,Schema.AllOf[1]);
end;

procedure TTestSchema.TestAnyOf;
var
  S1,S2 : TJSONSchema;
begin
  // anyOf keyword
  S1:=Schema.AnyOf.Add('x');
  CheckKeyword(jskAnyOf);
  S2:=Schema.AnyOf.Add('y');
  AssertEquals('Count',2,Schema.AnyOf.Count);
  AssertSame('Item 0',S1,Schema.AnyOf[0]);
  AssertSame('Item 1',S2,Schema.AnyOf[1]);
end;

procedure TTestSchema.TestOneOf;
var
  S1,S2 : TJSONSchema;
begin
  // oneOf keyword
  S1:=Schema.OneOf.Add('x');
  CheckKeyword(jskOneOf);
  S2:=Schema.oneOf.Add('y');
  AssertEquals('Count',2,Schema.OneOf.Count);
  AssertSame('Item 0',S1,Schema.OneOf[0]);
  AssertSame('Item 1',S2,Schema.OneOf[1]);
end;

procedure TTestSchema.TestNotSchema;
begin
  // not keyword
  Schema.NotSchema.MatchType:=smAny;
  CheckKeyword(jskNot);
end;

procedure TTestSchema.TestIfSchema;
begin
  // If keyword
  Schema.IfSchema.MatchType:=smAny;
  CheckKeyword(jskIf);
end;

procedure TTestSchema.TestThenSchema;
begin
  // then keyword
  Schema.ThenSchema.MatchType:=smAny;
  CheckKeyword(jskThen);

end;

procedure TTestSchema.TestElseSchema;
begin
  // else keyword
  Schema.ElseSchema.MatchType:=smAny;
  CheckKeyword(jskElse);
end;

procedure TTestSchema.TestProperties;
begin
  // properties keyword
  // property Properties: TJsonSchemaList read FProperties;
  Schema.Properties.Add('name');
  CheckKeyword(jskProperties);
end;

procedure TTestSchema.TestItems;
begin
  // Declared in draft 2020-12 as schema, but we keep it a List, so we can handle earlier drafts.
  Schema.Items.Add('name');
  CheckKeyword(jskItems);
end;

procedure TTestSchema.TestPrefixItems;
begin
  // prefixItems keyword.
  Schema.prefixItems.Add('name');
  CheckKeyword(jskPrefixItems);
end;

procedure TTestSchema.TestPatternProperties;
begin
  // patternProperties keyword
  Schema.PatternProperties.Add('ok').MatchType:=smAny;
  CheckKeyword(jskPatternProperties);
end;

procedure TTestSchema.TestPropertyNames;
begin
  // propertyNames keyword
  Schema.PropertyNames.MatchType:=smAny;
  CheckKeyword(jskPropertyNames);
end;

procedure TTestSchema.TestAdditionalProperties;
begin
  // additionalProperties keyword
  Schema.additionalProperties.MatchType:=smAny;
  CheckKeyword(jskAdditionalProperties);
end;

procedure TTestSchema.TestDependentSchemas;
begin
  // dependentSchemas keyword
  Schema.dependentSchemas.Add('name');
  CheckKeyword(jskDependentSchemas);
end;

procedure TTestSchema.TestContains;
begin
  // contains keyword
  Schema.PropertyNames.MatchType:=smAny;
  CheckKeyword(jskPropertyNames);
end;

procedure TTestSchema.TestUnevaluatedItems;
begin
  // unevaluatedItems keyword
  Schema.unevaluatedItems.MatchType:=smAny;
  CheckKeyword(jskunevaluatedItems);
end;

procedure TTestSchema.TestUnevaluatedProperties;
begin
  // unevaluatedProperties keyword
  Schema.unevaluatedProperties.MatchType:=smAny;
  CheckKeyword(jskUnevaluatedProperties);
end;

initialization
  RegisterTests([{TTestSchemaValue,}TTestMetadata,TTestSchemaValidations,TTestSchema]);
end.

