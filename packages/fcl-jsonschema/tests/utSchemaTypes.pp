{
    This file is part of the Free Component Library

    Testsuite for JSONSchema basic types
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utSchemaTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson.schema.types, fpjson.schema.testutils;

type

  { TTestSimpleTypes }

  TTestSimpleTypes = class(TSchemaTestcase)
  Public
  Published
    Procedure TestSchemaKeywordToString;
    Procedure TestJSONSubschemaToString;
    Procedure TestStringFormatValidatorToString;
    Procedure TestSchemaSimpleTypeToString;
  end;



implementation

uses typinfo,fpjson.schema.consts;

{ TTestSimpleTypes }


procedure TTestSimpleTypes.TestSchemaKeywordToString;

  Procedure TestAs(S : String; KW : TJSONSchemaKeyword);

  var
    tmp : TJSONSchemaKeyword;

  begin
    // kw -> string
    tmp:=KW;
    AssertEquals(S,S,tmp.AsString);
    // string -> KW
    tmp.AsString:=S;
    AssertEquals(S,KW,tmp);
  end;

begin
  TestAs('',jskUnknown);
  TestAs('$id',jskId);
  TestAs('$anchor',jskAnchor);
  TestAs('id',jskIdDraft4);
  TestAs('$schema',jskSchema);
  TestAs('$defs',jskDefs);
  TestAs('title',jskTitle);
  TestAs('description',jskDescription);
  TestAs('default',jskDefault);
  TestAs('multipleOf',jskMultipleOf);
  TestAs('maximum',jskMaximum);
  TestAs('exclusiveMaximum',jskExclusiveMaximum);
  TestAs('minimum',jskMinimum);
  TestAs('exclusiveMinimum',jskExclusiveMinimum);
  TestAs('maxLength',jskMaxLength);
  TestAs('minLength',jskMinLength);
  TestAs('pattern',jskPattern);
  TestAs('additionalItems',jskAdditionalItems);
  TestAs('items',jskItems);
  TestAs('prefixItems',jskPrefixItems);
  TestAs('maxItems',jskMaxItems);
  TestAs('minItems',jskMinItems);
  TestAs('uniqueItems',jskUniqueItems);
  TestAs('maxProperties',jskMaxProperties);
  TestAs('minProperties',jskMinProperties);
  TestAs('maxContains',jskMaxContains);
  TestAs('minContains',jskMinContains);
  TestAs('required',jskRequired);
  TestAs('additionalProperties',jskAdditionalProperties);
  TestAs('definitions',jskDefinitions);
  TestAs('properties',jskProperties);
  TestAs('patternProperties',jskPatternProperties);
  TestAs('propertyNames',jskPropertyNames);
  TestAs('dependentSchemas',jskDependentSchemas);
  TestAs('dependentRequired',jskDependentRequired);
  TestAs('enum',jskEnum);
  TestAs('type',jskType);
  TestAs('allOf',jskAllOf);
  TestAs('anyOf',jskAnyOf);
  TestAs('oneOf',jskOneOf);
  TestAs('not',jskNot);
  TestAs('format',jskFormat);
  TestAs('$ref',jskRef);
  TestAs('if',jskIf);
  TestAs('else',jskElse);
  TestAs('then',jskThen);
  TestAs('$dynamicRef',jskDynamicRef);
  TestAs('$dynamicAnchor',jskDynamicAnchor);
  TestAs('contains',jskContains);
  TestAs('$comment',jskComment);
  TestAs('const',jskConst);
  TestAs('unevaluatedItems',jskUnevaluatedItems);
  TestAs('unevaluatedProperties',jskUnevaluatedProperties);
  TestAs('contentEncoding',jskContentEncoding);
  TestAs('contentMediaType',jskContentMediaType);
  TestAs('contentSchema',jskContentSchema);
  TestAs('examples',jskExamples);
  TestAs('deprecated',jskDeprecated);
  TestAs('readOnly',jskReadOnly);
  TestAs('writeOnly',jskWriteOnly);
  TestAs('$vocabulary',jskVocabulary);
end;

procedure TTestSimpleTypes.TestJSONSubschemaToString;

  Procedure TestAs(S : String; sst : TJSONSubschema);

  var
    tmp : TJSONSubschema;

  begin
    // subschema -> string
    tmp:=sst;
    AssertEquals(S,S,tmp.AsString);
    // string -> subschema
    tmp.AsString:=S;
    AssertEquals(S,sst,tmp);
  end;

begin
  TestAs('not',ssNot);
  TestAs('if',ssIf);
  TestAs('then',ssThen);
  TestAs('else',ssElse);
  TestAs('contains',ssContains);
  TestAs('unevaluatedItems',ssUnevaluatedItems);
  TestAs('unevaluatedProperties',ssUnevaluatedProperties);
  TestAs('propertyNames',ssPropertyNames);
end;

procedure TTestSimpleTypes.TestStringFormatValidatorToString;

  Procedure TestAs(S : String; sfv : TStringFormatValidator);

  var
    tmp : TStringFormatValidator;

  begin
    // validator -> string
    tmp:=sfv;
    AssertEquals(S,S,tmp.AsString);
    // string -> validator
    tmp.AsString:=S;
    AssertEquals(S,sfv,tmp);
  end;

begin
  TestAs('date-time',sfvDatetime);
  TestAs('date',sfvDate);
  TestAs('time',sfvTime);
  TestAs('duration',sfvDuration);
  TestAs('email',sfvEmail);
  TestAs('idn-email',sfvIdnEmail);
  TestAs('hostname',sfvHostname);
  TestAs('idn-hostname',sfvIdnHostname);
  TestAs('ipv4',sfvIPV4);
  TestAs('ipv6',sfvIPV6);
  TestAs('uri',sfvURI);
  TestAs('uri-reference',sfvURIReference);
  TestAs('iri',sfvIRI);
  TestAs('iri-reference',sfvIRIReference);
  TestAs('uuid',sfvUUID);
  TestAs('uri-template',sfvURITemplate);
  TestAs('json-pointer',sfvJSONPointer);
  TestAs('relative-json-pointer',sfvRelativeJSONPointer);
  TestAs('regex',sfvRegex);
end;

procedure TTestSimpleTypes.TestSchemaSimpleTypeToString;

  Procedure TestAs(S : String; sst : TSchemaSimpleType);

  var
    tmp : TSchemaSimpleType;

  begin
    // simpletype -> string
    tmp:=sst;
    AssertEquals(S,S,tmp.AsString);
    // string -> simpletype
    tmp.AsString:=S;
    AssertEquals(S,sst,tmp);
  end;


begin
  TestAs('',sstNone);
  TestAs('null',sstNull);
  TestAs('boolean',sstBoolean);
  TestAs('integer',sstInteger);
  TestAs('number',sstNumber);
  TestAs('string',sstString);
  TestAs('array',sstArray);
  TestAs('object',sstObject);
  TestAs('any',sstAny);
end;

initialization
  RegisterTest(TTestSimpleTypes);
end.

