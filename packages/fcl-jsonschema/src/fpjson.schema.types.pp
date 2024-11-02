{
    This file is part of the Free Component Library

    JSON Schema basic types and helpers
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FpJson.Schema.Types;

{$mode ObjFPC}
{$H+}
{$modeswitch typehelpers}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

Type
  EJSONSchema = Class(exception);

  TJSONSchemaKeyword = (
    jskUnknown,
    jskId,
    jskAnchor,
    jskIdDraft4,
    jskSchema,
    jskDefs,
    jskTitle,
    jskDescription,
    jskDefault,
    jskMultipleOf,
    jskMaximum,
    jskExclusiveMaximum,
    jskMinimum,
    jskExclusiveMinimum,
    jskMaxLength,
    jskMinLength,
    jskPattern,
    jskAdditionalItems, //old
    jskItems,
    jskPrefixItems,
    jskMaxItems,
    jskMinItems,
    jskUniqueItems,
    jskMaxProperties,
    jskMinProperties,
    jskMaxContains,
    jskMinContains,
    jskRequired,
    jskDefinitions,
    jskProperties,
    jskPatternProperties,
    jskAdditionalProperties,
    jskPropertyNames,
    jskDependentSchemas,
    jskDependentRequired,
    jskEnum,
    jskType,
    jskAllOf,
    jskAnyOf,
    jskOneOf,
    jskNot,
    jskFormat,
    jskRef,
    jskIf,
    jskElse,
    jskThen,
    jskDynamicRef,
    jskDynamicAnchor,
    jskContains,
    jskComment,
    jskConst,
    jskUnevaluatedItems,
    jskUnevaluatedProperties,
    jskContentEncoding,
    jskContentMediaType,
    jskContentSchema,
    jskExamples,
    jskDeprecated,
    jskReadOnly,
    jskWriteOnly,
    jskVocabulary
  );
  TJSONSchemaKeywords = Set of TJSONSchemaKeyword;

  TJSONSubschema = (
   ssNot,
   ssIf,
   ssThen,
   ssElse,
   ssContains,
   ssUnevaluatedItems,
   ssUnevaluatedProperties,
   ssPropertyNames,
   ssAdditionalProperties
  );
  TJSONSubschemas = set of TJSONSubschema;

  TStringFormatValidator = (
   sfvCustom,
   sfvDatetime,
   sfvDate,
   sfvTime,
   sfvDuration,
   sfvEmail,
   sfvIdnEmail,
   sfvHostname,
   sfvIdnHostname,
   sfvIPV4,
   sfvIPV6,
   sfvURI,
   sfvURIReference,
   sfvIRI,
   sfvIRIReference,
   sfvUUID,
   sfvURITemplate,
   sfvJSONPointer,
   sfvRelativeJSONPointer,
   sfvRegex
  );
  TStringFormatValidators = set of TStringFormatValidator;

  TSchemaSimpleType = (
   sstNone,
   sstNull,
   sstBoolean,
   sstInteger,
   sstNumber,
   sstString,
   sstArray,
   sstObject,
   sstAny
  );
  TSchemaSimpleTypes = set of TSchemaSimpleType;

  TSchemaMatchType = (smAny,smNone,smConstrained); // corresponds to True, false or object schemas
  TSchemaMatchTypes = set of TSchemaMatchType;

  { TJSONSchemaKeywordHelper }

  TJSONSchemaKeywordHelper = Type helper for TJSONSchemaKeyword
    Function GetAsString: String;
    procedure SetAsString(const aValue: String);
    class function FromString(const aValue : String) : TJSONSchemaKeyword; static;
    property AsString : String Read GetAsString Write SetAsString;
  end;

  { TStringFormatValidatorHelper }

  TStringFormatValidatorHelper = Type helper for TStringFormatValidator
    Function GetAsString: String;
    procedure SetAsString(const aValue: String);
    class function FromString(const aValue : String) : TStringFormatValidator; static;
    property AsString : String Read GetAsString Write SetAsString;
  end;

  { TSchemaSimpleTypeHelper }
  TSchemaSimpleTypeHelper = Type Helper for TSchemaSimpleType
    Function GetAsString: String;
    procedure SetAsString(const aValue: String);
    class function FromString(const aValue : String) : TSchemaSimpleType; static;
    property AsString : String Read GetAsString Write SetAsString;
  end;

  TSchemaSimpleTypesHelper = Type Helper for TSchemaSimpleTypes
    Function ToString: String;
    property AsString : String Read ToString;
  end;


  { TJSONSubschemaHelper }

  TJSONSubschemaHelper = Type helper for TJSONSubschema
  private
    function GetAsSchemaKeyword: TJSONSchemaKeyword;
  Public
    Function GetAsString: String;
    procedure SetAsString(const aValue: String);
    class function FromString(const aValue : String) : TJSONSubschema; static;
    property AsString : String Read GetAsString Write SetAsString;
    property AsSchemaKeyword : TJSONSchemaKeyword Read GetAsSchemaKeyword;
  end;

const
  ValidatorKeywords = [
    jskType, jskconst, jskEnum, jskExclusiveMaximum, jskExclusiveMinimum, jskMaximum,
    jskMinimum, jskMaxItems, jskMinItems, jskRequired, jskMaxLength, jskMinLength, jskMaxProperties,
    jskMinProperties, jskPattern, jskUniqueItems, jskMinContains, jskMaxContains, jskMultipleOf,
    jskDependentRequired, jskFormat, jskcontentMediaType, jskcontentEncoding, jskcontentSchema];
  MetadataKeywords = [
    jskTitle,jskDescription,jskDefault,jskDeprecated,jskExamples,jskReadOnly,jskWriteOnly
    ];



implementation

uses fpjson.schema.consts;

const
  SimpleTypeNames : Array[TSchemaSimpleType] of string = (
    STNone,
    STNull,
    STBoolean,
    STInteger,
    STNumber,
    STString,
    STArray,
    STObject,
    STAny
  );

  KeywordNames : Array[TJSONSchemaKeyWord] of string = (
    SJKWUnknown, { jskUnknown }
    SJKWId, { jskId }
    SJKWAnchor, { jskAnchor }
    SJKWOldId, { jskId }
    SJKWSchema, { jskSchema }
    SJKWDefs, { jskDefs }
    SJKWTitle, { jskTitle }
    SJKWDescription, { jskDescription }
    SJKWDefault, { jskDefault }
    SJKWMultipleOf, { jskMultipleOf }
    SJKWMaximum, { jskMaximum }
    SJKWExclusiveMaximum, { jskExclusiveMaximum }
    SJKWMinimum, { jskMinimum }
    SJKWExclusiveMinimum, { jskExclusiveMinimum }
    SJKWMaxLength, { jskMaxLength }
    SJKWMinLength, { jskMinLength }
    SJKWPattern, { jskPattern }
    SJKWAdditionalItems, { jskAdditionalItems }
    SJKWItems, { jskItems }
    SJKWPrefixItems, { jskPrefixItems }
    SJKWMaxItems, { jskMaxItems }
    SJKWMinItems, { jskMinItems }
    SJKWUniqueItems, { jskUniqueItems }
    SJKWMaxProperties, { jskMaxProperties }
    SJKWMinProperties, { jskMinProperties }
    SJKWMaxContains,  { jskMaxContains }
    SJKWMinContains,  { jskMinContains }
    SJKWRequired, { jskRequired }
    SJKWDefinitions, { jskDefinitions }
    SJKWProperties, { jskProperties }
    SJKWPatternProperties, { jskPatternProperties }
    SJKWAdditionalProperties, { jskAdditionalProperties }
    SJKWPropertyNames, { jskPropertyNames  }
    SJKWDependentSchemas, { jskDependentSchemas }
    SJKWDependentRequired, { jskDependentRequired }
    SJKWEnum, { jskEnum }
    SJKWType, { jskType }
    SJKWAllOf, { jskAllOf }
    SJKWAnyOf, { jskAnyOf }
    SJKWOneOf, { jskOneOf }
    SJKWNot, { jskNot }
    SJKWFormat, { jskFormat }
    SJKWRef, { jskRef }
    SJKWIf, { jskIf }
    SJKWElse, { jskElse }
    SJKWThen, { jskThen }
    SJKWDynamicRef, { jskDynamicRef }
    SJKWDynamicAnchor,  { jskDynamicAnchor }
    SJKWContains,  { jskContains }
    SJKWComment, { jskComment }
    SJKWConst, { jskConst}
    SJKWUnevaluatedItems, { jskUnevaluatedItems }
    SJKWUnevaluatedProperties, {jskUnevaluatedProperties}
    SJKWContentEncoding, { jskContentEncoding }
    SJKWContentMediaType, { jskContentMediaType }
    SJKWContentSchema, { jskContentSchema }
    SJKWExamples, { jskExamples }
    SJKWDeprecated, { jskDeprecated}
    SJKWReadOnly, {jskReadOnly }
    SJKWWriteOnly, {jskWriteOnly }
    SJKWVocabulary { jskVocabulary }
  );

  JSONSubschemaKeys : Array[TJSONSubschema] of TJSONSchemaKeyWord = (
    jskNot,
    jskIf,
    jskThen,
    jskElse,
    jskContains,
    jskUnevaluatedItems,
    jskUnevaluatedProperties,
    jskPropertyNames,
    jskAdditionalProperties
  );

  StringFormatValidatorNames : Array[TStringFormatValidator] of string = (
    '',
    SFmtDatetime,
    SFmtDate,
    SFmtTime,
    SFmtDuration,
    SFmtEmail,
    SFmtIdnEmail,
    SFmtHostname,
    SFmtIdnHostname,
    SFmtIPV4,
    SFmtIPV6,
    SFmtURI,
    SFmtURIReference,
    SFmtIRI,
    SFmtIRIReference,
    SFmtUUID,
    SFmtURITemplate,
    SFmtJSONPointer,
    SFmtRelativeJSONPointer,
    SFmtRegex
  );

{ TJSONSchemaKeywordHelper }

function TJSONSchemaKeywordHelper.GetAsString: String;
begin
  Result:=KeyWordNames[Self];
end;

procedure TJSONSchemaKeywordHelper.SetAsString(const aValue: String);
var
  Kw : TJSONSchemaKeyword;
begin
  Self:=jskUnknown;
  for Kw in TJSONSchemaKeyword do
    if aValue=KeywordNames[kw] then
      begin
      Self:=kw;
      Exit;
      end;
end;

class function TJSONSchemaKeywordHelper.FromString(const aValue: String): TJSONSchemaKeyword;
begin
  Result:=jskUnknown;
  Result.SetAsString(aValue);
end;

{ TStringFormatValidatorHelper }

function TStringFormatValidatorHelper.GetAsString: String;
begin
  Result:=StringFormatValidatorNames[Self];
end;

procedure TStringFormatValidatorHelper.SetAsString(const aValue: String);

var
  sfv : TStringFormatValidator;
begin
  Self:=sfvCustom;
  For sfv in TStringFormatValidator do
    if StringFormatValidatorNames[sfv]=aValue then
      begin
      Self:=sfv;
      Exit;
      end;
end;

class function TStringFormatValidatorHelper.FromString(const aValue: String): TStringFormatValidator;
begin
  Result:=sfvCustom;
  Result.AsString:=aValue;
end;

{ TSchemaSimpleTypeHelper }

function TSchemaSimpleTypeHelper.GetAsString: String;
begin
  Result:=SimpleTypeNames[Self]
end;

procedure TSchemaSimpleTypeHelper.SetAsString(const aValue: String);
var
  st : TSchemaSimpleType;
begin
  Self:=sstNone;
  for ST in TSchemaSimpleType do
    if aValue=SimpleTypeNames[st] then
      begin
      Self:=st;
      Exit;
      end;
end;

class function TSchemaSimpleTypeHelper.FromString(const aValue: String): TSchemaSimpleType;
begin
  Result:=sstNone;
  Result.AsString:=aValue;
end;

function TSchemaSimpleTypesHelper.ToString: String;

var
  s : TSchemaSimpleType;

begin
  Result:='';
  For S in Self do
    begin
    if (Result<>'') then
      Result:=Result+',';
    Result:=Result+S.AsString;
    end;
end;

{ TJSONSubschemaHelper }

function TJSONSubschemaHelper.GetAsSchemaKeyword: TJSONSchemaKeyword;
begin
  Result:=JSONSubschemaKeys[Self];
end;

function TJSONSubschemaHelper.GetAsString: String;
begin
  Result:=GetAsSchemaKeyword.AsString;
end;

procedure TJSONSubschemaHelper.SetAsString(const aValue: String);

var
  Kw : TJSONSchemaKeyword;
  T : TJSONSubschema;
begin
  kw:=Default(TJSONSchemaKeyword);
  Kw.AsString:=aValue;
  For T in TJSONSubSchema do
    if JSONSubschemaKeys[T]=kw then
      begin
      Self:=T;
      exit;
      end;
end;

class function TJSONSubschemaHelper.FromString(const aValue: String): TJSONSubschema;
begin
  Result:=ssNot;
  Result.AsString:=aValue;
end;


end.

