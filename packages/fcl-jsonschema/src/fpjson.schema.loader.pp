{
    This file is part of the Free Component Library

    JSON Schema loader - load from JSON data
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpJson.Schema.Loader;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, FpJson.Schema.Schema, FpJson.Schema.Types;
  {$ELSE}
  Classes, SysUtils, fpjson, FpJson.Schema.Schema, FpJson.Schema.Types;
  {$ENDIF}

Type
  TJSONtypes = set of TJSONtype;
  EJsonSchemaLoader = class(EJSONSchema);

  { TJsonSchemaLoader }
  TKeywordInfo = Record
    Schema : TJSONSChema;
    Keyword : TJSONStringType;
    Value : TJSONData;
  end;

  TKeyWordHandler = Procedure(Sender : TObject; const Info : TKeywordInfo; var Handled: Boolean) of object;
  TSchemaLoadOption = (loSkipUnknownProperties);
  TSchemaLoadOptions = Set of TSchemaLoadOption;

  TJsonSchemaLoader = class(TComponent)
  private
    FCurrentKeyword : TJSONSchemaKeyword;
    FOnUnknownKeyword: TKeyWordHandler;
    FOptions: TSchemaLoadOptions;
    procedure ReadVocabulary(aData: TJSONData; aSchema: TJSONSchema);
  Protected
    class function JSONTypesToString(aTypes: TJSONTypes): string;
    function GetMatchType(aData : TJSONData) : TSchemaMatchType;
    // Check routines
    Procedure CheckType(aData : TJSONData; aType : TJSONtype; atKey : TJSONSchemaKeyword = jskUnknown);
    Procedure CheckType(aData : TJSONData; aType : TJSONtype; atPos : String);
    Procedure CheckType(aData : TJSONData; aTypes : TJSONtypes; atKey : TJSONSchemaKeyword = jskUnknown);
    Procedure CheckType(aData : TJSONData; aTypes : TJSONtypes; atPos : String);
    procedure InvalidType(aType: TJSONType);
    // Read simple values
    function ReadBoolean(aData : TJSONData): Boolean;
    function ReadNumber(aData : TJSONData): Double;
    function ReadPositiveInteger(aData : TJSONData): cardinal;
    function ReadString(aData : TJSONData): String;
    // Handle unknown props
    function HandleUnknownKeyWord(const aInfo : TKeywordInfo) : Boolean; virtual;
    // Read various special properties
    procedure ReadDependentRequired(aData: TJSONData; aList: TSchemaDependentRequiredList);
    procedure ReadArray(aData : TJSONData; aValues: TJSONArray);
    procedure ReadNamedSchemas(aData : TJSONData; ASchema: TJsonSchema; aList: TJSONSchemaList);
    procedure ReadItems(aData : TJSONData; ASchema: TJsonSchema);
    procedure ReadProperties(aData : TJSONData; ASchema: TJsonSchema);
    procedure ReadSchemaArray(aData : TJSONData; ASchema: TJsonSchema; ASchemaList: TJsonSchemaList);
    procedure ReadStringArray(aData : TJSONData; AStrings: TStrings);
    procedure ReadTypes(aData : TJSONData; AJsonSchema: TJsonSchema);
    // Main entry routines
    procedure ReadSchema(aData : TJSONData; ASchema: TJsonSchema);
    procedure ReadSchemaObject(aObject : TJSONObject; ASchema: TJsonSchema);
  public
    procedure ReadFromJSON(aSchema: TJSONSchema; aJSONData : TJSONData);
  Published
    property OnUnknownKeyword : TKeyWordHandler read FOnUnknownKeyword Write FOnUnknownKeyword;
    property Options : TSchemaLoadOptions Read FOptions Write FOptions;
  end;

implementation

uses FpJson.Schema.Consts;

{ ---------------------------------------------------------------------
  Auxiliary routines
  ---------------------------------------------------------------------}

procedure TJsonSchemaLoader.CheckType(aData: TJSONData; aType: TJSONtype; atKey: TJSONSchemaKeyword);

var
  Loc : String;

begin
  if aData.JSONType=aType then
    exit;
  Loc:=atkey.AsString;
  if Loc='' then
    Loc:=aData.asJSON;
  Raise EJsonSchemaLoader.CreateFmt(SErrUnexpectedType,[Loc,JSONTypeName(aType),JSONTypeName(aData.JSONType)]);
end;

procedure TJsonSchemaLoader.CheckType(aData: TJSONData; aType: TJSONtype; atPos: String);

begin
  if aData.JSONType=aType then
    exit;
  Raise EJsonSchemaLoader.CreateFmt(SErrUnexpectedType,[atPos,JSONTypeName(aType),JSONTypeName(aData.JSONType)]);
end;

class function TJsonSchemaLoader.JSONTypesToString(aTypes : TJSONTypes) : string;

var
  T : TJSONType;

begin
  Result:='';
  For T in TJSONType do
    if T in aTypes then
      begin
      if Result<>'' then
        Result:=Result+', ';
      Result:=Result+JSONTypeName(T);
      end;
end;

function TJsonSchemaLoader.GetMatchType(aData: TJSONData): TSchemaMatchType;
begin
  case aData.JSONType of
    jtBoolean:
      if aData.AsBoolean then
        Result:=smAny
      else
        Result:=smNone;
    jtObject:
      Result:=smConstrained;
  end;
end;

function TJsonSchemaLoader.HandleUnknownKeyWord(const aInfo: TKeywordInfo): Boolean;
begin
  Result:=False;
  if Assigned(FOnUnknownKeyword) then
    FOnUnknownKeyword(Self,aInfo,Result);
end;

procedure TJsonSchemaLoader.CheckType(aData: TJSONData; aTypes: TJSONtypes; atKey: TJSONSchemaKeyword);

var
  Types,Loc : String;

begin
  if aData.JSONType in aTypes then
    exit;
  Loc:=atkey.AsString;
  if Loc='' then
    Loc:=aData.asJSON;
  Types:=JSONTypesToString(aTypes);
  Raise EJsonSchemaLoader.CreateFmt(SErrUnexpectedTypeNotInSet,[Loc,Types,JSONTypeName(aData.JSONType)]);
end;

procedure TJsonSchemaLoader.CheckType(aData: TJSONData; aTypes: TJSONtypes; atPos: String);
var
  Types : String;

begin
  if aData.JSONType in aTypes then
    exit;
  Types:=JSONTypesToString(aTypes);
  Raise EJsonSchemaLoader.CreateFmt(SErrUnexpectedTypeNotInSet,[atPos,Types,JSONTypeName(aData.JSONType)]);
end;

procedure TJsonSchemaLoader.InvalidType(aType: TJSONType);
begin
  Raise EJsonSchemaLoader.CreateFmt(SErrInvalidType,[JSONTypeName(aType),FCurrentKeyWord.AsString]);
end;

function TJsonSchemaLoader.ReadBoolean(aData: TJSONData): Boolean;


begin
  CheckType(aData,jtBoolean,FCurrentKeyword);
  Result:=aData.AsBoolean;
end;

function TJsonSchemaLoader.ReadNumber(aData: TJSONData): Double;


begin
  CheckType(aData,jtNumber,FCurrentKeyword);
  Result:=aData.AsFloat;
end;

function TJsonSchemaLoader.ReadPositiveInteger(aData: TJSONData): cardinal;

var
  aValue : Int64;
begin
  CheckType(aData,jtNumber,FCurrentKeyword);
  if (TJSONNumber(aData).NumberType<>ntInteger) and (aData.AsFloat<>Trunc(aData.asFloat)) then
      raise EJsonSchemaLoader.CreateFmt(SErrNumberIsNotAnInteger, [FCurrentKeyword.AsString, aData.asJSON]);
  aValue:=aData.Asint64;
  if aValue<0 then
    raise EJsonSchemaLoader.CreateFmt(SErrIntegerIsNegative, [FCurrentKeyword.AsString, aValue]);
  Result:=aValue;
end;


{ ---------------------------------------------------------------------
  Actual reading of Schema
  ---------------------------------------------------------------------}

procedure TJsonSchemaLoader.ReadArray(aData: TJSONData; aValues: TJSONArray);

var
  i : Integer;

begin
  CheckType(aData,jtArray,FCurrentKeyWord);
  for I:=0 to aData.Count-1 do
    aValues.Add(aData.Items[I].clone);
end;


procedure TJsonSchemaLoader.ReadNamedSchemas(aData: TJSONData; ASchema: TJsonSchema; aList: TJSONSchemaList);
var
  Item : TJSONSchema;
  Enum : TJSONEnum;

begin
  CheckType(aData,jtObject,FCurrentKeyword);
  for Enum in aData do
    begin
    item:=aSchema.CreateChildSchema(Enum.Key);
    aList.Add(Item);
    ReadSchema(Enum.Value,Item);
    end;
end;

procedure TJsonSchemaLoader.ReadItems(aData: TJSONData; ASchema: TJsonSchema);

var
  item: TJsonSchema;

begin
  case aData.JSONType of
  jtBoolean,
  jtObject:
    begin
    item:=ASchema.CreateChildSchema('items');
    ASchema.Items.Add(Item);
    ReadSchema(TJSONObject(aData),Item);
    end;
  jtArray:
    begin
    ReadSchemaArray(aData, ASchema, ASchema.Items);
    end;
  else
    InvalidType(aData.JSONType);
  end;
end;


procedure TJsonSchemaLoader.ReadProperties(aData: TJSONData; ASchema: TJsonSchema);
var
  Item: TJsonSchema;
  Enum : TJSONEnum;

begin
  checkType(aData,jtObject,FCurrentKeyword);
  for Enum in aData do
    begin
    item:=aSchema.CreateChildSchema(Enum.key);
    aSchema.Properties.Add(Item);
    ReadSchema(Enum.Value,Item);
    end;
end;



procedure TJsonSchemaLoader.ReadSchema(aData: TJSONData; ASchema: TJsonSchema);


begin
  Case aData.JSONType of
    jtBoolean,
    jtObject:
      begin
      aSchema.MatchType:=GetMatchType(aData);
      if aData.JSONType=jtObject then
        ReadSchemaObject(TJSONObject(aData),aSchema);
      end
  else
    InvalidType(aData.JSONType);
  end;
end;

procedure TJsonSchemaLoader.ReadDependentRequired(aData : TJSONData; aList: TSchemaDependentRequiredList);

var
  Item : TSchemaDependentRequired;
  enum : TJSONEnum;

begin
  checkType(aData,jtObject,FCurrentKeyword);
  for Enum in aData do
    begin
    item:=aList.AddDependent(Enum.Key);
    ReadStringArray(Enum.Value,Item.Required);
    end;
end;

procedure TJsonSchemaLoader.ReadVocabulary(aData : TJSONData; aSchema: TJSONSchema);

var
  Enum : TJSONEnum;

begin
  CheckType(aData,jtObject);
  for Enum in aData do
    begin
    CheckType(enum.Value,jtBoolean,jskVocabulary);
    aSchema.Vocabulary.AddVocabulary(Enum.Key).Enabled:=Enum.Value.asBoolean;
    end;
end;

procedure TJsonSchemaLoader.ReadSchemaObject(aObject: TJSONObject; ASchema: TJsonSchema);

var
  Enum : TJSONEnum;
  aData : TJSONData;
  keyword : TJSONSchemaKeyword;
  Info : TKeywordInfo;

begin
  For Enum in aObject do
    begin
    keyWord:=TJSONSchemaKeyword.FromString(Enum.Key);
    aData:=Enum.Value;
    FCurrentKeyWord:=KeyWord;
    case KeyWord of
      // Older
      jskDefinitions,
      jskDefs: ReadNamedSchemas(aData,ASchema,aSchema.Defs);
      jskProperties: ReadProperties(aData,ASchema);
      jskDependentSchemas: ReadNamedSchemas(aData,ASchema,aSchema.DependentSchemas);
      jskType: ReadTypes(aData,ASchema);
      jskDescription: ASchema.MetaData.Description := ReadString(aData);
      jskTitle: ASchema.MetaData.Title := ReadString(aData);
      jskDefault: aSchema.MetaData.DefaultValue:=aData.Clone;
      jskSchema: ASchema.Schema := ReadString(aData);
      jskConst : ASchema.Validations.constValue:=aData.Clone;
      // Older
      jskIdDraft4,
      jskId: ASchema.Id := ReadString(aData);
      jskAnchor: ASchema.Anchor:= ReadString(aData);
      jskDynamicAnchor: ASchema.DynamicAnchor:= ReadString(aData);
      jskComment: ASchema.Comment:=ReadString(aData);
      jskMinimum: ASchema.Validations.Minimum := ReadNumber(aData);
      jskMaximum: ASchema.Validations.Maximum := ReadNumber(aData);
      jskMaxItems: ASchema.Validations.MaxItems := ReadPositiveInteger(aData);
      jskMinItems: ASchema.Validations.MinItems := ReadPositiveInteger(aData);
      jskMaxLength: ASchema.Validations.MaxLength := ReadPositiveInteger(aData);
      jskMinLength: ASchema.Validations.MinLength := ReadPositiveInteger(aData);
      jskMaxProperties: ASchema.Validations.MaxProperties := ReadPositiveInteger(aData);
      jskMinProperties: ASchema.Validations.MinProperties := ReadPositiveInteger(aData);
      jskMaxContains : aSchema.Validations.MaxContains:=ReadPositiveInteger(aData);
      jskMinContains : aSchema.Validations.MaxContains:=ReadPositiveInteger(aData);
      jskUniqueItems: ASchema.Validations.UniqueItems := ReadBoolean(aData);
      jskExclusiveMaximum: ASchema.Validations.ExclusiveMaximum := ReadNumber(aData);
      jskExclusiveMinimum: ASchema.Validations.ExclusiveMinimum := ReadNumber(aData);
      jskMultipleOf : ASchema.Validations.MultipleOf := ReadNumber(aData);
      jskEnum: ReadArray(aData,ASchema.Validations.Enum);
      jskItems: ReadItems(aData,ASchema);
      jskPrefixItems: ReadSchemaArray(aData,aSchema,aSchema.PrefixItems);
      // For backwards compatibility
      jskAdditionalItems: ReadItems(aData,aSchema);
      jskAdditionalProperties: ReadSchema(aData,ASchema.AdditionalProperties);
      jskPattern: ASchema.Validations.Pattern := ReadString(aData);
      jskPatternProperties: ReadNamedSchemas(aData,aSchema,ASchema.PatternProperties);
      jskRequired: ReadStringArray(aData,ASchema.Validations.Required);
      jskFormat: ASchema.Validations.Format := ReadString(aData);
      jskRef: ASchema.Ref := ReadString(aData);
      jskDynamicRef: ASchema.DynamicRef := ReadString(aData);
      jskAllOf: ReadSchemaArray(aData,ASchema, ASchema.AllOf);
      jskAnyOf: ReadSchemaArray(aData,ASchema, ASchema.AnyOf);
      jskOneOf: ReadSchemaArray(aData,ASchema, ASchema.OneOf);
      jskIf: ReadSchema(aData,ASchema.IfSchema);
      jskThen: ReadSchema(aData,ASchema.ThenSchema);
      jskElse: ReadSchema(aData,ASchema.ElseSchema);
      jskNot: ReadSchema(aData,ASchema.NotSchema);
      jskContains : ReadSchema(aData,ASchema.Contains);
      jskPropertyNames : ReadSchema(aData,aSchema.PropertyNames);
      jskDependentRequired: ReadDependentRequired(aData,aSchema.Validations.DependentRequired);
      jskUnevaluatedItems : ReadSchema(aData,aSchema.UnevaluatedItems);
      jskUnevaluatedProperties : ReadSchema(aData,aSchema.UnevaluatedProperties);
      jskContentEncoding : ASchema.Validations.ContentEncoding := ReadString(aData);
      jskContentMediaType  : ASchema.Validations.ContentMediaType := ReadString(aData);
      jskContentSchema : ReadSchema(aData,ASchema.Validations.ContentSchema);
      jskExamples : ReadArray(aData,aSchema.MetaData.Examples);
      jskDeprecated : aSchema.Metadata.Deprecated:=ReadBoolean(aData);
      jskReadOnly : aSchema.Metadata.ReadOnly:=ReadBoolean(aData);
      jskWriteOnly : aSchema.Metadata.WriteOnly:=ReadBoolean(aData);
      jskVocabulary : ReadVocabulary(aData,aSchema);
      jskUnknown:
        begin
        Info.Keyword:=Enum.Key;
        Info.Schema:=aSchema;
        Info.Value:=Enum.Value;
        if not HandleUnknownKeyWord(Info) then
          if Not (loSkipUnknownProperties in Options) then
            aSchema.UnknownKeywordData.Add(Info.Keyword,Info.Value.Clone);
        end;
      end;
    end;
end;

procedure TJsonSchemaLoader.ReadSchemaArray(aData: TJSONData; ASchema: TJsonSchema; ASchemaList: TJsonSchemaList);

var
  Item : TJsonSchema;
  Enum : TJSONEnum;

begin
  CheckType(aData,jtArray);
  For Enum in aData do
    begin
    Case Enum.Value.JSONType of
      jtBoolean,
      jtObject:
        begin
        Item:=ASchema.CreateChildSchema;
        ASchemaList.Add(Item);
        ReadSchema(TJSONObject(enum.Value),Item);
        end;
    else
      InvalidType(Enum.Value.JSONType);
    end;
    end;
end;

function TJsonSchemaLoader.ReadString(aData: TJSONData): String;

begin
  CheckType(aData,jtString);
  Result:=aData.AsString;
end;

procedure TJsonSchemaLoader.ReadStringArray(aData: TJSONData; AStrings: TStrings);

var
  Enum : TJSONEnum;

begin
  CheckType(aData,jtArray);
  for Enum in aData do
    begin
    CheckType(Enum.Value,jtString);
    AStrings.Add(Enum.Value.AsString);
    end;
end;

procedure TJsonSchemaLoader.ReadTypes(aData: TJSONData; AJsonSchema: TJsonSchema);
var
  aTypes : TSchemaSimpleTypes;
  Enum : TJSONEnum;

begin
  aTypes:=[];
  CheckType(aData,[jtString,jtArray]);
  for Enum in aData do
    begin
    CheckType(Enum.Value,jtString,FCurrentKeyword);
    Include(aTypes,TSchemaSimpleType.FromString(Enum.Value.AsString));
    end;
  AJSONSchema.Validations.Types:=aTypes;
end;

procedure TJsonSchemaLoader.ReadFromJSON(aSchema: TJSONSchema; aJSONData: TJSONData);
begin
  ReadSchema(aJSONData,aSchema);
end;


end.

