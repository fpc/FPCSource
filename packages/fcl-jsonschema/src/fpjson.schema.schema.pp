{
    This file is part of the Free Component Library

    JSON Schema class
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpjson.schema.schema;

{$mode ObjFPC}
{$H+}
{$modeswitch typehelpers}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, FpJson.Data, FpJson.Schema.Types;
  {$ELSE}
  Classes, SysUtils, contnrs, fpjson, fpjson.schema.types;
  {$ENDIF}

Type
  TJSONSchema = class;
  TJSONSchemaList = class;
  TJSONSchemaValidations = class;
  TJSONSchemaVocabulary = class;
  TJSONSchemaVocabularyList= class;

  { TJSONSchemaMetadata }

  TJSONSchemaMetadata = Class(TPersistent)
  Private
    FSchema: TJSONSchema;
    FTitle: String;
    FDescription: String;
    FDefaultValue: TJSONData;
    FDeprecated : Boolean;
    FExamples : TJSONArray;
    FReadOnly : Boolean;
    FWriteOnly : Boolean;
    FKeywordData : TJSONSchemaKeywords;
    procedure DoAddExample(Sender: TObject);
    procedure SetDefaultValue(AValue: TJSONData);
    procedure SetDeprecated(AValue: Boolean);
    procedure SetDescription(AValue: String);
    procedure SetExamples(AValue: TJSONArray);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetTitle(AValue: String);
    procedure SetWriteOnly(AValue: Boolean);
  Protected
    procedure SetConstrained; inline;
    Procedure SetKeywordData(aKeyword: TJSONSchemaKeyword);
    Procedure UnSetKeywordData(aKeyword: TJSONSchemaKeyword);
    function GetOwner: TPersistent; override;
    // List of possible keywords for this class
    function Keywords : TJSONSchemaKeywords; virtual;
  Public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aSchema : TJSONSchema); virtual;
    destructor Destroy; override;
    // List of keywords for which data is available
    function KeywordsWithData : TJSONSchemaKeywords; virtual;
    // Is this keyword set ?
    function HasKeywordData(aKeyword : TJSONSchemaKeyword) : boolean; virtual;
    property Schema : TJSONSchema Read FSchema;
  Published
    Property Title: String Read FTitle write SetTitle;
    Property Description: String Read FDescription write SetDescription;
    Property DefaultValue: TJSONData Read FDefaultValue Write SetDefaultValue;
    Property Deprecated: Boolean Read FDeprecated write SetDeprecated;
    Property Examples: TJSONArray Read FExamples Write SetExamples;
    Property ReadOnly: Boolean Read FReadOnly write SetReadOnly;
    Property WriteOnly: Boolean Read FWriteOnly write SetWriteOnly;
  end;

  { TSchemaDependentRequired }
  TSchemaDependentRequired = Class(TCollectionItem)
  private
    FRequired: TStrings;
    FURL: String;
    procedure SetRequired(AValue: TStrings);
  Public
    Constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
  Published
    Property Name : String Read FURL Write FURL;
    Property Required : TStrings Read FRequired Write SetRequired;
  end;

  { TSchemaDependentRequiredList }

  TSchemaDependentRequiredList = Class(TOwnedCollection)
  private
    function GetDependent(aIndex : integer): TSchemaDependentRequired;
    function GetValidations: TJSONSchemaValidations;
    procedure SetDependent(aIndex : integer; AValue: TSchemaDependentRequired);
  Public
    Property SchemaValidations : TJSONSchemaValidations Read GetValidations;
    function AddDependent(const aName : String) : TSchemaDependentRequired;
    Property Dependent[aIndex : integer] : TSchemaDependentRequired Read GetDependent Write SetDependent; default;
  end;

  { TJSONSchemaValidations }

  TJSONSchemaValidations = Class(TPersistent)
  private
    FConstValue: TJSONData;
    FcontentEncoding: String;
    FcontentMediaType: String;
    FcontentSchema: TJSONSchema;
    FDependentRequired: TSchemaDependentRequiredList;
    FEnum: TJSONArray;
    FExclusiveMaximum: Double;
    FExclusiveMinimum: Double;
    FFormat: String;
    FMaxContains: Integer;
    FMaximum: Double;
    FMaxItems: Cardinal;
    FMaxLength: Cardinal;
    FMaxProperties: Cardinal;
    FMinContains: Integer;
    FMinimum: Double;
    FMinItems: Cardinal;
    FMinLength: Cardinal;
    FMinProperties: Cardinal;
    FMultipleOf: Double;
    FPattern: String;
    FRequired: TStrings;
    FSchema: TJSONSchema;
    FTypes: TSchemaSimpleTypes;
    FUniqueItems: Boolean;
    FKeywordData : TJSONSchemaKeywords;
    procedure DoAdd(Sender: TObject);
    procedure DoRequiredChange(Sender: TObject);
    function GetContentSchema: TJSONSchema;
    function GetFormatValidator: TStringFormatValidator;
    procedure SetConstValue(AValue: TJSONData);
    procedure SetcontentEncoding(AValue: String);
    procedure SetcontentMediaType(AValue: String);
    procedure SetEnum(AValue: TJSONArray);
    procedure SetExclusiveMaximum(AValue: Double);
    procedure SetExclusiveMinimum(AValue: Double);
    procedure SetFormat(AValue: String);
    procedure SetFormatValidator(AValue: TStringFormatValidator);
    procedure SetMaxContains(AValue: Integer);
    procedure SetMaximum(AValue: Double);
    procedure SetMaxItems(AValue: Cardinal);
    procedure SetMaxLength(AValue: Cardinal);
    procedure SetMaxProperties(AValue: Cardinal);
    procedure SetMinContains(AValue: Integer);
    procedure SetMinimum(AValue: Double);
    procedure SetMinItems(AValue: Cardinal);
    procedure SetMinLength(AValue: Cardinal);
    procedure SetMinProperties(AValue: Cardinal);
    procedure SetMultipleOf(AValue: Double);
    procedure SetPattern(AValue: String);
    procedure SetRequired(AValue: TStrings);
    procedure SetTypes(AValue: TSchemaSimpleTypes);
    procedure SetUniqueItems(AValue: Boolean);
  Protected
    procedure SetConstrained;inline;
    procedure SetKeywordData(aKeyword: TJSONSchemaKeyword); virtual;
    procedure UnSetKeywordData(aKeyword: TJSONSchemaKeyword); virtual;
    Function CreateDependentRequired : TSchemaDependentRequiredList; virtual;
    function GetOwner: TPersistent; override;
    function Keywords : TJSONSchemaKeywords; virtual;
  Public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(aSchema : TJSONSchema);
    Destructor Destroy; override;
    // List of keywords for which data is available
    function KeywordsWithData : TJSONSchemaKeywords; virtual;
    // Is the keyword set
    function HasKeywordData(aKeyword : TJSONSchemaKeyword) : Boolean; virtual;
    // Count types
    Function TypesCount : Integer;
    // First type (in order of TSchemaSimpleType
    Function GetFirstType : TSchemaSimpleType;
    // Owner schema
    property Schema : TJSONSchema Read FSchema;
    // type keyword
    property Types: TSchemaSimpleTypes read FTypes Write SetTypes;
    property constValue : TJSONData Read FConstValue Write SetConstValue;
    property Enum: TJSONArray read FEnum Write SetEnum;
    property ExclusiveMaximum: Double read FExclusiveMaximum write SetExclusiveMaximum;
    property ExclusiveMinimum: Double read FExclusiveMinimum write SetExclusiveMinimum;
    property Maximum: Double read FMaximum write SetMaximum;
    property Minimum: Double read FMinimum write SetMinimum;
    property MaxItems: Cardinal read FMaxItems write SetMaxItems;
    property MinItems: Cardinal read FMinItems write SetMinItems;
    property Required: TStrings read FRequired write SetRequired;
    property MaxLength: Cardinal read FMaxLength write SetMaxLength;
    property MinLength: Cardinal read FMinLength write SetMinLength;
    property MaxProperties: Cardinal read FMaxProperties write SetMaxProperties;
    property MinProperties: Cardinal read FMinProperties write SetMinProperties;
    property Pattern: String read FPattern write SetPattern;
    property UniqueItems: Boolean read FUniqueItems write SetUniqueItems;
    property MinContains : Integer Read FMinContains Write SetMinContains;
    property MaxContains : Integer Read FMaxContains Write SetMaxContains;
    property MultipleOf : Double Read FMultipleOf Write SetMultipleOf;
    Property DependentRequired : TSchemaDependentRequiredList Read FDependentRequired;
    // Probably better under annotations...
    property Format: String read FFormat write SetFormat;
    property FormatValidator: TStringFormatValidator read GetFormatValidator write SetFormatValidator;
    property contentMediaType : String read FcontentMediaType write SetcontentMediaType;
    property contentEncoding : String read FcontentEncoding write SetcontentEncoding;
    property contentSchema : TJSONSchema read GetContentSchema;
  end;

  { TJSONSchemaVocabulary }

  TJSONSchemaVocabulary = Class(TCollectionItem)
  private
    FEnabled: Boolean;
    FURL: String;
  Public
    procedure Assign(Source: TPersistent); override;
    function ToString : String; override;
  Published
    Property URL : String Read FURL Write FUrl;
    Property Enabled : Boolean Read FEnabled Write FEnabled;
  end;

  { TJSONSChemaVocabularyList }

  TJSONSchemaVocabularyList = Class(TOwnedCollection)
  private
    function GetSchema: TJSONSchema;
    function GetVocabulary(aIndex : integer): TJSONSchemaVocabulary;
    procedure SetVocabulary(aIndex : integer; AValue: TJSONSchemaVocabulary);
  Public
    function IndexOfVocabulary(const aURL : String) : Integer;
    function FindVocabulary(aURL : String) : TJSONSchemaVocabulary;
    Function AddVocabulary(const aURL : String) : TJSONSchemaVocabulary;
    Property Schema : TJSONSchema Read GetSchema;
    Property Vocabularies[aIndex : integer] : TJSONSchemaVocabulary Read GetVocabulary Write SetVocabulary; default;
    function ToString : String; overload;
  end;

  { TJsonSchema }

  TJsonSchema = class(TPersistent)
  private
    FAnchor: String;
    FComment: String;
    FMatchType: TSchemaMatchType;
    FParent: TJsonSchema;
    FSubSchemas : Array[TJSONSubschema] of TJSONSchema;
    FPatternProperties : TJsonSchemaList;
    FPrefixItems: TJsonSchemaList;
    FProperties: TJsonSchemaList;
    FItems: TJsonSchemaList;
    FAllOf: TJsonSchemaList;
    FAnyOf: TJsonSchemaList;
    FOneOf: TJsonSchemaList;
    FDependentSchemas: TJSONSchemaList;
    FDefs: TJSONSchemaList;
    FDynamicAnchor: String;
    FDynamicRef: String;
    FMetaData: TJSONSchemaMetadata;
    FName: String;
    FId: String;
    FSchema: String;
    FRef: String;
    FAdditionalProperties: TJSONSchema;

    FValidations: TJsonSchemaValidations;
    FVocabulary: TJSONSchemaVocabularyList;
    FUnknownKeywordData : TJSONObject;
    FChildren : TFPList;
    FKeywordData : TJSONSchemaKeywords;
    function GetAdditionalProperties: TJSONSchema;
    function GetChildSchema(aIndex : Integer): TJSONSchema;
    function GetChildSchemaCount: Integer;
    function GetDependentSchemas: TJSONSchemaList;
    function GetNamedList(const aName: string): TJSONSchemaList;
    function GetPatternProperties: TJsonSchemaList;
    function GetUnknownKeywordData: TJSONObject;
    procedure SetMetadata(AValue: TJSONSchemaMetadata);
    procedure SetString(AIndex: Integer; AValue: String);
    procedure SetValidations(AValue: TJsonSchemaValidations);
    procedure SetVocabulary(AValue: TJSONSchemaVocabularyList);
  protected
    procedure SetKeyWordData(akeyWord : TJSONSchemaKeyword);
    procedure UnsetKeyWordData(akeyWord : TJSONSchemaKeyword);
    function GetSubSchema(aIndex : Integer) : TJSONSchema;
    function CreateVocabulary : TJSONSchemaVocabularyList; virtual;
    function CreateMetadata: TJSONSchemaMetadata; virtual;
    function CreateValidations: TJsonSchemaValidations; virtual;
    procedure CheckConstrained;
    function GetPath: String; virtual;
  public
    constructor Create(AParent: TJsonSchema);
    constructor Create;
    destructor Destroy; override;
    // Check if there is data for a given keyword.
    Function HasKeywordData(aKeyword : TJSONSchemaKeyword) : Boolean;
    // All set keywords
    Function KeywordsWithData : TJSONSchemaKeywords;
    // Create child schema with given name
    function CreateChildSchema(aName : string): TJsonSchema; overload;
    // Create child schema with name equal to keyword.
    function CreateChildSchema(aKeyword : TJSONSchemaKeyword): TJsonSchema; overload;
    // Create unnamed child schema
    function CreateChildSchema: TJsonSchema; virtual;
    // Toplevel schema (follows parent)
    Function RootSchema: TJSONSchema;
    // Find schema using schema-local $Ref URI
    function Find(const aPath : String) : TJSONSchema;
    // Find index of direct child schema with given name
    function IndexOfChild(const aName: String): Integer;
    // Find direct child schema with given name
    function FindChild(const aName: String): TJSONSchema;
    // Enumerate child schemas (named and unnamed)
    property ChildSchemas[aIndex : Integer] : TJSONSchema Read GetChildSchema;
    // Number of child schemas (named and unnamed)
    property ChildSchemaCount : Integer Read GetChildSchemaCount;
    // Parent schema
    Property Parent : TJSONSchema Read FParent;
    // Path till root schema.
    Property Path : String Read GetPath;
    // Name of current schema in parent.
    property Name: String read FName write FName;

     { Core vocabulary }
    // Was the schema read as True/False/Object
    property MatchType : TSchemaMatchType Read FMatchType Write FMatchType;
    // Identifier of used JSON schema
    property Schema: String Index Ord(jskSchema) read FSchema write SetString;
    // ID of this schema
    property Id: String Index Ord(jskID) read FId write SetString;
    // $ref
    property Ref: String Index Ord(jskRef) read FRef write SetString;
    // $comment
    property Comment: String Index Ord(jskComment) read FComment write SetString;
    // $anchor
    property Anchor: String Index Ord(jskAnchor) read FAnchor write SetString;
    // $dynamicAnchor
    property DynamicAnchor: String Index Ord(jskDynamicAnchor) read FDynamicAnchor write SetString;
    // $dynamicRef
    property DynamicRef: String Index Ord(jskDynamicRef) read FDynamicRef write SetString;
    // $vocabulary
    property Vocabulary : TJSONSchemaVocabularyList Read FVocabulary Write SetVocabulary;
    // $defs
    property Defs: TJSONSchemaList read FDefs;

    // Metadata vocabulary keywords
    Property MetaData : TJSONSchemaMetadata Read FMetaData Write SetMetadata;
    // Validations vocabulary keywords
    Property Validations : TJsonSchemaValidations Read FValidations Write SetValidations;
    // Applicator vocabulary keywords
    // allOf keyword
    property AllOf: TJsonSchemaList read FAllOf;
    // anyOf keyword
    property AnyOf: TJsonSchemaList read FAnyOf;
    // OneOf keyword
    property OneOf: TJsonSchemaList read FOneOf;
    // Not keyword
    property NotSchema: TJsonSchema index Ord(ssNot) read GetSubSchema;
    // if keyword
    property IfSchema: TJsonSchema index Ord(ssIf) read GetSubSchema;
    // Then keyword
    property ThenSchema: TJsonSchema index Ord(ssThen) read GetSubSchema;
    // Else keyword
    property ElseSchema: TJsonSchema index Ord(ssElse) read GetSubSchema;
    // properties keyword
    property Properties: TJsonSchemaList read FProperties;
    // items keyword.
    // Declared in draft 2020-12 as schema, but we keep it a List, so we can handle earlier drafts.
    property Items: TJsonSchemaList read FItems;
    // prefixItems keyword
    property PrefixItems: TJsonSchemaList read FPrefixItems;
    // patternProperties keyword
    property PatternProperties: TJsonSchemaList Read GetPatternProperties;
    // propertyNames keyword
    property PropertyNames: TJsonSchema index Ord(ssPropertyNames) read GetSubSchema;
    // additionalProperties keyword
    property AdditionalProperties: TJSONSchema index Ord(ssAdditionalProperties) read GetSubschema;
    // dependentSchemas keyw
    property DependentSchemas: TJSONSchemaList read GetDependentSchemas;
    // contains keyword
    property Contains: TJsonSchema Index Ord(ssContains) read GetSubSchema;
    // unevaluatedItems keyword
    property UnevaluatedItems : TJSONSchema Index Ord(ssUnevaluatedItems) read GetSubSchema;
    // unevaluatedProperties keyword
    property UnevaluatedProperties : TJSONSchema Index Ord(ssUnevaluatedProperties) read GetSubSchema;
    // Not in any vocabulary:
    // Can be filled by reader with unknown properties.
    property UnknownKeywordData : TJSONObject Read GetUnknownKeywordData;
  end;
  TJsonSchemaClass = Class of TJsonSchema;

  { TJSONSchemaList }

  TJSONSchemaList = Class(TFPObjectList)
  private
    FKeyword: TJSONSchemaKeyword;
    FSchemaOwner: TJSONSchema;
    function GetSchema(aIndex : integer): TJSONSchema;
    procedure SetSchema(aIndex : integer; AValue: TJSONSchema);
  Public
    Constructor Create(aOwner : TJSONSchema; aKeyWord : TJSONSchemaKeyword); overload;
    Function FindIDOrNames(aPath : String) : TJSONSchema;
    Function Add(const aName : string = ''): TJSONSchema; overload;
    Function Add(Schema : TJSONSchema): Integer; overload;
    property SchemaOwner : TJSONSchema Read FSchemaOwner;
    property Keyword : TJSONSchemaKeyword Read FKeyword;
    Property Schemas[aIndex : integer] : TJSONSchema Read GetSchema Write SetSchema; default;
  end;

implementation

{ TJSONSchemaMetadata }

procedure TJSONSchemaMetadata.SetConstrained;

begin
  if Assigned(Schema) then
    Schema.MatchType:=smConstrained;
end;

procedure TJSONSchemaMetadata.DoAddExample(Sender: TObject);
begin
  SetConstrained;
end;

procedure TJSONSchemaMetadata.SetDefaultValue(AValue: TJSONData);
begin
  if FDefaultValue=AValue then Exit;
  FreeAndNil(FDefaultValue);
  FDefaultValue:=AValue;
  if Assigned(FDefaultValue) then
    SetKeywordData(jskDefault);
end;

procedure TJSONSchemaMetadata.SetDeprecated(AValue: Boolean);
begin
  SetKeywordData(jskDeprecated);
  if FDeprecated=AValue then Exit;
  FDeprecated:=AValue;
end;

procedure TJSONSchemaMetadata.SetDescription(AValue: String);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
  SetKeywordData(jskDescription);
end;

procedure TJSONSchemaMetadata.SetExamples(AValue: TJSONArray);
begin
  if FExamples=AValue then Exit;
  FreeAndNil(FExamples);
  FExamples:=AValue;
  if FExamples=Nil then
    FExamples:=TJSONArray.Create;
  SetKeywordData(jskExamples);
end;

procedure TJSONSchemaMetadata.SetReadOnly(AValue: Boolean);
begin
  SetKeywordData(jskReadOnly);
  if FReadOnly=AValue then Exit;
  FReadOnly:=AValue;
end;

procedure TJSONSchemaMetadata.SetTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  SetKeywordData(jskTitle);
end;

procedure TJSONSchemaMetadata.SetWriteOnly(AValue: Boolean);
begin
  SetKeywordData(jskWriteOnly);
  if FWriteOnly=AValue then Exit;
  FWriteOnly:=AValue;
end;

procedure TJSONSchemaMetadata.SetKeywordData(aKeyword: TJSONSchemaKeyword);
begin
  Include(FKeywordData,aKeyword);
  SetConstrained;
end;

procedure TJSONSchemaMetadata.UnSetKeywordData(aKeyword: TJSONSchemaKeyword);
begin
  Exclude(FKeywordData,aKeyword);
  if Assigned(Schema) then
    Schema.CheckConstrained;
end;

procedure TJSONSchemaMetadata.Assign(Source: TPersistent);
var
  aSource: TJSONSchemaMetadata absolute source;
begin
  if Source is TJSONSchemaMetadata then
    begin
    FKeywordData:=[];
    WriteOnly:=aSource.FWriteOnly;
    Title:=aSource.FTitle;
    ReadOnly:=aSource.FReadOnly;
    Description:=aSource.FDescription;
    Deprecated:=aSource.FDeprecated;
    DefaultValue:=aSource.DefaultValue.Clone;
    Examples:=aSource.Examples.Clone as TJSONArray;
    end
  else
    inherited Assign(Source);
end;

constructor TJSONSchemaMetadata.Create(aSchema: TJSONSchema);
begin
  FSchema:=aSchema;
  Fexamples:=TJSONArray.Create;
end;

destructor TJSONSchemaMetadata.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDefaultValue);
  FreeAndNil(FExamples);
end;

function TJSONSchemaMetadata.GetOwner: TPersistent;
begin
  Result:=FSchema;
end;

function TJSONSchemaMetadata.Keywords: TJSONSchemaKeywords;
begin
  Result:=MetadataKeywords;
end;

function TJSONSchemaMetadata.KeywordsWithData: TJSONSchemaKeywords;

var
  K : TJSONSchemaKeyword;

begin
  Result:=[];
  For K in Keywords do
    if HasKeywordData(K) then
      Include(Result,K);
end;

function TJSONSchemaMetadata.HasKeywordData(aKeyword: TJSONSchemaKeyword): boolean;
begin
  Case aKeyword of
    jskDefault: Result:=Assigned(FDefaultValue);
    jskExamples : Result:=(FExamples.Count>0);
  else
    Result:=aKeyword in FKeywordData;
  end;
end;


{ TSchemaDependentRequired }

procedure TSchemaDependentRequired.SetRequired(AValue: TStrings);
begin
  if FRequired=AValue then Exit;
  FRequired.Assign(AValue);
end;

constructor TSchemaDependentRequired.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FRequired:=TStringList.Create;
end;

destructor TSchemaDependentRequired.Destroy;
begin
  FreeAndNil(FRequired);
  inherited Destroy;
end;

procedure TSchemaDependentRequired.Assign(Source: TPersistent);

var
  Src :TSchemaDependentRequired absolute Source;

begin
  if Source is TSchemaDependentRequired then
    begin
    Name:=Src.Name;
    Required.Assign(Src.Required);
    end
  else
    inherited Assign(Source);
end;

{ TSchemaDependentRequiredList }

function TSchemaDependentRequiredList.GetDependent(aIndex : integer): TSchemaDependentRequired;
begin
  Result:=Items[aIndex] as TSchemaDependentRequired;
end;

function TSchemaDependentRequiredList.GetValidations: TJSONSchemaValidations;
begin
  if owner is TJSONSchemaValidations then
    Result:=TJSONSchemaValidations(Owner)
  else
    Result:=Nil;
end;

procedure TSchemaDependentRequiredList.SetDependent(aIndex : integer; AValue: TSchemaDependentRequired);
begin
  Items[aIndex]:=aValue;
end;

function TSchemaDependentRequiredList.AddDependent(const aName: String): TSchemaDependentRequired;
begin
  Result:=Add as TSchemaDependentRequired;
  Result.Name:=aName;
  if Assigned(SchemaValidations) and Assigned(SchemaValidations.Schema) then
    SchemaValidations.Schema.MatchType:=smConstrained;
end;

{ TJsonSchemaValidations }

procedure TJSONSchemaValidations.SetConstValue(AValue: TJSONData);
begin
  if FConstValue=AValue then Exit;
  FreeAndNil(FConstValue);
  FConstValue:=AValue;
  SetKeyWordData(jskConst);
end;

procedure TJSONSchemaValidations.SetcontentEncoding(AValue: String);
begin
  if FcontentEncoding=AValue then Exit;
  FcontentEncoding:=AValue;
  SetKeywordData(jskContentEncoding);
end;

procedure TJSONSchemaValidations.SetcontentMediaType(AValue: String);
begin
  if FcontentMediaType=AValue then Exit;
  FcontentMediaType:=AValue;
  SetKeywordData(jskContentMediaType);
end;

procedure TJSONSchemaValidations.SetEnum(AValue: TJSONArray);
begin
  if FEnum=AValue then Exit;
  FreeAndNil(FEnum);
  FEnum:=AValue;
  if FEnum=Nil then
    FEnum:=TJSONArray.Create;
  SetKeywordData(jskEnum);
end;

procedure TJSONSchemaValidations.SetExclusiveMaximum(AValue: Double);
begin
  if FExclusiveMaximum=AValue then Exit;
  FExclusiveMaximum:=AValue;
  SetKeywordData(jskExclusiveMaximum);
end;

procedure TJSONSchemaValidations.SetExclusiveMinimum(AValue: Double);
begin
  if FExclusiveMinimum=AValue then Exit;
  FExclusiveMinimum:=AValue;
  SetKeywordData(jskExclusiveMinimum);
end;

procedure TJSONSchemaValidations.SetFormat(AValue: String);
begin
  if FFormat=AValue then Exit;
  FFormat:=AValue;
  SetKeywordData(jskFormat);
end;

procedure TJSONSchemaValidations.SetFormatValidator(AValue: TStringFormatValidator);
begin
  Format:=aValue.AsString;
end;

procedure TJSONSchemaValidations.SetMaxContains(AValue: Integer);
begin
  if FMaxContains=AValue then Exit;
  FMaxContains:=AValue;
  SetKeywordData(jskMaxContains);
end;

procedure TJSONSchemaValidations.SetMaximum(AValue: Double);
begin
  if FMaximum=AValue then Exit;
  FMaximum:=AValue;
  SetKeywordData(jskMaximum);
end;

procedure TJSONSchemaValidations.SetMaxItems(AValue: Cardinal);
begin
  if FMaxItems=AValue then Exit;
  FMaxItems:=AValue;
  SetKeywordData(jskMaxItems);
end;

procedure TJSONSchemaValidations.SetMaxLength(AValue: Cardinal);
begin
  if FMaxLength=AValue then Exit;
  FMaxLength:=AValue;
  SetKeywordData(jskMaxLength);
end;

procedure TJSONSchemaValidations.SetMaxProperties(AValue: Cardinal);
begin
  if FMaxProperties=AValue then Exit;
  FMaxProperties:=AValue;
  SetKeywordData(jskMaxProperties);
end;

procedure TJSONSchemaValidations.SetMinContains(AValue: Integer);
begin
  if FMinContains=AValue then Exit;
  FMinContains:=AValue;
  SetKeywordData(jskMinContains);
end;

procedure TJSONSchemaValidations.SetMinimum(AValue: Double);
begin
  if FMinimum=AValue then Exit;
  FMinimum:=AValue;
  SetKeywordData(jskMinimum);
end;

procedure TJSONSchemaValidations.SetMinItems(AValue: Cardinal);
begin
  if FMinItems=AValue then Exit;
  FMinItems:=AValue;
  SetKeywordData(jskMinItems);
end;

procedure TJSONSchemaValidations.SetMinLength(AValue: Cardinal);
begin
  if FMinLength=AValue then Exit;
  FMinLength:=AValue;
  SetKeywordData(jskMinLength);
end;

procedure TJSONSchemaValidations.SetMinProperties(AValue: Cardinal);
begin
  if FMinProperties=AValue then Exit;
  FMinProperties:=AValue;
  SetKeywordData(jskMinProperties);
end;

procedure TJSONSchemaValidations.SetMultipleOf(AValue: Double);
begin
  if FMultipleOf=AValue then Exit;
  FMultipleOf:=AValue;
  SetKeywordData(jskMultipleOf);
end;

procedure TJSONSchemaValidations.SetPattern(AValue: String);
begin
  if FPattern=AValue then Exit;
  FPattern:=AValue;
  SetKeywordData(jskPattern);
end;

procedure TJSONSchemaValidations.SetRequired(AValue: TStrings);
begin
  if FRequired=AValue then Exit;
  FRequired.Assign(AValue);
  SetKeywordData(jskRequired);
end;

procedure TJSONSchemaValidations.SetTypes(AValue: TSchemaSimpleTypes);
begin
  if FTypes=AValue then Exit;
  FTypes:=AValue;
  SetKeywordData(jskType);
end;

procedure TJSONSchemaValidations.SetUniqueItems(AValue: Boolean);
begin
  if FUniqueItems=AValue then Exit;
  FUniqueItems:=AValue;
  SetKeywordData(jskUniqueItems);
end;

procedure TJSONSchemaValidations.SetConstrained;
begin
  if Assigned(Schema) then
    Schema.MatchType:=smConstrained;
end;

procedure TJSONSchemaValidations.SetKeywordData(aKeyword: TJSONSchemaKeyword);
begin
  Include(FKeywordData,aKeyword);
  SetConstrained;
end;

procedure TJSONSchemaValidations.UnSetKeywordData(aKeyword: TJSONSchemaKeyword);
begin
  Exclude(FKeywordData,aKeyword);
end;

function TJSONSchemaValidations.GetContentSchema: TJSONSchema;
begin
  if FcontentSchema=Nil then
    begin
    if Assigned(Schema) then
      FcontentSchema:=Schema.CreateChildSchema(jskContentSchema)
    else
      begin
      FContentSchema:=TJSONSchema.Create(Nil);
      FContentSchema.Name:=jskContentSchema.AsString;
      end;
    SetConstrained;
    end;
  Result:=FcontentSchema;
end;

procedure TJSONSchemaValidations.DoRequiredChange(Sender: TObject);
begin
  if HasKeywordData(jskRequired) then
     SetConstrained;
end;

procedure TJSONSchemaValidations.DoAdd(Sender: TObject);
begin
  SetConstrained;
end;

function TJSONSchemaValidations.GetFormatValidator: TStringFormatValidator;
begin
  Result:=TStringFormatValidator.FromString(Format);
end;


constructor TJSONSchemaValidations.Create(aSchema: TJSONSchema);
begin
  FSchema:=aSchema;
  FRequired:=TStringList.Create;
  TStringList(FRequired).OnChange:=@DoRequiredChange;
  FEnum:=TJSONArray.Create;
  FDependentRequired:=CreateDependentRequired;
end;

destructor TJSONSchemaValidations.Destroy;
begin
  ConstValue:=Nil;
  FreeAndNil(FDependentRequired);
  FreeAndNil(FRequired);
  FreeAndNil(FEnum);
  FreeAndNil(FcontentSchema);
  inherited Destroy;
end;

function TJSONSchemaValidations.KeywordsWithData: TJSONSchemaKeywords;

var
  K : TJSONSchemaKeyword;

begin
  Result:=[];
  For K in Keywords do
    if HasKeyWordData(K) then
      Include(Result,K);
end;

function TJSONSchemaValidations.HasKeywordData(aKeyword: TJSONSchemaKeyword): Boolean;
begin
  case aKeyword of
    jskEnum : Result:=Assigned(FEnum) and (FEnum.Count>0);
    jskRequired : Result:=Assigned(FRequired) and (Required.Count>0);
    jskDependentRequired : Result:=Assigned(FDependentRequired) and (FDependentRequired.Count>0);
    jskContentSchema : Result:=Assigned(FContentSchema);
  else
    Result:=aKeyword in FKeywordData
  end;
end;

function TJSONSchemaValidations.TypesCount: Integer;

var
  T : TSchemaSimpleType;

begin
  Result:=0;
  For T in TSchemaSimpleType do
    if T in Types then
      Inc(Result);
end;

function TJSONSchemaValidations.GetFirstType: TSchemaSimpleType;
var
  T : TSchemaSimpleType;

begin
  Result:=sstNone;
  For T in TSchemaSimpleType do
    if T in Types then
      Exit(T);
end;

{ TJSONSChemaVocabulary }

procedure TJSONSchemaVocabulary.Assign(Source: TPersistent);
var
  aSource: TJSONSChemaVocabulary absolute source;
begin
  if Source is TJSONSChemaVocabulary then
    begin
    URL:=aSource.URL;
    Enabled:=aSource.Enabled;
    end
  else
    inherited Assign(Source);
end;

function TJSONSchemaVocabulary.ToString: String;
begin
  Result:=URL+': '+BoolToStr(Enabled,'True','False');
end;

{ TJSONSChemaVocabularyList }

function TJSONSchemaVocabularyList.GetSchema: TJSONSchema;
begin
  if Owner is TJSONSchema then
    Result:=TJSONSchema(Owner)
  else
    Result:=Nil;
end;

function TJSONSchemaVocabularyList.GetVocabulary(aIndex : integer): TJSONSchemaVocabulary;
begin
  Result:=TJSONSchemaVocabulary(Items[aIndex])
end;

procedure TJSONSchemaVocabularyList.SetVocabulary(aIndex : integer; AValue: TJSONSchemaVocabulary);
begin
  Items[aIndex]:=aValue;
end;

function TJSONSchemaVocabularyList.IndexOfVocabulary(const aURL: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and not SameText(aURL,GetVocabulary(Result).URL) do
    Dec(Result);
end;

function TJSONSchemaVocabularyList.FindVocabulary(aURL: String): TJSONSchemaVocabulary;

var
  Idx : Integer;
begin
  Idx:=IndexOfVocabulary(aURL);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=GetVocabulary(Idx);
end;

function TJSONSchemaVocabularyList.AddVocabulary(const aURL: String): TJSONSchemaVocabulary;
begin
  Result:=add as TJSONSchemaVocabulary;
  Result.URL:=aURL;
  if assigned(Schema) then
    Schema.MatchType:=smConstrained;
end;

function TJSONSchemaVocabularyList.ToString: String;

var
  I : Integer;

begin
  Result:='';
  For I:=0 to Count-1 do
    if Vocabularies[I].Enabled then
      Result:=Result+Vocabularies[I].ToString;
end;

{ TJsonSchema }


function TJsonSchema.GetAdditionalProperties: TJSONSchema;
begin
  if Not Assigned(FAdditionalproperties) then
    FAdditionalproperties:=CreateChildSchema(jskAdditionalProperties);
  Result:=FAdditionalproperties;
end;

function TJsonSchema.GetChildSchema(aIndex : Integer): TJSONSchema;
begin
  Result:=TJSONSchema(FChildren[aIndex]);
end;

function TJsonSchema.GetChildSchemaCount: Integer;
begin
  Result:=FChildren.Count;
end;

function TJsonSchema.GetDependentSchemas: TJSONSchemaList;
begin
  if FDependentSchemas=Nil then
    FDependentSchemas:=TJSONSchemaList.Create(Self,jskDependentSchemas);
  Result:=FDependentSchemas;
end;

function TJsonSchema.GetPatternProperties: TJsonSchemaList;
begin
  if FPatternProperties=Nil then
    FPatternProperties:=TJSONSchemaList.Create(Self,jskPatternProperties);
  Result:=FPatternProperties;
end;

function TJsonSchema.GetUnknownKeywordData: TJSONObject;
begin
  if FUnknownKeywordData=Nil then
    FUnknownKeywordData:=TJSONObject.Create;
  Result:=FUnknownKeywordData;
end;

procedure TJsonSchema.SetMetadata(AValue: TJSONSchemaMetadata);
begin
  if FMetaData=AValue then Exit;
  FMetaData.Assign(AValue);
end;

procedure TJsonSchema.SetString(AIndex: Integer; AValue: String);

var
  KW : TJSONSchemaKeyword;

begin
  kw:=TJSONSchemaKeyword(aIndex);
  SetKeyWordData(kw);
  Case kw of
    jskSchema: FSchema:=aValue;
    jskId: FId:=aValue;
    jskRef: FRef:=aValue;
    jskComment: FComment:=aValue;
    jskAnchor: FAnchor:=aValue;
    jskDynamicAnchor: FDynamicAnchor:=aValue;
    jskDynamicRef: FDynamicRef:=aValue;
  end;
end;

procedure TJsonSchema.SetValidations(AValue: TJsonSchemaValidations);
begin
  if FValidations=AValue then Exit;
  FValidations.Assign(AValue);
end;

procedure TJsonSchema.SetVocabulary(AValue: TJSONSchemaVocabularyList);
begin
  if FVocabulary=AValue then Exit;
  FVocabulary.Assign(AValue);
end;

procedure TJsonSchema.SetKeyWordData(akeyWord: TJSONSchemaKeyword);
begin
  Include(FKeywordData,aKeyword);
  FMatchType:=smConstrained;
end;

procedure TJsonSchema.UnsetKeyWordData(akeyWord: TJSONSchemaKeyword);
begin
  Exclude(FKeywordData,aKeyword);
end;

function TJsonSchema.GetSubSchema(aIndex: Integer): TJSONSchema;

var
  SS : TJSONSubSchema;

begin
  SS:=TJSONSubSchema(aindex);
  if Not Assigned(FSubSchemas[SS]) then
    begin
    FSubSchemas[SS]:=CreateChildSchema(SS.AsSchemaKeyword);
    SetKeyWordData(SS.AsSchemaKeyword);
    end;
  Result:=FSubSchemas[SS];
end;

function TJsonSchema.CreateChildSchema(aName: string): TJsonSchema;
begin
  Result:=CreateChildSchema();
  Result.Name:=aName;
end;

function TJsonSchema.CreateChildSchema(aKeyword: TJSONSchemaKeyword): TJsonSchema;
begin
  Result:=CreateChildSchema(aKeyWord.AsString);
end;

function TJsonSchema.CreateVocabulary: TJSONSchemaVocabularyList;
begin
  Result:=TJSONSchemaVocabularyList.Create(Self,TJSONSchemaVocabulary);
end;

constructor TJsonSchema.Create(AParent: TJsonSchema);
begin
  FParent:=aParent;
  if Assigned(FParent) then
    FParent.FChildren.Add(Self);
  FDefs:=TJsonSchemaList.Create(Self,jskDefs);
  FItems:=TJsonSchemaList.Create(Self,jskItems);
  FAllOf:=TJsonSchemaList.Create(Self,jskAllof);
  FAnyOf:=TJsonSchemaList.Create(Self,jskAnyOf);
  FOneOf:=TJsonSchemaList.Create(Self,jskOneOf);
  FProperties:=TJsonSchemaList.Create(Self,jskProperties);
  FPrefixItems:=TJsonSchemaList.Create(Self,jskPrefixItems);
  FVocabulary:=CreateVocabulary;
  FMetaData:=CreateMetaData;
  FValidations:=CreateValidations;
  FChildren:=TFPList.Create;
end;

constructor TJsonSchema.Create;
begin
  Create(Nil);
end;

destructor TJsonSchema.Destroy;

var
  SS :TJSONSubschema;
  I : Integer;

begin
  if Assigned(FParent) then
    FParent.FChildren.Remove(Self);
  For I:=0 to FChildren.Count-1 do
    TJsonSchema(FChildren[I]).FParent:=Nil;
  For SS in TJSONSubschema do
    FreeAndNil(FSubSchemas[SS]);
  FreeAndNil(FValidations);
  FreeAndNil(FItems);
  FreeAndNil(FAllOf);
  FreeAndNil(FAnyOf);
  FreeAndNil(FOneOf);
  FreeAndNil(FProperties);
  FreeAndNil(FPrefixItems);
  FreeAndNil(FVocabulary);
  FreeAndNil(FMetaData);
  FreeAndNil(FPrefixItems);
  FreeAndNil(FAdditionalProperties);
  FreeAndNil(FPatternProperties);
  FreeAndNil(FDependentSchemas);
  FreeAndNil(FDefs);
  FreeAndNil(FUnknownKeywordData);
  FreeAndNil(FChildren);
  inherited Destroy;
end;

function TJsonSchema.HasKeywordData(aKeyword: TJSONSchemaKeyword): Boolean;
begin
  if aKeyword in Validations.Keywords then
    Result:=Validations.HasKeywordData(aKeyWord)
  else if aKeyword in MetaData.Keywords then
    Result:=MetaData.HasKeywordData(aKeyWord)
  else
    begin
    Case aKeyword of
      jskPrefixItems: Result:=FPrefixItems.Count>0;
      jskProperties : Result:=FProperties.Count>0;
      jskItems : Result:=FItems.Count>0;
      jskAllOf : Result:=FAllOf.Count>0;
      jskAnyOf : Result:=FAnyOf.Count>0;
      jskOneOf : Result:=FOneOf.Count>0;
      jskDependentSchemas: Result:=Assigned(FDependentSchemas) and (FDependentSchemas.Count>0);
      jskPatternProperties: Result:=Assigned(FPatternProperties) and (FPatternProperties.Count>0);
      jskDefs : Result:=FDefs.Count>0;
      jskVocabulary : Result:=Assigned(FVocabulary) and (FVocabulary.Count>0);
    else
      Result:=aKeyword in FKeywordData;
    end;
    end;
end;

function TJsonSchema.KeywordsWithData: TJSONSchemaKeywords;
begin
  Result:=FKeywordData+Validations.FKeywordData+MetaData.Keywords;
end;


function TJSONSchemaValidations.CreateDependentRequired: TSchemaDependentRequiredList;
begin
  Result:=TSchemaDependentRequiredList.Create(Self,TSchemaDependentRequired);
end;

function TJSONSchemaValidations.GetOwner: TPersistent;
begin
  Result:=FSchema;
end;

function TJSONSchemaValidations.Keywords: TJSONSchemaKeywords;
begin
  Result:=ValidatorKeywords;
end;

procedure TJSONSchemaValidations.Assign(Source: TPersistent);
var
  aSource: TJSONSchemaValidations absolute Source;
begin
  if Source is TJSONSchemaValidations then
  begin
    FKeywordData:=[];
    UniqueItems:=aSource.UniqueItems;
    Types:=aSource.Types;
    Required:=aSource.Required;
    Pattern:=aSource.Pattern;
    MultipleOf:=aSource.MultipleOf;
    MinProperties:=aSource.MinProperties;
    MinLength:=aSource.MinLength;
    MinItems:=aSource.MinItems;
    Minimum:=aSource.Minimum;
    MinContains:=aSource.MinContains;
    MaxProperties:=aSource.MaxProperties;
    MaxLength:=aSource.MaxLength;
    MaxItems:=aSource.MaxItems;
    Maximum:=aSource.Maximum;
    MaxContains:=aSource.MaxContains;
    Format:=aSource.Format;
    ExclusiveMinimum:=aSource.ExclusiveMinimum;
    ExclusiveMaximum:=aSource.ExclusiveMaximum;
    contentMediaType:=aSource.contentMediaType;
    contentEncoding:=aSource.contentEncoding;
    constValue:=aSource.constValue.Clone;
    enum:=aSource.Enum.Clone as TJSONArray;
  end else
    inherited Assign(Source);
end;

function TJsonSchema.CreateMetadata: TJSONSchemaMetadata;
begin
  Result:=TJSONSchemaMetadata.Create(Self);
end;

function TJsonSchema.CreateValidations: TJsonSchemaValidations;

begin
  Result:=TJsonSchemaValidations.Create(Self);
end;

procedure TJsonSchema.CheckConstrained;


begin

end;

function TJsonSchema.CreateChildSchema: TJsonSchema;
begin
  Result:=TJsonSchemaClass(ClassType).Create(Self);
end;

function TJsonSchema.RootSchema: TJSONSchema;
begin
  Result:=Self;
  While Assigned(Result.Parent) do
    Result:=Result.Parent;
end;

function TJsonSchema.GetNamedList(const aName : string) : TJSONSchemaList;

  Function TestList(aList : TJSONSchemaList; out aRes : TJSONSchemaList) : Boolean;

  begin
    Result:=Assigned(aList) and (aList.Keyword<>jskUnknown) and (aName=aList.Keyword.AsString);
    if Result then
      aRes:=aList;
  end;

begin
  Result:=Nil;
  If TestList(FPatternProperties,Result) then exit;
  If TestList(FPrefixItems,Result) then exit;
  If TestList(FProperties,Result) then exit;
  If TestList(FItems,Result) then exit;
  If TestList(FAllOf,Result) then exit;
  If TestList(FAnyOf,Result) then exit;
  If TestList(FOneOf,Result) then exit;
  If TestList(FDependentSchemas,Result) then exit;
  If TestList(FDefs,Result) then exit;

end;

function TJsonSchema.Find(const aPath: String): TJSONSchema;

var
  P : Integer;
  aSubPath,aName : string;
  SchemaList : TJSONSchemaList;

begin
  Result:=Nil;
  aSubPath:='';
  P:=Pos('/',aPath);
  if P=0 then
    aName:=aPath
  else
    begin
    aName:=Copy(aPath,1,P-1);
    aSubPath:=Copy(aPath,P+1);
    end;
  if aName='' then
    Result:=Self
  else if aName='#' then
    Result:=RootSchema
  else
    begin
    SchemaList:=GetNamedList(aName);
    if Assigned(SchemaList) then
      Exit(SchemaList.FindIDOrNames(aSubPath))
    else
      Result:=FindChild(aName);
    end;
  if Assigned(Result) and (aSubPath<>'') then
    Result:=Result.Find(aSubPath);
end;

function TJsonSchema.IndexOfChild(const aName: String): Integer;
begin
  Result:=FChildren.Count-1;
  While (Result>=0) and (ChildSchemas[Result].Name<>aName) do
    Dec(Result);
end;

function TJsonSchema.FindChild(const aName : String) : TJSONSchema;

var
  Idx : integer;

begin
  Idx:=IndexOfChild(aName);
  if Idx=-1 then
    Result:=nil
  else
    Result:=ChildSchemas[Idx];
end;

function TJsonSchema.GetPath: String;

begin
  if Parent=Nil then
    Result:='#'
  else
    Result:=Parent.Path+'/'+Name;
end;

{ TJSONSchemaList }

function TJSONSchemaList.GetSchema(aIndex : integer): TJSONSchema;
begin
  Result:=Items[aIndex] as TJSONSchema;
end;

procedure TJSONSchemaList.SetSchema(aIndex : integer; AValue: TJSONSchema);
begin
  Items[aIndex]:=aValue;
end;

constructor TJSONSchemaList.Create(aOwner: TJSONSchema; aKeyWord: TJSONSchemaKeyword);
begin
  Inherited Create(True);
  FSchemaOwner:=aOwner;
  FKeyword:=aKeyword;
end;

function TJSONSchemaList.FindIDOrNames(aPath: String): TJSONSchema;

var
  I,P : Integer;
  aSubPath,aName : string;

begin
  Result:=Nil;
  aSubPath:='';
  P:=Pos('/',aPath);
  if (P=0) then
    aName:=aPath
  else
    begin
    aName:=Copy(aPath,1,P-1);
    aSubPath:=Copy(aPath,P+1);
    end;
  if aName='' then
    exit;
  I:=Count-1;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=Schemas[I];
    if (Result.ID<>aName) and (Result.Name<>aName) then
      Result:=Nil;
    Dec(I);
    end;
  if Assigned(Result) and (aSubPath<>'') then
    Result.Find(aSubPath);
end;

function TJSONSchemaList.Add(const aName: string): TJSONSchema;
begin
  if FSchemaOwner=Nil then
    Result:=TJSONSchema.Create
  else
    begin
    Result:=SchemaOwner.CreateChildSchema(aName);
    if Keyword<>jskUnknown then
      SchemaOwner.SetKeyWordData(Keyword);
    end;
  Inherited Add(Result);
end;

function TJSONSchemaList.Add(Schema: TJSONSchema): Integer;
begin
  Result:=Inherited Add(Schema);
  if Keyword<>jskUnknown then
    SchemaOwner.SetKeyWordData(Keyword);
end;


end.

