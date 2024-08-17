{
    This file is part of the Free Component Library

    JSON Schema reader - read directly from stream
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FpJson.Schema.Reader;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, FpJson.Schema.Schema, FpJson.Schema.Types, FpJson.Scanner;
  {$ELSE}
  Classes, SysUtils, fpjson, FpJson.Schema.Schema, FpJson.Schema.Types, jsonscanner;
  {$ENDIF}

Type
  EJsonSchemaReader = class(EJSONSchema);

  TJsonTokens = Set of TJsonToken;

  TKeywordInfo = Record
    Schema : TJSONSChema;
    Keyword : TJSONStringType;
    Scanner : TJSONScanner;
  end;

  TKeyWordHandler = Procedure(Sender : TObject; const Info : TKeywordInfo; var Handled: Boolean) of object;
  TSchemaReadOption = (roSkipUnknownProperties);
  TSchemaReadOptions = Set of TSchemaReadOption;


  { TJsonSchemaReader }

  TJsonSchemaReader = class(TComponent)
  private
    FOnUnknownKeyWord: TKeywordHandler;
    FOptions: TSchemaReadOptions;
    FScanner : TJSONScanner;
  Protected
    function GetMatchType(aToken: TJSONToken): TSchemaMatchType;
    // Getting scanner info
    function GetToken : TJSONToken;
    function GetTokenString: TJSONStringType;
    function CurPosAsString: String;
    function TokensToString(aTokens: TJSONTokens): String;
    // Checking things
    function  CheckNextToken(aExpectedTokens: TJSONTokens): TJSONToken;
    procedure CheckNextToken(aExpectedToken: TJSONToken);
    procedure CheckToken(aExpectedToken, aActualToken: TJSONToken);
    procedure CheckToken(aExpectedTokens: TJSONTokens; aActualToken: TJSONToken);
    procedure InvalidToken(aToken: TJSONToken);
    // Parsing utilities
    function StringToJSONNumber(S: RawByteString): TJSONNumber;
    function StringToJSONString(S: RawByteString): TJSONString;
    function ReadJSONData(aToken: TJSONToken): TJSONData;
    procedure ReadJSONArray(aArray: TJSONArray);
    procedure ReadJSONObject(aObj: TJSONObject);
    function ReadBoolean: Boolean;
    function ReadNumber: Double;
    function ReadPositiveInteger: Cardinal;
    function ReadString: String;
    // Read properties
    function HandleUnknownKeyword(aInfo : TKeyWordInfo) : Boolean;
    procedure ReadDependentRequired(aSchema: TJsonSchema; aList: TSchemaDependentRequiredList);
//    procedure ReadSchemaValue(aSchema: TJsonSchema; AValue: TSchemaValue; const AValidTypes: TJsonTokens = []);
    procedure ReadArray(aValues: TJSONArray; Full : Boolean);
    procedure ReadNamedSchemas(ASchema: TJsonSchema; aList: TJSONSchemaList);
    procedure ReadItems(ASchema: TJsonSchema);
    procedure ReadProperties(ASchema: TJsonSchema);
    procedure ReadSchemaArray(ASchema: TJsonSchema; ASchemaList: TJsonSchemaList);
    procedure ReadStringArray(AStrings: TStrings);
    procedure ReadTypes(AJsonSchema: TJsonSchema);
    procedure ReadVocabulary(aSchema: TJsonSchema);
    // Entry points
    procedure ReadSchemaObject(ASchema: TJsonSchema);
    procedure ReadSchema(ASchema: TJsonSchema);
    property Scanner : TJSONScanner Read FScanner;
  public
    procedure ReadFromFile(aSchema: TJSONSchema; const AFilename: String);
    procedure ReadFromStream(aSchema: TJSONSchema; AStream: TStream);
    procedure ReadFromString(aSchema: TJSONSchema; const AString: TJSONStringType);
    function ReadJSONData: TJSONData;
    Property Options : TSchemaReadOptions Read FOptions Write FOptions;
    Property OnUnknownKeyWord : TKeywordHandler Read FOnUnknownKeyWord Write FOnUnknownKeyWord;
  end;

implementation

uses fpjson.schema.consts;


{ ---------------------------------------------------------------------
  Auxiliary routines
  ---------------------------------------------------------------------}

function TJsonSchemaReader.CurPosAsString: String;

begin
  Result:=Format('%d:%d',[FScanner.CurRow,FScanner.CurColumn]);
end;

procedure TJsonSchemaReader.InvalidToken(aToken: TJSONToken);

begin
  raise EJsonSchemaReader.CreateFmt(SErrInvalidToken, [CurPosAsString, TokenInfos[aToken]]);
end;

procedure TJsonSchemaReader.CheckToken(aExpectedToken,aActualToken: TJSONToken);

begin
  if aExpectedToken<>aActualToken then
    raise EJsonSchemaReader.CreateFmt(SErrUnexpectedToken, [CurPosAsString, TokenInfos[aExpectedToken], TokenInfos[aActualToken]]);
end;

function TJsonSchemaReader.TokensToString(aTokens : TJSONTokens) : String;

var
  T : TJSONToken;
  S : String;

begin
  S:='';
  For T in aTokens do
    begin
    if (S<>'') then
      S:=S+', ';
    S:=S+'"'+TokenInfos[T]+'"';
    end;
  Result:=S;
end;

procedure TJsonSchemaReader.CheckToken(aExpectedTokens : TJSONTokens; aActualToken: TJSONToken);

begin
  if not (aActualToken in aExpectedTokens) then
    raise EJsonSchemaReader.CreateFmt(SErrUnexpectedTokenNotInSet, [TokensToString(aExpectedTokens), TokenInfos[aActualToken]]);
end;


function TJsonSchemaReader.CheckNextToken(aExpectedTokens : TJSONTokens) : TJSONToken;

begin
  Result:=GetToken;
  CheckToken(aExpectedTokens,Result);
end;

procedure TJsonSchemaReader.CheckNextToken(aExpectedToken : TJSONToken);
begin
  CheckToken(aExpectedToken,GetToken);
end;

function TJsonSchemaReader.StringToJSONNumber(S : RawByteString): TJSONNumber;

var
  I : integer;
  I64 : Int64;
  Q : QWord;
  F : TJSONFloat;

begin
  if TryStrToInt(S,I) then
    Exit(TJSONIntegerNumber.Create(I));
  if TryStrToInt64(S,I64) then
    Exit(TJSONInt64Number.Create(I64));
  if TryStrToQWord(S,Q) then
    Exit(TJSONQWordNumber.Create(Q));
  Val(S,F,I);
  If (I<>0) then
    EConvertError.CreateFmt(SErrInvalidNumber,[S]);
  Result:=TJSONFloatNumber.Create(F);
end;

function TJsonSchemaReader.StringToJSONString(S : RawByteString): TJSONString;

begin
  Result:=TJSONString.Create(UTF8Decode(S))
end;

function TJsonSchemaReader.GetTokenString: TJSONStringType;

begin
  Result:=FScanner.CurTokenString;
end;

function TJsonSchemaReader.GetToken: TJSONToken;

const
  IgnoredTokens = [tkWhitespace,tkComment];

begin
  repeat
    Result:=FScanner.FetchToken;
  until Not (Result in IgnoredTokens);
end;

function TJsonSchemaReader.ReadBoolean: Boolean;

var
  aToken : TJSONToken;

begin
  aToken:=GetToken;
  CheckToken([tkTrue,tkFalse],aToken);
  Result:=aToken=tkTrue;
end;

function TJsonSchemaReader.ReadNumber: Double;

var
  N : TJSONNumber;
  aToken : TJSONToken;
begin
  aToken:=GetToken;
  CheckToken(tkNumber,aToken);
  N:=StringToJSONNumber(GetTokenString);
  try
    Result:=N.AsFloat;
  finally
    N.Free;
  end;
end;

function TJsonSchemaReader.ReadPositiveInteger: Cardinal;

var
  N : TJSONNumber;
  aToken : TJSONToken;
  aValue : Int64;
begin
  aToken:=GetToken;
  CheckToken(tkNumber,aToken);
  N:=StringToJSONNumber(GetTokenString);
  try
    if (N.NumberType<>ntInteger) and (N.AsFloat<>Trunc(N.asFloat)) then
      raise EJsonSchemaReader.CreateFmt(SErrNumberIsNotAnInteger, [CurPosAsString, GetTokenString]);

    aValue:=N.Asint64;

    if aValue<0 then
      raise EJsonSchemaReader.CreateFmt(SErrIntegerIsNegative, [CurPosAsString, aValue]);
    Result:=aValue;
  finally
    N.Free;
  end;
end;


{ ---------------------------------------------------------------------
  Actual reading of Schema
  ---------------------------------------------------------------------}

procedure TJsonSchemaReader.ReadJSONObject(aObj : TJSONObject);
// On entry, we're on {

var
  aToken : TJSONToken;
  aName : TJSONStringtype;

begin
  aToken:=CheckNextToken([tkString,tkIdentifier,tkCurlyBraceClose]);
  While not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    aName:=GetTokenString;
    CheckNextToken(tkColon);
    aObj.Add(aNAme,ReadJSONData);
    aToken:=CheckNextToken([tkComma,tkCurlyBraceClose]);
    if aToken=tkComma then
      aToken:=CheckNextToken([tkString,tkIdentifier]);
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TJsonSchemaReader.ReadJSONArray(aArray : TJSONArray);
// On entry, we're on [

var
  aToken : TJSONToken;

begin
  Repeat
    aToken:=GetToken;
    if Not (aToken in [tkComma,tkSquaredBraceClose]) then
      aArray.Add(ReadJSONData(aToken));
  until (aToken in [tkSquaredBraceClose,tkEOF]);
  CheckToken(tkSquaredBraceClose,aToken);
end;


function TJsonSchemaReader.ReadJSONData : TJSONData;

// Read token and construct JSON Value from it

begin
  Result:=ReadJSONData(GetToken);
end;

function TJsonSchemaReader.ReadJSONData(aToken : TJSONToken) : TJSONData;
// construct JSON value from atoken

begin
  Result:=nil;
  try
    case aToken of
      tkNull : Result:=TJSONNull.Create;
      tkNumber : Result:=StringToJSONNumber(GetTokenString);
      tkTrue,
      tkFalse : Result:=TJSONBoolean.Create(aToken=tkTrue);
      tkString : Result:=TJSONString.Create(GetTokenString);
      tkCurlyBraceOpen :
        begin
        Result:=TJSONObject.Create;
        ReadJSONObject(TJSONObject(Result));
        end;
      tkSquaredBraceOpen :
        begin
        Result:=TJSONArray.Create;
        ReadJSONArray(TJSONArray(Result));
        end
    else
      InvalidToken(aToken);
    end;
  except
    Result.Free;
    Raise;
  end;
end;

(*
procedure TJsonSchemaReader.ReadSchemaValue(aSchema: TJsonSchema; AValue: TSchemaValue; const AValidTypes: TJsonTokens);

var
  aToken : TJSONToken;

begin
  aToken:=GetToken;
  if aToken=tkEOF then
    Exit;
  if (aValidTypes<>[]) then
    CheckToken(aValidTypes,aToken);
  case aToken of
  tkCurlyBraceOpen:
    begin
    aValue.Schema := aSchema.CreateChildSchema;
    aValue.Schema.MatchType:=GetMatchType(aToken);
    ReadSchemaObject(aValue.Schema);
    end;
  tkSquaredBraceOpen:
    begin
    aValue.List:=TSchemaValueList.Create(jskUnknown);
    ReadArray(aValue.List,False);
    end;
  tkNumber:
    aValue.SimpleValue:=StringToJSONNumber(GetTokenString);
  tkString:
    aValue.SimpleValue :=StringToJSONString(GetTokenString);
  tkTrue,tkFalse:
    aValue.SimpleValue:=TJSONBoolean.Create(aToken=tkTrue);
  tkNull:
    aValue.SimpleValue:=TJSONNull.Create();
  end;
end;
*)
procedure TJsonSchemaReader.ReadArray(aValues: TJSONArray; Full: Boolean);

begin
  if Full then
    CheckNextToken(tkSquaredBraceOpen);
  ReadJSONArray(aValues);
end;


procedure TJsonSchemaReader.ReadNamedSchemas(ASchema: TJsonSchema; aList : TJSONSchemaList);
var
  Item : TJSONSchema;
  aName : TJSONStringType;
  aToken : TJSONToken;

begin
  CheckNextToken(tkCurlyBraceOpen);
  aToken:=GetToken;
  While Not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    case aToken of
    tkIdentifier,
    tkString:
      begin
      aName:=GetTokenString;
      item:=aSchema.CreateChildSchema(aName);
      aList.Add(Item);
      CheckNextToken(tkColon);
      ReadSchema(Item);
      end;
    tkComma: ;
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TJsonSchemaReader.ReadFromFile(aSchema : TJSONSchema; const AFilename: String);
var
  fileStream: TFileStream;
begin
  fileStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    ReadFromStream(aSchema,fileStream);
    aSchema.Name := ChangeFileExt(ExtractFileName(AFilename), '');
  finally
    fileStream.Free;
  end;
end;

procedure TJsonSchemaReader.ReadFromStream(aSchema : TJSONSchema; AStream: TStream);

begin
  FScanner:= TJSONScanner.Create(AStream,[joUTF8]);
  try
    ReadSchema(aSchema)
  finally
    FScanner.Free;
  end;
end;

procedure TJsonSchemaReader.ReadFromString(aSchema: TJSONSchema; const AString: TJSONStringType);
var
  S: TStringStream;
begin
  S:=TStringStream.Create(AString);
  try
    ReadFromStream(aSchema,S);
  finally
    S.Free;
  end;
end;

function TJsonSchemaReader.GetMatchType(aToken : TJSONToken) : TSchemaMatchType;

begin
  case aToken of
    tkTrue : Result:=smAny;
    tkFalse : Result:=smNone;
    tkCurlyBraceOpen : Result:=smConstrained;
  else
    InvalidToken(aToken);
  end;
end;

procedure TJsonSchemaReader.ReadItems(ASchema: TJsonSchema);
var
  itemSchema: TJsonSchema;
  aToken : TJSONToken;

begin
  aToken:=GetToken;
  case aToken of
   tkTrue,
   tkFalse,
   tkCurlyBraceOpen:
    begin
      itemSchema := ASchema.CreateChildSchema;
      itemSchema.Name := 'items';
      ASchema.Items.Add(itemSchema);
      ItemSchema.MatchType:=GetMatchType(aToken);
      if aToken=tkCurlyBraceOpen then
        ReadSchemaObject(itemSchema);
    end;
    tkSquaredBraceOpen:
    begin
      ReadSchemaArray(ASchema, ASchema.Items);
    end;
  else
    InvalidToken(aToken);
  end;
end;


procedure TJsonSchemaReader.ReadProperties(ASchema: TJsonSchema);
var
  propName: String;
  schemaItem: TJsonSchema;
  aToken: TJSONToken;
begin
    aToken:=GetToken;
  while Not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    case aToken of
      tkIdentifier,
      tkString:
      begin
        CheckNextToken(tkColon);
        propName := GetTokenString;
        schemaItem := ASchema.CreateChildSchema;
        schemaItem.Name := propName;
        ASchema.Properties.Add(schemaItem);
        ReadSchema(schemaItem);
      end;
      tkComma : ;
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;



procedure TJsonSchemaReader.ReadSchema(ASchema: TJsonSchema);

var
  aToken : TJSONToken;

begin
  aToken:=GetToken;
  Case aToken of
    tkTrue,
    tkFalse,
    tkCurlyBraceOpen:
      begin
      aSchema.MatchType:=GetMatchType(aToken);
      if aToken=tkCurlyBraceOpen then
        ReadSchemaObject(aSchema);
      end
  else
    InvalidToken(aToken);
  end;
end;

procedure TJsonSchemaReader.ReadDependentRequired(aSchema: TJsonSchema; aList: TSchemaDependentRequiredList);

var
  aName : string;
  Item : TSchemaDependentRequired;
  aToken : TJSONToken;

begin
  CheckNextToken(tkCurlyBraceOpen);
  aToken:=GetToken;
  While Not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    case aToken of
    tkIdentifier,
    tkString:
      begin
      aName:=GetTokenString;
      CheckNextToken(tkColon);
      item:=aList.AddDependent(aName);
      ReadStringArray(Item.Required);
      end;
    tkComma: ;
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TJsonSchemaReader.ReadVocabulary(aSchema: TJsonSchema);

var
  aName : string;
  aToken : TJSONToken;

begin
  CheckNextToken(tkCurlyBraceOpen);
  aToken:=GetToken;
  While Not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    case aToken of
    tkIdentifier,
    tkString:
      begin
      aName:=GetTokenString;
      CheckNextToken(tkColon);
      aSchema.Vocabulary.AddVocabulary(aName).Enabled:=ReadBoolean;
      end;
    tkComma: ;
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;


procedure TJsonSchemaReader.ReadSchemaObject(ASchema: TJsonSchema);

var
  propName: String;
  aToken: TJSONToken;
  keyword : TJSONSchemaKeyword;
  Info : TKeywordInfo;
  aValue : TJSONData;

begin
  aToken:=GetToken;
  while not (aToken in [tkEOF,tkCurlyBraceClose]) do
    begin
    case aToken of
    tkString,
    tkIdentifier:
      begin
        propName := GetTokenString;
        CheckNextToken(tkColon);
        keyWord:=TJSONSchemaKeyword.FromString(PropName);
        case KeyWord of
          jskDefs: ReadNamedSchemas(ASchema,aSchema.Defs);
          jskProperties: ReadProperties(ASchema);
          jskDependentSchemas: ReadNamedSchemas(ASchema,aSchema.DependentSchemas);
          jskType: ReadTypes(ASchema);
          jskDescription: ASchema.MetaData.Description := ReadString;
          jskTitle: ASchema.MetaData.Title := ReadString;
          jskDefault: aSchema.Metadata.DefaultValue:=ReadJSONData;
          jskSchema: ASchema.Schema := ReadString;
          jskConst : ASchema.Validations.constValue:=ReadJSONData;
          jskIdDraft4,
          jskId: ASchema.Id := ReadString;
          jskAnchor: ASchema.Anchor:= ReadString;
          jskDynamicAnchor: ASchema.DynamicAnchor:= ReadString;
          jskComment: ASchema.Comment:=ReadString;
          jskMinimum: ASchema.Validations.Minimum := ReadNumber;
          jskMaximum: ASchema.Validations.Maximum := ReadNumber;
          jskMaxItems: ASchema.Validations.MaxItems := ReadPositiveInteger;
          jskMinItems: ASchema.Validations.MinItems := ReadPositiveInteger;
          jskMaxLength: ASchema.Validations.MaxLength := ReadPositiveInteger;
          jskMinLength: ASchema.Validations.MinLength := ReadPositiveInteger;
          jskMaxProperties: ASchema.Validations.MaxProperties := ReadPositiveInteger;
          jskMinProperties: ASchema.Validations.MinProperties := ReadPositiveInteger;
          jskMaxContains : aSchema.Validations.MaxContains:=ReadPositiveInteger;
          jskMinContains : aSchema.Validations.MaxContains:=ReadPositiveInteger;
          jskUniqueItems: ASchema.Validations.UniqueItems := ReadBoolean;
          jskExclusiveMaximum: ASchema.Validations.ExclusiveMaximum := ReadNumber;
          jskExclusiveMinimum: ASchema.Validations.ExclusiveMinimum := ReadNumber;
          jskMultipleOf : ASchema.Validations.MultipleOf := ReadNumber;
          jskEnum: ReadArray(ASchema.Validations.Enum,True);
          jskItems: ReadItems(ASchema);
          jskPrefixItems: ReadSchemaArray(aSchema,aSchema.PrefixItems);
          // For backwards compatibility
          jskAdditionalItems: ReadItems(aSchema);
//          jskAdditionalItems: ReadSchemaValue(ASchema, ASchema.AdditionalItems, [tkBoolean, tkCurlyBraceOpen]);
          jskAdditionalProperties: ReadSchema(ASchema.AdditionalProperties);
          jskPattern: ASchema.Validations.Pattern := ReadString;
          jskPatternProperties: ReadNamedSchemas(aSchema,ASchema.PatternProperties);
          jskRequired: ReadStringArray(ASchema.Validations.Required);
          jskFormat: ASchema.Validations.Format := ReadString;
          jskRef: ASchema.Ref := ReadString;
          jskDynamicRef: ASchema.DynamicRef := ReadString;
          jskAllOf: ReadSchemaArray(ASchema, ASchema.AllOf);
          jskAnyOf: ReadSchemaArray(ASchema, ASchema.AnyOf);
          jskOneOf: ReadSchemaArray(ASchema, ASchema.OneOf);
          jskIf: ReadSchema(ASchema.IfSchema);
          jskThen: ReadSchema(ASchema.ThenSchema);
          jskElse: ReadSchema(ASchema.ElseSchema);
          jskNot: ReadSchema(ASchema.NotSchema);
          jskContains : ReadSchema(ASchema.Contains);
          jskPropertyNames : ReadSchema(aSchema.PropertyNames);
          jskDependentRequired: ReadDependentRequired(aSchema,aSchema.Validations.DependentRequired);
          jskUnevaluatedItems : ReadSchema(aSchema.UnevaluatedItems);
          jskUnevaluatedProperties : ReadSchema(aSchema.UnevaluatedProperties);
          jskDefinitions : ReadNamedSchemas(ASchema,aSchema.Defs);
          jskContentEncoding : ASchema.Validations.ContentEncoding := ReadString;
          jskContentMediaType  : ASchema.Validations.ContentMediaType := ReadString;
          jskContentSchema : ReadSchema(ASchema.Validations.ContentSchema);
          jskExamples : ReadArray(aSchema.MetaData.Examples,True);
          jskDeprecated : aSchema.Metadata.Deprecated:=ReadBoolean();
          jskReadOnly : aSchema.Metadata.ReadOnly:=ReadBoolean();
          jskWriteOnly : aSchema.Metadata.WriteOnly:=ReadBoolean();
          jskVocabulary : ReadVocabulary(aSchema);
          jskUnknown:
            begin
            Info.Keyword:=PropName;
            Info.Schema:=aSchema;
            Info.Scanner:=Scanner;
            If not HandleUnknownKeyword(info) then
              begin
              aValue:=ReadJSONData;
              if (roSkipUnknownProperties in Options) then
                aValue.Free
              else
                aSchema.UnknownKeywordData.Add(PropName,aValue);
              end;
            end;
        end;
      end;
    tkComma:
      ;
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkCurlyBraceClose,aToken);
end;

procedure TJsonSchemaReader.ReadSchemaArray(ASchema: TJsonSchema; ASchemaList: TJsonSchemaList);

var
  schemaItem: TJsonSchema;
  aToken: TJSONToken;

begin
  CheckNextToken(tkSquaredBraceOpen);
  aToken:=GetToken;
  while not (aToken in [tkEOF,tkSquaredBraceClose]) do
    begin
    case aToken of
      tkComma:
        ;
      tkTrue,
      tkFalse,
      tkCurlyBraceOpen:
        begin
        schemaItem := ASchema.CreateChildSchema;
        ASchemaList.Add(schemaItem);
        schemaItem.MatchType:=GetMatchType(aToken);
        if aToken=tkCurlyBraceOpen then
          ReadSchemaObject(schemaItem);
        end;
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
  CheckToken(tkSquaredBraceClose,aToken);
end;

function TJsonSchemaReader.ReadString(): String;
var
  aToken: TJSONToken;
begin
  aToken:=GetToken;
  CheckToken(tkString,aToken);
  Result:=GetTokenString;
end;

function TJsonSchemaReader.HandleUnknownKeyword(aInfo: TKeyWordInfo): Boolean;
begin
  Result:=False;
  if Assigned(OnUnknownKeyword) then
    OnUnknownKeyword(Self,aInfo,Result);
end;

procedure TJsonSchemaReader.ReadStringArray(AStrings: TStrings);
var
  aToken: TJSONToken;
begin
  CheckNextToken(tkSquaredBraceOpen);
  aToken:=GetToken;
  while Not (aToken in [tkEOF,tkSquaredBraceClose]) do
    begin
    case aToken of
      tkComma:
        ;
      tkString:
        AStrings.Add(GetTokenString);
    else
      InvalidToken(aToken);
    end;
    aToken:=GetToken;
    end;
end;

procedure TJsonSchemaReader.ReadTypes(AJsonSchema: TJsonSchema);
var
  aTypes : TSchemaSimpleTypes;
  aToken: TJSONToken;
begin
  aTypes:=[];
  aToken:=CheckNextToken([tkString,tkSquaredBraceOpen]);
  if aToken=tkString then
    Include(aTypes,TSchemaSimpleType.FromString(GetTokenString))
  else
    begin
    aToken:=GetToken;
    while Not (aToken in [tkEOF,tkSquaredBraceClose]) do
      begin
      Case aToken of
      tkComma:
        ;
      tkString:
        Include(aTypes,TSchemaSimpleType.FromString(GetTokenString));
      else
        InvalidToken(aToken);
      end;
      aToken:=GetToken;
      end;
    CheckToken(tkSquaredBraceClose,aToken);
    end;
  AJSONSchema.Validations.Types:=aTypes;
end;


end.

