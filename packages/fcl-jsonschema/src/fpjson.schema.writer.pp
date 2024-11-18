{
    This file is part of the Free Component Library

    JSON Schema - Write as JSON or to stream
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FpJson.Schema.Writer;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, FpJson.Schema.Types, FpJson.Schema.Schema, FpJson.Writer;
  {$ELSE}
  Classes, SysUtils, fpjson, FpJson.Schema.Types, FpJson.Schema.Schema, jsonwriter;
  {$ENDIF}

Type
  EJSONSchemaWriter = class(EJSONSchema);

  { TJSONSchemaWriter }

  TJSONSchemaWriter = class(TComponent)
  private
    FWriter : TAbstractJSONWriter;
  Protected
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TSchemaDependentRequiredList);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TJSONSchemaVocabularyList);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TSchemaSimpleTypes);
    procedure WriteProperty(const aName: TJSONStringType; aValue: TJSONSchema);
    procedure WriteListAsObject(const aName: TJSONStringType; aValue: TJSONSchemaList);
    procedure WriteProperty(const aName: TJSONStringType; aValue: TJSONSchemaList; AllowAsObject: Boolean = False);
    Procedure DoWriteSchema(aSchema : TJSONSchema);
  Public
    Procedure Writeschema(aSchema : TJSONSchema; aWriter : TAbstractJSONWriter);
  end;

  { TJSONSchemaWriterJSON }

  TJSONSchemaWriterJSON = class(TJSONSchemaWriter)
  Public
    function WriteSchema(aSchema : TJSONSchema) : TJSONData; overload;
  end;

  { TJSONSchemaWriterStream }

  TJSONSchemaWriterStream = class(TJSONSchemaWriter)
  private
    FStrictStrings: Boolean;
  Public
    procedure WriteSchema(aSchema : TJSONSchema; aStream : TStream); overload;
    property StrictStrings : Boolean Read FStrictStrings Write FStrictStrings;
  end;

implementation

{ TJSONSchemaWriter }

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONSchema);
begin
 case aValue.MatchType of
   smNone,smAny : FWriter.WriteProperty(aName,aValue.MatchType=smAny);
 else
   begin
   FWriter.StartProperty(aName);
   DoWriteSchema(aValue);
   FWriter.EndProperty;
   end;
 end;
end;

procedure TJSONSchemaWriter.WriteListAsObject(const aName: TJSONStringType; aValue: TJSONSchemaList);
var
  I : Integer;
begin
 FWriter.StartProperty(aName);
 FWriter.StartObject;
 for I:=0 to aValue.Count-1 do
   begin
   FWriter.NextElement;
   FWriter.StartProperty(aValue.Schemas[i].Name);
   DoWriteSchema(aValue.Schemas[i]);
   end;
 FWriter.EndObject;
 FWriter.EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONSchemaList; AllowAsObject: Boolean);

var
  I : Integer;

begin
  FWriter.StartProperty(aName);
  if AllowAsObject and (aValue.Count=1) then
    DoWriteSchema(aValue.Schemas[0])
  else
    for I:=0 to aValue.Count-1 do
      begin
      FWriter.StartArray;
      FWriter.NextElement;
      DoWriteSchema(aValue.Schemas[i]);
      FWriter.EndArray;
      end;
  FWriter.EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TSchemaDependentRequiredList);
var
  I : Integer;
  D : TSchemaDependentRequired;

begin
  if aValue.Count=0 then
    exit;
  With FWriter do
    begin
    StartProperty(aName);
    StartObject;
    For I:=0 to aValue.Count-1 do
      begin
      D:=aValue[I];
      StartProperty(D.Name);
      WriteValue(D.Required);
      EndProperty;
      end;
    EndObject;
    EndProperty;
    end;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONSchemaVocabularyList);

var
  I : Integer;
  V : TJSONSchemaVocabulary;

begin
  if aValue.Count=0 then
    exit;
  With FWriter do
    begin
    StartProperty(aName);
    StartObject;
    For I:=0 to aValue.Count-1 do
      begin
      V:=aValue[I];
      StartProperty(V.URL);
      WriteValue(V.Enabled);
      EndProperty;
      end;
    EndObject;
    EndProperty;
    end;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TSchemaSimpleTypes);

var
  lCount : Integer;
  St,stVal : TSchemaSimpleType;

begin
  lCount:=0;
  for St in TSchemaSimpleType do
    if st in aValue then
      begin
      inc(lCount);
      stVal:=St;
      end;
  With FWriter do
    begin
    StartProperty(aName);
    if lCount=1 then
      WriteValue(stVal.AsString)
    else
      begin
      StartArray;
      For ST in aValue do
        begin
        NextElement;
        WriteValue(ST.AsString);
        end;
      EndArray;
      end;
    EndProperty;
    end;
end;

procedure TJSONSchemaWriter.DoWriteSchema(aSchema: TJSONSchema);

var
  aKeyword : TJSONSchemaKeyword;
  PropName : TJSONStringType;
  W : TAbstractJSONWriter;

begin
  W:=FWriter;
  if (aSchema.MatchType in [smAny,smNone]) then
    W.WriteValue(aSchema.MatchType=smAny)
  else
    begin
    W.StartObject;
    For aKeyword in TJSONSchemaKeyword do
      if aSchema.HasKeywordData(aKeyWord) then
        begin
        PropName:=aKeyword.AsString;
        Case aKeyword of
        jskUnknown : ;
        jskId : W.WriteProperty(PropName,aSchema.ID);
        jskAnchor : W.WriteProperty(PropName,aSchema.Anchor);
        jskSchema : W.WriteProperty(PropName,aSchema.Schema);
        jskDefs : WriteProperty(PropName,aSchema.Defs);
        jskTitle : W.WriteProperty(PropName,aSchema.Metadata.Title);
        jskDescription : W.WriteProperty(PropName,aSchema.Metadata.Description);
        jskDefault : W.WriteProperty(PropName,aSchema.MetaData.DefaultValue);
        jskMultipleOf : W.WriteProperty(PropName,aSchema.Validations.MultipleOf);
        jskMaximum : W.WriteProperty(PropName,aSchema.Validations.Maximum);
        jskExclusiveMaximum : W.WriteProperty(PropName,aSchema.Validations.ExclusiveMaximum);
        jskMinimum : W.WriteProperty(PropName,aSchema.Validations.Minimum);
        jskExclusiveMinimum : W.WriteProperty(PropName,aSchema.Validations.ExclusiveMinimum);
        jskMaxLength : W.WriteProperty(PropName,aSchema.Validations.MaxLength);
        jskMinLength : W.WriteProperty(PropName,aSchema.Validations.MinLength);
        jskPattern : W.WriteProperty(PropName,aSchema.Validations.Pattern);
        // jskAdditionalItems : WriteProperty(PropName,aSchema.Validations.AdditionalItems);
        jskItems : WriteProperty(PropName,aSchema.Items,true);
        jskPrefixItems : WriteProperty(PropName,aSchema.PrefixItems);
        jskMaxItems : W.WriteProperty(PropName,aSchema.Validations.MaxItems);
        jskMinItems : W.WriteProperty(PropName,aSchema.Validations.MinItems);
        jskUniqueItems : W.WriteProperty(PropName,aSchema.Validations.UniqueItems);
        jskMaxProperties : W.WriteProperty(PropName,aSchema.Validations.MaxProperties);
        jskMinProperties : W.WriteProperty(PropName,aSchema.Validations.MinProperties);
        jskMaxContains : W.WriteProperty(PropName,aSchema.Validations.MaxContains);
        jskMinContains : W.WriteProperty(PropName,aSchema.Validations.MinContains);
        jskRequired : W.WriteProperty(PropName,aSchema.Validations.Required);
        jskAdditionalProperties : WriteProperty(PropName,aSchema.AdditionalProperties);
        jskProperties : WriteListAsObject(PropName,aSchema.Properties);
        jskPatternProperties: WriteProperty(PropName,aSchema.PatternProperties);
        jskPropertyNames : WriteProperty(PropName,aSchema.PropertyNames);
        jskDependentSchemas : WriteProperty(PropName,aSchema.DependentSchemas);

        jskDependentRequired : WriteProperty(PropName,aSchema.Validations.DependentRequired);
        jskEnum: W.WriteProperty(PropName,aSchema.Validations.Enum);
        jskType : WriteProperty(PropName,aSchema.Validations.Types);

        jskAllOf : WriteProperty(PropName,aSchema.AllOf);
        jskAnyOf : WriteProperty(PropName,aSchema.AnyOf);
        jskOneOf : WriteProperty(PropName,aSchema.OneOf);
        jskNot : WriteProperty(PropName,aSchema.NotSchema);
        jskFormat  : W.WriteProperty(PropName,aSchema.Validations.Format);
        jskRef : W.WriteProperty(PropName,aSchema.Ref);
        jskIf : WriteProperty(PropName,aSchema.IfSchema);
        jskElse : WriteProperty(PropName,aSchema.ElseSchema);
        jskThen : WriteProperty(PropName,aSchema.ThenSchema);
        jskDynamicRef : W.WriteProperty(PropName,aSchema.DynamicRef);
        jskDynamicAnchor : W.WriteProperty(PropName,aSchema.DynamicAnchor);
        jskContains : WriteProperty(PropName,aSchema.Contains);
        jskComment : W.WriteProperty(PropName,aSchema.Comment);
        jskConst : W.WriteProperty(PropName,aSchema.Validations.constValue);
        jskUnevaluatedItems : WriteProperty(PropName,aSchema.UnevaluatedItems);
        jskUnevaluatedProperties : WriteProperty(PropName,aSchema.UnevaluatedProperties);
        jskContentEncoding : W.WriteProperty(PropName,aSchema.Validations.contentEncoding);
        jskContentMediaType : W.WriteProperty(PropName,aSchema.Validations.contentMediaType);
        jskContentSchema : WriteProperty(PropName,aSchema.Validations.contentSchema);
        jskExamples : W.WriteProperty(PropName,aSchema.Metadata.Examples);
        jskDeprecated : W.WriteProperty(PropName,aSchema.Metadata.Deprecated);
        jskReadOnly : W.WriteProperty(PropName,aSchema.Metadata.ReadOnly);
        jskWriteOnly : W.WriteProperty(PropName,aSchema.Metadata.WriteOnly);
        jskVocabulary : WriteProperty(PropName,aSchema.Vocabulary);
        end;
        end;
    W.EndObject;
    end;
end;

procedure TJSONSchemaWriter.Writeschema(aSchema: TJSONSchema; aWriter: TAbstractJSONWriter);
begin
  FWriter:=aWriter;
  try
    DoWriteSchema(aSchema);
  finally
    FWriter:=Nil;
  end;
end;

{ TJSONSchemaWriterJSON }

function TJSONSchemaWriterJSON.WriteSchema(aSchema: TJSONSchema): TJSONData;

var
  lWriter : TJSONDataWriter;

begin
  lWriter:=TJSONDataWriter.Create;
  try
    WriteSchema(aSchema,lWriter);
    Result:=lWriter.ExtractData;
  finally
    lWriter.Free;
  end;
end;

{ TJSONSchemaWriterStream }

procedure TJSONSchemaWriterStream.WriteSchema(aSchema: TJSONSchema; aStream: TStream);

var
  lWriter : TJSONStreamWriter;

begin
  lWriter:=TJSONStreamWriter.Create(aStream);
  try
    lWriter.StrictStrings:=StrictStrings;
    WriteSchema(aSchema,lWriter);
  finally
    LWriter.Free;
  end;
end;

end.

