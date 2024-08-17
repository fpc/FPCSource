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
  System.Classes, System.SysUtils, FpJson.Data, FpJson.Schema.Types, FpJson.Schema.Schema;
  {$ELSE}
  Classes, SysUtils, fpjson, FpJson.Schema.Types, FpJson.Schema.Schema;
  {$ENDIF}

Type
  EJSONSchemaWriter = class(EJSONSchema);

  { TJSONSchemaWriter }

  TJSONSchemaWriter = class(TComponent)
  private
  Protected
    Procedure WriteProperty(const aName : TJSONStringType);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Boolean);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Integer);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Int64);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Double);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : String);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TJSONSchema);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TJSONSchemaList);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TStrings);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TJSONData);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TSchemaDependentRequiredList);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TJSONSchemaVocabularyList);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TSchemaSimpleTypes);
    procedure WriteValue(aValue: TJSONData);
    procedure WriteValue(aValue: TStrings);
    // Override in descendants
    Procedure WriteValue(); virtual; abstract;
    Procedure WriteValue(aValue : Boolean); virtual; abstract;
    Procedure WriteValue(aValue : Integer); virtual; abstract;
    Procedure WriteValue(aValue : Int64); virtual; abstract;
    Procedure WriteValue(aValue : Double); virtual; abstract;
    Procedure WriteValue(const aValue : String); virtual; abstract;
    Procedure StartProperty(const aName: string); virtual; abstract;
    Procedure EndProperty; virtual; abstract;
    Procedure StartArray; virtual; abstract;
    Procedure EndArray; virtual; abstract;
    Procedure NextElement; virtual; abstract;
    Procedure StartObject; virtual; abstract;
    Procedure EndObject; virtual; abstract;
    Procedure DoWriteSchema(aSchema : TJSONSchema);
  end;

  { TJSONSchemaWriterJSON }

  TJSONSchemaWriterJSON = class(TJSONSchemaWriter)
  Private
    FStack : Array of TJSONData;
    FCount : Integer;
    FPropertyName : String;
  protected
    function CurrentStruct : TJSONData;
    Procedure PushData(Obj : TJSONData);
    Procedure PopData;
    procedure EndArray; override;
    procedure EndObject; override;
    procedure EndProperty; override;
    procedure NextElement; override;
    procedure StartArray; override;
    procedure StartObject; override;
    procedure StartProperty(const aName: string); override;
    procedure WriteValue(aValue: Boolean); override;
    procedure WriteValue(aValue: Double); override;
    procedure WriteValue(aValue: Int64); override;
    procedure WriteValue(aValue: Integer); override;
    procedure WriteValue(const aValue: String); override;
    procedure WriteValue; override;
  Public
    function WriteSchema(aSchema : TJSONSchema) : TJSONData;
  end;

  { TJSONSchemaWriterStream }

  TJSONSchemaWriterStream = class(TJSONSchemaWriter)
  private
    FStream: TStream;
    FCounts : Array of Integer;
    FLen : Integer;
    FStrictStrings: Boolean;
  Protected
    Procedure PushElCount;
    Procedure PopElCount;
    Procedure IncElCount;
    Function ElCount : Integer;
    procedure WriteString(const aString : TJSONStringType);
    Procedure WriteValue(); override;
    Procedure WriteValue(aValue : Boolean); override;
    Procedure WriteValue(aValue : Integer); override;
    Procedure WriteValue(aValue : Int64); override;
    Procedure WriteValue(aValue : Double); override;
    Procedure WriteValue(const aValue : String); override;
    Procedure StartProperty(const aName: string); override;
    Procedure EndProperty; override;
    Procedure StartArray; override;
    Procedure EndArray; override;
    Procedure NextElement; override;
    Procedure StartObject; override;
    Procedure EndObject; override;
    Property Stream : TStream Read FStream;
  Public
    procedure WriteSchema(aSchema : TJSONSchema; aStream : TStream);
    property StrictStrings : Boolean Read FStrictStrings Write FStrictStrings;
  end;

implementation

uses FpJson.Schema.Consts;

{ TJSONSchemaWriter }

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType);
begin
  WriteValue(aName);
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: Boolean);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: Integer);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: Int64);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: Double);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: String);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONSchema);
begin
 case aValue.MatchType of
   smNone,smAny : WriteProperty(aName,aValue.MatchType=smAny);
 else
   begin
   StartProperty(aName);
   DoWriteSchema(aValue);
   EndProperty;
   end;
 end;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONSchemaList);

var
  I : Integer;

begin
  StartProperty(aName);
  StartArray;
  for I:=0 to aValue.Count-1 do
    begin
    NextElement;
    DoWriteSchema(aValue.Schemas[i]);
    end;
  EndArray;
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteValue(aValue: TStrings);

var
  S : String;

begin
  StartArray;
  For S in aValue do
    begin
    NextElement;
    WriteValue(S);
    end;
  EndArray;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TStrings);

begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONData);

begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty();
end;

procedure TJSONSchemaWriter.WriteValue(aValue: TJSONData);

var
  Enum : TJSONEnum;

begin
  Case aValue.JSONType of
    jtNull : WriteValue();
    jtBoolean : WriteValue(aValue.AsBoolean);
    jtString : WriteValue(aValue.AsString);
    jtNumber :
      case TJSONNumber(aValue).NumberType of
       ntInteger : WriteValue(aValue.AsInteger);
       ntInt64 : WriteValue(aValue.AsInt64);
       ntFloat : WriteValue(aValue.AsFloat);
       ntQword : WriteValue(aValue.AsInt64);
      end;
    jtObject :
      begin
      StartObject;
      For Enum in aValue do
        WriteProperty(Enum.Key,enum.Value);
      EndObject;
      end;
    jtArray :
      begin
      StartArray;
      For Enum in aValue do
        begin
        NextElement;
        WriteValue(Enum.Value);
        end;
      EndArray;
      end;
  end;

end;

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TSchemaDependentRequiredList);
var
  I : Integer;
  D : TSchemaDependentRequired;

begin
  if aValue.Count=0 then
    exit;
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

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONSchemaVocabularyList);

var
  I : Integer;
  V : TJSONSchemaVocabulary;

begin
  if aValue.Count=0 then
    exit;
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

procedure TJSONSchemaWriter.WriteProperty(const aName: TJSONStringType; aValue: TSchemaSimpleTypes);

var
  St : TSchemaSimpleType;

begin
  StartProperty(aName);
  StartArray;
  For ST in aValue do
    begin
    NextElement;
    WriteValue(ST.AsString);
    end;
  EndArray;
  EndProperty;
end;

procedure TJSONSchemaWriter.DoWriteSchema(aSchema: TJSONSchema);

var
  aKeyword : TJSONSchemaKeyword;
  PropName : TJSONStringType;

begin
  if (aSchema.MatchType in [smAny,smNone]) then
    WriteValue(aSchema.MatchType=smAny)
  else
    begin
    StartObject;
    For aKeyword in TJSONSchemaKeyword do
      if aSchema.HasKeywordData(aKeyWord) then
        begin
        PropName:=aKeyword.AsString;
        Case aKeyword of
        jskUnknown : ;
        jskId : WriteProperty(PropName,aSchema.ID);
        jskAnchor : WriteProperty(PropName,aSchema.Anchor);
        jskSchema : WriteProperty(PropName,aSchema.Schema);
        jskDefs : WriteProperty(PropName,aSchema.Defs);
        jskTitle : WriteProperty(PropName,aSchema.Metadata.Title);
        jskDescription : WriteProperty(PropName,aSchema.Metadata.Description);
        jskDefault : WriteProperty(PropName,aSchema.MetaData.DefaultValue);
        jskMultipleOf : WriteProperty(PropName,aSchema.Validations.MultipleOf);
        jskMaximum : WriteProperty(PropName,aSchema.Validations.Maximum);
        jskExclusiveMaximum : WriteProperty(PropName,aSchema.Validations.ExclusiveMaximum);
        jskMinimum : WriteProperty(PropName,aSchema.Validations.Minimum);
        jskExclusiveMinimum : WriteProperty(PropName,aSchema.Validations.ExclusiveMinimum);
        jskMaxLength : WriteProperty(PropName,aSchema.Validations.MaxLength);
        jskMinLength : WriteProperty(PropName,aSchema.Validations.MinLength);
        jskPattern : WriteProperty(PropName,aSchema.Validations.Pattern);
        // jskAdditionalItems : WriteProperty(PropName,aSchema.Validations.AdditionalItems);
        jskItems : WriteProperty(PropName,aSchema.Items);
        jskPrefixItems : WriteProperty(PropName,aSchema.PrefixItems);
        jskMaxItems : WriteProperty(PropName,aSchema.Validations.MaxItems);
        jskMinItems : WriteProperty(PropName,aSchema.Validations.MinItems);
        jskUniqueItems : WriteProperty(PropName,aSchema.Validations.UniqueItems);
        jskMaxProperties : WriteProperty(PropName,aSchema.Validations.MaxProperties);
        jskMinProperties : WriteProperty(PropName,aSchema.Validations.MinProperties);
        jskMaxContains : WriteProperty(PropName,aSchema.Validations.MaxContains);
        jskMinContains : WriteProperty(PropName,aSchema.Validations.MinContains);
        jskRequired : WriteProperty(PropName,aSchema.Validations.Required);
        jskAdditionalProperties : WriteProperty(PropName,aSchema.AdditionalProperties);
        jskProperties : WriteProperty(PropName,aSchema.Properties);
        jskPatternProperties: WriteProperty(PropName,aSchema.PatternProperties);
        jskPropertyNames : WriteProperty(PropName,aSchema.PropertyNames);
        jskDependentSchemas : WriteProperty(PropName,aSchema.DependentSchemas);

        jskDependentRequired : WriteProperty(PropName,aSchema.Validations.DependentRequired);
        jskEnum: WriteProperty(PropName,aSchema.Validations.Enum);
        jskType : WriteProperty(PropName,aSchema.Validations.Types);

        jskAllOf : WriteProperty(PropName,aSchema.AllOf);
        jskAnyOf : WriteProperty(PropName,aSchema.AnyOf);
        jskOneOf : WriteProperty(PropName,aSchema.OneOf);
        jskNot : WriteProperty(PropName,aSchema.NotSchema);
        jskFormat  : WriteProperty(PropName,aSchema.Validations.Format);
        jskRef : WriteProperty(PropName,aSchema.Ref);
        jskIf : WriteProperty(PropName,aSchema.IfSchema);
        jskElse : WriteProperty(PropName,aSchema.ElseSchema);
        jskThen : WriteProperty(PropName,aSchema.ThenSchema);
        jskDynamicRef : WriteProperty(PropName,aSchema.DynamicRef);
        jskDynamicAnchor : WriteProperty(PropName,aSchema.DynamicAnchor);
        jskContains : WriteProperty(PropName,aSchema.Contains);
        jskComment : WriteProperty(PropName,aSchema.Comment);
        jskConst : WriteProperty(PropName,aSchema.Validations.constValue);
        jskUnevaluatedItems : WriteProperty(PropName,aSchema.UnevaluatedItems);
        jskUnevaluatedProperties : WriteProperty(PropName,aSchema.UnevaluatedProperties);
        jskContentEncoding : WriteProperty(PropName,aSchema.Validations.contentEncoding);
        jskContentMediaType : WriteProperty(PropName,aSchema.Validations.contentMediaType);
        jskContentSchema : WriteProperty(PropName,aSchema.Validations.contentSchema);
        jskExamples : WriteProperty(PropName,aSchema.Metadata.Examples);
        jskDeprecated : WriteProperty(PropName,aSchema.Metadata.Deprecated);
        jskReadOnly : WriteProperty(PropName,aSchema.Metadata.ReadOnly);
        jskWriteOnly : WriteProperty(PropName,aSchema.Metadata.WriteOnly);
        jskVocabulary : WriteProperty(PropName,aSchema.Vocabulary);
        end;
        end;
    EndObject;
    end;
end;

{ TJSONSchemaWriterJSON }

function TJSONSchemaWriterJSON.CurrentStruct: TJSONData;
begin
  Result:=Nil;
  if FCount>0 then
    begin
    Result:=FStack[FCount-1];
    if not (Result.JSONType in StructuredJSONTypes) then
      Result:=Nil;
    end;
end;

procedure TJSONSchemaWriterJSON.PushData(Obj: TJSONData);

var
  D : TJSONData;
  O : TJSONObject absolute D;
  A : TJSONArray absolute D;
  AddToStack : Boolean;

begin
  AddToStack:=(Obj.JSONType in StructuredJSONTypes) or (FCount=0);
  D:=CurrentStruct;
  if (D=Nil) then
    begin
    if (FCount>0) then
      Raise EJSONSchemaWriter.Create(SErrNoPushOnSimpleValue);
    end
  else
    Case D.JSONType of
    jtObject:
      begin
      if FPropertyName = '' then
        Raise EJSONSchemaWriter.Create(SErrNoPropertyNameForPush);
      O.Add(FPropertyName,Obj);
      FPropertyName:='';
      end;
    jtArray:
      begin
      A.Add(Obj);
      FPropertyName:='';
      end;
    end;
  if AddToStack then
    begin
    if FCount=Length(FStack) then
      SetLength(FStack,FCount+10);
    FStack[FCount]:=Obj;
    Inc(FCount);
    end;
end;

procedure TJSONSchemaWriterJSON.PopData;
begin
 if FCount=0 then
   Raise EJSONSchemaWriter.Create(SErrCannotPop);
 Dec(FCount);
end;

procedure TJSONSchemaWriterJSON.EndArray;
begin
  PopData;
end;

procedure TJSONSchemaWriterJSON.EndObject;
begin
  PopData;
end;

procedure TJSONSchemaWriterJSON.EndProperty;
begin
  If CurrentStruct=Nil then
    Raise EJSONSchemaWriter.Create(SErrNotAtStructuredValue);
end;

procedure TJSONSchemaWriterJSON.NextElement;
begin
  If CurrentStruct=Nil then
    Raise EJSONSchemaWriter.Create(SErrNotAtStructuredValue);
end;

procedure TJSONSchemaWriterJSON.StartArray;
begin
  PushData(TJSONArray.Create);
end;

procedure TJSONSchemaWriterJSON.StartObject;
begin
  PushData(TJSONObject.Create);
end;

procedure TJSONSchemaWriterJSON.StartProperty(const aName: string);
begin
  if FPropertyName<>'' then
    Raise EJSONSchemaWriter.CreateFmt(SPropertyNameAlreadySet,[aName,FPropertyName]);
  FPropertyName:=aName;
end;

procedure TJSONSchemaWriterJSON.WriteValue(aValue: Boolean);
begin
  PushData(TJSONBoolean.Create(aValue));
end;

procedure TJSONSchemaWriterJSON.WriteValue(aValue: Double);
begin
  PushData(TJSONFloatNumber.Create(aValue));
end;

procedure TJSONSchemaWriterJSON.WriteValue(aValue: Int64);
begin
  PushData(TJSONInt64Number.Create(aValue));
end;

procedure TJSONSchemaWriterJSON.WriteValue(aValue: Integer);
begin
  PushData(TJSONIntegerNumber.Create(aValue));
end;

procedure TJSONSchemaWriterJSON.WriteValue(const aValue: String);
begin
  PushData(TJSONString.Create(aValue));
end;

procedure TJSONSchemaWriterJSON.WriteValue;
begin
  PushData(TJSONNull.Create);
end;

function TJSONSchemaWriterJSON.WriteSchema(aSchema: TJSONSchema): TJSONData;
begin
  DoWriteSchema(aSchema);
  if (Length(FStack)=0) or Not Assigned(FStack[0]) then
    Raise EJSONSchemaWriter.Create(SErrNoObjectsOnStack);
  Result:=FStack[0];
end;

{ TJSONSchemaWriterStream }

procedure TJSONSchemaWriterStream.WriteSchema(aSchema: TJSONSchema; aStream: TStream);
begin
  FStream:=aStream;
  DoWriteSchema(aSchema);
end;

procedure TJSONSchemaWriterStream.PushElCount;
begin
  if FLen=Length(FCounts) then
    SetLength(FCounts,FLen+10);
  FCounts[FLen]:=0;
  Inc(Flen);
end;

procedure TJSONSchemaWriterStream.PopElCount;
begin
  if FLen>0 then
    Dec(FLen);
end;

procedure TJSONSchemaWriterStream.IncElCount;
begin
  if Flen>0 then
    Inc(FCounts[FLen-1]);
end;

function TJSONSchemaWriterStream.ElCount: Integer;
begin
  Result:=FCounts[FLen-1];
end;

procedure TJSONSchemaWriterStream.WriteString(const aString: TJSONStringType);
begin
  if Length(aString)>0 then
    FStream.WriteBuffer(aString[1],Length(aString))
end;

procedure TJSONSchemaWriterStream.WriteValue();
begin
  WriteString('null');
end;

procedure TJSONSchemaWriterStream.WriteValue(aValue: Boolean);
begin
  WriteString(BoolToStr(aValue,'true','false'));
end;

procedure TJSONSchemaWriterStream.WriteValue(aValue: Integer);
begin
  WriteString(IntToStr(aValue));
end;

procedure TJSONSchemaWriterStream.WriteValue(aValue: Int64);
begin
  WriteString(IntToStr(aValue));
end;

procedure TJSONSchemaWriterStream.WriteValue(aValue: Double);

var
  s : String;
begin
  Str(aValue,s);
  WriteString(S);
end;

procedure TJSONSchemaWriterStream.WriteValue(const aValue: String);

begin
  WriteString('"'+StringToJSONString(aValue,StrictStrings)+'"');
end;

procedure TJSONSchemaWriterStream.StartProperty(const aName: string);
begin
  if ElCount>0 then
    NextElement;
  WriteString('"'+StringToJSONString(aName,StrictStrings)+'":');
  IncElCount;
end;

procedure TJSONSchemaWriterStream.EndProperty;
begin
  // Nothing
end;

procedure TJSONSchemaWriterStream.StartArray;
begin
  WriteString('[');
  PushElCount;
end;

procedure TJSONSchemaWriterStream.EndArray;
begin
  PopElCount;
  WriteString(']');
end;

procedure TJSONSchemaWriterStream.NextElement;
begin
  if ElCount>0 then
    WriteString(',');
  IncElCount;
end;


procedure TJSONSchemaWriterStream.StartObject;
begin
  WriteString('{');
  PushElCount;
end;

procedure TJSONSchemaWriterStream.EndObject;
begin
  WriteString('}');
  PopElCount;
end;

end.

