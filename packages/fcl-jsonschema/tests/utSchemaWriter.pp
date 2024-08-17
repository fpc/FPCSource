{
    This file is part of the Free Component Library

    Testsuite for JSONSchema writer
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utSchemaWriter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson,fpjson.schema.types, fpjson.schema.schema, fpjson.schema.writer;

Type

   { TTestSchemaWriter }

   TTestSchemaWriter = Class(TTestCase)
   Private
     FSchema : TJSONSchema;
     FStream: TStringStream;
   Public
     Procedure Setup; override;
     Procedure TearDown; override;
     Procedure CheckStream(aJSON : String); virtual; abstract;
     Property Schema : TJSONSchema Read FSchema;
     Property Stream : TStringStream Read FStream;
   Published
     Procedure TestHookup;
     Procedure TestAny;
     Procedure TestNone;
     Procedure TestEmpty;
     Procedure TestMetaDataSchema;
     Procedure TestRequired;
     procedure TestJSONValue;
     procedure TestDefaultValueObject;
     procedure TestDefaultValueSimpleValue;
     procedure TestDefaultValueArray;
     procedure TestVocabulary;
     procedure TestDependentRequired;
     procedure TestTypes;
   end;

   TTestStreamWriter = Class(TTestSchemaWriter)
   Public
     Procedure CheckStream(aJSON : String); override;
   end;

   { TTestJSONWriter }

   TTestJSONWriter = Class(TTestSchemaWriter)
   Public
     Procedure CheckStream(aJSON : String); override;
   end;


implementation

{ TTestSchemaWriter }

procedure TTestSchemaWriter.Setup;
begin
  inherited Setup;
  FSchema:=TJSONSchema.Create;
  FStream:=TStringStream.Create('');
end;

procedure TTestSchemaWriter.TearDown;
begin
  FreeAndNil(FStream);
  FreeAndNil(FSchema);
  inherited TearDown;
end;

procedure TTestSchemaWriter.TestHookup;
begin
  AssertNotNull('Have schema',Schema);
  AssertNotNull('Have stream',Stream);
end;

procedure TTestSchemaWriter.TestAny;
begin
  Schema.MatchType:=smAny;
  CheckStream('true');
end;

procedure TTestSchemaWriter.TestNone;
begin
  Schema.MatchType:=smNone;
  CheckStream('false');
end;

procedure TTestSchemaWriter.TestEmpty;
begin
  Schema.MatchType:=smConstrained;
  CheckStream('{}');
end;

procedure TTestSchemaWriter.TestMetaDataSchema;
begin
  Schema.MetaData.Title:='soso';
  Schema.MetaData.Description:='many';
  CheckStream('{"title":"soso","description":"many"}');
end;

procedure TTestSchemaWriter.TestRequired;
begin
  Schema.Validations.Required.Add('one');
  Schema.Validations.Required.Add('two');
  Schema.Validations.Required.Add('three');
  CheckStream('{"required":["one","two","three"]}');
end;

procedure TTestSchemaWriter.TestDefaultValueObject;
begin
  Schema.MetaData.DefaultValue:=TJSONObject.Create(['one',1]);
  CheckStream('{"default":{"one":1}}');
end;

procedure TTestSchemaWriter.TestDefaultValueSimpleValue;
begin
  Schema.MetaData.DefaultValue:=TJSONString.Create('self');
  CheckStream('{"default":"self"}');
end;

procedure TTestSchemaWriter.TestDefaultValueArray;
begin
  Schema.MetaData.DefaultValue:=TJSONArray.Create(['self']);
  CheckStream('{"default":["self"]}');
end;

procedure TTestSchemaWriter.TestVocabulary;
begin
  Schema.Vocabulary.AddVocabulary('http://www.freepascal.org/voc').Enabled:=true;
  Schema.Vocabulary.AddVocabulary('http://www.freepascal.org/voc2').Enabled:=true;
  CheckStream('{"$vocabulary":{"http://www.freepascal.org/voc":true,"http://www.freepascal.org/voc2":true}}');
end;

procedure TTestSchemaWriter.TestDependentRequired;
begin
  With Schema.Validations.DependentRequired.AddDependent('license') do
    begin
    Required.Add('one');
    Required.Add('two');
    Required.Add('three');
    end;
  CheckStream('{"dependentRequired":{"license":["one","two","three"]}}');
end;

procedure TTestSchemaWriter.TestTypes;
begin
  Schema.Validations.Types:=[sstString,sstNull,sstNumber];
  CheckStream('{"type":["null","number","string"]}');
end;

procedure TTestSchemaWriter.TestJSONValue;
begin
  Schema.Validations.constValue:=TJSONString.Create('self');
  CheckStream('{"const":"self"}');
end;

{ TTestStreamWriter }

procedure TTestStreamWriter.CheckStream(aJSON: String);
begin
  With TJSONSchemaWriterStream.Create(Nil) do
    try
      WriteSchema(Schema,Stream);
    finally
      Free;
    end;
  AssertEquals('Streamed content',aJSON,Stream.DataString);
end;

{ TTestJSONWriter }

procedure TTestJSONWriter.CheckStream(aJSON: String);

var
  D : TJSONData;

begin
  D:=Nil;
  try
    With TJSONSchemaWriterJSON.Create(Nil) do
      try
        D:=WriteSchema(Schema);
      finally
        Free;
      end;
    AssertEquals('Streamed content',aJSON,D.FormatJSON([foSingleLineObject,foSingleLineArray,foSkipWhiteSpace],0));
    {
    foSingleLineArray,   // Array without CR/LF : all on one line
                       foSingleLineObject,  // Object without CR/LF : all on one line
                       foDoNotQuoteMembers, // Do not quote object member names.
                       foUseTabchar,        // Use tab characters instead of spaces.
                       foSkipWhiteSpace,    // Do not use whitespace at all

    }
  finally
    D.Free;
  end;
end;


initialization
  RegisterTests([TTestStreamWriter,TTestJSONWriter])
end.

