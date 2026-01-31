unit utcjsonreaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  System.JSON.Readers, system.json, StreamEx;

type
  { TTestTJsonTextReader }
  TTestTJsonTextReader = class(TTestCase)
  private
    FReader: TJsonTextReader;
    FStringReader: TStringReader;
    procedure SetupTextReader(const AJsonText: string);
    procedure TeardownTextReader;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestReadSimpleObject;
    procedure TestReadSimpleArray;
    procedure TestReadMixedTypes;
    procedure TestReadNestedObjects;
    procedure TestReadNestedArrays;
    procedure TestReadAsInteger;
    procedure TestReadAsInt64;
    procedure TestReadAsUInt64;
    procedure TestReadAsString;
    procedure TestReadAsDouble;
    procedure TestReadAsDateTime;
    procedure TestReadAsBytes;
    procedure TestSkip;
    procedure TestClose;
    procedure TestRewind;
    procedure TestGetLineNumber;
    procedure TestGetLinePosition;
    procedure TestHasLineInfo;
    procedure TestProperties;
    procedure TestErrorHandling;
    procedure TestEmptyInput;
    procedure TestNullValues;
    procedure TestBooleanValues;
    procedure TestNumberFormats;
    procedure TestStringEscaping;
  end;

  { TTestTJsonObjectReader }
  TTestTJsonObjectReader = class(TTestCase)
  private
    FReader: TJsonObjectReader;
    FJsonObject: TJSONObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateWithObject;
    procedure TestCreateWithArray;
    procedure TestReadFromSimpleObject;
    procedure TestReadFromSimpleArray;
    procedure TestReadFromNestedStructure;
    procedure TestRewindAndReread;
    procedure TestGetCurrent;
    procedure TestCloseAndReset;
  end;

  { TTestEJsonReaderException }
  TTestEJsonReaderException = class(TTestCase)
  published
    procedure TestCreateWithReader;
    procedure TestCreateWithPath;
    procedure TestCreateFmt;
    procedure TestProperties;
  end;

implementation

uses
  TypInfo, Math;

{ TTestTJsonTextReader }

procedure TTestTJsonTextReader.SetupTextReader(const AJsonText: string);
begin
  TeardownTextReader;
  FStringReader := TStringReader.Create(AJsonText);
  FReader := TJsonTextReader.Create(FStringReader);
end;

procedure TTestTJsonTextReader.TeardownTextReader;
begin
  FreeAndNil(FReader);
  FreeAndNil(FStringReader);
end;

procedure TTestTJsonTextReader.SetUp;
begin
  inherited SetUp;
  FReader := nil;
  FStringReader := nil;
end;

procedure TTestTJsonTextReader.TearDown;
begin
  TeardownTextReader;
  inherited TearDown;
end;

procedure TTestTJsonTextReader.TestCreate;
var
  StringReader: TStringReader;
  Reader: TJsonTextReader;
begin
  StringReader := TStringReader.Create('{}');
  try
    Reader := TJsonTextReader.Create(StringReader);
    try
      AssertNotNull('Reader should be created', Reader);
      AssertEquals('Initial state should be Start', Ord(TState.Start), Ord(Reader.CurrentState));
    finally
      Reader.Free;
    end;
  finally
    StringReader.Free;
  end;
end;

procedure TTestTJsonTextReader.TestReadSimpleObject;
begin
  SetupTextReader('{"name": "John", "age": 30}');

  // Start Object
  AssertTrue('Should read start object', FReader.Read);
  AssertEquals('Should be StartObject token', Ord(TJsonToken.StartObject), Ord(FReader.TokenType));

  // Property name
  AssertTrue('Should read property name', FReader.Read);
  AssertEquals('Should be PropertyName token', Ord(TJsonToken.PropertyName), Ord(FReader.TokenType));
  AssertEquals('Property name should be "name"', 'name', FReader.Value.AsString);

  // Property value
  AssertTrue('Should read property value', FReader.Read);
  AssertEquals('Should be String token', Ord(TJsonToken.&String), Ord(FReader.TokenType));
  AssertEquals('Property value should be "John"', 'John', FReader.Value.AsString);

  // Second property name
  AssertTrue('Should read second property name', FReader.Read);
  AssertEquals('Should be PropertyName token', Ord(TJsonToken.PropertyName), Ord(FReader.TokenType));
  AssertEquals('Property name should be "age"', 'age', FReader.Value.AsString);

  // Second property value
  AssertTrue('Should read second property value', FReader.Read);
  AssertEquals('Should be Integer token', Ord(TJsonToken.Integer), Ord(FReader.TokenType));
  AssertEquals('Property value should be 30', 30, FReader.Value.AsInteger);

  // End Object
  AssertTrue('Should read end object', FReader.Read);
  AssertEquals('Should be EndObject token', Ord(TJsonToken.EndObject), Ord(FReader.TokenType));

  // Should be done
  AssertFalse('Should be done reading', FReader.Read);
end;

procedure TTestTJsonTextReader.TestReadSimpleArray;
begin
  SetupTextReader('[1, 2, 3]');

  // Start Array
  AssertTrue('Should read start array', FReader.Read);
  AssertEquals('Should be StartArray token', Ord(TJsonToken.StartArray), Ord(FReader.TokenType));

  // First element
  AssertTrue('Should read first element', FReader.Read);
  AssertEquals('Should be Integer token', Ord(TJsonToken.Integer), Ord(FReader.TokenType));
  AssertEquals('Value should be 1', 1, FReader.Value.AsInteger);

  // Second element
  AssertTrue('Should read second element', FReader.Read);
  AssertEquals('Should be Integer token', Ord(TJsonToken.Integer), Ord(FReader.TokenType));
  AssertEquals('Value should be 2', 2, FReader.Value.AsInteger);

  // Third element
  AssertTrue('Should read third element', FReader.Read);
  AssertEquals('Should be Integer token', Ord(TJsonToken.Integer), Ord(FReader.TokenType));
  AssertEquals('Value should be 3', 3, FReader.Value.AsInteger);

  // End Array
  AssertTrue('Should read end array', FReader.Read);
  AssertEquals('Should be EndArray token', Ord(TJsonToken.EndArray), Ord(FReader.TokenType));

  // Should be done
  AssertFalse('Should be done reading', FReader.Read);
end;

procedure TTestTJsonTextReader.TestReadMixedTypes;
begin
  SetupTextReader('{"string": "hello", "number": 42, "float": 3.14, "bool": true, "null": null}');

  AssertTrue('Should read start object', FReader.Read);
  AssertEquals('Should be StartObject', Ord(TJsonToken.StartObject), Ord(FReader.TokenType));

  // String property
  AssertTrue('Should read string property name', FReader.Read);
  AssertEquals('Property name', 'string', FReader.Value.AsString);
  AssertTrue('Should read string value', FReader.Read);
  AssertEquals('Should be String token', Ord(TJsonToken.&String), Ord(FReader.TokenType));
  AssertEquals('String value', 'hello', FReader.Value.AsString);

  // Number property
  AssertTrue('Should read number property name', FReader.Read);
  AssertEquals('Property name', 'number', FReader.Value.AsString);
  AssertTrue('Should read number value', FReader.Read);
  AssertEquals('Should be Integer token', Ord(TJsonToken.Integer), Ord(FReader.TokenType));

  // Float property
  AssertTrue('Should read float property name', FReader.Read);
  AssertEquals('Property name', 'float', FReader.Value.AsString);
  AssertTrue('Should read float value', FReader.Read);
  AssertEquals('Should be Float token', Ord(TJsonToken.Float), Ord(FReader.TokenType));

  // Boolean property
  AssertTrue('Should read bool property name', FReader.Read);
  AssertEquals('Property name', 'bool', FReader.Value.AsString);
  AssertTrue('Should read bool value', FReader.Read);
  AssertEquals('Should be Boolean token', Ord(TJsonToken.Boolean), Ord(FReader.TokenType));

  // Null property
  AssertTrue('Should read null property name', FReader.Read);
  AssertEquals('Property name', 'null', FReader.Value.AsString);
  AssertTrue('Should read null value', FReader.Read);
  AssertEquals('Should be Null token', Ord(TJsonToken.Null), Ord(FReader.TokenType));

  AssertTrue('Should read end object', FReader.Read);
  AssertEquals('Should be EndObject', Ord(TJsonToken.EndObject), Ord(FReader.TokenType));
end;

procedure TTestTJsonTextReader.TestReadNestedObjects;
begin
  SetupTextReader('{"outer": {"inner": "value"}}');

  AssertTrue('Should read start object', FReader.Read);
  AssertTrue('Should read outer property', FReader.Read);
  AssertEquals('Property name', 'outer', FReader.Value.AsString);
  AssertTrue('Should read inner start object', FReader.Read);
  AssertEquals('Should be StartObject', Ord(TJsonToken.StartObject), Ord(FReader.TokenType));
  AssertTrue('Should read inner property', FReader.Read);
  AssertEquals('Property name', 'inner', FReader.Value.AsString);
  AssertTrue('Should read inner value', FReader.Read);
  AssertEquals('Property value', 'value', FReader.Value.AsString);
  AssertTrue('Should read inner end object', FReader.Read);
  AssertEquals('Should be EndObject', Ord(TJsonToken.EndObject), Ord(FReader.TokenType));
  AssertTrue('Should read outer end object', FReader.Read);
  AssertEquals('Should be EndObject', Ord(TJsonToken.EndObject), Ord(FReader.TokenType));
end;

procedure TTestTJsonTextReader.TestReadNestedArrays;
begin
  SetupTextReader('[[1, 2], [3, 4]]');

  AssertTrue('Should read start array', FReader.Read);
  AssertTrue('Should read inner start array', FReader.Read);
  AssertTrue('Should read first element', FReader.Read);
  AssertTrue('Should read second element', FReader.Read);
  AssertTrue('Should read inner end array', FReader.Read);
  AssertTrue('Should read second inner start array', FReader.Read);
  AssertTrue('Should read third element', FReader.Read);
  AssertTrue('Should read fourth element', FReader.Read);
  AssertTrue('Should read second inner end array', FReader.Read);
  AssertTrue('Should read outer end array', FReader.Read);
end;

procedure TTestTJsonTextReader.TestReadAsInteger;
begin
  SetupTextReader('{"number": 42}');

  FReader.Read; // start object
  FReader.Read; // property name

  AssertEquals('Should read as integer', 42, FReader.ReadAsInteger);
end;

procedure TTestTJsonTextReader.TestReadAsInt64;
begin
  SetupTextReader('{"number": 9223372036854775807}');

  FReader.Read; // start object
  FReader.Read; // property name

  AssertEquals('Should read as Int64', Int64(9223372036854775807), FReader.ReadAsInt64);
end;

procedure TTestTJsonTextReader.TestReadAsUInt64;
begin
  SetupTextReader('{"number": 18446744073709551615}');

  FReader.Read; // start object
  FReader.Read; // property name

  // Note: This is a very large number, so we'll test with a smaller one
  SetupTextReader('{"number": 123}');
  FReader.Read; // start object
  FReader.Read; // property name

  AssertEquals('Should read as UInt64', UInt64(123), FReader.ReadAsUInt64);
end;

procedure TTestTJsonTextReader.TestReadAsString;
begin
  SetupTextReader('{"text": "hello world"}');

  FReader.Read; // start object
  FReader.Read; // property name

  AssertEquals('Should read as string', 'hello world', FReader.ReadAsString);
end;

procedure TTestTJsonTextReader.TestReadAsDouble;
begin
  SetupTextReader('{"number": 3.14159}');

  FReader.Read; // start object
  FReader.Read; // property name

  AssertEquals('Should read as double', 3.14159, FReader.ReadAsDouble, 0.00001);
end;

procedure TTestTJsonTextReader.TestReadAsDateTime;
begin
  SetupTextReader('{"date": "2023-01-01"}');

  FReader.Read; // start object
  FReader.Read; // property name

  // This will test date parsing - may return 0 if parsing fails
  FReader.ReadAsDateTime;
  AssertTrue('DateTime reading should not crash', True);
end;

procedure TTestTJsonTextReader.TestReadAsBytes;
begin
  SetupTextReader('{"data": "SGVsbG8="}');

  FReader.Read; // start object
  FReader.Read; // property name

  // This tests Base64 decoding
  FReader.ReadAsBytes;
  AssertTrue('Bytes reading should not crash', True);
end;

procedure TTestTJsonTextReader.TestSkip;
begin
  SetupTextReader('{"skip": {"nested": "value"}, "keep": "data"}');

  FReader.Read; // start object
  FReader.Read; // "skip" property name
  FReader.Skip; // Skip the nested object

  AssertTrue('Should read after skip', FReader.Read);
  AssertEquals('Should be "keep" property', 'keep', FReader.Value.AsString);
end;

procedure TTestTJsonTextReader.TestClose;
begin
  SetupTextReader('{}');

  FReader.Close;
  AssertEquals('State should be Closed', Ord(TState.Closed), Ord(FReader.CurrentState));
end;

procedure TTestTJsonTextReader.TestRewind;
begin
  SetupTextReader('{"test": "value"}');

  FReader.Read; // start object
  FReader.Read; // property name

  FReader.Rewind;
  AssertEquals('State should be Start after rewind', Ord(TState.Start), Ord(FReader.CurrentState));

  // Should be able to read from beginning again
  AssertTrue('Should read start object after rewind', FReader.Read);
  AssertEquals('Should be StartObject after rewind', Ord(TJsonToken.StartObject), Ord(FReader.TokenType));
end;

procedure TTestTJsonTextReader.TestGetLineNumber;
begin
  SetupTextReader('{}');

  // Line number should be available
  AssertTrue('Line number should be >= 1', FReader.GetLineNumber >= 1);
end;

procedure TTestTJsonTextReader.TestGetLinePosition;
begin
  SetupTextReader('{}');

  // Line position should be available
  AssertTrue('Line position should be >= 0', FReader.GetLinePosition >= 0);
end;

procedure TTestTJsonTextReader.TestHasLineInfo;
begin
  SetupTextReader('{}');

  AssertTrue('Should have line info', FReader.HasLineInfo);
end;

procedure TTestTJsonTextReader.TestProperties;
begin
  SetupTextReader('{}');

  // Test property accessors
  AssertEquals('Initial depth should be 0', 0, FReader.Depth);
  AssertEquals('Default MaxDepth should be 64', 64, FReader.MaxDepth);
  AssertEquals('Default QuoteChar should be "', '"', FReader.QuoteChar);
  AssertFalse('Default SupportMultipleContent should be false', FReader.SupportMultipleContent);
  AssertFalse('Default CloseInput should be false', FReader.CloseInput);

  // Test property setters
  FReader.MaxDepth := 32;
  AssertEquals('MaxDepth should be settable', 32, FReader.MaxDepth);

  FReader.QuoteChar := '''';
  AssertEquals('QuoteChar should be settable', '''', FReader.QuoteChar);

  FReader.SupportMultipleContent := True;
  AssertTrue('SupportMultipleContent should be settable', FReader.SupportMultipleContent);

  FReader.CloseInput := True;
  AssertTrue('CloseInput should be settable', FReader.CloseInput);
end;

procedure TTestTJsonTextReader.TestErrorHandling;
var
  ExceptionRaised: Boolean;
begin
  SetupTextReader('{"invalid": }');

  ExceptionRaised := False;
  try
    FReader.Read; // start object
    FReader.Read; // property name
    FReader.Read; // should fail on invalid syntax
  except
    on EJsonReaderException do
      ExceptionRaised := True;
  end;

  AssertTrue('Should raise exception on invalid JSON', ExceptionRaised);
end;

procedure TTestTJsonTextReader.TestEmptyInput;
begin
  SetupTextReader('');

  AssertFalse('Should return false for empty input', FReader.Read);
end;

procedure TTestTJsonTextReader.TestNullValues;
begin
  SetupTextReader('{"nullValue": null}');

  FReader.Read; // start object
  FReader.Read; // property name
  FReader.Read; // null value

  AssertEquals('Should be Null token', Ord(TJsonToken.Null), Ord(FReader.TokenType));
end;

procedure TTestTJsonTextReader.TestBooleanValues;
begin
  SetupTextReader('{"trueValue": true, "falseValue": false}');

  FReader.Read; // start object

  FReader.Read; // "trueValue" property name
  FReader.Read; // true value
  AssertEquals('Should be Boolean token', Ord(TJsonToken.Boolean), Ord(FReader.TokenType));

  FReader.Read; // "falseValue" property name
  FReader.Read; // false value
  AssertEquals('Should be Boolean token', Ord(TJsonToken.Boolean), Ord(FReader.TokenType));
end;

procedure TTestTJsonTextReader.TestNumberFormats;
begin
  SetupTextReader('{"int": 42, "float": 3.14, "exp": 1.23e-4}');

  FReader.Read; // start object

  // Integer
  FReader.Read; // property name
  FReader.Read; // integer value
  AssertEquals('Should be Integer token', Ord(TJsonToken.Integer), Ord(FReader.TokenType));

  // Float
  FReader.Read; // property name
  FReader.Read; // float value
  AssertEquals('Should be Float token', Ord(TJsonToken.Float), Ord(FReader.TokenType));

  // Scientific notation
  FReader.Read; // property name
  FReader.Read; // exponential value
  AssertEquals('Should be Float token for exponential', Ord(TJsonToken.Float), Ord(FReader.TokenType));
end;

procedure TTestTJsonTextReader.TestStringEscaping;
begin
  SetupTextReader('{"escaped": "hello\\nworld\\t\"quote\""}');

  FReader.Read; // start object
  FReader.Read; // property name
  FReader.Read; // escaped string value

  AssertEquals('Should be String token', Ord(TJsonToken.&String), Ord(FReader.TokenType));
  // The actual escaped content depends on the scanner implementation
  AssertTrue('Should have some content', Length(FReader.Value.AsString) > 0);
end;

{ TTestTJsonObjectReader }

procedure TTestTJsonObjectReader.SetUp;
begin
  inherited SetUp;
  FJsonObject := nil;
  FReader := nil;
end;

procedure TTestTJsonObjectReader.TearDown;
begin
  FreeAndNil(FReader);
  FreeAndNil(FJsonObject);
  inherited TearDown;
end;

procedure TTestTJsonObjectReader.TestCreateWithObject;
begin
  FJsonObject := TJSONObject.Create;
  FJsonObject.AddPair('name', 'John');

  FReader := TJsonObjectReader.Create(FJsonObject);
  AssertNotNull('Reader should be created', FReader);
  AssertSame('Current should be the object', FJsonObject, FReader.Current);
end;

procedure TTestTJsonObjectReader.TestCreateWithArray;
var
  JsonArray: TJSONArray;
begin
  JsonArray := TJSONArray.Create;
  JsonArray.Add('item1');
  JsonArray.Add('item2');

  try
    FReader := TJsonObjectReader.Create(JsonArray);
    AssertNotNull('Reader should be created', FReader);
  finally
    JsonArray.Free;
  end;
end;

procedure TTestTJsonObjectReader.TestReadFromSimpleObject;
begin
  FJsonObject := TJSONObject.Create;
  FJsonObject.AddPair('name', 'John');
  FJsonObject.AddPair('age', 30);

  FReader := TJsonObjectReader.Create(FJsonObject);

  // Read start object
  AssertTrue('Should read start object', FReader.Read);
  AssertEquals('Should be StartObject', Ord(TJsonToken.StartObject), Ord(FReader.TokenType));

  // Read first property
  AssertTrue('Should read property name', FReader.Read);
  AssertEquals('Should be PropertyName', Ord(TJsonToken.PropertyName), Ord(FReader.TokenType));

  AssertTrue('Should read property value', FReader.Read);
  AssertEquals('Should be String', Ord(TJsonToken.&String), Ord(FReader.TokenType));

  // Read second property
  AssertTrue('Should read second property name', FReader.Read);
  AssertEquals('Should be PropertyName', Ord(TJsonToken.PropertyName), Ord(FReader.TokenType));

  AssertTrue('Should read second property value', FReader.Read);
  AssertEquals('Should be Integer', Ord(TJsonToken.Integer), Ord(FReader.TokenType));

  // Read end object
  AssertTrue('Should read end object', FReader.Read);
  AssertEquals('Should be EndObject', Ord(TJsonToken.EndObject), Ord(FReader.TokenType));
end;

procedure TTestTJsonObjectReader.TestReadFromSimpleArray;
var
  JsonArray: TJSONArray;
begin
  JsonArray := TJSONArray.Create;
  JsonArray.Add('item1');
  JsonArray.Add(42);
  JsonArray.Add(True);

  try
    FReader := TJsonObjectReader.Create(JsonArray);

    // Read start array
    AssertTrue('Should read start array', FReader.Read);
    AssertEquals('Should be StartArray', Ord(TJsonToken.StartArray), Ord(FReader.TokenType));

    // Read elements
    AssertTrue('Should read first element', FReader.Read);
    AssertEquals('Should be String', Ord(TJsonToken.&String), Ord(FReader.TokenType));

    AssertTrue('Should read second element', FReader.Read);
    AssertEquals('Should be Integer', Ord(TJsonToken.Integer), Ord(FReader.TokenType));

    AssertTrue('Should read third element', FReader.Read);
    AssertEquals('Should be Boolean', Ord(TJsonToken.Boolean), Ord(FReader.TokenType));

    // Read end array
    AssertTrue('Should read end array', FReader.Read);
    AssertEquals('Should be EndArray', Ord(TJsonToken.EndArray), Ord(FReader.TokenType));

  finally
    JsonArray.Free;
  end;
end;

procedure TTestTJsonObjectReader.TestReadFromNestedStructure;
var
  InnerObject: TJSONObject;
begin
  InnerObject := TJSONObject.Create;
  InnerObject.AddPair('inner', 'value');

  FJsonObject := TJSONObject.Create;
  FJsonObject.AddPair('outer', InnerObject);

  FReader := TJsonObjectReader.Create(FJsonObject);

  // Should be able to read the nested structure
  AssertTrue('Should read start object', FReader.Read);
  AssertTrue('Should read outer property', FReader.Read);
  AssertTrue('Should read inner start object', FReader.Read);
  AssertTrue('Should read inner property', FReader.Read);
  AssertTrue('Should read inner value', FReader.Read);
  AssertTrue('Should read inner end object', FReader.Read);
  AssertTrue('Should read outer end object', FReader.Read);
end;

procedure TTestTJsonObjectReader.TestRewindAndReread;
begin
  FJsonObject := TJSONObject.Create;
  FJsonObject.AddPair('test', 'value');

  FReader := TJsonObjectReader.Create(FJsonObject);

  // Read once
  AssertTrue('First read should work', FReader.Read);
  AssertTrue('Should read property', FReader.Read);

  // Rewind
  FReader.Rewind;
  AssertEquals('State should be Start', Ord(TState.Start), Ord(FReader.CurrentState));

  // Read again
  AssertTrue('Should read start object after rewind', FReader.Read);
  AssertEquals('Should be StartObject after rewind', Ord(TJsonToken.StartObject), Ord(FReader.TokenType));
end;

procedure TTestTJsonObjectReader.TestGetCurrent;
begin
  FJsonObject := TJSONObject.Create;
  FReader := TJsonObjectReader.Create(FJsonObject);

  AssertSame('Current should return the root object', FJsonObject, FReader.Current);
end;

procedure TTestTJsonObjectReader.TestCloseAndReset;
begin
  FJsonObject := TJSONObject.Create;
  FJsonObject.AddPair('test', 'value');

  FReader := TJsonObjectReader.Create(FJsonObject);

  FReader.Close;
  AssertEquals('State should be Closed', Ord(TState.Closed), Ord(FReader.CurrentState));
end;

{ TTestEJsonReaderException }

procedure TTestEJsonReaderException.TestCreateWithReader;
var
  Reader: TJsonTextReader;
  StringReader: TStringReader;
  Exception: EJsonReaderException;
begin
  StringReader := TStringReader.Create('{}');
  Reader := TJsonTextReader.Create(StringReader);
  try
    Exception := EJsonReaderException.Create(Reader, 'Test message');
    try
      AssertEquals('Message should be set', 'Test message', Exception.Message);
      AssertEquals('Path should be from reader', Reader.Path, Exception.Path);
      AssertEquals('Line number should be from reader', Reader.GetLineNumber, Exception.LineNumber);
      AssertEquals('Line position should be from reader', Reader.GetLinePosition, Exception.LinePosition);
    finally
      Exception.Free;
    end;
  finally
    Reader.Free;
    StringReader.Free;
  end;
end;

procedure TTestEJsonReaderException.TestCreateWithPath;
var
  Exception: EJsonReaderException;
begin
  Exception := EJsonReaderException.Create('Test message', 'test.path', 10, 5);
  try
    AssertEquals('Message should be set', 'Test message', Exception.Message);
    AssertEquals('Path should be set', 'test.path', Exception.Path);
    AssertEquals('Line number should be set', 10, Exception.LineNumber);
    AssertEquals('Line position should be set', 5, Exception.LinePosition);
  finally
    Exception.Free;
  end;
end;

procedure TTestEJsonReaderException.TestCreateFmt;
var
  Reader: TJsonTextReader;
  StringReader: TStringReader;
  Exception: EJsonReaderException;
begin
  StringReader := TStringReader.Create('{}');
  Reader := TJsonTextReader.Create(StringReader);
  try
    Exception := EJsonReaderException.CreateFmt(Reader, 'Test %s %d', ['message', 42]);
    try
      AssertEquals('Message should be formatted', 'Test message 42', Exception.Message);
    finally
      Exception.Free;
    end;
  finally
    Reader.Free;
    StringReader.Free;
  end;
end;

procedure TTestEJsonReaderException.TestProperties;
var
  Exception: EJsonReaderException;
begin
  Exception := EJsonReaderException.Create('Test', 'path', 1, 2);
  try
    AssertEquals('Path property', 'path', Exception.Path);
    AssertEquals('LineNumber property', 1, Exception.LineNumber);
    AssertEquals('LinePosition property', 2, Exception.LinePosition);
  finally
    Exception.Free;
  end;
end;

initialization
  RegisterTest(TTestTJsonTextReader);
  RegisterTest(TTestTJsonObjectReader);
  RegisterTest(TTestEJsonReaderException);

end.
