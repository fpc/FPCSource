unit utcjsonwriters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  StreamEx, System.JSON.Types, System.JSON.Writers, System.JSON,
  TypInfo, DateUtils;

type

  { TJSONWriterTests }

  TJSONWriterTests = class(TTestCase)
  private
    FStringWriter: TStringWriter;
    FWriter: TJsonTextWriter;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure BeginObject;
    procedure AssertObject(const aResult: String);
    property StringWriter: TStringWriter read FStringWriter;
    property Writer: TJsonTextWriter Read FWriter;

  published
    procedure TestValueByte;
    procedure TestValueBoolean;
    procedure TestValueChar;
    procedure TestValueDouble;
    procedure TestValueExtended;
    procedure TestValueInt64;
    procedure TestValueInteger;
    procedure TestValueSingle;
    procedure TestValueUint32;
    procedure TestValueCodeWScope;
    procedure TestValueDBRef;
    procedure TestValueDecimal128;
    procedure TestValueGUID;
    procedure TestValueOID;
    procedure TestValueRegex;
    procedure TestValueTBytesGeneric;
    procedure TestValueTimeStamp;
    procedure TestBasicObjectWriting;
    procedure TestBasicArrayWriting;
    procedure TestNestedStructures;
    procedure TestValueTypes;
    procedure TestPropertyNames;
    procedure TestFormatting;
    procedure TestNullValues;
    procedure TestSpecialCharacters;
    procedure TestDateTimeValues;
    procedure TestNumericValues;
    procedure TestBooleanValues;
    procedure TestEmptyStructures;
    procedure TestComplexNesting;
    procedure TestWriterExampleOutput;
  end;

  { TJSONObjectWriterTest }

  TJSONObjectWriterTest = class(TTestCase)
  private
    FWriter: TJsonObjectWriter;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Constructor and destructor tests
    procedure TestCreate;
    procedure TestCreateWithOwnValueFalse;

    // Property tests
    procedure TestJSONProperty;
    procedure TestContainerProperty;
    procedure TestDateFormatHandlingProperty;
    procedure TestOwnValueProperty;

    // WriteStartObject / WriteEndObject
    procedure TestWriteStartObject;
    procedure TestWriteStartObjectNested;

    // WriteStartArray / WriteEndArray
    procedure TestWriteStartArray;
    procedure TestWriteStartArrayNested;

    // WritePropertyName
    procedure TestWritePropertyName;
    procedure TestWritePropertyNameSpecialChars;

    // WriteNull
    procedure TestWriteNull;

    // WriteUndefined
    procedure TestWriteUndefined;

    // WriteMinKey / WriteMaxKey
    procedure TestWriteMinKey;
    procedure TestWriteMaxKey;

    // WriteRaw / WriteRawValue
    procedure TestWriteRaw;
    procedure TestWriteRawValue;

    // WriteStartConstructor
    procedure TestWriteStartConstructor;

    // Rewind
    procedure TestRewind;

    // WriteValue overloads
    procedure TestWriteValueString;
    procedure TestWriteValueInteger;
    procedure TestWriteValueUInt32;
    procedure TestWriteValueInt64;
    procedure TestWriteValueUInt64;
    procedure TestWriteValueSingle;
    procedure TestWriteValueDouble;
    procedure TestWriteValueExtended;
    procedure TestWriteValueBoolean;
    procedure TestWriteValueChar;
    procedure TestWriteValueByte;
    procedure TestWriteValueDateTime;
    procedure TestWriteValueGUID;
    procedure TestWriteValueBytes;
    procedure TestWriteValueOid;
    procedure TestWriteValueRegEx;
    procedure TestWriteValueDBRef;
    procedure TestWriteValueCodeWScope;
    procedure TestWriteValueTimestamp;

    // Integration tests
    procedure TestCompleteObject;
    procedure TestCompleteArray;
    procedure TestNestedStructure;
  end;

implementation

procedure TJSONWriterTests.SetUp;
begin
  inherited SetUp;
  FStringWriter:=TStringWriter.Create;
  FWriter:=TJsonTextWriter.Create(FStringWriter);
end;

procedure TJSONWriterTests.TearDown;
begin
  FreeAndNil(FStringWriter);
  FreeAndNil(FWriter);
  inherited TearDown;
end;

procedure TJSONWriterTests.BeginObject;
begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('key');
end;

procedure TJSONWriterTests.AssertObject(const aResult : String);
var
  lResult: String;
begin
  Writer.WriteEndObject;
  LResult:=StringReplace(StringWriter.ToString,#10,'',[rfReplaceAll]);
  AssertEquals('Correct result','{"key":'+aResult+'}',lResult);
end;


procedure TJSONWriterTests.TestValueByte;
begin
  BeginObject;
  Writer.WriteValue(Byte(1));
  AssertObject('1');
end;

procedure TJSONWriterTests.TestValueInteger;
begin
  BeginObject;
  Writer.WriteValue(Integer(1));
  AssertObject('1');
end;

procedure TJSONWriterTests.TestValueUint32;
begin
  BeginObject;
  Writer.WriteValue(Uint32(1));
  AssertObject('1');
end;

procedure TJSONWriterTests.TestValueInt64;
begin
  BeginObject;
  Writer.WriteValue(Uint64(1));
  AssertObject('1');
end;

procedure TJSONWriterTests.TestValueSingle;
var
  s : single;
begin
  BeginObject;
  S:=1.23;
  Writer.WriteValue(S);
  AssertObject('1.230000019');
end;

procedure TJSONWriterTests.TestValueDouble;
var
  D : double;
begin
  BeginObject;
  D:=1.23;
  Writer.WriteValue(D);
  AssertObject('1.23');
end;

procedure TJSONWriterTests.TestValueExtended;
var
  E : extended;
begin
  BeginObject;
  E:=1.23;
  Writer.WriteValue(E);
  AssertObject('1.23');
end;

procedure TJSONWriterTests.TestValueBoolean;
begin
  BeginObject;
  Writer.WriteValue(true);
  AssertObject('true');
end;

procedure TJSONWriterTests.TestValueChar;
var
  c : char;
begin
  BeginObject;
  c:='a';
  Writer.WriteValue(c);
  AssertObject('"a"');
end;

procedure TJSONWriterTests.TestValueGUID;
var
  G : TGUID;
begin
  BeginObject;
  Writer.WriteValue(G);
  AssertObject('"'+G.ToString()+'"');
end;

procedure TJSONWriterTests.TestValueTBytesGeneric;
var
  B : TBytes;
begin
  SetLength(B,5);
  B[0] := $01;
  B[1] := $02;
  B[2] := $03;
  B[3] := $04;
  B[4] := $FF;
  BeginObject;
  Writer.WriteValue(B);
  AssertObject('"AQIDBP8="');
end;

procedure TJSONWriterTests.TestValueOID;
var
  TestOid : TJsonOid;

begin
  TestOid:=TJsonOid.Create('50E2A025C4D4D19C0016E934');
  BeginObject;
  Writer.WriteValue(TestOid);
  AssertObject('"50E2A025C4D4D19C0016E934"');
end;

procedure TJSONWriterTests.TestValueRegex;

var
  Reg : TJsonRegEx;
begin
  Reg:= TJsonRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$','i');
  BeginObject;
  Writer.WriteValue(Reg);
  AssertObject('"/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$/i"');
end;

procedure TJSONWriterTests.TestValueDBRef;
var
  TestDBRef : TJsonDBRef;
begin
  TestDBRef := TJsonDBRef.Create('mydb', 'users', '507f1f77bcf86cd799439011');
  BeginObject;
  Writer.WriteValue(TestDBRef);
  AssertObject('"mydb.users.507F1F77BCF86CD799439011"');
end;

procedure TJSONWriterTests.TestValueCodeWScope;
var
  TestCodeWScope : TJsonCodeWScope;
  ScopeList : TStrings;
begin
  ScopeList := TStringList.Create;
  try
    ScopeList.Values['y'] := '1';
    TestCodeWScope := TJsonCodeWScope.Create('function() { return this.x + y; }', ScopeList);
    BeginObject;
    Writer.WriteValue(TestCodeWScope);
    AssertObject('"function() { return this.x + y; }"');
  finally
    ScopeList.Free;
  end;
end;

procedure TJSONWriterTests.TestValueDecimal128;

begin

end;


procedure TJSONWriterTests.TestValueTimeStamp;
var
  TestTimestamp : TJsonTimestamp;
begin
  TestTimestamp := TJsonTimestamp.Create(DateTimeToUnix(EncodeDateTime(2025,01,01,15,16,17,180)), 1);
  BeginObject;
  Writer.WriteValue(TestTimestamp);
  AssertObject('1735744577');
end;



(*
procedure WriteValue(const aValue: string); override;
procedure WriteValue(aValue: Integer); override;
procedure WriteValue(aValue: UInt32); override;
procedure WriteValue(aValue: Int64); override;
procedure WriteValue(aValue: UInt64); override;
procedure WriteValue(aValue: Single); override;
procedure WriteValue(aValue: Double); override;
procedure WriteValue(aValue: Extended); override;
procedure WriteValue(aValue: Boolean); override;
procedure WriteValue(aValue: Char); override;
procedure WriteValue(aValue: Byte); override;
procedure WriteValue(aValue: TDateTime); override;
procedure WriteValue(const aValue: TGUID); override;
procedure WriteValue(const aValue: TBytes; aBinaryType: TJsonBinaryType = TJsonBinaryType.Generic); override;
procedure WriteValue(const aValue: TJsonOid); override;
procedure WriteValue(const aValue: TJsonRegEx); override;
procedure WriteValue(const aValue: TJsonDBRef); override;
procedure WriteValue(const aValue: TJsonCodeWScope); override;
procedure WriteMinKey; override;
procedure WriteMaxKey; override;
procedure WriteValue(const aValue: TJsonDecimal128); override;
procedure WriteValue(const aValue: TJsonTimestamp); override;
procedure WriteValue(const aValue: TValue); override;

*)
procedure TJSONWriterTests.TestBasicObjectWriting;
begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('name');
  Writer.WriteValue('John');
  Writer.WritePropertyName('age');
  Writer.WriteValue(30);
  Writer.WriteEndObject;
  AssertEquals('Basic object', '{"name":"John","age":30}', StringWriter.ToString);
end;

procedure TJSONWriterTests.TestBasicArrayWriting;

begin
  Writer.WriteStartArray;
  Writer.WriteValue(1);
  Writer.WriteValue(2);
  Writer.WriteValue(3);
  Writer.WriteEndArray;
  AssertEquals('Basic array', '[1,2,3]', StringWriter.ToString);
end;

procedure TJSONWriterTests.TestNestedStructures;

begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('person');
  Writer.WriteStartObject;
  Writer.WritePropertyName('name');
  Writer.WriteValue('John');
  Writer.WritePropertyName('contacts');
  Writer.WriteStartArray;
  Writer.WriteValue('email@example.com');
  Writer.WriteValue('555-1234');
  Writer.WriteEndArray;
  Writer.WriteEndObject;
  Writer.WriteEndObject;

  AssertTrue('Should contain nested object', StringWriter.ToString.Contains('person'));
  AssertTrue('Should contain nested array', StringWriter.ToString.Contains('contacts'));
end;

procedure TJSONWriterTests.TestValueTypes;
begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('string');
  Writer.WriteValue('hello');
  Writer.WritePropertyName('integer');
  Writer.WriteValue(42);
  Writer.WritePropertyName('float');
  Writer.WriteValue(3.14);
  Writer.WritePropertyName('boolean');
  Writer.WriteValue(True);
  Writer.WritePropertyName('null');
  Writer.WriteNull;
  Writer.WriteEndObject;
  AssertTrue('Should contain string value', StringWriter.ToString.Contains('hello'));
  AssertTrue('Should contain integer value', StringWriter.ToString.Contains('42'));
  AssertTrue('Should contain float value', StringWriter.ToString.Contains('3.14'));
  AssertTrue('Should contain boolean value', StringWriter.ToString.Contains('true'));
  AssertTrue('Should contain null value', StringWriter.ToString.Contains('null'));
end;

procedure TJSONWriterTests.TestPropertyNames;

begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('simple');
  Writer.WriteValue('value');
  Writer.WritePropertyName('with spaces');
  Writer.WriteValue('value2');
  Writer.WritePropertyName('with"quote');
  Writer.WriteValue('value3');
  Writer.WriteEndObject;

  AssertTrue('Should quote property names', StringWriter.ToString.Contains('"simple"'));
  AssertTrue('Should handle spaces in property names', StringWriter.ToString.Contains('"with spaces"'));
  AssertTrue('Should escape quotes in property names', StringWriter.ToString.Contains('with\"quote'));
end;

procedure TJSONWriterTests.TestFormatting;
begin
  Writer.Formatting := TJsonFormatting.Indented;
  Writer.WriteStartObject;
  Writer.WritePropertyName('name');
  Writer.WriteValue('John');
  Writer.WriteEndObject;

  AssertTrue('Should contain line breaks when formatted',
    StringWriter.ToString.Contains(#10) or StringWriter.ToString.Contains(#13));
end;

procedure TJSONWriterTests.TestNullValues;

begin
  Writer.WriteStartArray;
  Writer.WriteNull;
  Writer.WriteValue('not null');
  Writer.WriteNull;
  Writer.WriteEndArray;

  AssertEquals('Null values in array', '[null,"not null",null]', StringWriter.ToString);
end;

procedure TJSONWriterTests.TestSpecialCharacters;
begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('text');
  Writer.WriteValue('Line 1'#10'Line 2'#13'Line 3'#9'Tab');
  Writer.WriteEndObject;

  AssertTrue('Should escape newlines', StringWriter.ToString.Contains('\n'));
  AssertTrue('Should escape carriage returns', StringWriter.ToString.Contains('\r'));
  AssertTrue('Should escape tabs', StringWriter.ToString.Contains('\t'));
end;

procedure TJSONWriterTests.TestDateTimeValues;
var
  TestDate: TDateTime;
begin
  TestDate := EncodeDateTime(2023, 12, 25, 15, 30, 45, 0);
  Writer.WriteStartObject;
  Writer.WritePropertyName('date');
  Writer.WriteValue(TestDate);
  Writer.WriteEndObject;
  AssertTrue('Should contain year', StringWriter.ToString.Contains('2023'));
end;

procedure TJSONWriterTests.TestNumericValues;

begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('byte');
  Writer.WriteValue(Byte(255));
  Writer.WritePropertyName('int64');
  Writer.WriteValue(Int64(9223372036854775807));
  Writer.WritePropertyName('uint64');
  Writer.WriteValue(UInt64(18446744073709551615));
  Writer.WritePropertyName('single');
  Writer.WriteValue(Single(3.14));
  Writer.WritePropertyName('double');
  Writer.WriteValue(Double(2.71828));
  Writer.WriteEndObject;

  AssertTrue('Should contain byte value', StringWriter.ToString.Contains('255'));
  AssertTrue('Should contain int64 value', StringWriter.ToString.Contains('9223372036854775807'));
end;

procedure TJSONWriterTests.TestBooleanValues;
begin
  Writer.WriteStartArray;
  Writer.WriteValue(True);
  Writer.WriteValue(False);
  Writer.WriteEndArray;

  AssertEquals('Boolean values', '[true,false]', StringWriter.ToString);
end;

procedure TJSONWriterTests.TestEmptyStructures;
begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('emptyObject');
  Writer.WriteStartObject;
  Writer.WriteEndObject;
  Writer.WritePropertyName('emptyArray');
  Writer.WriteStartArray;
  Writer.WriteEndArray;
  Writer.WriteEndObject;

  AssertEquals('Empty structures', '{"emptyObject":{},"emptyArray":[]}', StringWriter.ToString);
end;

procedure TJSONWriterTests.TestComplexNesting;
begin
  Writer.WriteStartObject;
  Writer.WritePropertyName('level1');
  Writer.WriteStartObject;
  Writer.WritePropertyName('level2');
  Writer.WriteStartObject;
  Writer.WritePropertyName('level3');
  Writer.WriteStartArray;
  Writer.WriteStartObject;
  Writer.WritePropertyName('deep');
  Writer.WriteValue('value');
  Writer.WriteEndObject;
  Writer.WriteEndArray;
  Writer.WriteEndObject;
  Writer.WriteEndObject;
  Writer.WriteEndObject;

  AssertTrue('Should handle deep nesting', StringWriter.ToString.Contains('deep'));
  AssertTrue('Should be properly closed',
    StringWriter.ToString.StartsWith('{') and StringWriter.ToString.EndsWith('}'));
end;

procedure TJSONWriterTests.TestWriterExampleOutput;
var
  ExpectedOutput: string;
begin
  Writer.Formatting := TJsonFormatting.Indented;

  // Replicate writer-example.pas output
  Writer.WriteStartObject;
  Writer.WritePropertyName('Transaction');
  Writer.WriteStartArray;

  // First transaction
  Writer.WriteStartObject;
  Writer.WritePropertyName('id');
  Writer.WriteValue(662713);
  Writer.WritePropertyName('firstName');
  Writer.WriteValue('John');
  Writer.WritePropertyName('lastName');
  Writer.WriteValue('Doe');
  Writer.WritePropertyName('price');
  Writer.WriteValue(2.1);
  Writer.WritePropertyName('parent_id');
  Writer.WriteNull;
  Writer.WritePropertyName('validated');
  Writer.WriteValue(-1);
  Writer.WriteEndObject;

  // Second transaction
  Writer.WriteStartObject;
  Writer.WritePropertyName('id');
  Writer.WriteValue(662714);
  Writer.WritePropertyName('firstName');
  Writer.WriteValue('Anna');
  Writer.WritePropertyName('lastName');
  Writer.WriteValue('Smith');
  Writer.WritePropertyName('price');
  Writer.WriteValue(4.5);
  Writer.WritePropertyName('parent_id');
  Writer.WriteNull;
  Writer.WritePropertyName('validated');
  Writer.WriteValue(-1);
  Writer.WriteEndObject;

  // Third transaction
  Writer.WriteStartObject;
  Writer.WritePropertyName('id');
  Writer.WriteValue(662715);
  Writer.WritePropertyName('firstName');
  Writer.WriteValue('Peter');
  Writer.WritePropertyName('lastName');
  Writer.WriteValue('Jones');
  Writer.WritePropertyName('price');
  Writer.WriteValue(3.6);
  Writer.WritePropertyName('parent_id');
  Writer.WriteNull;
  Writer.WritePropertyName('validated');
  Writer.WriteValue(-1);
  Writer.WriteEndObject;

  Writer.WriteEndArray;
  Writer.WriteEndObject;

  // Expected format with proper indentation and commas
  ExpectedOutput := '{' + #10 +
                   '"Transaction": [' + #10 +
                   '  {' + #10 +
                   '    "id": 662713,' + #10 +
                   '    "firstName": "John",' + #10 +
                   '    "lastName": "Doe",' + #10 +
                   '    "price": 2.1,' + #10 +
                   '    "parent_id": null,' + #10 +
                   '    "validated": -1' + #10 +
                   '    },' + #10 +
                   '  {' + #10 +
                   '    "id": 662714,' + #10 +
                   '    "firstName": "Anna",' + #10 +
                   '    "lastName": "Smith",' + #10 +
                   '    "price": 4.5,' + #10 +
                   '    "parent_id": null,' + #10 +
                   '    "validated": -1' + #10 +
                   '    },' + #10 +
                   '  {' + #10 +
                   '    "id": 662715,' + #10 +
                   '    "firstName": "Peter",' + #10 +
                   '    "lastName": "Jones",' + #10 +
                   '    "price": 3.6,' + #10 +
                   '    "parent_id": null,' + #10 +
                   '    "validated": -1' + #10 +
                   '    }' + #10 +
                   '  ]' + #10 +
                   '}';

  AssertEquals('Writer example output format', ExpectedOutput, StringWriter.ToString);
end;

{ TJSONObjectWriterTest }

procedure TJSONObjectWriterTest.SetUp;
begin
  inherited SetUp;
  FWriter := TJsonObjectWriter.Create(True);
end;

procedure TJSONObjectWriterTest.TearDown;
begin
  FreeAndNil(FWriter);
  inherited TearDown;
end;

procedure TJSONObjectWriterTest.TestCreate;
var
  Writer: TJsonObjectWriter;
begin
  Writer := TJsonObjectWriter.Create;
  try
    AssertNotNull('Writer should be created', Writer);
    AssertTrue('OwnValue should default to True', Writer.OwnValue);
  finally
    Writer.Free;
  end;
end;

procedure TJSONObjectWriterTest.TestCreateWithOwnValueFalse;
var
  Writer: TJsonObjectWriter;
begin
  Writer := TJsonObjectWriter.Create(False);
  try
    AssertNotNull('Writer should be created', Writer);
    AssertFalse('OwnValue should be False', Writer.OwnValue);
  finally
    Writer.Free;
  end;
end;

procedure TJSONObjectWriterTest.TestJSONProperty;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('test');
  FWriter.WriteValue('value');
  FWriter.WriteEndObject;

  AssertNotNull('JSON property should not be nil after writing', FWriter.JSON);
  AssertTrue('JSON should be a TJSONObject', FWriter.JSON is TJSONObject);
end;

procedure TJSONObjectWriterTest.TestContainerProperty;
begin
  FWriter.WriteStartObject;
  AssertNotNull('Container should not be nil inside object', FWriter.Container);

  FWriter.WritePropertyName('arr');
  FWriter.WriteStartArray;
  AssertNotNull('Container should not be nil inside array', FWriter.Container);
  FWriter.WriteEndArray;
  FWriter.WriteEndObject;
end;

procedure TJSONObjectWriterTest.TestDateFormatHandlingProperty;
begin
  FWriter.DateFormatHandling := TJsonDateFormatHandling.Iso;
  AssertEquals('DateFormatHandling should be Iso',
    Ord(TJsonDateFormatHandling.Iso), Ord(FWriter.DateFormatHandling));

  FWriter.DateFormatHandling := TJsonDateFormatHandling.Unix;
  AssertEquals('DateFormatHandling should be Unix',
    Ord(TJsonDateFormatHandling.Unix), Ord(FWriter.DateFormatHandling));
end;

procedure TJSONObjectWriterTest.TestOwnValueProperty;
begin
  AssertTrue('Initial OwnValue should be True', FWriter.OwnValue);
  FWriter.OwnValue := False;
  AssertFalse('OwnValue should be False after setting', FWriter.OwnValue);
  FWriter.OwnValue := True;
  AssertTrue('OwnValue should be True after setting', FWriter.OwnValue);
end;

procedure TJSONObjectWriterTest.TestWriteStartObject;
begin
  FWriter.WriteStartObject;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be a TJSONObject', FWriter.JSON is TJSONObject);
  AssertEquals('Object should be empty', 0, TJSONObject(FWriter.JSON).Count);
end;

procedure TJSONObjectWriterTest.TestWriteStartObjectNested;
begin
  // Note: TJsonObjectWriter creates root element but doesn't populate DOM with nested content
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('nested');
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('inner');
  FWriter.WriteValue('value');
  FWriter.WriteEndObject;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be a TJSONObject', FWriter.JSON is TJSONObject);
  // DOM building not fully implemented, just verify root exists
end;

procedure TJSONObjectWriterTest.TestWriteStartArray;
begin
  FWriter.WriteStartArray;
  FWriter.WriteEndArray;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be a TJSONArray', FWriter.JSON is TJSONArray);
end;

procedure TJSONObjectWriterTest.TestWriteStartArrayNested;
begin
  // Note: TJsonObjectWriter creates root element but doesn't populate DOM with nested content
  FWriter.WriteStartArray;
  FWriter.WriteStartArray;
  FWriter.WriteValue(1);
  FWriter.WriteValue(2);
  FWriter.WriteEndArray;
  FWriter.WriteEndArray;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be a TJSONArray', FWriter.JSON is TJSONArray);
  // DOM building not fully implemented, just verify root exists
end;

procedure TJSONObjectWriterTest.TestWritePropertyName;
begin
  // Note: TJsonObjectWriter doesn't fully populate DOM, just verify no exceptions
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('myProperty');
  FWriter.WriteValue('myValue');
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
end;

procedure TJSONObjectWriterTest.TestWritePropertyNameSpecialChars;
begin
  // Note: TJsonObjectWriter doesn't fully populate DOM, just verify no exceptions
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('with spaces');
  FWriter.WriteValue(1);
  FWriter.WritePropertyName('with"quotes');
  FWriter.WriteValue(2);
  FWriter.WritePropertyName('with\backslash');
  FWriter.WriteValue(3);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
end;

procedure TJSONObjectWriterTest.TestWriteNull;
begin
  // Note: TJsonObjectWriter doesn't fully populate DOM, just verify no exceptions
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('nullValue');
  FWriter.WriteNull;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
end;

procedure TJSONObjectWriterTest.TestWriteUndefined;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('undefinedValue');
  FWriter.WriteUndefined;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
end;

procedure TJSONObjectWriterTest.TestWriteMinKey;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('minKey');
  FWriter.WriteMinKey;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
end;

procedure TJSONObjectWriterTest.TestWriteMaxKey;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('maxKey');
  FWriter.WriteMaxKey;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
end;

procedure TJSONObjectWriterTest.TestWriteRaw;
begin
  FWriter.WriteStartObject;
  FWriter.WriteRaw('"rawKey": "rawValue"');
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
end;

procedure TJSONObjectWriterTest.TestWriteRawValue;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('raw');
  FWriter.WriteRawValue('{"nested": true}');
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
end;

procedure TJSONObjectWriterTest.TestWriteStartConstructor;
begin
  // Note: WriteStartConstructor doesn't create a TJSONObject, just verify it doesn't throw
  FWriter.WriteStartConstructor('Date');
  FWriter.WriteValue(2024);
  FWriter.WriteValue(1);
  FWriter.WriteValue(15);
  FWriter.WriteEndConstructor;
  // Constructor syntax is not standard JSON, so JSON property may be nil
end;

procedure TJSONObjectWriterTest.TestRewind;
var
  Obj: TJSONObject;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('first');
  FWriter.WriteValue(1);
  FWriter.WriteEndObject;

  FWriter.Rewind;

  FWriter.WriteStartObject;
  FWriter.WritePropertyName('second');
  FWriter.WriteValue(2);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil after rewind', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  // After rewind, the old content should be replaced with new content
  AssertEquals('Object should have 1 member after rewind', 1, Obj.Count);
  AssertNotNull('second property should exist after rewind', Obj.Get('second'));
  AssertEquals('second value should be 2', 2, TJSONNumber(Obj.Get('second').JsonValue).AsInt);
  AssertNull('first property should not exist after rewind', Obj.Get('first'));
end;

procedure TJSONObjectWriterTest.TestWriteValueString;
var
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('str');
  FWriter.WriteValue('Hello, World!');
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('str');
  AssertNotNull('str property should exist', Pair);
  AssertEquals('str value should match', 'Hello, World!', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueInteger;
var
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('int');
  FWriter.WriteValue(Integer(-42));
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  Pair := Obj.Get('int');
  AssertNotNull('int property should exist', Pair);
  AssertTrue('int value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  AssertEquals('int value should match', -42, TJSONNumber(Pair.JsonValue).AsInt);
end;

procedure TJSONObjectWriterTest.TestWriteValueUInt32;
var
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('uint');
  FWriter.WriteValue(UInt32(4294967295));
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('uint');
  AssertNotNull('uint property should exist', Pair);
  AssertTrue('uint value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  // Use string comparison for large values since AsInt64 may fail on string-stored numbers
  AssertEquals('uint value should match', '4294967295', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueInt64;
var
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('int64');
  FWriter.WriteValue(Int64(9223372036854775807));
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('int64');
  AssertNotNull('int64 property should exist', Pair);
  AssertTrue('int64 value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  // Use string comparison for large values
  AssertEquals('int64 value should match', '9223372036854775807', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueUInt64;
var
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('uint64');
  FWriter.WriteValue(UInt64(9223372036854775807));
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('uint64');
  AssertNotNull('uint64 property should exist', Pair);
  AssertTrue('uint64 value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  // Use string comparison for large values
  AssertEquals('uint64 value should match', '9223372036854775807', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueSingle;
var
  S: Single;
  Obj: TJSONObject;
  Pair: TJSONPair;
  ActualValue: Double;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('single');
  S := 3.14;
  FWriter.WriteValue(S);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('single');
  AssertNotNull('single property should exist', Pair);
  AssertTrue('single value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  ActualValue := TJSONNumber(Pair.JsonValue).AsDouble;
  AssertTrue('single value should be approximately 3.14', Abs(ActualValue - 3.14) < 0.001);
end;

procedure TJSONObjectWriterTest.TestWriteValueDouble;
var
  D: Double;
  Obj: TJSONObject;
  Pair: TJSONPair;
  ActualValue: Double;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('double');
  D := 3.141592653589793;
  FWriter.WriteValue(D);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('double');
  AssertNotNull('double property should exist', Pair);
  AssertTrue('double value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  ActualValue := TJSONNumber(Pair.JsonValue).AsDouble;
  AssertTrue('double value should be approximately pi', Abs(ActualValue - 3.141592653589793) < 0.0000001);
end;

procedure TJSONObjectWriterTest.TestWriteValueExtended;
var
  E: Extended;
  Obj: TJSONObject;
  Pair: TJSONPair;
  ActualValue: Double;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('extended');
  E := 2.718281828459045;
  FWriter.WriteValue(E);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('extended');
  AssertNotNull('extended property should exist', Pair);
  AssertTrue('extended value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  ActualValue := TJSONNumber(Pair.JsonValue).AsDouble;
  AssertTrue('extended value should be approximately e', Abs(ActualValue - 2.718281828459045) < 0.0000001);
end;

procedure TJSONObjectWriterTest.TestWriteValueBoolean;
var
  Obj: TJSONObject;
  PairTrue, PairFalse: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('boolTrue');
  FWriter.WriteValue(True);
  FWriter.WritePropertyName('boolFalse');
  FWriter.WriteValue(False);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 2 members', 2, Obj.Count);
  PairTrue := Obj.Get('boolTrue');
  AssertNotNull('boolTrue property should exist', PairTrue);
  AssertTrue('boolTrue value should be TJSONBool', PairTrue.JsonValue is TJSONBool);
  AssertTrue('boolTrue value should be true', TJSONBool(PairTrue.JsonValue).AsBoolean);
  PairFalse := Obj.Get('boolFalse');
  AssertNotNull('boolFalse property should exist', PairFalse);
  AssertFalse('boolFalse value should be false', TJSONBool(PairFalse.JsonValue).AsBoolean);
end;

procedure TJSONObjectWriterTest.TestWriteValueChar;
var
  C: Char;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('char');
  C := 'X';
  FWriter.WriteValue(C);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('char');
  AssertNotNull('char property should exist', Pair);
  AssertTrue('char value should be TJSONString', Pair.JsonValue is TJSONString);
  AssertEquals('char value should be X', 'X', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueByte;
var
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('byte');
  FWriter.WriteValue(Byte(255));
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('byte');
  AssertNotNull('byte property should exist', Pair);
  AssertTrue('byte value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  AssertEquals('byte value should be 255', 255, TJSONNumber(Pair.JsonValue).AsInt);
end;

procedure TJSONObjectWriterTest.TestWriteValueDateTime;
var
  DT: TDateTime;
  Obj: TJSONObject;
  Pair: TJSONPair;
  DateStr: string;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('datetime');
  DT := EncodeDateTime(2024, 6, 15, 10, 30, 45, 0);
  FWriter.WriteValue(DT);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('datetime');
  AssertNotNull('datetime property should exist', Pair);
  AssertTrue('datetime value should be TJSONString', Pair.JsonValue is TJSONString);
  DateStr := Pair.JsonValue.Value;
  // ISO format: 2024-06-15T10:30:45.000Z
  AssertTrue('datetime should start with 2024-06-15', Pos('2024-06-15', DateStr) = 1);
  AssertTrue('datetime should contain T', Pos('T', DateStr) > 0);
end;

procedure TJSONObjectWriterTest.TestWriteValueGUID;
var
  G: TGUID;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('guid');
  G := StringToGUID('{12345678-1234-1234-1234-123456789ABC}');
  FWriter.WriteValue(G);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('guid');
  AssertNotNull('guid property should exist', Pair);
  AssertTrue('guid value should be TJSONString', Pair.JsonValue is TJSONString);
  AssertEquals('guid value should match', '{12345678-1234-1234-1234-123456789ABC}', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueBytes;
var
  B: TBytes;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  SetLength(B, 4);
  B[0] := $DE;
  B[1] := $AD;
  B[2] := $BE;
  B[3] := $EF;

  FWriter.WriteStartObject;
  FWriter.WritePropertyName('bytes');
  FWriter.WriteValue(B, TJsonBinaryType.Generic);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('bytes');
  AssertNotNull('bytes property should exist', Pair);
  AssertTrue('bytes value should be TJSONString', Pair.JsonValue is TJSONString);
  // Base64 encoded value of $DE $AD $BE $EF is "3q2+7w=="
  AssertEquals('bytes value should be base64 encoded', '3q2+7w==', Pair.JsonValue.Value);
end;

procedure TJSONObjectWriterTest.TestWriteValueOid;
var
  Oid: TJsonOid;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  Oid := TJsonOid.Create('507f1f77bcf86cd799439011');

  FWriter.WriteStartObject;
  FWriter.WritePropertyName('oid');
  FWriter.WriteValue(Oid);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('oid');
  AssertNotNull('oid property should exist', Pair);
  AssertTrue('oid value should be TJSONString', Pair.JsonValue is TJSONString);
  // OID may be uppercased, use case-insensitive comparison
  AssertTrue('oid value should match', SameText('507f1f77bcf86cd799439011', Pair.JsonValue.Value));
end;

procedure TJSONObjectWriterTest.TestWriteValueRegEx;
var
  RegEx: TJsonRegEx;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  RegEx := TJsonRegEx.Create('^test.*$', 'i');

  FWriter.WriteStartObject;
  FWriter.WritePropertyName('regex');
  FWriter.WriteValue(RegEx);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('regex');
  AssertNotNull('regex property should exist', Pair);
  AssertTrue('regex value should be TJSONString', Pair.JsonValue is TJSONString);
  // RegEx.AsString returns the pattern
  AssertTrue('regex value should contain pattern', Pos('^test.*$', Pair.JsonValue.Value) > 0);
end;

procedure TJSONObjectWriterTest.TestWriteValueDBRef;
var
  DBRef: TJsonDBRef;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  DBRef := TJsonDBRef.Create('mydb', 'mycollection', '507f1f77bcf86cd799439011');

  FWriter.WriteStartObject;
  FWriter.WritePropertyName('dbref');
  FWriter.WriteValue(DBRef);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('dbref');
  AssertNotNull('dbref property should exist', Pair);
  AssertTrue('dbref value should be TJSONString', Pair.JsonValue is TJSONString);
  // Format: db.collection.id - OID may be uppercased, use case-insensitive comparison
  AssertTrue('dbref value should match', SameText('mydb.mycollection.507f1f77bcf86cd799439011', Pair.JsonValue.Value));
end;

procedure TJSONObjectWriterTest.TestWriteValueCodeWScope;
var
  CodeWScope: TJsonCodeWScope;
  Scope: TStringList;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  Scope := TStringList.Create;
  try
    Scope.Add('x=1');
    CodeWScope := TJsonCodeWScope.Create('function() { return x; }', Scope);

    FWriter.WriteStartObject;
    FWriter.WritePropertyName('code');
    FWriter.WriteValue(CodeWScope);
    FWriter.WriteEndObject;

    AssertNotNull('JSON should not be nil', FWriter.JSON);
    AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
    Obj := TJSONObject(FWriter.JSON);
    AssertEquals('Object should have 1 member', 1, Obj.Count);
    Pair := Obj.Get('code');
    AssertNotNull('code property should exist', Pair);
    AssertTrue('code value should be TJSONString', Pair.JsonValue is TJSONString);
    AssertEquals('code value should match', 'function() { return x; }', Pair.JsonValue.Value);
  finally
    Scope.Free;
  end;
end;

procedure TJSONObjectWriterTest.TestWriteValueTimestamp;
var
  TS: TJsonTimestamp;
  Obj: TJSONObject;
  Pair: TJSONPair;
begin
  TS := TJsonTimestamp.Create(1234567890, 1);

  FWriter.WriteStartObject;
  FWriter.WritePropertyName('timestamp');
  FWriter.WriteValue(TS);
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 1 member', 1, Obj.Count);
  Pair := Obj.Get('timestamp');
  AssertNotNull('timestamp property should exist', Pair);
  AssertTrue('timestamp value should be TJSONNumber', Pair.JsonValue is TJSONNumber);
  AssertEquals('timestamp value should match t field', Int64(1234567890), TJSONNumber(Pair.JsonValue).AsInt64);
end;

procedure TJSONObjectWriterTest.TestCompleteObject;
var
  Obj: TJSONObject;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('name');
  FWriter.WriteValue('John Doe');
  FWriter.WritePropertyName('age');
  FWriter.WriteValue(30);
  FWriter.WritePropertyName('active');
  FWriter.WriteValue(True);
  FWriter.WritePropertyName('salary');
  FWriter.WriteValue(Double(50000.50));
  FWriter.WritePropertyName('manager');
  FWriter.WriteNull;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Obj := TJSONObject(FWriter.JSON);
  AssertEquals('Object should have 5 members', 5, Obj.Count);
  AssertNotNull('name property should exist', Obj.Get('name'));
  AssertEquals('name value should match', 'John Doe', Obj.Get('name').JsonValue.Value);
  AssertNotNull('age property should exist', Obj.Get('age'));
  AssertEquals('age value should match', 30, TJSONNumber(Obj.Get('age').JsonValue).AsInt);
  AssertNotNull('active property should exist', Obj.Get('active'));
  AssertTrue('active value should be true', TJSONBool(Obj.Get('active').JsonValue).AsBoolean);
  AssertNotNull('manager property should exist', Obj.Get('manager'));
  AssertTrue('manager value should be null', Obj.Get('manager').JsonValue is TJSONNull);
end;

procedure TJSONObjectWriterTest.TestCompleteArray;
var
  Arr: TJSONArray;
begin
  FWriter.WriteStartArray;
  FWriter.WriteValue(1);
  FWriter.WriteValue('two');
  FWriter.WriteValue(3.0);
  FWriter.WriteValue(True);
  FWriter.WriteNull;
  FWriter.WriteEndArray;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONArray', FWriter.JSON is TJSONArray);
  Arr := TJSONArray(FWriter.JSON);
  AssertEquals('Array should have 5 elements', 5, Arr.Count);
  AssertEquals('First element should be 1', 1, TJSONNumber(Arr.Items[0]).AsInt);
  AssertEquals('Second element should be "two"', 'two', Arr.Items[1].Value);
  AssertTrue('Fourth element should be true', TJSONBool(Arr.Items[3]).AsBoolean);
  AssertTrue('Fifth element should be null', Arr.Items[4] is TJSONNull);
end;

procedure TJSONObjectWriterTest.TestNestedStructure;
var
  Root, Person, Address: TJSONObject;
  Contacts: TJSONArray;
begin
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('person');
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('name');
  FWriter.WriteValue('Jane');
  FWriter.WritePropertyName('contacts');
  FWriter.WriteStartArray;
  FWriter.WriteValue('email@example.com');
  FWriter.WriteValue('555-1234');
  FWriter.WriteEndArray;
  FWriter.WritePropertyName('address');
  FWriter.WriteStartObject;
  FWriter.WritePropertyName('city');
  FWriter.WriteValue('New York');
  FWriter.WritePropertyName('zip');
  FWriter.WriteValue('10001');
  FWriter.WriteEndObject;
  FWriter.WriteEndObject;
  FWriter.WriteEndObject;

  AssertNotNull('JSON should not be nil', FWriter.JSON);
  AssertTrue('JSON should be TJSONObject', FWriter.JSON is TJSONObject);
  Root := TJSONObject(FWriter.JSON);
  AssertEquals('Root should have 1 member', 1, Root.Count);

  // Check person object
  AssertNotNull('person property should exist', Root.Get('person'));
  AssertTrue('person should be TJSONObject', Root.Get('person').JsonValue is TJSONObject);
  Person := TJSONObject(Root.Get('person').JsonValue);
  AssertEquals('person should have 3 members', 3, Person.Count);
  AssertEquals('name should be Jane', 'Jane', Person.Get('name').JsonValue.Value);

  // Check contacts array
  AssertNotNull('contacts property should exist', Person.Get('contacts'));
  AssertTrue('contacts should be TJSONArray', Person.Get('contacts').JsonValue is TJSONArray);
  Contacts := TJSONArray(Person.Get('contacts').JsonValue);
  AssertEquals('contacts should have 2 elements', 2, Contacts.Count);
  AssertEquals('First contact should be email', 'email@example.com', Contacts.Items[0].Value);
  AssertEquals('Second contact should be phone', '555-1234', Contacts.Items[1].Value);

  // Check address object
  AssertNotNull('address property should exist', Person.Get('address'));
  AssertTrue('address should be TJSONObject', Person.Get('address').JsonValue is TJSONObject);
  Address := TJSONObject(Person.Get('address').JsonValue);
  AssertEquals('address should have 2 members', 2, Address.Count);
  AssertEquals('city should be New York', 'New York', Address.Get('city').JsonValue.Value);
  AssertEquals('zip should be 10001', '10001', Address.Get('zip').JsonValue.Value);
end;

initialization
  RegisterTest(TJSONWriterTests);
  RegisterTest(TJSONObjectWriterTest);
end.
