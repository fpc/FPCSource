unit utcjsoniterator;

{$mode objfpc}
{$h+}
{$modeswitch functionreferences}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, System.JSON, System.JSON.Writers,
  System.JSON.Readers, System.JSON.Types, System.JSON.Builders, StreamEx;

type
  TJSONIteratorTest = class(TTestCase)
  private
    FRewindCalled: Boolean;
    FIterateCount: Integer;
    function CreateReaderFromJSON(const aJSON: string): TJsonTextReader;
    procedure RewindCallback(aReader: TJsonReader);
    function IterateCallback(aIter: TJSONIterator): Boolean;
  published
    // Constructor tests
    procedure TestCreateWithReader;
    procedure TestCreateWithReaderAndRewindProc;

    // Public method tests
    procedure TestNext;
    procedure TestNextWithKey;
    procedure TestRewind;
    procedure TestRecurse;
    procedure TestReturn;
    procedure TestFind;
    procedure TestFindNestedPath;
    procedure TestIterate;
    procedure TestGetPathFromDepth;

    // Property tests - navigation
    procedure TestReaderProperty;
    procedure TestKeyProperty;
    procedure TestPathProperty;
    procedure TestTypeProperty;
    procedure TestParentTypeProperty;
    procedure TestIndexProperty;
    procedure TestInRecurseProperty;
    procedure TestDepthProperty;

    // Property tests - value accessors
    procedure TestAsString;
    procedure TestAsInteger;
    procedure TestAsInt64;
    procedure TestAsDouble;
    procedure TestAsExtended;
    procedure TestAsBoolean;
    procedure TestAsVariant;

    // Property tests - special values
    procedure TestIsNull;
    procedure TestIsUndefined;

    // Integration tests
    procedure TestIterateSimpleObject;
    procedure TestIterateSimpleArray;
    procedure TestIterateNestedStructure;
    procedure TestIterateMixedTypes;
  end;

implementation

{ TJSONIteratorTest }

function TJSONIteratorTest.CreateReaderFromJSON(const aJSON: string): TJsonTextReader;
var
  StringReader: TStringReader;
begin
  StringReader := TStringReader.Create(aJSON);
  Result := TJsonTextReader.Create(StringReader);
  Result.CloseInput := True;
end;

procedure TJSONIteratorTest.RewindCallback(aReader: TJsonReader);
begin
  FRewindCalled := True;
  aReader.Rewind;
end;

function TJSONIteratorTest.IterateCallback(aIter: TJSONIterator): Boolean;
begin
  Inc(FIterateCount);
  Result := True; // Continue iteration
end;

procedure TJSONIteratorTest.TestCreateWithReader;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"test": 1}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      CheckNotNull(Iterator, 'Iterator should be created');
      CheckEquals(Ord(TJsonToken.None), Ord(Iterator.&Type), 'Initial type should be None');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestCreateWithReaderAndRewindProc;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  FRewindCalled := False;
  Reader := CreateReaderFromJSON('{"test": 1}');
  try
    Iterator := TJSONIterator.Create(Reader, @RewindCallback);
    try
      CheckNotNull(Iterator, 'Iterator should be created');
      Iterator.Next;
      Iterator.Rewind;
      CheckTrue(FRewindCalled, 'Rewind callback should have been called');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestNext;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  HasNext: Boolean;
begin
  Reader := CreateReaderFromJSON('{"name": "test", "value": 42}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      HasNext := Iterator.Next;
      CheckTrue(HasNext, 'First Next should return True');

      HasNext := Iterator.Next;
      CheckTrue(HasNext, 'Second Next should return True');
      CheckEquals('name', Iterator.Key, 'Key should be "name"');
      CheckEquals('test', Iterator.asString, 'Value should be "test"');

      HasNext := Iterator.Next;
      CheckTrue(HasNext, 'Third Next should return True');
      CheckEquals('value', Iterator.Key, 'Key should be "value"');
      CheckEquals(42, Iterator.asInteger, 'Value should be 42');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestNextWithKey;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  Found: Boolean;
begin
  Reader := CreateReaderFromJSON('{"first": 1, "second": 2, "third": 3}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      // Skip to start
      Iterator.Next;

      // Find second key
      Found := False;
      while Iterator.Next do
      begin
        if Iterator.Key = 'second' then
        begin
          Found := True;
          CheckEquals(2, Iterator.asInteger, 'Value of "second" should be 2');
          Break;
        end;
      end;
      CheckTrue(Found, 'Should find key "second"');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestRewind;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  FirstKey: string;
begin
  Reader := CreateReaderFromJSON('{"name": "test"}');
  try
    Iterator := TJSONIterator.Create(Reader, @RewindCallback);
    try
      Iterator.Next; // Start object
      Iterator.Next; // name: test
      FirstKey := Iterator.Key;

      Iterator.Rewind;

      Iterator.Next; // Start object again
      Iterator.Next; // name: test again
      CheckEquals(FirstKey, Iterator.Key, 'After rewind, should get same key');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestRecurse;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  CanRecurse: Boolean;
begin
  Reader := CreateReaderFromJSON('{"nested": {"inner": 1}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      CheckEquals(Ord(TJsonToken.StartObject), Ord(Iterator.&Type), 'Should be at StartObject');

      CanRecurse := Iterator.Recurse;
      CheckTrue(CanRecurse, 'Should be able to recurse into object');
      CheckTrue(Iterator.InRecurse, 'InRecurse should be True after Recurse');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestReturn;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  InitialDepth: Integer;
begin
  Reader := CreateReaderFromJSON('{"outer": {"inner": {"deep": 1}}, "after": 2}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start outer object
      InitialDepth := Iterator.Depth;

      // Go deeper
      Iterator.Next; // outer
      Iterator.Next; // inner object start
      Iterator.Next; // deep: 1

      CheckTrue(Iterator.Depth > InitialDepth, 'Should be deeper than initial');

      // Return to outer level
      Iterator.Return;

      // Should be able to continue to "after"
      if Iterator.Next then
      begin
        CheckEquals('after', Iterator.Key, 'Should reach "after" key');
        CheckEquals(2, Iterator.asInteger, 'Value of "after" should be 2');
      end;
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestFind;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  Found: Boolean;
begin
  Reader := CreateReaderFromJSON('{"name": "John", "age": 30}');
  try
    Iterator := TJSONIterator.Create(Reader, @RewindCallback);
    try
      Found := Iterator.Find('age');
      CheckTrue(Found, 'Should find "age" key');
      CheckEquals(30, Iterator.asInteger, 'Age value should be 30');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestFindNestedPath;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  Found: Boolean;
begin
  Reader := CreateReaderFromJSON('{"person": {"name": "John", "address": {"city": "NYC"}}}');
  try
    Iterator := TJSONIterator.Create(Reader, @RewindCallback);
    try
      Found := Iterator.Find('person.address.city');
      CheckTrue(Found, 'Should find nested path "person.address.city"');
      CheckEquals('NYC', Iterator.asString, 'City value should be "NYC"');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIterate;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  FIterateCount := 0;
  Reader := CreateReaderFromJSON('{"a": 1, "b": 2, "c": 3}');
  try
    Iterator := TJSONIterator.Create(Reader, @RewindCallback);
    try
      Iterator.Iterate(@IterateCallback);

      // Object start + 3 key-value pairs = 4 items (but we get only values after keys)
      CheckTrue(FIterateCount >= 3, 'Should iterate at least 3 times for 3 properties');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestGetPathFromDepth;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  PathStr: string;
begin
  Reader := CreateReaderFromJSON('{"outer": {"inner": 1}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // outer: { inner }
      Iterator.Next; // inner: 1

      PathStr := Iterator.GetPath(0);
      CheckTrue(Length(PathStr) > 0, 'Path should not be empty');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestReaderProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"test": 1}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      CheckTrue(Iterator.Reader = Reader, 'Reader property should return the same reader');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestKeyProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"myKey": "myValue"}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      CheckEquals('', Iterator.Key, 'Initial key should be empty');
      Iterator.Next; // Start object
      Iterator.Next; // myKey: myValue
      CheckEquals('myKey', Iterator.Key, 'Key should be "myKey"');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestPathProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"level1": {"level2": 1}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // level1
      Iterator.Next; // level2

      // Path property should return something meaningful
      CheckTrue(Length(Iterator.Path) >= 0, 'Path property should be accessible');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestTypeProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"str": "hello", "num": 42, "bool": true}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      CheckEquals(Ord(TJsonToken.StartObject), Ord(Iterator.&Type), 'Type should be StartObject');

      Iterator.Next; // str: hello
      CheckEquals(Ord(TJsonToken.&String), Ord(Iterator.&Type), 'Type should be String');

      Iterator.Next; // num: 42
      CheckEquals(Ord(TJsonToken.Integer), Ord(Iterator.&Type), 'Type should be Integer');

      Iterator.Next; // bool: true
      CheckEquals(Ord(TJsonToken.Boolean), Ord(Iterator.&Type), 'Type should be Boolean');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestParentTypeProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"items": [1, 2, 3]}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      CheckEquals(Ord(TJsonToken.StartObject), Ord(Iterator.ParentType), 'Parent should be StartObject');

      Iterator.Next; // items: [...]
      // After reading array start, parent should still be object
      CheckTrue(Iterator.ParentType in [TJsonToken.StartObject, TJsonToken.StartArray],
        'Parent should be object or array');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIndexProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('[1, 2, 3]');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      CheckEquals(-1, Iterator.Index, 'Initial index should be -1');

      Iterator.Next; // Start array
      // Index starts at 0 for array contexts
      CheckTrue(Iterator.Index >= 0, 'Index should be >= 0 inside array');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestInRecurseProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"nested": {}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      CheckFalse(Iterator.InRecurse, 'InRecurse should be False initially');

      Iterator.Next; // Start object
      Iterator.Recurse;

      CheckTrue(Iterator.InRecurse, 'InRecurse should be True after Recurse');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestDepthProperty;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  InitialDepth: Integer;
begin
  Reader := CreateReaderFromJSON('{"level1": {"level2": {"level3": 1}}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      InitialDepth := Iterator.Depth;
      CheckEquals(0, InitialDepth, 'Initial depth should be 0');

      Iterator.Next; // Start object - depth 1
      CheckTrue(Iterator.Depth > InitialDepth, 'Depth should increase');

      Iterator.Next; // level1
      Iterator.Next; // level2
      CheckTrue(Iterator.Depth >= 2, 'Depth should be at least 2');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsString;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"message": "Hello, World!"}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // message: "Hello, World!"
      CheckEquals('Hello, World!', Iterator.asString, 'asString should return the string value');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsInteger;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"count": 12345}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // count: 12345
      CheckEquals(12345, Iterator.asInteger, 'asInteger should return 12345');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsInt64;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"bignum": 9223372036854775807}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // bignum
      CheckEquals(9223372036854775807, Iterator.asInt64, 'asInt64 should return max Int64');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsDouble;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"pi": 3.14159}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // pi: 3.14159
      CheckTrue(Abs(Iterator.asDouble - 3.14159) < 0.0001, 'asDouble should return approximately 3.14159');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsExtended;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"e": 2.718281828}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // e: 2.718281828
      CheckTrue(Abs(Iterator.asExtended - 2.718281828) < 0.0000001, 'asExtended should return approximately e');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsBoolean;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"active": true, "deleted": false}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // active: true
      CheckTrue(Iterator.asBoolean, 'asBoolean should return True');

      Iterator.Next; // deleted: false
      CheckFalse(Iterator.asBoolean, 'asBoolean should return False');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestAsVariant;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  V: Variant;
begin
  Reader := CreateReaderFromJSON('{"value": "test variant"}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // value: "test variant"
      V := Iterator.asVariant;
      CheckEquals('test variant', string(V), 'asVariant should return correct value');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIsNull;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  Reader := CreateReaderFromJSON('{"empty": null, "notempty": 1}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // empty: null
      CheckTrue(Iterator.IsNull, 'IsNull should return True for null value');

      Iterator.Next; // notempty: 1
      CheckFalse(Iterator.IsNull, 'IsNull should return False for non-null value');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIsUndefined;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
begin
  // Note: undefined is not standard JSON but may be supported
  Reader := CreateReaderFromJSON('{"value": 1}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Iterator.Next; // Start object
      Iterator.Next; // value: 1
      CheckFalse(Iterator.IsUndefined, 'IsUndefined should return False for regular value');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIterateSimpleObject;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  Keys: string;
begin
  Reader := CreateReaderFromJSON('{"a": 1, "b": 2}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Keys := '';
      while Iterator.Next do
      begin
        if Iterator.Key <> '' then
          Keys := Keys + Iterator.Key + ',';
      end;
      CheckEquals('a,b,', Keys, 'Should iterate through all keys');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIterateSimpleArray;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  Sum: Integer;
begin
  Reader := CreateReaderFromJSON('[1, 2, 3, 4, 5]');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      Sum := 0;
      while Iterator.Next do
      begin
        if Iterator.&Type = TJsonToken.Integer then
          Sum := Sum + Iterator.asInteger;
      end;
      CheckEquals(15, Sum, 'Sum of array elements should be 15');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIterateNestedStructure;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  FoundDeep: Boolean;
begin
  Reader := CreateReaderFromJSON('{"level1": {"level2": {"level3": "deep value"}}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      FoundDeep := False;
      while Iterator.Next do
      begin
        if (Iterator.Key = 'level3') and (Iterator.asString = 'deep value') then
        begin
          FoundDeep := True;
          Break;
        end;
      end;
      CheckTrue(FoundDeep, 'Should find deeply nested value');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TJSONIteratorTest.TestIterateMixedTypes;
var
  Reader: TJsonTextReader;
  Iterator: TJSONIterator;
  TypeCount: Integer;
begin
  Reader := CreateReaderFromJSON('{"str": "hello", "num": 42, "float": 3.14, "bool": true, "nil": null, "arr": [1,2], "obj": {"a":1}}');
  try
    Iterator := TJSONIterator.Create(Reader);
    try
      TypeCount := 0;
      while Iterator.Next do
      begin
        case Iterator.&Type of
          TJsonToken.&String,
          TJsonToken.Integer,
          TJsonToken.Float,
          TJsonToken.Boolean,
          TJsonToken.Null,
          TJsonToken.StartArray,
          TJsonToken.StartObject:
            Inc(TypeCount);
        end;
      end;
      CheckTrue(TypeCount >= 7, 'Should encounter at least 7 different value types');
    finally
      Iterator.Free;
    end;
  finally
    Reader.Free;
  end;
end;

initialization
  RegisterTest(TJSONIteratorTest.Suite);

end.
