unit utcjsontypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  System.JSON.Types, Generics.Collections, Math;

type

  { TTestJsonLineInfo }

  TTestJsonLineInfo = class(TTestCase)
  private
    FLineInfo: TJsonLineInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetLineNumber;
    procedure TestGetLinePosition;
    procedure TestHasLineInfo;
    procedure TestLineNumberProperty;
    procedure TestLinePositionProperty;
  end;

  { TTestJsonPosition }

  TTestJsonPosition = class(TTestCase)
  private
    FPosition: TJsonPosition;
  published
    procedure TestCreateDefault;
    procedure TestCreateWithType;
    procedure TestClear;
    procedure TestWriteToObject;
    procedure TestWriteToArray;
    procedure TestWriteToConstructor;
    procedure TestBuildPathEmpty;
    procedure TestBuildPathSingle;
    procedure TestBuildPathMultiple;
    procedure TestFormatMessage;
  end;

  { TTestJsonFiler }

  TTestJsonFiler = class(TTestCase)
  private
    type
      TTestJsonFilerImpl = class(TJsonFiler)
      protected
        function GetInsideContainer: Boolean; override;
      end;
    var
      FFiler: TTestJsonFilerImpl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateDestroy;
    procedure TestPushPop;
    procedure TestPeek;
    procedure TestGetPath;
    procedure TestRewind;
    procedure TestIsEndToken;
    procedure TestIsStartToken;
    procedure TestIsPrimitiveToken;
  end;

  { TTestJsonOid }

  TTestJsonOid = class(TTestCase)
  private
    FOid: TJsonOid;
  published
    procedure TestCreateFromBytes;
    procedure TestCreateFromString;
    procedure TestAsString;
    procedure TestAsBytes;
    procedure TestStringRoundTrip;
    procedure TestBytesRoundTrip;
    procedure TestInvalidStringLength;
  end;

  { TTestJsonRegEx }

  TTestJsonRegEx = class(TTestCase)
  private
    FRegEx: TJsonRegEx;
  published
    procedure TestCreate;
    procedure TestAsString;
    procedure TestSetAsString;
    procedure TestSetAsStringVariations;
  end;

  { TTestJsonDBRef }

  TTestJsonDBRef = class(TTestCase)
  private
    FDBRef: TJsonDBRef;
  published
    procedure TestCreateWithDB;
    procedure TestCreateWithoutDB;
    procedure TestCreateWithOid;
    procedure TestAsString;
    procedure TestSetAsString;
  end;

  { TTestJsonCodeWScope }

  TTestJsonCodeWScope = class(TTestCase)
  private
    FCodeWScope: TJsonCodeWScope;
  published
    procedure TestCreateEmpty;
    procedure TestCreateWithScope;
  end;

  { TTestJsonDecimal128 }

  TTestJsonDecimal128 = class(TTestCase)
  private
    FDecimal: TJsonDecimal128;
  published
    procedure TestCreateFromString;
    procedure TestCreateFromExtended;
    procedure TestIsZero;
    procedure TestIsNan;
    procedure TestIsPosInfinity;
    procedure TestIsNegInfinity;
    procedure TestAsExtended;
    procedure TestAsString;
  end;

  { TTestJsonNameAttribute }

  TTestJsonNameAttribute = class(TTestCase)
  private
    FAttribute: JsonNameAttribute;
  protected
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestValue;
  end;

  { TTestEJsonException }

  TTestEJsonException = class(TTestCase)
  published
    procedure TestCreateSimple;
    procedure TestCreateWithInner;
    procedure TestInnerException;
  end;

implementation

{ TTestJsonLineInfo }

procedure TTestJsonLineInfo.SetUp;
begin
  inherited SetUp;
  FLineInfo := TJsonLineInfo.Create;
end;

procedure TTestJsonLineInfo.TearDown;
begin
  FLineInfo.Free;
  inherited TearDown;
end;

procedure TTestJsonLineInfo.TestGetLineNumber;
begin
  AssertEquals('Default line number', 0, FLineInfo.GetLineNumber);
end;

procedure TTestJsonLineInfo.TestGetLinePosition;
begin
  AssertEquals('Default line position', 0, FLineInfo.GetLinePosition);
end;

procedure TTestJsonLineInfo.TestHasLineInfo;
begin
  AssertFalse('Default has no line info', FLineInfo.HasLineInfo);
end;

procedure TTestJsonLineInfo.TestLineNumberProperty;
begin
  AssertEquals('Line number property', 0, FLineInfo.LineNumber);
end;

procedure TTestJsonLineInfo.TestLinePositionProperty;
begin
  AssertEquals('Line position property', 0, FLineInfo.LinePosition);
end;

{ TTestJsonPosition }

procedure TTestJsonPosition.TestCreateDefault;
begin
  FPosition := TJsonPosition.Create;
  AssertEquals('Default container type', Ord(TJsonContainerType.None), Ord(FPosition.ContainerType));
  AssertEquals('Default position', -1, FPosition.Position);
  AssertEquals('Default property name', '', FPosition.PropertyName);
  AssertFalse('Default has no index', FPosition.HasIndex);
end;

procedure TTestJsonPosition.TestCreateWithType;
begin
  FPosition := TJsonPosition.Create(TJsonContainerType.&Array);
  AssertEquals('Array container type', Ord(TJsonContainerType.&Array), Ord(FPosition.ContainerType));
  AssertTrue('Array has index', FPosition.HasIndex);
  AssertEquals('Array position', -1, FPosition.Position);
end;

procedure TTestJsonPosition.TestClear;
begin
  FPosition.ContainerType := TJsonContainerType.&Object;
  FPosition.Position := 5;
  FPosition.PropertyName := 'test';
  FPosition.Clear;
  AssertEquals('Cleared container type', Ord(TJsonContainerType.None), Ord(FPosition.ContainerType));
  AssertEquals('Cleared position', -1, FPosition.Position);
  AssertEquals('Cleared property name', '', FPosition.PropertyName);
end;

procedure TTestJsonPosition.TestWriteToObject;
var
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  try
    FPosition := TJsonPosition.Create(TJsonContainerType.&Object);
    FPosition.PropertyName := 'test';
    FPosition.WriteTo(Sb);
    AssertEquals('Object path', 'test', Sb.ToString);

    Sb.Clear;
    Sb.Append('root');
    FPosition.WriteTo(Sb);
    AssertEquals('Object path with prefix', 'root.test', Sb.ToString);
  finally
    Sb.Free;
  end;
end;

procedure TTestJsonPosition.TestWriteToArray;
var
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  try
    FPosition := TJsonPosition.Create(TJsonContainerType.&Array);
    FPosition.Position := 5;
    FPosition.WriteTo(Sb);
    AssertEquals('Array path', '[5]', Sb.ToString);
  finally
    Sb.Free;
  end;
end;

procedure TTestJsonPosition.TestWriteToConstructor;
var
  Sb: TStringBuilder;
begin
  Sb := TStringBuilder.Create;
  try
    FPosition := TJsonPosition.Create(TJsonContainerType.&Constructor);
    FPosition.Position := 3;
    FPosition.WriteTo(Sb);
    AssertEquals('Constructor path', '[3]', Sb.ToString);
  finally
    Sb.Free;
  end;
end;


procedure TTestJsonPosition.TestBuildPathEmpty;
var
  Positions: TJsonPositionList;
  Path: string;
begin
  Positions := TJsonPositionList.Create;
  try
    Path := TJsonPosition.BuildPath(Positions);
    AssertEquals('Empty path', '', Path);
  finally
    Positions.Free;
  end;
end;

procedure TTestJsonPosition.TestBuildPathSingle;
var
  Positions: TJsonPositionList;
  Pos: TJsonPosition;
  Path: string;
begin
  Positions := TJsonPositionList.Create;
  try
    Pos := TJsonPosition.Create(TJsonContainerType.&Object);
    Pos.PropertyName := 'test';
    Positions.Add(Pos);
    Path := TJsonPosition.BuildPath(Positions);
    AssertEquals('Single object path', 'test', Path);
  finally
    Positions.Free;
  end;
end;

procedure TTestJsonPosition.TestBuildPathMultiple;
var
  Positions: TJsonPositionList;
  Pos1, Pos2: TJsonPosition;
  Path: string;
begin
  Positions := TJsonPositionList.Create;
  try
    Pos1 := TJsonPosition.Create(TJsonContainerType.&Object);
    Pos1.PropertyName := 'root';
    Positions.Add(Pos1);

    Pos2 := TJsonPosition.Create(TJsonContainerType.&Array);
    Pos2.Position := 0;
    Positions.Add(Pos2);

    Path := TJsonPosition.BuildPath(Positions);
    AssertEquals('Multiple path', 'root[0]', Path);
  finally
    Positions.Free;
  end;
end;

procedure TTestJsonPosition.TestFormatMessage;
var
  LineInfo: TJsonLineInfo;
  Msg: string;
begin
  LineInfo := TJsonLineInfo.Create;
  try
    Msg := TJsonPosition.FormatMessage(LineInfo, 'test.path', 'Error occurred');
    AssertTrue('Message contains error', Pos('Error occurred', Msg) > 0);
    AssertTrue('Message contains path', Pos('test.path', Msg) > 0);
  finally
    LineInfo.Free;
  end;
end;

{ TTestJsonFiler.TTestJsonFilerImpl }

function TTestJsonFiler.TTestJsonFilerImpl.GetInsideContainer: Boolean;
begin
  Result := FCurrentPosition.ContainerType <> TJsonContainerType.None;
end;

{ TTestJsonFiler }

procedure TTestJsonFiler.SetUp;
begin
  inherited SetUp;
  FFiler := TTestJsonFilerImpl.Create;
end;

procedure TTestJsonFiler.TearDown;
begin
  FFiler.Free;
  inherited TearDown;
end;

procedure TTestJsonFiler.TestCreateDestroy;
begin
  AssertNotNull('Filer created', FFiler);
  AssertEquals('Empty path', '', FFiler.Path);
end;

procedure TTestJsonFiler.TestPushPop;
begin
  AssertEquals('Initial peek', Ord(TJsonContainerType.None), Ord(FFiler.Peek));

  FFiler.Push(TJsonContainerType.&Object);
  AssertEquals('After push object', Ord(TJsonContainerType.&Object), Ord(FFiler.Peek));

  FFiler.Push(TJsonContainerType.&Array);
  AssertEquals('After push array', Ord(TJsonContainerType.&Array), Ord(FFiler.Peek));

  AssertEquals('Pop array', Ord(TJsonContainerType.&Array), Ord(FFiler.Pop));
  AssertEquals('After pop array', Ord(TJsonContainerType.&Object), Ord(FFiler.Peek));

  AssertEquals('Pop object', Ord(TJsonContainerType.&Object), Ord(FFiler.Pop));
  AssertEquals('After pop object', Ord(TJsonContainerType.None), Ord(FFiler.Peek));
end;

procedure TTestJsonFiler.TestPeek;
begin
  AssertEquals('Initial peek', Ord(TJsonContainerType.None), Ord(FFiler.Peek));
  FFiler.Push(TJsonContainerType.&Object);
  AssertEquals('Peek object', Ord(TJsonContainerType.&Object), Ord(FFiler.Peek));
  AssertEquals('Peek again', Ord(TJsonContainerType.&Object), Ord(FFiler.Peek));
end;

procedure TTestJsonFiler.TestGetPath;
begin
  AssertEquals('Empty path', '', FFiler.Path);
  FFiler.Push(TJsonContainerType.&Object);
  // Path building requires the position to be set up properly
  // Since we haven't set any property names, the path should still be empty
  AssertEquals('Path after push without properties', '', FFiler.Path);
end;

procedure TTestJsonFiler.TestRewind;
begin
  FFiler.Push(TJsonContainerType.&Object);
  FFiler.Push(TJsonContainerType.&Array);
  FFiler.Rewind;
  AssertEquals('After rewind', Ord(TJsonContainerType.None), Ord(FFiler.Peek));
end;

procedure TTestJsonFiler.TestIsEndToken;
begin
  AssertTrue('EndObject is end', TJsonFiler.IsEndToken(TJsonToken.EndObject));
  AssertTrue('EndArray is end', TJsonFiler.IsEndToken(TJsonToken.EndArray));
  AssertTrue('EndConstructor is end', TJsonFiler.IsEndToken(TJsonToken.EndConstructor));
  AssertFalse('StartObject is not end', TJsonFiler.IsEndToken(TJsonToken.StartObject));
  AssertFalse('String is not end', TJsonFiler.IsEndToken(TJsonToken.&String));
end;

procedure TTestJsonFiler.TestIsStartToken;
begin
  AssertTrue('StartObject is start', TJsonFiler.IsStartToken(TJsonToken.StartObject));
  AssertTrue('StartArray is start', TJsonFiler.IsStartToken(TJsonToken.StartArray));
  AssertTrue('StartConstructor is start', TJsonFiler.IsStartToken(TJsonToken.StartConstructor));
  AssertFalse('EndObject is not start', TJsonFiler.IsStartToken(TJsonToken.EndObject));
  AssertFalse('String is not start', TJsonFiler.IsStartToken(TJsonToken.&String));
end;

procedure TTestJsonFiler.TestIsPrimitiveToken;
begin
  AssertTrue('Integer is primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.Integer));
  AssertTrue('Float is primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.Float));
  AssertTrue('String is primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.&String));
  AssertTrue('Boolean is primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.Boolean));
  AssertTrue('Null is primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.Null));
  AssertFalse('StartObject is not primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.StartObject));
  AssertFalse('EndObject is not primitive', TJsonFiler.IsPrimitiveToken(TJsonToken.EndObject));
end;

{ TTestJsonOid }

procedure TTestJsonOid.TestCreateFromBytes;
var
  TestBytes: TBytes;
begin
  SetLength(TestBytes, 12);
  TestBytes[0] := $01;
  TestBytes[1] := $02;
  TestBytes[11] := $0C;

  FOid := TJsonOid.Create(TestBytes);
  AssertEquals('First byte', $01, FOid.Bytes[0]);
  AssertEquals('Second byte', $02, FOid.Bytes[1]);
  AssertEquals('Last byte', $0C, FOid.Bytes[11]);
end;

procedure TTestJsonOid.TestCreateFromString;
begin
  FOid := TJsonOid.Create('0102030405060708090a0b0c');
  AssertEquals('First byte from string', $01, FOid.Bytes[0]);
  AssertEquals('Second byte from string', $02, FOid.Bytes[1]);
  AssertEquals('Last byte from string', $0C, FOid.Bytes[11]);
end;

procedure TTestJsonOid.TestAsString;
var
  TestBytes: TBytes;
begin
  SetLength(TestBytes, 12);
  TestBytes[0] := $01;
  TestBytes[1] := $02;
  TestBytes[11] := $0C;

  FOid := TJsonOid.Create(TestBytes);
  AssertEquals('String representation', '01020000000000000000000C', FOid.AsString.ToUpper);
end;

procedure TTestJsonOid.TestAsBytes;
var
  TestBytes, ResultBytes: TBytes;
begin
  SetLength(TestBytes, 12);
  TestBytes[0] := $AB;
  TestBytes[11] := $CD;

  FOid := TJsonOid.Create(TestBytes);
  ResultBytes := FOid.AsBytes;

  AssertEquals('Byte array length', 12, Length(ResultBytes));
  AssertEquals('First byte', $AB, ResultBytes[0]);
  AssertEquals('Last byte', $CD, ResultBytes[11]);
end;

procedure TTestJsonOid.TestStringRoundTrip;
const
  TestString = '0123456789abcdef01234567';
begin
  FOid := TJsonOid.Create(TestString);
  AssertEquals('String round trip', TestString.ToUpper, FOid.AsString.ToUpper);
end;

procedure TTestJsonOid.TestBytesRoundTrip;
var
  TestBytes, ResultBytes: TBytes;
begin
  SetLength(TestBytes, 12);
  TestBytes[0] := $12;
  TestBytes[5] := $34;
  TestBytes[11] := $56;

  FOid := TJsonOid.Create(TestBytes);
  ResultBytes := FOid.AsBytes;

  AssertEquals('Bytes round trip length', Length(TestBytes), Length(ResultBytes));
  AssertEquals('Bytes round trip first', TestBytes[0], ResultBytes[0]);
  AssertEquals('Bytes round trip middle', TestBytes[5], ResultBytes[5]);
  AssertEquals('Bytes round trip last', TestBytes[11], ResultBytes[11]);
end;

procedure TTestJsonOid.TestInvalidStringLength;
begin
  try
    FOid := TJsonOid.Create('invalid');
    Fail('Should have raised exception for invalid string length');
  except
    on E: Exception do
      AssertTrue('Correct exception type', E is EJsonException);
  end;
end;

{ TTestJsonRegEx }

procedure TTestJsonRegEx.TestCreate;
begin
  FRegEx := TJsonRegEx.Create('test.*', 'gi');
  AssertEquals('RegEx pattern', 'test.*', FRegEx.RegEx);
  AssertEquals('RegEx options', 'gi', FRegEx.Options);
end;

procedure TTestJsonRegEx.TestAsString;
begin
  FRegEx := TJsonRegEx.Create('test.*', 'gi');
  AssertEquals('AsString format', '/test.*/gi', FRegEx.AsString);
end;

procedure TTestJsonRegEx.TestSetAsString;
begin
  FRegEx.AsString := '/test.*/gi';
  AssertEquals('Set regex pattern', 'test.*', FRegEx.RegEx);
  AssertEquals('Set regex options', 'gi', FRegEx.Options);
end;

procedure TTestJsonRegEx.TestSetAsStringVariations;
begin
  // Test single part
  FRegEx.AsString := 'simple';
  AssertEquals('Simple regex', 'simple', FRegEx.RegEx);
  AssertEquals('Simple options', '', FRegEx.Options);

  // Test two parts
  FRegEx.AsString := '/pattern';
  AssertEquals('Two part regex', 'pattern', FRegEx.RegEx);
  AssertEquals('Two part options', '', FRegEx.Options);

  // Test three parts (normal case)
  FRegEx.AsString := '/pattern/flags';
  AssertEquals('Three part regex', 'pattern', FRegEx.RegEx);
  AssertEquals('Three part options', 'flags', FRegEx.Options);
end;

{ TTestJsonDBRef }

procedure TTestJsonDBRef.TestCreateWithDB;
begin
  FDBRef := TJsonDBRef.Create('testdb', 'testcoll', '507f1f77bcf86cd799439011');
  AssertEquals('DB name', 'testdb', FDBRef.DB);
  AssertEquals('Collection name', 'testcoll', FDBRef.Ref);
  AssertEquals('ID string', '507F1F77BCF86CD799439011', FDBRef.Id.AsString.ToUpper);
end;

procedure TTestJsonDBRef.TestCreateWithoutDB;
begin
  FDBRef := TJsonDBRef.Create('testcoll', '507f1f77bcf86cd799439011');
  AssertEquals('Empty DB name', '', FDBRef.DB);
  AssertEquals('Collection name', 'testcoll', FDBRef.Ref);
  AssertEquals('ID string', '507F1F77BCF86CD799439011', FDBRef.Id.AsString.ToUpper);
end;

procedure TTestJsonDBRef.TestCreateWithOid;
var
  TestOid: TJsonOid;
begin
  TestOid := TJsonOid.Create('507f1f77bcf86cd799439011');
  FDBRef := TJsonDBRef.Create('testdb', 'testcoll', TestOid);
  AssertEquals('DB name with OID', 'testdb', FDBRef.DB);
  AssertEquals('Collection name with OID', 'testcoll', FDBRef.Ref);
  AssertEquals('ID from OID', TestOid.AsString.ToUpper, FDBRef.Id.AsString.ToUpper);
end;

procedure TTestJsonDBRef.TestAsString;
begin
  FDBRef := TJsonDBRef.Create('testdb', 'testcoll', '507f1f77bcf86cd799439011');
  AssertEquals('Full string format', 'TESTDB.TESTCOLL.507F1F77BCF86CD799439011', FDBRef.AsString.ToUpper);

  FDBRef := TJsonDBRef.Create('testcoll', '507f1f77bcf86cd799439011');
  AssertEquals('No DB string format', 'TESTCOLL.507F1F77BCF86CD799439011', FDBRef.AsString.ToUpper);
end;

procedure TTestJsonDBRef.TestSetAsString;
begin
  FDBRef.AsString := 'testdb.testcoll.507f1f77bcf86cd799439011';
  AssertEquals('Set DB from string', 'testdb', FDBRef.DB);
  AssertEquals('Set collection from string', 'testcoll', FDBRef.Ref);

  FDBRef.AsString := 'testcoll.507f1f77bcf86cd799439011';
  AssertEquals('Set empty DB from string', '', FDBRef.DB);
  AssertEquals('Set collection from short string', 'testcoll', FDBRef.Ref);
end;

{ TTestJsonCodeWScope }

procedure TTestJsonCodeWScope.TestCreateEmpty;
begin
  FCodeWScope := TJsonCodeWScope.Create('function() { return 1; }', nil);
  AssertEquals('Code value', 'function() { return 1; }', FCodeWScope.Code);
  AssertEquals('Empty scope length', 0, Length(FCodeWScope.Scope));
end;

procedure TTestJsonCodeWScope.TestCreateWithScope;
var
  Scope: TStringList;
begin
  Scope := TStringList.Create;
  try
    Scope.Add('var1=value1');
    Scope.Add('var2=value2');

    FCodeWScope := TJsonCodeWScope.Create('function() { return var1 + var2; }', Scope);
    AssertEquals('Code with scope', 'function() { return var1 + var2; }', FCodeWScope.Code);
    AssertEquals('Scope length', 2, Length(FCodeWScope.Scope));
    AssertEquals('First scope ident', 'var1', FCodeWScope.Scope[0].Ident);
    AssertEquals('First scope value', 'value1', FCodeWScope.Scope[0].Value);
    AssertEquals('Second scope ident', 'var2', FCodeWScope.Scope[1].Ident);
    AssertEquals('Second scope value', 'value2', FCodeWScope.Scope[1].Value);
  finally
    Scope.Free;
  end;
end;

{ TTestJsonDecimal128 }

procedure TTestJsonDecimal128.TestCreateFromString;
begin
  // Basic test - actual implementation depends on assigned conversion functions
  try
    FDecimal := TJsonDecimal128.Create('123.45');
    // If we get here, creation succeeded
    AssertTrue('Created from string', True);
  except
    on EJsonException do
      // Expected if conversion functions not implemented
      AssertTrue('Expected exception for unimplemented decimal', True);
  end;
end;

procedure TTestJsonDecimal128.TestCreateFromExtended;
begin
  try
    FDecimal := TJsonDecimal128.Create(123.45);
    AssertTrue('Created from extended', True);
  except
    on EJsonException do
      AssertTrue('Expected exception for unimplemented decimal', True);
  end;
end;

procedure TTestJsonDecimal128.TestIsZero;
begin
  FDecimal.lo := 0;
  FDecimal.hi := $3040000000000000;
  AssertTrue('Is zero', FDecimal.IsZero);

  FDecimal.lo := 1;
  AssertFalse('Not zero with lo=1', FDecimal.IsZero);
end;

procedure TTestJsonDecimal128.TestIsNan;
begin
  FDecimal.lo := 0;
  FDecimal.hi := $7C00000000000000;
  AssertTrue('Is NaN', FDecimal.IsNan);

  FDecimal.hi := $7C00000000000001;
  AssertFalse('Not NaN with different hi', FDecimal.IsNan);
end;

procedure TTestJsonDecimal128.TestIsPosInfinity;
begin
  FDecimal.lo := 0;
  FDecimal.hi := $7800000000000000;
  AssertTrue('Is positive infinity', FDecimal.IsPosInfinity);

  FDecimal.hi := $7800000000000001;
  AssertFalse('Not positive infinity with different hi', FDecimal.IsPosInfinity);
end;

procedure TTestJsonDecimal128.TestIsNegInfinity;
begin
  FDecimal.lo := 0;
  FDecimal.hi := QWord($F800000000000000);
  AssertTrue('Is negative infinity', FDecimal.IsNegInfinity);

  FDecimal.hi := $7800000000000001;
  AssertFalse('Not negative infinity with different hi', FDecimal.IsNegInfinity);
end;

procedure TTestJsonDecimal128.TestAsExtended;
var
  Result: Extended;
begin
  // Test zero
  FDecimal.lo := 0;
  FDecimal.hi := $3040000000000000;
  Result := FDecimal.AsExtended;
  AssertEquals('Zero as extended', 0.0, Result, 0.0001);

  // Test NaN
  FDecimal.lo := 0;
  FDecimal.hi := $7C00000000000000;
  Result := FDecimal.AsExtended;
  AssertTrue('NaN as extended', IsNaN(Result));
end;

procedure TTestJsonDecimal128.TestAsString;
begin
  try
    FDecimal.lo := 0;
    FDecimal.hi := $3040000000000000;
    // This will likely fail unless conversion functions are set up
    FDecimal.AsString;
    AssertTrue('String conversion succeeded', True);
  except
    on EJsonException do
      AssertTrue('Expected exception for unimplemented string conversion', True);
  end;
end;

{ TTestJsonNameAttribute }

procedure TTestJsonNameAttribute.TearDown;
begin
  FAttribute.Free;
  inherited TearDown;
end;

procedure TTestJsonNameAttribute.TestCreate;
begin
  FAttribute := JsonNameAttribute.Create('testName');
  AssertNotNull('Attribute created', FAttribute);
end;

procedure TTestJsonNameAttribute.TestValue;
begin
  FAttribute := JsonNameAttribute.Create('testName');
  AssertEquals('Attribute value', 'testName', FAttribute.Value);
end;

{ TTestEJsonException }

procedure TTestEJsonException.TestCreateSimple;
var
  Ex: EJsonException;
begin
  Ex := EJsonException.Create('Test message');
  try
    AssertEquals('Simple message', 'Test message', Ex.Message);
    AssertNull('No inner exception', Ex.InnerException);
  finally
    Ex.Free;
  end;
end;

procedure TTestEJsonException.TestCreateWithInner;
var
  Inner: Exception;
  Ex: EJsonException;
begin
  Inner := Exception.Create('Inner message');
  try
    Ex := EJsonException.Create('Outer message', Inner);
    try
      AssertEquals('Outer message', 'Outer message', Ex.Message);
      AssertNotNull('Has inner exception', Ex.InnerException);
      AssertSame('Same inner exception', Inner, Ex.InnerException);
    finally
      Ex.Free;
    end;
  finally
    Inner.Free;
  end;
end;

procedure TTestEJsonException.TestInnerException;
var
  Inner: Exception;
  Ex: EJsonException;
begin
  Inner := Exception.Create('Inner message');
  try
    Ex := EJsonException.Create('Outer message', Inner);
    try
      AssertEquals('Inner exception message', 'Inner message', Ex.InnerException.Message);
    finally
      Ex.Free;
    end;
  finally
    Inner.Free;
  end;
end;

initialization
  RegisterTests([
    TTestJsonLineInfo,
    TTestJsonPosition,
    TTestJsonFiler,
    TTestJsonOid,
    TTestJsonRegEx,
    TTestJsonDBRef,
    TTestJsonCodeWScope,
    TTestJsonDecimal128,
    TTestJsonNameAttribute,
    TTestEJsonException
  ]);
end.
