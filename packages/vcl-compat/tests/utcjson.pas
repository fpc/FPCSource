unit utcjson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, System.JSON;

type

  { TTestJSONObject }

  TTestJSONObject= class(TTestCase)
  private
    FPair: TJSONPair;
    FValue: TJSONValue;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property Value : TJSONValue Read FValue Write FValue;
    Property Pair : TJSONPair Read FPair Write FPair;
  published
    procedure TestHookUp;
    Procedure TestNull;
    Procedure TestParseNull;
    Procedure TestBoolean;
    Procedure TestParseBoolean;
    Procedure TestBoolTrue;
    Procedure TestParseBoolTrue;
    Procedure TestBoolFalse;
    Procedure TestParseBoolFalse;
    Procedure TestString;
    Procedure TestStringNull;
    Procedure TestParseString;
    Procedure TestNumberInt;
    Procedure TestParseNumberInt;
    Procedure TestNumberInt64;
    Procedure TestParseNumberInt64;
    Procedure TestNumberFloat;
    Procedure TestParseNumberFloat;
    Procedure TestJSONPairPair;
    Procedure TestJSONPairStringString;
    Procedure TestJSONPairStringValue;
    Procedure TestJSONPairStringInt;
    Procedure TestJSONPairStringBool;
    Procedure TestJSONPairStringFloat;
    Procedure TestJSONPairStringInt64;
    Procedure TestJSONObjectEmpty;
    Procedure TestJSONObjectOneElement;
    Procedure TestJSONObjectTwoElements;
    Procedure TestJSONParseObjectEmpty;
    Procedure TestJSONParseObjectOneElement;
    Procedure TestJSONParseObjectTwoElements;
    Procedure TestJSONArrayEmpty;
    Procedure TestJSONArrayOneElement;
    Procedure TestJSONArrayTwoElements;
    Procedure TestFindEmptyValue;
    Procedure TestFindObjectName;
    Procedure TestFindObjectIndexName;
    Procedure TestFindObjectNameRecurse;
    Procedure TestFindArrayIndex;
    Procedure TestFindArrayName;
  end;

  { TTestJSONPathParser }

  TTestJSONPathParser = class(TTestCase)
  private
    FParser: TJSONPathParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure AssertEquals(Msg : string; aExpected, aActual : TJSONPathToken); overload;
    Property Parser : TJSONPathParser Read FParser;
  published
    Procedure TestHookup;
    Procedure TestEof;
    Procedure TestName;
    Procedure TestIndexNumeric;
    Procedure TestIndexName;
    Procedure TestDotName;
  end;


implementation

uses typinfo;

procedure TTestJSONObject.TestHookUp;
begin
  AssertNull('No value',value);
end;

procedure TTestJSONObject.TestNull;
begin
  Value:=TJSONNull.Create;
  AssertEquals('ToString','null',Value.ToString);
  AssertEquals('ToJSON','null',Value.ToJSON);
  AssertEquals('Format','null',Value.Format);
  AssertEquals('Null',True,Value.Null);
end;

procedure TTestJSONObject.TestParseNull;
begin
  Value:=TJSONValue.ParseJSONValue('null');
  AssertEquals('Class',TJSONNull,Value.ClassType);
end;

procedure TTestJSONObject.TestBoolean;
begin
  Value:=TJSONBool.Create(True);
  AssertEquals('ToString','true',Value.ToString);
  AssertEquals('ToJSON','true',Value.ToJSON);
  AssertEquals('Format','true',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestParseBoolean;
begin
  Value:=TJSONValue.ParseJSONValue('true',False);
  AssertEquals('Class',TJSONBool,Value.ClassType);
  AssertEquals('ToString','true',Value.ToString);
end;

procedure TTestJSONObject.TestBoolTrue;
begin
  Value:=TJSONTrue.Create;
  AssertEquals('ToString','true',Value.ToString);
  AssertEquals('ToJSON','true',Value.ToJSON);
  AssertEquals('Format','true',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestParseBoolTrue;
begin
  Value:=TJSONValue.ParseJSONValue('true',True);
  AssertEquals('Class',TJSONTrue,Value.ClassType);
  AssertEquals('ToString','true',Value.ToString);
end;

procedure TTestJSONObject.TestBoolFalse;
begin
  Value:=TJSONfalse.Create;
  AssertEquals('ToString','false',Value.ToString);
  AssertEquals('ToJSON','false',Value.ToJSON);
  AssertEquals('Format','false',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestParseBoolFalse;
begin
  Value:=TJSONValue.ParseJSONValue('false',True);
  AssertEquals('Class',TJSONFalse,Value.ClassType);
  AssertEquals('ToString','false',Value.ToString);
end;

procedure TTestJSONObject.TestString;
begin
  Value:=TJSONString.Create('string');
  AssertEquals('ToString','"string"',Value.ToString);
  AssertEquals('ToJSON','"string"',Value.ToJSON);
  AssertEquals('Format','"string"',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestStringNull;
begin
  Value:=TJSONString.Create();
  AssertEquals('ToString','null',Value.ToString);
  AssertEquals('Null',True,Value.Null);
end;

procedure TTestJSONObject.TestParseString;
begin
  Value:=TJSONValue.ParseJSONValue('"string"',True);
  AssertEquals('Class',TJSONString,Value.ClassType);
  AssertEquals('ToString','"string"',Value.ToString);
end;

procedure TTestJSONObject.TestNumberInt;
begin
  Value:=TJSONNumber.Create(12);
  AssertEquals('ToString','12',Value.ToString);
  AssertEquals('ToJSON','12',Value.ToJSON);
  AssertEquals('Format','12',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestParseNumberInt;
begin
  Value:=TJSONValue.ParseJSONValue('12',True);
  AssertEquals('Class',TJSONNumber,Value.ClassType);
  AssertEquals('ToString','12',Value.ToString);
end;

procedure TTestJSONObject.TestNumberInt64;
begin
  Value:=TJSONNumber.Create(Int64(12));
  AssertEquals('ToString','12',Value.ToString);
  AssertEquals('ToJSON','12',Value.ToJSON);
  AssertEquals('Format','12',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestParseNumberInt64;
begin
  Value:=TJSONValue.ParseJSONValue('1221212121111',True);
  AssertEquals('Class',TJSONNumber,Value.ClassType);
  AssertEquals('ToString','1221212121111',Value.ToString);
end;

procedure TTestJSONObject.TestNumberFloat;

var
  V : String;

begin
  V:=FloatToStr(12.34,TFormatSettings.Invariant);
  Value:=TJSONNumber.Create(12.34);
  AssertEquals('ToString',V,Value.ToString);
  AssertEquals('ToJSON',V,Value.ToJSON);
  AssertEquals('Format',V,Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestParseNumberFloat;
var
  V : String;

begin
  V:=FloatToStr(12.34,TFormatSettings.Invariant);
  Value:=TJSONValue.ParseJSONValue('12.34',True);
  AssertEquals('Class',TJSONNumber,Value.ClassType);
  AssertEquals('ToString',V,Value.ToString);
end;

procedure TTestJSONObject.TestJSONPairPair;
begin
  Pair:=TJSONPair.Create(TJSONString.Create('a'),TJSONNumber.Create('12'));
  AssertEquals('ToString','"a":12',Pair.ToString);
  AssertEquals('ToJSON','"a":12',Pair.ToJSON);
  AssertEquals('Format','"a":12',Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONPairStringString;
begin
  Pair:=TJSONPair.Create('a','b');
  AssertEquals('ToString','"a":"b"',Pair.ToString);
  AssertEquals('ToJSON','"a":"b"',Pair.ToJSON);
  AssertEquals('Format','"a":"b"',Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONPairStringValue;
begin
  Pair:=TJSONPair.Create('a',TJSONString.Create('b'));
  AssertEquals('ToString','"a":"b"',Pair.ToString);
  AssertEquals('ToJSON','"a":"b"',Pair.ToJSON);
  AssertEquals('Format','"a":"b"',Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONPairStringInt;
begin
  Pair:=TJSONPair.Create('a',1);
  AssertEquals('ToString','"a":1',Pair.ToString);
  AssertEquals('ToJSON','"a":1',Pair.ToJSON);
  AssertEquals('Format','"a":1',Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONPairStringBool;
begin
  Pair:=TJSONPair.Create('a',True);
  AssertEquals('ToString','"a":true',Pair.ToString);
  AssertEquals('ToJSON','"a":true',Pair.ToJSON);
  AssertEquals('Format','"a":true',Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONPairStringFloat;

var
  V : String;

begin
  V:=FloatToStr(12.34,TFormatSettings.Invariant);
  Pair:=TJSONPair.Create('a',12.34);
  AssertEquals('ToString','"a":'+V,Pair.ToString);
  AssertEquals('ToJSON','"a":'+V,Pair.ToJSON);
  AssertEquals('Format','"a":'+V,Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONPairStringInt64;

begin
  Pair:=TJSONPair.Create('a',1221212121111);
  AssertEquals('ToString','"a":1221212121111',Pair.ToString);
  AssertEquals('ToJSON','"a":1221212121111',Pair.ToJSON);
  AssertEquals('Format','"a":1221212121111',Pair.Format);
  AssertEquals('Null',False,Pair.Null);
end;

procedure TTestJSONObject.TestJSONObjectEmpty;
begin
  Value:=TJSONObject.Create;
  AssertEquals('ToString','{}',Value.ToString);
  AssertEquals('ToJSON','{}',Value.ToJSON);
  AssertEquals('Format','{'+sLineBreak+'}',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestJSONObjectOneElement;
begin
  Value:=TJSONObject.Create(TJSONPair.Create('a','b'));
  AssertEquals('ToString','{"a":"b"}',Value.ToString);
  AssertEquals('ToJSON','{"a":"b"}',Value.ToJSON);
  AssertEquals('Format','{'+sLineBreak+'    "a":"b"'+sLineBreak+'}',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestJSONObjectTwoElements;

var
  O : TJSONObject;

begin
  O:=TJSONObject.Create(TJSONPair.Create('a','b'));
  O.AddPair('c','d');
  AssertEquals('Count',2,O.Count);
  Value:=O;
  AssertEquals('ToString','{"a":"b","c":"d"}',Value.ToString);
  AssertEquals('ToJSON','{"a":"b","c":"d"}',Value.ToJSON);
  AssertEquals('Format','{'+sLineBreak+
                           '    "a":"b",'+sLineBreak+
                           '    "c":"d"'+sLineBreak+'}',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestJSONParseObjectEmpty;
begin
  Value:=TJSONValue.ParseJSONValue('{}',True);
  AssertEquals('Class',TJSONObject,Value.ClassType);
  AssertEquals('ToString','{}',Value.ToString);
end;

procedure TTestJSONObject.TestJSONParseObjectOneElement;
begin
  Value:=TJSONValue.ParseJSONValue('{"a":"b"}',True);
  AssertEquals('Class',TJSONObject,Value.ClassType);
  AssertEquals('ToString','{"a":"b"}',Value.ToString);
end;

procedure TTestJSONObject.TestJSONParseObjectTwoElements;
begin
  Value:=TJSONValue.ParseJSONValue('{"a":"b","c":"d"}',True);
  AssertEquals('Class',TJSONObject,Value.ClassType);
  AssertEquals('ToString','{"a":"b","c":"d"}',Value.ToString);
end;

procedure TTestJSONObject.TestJSONArrayEmpty;

begin
  Value:=TJSONArray.Create;
  AssertEquals('ToString','[]',Value.ToString);
  AssertEquals('ToJSON','[]',Value.ToJSON);
  AssertEquals('Format','['+sLineBreak+']',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestJSONArrayOneElement;
begin
  Value:=TJSONArray.Create(TJSONNumber.Create(1));
  AssertEquals('ToString','[1]',Value.ToString);
  AssertEquals('ToJSON','[1]',Value.ToJSON);
  AssertEquals('Format','['+sLineBreak+'    1'+sLineBreak+']',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestJSONArrayTwoElements;
begin
  Value:=TJSONArray.Create(TJSONNumber.Create(1),TJSONNumber.Create(2));
  AssertEquals('ToString','[1,2]',Value.ToString);
  AssertEquals('ToJSON','[1,2]',Value.ToJSON);
  AssertEquals('Format','['+sLineBreak+
                          '    1,'+sLineBreak+
                          '    2'+sLineBreak+
                          ']',Value.Format);
  AssertEquals('Null',False,Value.Null);
end;

procedure TTestJSONObject.TestFindEmptyValue;
begin
  FValue:=TJSONString.Create('Name');
  AssertSame('Empty returns same',FValue,FValue.FindValue(''));
end;

procedure TTestJSONObject.TestFindObjectName;

var
  V : TJSONValue;
begin
  FValue:=TJSONObject.Create(TJSONPair.Create('a','b'));
  V:=FValue.FindValue('a');
  AssertNotNull('Have JSON value',V);
  AssertEquals('Have correct value','b',V.Value);
  V:=FValue.FindValue('c');
  AssertNull('Have no JSON value',V);
end;

procedure TTestJSONObject.TestFindObjectIndexName;
var
  V : TJSONValue;
begin
  FValue:=TJSONObject.Create(TJSONPair.Create('a','b'));
  V:=FValue.FindValue('["a"]');
  AssertNotNull('Have JSON value',V);
  AssertEquals('Have correct value','b',V.Value);
  V:=FValue.FindValue('c');
  AssertNull('Have no JSON value',V);
end;

procedure TTestJSONObject.TestFindObjectNameRecurse;

var
  V : TJSONValue;

begin
  V:=TJSONObject.Create(TJSONPair.Create('b','c'));
  FValue:=TJSONObject.Create(TJSONPair.Create('a',v));
  V:=FValue.FindValue('a.b');
  AssertNotNull('Have JSON value',V);
  AssertEquals('Have correct value','c',V.Value);
  V:=FValue.FindValue('a.c');
  AssertNull('Have no JSON value',V);
end;

procedure TTestJSONObject.TestFindArrayIndex;
var
  V : TJSONValue;
begin
  FValue:=TJSONArray.Create('a','b');
  V:=FValue.FindValue('[0]');
  AssertNotNull('Have JSON value',V);
  AssertEquals('Have correct value','a',V.Value);
  V:=FValue.FindValue('[1]');
  AssertNotNull('Have JSON value',V);
  AssertEquals('Have correct value','b',V.Value);
  V:=FValue.FindValue('[-1]');
  AssertNull('Have no JSON value -1',V);
  V:=FValue.FindValue('[2]');
  AssertNull('Have no JSON value 2',V);
end;

procedure TTestJSONObject.TestFindArrayName;
var
  V : TJSONValue;
begin
  FValue:=TJSONArray.Create('a','b');
  V:=FValue.FindValue('a');
  AssertNull('Have no JSON value',V);
end;

procedure TTestJSONObject.SetUp;
begin
  FreeAndNil(Fvalue);
  FreeAndNil(FPair);
end;

procedure TTestJSONObject.TearDown;
begin
  FreeAndNil(FPair);
  FreeAndNil(Fvalue);
end;

{ TTestJSONPathParser }

procedure TTestJSONPathParser.SetUp;
begin
  inherited SetUp;
  FParser:=Default(TJSONPathParser);
end;

procedure TTestJSONPathParser.TearDown;
begin
  FParser:=Default(TJSONPathParser);
  inherited TearDown;
end;

procedure TTestJSONPathParser.AssertEquals(Msg: string; aExpected, aActual: TJSONPathToken);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TJSONPathParser.TToken),Ord(aExpected)),
                   GetEnumName(TypeInfo(TJSONPathParser.TToken),Ord(aActual)));
end;

procedure TTestJSONPathParser.TestHookup;

begin
  AssertEquals('Default', TJSONPathToken.UnDefined, FParser.Token);
end;

procedure TTestJSONPathParser.TestEof;
begin
  FParser:=TJSONPathParser.Create('');
  AssertEquals('Empty is eof ',TJSONPathToken.Eof,FParser.NextToken);
  AssertTrue('eof',FParser.IsEOF);
end;

procedure TTestJSONPathParser.TestName;
begin
  FParser:=TJSONPathParser.Create('name');
  AssertEquals('Type',TJSONPathToken.Name,FParser.NextToken);
  AssertEquals('Value','name',FParser.TokenName);
  AssertTrue('eof',FParser.IsEOF);
end;

procedure TTestJSONPathParser.TestIndexNumeric;
begin
  FParser:=TJSONPathParser.Create('[12]');
  AssertEquals('Type',TJSONPathToken.ArrayIndex,FParser.NextToken);
  AssertEquals('Value',12,FParser.TokenArrayIndex);
  AssertTrue('eof',FParser.IsEOF);
end;

procedure TTestJSONPathParser.TestIndexName;
begin
  FParser:=TJSONPathParser.Create('["name"]');
  AssertEquals('Type',TJSONPathToken.name,FParser.NextToken);
  AssertEquals('Value','name',FParser.TokenName);
  AssertTrue('eof',FParser.IsEOF);
end;

procedure TTestJSONPathParser.TestDotName;
begin
  FParser:=TJSONPathParser.Create('.name');
  AssertEquals('Type',TJSONPathToken.Name,FParser.NextToken);
  AssertEquals('Value','name',FParser.TokenName);
  AssertTrue('eof',FParser.IsEOF);
end;

initialization

  RegisterTests([TTestJSONObject,TTestJSONPathParser]);
end.

