unit utc.opf.variants;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Variants,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  SysUtils, Variants,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Variants;

type

  { TTestVariantJSON }

  TTestVariantJSON = class(TTestCase)
  published
    procedure TestStringRoundTrip;
    procedure TestIntegerRoundTrip;
    procedure TestFloatRoundTrip;
    procedure TestBooleanTrueRoundTrip;
    procedure TestBooleanFalseRoundTrip;
    procedure TestNullRoundTrip;
    procedure TestEmptyRoundTrip;
    procedure TestMalformedJSONRaises;
  end;

  { TTestVariantArrayJSON }

  TTestVariantArrayJSON = class(TTestCase)
  published
    procedure TestIntegerArrayRoundTrip;
    procedure TestStringArrayRoundTrip;
    procedure TestMixedTypeArrayRoundTrip;
    procedure TestEmptyArrayRoundTrip;
    procedure TestVariantElementArrayRoundTrip;
    procedure TestNestedArrayRoundTrip;
    procedure TestMatrix2DRoundTrip;
    procedure TestArray3DRoundTrip;
    procedure TestRaggedArrayDeserialize;
  end;

implementation

{ TTestVariantJSON }

procedure TTestVariantJSON.TestStringRoundTrip;
var
  V: Variant;
  J: UTF8String;
begin
  V := 'hello world';
  J := VariantToJSON(V);
  V := JSONToVariant(J);
  AssertEquals('String round-trip', 'hello world', String(V));
end;

procedure TTestVariantJSON.TestIntegerRoundTrip;
var
  V: Variant;
  J: UTF8String;
begin
  V := Integer(42);
  J := VariantToJSON(V);
  AssertEquals('JSON integer', '42', J);
  V := JSONToVariant(J);
  AssertEquals('Integer round-trip', 42, Integer(V));
end;

procedure TTestVariantJSON.TestFloatRoundTrip;
var
  V: Variant;
  J: UTF8String;
  D: Double;
begin
  V := Double(3.14);
  J := VariantToJSON(V);
  V := JSONToVariant(J);
  D := Double(V);
  AssertTrue('Float round-trip', Abs(D - 3.14) < 0.001);
end;

procedure TTestVariantJSON.TestBooleanTrueRoundTrip;
var
  V: Variant;
  J: UTF8String;
begin
  V := True;
  J := VariantToJSON(V);
  AssertEquals('JSON true', 'true', J);
  V := JSONToVariant(J);
  AssertTrue('Boolean true round-trip', Boolean(V));
end;

procedure TTestVariantJSON.TestBooleanFalseRoundTrip;
var
  V: Variant;
  J: UTF8String;
begin
  V := False;
  J := VariantToJSON(V);
  AssertEquals('JSON false', 'false', J);
  V := JSONToVariant(J);
  AssertFalse('Boolean false round-trip', Boolean(V));
end;

procedure TTestVariantJSON.TestNullRoundTrip;
var
  V: Variant;
  J: UTF8String;
begin
  V := Null;
  J := VariantToJSON(V);
  AssertEquals('JSON null', 'null', J);
  V := JSONToVariant(J);
  AssertTrue('Null round-trip', VarIsNull(V));
end;

procedure TTestVariantJSON.TestEmptyRoundTrip;
var
  V: Variant;
  J: UTF8String;
begin
  V := Unassigned;
  J := VariantToJSON(V);
  AssertEquals('JSON empty', '{"$vartype": "empty"}', J);
  V := JSONToVariant(J);
  AssertTrue('Empty stays empty', VarIsEmpty(V));
end;

procedure TTestVariantJSON.TestMalformedJSONRaises;
begin
  try
    JSONToVariant('{broken');
    Fail('Expected EOPFJson');
  except
    on E: EOPFJson do
      AssertTrue('Has error message', Length(E.Message) > 0);
  end;
end;

{ TTestVariantArrayJSON }

procedure TTestVariantArrayJSON.TestIntegerArrayRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
begin
  V := VarArrayOf([1, 2, 3]);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '[1, 2, 3]', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Length', 2, VarArrayHighBound(R, 1));
  AssertEquals('Elem 0', 1, Integer(VarArrayGet(R, [0])));
  AssertEquals('Elem 1', 2, Integer(VarArrayGet(R, [1])));
  AssertEquals('Elem 2', 3, Integer(VarArrayGet(R, [2])));
end;

procedure TTestVariantArrayJSON.TestStringArrayRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
begin
  V := VarArrayOf(['alpha', 'beta']);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '["alpha", "beta"]', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Elem 0', 'alpha', String(VarArrayGet(R, [0])));
  AssertEquals('Elem 1', 'beta', String(VarArrayGet(R, [1])));
end;

procedure TTestVariantArrayJSON.TestMixedTypeArrayRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
begin
  V := VarArrayOf([1, 'hello', True, Null]);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '[1, "hello", true, null]', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Elem 0', 1, Integer(VarArrayGet(R, [0])));
  AssertEquals('Elem 1', 'hello', String(VarArrayGet(R, [1])));
  AssertTrue('Elem 2', Boolean(VarArrayGet(R, [2])));
  AssertTrue('Elem 3 null', VarIsNull(VarArrayGet(R, [3])));
end;

procedure TTestVariantArrayJSON.TestEmptyArrayRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
begin
  V := VarArrayCreate([0, -1], varVariant);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '[]', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertTrue('Is empty', VarArrayHighBound(R, 1) < VarArrayLowBound(R, 1));
end;

procedure TTestVariantArrayJSON.TestVariantElementArrayRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
begin
  V := VarArrayCreate([0, 2], varVariant);
  VarArrayPut(V, Integer(1), [0]);
  VarArrayPut(V, 'two', [1]);
  VarArrayPut(V, True, [2]);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '[1, "two", true]', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Elem 0', 1, Integer(VarArrayGet(R, [0])));
  AssertEquals('Elem 1', 'two', String(VarArrayGet(R, [1])));
  AssertTrue('Elem 2', Boolean(VarArrayGet(R, [2])));
end;

procedure TTestVariantArrayJSON.TestNestedArrayRoundTrip;
var
  V, Inner1, Inner2, R, RInner: Variant;
  J: UTF8String;
begin
  Inner1 := VarArrayOf([1, 2]);
  Inner2 := VarArrayOf([3]);
  V := VarArrayCreate([0, 1], varVariant);
  VarArrayPut(V, Inner1, [0]);
  VarArrayPut(V, Inner2, [1]);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '{"$vartype": "array", "value": [[1, 2], [3]]}', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Dim count', 1, VarArrayDimCount(R));
  RInner := VarArrayGet(R, [0]);
  AssertTrue('Inner 0 is array', VarIsArray(RInner));
  AssertEquals('Inner 0 elem 0', 1, Integer(VarArrayGet(RInner, [0])));
  AssertEquals('Inner 0 elem 1', 2, Integer(VarArrayGet(RInner, [1])));
  RInner := VarArrayGet(R, [1]);
  AssertTrue('Inner 1 is array', VarIsArray(RInner));
  AssertEquals('Inner 1 elem 0', 3, Integer(VarArrayGet(RInner, [0])));
end;

procedure TTestVariantArrayJSON.TestMatrix2DRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
begin
  V := VarArrayCreate([0, 1, 0, 2], varDouble);
  VarArrayPut(V, Double(1.0), [0, 0]);
  VarArrayPut(V, Double(2.0), [0, 1]);
  VarArrayPut(V, Double(3.0), [0, 2]);
  VarArrayPut(V, Double(4.0), [1, 0]);
  VarArrayPut(V, Double(5.0), [1, 1]);
  VarArrayPut(V, Double(6.0), [1, 2]);
  J := VariantToJSON(V);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Dim count', 2, VarArrayDimCount(R));
  AssertEquals('Dim 1 high', 1, VarArrayHighBound(R, 1));
  AssertEquals('Dim 2 high', 2, VarArrayHighBound(R, 2));
  AssertTrue('Elem [0,0]', Abs(Double(VarArrayGet(R, [0, 0])) - 1.0) < 0.001);
  AssertTrue('Elem [0,2]', Abs(Double(VarArrayGet(R, [0, 2])) - 3.0) < 0.001);
  AssertTrue('Elem [1,1]', Abs(Double(VarArrayGet(R, [1, 1])) - 5.0) < 0.001);
  AssertTrue('Elem [1,2]', Abs(Double(VarArrayGet(R, [1, 2])) - 6.0) < 0.001);
end;

procedure TTestVariantArrayJSON.TestArray3DRoundTrip;
var
  V, R: Variant;
  J: UTF8String;
  D1, D2, D3: Integer;
begin
  V := VarArrayCreate([0, 1, 0, 1, 0, 1], varInteger);
  { Fill 2x2x2 with values 1..8 }
  VarArrayPut(V, Integer(1), [0, 0, 0]);
  VarArrayPut(V, Integer(2), [0, 0, 1]);
  VarArrayPut(V, Integer(3), [0, 1, 0]);
  VarArrayPut(V, Integer(4), [0, 1, 1]);
  VarArrayPut(V, Integer(5), [1, 0, 0]);
  VarArrayPut(V, Integer(6), [1, 0, 1]);
  VarArrayPut(V, Integer(7), [1, 1, 0]);
  VarArrayPut(V, Integer(8), [1, 1, 1]);
  J := VariantToJSON(V);
  AssertEquals('JSON output', '[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]', J);
  R := JSONToVariant(J);
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Dim count', 3, VarArrayDimCount(R));
  for D1 := 0 to 1 do
    for D2 := 0 to 1 do
      for D3 := 0 to 1 do
        AssertEquals(Format('Elem [%d,%d,%d]', [D1, D2, D3]),
          D1 * 4 + D2 * 2 + D3 + 1,
          Integer(VarArrayGet(R, [D1, D2, D3])));
end;

procedure TTestVariantArrayJSON.TestRaggedArrayDeserialize;
var
  R, Inner: Variant;
begin
  R := JSONToVariant('[[1,2],[3]]');
  AssertTrue('Is array', VarIsArray(R));
  AssertEquals('Dim count', 1, VarArrayDimCount(R));
  AssertEquals('Length', 1, VarArrayHighBound(R, 1));
  Inner := VarArrayGet(R, [0]);
  AssertTrue('Inner 0 is array', VarIsArray(Inner));
  AssertEquals('Inner 0 len', 1, VarArrayHighBound(Inner, 1));
  AssertEquals('Inner 0 elem 0', 1, Integer(VarArrayGet(Inner, [0])));
  AssertEquals('Inner 0 elem 1', 2, Integer(VarArrayGet(Inner, [1])));
  Inner := VarArrayGet(R, [1]);
  AssertTrue('Inner 1 is array', VarIsArray(Inner));
  AssertEquals('Inner 1 len', 0, VarArrayHighBound(Inner, 1));
  AssertEquals('Inner 1 elem 0', 3, Integer(VarArrayGet(Inner, [0])));
end;

initialization
  RegisterTest(TTestVariantJSON);
  RegisterTest(TTestVariantArrayJSON);
end.
