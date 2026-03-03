unit utcjsonconverters;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TypInfo, Rtti, Generics.Collections, StreamEx,
  System.JSON.Types, System.JSON.Writers, System.JSON.Readers,
  System.JSON.Serializers, System.JSON.Converters;

type
  TConvTestColor = (ctRed, ctGreen, ctBlue, ctYellow);
  TConvTestColors = set of TConvTestColor;
  TIntegerArray = array of Integer;

  { TTestJsonEnumNameConverter }

  TTestJsonEnumNameConverter = class(TTestCase)
  private
    FConverter: TJsonEnumNameConverter;
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanConvertEnum;
    procedure TestCanConvertNonEnum;
    procedure TestWriteJsonEnum;
    procedure TestReadJsonEnum;
    procedure TestRoundTripEnum;
  end;

  { TTestJsonSetNamesConverter }

  TTestJsonSetNamesConverter = class(TTestCase)
  private
    FConverter: TJsonSetNamesConverter;
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanConvertSet;
    procedure TestCanConvertNonSet;
    procedure TestWriteJsonEmptySet;
    procedure TestWriteJsonSingleElement;
    procedure TestWriteJsonMultipleElements;
    procedure TestReadJsonEmptySet;
    procedure TestReadJsonSingleElement;
    procedure TestRoundTripSet;
  end;

  { TTestJsonGUIDConverter }

  TTestJsonGUIDConverter = class(TTestCase)
  private
    FConverter: TJsonGUIDConverter;
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanConvertGUID;
    procedure TestCanConvertNonGUID;
    procedure TestWriteJsonGUID;
    procedure TestReadJsonGUID;
    procedure TestRoundTripGUID;
  end;

  { TTestJsonListHelperConverter }

  TTestJsonListHelperConverter = class(TTestCase)
  private
    FConverter: TJsonListHelperConverter;
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCanConvertDynArray;
    procedure TestCanConvertNonArray;
    procedure TestShouldEditDynArray;
    procedure TestWriteJsonIntArray;
    procedure TestWriteJsonEmptyArray;
    procedure TestReadJsonIntArray;
    procedure TestRoundTripDynArray;
  end;

  { TTestJsonConverterIntegration }

  TTestJsonConverterIntegration = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEnumConverterViaSerializer;
    procedure TestSetConverterViaSerializer;
    procedure TestGUIDConverterViaSerializer;
  end;

implementation

{ Helper to write a TValue through a converter and capture the JSON string }

function WriteConverterToString(const AConverter: TJsonConverter; const AValue: TValue;
  const ASerializer: TJsonSerializer): string;
var
  SW: TStringWriter;
  W: TJsonTextWriter;
begin
  SW := TStringWriter.Create;
  try
    W := TJsonTextWriter.Create(SW);
    try
      AConverter.WriteJson(W, AValue, ASerializer);
      W.Flush;
      Result := SW.ToString;
    finally
      W.Free;
    end;
  finally
    SW.Free;
  end;
end;

{ Helper to read a TValue through a converter from a JSON string }

function ReadConverterFromString(const AConverter: TJsonConverter; const AJson: string;
  ATypeInf: PTypeInfo; const ASerializer: TJsonSerializer): TValue;
var
  SR: TStringReader;
  R: TJsonTextReader;
begin
  SR := TStringReader.Create(AJson);
  R := TJsonTextReader.Create(SR);
  try
    R.Read;
    Result := AConverter.ReadJson(R, ATypeInf, TValue.Empty, ASerializer);
  finally
    R.Free; // CloseInput frees SR
  end;
end;

{ TTestJsonEnumNameConverter }

procedure TTestJsonEnumNameConverter.SetUp;
begin
  FConverter := TJsonEnumNameConverter.Create;
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonEnumNameConverter.TearDown;
begin
  FConverter.Free;
  FSerializer.Free;
end;

procedure TTestJsonEnumNameConverter.TestCanConvertEnum;
begin
  AssertTrue('Should convert enums', FConverter.CanConvert(TypeInfo(TConvTestColor)));
end;

procedure TTestJsonEnumNameConverter.TestCanConvertNonEnum;
begin
  AssertFalse('Should not convert Integer', FConverter.CanConvert(TypeInfo(Integer)));
  AssertFalse('Should not convert string', FConverter.CanConvert(TypeInfo(string)));
end;

procedure TTestJsonEnumNameConverter.TestWriteJsonEnum;
var
  S: string;
begin
  S := WriteConverterToString(FConverter, TValue.From<TConvTestColor>(ctGreen), FSerializer);
  AssertEquals('Should write enum name', '"ctGreen"', S);
end;

procedure TTestJsonEnumNameConverter.TestReadJsonEnum;
var
  V: TValue;
begin
  V := ReadConverterFromString(FConverter, '"ctBlue"', TypeInfo(TConvTestColor), FSerializer);
  AssertEquals('Should read enum from name', Ord(ctBlue), V.AsOrdinal);
end;

procedure TTestJsonEnumNameConverter.TestRoundTripEnum;
var
  S: string;
  V: TValue;
begin
  S := WriteConverterToString(FConverter, TValue.From<TConvTestColor>(ctYellow), FSerializer);
  V := ReadConverterFromString(FConverter, S, TypeInfo(TConvTestColor), FSerializer);
  AssertEquals('Round-trip should preserve enum', Ord(ctYellow), V.AsOrdinal);
end;

{ TTestJsonSetNamesConverter }

procedure TTestJsonSetNamesConverter.SetUp;
begin
  FConverter := TJsonSetNamesConverter.Create;
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSetNamesConverter.TearDown;
begin
  FConverter.Free;
  FSerializer.Free;
end;

procedure TTestJsonSetNamesConverter.TestCanConvertSet;
begin
  AssertTrue('Should convert sets', FConverter.CanConvert(TypeInfo(TConvTestColors)));
end;

procedure TTestJsonSetNamesConverter.TestCanConvertNonSet;
begin
  AssertFalse('Should not convert Integer', FConverter.CanConvert(TypeInfo(Integer)));
end;

procedure TTestJsonSetNamesConverter.TestWriteJsonEmptySet;
var
  S: string;
  Colors: TConvTestColors;
begin
  Colors := [];
  S := WriteConverterToString(FConverter, TValue.From<TConvTestColors>(Colors), FSerializer);
  AssertEquals('Empty set should produce empty string', '""', S);
end;

procedure TTestJsonSetNamesConverter.TestWriteJsonSingleElement;
var
  S: string;
  Colors: TConvTestColors;
begin
  Colors := [ctRed];
  S := WriteConverterToString(FConverter, TValue.From<TConvTestColors>(Colors), FSerializer);
  AssertTrue('Should contain ctRed', Pos('ctRed', S) > 0);
end;

procedure TTestJsonSetNamesConverter.TestWriteJsonMultipleElements;
var
  S: string;
  Colors: TConvTestColors;
begin
  Colors := [ctRed, ctBlue];
  S := WriteConverterToString(FConverter, TValue.From<TConvTestColors>(Colors), FSerializer);
  AssertTrue('Should contain ctRed', Pos('ctRed', S) > 0);
  AssertTrue('Should contain ctBlue', Pos('ctBlue', S) > 0);
end;

procedure TTestJsonSetNamesConverter.TestReadJsonEmptySet;
var
  V: TValue;
  Colors: TConvTestColors;
begin
  V := ReadConverterFromString(FConverter, '""', TypeInfo(TConvTestColors), FSerializer);
  Colors := V.AsType<TConvTestColors>;
  AssertTrue('Should be empty set', Colors = []);
end;

procedure TTestJsonSetNamesConverter.TestReadJsonSingleElement;
var
  V: TValue;
  Colors: TConvTestColors;
begin
  V := ReadConverterFromString(FConverter, '"ctGreen"', TypeInfo(TConvTestColors), FSerializer);
  Colors := V.AsType<TConvTestColors>;
  AssertTrue('Should contain ctGreen', ctGreen in Colors);
end;

procedure TTestJsonSetNamesConverter.TestRoundTripSet;
var
  S: string;
  V: TValue;
  Original, Restored: TConvTestColors;
begin
  Original := [ctRed, ctGreen, ctYellow];
  S := WriteConverterToString(FConverter, TValue.From<TConvTestColors>(Original), FSerializer);
  V := ReadConverterFromString(FConverter, S, TypeInfo(TConvTestColors), FSerializer);
  Restored := V.AsType<TConvTestColors>;
  AssertTrue('Should contain ctRed', ctRed in Restored);
  AssertTrue('Should contain ctGreen', ctGreen in Restored);
  AssertTrue('Should contain ctYellow', ctYellow in Restored);
  AssertFalse('Should not contain ctBlue', ctBlue in Restored);
end;

{ TTestJsonGUIDConverter }

procedure TTestJsonGUIDConverter.SetUp;
begin
  FConverter := TJsonGUIDConverter.Create;
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonGUIDConverter.TearDown;
begin
  FConverter.Free;
  FSerializer.Free;
end;

procedure TTestJsonGUIDConverter.TestCanConvertGUID;
begin
  AssertTrue('Should convert TGUID', FConverter.CanConvert(TypeInfo(TGUID)));
end;

procedure TTestJsonGUIDConverter.TestCanConvertNonGUID;
begin
  AssertFalse('Should not convert Integer', FConverter.CanConvert(TypeInfo(Integer)));
  AssertFalse('Should not convert string', FConverter.CanConvert(TypeInfo(string)));
end;

procedure TTestJsonGUIDConverter.TestWriteJsonGUID;
var
  G: TGUID;
  S: string;
begin
  G := StringToGUID('{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}');
  S := WriteConverterToString(FConverter, TValue.From<TGUID>(G), FSerializer);
  AssertTrue('Should contain GUID components', Pos('A1B2C3D4', S) > 0);
end;

procedure TTestJsonGUIDConverter.TestReadJsonGUID;
var
  V: TValue;
  G: TGUID;
begin
  V := ReadConverterFromString(FConverter, '"{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}"',
    TypeInfo(TGUID), FSerializer);
  G := V.AsType<TGUID>;
  AssertTrue('Should read GUID correctly', IsEqualGUID(G, StringToGUID('{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}')));
end;

procedure TTestJsonGUIDConverter.TestRoundTripGUID;
var
  G1, G2: TGUID;
  S: string;
  V: TValue;
begin
  G1 := StringToGUID('{DEADBEEF-CAFE-BABE-DEAD-BEEFCAFEBABE}');
  S := WriteConverterToString(FConverter, TValue.From<TGUID>(G1), FSerializer);
  V := ReadConverterFromString(FConverter, S, TypeInfo(TGUID), FSerializer);
  G2 := V.AsType<TGUID>;
  AssertTrue('Round-trip should preserve GUID', IsEqualGUID(G1, G2));
end;

{ TTestJsonListHelperConverter }

procedure TTestJsonListHelperConverter.SetUp;
begin
  FConverter := TJsonListHelperConverter.Create;
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonListHelperConverter.TearDown;
begin
  FConverter.Free;
  FSerializer.Free;
end;

procedure TTestJsonListHelperConverter.TestCanConvertDynArray;
begin
  AssertTrue('Should convert dynamic arrays', FConverter.CanConvert(TypeInfo(TIntegerArray)));
end;

procedure TTestJsonListHelperConverter.TestCanConvertNonArray;
begin
  AssertFalse('Should not convert Integer', FConverter.CanConvert(TypeInfo(Integer)));
  AssertFalse('Should not convert string', FConverter.CanConvert(TypeInfo(string)));
end;

procedure TTestJsonListHelperConverter.TestShouldEditDynArray;
begin
  AssertTrue('Should edit dynamic arrays', TJsonListHelperConverter.ShouldEdit(TypeInfo(TIntegerArray)));
  AssertFalse('Should not edit Integer', TJsonListHelperConverter.ShouldEdit(TypeInfo(Integer)));
end;

procedure TTestJsonListHelperConverter.TestWriteJsonIntArray;
var
  A: TIntegerArray;
  S: string;
begin
  A := [10, 20, 30];
  S := WriteConverterToString(FConverter, TValue.From<TIntegerArray>(A), FSerializer);
  AssertEquals('Should write integer array', '[10,20,30]', S);
end;

procedure TTestJsonListHelperConverter.TestWriteJsonEmptyArray;
var
  A: TIntegerArray;
  S: string;
begin
  A := [];
  S := WriteConverterToString(FConverter, TValue.From<TIntegerArray>(A), FSerializer);
  AssertEquals('Should write empty array', '[]', S);
end;

procedure TTestJsonListHelperConverter.TestReadJsonIntArray;
var
  V: TValue;
  A: TIntegerArray;
begin
  V := ReadConverterFromString(FConverter, '[1,2,3]', TypeInfo(TIntegerArray), FSerializer);
  A := V.AsType<TIntegerArray>;
  AssertEquals('Should have 3 elements', 3, Length(A));
  AssertEquals(1, A[0]);
  AssertEquals(2, A[1]);
  AssertEquals(3, A[2]);
end;

procedure TTestJsonListHelperConverter.TestRoundTripDynArray;
var
  A1, A2: TIntegerArray;
  S: string;
  V: TValue;
begin
  A1 := [100, 200, 300];
  S := WriteConverterToString(FConverter, TValue.From<TIntegerArray>(A1), FSerializer);
  V := ReadConverterFromString(FConverter, S, TypeInfo(TIntegerArray), FSerializer);
  A2 := V.AsType<TIntegerArray>;
  AssertEquals('Should have 3 elements', 3, Length(A2));
  AssertEquals(100, A2[0]);
  AssertEquals(200, A2[1]);
  AssertEquals(300, A2[2]);
end;

{ TTestJsonConverterIntegration }

procedure TTestJsonConverterIntegration.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonConverterIntegration.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonConverterIntegration.TestEnumConverterViaSerializer;
var
  S: string;
  V: TConvTestColor;
begin
  FSerializer.Converters.Add(TJsonEnumNameConverter.Create);
  S := FSerializer.Serialize<TConvTestColor>(ctGreen);
  AssertEquals('Should serialize enum as name', '"ctGreen"', S);
  V := FSerializer.Deserialize<TConvTestColor>('"ctRed"');
  AssertEquals('Should deserialize enum from name', Ord(ctRed), Ord(V));
end;

procedure TTestJsonConverterIntegration.TestSetConverterViaSerializer;
var
  S: string;
  Colors: TConvTestColors;
begin
  FSerializer.Converters.Add(TJsonSetNamesConverter.Create);
  Colors := [ctRed, ctBlue];
  S := FSerializer.Serialize<TConvTestColors>(Colors);
  AssertTrue('Should contain ctRed', Pos('ctRed', S) > 0);
  AssertTrue('Should contain ctBlue', Pos('ctBlue', S) > 0);
end;

procedure TTestJsonConverterIntegration.TestGUIDConverterViaSerializer;
var
  G1, G2: TGUID;
  S: string;
begin
  FSerializer.Converters.Add(TJsonGUIDConverter.Create);
  G1 := StringToGUID('{01020304-0506-0708-090A-0B0C0D0E0F10}');
  S := FSerializer.Serialize<TGUID>(G1);
  G2 := FSerializer.Deserialize<TGUID>(S);
  AssertTrue('GUID round-trip via serializer', IsEqualGUID(G1, G2));
end;

initialization
  RegisterTest(TTestJsonEnumNameConverter);
  RegisterTest(TTestJsonSetNamesConverter);
  RegisterTest(TTestJsonGUIDConverter);
  RegisterTest(TTestJsonListHelperConverter);
  RegisterTest(TTestJsonConverterIntegration);

end.
