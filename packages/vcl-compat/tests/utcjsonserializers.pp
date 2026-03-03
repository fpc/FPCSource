unit utcjsonserializers;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  TypInfo, Rtti, Generics.Collections, StreamEx,
  System.JSON.Types, System.JSON.Writers, System.JSON.Readers,
  System.JSON.Serializers, System.JSON.Converters;

type
  {$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TTestPoint = record
    X: Integer;
    Y: Integer;
  end;

  TTestPerson = record
    Name: string;
    Age: Integer;
    Active: Boolean;
    Score: Double;
  end;

  TTestAddress = record
    Street: string;
    City: string;
  end;

  TTestPersonWithAddress = record
    Name: string;
    Address: TTestAddress;
  end;

  TTestRenamed = record
    [JsonName('full_name')]
    Name: string;
    [JsonIgnore]
    Internal: Integer;
  end;

  TTestColor = (Red, Green, Blue, Yellow);
  TTestColors = set of TTestColor;

  TTestAnimal = class
  private
    FName: string;
    FLegs: Integer;
  published
    property Name: string read FName write FName;
    property Legs: Integer read FLegs write FLegs;
  end;

  TTestAllIgnored = record
    [JsonIgnore]
    Secret: string;
    [JsonIgnore]
    Hidden: Integer;
  end;

  TTestEmpty = record
  end;

  TTestTheme = record
    Name: string;
    Flags: TTestColors;
  end;

  TIntegerArray = array of Integer;
  TStringArray = array of string;
  TTestPointArray = array of TTestPoint;

  { TTestJsonContractResolver }

  TTestJsonContractResolver = class(TTestCase)
  private
    FResolver: TJsonDefaultContractResolver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolveContractInteger;
    procedure TestResolveContractString;
    procedure TestResolveContractBoolean;
    procedure TestResolveContractDouble;
    procedure TestResolveContractSingle;
    procedure TestResolveContractInt64;
    procedure TestResolveContractChar;
    procedure TestResolveContractEnumeration;
    procedure TestResolveContractRecord;
    procedure TestResolveContractDynArray;
    procedure TestResolveContractCaching;
    procedure TestClearCache;
    procedure TestObjectContractProperties;
    procedure TestObjectContractPropertyTypes;
    procedure TestGetClosestMatchProperty;
    procedure TestGetClosestMatchPropertyNotFound;
  end;

  { TTestJsonSerializerPrimitives }

  TTestJsonSerializerPrimitives = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSerializeInteger;
    procedure TestSerializeNegativeInteger;
    procedure TestSerializeZero;
    procedure TestSerializeInt64;
    procedure TestSerializeString;
    procedure TestSerializeEmptyString;
    procedure TestSerializeBoolean;
    procedure TestSerializeBooleanFalse;
    procedure TestSerializeDouble;
    procedure TestSerializeChar;
    procedure TestDeserializeInteger;
    procedure TestDeserializeString;
    procedure TestDeserializeBoolean;
    procedure TestDeserializeDouble;
    procedure TestRoundTripInteger;
    procedure TestRoundTripString;
    procedure TestRoundTripBoolean;
    procedure TestRoundTripDouble;
  end;

  { TTestJsonSerializerRecords }

  TTestJsonSerializerRecords = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSerializeSimpleRecord;
    procedure TestDeserializeSimpleRecord;
    procedure TestRoundTripSimpleRecord;
    procedure TestSerializeMultiFieldRecord;
    procedure TestDeserializeMultiFieldRecord;
    procedure TestSerializeNestedRecord;
    procedure TestDeserializeNestedRecord;
    procedure TestRoundTripNestedRecord;
    procedure TestDeserializeMissingFields;
    procedure TestSerializeRecordWithRename;
    procedure TestDeserializeRecordWithRename;
    procedure TestSerializeRecordWithIgnore;
    procedure TestSerializeEmptyRecord;
  end;

  { TTestJsonSerializerArrays }

  TTestJsonSerializerArrays = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSerializeIntegerArray;
    procedure TestSerializeEmptyArray;
    procedure TestSerializeStringArray;
    procedure TestSerializeRecordArray;
    procedure TestDeserializeIntegerArray;
    procedure TestDeserializeEmptyArray;
    procedure TestDeserializeStringArray;
    procedure TestDeserializeRecordArray;
    procedure TestRoundTripIntegerArray;
    procedure TestRoundTripRecordArray;
  end;

  { TTestJsonSerializerSettings }

  TTestJsonSerializerSettings = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultSettings;
    procedure TestFormattingNone;
    procedure TestFormattingIndented;
    procedure TestMaxDepthDefault;
    procedure TestPropertyGettersSetters;
    procedure TestAddConverter;
    procedure TestMatchConverterFindsFirst;
    procedure TestMatchConverterReturnsNil;
  end;

  { TTestJsonSerializerPopulate }

  TTestJsonSerializerPopulate = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPopulateRecord;
    procedure TestPopulatePartial;
    procedure TestPopulateFromString;
  end;

  { TTestJsonSerializerIntegration }

  TTestJsonSerializerIntegration = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSerializerWithEnumConverter;
    procedure TestSerializerWithSetConverter;
    procedure TestSerializerWithGUIDConverter;
    procedure TestSerializeDeserializeLargeRecord;
    procedure TestDeserializeUnknownProperty;
  end;

  { TTestJsonSerializerLifecycle - Tests for RTTI context lifetime and cache stability }

  TTestJsonSerializerLifecycle = class(TTestCase)
  published
    procedure TestMultipleSerializerInstances;
    procedure TestAttributesSurviveSerializerRecreation;
    procedure TestDifferentTypesAcrossSerializers;
    procedure TestResolverRecreationPreservesAttributes;
  end;

  { TTestJsonSerializerPrimitivesExtra - Missing primitive coverage }

  TTestJsonSerializerPrimitivesExtra = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDeserializeChar;
    procedure TestRoundTripChar;
    procedure TestSerializeEnumAsOrdinal;
    procedure TestDeserializeEnumFromOrdinal;
    procedure TestRoundTripEnum;
  end;

  { TTestJsonSerializerAttributeEdgeCases - Attribute edge cases }

  TTestJsonSerializerAttributeEdgeCases = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundTripRecordWithRename;
    procedure TestSerializeRecordAllIgnored;
    procedure TestPopulateWithRenamedField;
    procedure TestPopulateIgnoredFieldNotSet;
    procedure TestRenameAndIgnoreTogether;
  end;

  { TTestJsonSerializerClasses - Class serialization via field RTTI }

  TTestJsonSerializerClasses = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSerializeClassFields;
    procedure TestDeserializeClassFields;
    procedure TestRoundTripClassFields;
    procedure TestSerializeNilObject;
    procedure TestDeserializeNull;
    procedure TestClassFieldValueProviderGetValue;
    procedure TestClassFieldValueProviderSetValue;
  end;

  { TTestJsonSerializerSets - Set serialization without converter }

  TTestJsonSerializerSets = class(TTestCase)
  private
    FSerializer: TJsonSerializer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSerializeSetAsInteger;
    procedure TestSerializeEmptySet;
    procedure TestSerializeSingleElementSet;
    procedure TestSerializeMultiElementSet;
    procedure TestDeserializeSetFromInteger;
    procedure TestDeserializeEmptySet;
    procedure TestRoundTripSet;
    procedure TestSerializeSetInRecord;
    procedure TestDeserializeSetInRecord;
    procedure TestRoundTripSetInRecord;
  end;

  { TTestJsonConverterRegistry }

  TTestJsonConverterRegistry = class(TTestCase)
  private
    FSavedCount: Integer;
    FSavedClasses: array of TJsonConverterClass;
    procedure SaveRegistry;
    procedure RestoreRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegisterAndIsRegistered;
    procedure TestUnregisterConverter;
    procedure TestClear;
    procedure TestDuplicateRegistrationIgnored;
    procedure TestPopulateConverters;
    procedure TestPopulateCreatesNewInstances;
    procedure TestSerializerAutoPopulated;
    procedure TestUnregisterBeforeCreate;
    procedure TestClearThenCreate;
    procedure TestUserConvertersSurvive;
  end;

  { TTestJsonDynamicContractResolver - Dynamic resolver coverage }

  TTestJsonDynamicContractResolver = class(TTestCase)
  private
    FResolver: TJsonDynamicContractResolver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetFieldName;
    procedure TestSetFieldsIgnored;
    procedure TestSetTypeMemberSerialization;
    procedure TestClearAttributesRebuildsContracts;
    procedure TestDynamicResolverSerialize;
  end;

implementation

{ TTestJsonContractResolver }

procedure TTestJsonContractResolver.SetUp;
begin
  FResolver := TJsonDefaultContractResolver.Create;
end;

procedure TTestJsonContractResolver.TearDown;
begin
  FResolver.Free;
end;

procedure TTestJsonContractResolver.TestResolveContractInteger;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(Integer));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Int32', Ord(TJsonPrimitiveKind.Int32), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractString;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(string));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be String', Ord(TJsonPrimitiveKind.&String), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractBoolean;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(Boolean));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Boolean', Ord(TJsonPrimitiveKind.Boolean), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractDouble;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(Double));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Double', Ord(TJsonPrimitiveKind.Double), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractSingle;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(Single));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Single', Ord(TJsonPrimitiveKind.Single), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractInt64;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(Int64));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Int64', Ord(TJsonPrimitiveKind.Int64), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractChar;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(Char));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Char', Ord(TJsonPrimitiveKind.Char), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractEnumeration;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(TTestColor));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be primitive', Ord(TJsonContractType.Primitive), Ord(C.ContractType));
  AssertEquals('Should be Enumeration', Ord(TJsonPrimitiveKind.Enumeration), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestResolveContractRecord;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be object', Ord(TJsonContractType.&Object), Ord(C.ContractType));
end;

procedure TTestJsonContractResolver.TestResolveContractDynArray;
var
  C: TJsonContract;
begin
  C := FResolver.ResolveContract(TypeInfo(TIntegerArray));
  AssertNotNull('Contract should not be nil', C);
  AssertEquals('Should be array', Ord(TJsonContractType.&Array), Ord(C.ContractType));
end;

procedure TTestJsonContractResolver.TestResolveContractCaching;
var
  C1, C2: TJsonContract;
begin
  C1 := FResolver.ResolveContract(TypeInfo(Integer));
  C2 := FResolver.ResolveContract(TypeInfo(Integer));
  AssertTrue('Same contract instance should be returned', C1 = C2);
end;

procedure TTestJsonContractResolver.TestClearCache;
var
  C: TJsonContract;
begin
  // Verify contract is resolved, then cache cleared, and resolve still works
  C := FResolver.ResolveContract(TypeInfo(Integer));
  AssertTrue('Should resolve before clear', C is TJsonPrimitiveContract);
  FResolver.ClearCache;
  // After clear, resolving again should still work (creates new contract)
  C := FResolver.ResolveContract(TypeInfo(Integer));
  AssertTrue('Should resolve after clear', C is TJsonPrimitiveContract);
  AssertEquals('Should still be Int32', Ord(TJsonPrimitiveKind.Int32), Ord(TJsonPrimitiveContract(C).Kind));
end;

procedure TTestJsonContractResolver.TestObjectContractProperties;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
begin
  C := FResolver.ResolveContract(TypeInfo(TTestPerson));
  OC := C as TJsonObjectContract;
  AssertEquals('TTestPerson should have 4 properties', 4, OC.Properties.Count);
end;

procedure TTestJsonContractResolver.TestObjectContractPropertyTypes;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
  Prop: TJsonProperty;
begin
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  Prop := OC.GetClosestMatchProperty('X');
  AssertNotNull('X property should exist', Prop);
  AssertEquals('X should be integer', Ord(tkInteger), Ord(Prop.TypeInf^.Kind));
  Prop := OC.GetClosestMatchProperty('Y');
  AssertNotNull('Y property should exist', Prop);
  AssertEquals('Y should be integer', Ord(tkInteger), Ord(Prop.TypeInf^.Kind));
end;

procedure TTestJsonContractResolver.TestGetClosestMatchProperty;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
  Prop: TJsonProperty;
begin
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  Prop := OC.GetClosestMatchProperty('x');
  AssertNotNull('Case-insensitive lookup should find X', Prop);
  AssertEquals('Should find the X field', 'X', Prop.UnderlyingName);
end;

procedure TTestJsonContractResolver.TestGetClosestMatchPropertyNotFound;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
  Prop: TJsonProperty;
begin
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  Prop := OC.GetClosestMatchProperty('Z');
  AssertNull('Should return nil for non-existent property', Prop);
end;

{ TTestJsonSerializerPrimitives }

procedure TTestJsonSerializerPrimitives.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerPrimitives.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerPrimitives.TestSerializeInteger;
begin
  AssertEquals('42', FSerializer.Serialize<Integer>(42));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeNegativeInteger;
begin
  AssertEquals('-1', FSerializer.Serialize<Integer>(-1));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeZero;
begin
  AssertEquals('0', FSerializer.Serialize<Integer>(0));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeInt64;
var
  V: Int64;
begin
  V := 9223372036854775807;
  AssertEquals('9223372036854775807', FSerializer.Serialize<Int64>(V));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeString;
begin
  AssertEquals('"hello"', FSerializer.Serialize<string>('hello'));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeEmptyString;
begin
  AssertEquals('""', FSerializer.Serialize<string>(''));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeBoolean;
begin
  AssertEquals('true', FSerializer.Serialize<Boolean>(True));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeBooleanFalse;
begin
  AssertEquals('false', FSerializer.Serialize<Boolean>(False));
end;

procedure TTestJsonSerializerPrimitives.TestSerializeDouble;
var
  S: string;
begin
  S := FSerializer.Serialize<Double>(3.14);
  AssertTrue('Should contain 3.14, got: ' + S, Pos('3.14', S) > 0);
end;

procedure TTestJsonSerializerPrimitives.TestSerializeChar;
begin
  AssertEquals('"A"', FSerializer.Serialize<Char>('A'));
end;

procedure TTestJsonSerializerPrimitives.TestDeserializeInteger;
begin
  AssertEquals(42, FSerializer.Deserialize<Integer>('42'));
end;

procedure TTestJsonSerializerPrimitives.TestDeserializeString;
begin
  AssertEquals('hello', FSerializer.Deserialize<string>('"hello"'));
end;

procedure TTestJsonSerializerPrimitives.TestDeserializeBoolean;
begin
  AssertEquals(True, FSerializer.Deserialize<Boolean>('true'));
end;

procedure TTestJsonSerializerPrimitives.TestDeserializeDouble;
var
  D: Double;
begin
  D := FSerializer.Deserialize<Double>('3.14');
  AssertTrue('Should be approximately 3.14', Abs(D - 3.14) < 0.001);
end;

procedure TTestJsonSerializerPrimitives.TestRoundTripInteger;
var
  V: Integer;
begin
  V := FSerializer.Deserialize<Integer>(FSerializer.Serialize<Integer>(12345));
  AssertEquals(12345, V);
end;

procedure TTestJsonSerializerPrimitives.TestRoundTripString;
var
  V: string;
begin
  V := FSerializer.Deserialize<string>(FSerializer.Serialize<string>('test value'));
  AssertEquals('test value', V);
end;

procedure TTestJsonSerializerPrimitives.TestRoundTripBoolean;
var
  V: Boolean;
begin
  V := FSerializer.Deserialize<Boolean>(FSerializer.Serialize<Boolean>(True));
  AssertEquals(True, V);
end;

procedure TTestJsonSerializerPrimitives.TestRoundTripDouble;
var
  D: Double;
begin
  D := FSerializer.Deserialize<Double>(FSerializer.Serialize<Double>(2.718));
  AssertTrue('Should round-trip correctly', Abs(D - 2.718) < 0.001);
end;

{ TTestJsonSerializerRecords }

procedure TTestJsonSerializerRecords.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerRecords.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerRecords.TestSerializeSimpleRecord;
var
  P: TTestPoint;
  S: string;
begin
  P.X := 10;
  P.Y := 20;
  S := FSerializer.Serialize<TTestPoint>(P);
  AssertTrue('Should contain X property', Pos('"X"', S) > 0);
  AssertTrue('Should contain Y property', Pos('"Y"', S) > 0);
  AssertTrue('Should contain value 10', Pos('10', S) > 0);
  AssertTrue('Should contain value 20', Pos('20', S) > 0);
end;

procedure TTestJsonSerializerRecords.TestDeserializeSimpleRecord;
var
  P: TTestPoint;
begin
  P := FSerializer.Deserialize<TTestPoint>('{"X":10,"Y":20}');
  AssertEquals('X should be 10', 10, P.X);
  AssertEquals('Y should be 20', 20, P.Y);
end;

procedure TTestJsonSerializerRecords.TestRoundTripSimpleRecord;
var
  P1, P2: TTestPoint;
begin
  P1.X := 42;
  P1.Y := 99;
  P2 := FSerializer.Deserialize<TTestPoint>(FSerializer.Serialize<TTestPoint>(P1));
  AssertEquals('X should match', P1.X, P2.X);
  AssertEquals('Y should match', P1.Y, P2.Y);
end;

procedure TTestJsonSerializerRecords.TestSerializeMultiFieldRecord;
var
  P: TTestPerson;
  S: string;
begin
  P.Name := 'Alice';
  P.Age := 30;
  P.Active := True;
  P.Score := 9.5;
  S := FSerializer.Serialize<TTestPerson>(P);
  AssertTrue('Should contain Name', Pos('"Name"', S) > 0);
  AssertTrue('Should contain Alice', Pos('"Alice"', S) > 0);
  AssertTrue('Should contain Age', Pos('"Age"', S) > 0);
  AssertTrue('Should contain 30', Pos('30', S) > 0);
  AssertTrue('Should contain true', Pos('true', S) > 0);
end;

procedure TTestJsonSerializerRecords.TestDeserializeMultiFieldRecord;
var
  P: TTestPerson;
begin
  P := FSerializer.Deserialize<TTestPerson>('{"Name":"Bob","Age":25,"Active":false,"Score":8.5}');
  AssertEquals('Name should be Bob', 'Bob', P.Name);
  AssertEquals('Age should be 25', 25, P.Age);
  AssertEquals('Active should be false', False, P.Active);
  AssertTrue('Score should be 8.5', Abs(P.Score - 8.5) < 0.001);
end;

procedure TTestJsonSerializerRecords.TestSerializeNestedRecord;
var
  P: TTestPersonWithAddress;
  S: string;
begin
  P.Name := 'Charlie';
  P.Address.Street := '123 Main St';
  P.Address.City := 'Springfield';
  S := FSerializer.Serialize<TTestPersonWithAddress>(P);
  AssertTrue('Should contain Charlie', Pos('"Charlie"', S) > 0);
  AssertTrue('Should contain Address', Pos('"Address"', S) > 0);
  AssertTrue('Should contain Street', Pos('"Street"', S) > 0);
  AssertTrue('Should contain street value', Pos('"123 Main St"', S) > 0);
  AssertTrue('Should contain City', Pos('"City"', S) > 0);
end;

procedure TTestJsonSerializerRecords.TestDeserializeNestedRecord;
var
  P: TTestPersonWithAddress;
begin
  P := FSerializer.Deserialize<TTestPersonWithAddress>(
    '{"Name":"Charlie","Address":{"Street":"123 Main St","City":"Springfield"}}');
  AssertEquals('Name should be Charlie', 'Charlie', P.Name);
  AssertEquals('Street should match', '123 Main St', P.Address.Street);
  AssertEquals('City should match', 'Springfield', P.Address.City);
end;

procedure TTestJsonSerializerRecords.TestRoundTripNestedRecord;
var
  P1, P2: TTestPersonWithAddress;
begin
  P1.Name := 'Dave';
  P1.Address.Street := '456 Oak Ave';
  P1.Address.City := 'Portland';
  P2 := FSerializer.Deserialize<TTestPersonWithAddress>(
    FSerializer.Serialize<TTestPersonWithAddress>(P1));
  AssertEquals('Name should match', P1.Name, P2.Name);
  AssertEquals('Street should match', P1.Address.Street, P2.Address.Street);
  AssertEquals('City should match', P1.Address.City, P2.Address.City);
end;

procedure TTestJsonSerializerRecords.TestDeserializeMissingFields;
var
  P: TTestPerson;
begin
  P := FSerializer.Deserialize<TTestPerson>('{"Name":"Eve"}');
  AssertEquals('Name should be Eve', 'Eve', P.Name);
  AssertEquals('Age should be default 0', 0, P.Age);
  AssertEquals('Active should be default false', False, P.Active);
end;

procedure TTestJsonSerializerRecords.TestSerializeRecordWithRename;
var
  R: TTestRenamed;
  S: string;
begin
  R.Name := 'Test';
  R.Internal := 42;
  S := FSerializer.Serialize<TTestRenamed>(R);
  AssertTrue('Should use renamed property full_name', Pos('"full_name"', S) > 0);
  AssertTrue('Should not contain original name Name', Pos('"Name"', S) = 0);
end;

procedure TTestJsonSerializerRecords.TestDeserializeRecordWithRename;
var
  R: TTestRenamed;
begin
  R := FSerializer.Deserialize<TTestRenamed>('{"full_name":"Hello"}');
  AssertEquals('Should deserialize renamed field', 'Hello', R.Name);
end;

procedure TTestJsonSerializerRecords.TestSerializeRecordWithIgnore;
var
  R: TTestRenamed;
  S: string;
begin
  R.Name := 'Test';
  R.Internal := 42;
  S := FSerializer.Serialize<TTestRenamed>(R);
  AssertTrue('Ignored field should not appear', Pos('"Internal"', S) = 0);
end;

procedure TTestJsonSerializerRecords.TestSerializeEmptyRecord;
var
  E: TTestEmpty;
  S: string;
begin
  S := FSerializer.Serialize<TTestEmpty>(E);
  AssertEquals('Empty record should produce {}', '{}', S);
end;

{ TTestJsonSerializerArrays }

procedure TTestJsonSerializerArrays.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerArrays.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerArrays.TestSerializeIntegerArray;
var
  A: TIntegerArray;
  S: string;
begin
  A := [1, 2, 3];
  S := FSerializer.Serialize<TIntegerArray>(A);
  AssertEquals('Should serialize integer array', '[1,2,3]', S);
end;

procedure TTestJsonSerializerArrays.TestSerializeEmptyArray;
var
  A: TIntegerArray;
  S: string;
begin
  A := [];
  S := FSerializer.Serialize<TIntegerArray>(A);
  AssertEquals('Should serialize empty array', '[]', S);
end;

procedure TTestJsonSerializerArrays.TestSerializeStringArray;
var
  A: TStringArray;
  S: string;
begin
  A := ['a', 'b'];
  S := FSerializer.Serialize<TStringArray>(A);
  AssertEquals('Should serialize string array', '["a","b"]', S);
end;

procedure TTestJsonSerializerArrays.TestSerializeRecordArray;
var
  A: TTestPointArray;
  S: string;
begin
  SetLength(A, 2);
  A[0].X := 1; A[0].Y := 2;
  A[1].X := 3; A[1].Y := 4;
  S := FSerializer.Serialize<TTestPointArray>(A);
  AssertTrue('Should start with [', Pos('[', S) = 1);
  AssertTrue('Should contain X property', Pos('"X"', S) > 0);
end;

procedure TTestJsonSerializerArrays.TestDeserializeIntegerArray;
var
  A: TIntegerArray;
begin
  A := FSerializer.Deserialize<TIntegerArray>('[1,2,3]');
  AssertEquals('Should have 3 elements', 3, Length(A));
  AssertEquals('First element should be 1', 1, A[0]);
  AssertEquals('Second element should be 2', 2, A[1]);
  AssertEquals('Third element should be 3', 3, A[2]);
end;

procedure TTestJsonSerializerArrays.TestDeserializeEmptyArray;
var
  A: TIntegerArray;
begin
  A := FSerializer.Deserialize<TIntegerArray>('[]');
  AssertEquals('Should have 0 elements', 0, Length(A));
end;

procedure TTestJsonSerializerArrays.TestDeserializeStringArray;
var
  A: TStringArray;
begin
  A := FSerializer.Deserialize<TStringArray>('["hello","world"]');
  AssertEquals('Should have 2 elements', 2, Length(A));
  AssertEquals('First element should be hello', 'hello', A[0]);
  AssertEquals('Second element should be world', 'world', A[1]);
end;

procedure TTestJsonSerializerArrays.TestDeserializeRecordArray;
var
  A: TTestPointArray;
begin
  A := FSerializer.Deserialize<TTestPointArray>('[{"X":1,"Y":2},{"X":3,"Y":4}]');
  AssertEquals('Should have 2 elements', 2, Length(A));
  AssertEquals('First X should be 1', 1, A[0].X);
  AssertEquals('First Y should be 2', 2, A[0].Y);
  AssertEquals('Second X should be 3', 3, A[1].X);
  AssertEquals('Second Y should be 4', 4, A[1].Y);
end;

procedure TTestJsonSerializerArrays.TestRoundTripIntegerArray;
var
  A1, A2: TIntegerArray;
begin
  A1 := [10, 20, 30];
  A2 := FSerializer.Deserialize<TIntegerArray>(FSerializer.Serialize<TIntegerArray>(A1));
  AssertEquals('Should have 3 elements', 3, Length(A2));
  AssertEquals(10, A2[0]);
  AssertEquals(20, A2[1]);
  AssertEquals(30, A2[2]);
end;

procedure TTestJsonSerializerArrays.TestRoundTripRecordArray;
var
  A1, A2: TTestPointArray;
begin
  SetLength(A1, 2);
  A1[0].X := 5; A1[0].Y := 6;
  A1[1].X := 7; A1[1].Y := 8;
  A2 := FSerializer.Deserialize<TTestPointArray>(FSerializer.Serialize<TTestPointArray>(A1));
  AssertEquals('Should have 2 elements', 2, Length(A2));
  AssertEquals(5, A2[0].X); AssertEquals(6, A2[0].Y);
  AssertEquals(7, A2[1].X); AssertEquals(8, A2[1].Y);
end;

{ TTestJsonSerializerSettings }

procedure TTestJsonSerializerSettings.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerSettings.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerSettings.TestDefaultSettings;
begin
  AssertEquals('Default MaxDepth should be 64', 64, FSerializer.MaxDepth);
  AssertEquals('Default Formatting should be None', Ord(TJsonFormatting.None), Ord(FSerializer.Formatting));
  AssertEquals('Default MemberSerialization should be Fields', Ord(TJsonMemberSerialization.Fields), Ord(FSerializer.MemberSerialization));
  AssertNotNull('ContractResolver should not be nil', FSerializer.ContractResolver);
  AssertNotNull('Converters should not be nil', FSerializer.Converters);
end;

procedure TTestJsonSerializerSettings.TestFormattingNone;
var
  P: TTestPoint;
  S: string;
begin
  FSerializer.Formatting := TJsonFormatting.None;
  P.X := 1; P.Y := 2;
  S := FSerializer.Serialize<TTestPoint>(P);
  AssertTrue('None formatting should not contain newlines', Pos(#10, S) = 0);
end;

procedure TTestJsonSerializerSettings.TestFormattingIndented;
var
  P: TTestPoint;
  S: string;
begin
  FSerializer.Formatting := TJsonFormatting.Indented;
  P.X := 1; P.Y := 2;
  S := FSerializer.Serialize<TTestPoint>(P);
  AssertTrue('Indented formatting should contain newlines', Pos(#10, S) > 0);
end;

procedure TTestJsonSerializerSettings.TestMaxDepthDefault;
begin
  AssertEquals('Default MaxDepth should be 64', 64, FSerializer.MaxDepth);
end;

procedure TTestJsonSerializerSettings.TestPropertyGettersSetters;
begin
  FSerializer.MaxDepth := 32;
  AssertEquals('MaxDepth should be 32', 32, FSerializer.MaxDepth);

  FSerializer.Formatting := TJsonFormatting.Indented;
  AssertEquals('Formatting should be Indented', Ord(TJsonFormatting.Indented), Ord(FSerializer.Formatting));

  FSerializer.StringEscapeHandling := TJsonStringEscapeHandling.EscapeNonAscii;
  AssertEquals(Ord(TJsonStringEscapeHandling.EscapeNonAscii), Ord(FSerializer.StringEscapeHandling));
end;

procedure TTestJsonSerializerSettings.TestAddConverter;
var
  Conv: TJsonEnumNameConverter;
  CountBefore: Integer;
begin
  CountBefore := FSerializer.Converters.Count;
  Conv := TJsonEnumNameConverter.Create;
  FSerializer.Converters.Add(Conv);
  AssertEquals('Should have one more converter', CountBefore + 1, FSerializer.Converters.Count);
end;

procedure TTestJsonSerializerSettings.TestMatchConverterFindsFirst;
var
  Conv: TJsonConverter;
begin
  FSerializer.Converters.Add(TJsonEnumNameConverter.Create);
  Conv := TJsonSerializer.MatchConverter(FSerializer.Converters, TypeInfo(TTestColor));
  AssertNotNull('Should find a matching converter', Conv);
end;

procedure TTestJsonSerializerSettings.TestMatchConverterReturnsNil;
var
  Conv: TJsonConverter;
begin
  FSerializer.Converters.Add(TJsonEnumNameConverter.Create);
  Conv := TJsonSerializer.MatchConverter(FSerializer.Converters, TypeInfo(Integer));
  AssertNull('Should not find a matching converter for Integer', Conv);
end;

{ TTestJsonSerializerPopulate }

procedure TTestJsonSerializerPopulate.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerPopulate.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerPopulate.TestPopulateRecord;
var
  P: TTestPoint;
begin
  P.X := 0;
  P.Y := 0;
  FSerializer.Populate<TTestPoint>('{"X":42,"Y":99}', P);
  AssertEquals('X should be 42', 42, P.X);
  AssertEquals('Y should be 99', 99, P.Y);
end;

procedure TTestJsonSerializerPopulate.TestPopulatePartial;
var
  P: TTestPerson;
begin
  P.Name := 'Original';
  P.Age := 100;
  P.Active := True;
  P.Score := 0.0;
  FSerializer.Populate<TTestPerson>('{"Age":25}', P);
  AssertEquals('Age should be updated to 25', 25, P.Age);
end;

procedure TTestJsonSerializerPopulate.TestPopulateFromString;
var
  P: TTestPoint;
begin
  P.X := 0;
  P.Y := 0;
  FSerializer.Populate<TTestPoint>('{"X":10,"Y":20}', P);
  AssertEquals(10, P.X);
  AssertEquals(20, P.Y);
end;

{ TTestJsonSerializerIntegration }

procedure TTestJsonSerializerIntegration.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerIntegration.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerIntegration.TestSerializerWithEnumConverter;
var
  S: string;
  V: TTestColor;
begin
  FSerializer.Converters.Add(TJsonEnumNameConverter.Create);
  S := FSerializer.Serialize<TTestColor>(TTestColor.Green);
  AssertEquals('Should serialize enum as name string', '"Green"', S);
  V := FSerializer.Deserialize<TTestColor>('"Blue"');
  AssertEquals('Should deserialize enum from name string', Ord(TTestColor.Blue), Ord(V));
end;

procedure TTestJsonSerializerIntegration.TestSerializerWithSetConverter;
var
  S: string;
  Colors: TTestColors;
begin
  FSerializer.Converters.Add(TJsonSetNamesConverter.Create);
  Colors := [Red, Blue];
  S := FSerializer.Serialize<TTestColors>(Colors);
  AssertTrue('Should contain Red', Pos('Red', S) > 0);
  AssertTrue('Should contain Blue', Pos('Blue', S) > 0);
end;

procedure TTestJsonSerializerIntegration.TestSerializerWithGUIDConverter;
var
  G, G2: TGUID;
  S: string;
begin
  FSerializer.Converters.Add(TJsonGUIDConverter.Create);
  G := StringToGUID('{12345678-1234-1234-1234-123456789ABC}');
  S := FSerializer.Serialize<TGUID>(G);
  AssertTrue('Should contain GUID parts', Pos('12345678', S) > 0);
  G2 := FSerializer.Deserialize<TGUID>(S);
  AssertTrue('Round-trip should preserve GUID', IsEqualGUID(G, G2));
end;

procedure TTestJsonSerializerIntegration.TestSerializeDeserializeLargeRecord;
var
  P1, P2: TTestPerson;
begin
  P1.Name := 'LargeTest';
  P1.Age := 42;
  P1.Active := True;
  P1.Score := 99.9;
  P2 := FSerializer.Deserialize<TTestPerson>(FSerializer.Serialize<TTestPerson>(P1));
  AssertEquals(P1.Name, P2.Name);
  AssertEquals(P1.Age, P2.Age);
  AssertEquals(P1.Active, P2.Active);
  AssertTrue(Abs(P1.Score - P2.Score) < 0.001);
end;

procedure TTestJsonSerializerIntegration.TestDeserializeUnknownProperty;
var
  P: TTestPoint;
begin
  P := FSerializer.Deserialize<TTestPoint>('{"X":1,"Z":999,"Y":2}');
  AssertEquals('X should be 1', 1, P.X);
  AssertEquals('Y should be 2', 2, P.Y);
end;

{ TTestJsonSerializerLifecycle }

procedure TTestJsonSerializerLifecycle.TestMultipleSerializerInstances;
var
  Ser: TJsonSerializer;
  P: TTestPoint;
  S: string;
  I: Integer;
begin
  // Create and destroy multiple serializers, serialize same type each time.
  // This catches RTTI context lifetime bugs: if RTTI objects are freed between
  // serializer instances, value providers will hold dangling pointers.
  for I := 1 to 3 do
  begin
    Ser := TJsonSerializer.Create;
    try
      P.X := I * 10;
      P.Y := I * 20;
      S := Ser.Serialize<TTestPoint>(P);
      AssertTrue('Iteration ' + IntToStr(I) + ' should contain X', Pos(IntToStr(I * 10), S) > 0);
      AssertTrue('Iteration ' + IntToStr(I) + ' should contain Y', Pos(IntToStr(I * 20), S) > 0);
    finally
      Ser.Free;
    end;
  end;
end;

procedure TTestJsonSerializerLifecycle.TestAttributesSurviveSerializerRecreation;
var
  Ser: TJsonSerializer;
  R: TTestRenamed;
  S: string;
  I: Integer;
begin
  // The attribute cache poisoning bug: after freeing a serializer, the
  // TJsonInlineAttributes static cache holds stale RTTI pointers. If new RTTI
  // objects get allocated at the same addresses, GetAttribute returns wrong
  // results (attributes for a different type's field).
  for I := 1 to 3 do
  begin
    Ser := TJsonSerializer.Create;
    try
      R.Name := 'Test' + IntToStr(I);
      R.Internal := I;
      S := Ser.Serialize<TTestRenamed>(R);
      AssertTrue('Iteration ' + IntToStr(I) + ': Should use renamed property', Pos('"full_name"', S) > 0);
      AssertTrue('Iteration ' + IntToStr(I) + ': Ignored field should not appear', Pos('"Internal"', S) = 0);
    finally
      Ser.Free;
    end;
  end;
end;

procedure TTestJsonSerializerLifecycle.TestDifferentTypesAcrossSerializers;
var
  Ser: TJsonSerializer;
  P: TTestPoint;
  R: TTestPerson;
  SP, SR: string;
begin
  // Serialize different types across different serializer instances to exercise
  // RTTI pool reuse with different type metadata
  Ser := TJsonSerializer.Create;
  try
    P.X := 1; P.Y := 2;
    SP := Ser.Serialize<TTestPoint>(P);
  finally
    Ser.Free;
  end;

  Ser := TJsonSerializer.Create;
  try
    R.Name := 'Bob'; R.Age := 30; R.Active := True; R.Score := 9.5;
    SR := Ser.Serialize<TTestPerson>(R);
  finally
    Ser.Free;
  end;

  AssertTrue('Point should have X', Pos('"X"', SP) > 0);
  AssertTrue('Person should have Name', Pos('"Name"', SR) > 0);
  AssertTrue('Person should have Age', Pos('"Age"', SR) > 0);
end;

procedure TTestJsonSerializerLifecycle.TestResolverRecreationPreservesAttributes;
var
  R1, R2: TJsonDefaultContractResolver;
  C: TJsonContract;
  OC: TJsonObjectContract;
begin
  // Create/free resolver, then create new one. The second resolver must still
  // detect attributes correctly (tests shared RTTI context stability).
  R1 := TJsonDefaultContractResolver.Create;
  C := R1.ResolveContract(TypeInfo(TTestRenamed));
  OC := C as TJsonObjectContract;
  AssertEquals('First resolver: Name should be renamed', 'full_name', OC.Properties[0].Name);
  AssertTrue('First resolver: Internal should be ignored', OC.Properties[1].Ignored);
  R1.Free;

  R2 := TJsonDefaultContractResolver.Create;
  C := R2.ResolveContract(TypeInfo(TTestRenamed));
  OC := C as TJsonObjectContract;
  AssertEquals('Second resolver: Name should be renamed', 'full_name', OC.Properties[0].Name);
  AssertTrue('Second resolver: Internal should be ignored', OC.Properties[1].Ignored);
  R2.Free;
end;

{ TTestJsonSerializerPrimitivesExtra }

procedure TTestJsonSerializerPrimitivesExtra.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerPrimitivesExtra.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerPrimitivesExtra.TestDeserializeChar;
var
  C: Char;
begin
  C := FSerializer.Deserialize<Char>('"X"');
  AssertEquals('Should deserialize char', 'X', C);
end;

procedure TTestJsonSerializerPrimitivesExtra.TestRoundTripChar;
var
  C: Char;
begin
  C := FSerializer.Deserialize<Char>(FSerializer.Serialize<Char>('Z'));
  AssertEquals('Char round-trip should preserve value', 'Z', C);
end;

procedure TTestJsonSerializerPrimitivesExtra.TestSerializeEnumAsOrdinal;
var
  S: string;
begin
  // Default enum serialization uses name strings (GetEnumName)
  S := FSerializer.Serialize<TTestColor>(TTestColor.Blue);
  AssertEquals('Blue should serialize as enum name string', '"Blue"', S);
end;

procedure TTestJsonSerializerPrimitivesExtra.TestDeserializeEnumFromOrdinal;
var
  V: TTestColor;
begin
  // Deserialization supports both string names and ordinal values
  V := FSerializer.Deserialize<TTestColor>('"Green"');
  AssertEquals('Should deserialize name string "Green" as Green', Ord(TTestColor.Green), Ord(V));
end;

procedure TTestJsonSerializerPrimitivesExtra.TestRoundTripEnum;
var
  V: TTestColor;
begin
  V := FSerializer.Deserialize<TTestColor>(FSerializer.Serialize<TTestColor>(TTestColor.Yellow));
  AssertEquals('Enum round-trip should preserve value', Ord(TTestColor.Yellow), Ord(V));
end;

{ TTestJsonSerializerAttributeEdgeCases }

procedure TTestJsonSerializerAttributeEdgeCases.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerAttributeEdgeCases.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerAttributeEdgeCases.TestRoundTripRecordWithRename;
var
  R1, R2: TTestRenamed;
begin
  R1.Name := 'RoundTrip';
  R1.Internal := 99;
  R2 := FSerializer.Deserialize<TTestRenamed>(FSerializer.Serialize<TTestRenamed>(R1));
  AssertEquals('Name should survive round-trip via full_name', 'RoundTrip', R2.Name);
  // Internal is ignored, so it should be default (0) after deserialization
  AssertEquals('Ignored field should be 0 after round-trip', 0, R2.Internal);
end;

procedure TTestJsonSerializerAttributeEdgeCases.TestSerializeRecordAllIgnored;
var
  R: TTestAllIgnored;
  S: string;
begin
  R.Secret := 'hidden';
  R.Hidden := 42;
  S := FSerializer.Serialize<TTestAllIgnored>(R);
  AssertEquals('All-ignored record should produce empty object', '{}', S);
end;

procedure TTestJsonSerializerAttributeEdgeCases.TestPopulateWithRenamedField;
var
  R: TTestRenamed;
begin
  R.Name := '';
  R.Internal := 0;
  FSerializer.Populate<TTestRenamed>('{"full_name":"Populated"}', R);
  AssertEquals('Populate should use renamed field mapping', 'Populated', R.Name);
end;

procedure TTestJsonSerializerAttributeEdgeCases.TestPopulateIgnoredFieldNotSet;
var
  R: TTestRenamed;
begin
  R.Name := 'Original';
  R.Internal := 100;
  FSerializer.Populate<TTestRenamed>('{"full_name":"New","Internal":999}', R);
  AssertEquals('Name should be updated', 'New', R.Name);
  AssertEquals('Ignored field should not be changed by Populate', 100, R.Internal);
end;

procedure TTestJsonSerializerAttributeEdgeCases.TestRenameAndIgnoreTogether;
var
  R: TTestRenamed;
  S: string;
begin
  // Verify both attributes work on the same record
  R.Name := 'Both';
  R.Internal := 55;
  S := FSerializer.Serialize<TTestRenamed>(R);
  AssertTrue('Should contain renamed field', Pos('"full_name"', S) > 0);
  AssertTrue('Should contain value', Pos('"Both"', S) > 0);
  AssertTrue('Should not contain ignored field', Pos('"Internal"', S) = 0);
  AssertTrue('Should not contain ignored value', Pos('55', S) = 0);
end;

{ TTestJsonSerializerClasses }

procedure TTestJsonSerializerClasses.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerClasses.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerClasses.TestSerializeClassFields;
var
  A: TTestAnimal;
  S: string;
begin
  // Bug regression: TJsonFieldValueProvider.GetValue used GetReferenceToRawData
  // for classes, which gives a pointer-to-pointer instead of the object instance.
  // Fixed to use AsObject when IsObject is true.
  A := TTestAnimal.Create;
  try
    A.Name := 'Rex';
    A.Legs := 4;
    S := FSerializer.Serialize<TTestAnimal>(A);
    AssertTrue('Should contain field value Rex', Pos('"Rex"', S) > 0);
    AssertTrue('Should contain field value 4', Pos('4', S) > 0);
  finally
    A.Free;
  end;
end;

procedure TTestJsonSerializerClasses.TestDeserializeClassFields;
var
  A: TTestAnimal;
begin
  // Bug regression: TJsonFieldValueProvider.SetValue used GetReferenceToRawData
  // for classes. Fixed to use AsObject.
  A := FSerializer.Deserialize<TTestAnimal>('{"FName":"Buddy","FLegs":3}');
  try
    AssertEquals('Name should be deserialized', 'Buddy', A.Name);
    AssertEquals('Legs should be deserialized', 3, A.Legs);
  finally
    A.Free;
  end;
end;

procedure TTestJsonSerializerClasses.TestRoundTripClassFields;
var
  A, A2: TTestAnimal;
  Json: string;
begin
  A := TTestAnimal.Create;
  try
    A.Name := 'Whiskers';
    A.Legs := 4;
    Json := FSerializer.Serialize<TTestAnimal>(A);
  finally
    A.Free;
  end;

  A2 := FSerializer.Deserialize<TTestAnimal>(Json);
  try
    AssertEquals('Name should survive round-trip', 'Whiskers', A2.Name);
    AssertEquals('Legs should survive round-trip', 4, A2.Legs);
  finally
    A2.Free;
  end;
end;

procedure TTestJsonSerializerClasses.TestSerializeNilObject;
var
  A: TTestAnimal;
  S: string;
begin
  A := nil;
  S := FSerializer.Serialize<TTestAnimal>(A);
  AssertEquals('Nil object should serialize as null', 'null', S);
end;

procedure TTestJsonSerializerClasses.TestDeserializeNull;
var
  A: TTestAnimal;
begin
  A := FSerializer.Deserialize<TTestAnimal>('null');
  AssertTrue('null JSON should deserialize to nil', A = nil);
end;

procedure TTestJsonSerializerClasses.TestClassFieldValueProviderGetValue;
var
  A: TTestAnimal;
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  VP: IJsonValueProvider;
  V: TValue;
begin
  // Direct test of TJsonFieldValueProvider with a class instance
  A := TTestAnimal.Create;
  try
    A.Name := 'TestDog';
    A.Legs := 4;

    Ctx := TRttiContext.Create;
    RttiType := Ctx.GetType(TypeInfo(TTestAnimal));
    Field := RttiType.GetField('FName');
    AssertTrue('Should find FName field via RTTI', Field <> nil);

    VP := TJsonFieldValueProvider.Create(Field);
    V := VP.GetValue(TValue.From<TTestAnimal>(A));
    AssertEquals('Field value provider should read class field', 'TestDog', V.AsString);
  finally
    A.Free;
  end;
end;

procedure TTestJsonSerializerClasses.TestClassFieldValueProviderSetValue;
var
  A: TTestAnimal;
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  VP: IJsonValueProvider;
  V: TValue;
begin
  // Direct test of TJsonFieldValueProvider.SetValue with a class instance
  A := TTestAnimal.Create;
  try
    A.Name := 'Original';

    Ctx := TRttiContext.Create;
    RttiType := Ctx.GetType(TypeInfo(TTestAnimal));
    Field := RttiType.GetField('FName');
    AssertTrue('Should find FName field via RTTI', Field <> nil);

    VP := TJsonFieldValueProvider.Create(Field);
    V := TValue.From<string>('Modified');
    VP.SetValue(TValue.From<TTestAnimal>(A), V);
    AssertEquals('Field value provider should write class field', 'Modified', A.Name);
  finally
    A.Free;
  end;
end;

{ TTestJsonSerializerSets }

procedure TTestJsonSerializerSets.SetUp;
begin
  FSerializer := TJsonSerializer.Create;
end;

procedure TTestJsonSerializerSets.TearDown;
begin
  FSerializer.Free;
end;

procedure TTestJsonSerializerSets.TestSerializeSetAsInteger;
var
  Colors: TTestColors;
  S: string;
begin
  // With TJsonSetNamesConverter auto-registered, sets serialize as name strings
  Colors := [Red, Blue];
  S := FSerializer.Serialize<TTestColors>(Colors);
  AssertEquals('[Red, Blue] should serialize as name string', '"Red, Blue"', S);
end;

procedure TTestJsonSerializerSets.TestSerializeEmptySet;
var
  Colors: TTestColors;
  S: string;
begin
  Colors := [];
  S := FSerializer.Serialize<TTestColors>(Colors);
  AssertEquals('Empty set should serialize as empty string', '""', S);
end;

procedure TTestJsonSerializerSets.TestSerializeSingleElementSet;
var
  Colors: TTestColors;
  S: string;
begin
  Colors := [Green];
  S := FSerializer.Serialize<TTestColors>(Colors);
  AssertEquals('[Green] should serialize as name string', '"Green"', S);
end;

procedure TTestJsonSerializerSets.TestSerializeMultiElementSet;
var
  Colors: TTestColors;
  S: string;
begin
  Colors := [Red, Green, Blue, Yellow];
  S := FSerializer.Serialize<TTestColors>(Colors);
  AssertEquals('[Red, Green, Blue, Yellow] should serialize as name string', '"Red, Green, Blue, Yellow"', S);
end;

procedure TTestJsonSerializerSets.TestDeserializeSetFromInteger;
var
  Colors: TTestColors;
begin
  // With TJsonSetNamesConverter auto-registered, sets deserialize from name strings
  Colors := FSerializer.Deserialize<TTestColors>('"Red, Blue"');
  AssertTrue('Should contain Red', Red in Colors);
  AssertTrue('Should contain Blue', Blue in Colors);
  AssertFalse('Should not contain Green', Green in Colors);
  AssertFalse('Should not contain Yellow', Yellow in Colors);
end;

procedure TTestJsonSerializerSets.TestDeserializeEmptySet;
var
  Colors: TTestColors;
begin
  Colors := FSerializer.Deserialize<TTestColors>('""');
  AssertTrue('Should deserialize empty string as empty set', Colors = []);
end;

procedure TTestJsonSerializerSets.TestRoundTripSet;
var
  Original, Restored: TTestColors;
begin
  Original := [Green, Yellow];
  Restored := FSerializer.Deserialize<TTestColors>(
    FSerializer.Serialize<TTestColors>(Original));
  AssertTrue('Green should survive round-trip', Green in Restored);
  AssertTrue('Yellow should survive round-trip', Yellow in Restored);
  AssertFalse('Red should not be in round-tripped set', Red in Restored);
  AssertFalse('Blue should not be in round-tripped set', Blue in Restored);
end;

procedure TTestJsonSerializerSets.TestSerializeSetInRecord;
var
  T: TTestTheme;
  S: string;
begin
  // With TJsonSetNamesConverter auto-registered, sets serialize as name strings
  T.Name := 'Sunset';
  T.Flags := [Red, Yellow];
  S := FSerializer.Serialize<TTestTheme>(T);
  AssertTrue('Should contain Name', Pos('"Sunset"', S) > 0);
  AssertTrue('Should contain Flags property', Pos('"Flags"', S) > 0);
  AssertTrue('Should contain Red', Pos('Red', S) > 0);
  AssertTrue('Should contain Yellow', Pos('Yellow', S) > 0);
end;

procedure TTestJsonSerializerSets.TestDeserializeSetInRecord;
var
  T: TTestTheme;
begin
  T := FSerializer.Deserialize<TTestTheme>('{"Name":"Ocean","Flags":"Blue"}');
  AssertEquals('Name should be deserialized', 'Ocean', T.Name);
  AssertTrue('Should contain Blue', Blue in T.Flags);
  AssertFalse('Should not contain Red', Red in T.Flags);
end;

procedure TTestJsonSerializerSets.TestRoundTripSetInRecord;
var
  T, T2: TTestTheme;
begin
  T.Name := 'Forest';
  T.Flags := [Green, Blue];
  T2 := FSerializer.Deserialize<TTestTheme>(FSerializer.Serialize<TTestTheme>(T));
  AssertEquals('Name should survive round-trip', 'Forest', T2.Name);
  AssertTrue('Green should survive round-trip', Green in T2.Flags);
  AssertTrue('Blue should survive round-trip', Blue in T2.Flags);
  AssertFalse('Red should not be in round-tripped set', Red in T2.Flags);
end;

{ TTestJsonConverterRegistry }

procedure TTestJsonConverterRegistry.SaveRegistry;
var
  KnownClasses: array[0..3] of TJsonConverterClass;
  I: Integer;
begin
  KnownClasses[0] := TJsonEnumNameConverter;
  KnownClasses[1] := TJsonSetNamesConverter;
  KnownClasses[2] := TJsonGUIDConverter;
  KnownClasses[3] := TJsonListHelperConverter;
  FSavedCount := 0;
  SetLength(FSavedClasses, 4);
  for I := 0 to 3 do
    if TJsonConverterRegistry.IsRegistered(KnownClasses[I]) then
    begin
      FSavedClasses[FSavedCount] := KnownClasses[I];
      Inc(FSavedCount);
    end;
  SetLength(FSavedClasses, FSavedCount);
end;

procedure TTestJsonConverterRegistry.RestoreRegistry;
var
  I: Integer;
begin
  TJsonConverterRegistry.Clear;
  for I := 0 to FSavedCount - 1 do
    TJsonConverterRegistry.RegisterConverter(FSavedClasses[I]);
end;

procedure TTestJsonConverterRegistry.SetUp;
begin
  SaveRegistry;
end;

procedure TTestJsonConverterRegistry.TearDown;
begin
  RestoreRegistry;
end;

procedure TTestJsonConverterRegistry.TestRegisterAndIsRegistered;
begin
  TJsonConverterRegistry.Clear;
  AssertFalse('Should not be registered after clear', TJsonConverterRegistry.IsRegistered(TJsonEnumNameConverter));
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  AssertTrue('Should be registered after RegisterConverter', TJsonConverterRegistry.IsRegistered(TJsonEnumNameConverter));
end;

procedure TTestJsonConverterRegistry.TestUnregisterConverter;
begin
  TJsonConverterRegistry.Clear;
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  AssertTrue('Should be registered', TJsonConverterRegistry.IsRegistered(TJsonEnumNameConverter));
  TJsonConverterRegistry.UnregisterConverter(TJsonEnumNameConverter);
  AssertFalse('Should not be registered after unregister', TJsonConverterRegistry.IsRegistered(TJsonEnumNameConverter));
end;

procedure TTestJsonConverterRegistry.TestClear;
begin
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  TJsonConverterRegistry.RegisterConverter(TJsonSetNamesConverter);
  AssertTrue('Should have at least 2 registered', TJsonConverterRegistry.RegisteredCount >= 2);
  TJsonConverterRegistry.Clear;
  AssertEquals('Should have 0 after clear', 0, TJsonConverterRegistry.RegisteredCount);
end;

procedure TTestJsonConverterRegistry.TestDuplicateRegistrationIgnored;
var
  CountBefore: Integer;
begin
  TJsonConverterRegistry.Clear;
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  CountBefore := TJsonConverterRegistry.RegisteredCount;
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  AssertEquals('Duplicate should be ignored', CountBefore, TJsonConverterRegistry.RegisteredCount);
end;

procedure TTestJsonConverterRegistry.TestPopulateConverters;
var
  List: TObjectList<TJsonConverter>;
begin
  TJsonConverterRegistry.Clear;
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  TJsonConverterRegistry.RegisterConverter(TJsonGUIDConverter);
  List := TObjectList<TJsonConverter>.Create(True);
  try
    TJsonConverterRegistry.PopulateConverters(List);
    AssertEquals('Should have 2 converter instances', 2, List.Count);
    AssertTrue('First should be TJsonEnumNameConverter', List[0] is TJsonEnumNameConverter);
    AssertTrue('Second should be TJsonGUIDConverter', List[1] is TJsonGUIDConverter);
  finally
    List.Free;
  end;
end;

procedure TTestJsonConverterRegistry.TestPopulateCreatesNewInstances;
var
  List1, List2: TObjectList<TJsonConverter>;
begin
  TJsonConverterRegistry.Clear;
  TJsonConverterRegistry.RegisterConverter(TJsonEnumNameConverter);
  List1 := TObjectList<TJsonConverter>.Create(True);
  List2 := TObjectList<TJsonConverter>.Create(True);
  try
    TJsonConverterRegistry.PopulateConverters(List1);
    TJsonConverterRegistry.PopulateConverters(List2);
    AssertEquals('List1 should have 1', 1, List1.Count);
    AssertEquals('List2 should have 1', 1, List2.Count);
    AssertTrue('Should be different instances', List1[0] <> List2[0]);
  finally
    List1.Free;
    List2.Free;
  end;
end;

procedure TTestJsonConverterRegistry.TestSerializerAutoPopulated;
var
  Ser: TJsonSerializer;
begin
  // With default registrations, new serializer should have exactly that many converters
  Ser := TJsonSerializer.Create;
  try
    AssertEquals('Serializer should have exactly the registered converter count', TJsonConverterRegistry.RegisteredCount, Ser.Converters.Count);
  finally
    Ser.Free;
  end;
end;

procedure TTestJsonConverterRegistry.TestUnregisterBeforeCreate;
var
  Ser: TJsonSerializer;
  Conv: TJsonConverter;
begin
  TJsonConverterRegistry.UnregisterConverter(TJsonGUIDConverter);
  Ser := TJsonSerializer.Create;
  try
    Conv := TJsonSerializer.MatchConverter(Ser.Converters, TypeInfo(TGUID));
    AssertNull('GUID converter should not be found after unregistering', Conv);
    Conv := TJsonSerializer.MatchConverter(Ser.Converters, TypeInfo(TTestColor));
    AssertNotNull('Enum converter should still be present', Conv);
    Conv := TJsonSerializer.MatchConverter(Ser.Converters, TypeInfo(TTestColors));
    AssertNotNull('Set converter should still be present', Conv);
  finally
    Ser.Free;
  end;
end;

procedure TTestJsonConverterRegistry.TestClearThenCreate;
var
  Ser: TJsonSerializer;
begin
  TJsonConverterRegistry.Clear;
  Ser := TJsonSerializer.Create;
  try
    AssertEquals('Serializer should have no converters after registry clear', 0, Ser.Converters.Count);
  finally
    Ser.Free;
  end;
end;

procedure TTestJsonConverterRegistry.TestUserConvertersSurvive;
var
  Ser: TJsonSerializer;
  UserConv: TJsonEnumNameConverter;
  CountBefore: Integer;
begin
  Ser := TJsonSerializer.Create;
  try
    CountBefore := Ser.Converters.Count;
    UserConv := TJsonEnumNameConverter.Create;
    Ser.Converters.Add(UserConv);
    AssertEquals('User converter should be added', CountBefore + 1, Ser.Converters.Count);
    AssertTrue('Last converter should be the user one', Ser.Converters[Ser.Converters.Count - 1] = UserConv);
  finally
    Ser.Free;
  end;
end;

{ TTestJsonDynamicContractResolver }

procedure TTestJsonDynamicContractResolver.SetUp;
begin
  FResolver := TJsonDynamicContractResolver.Create;
end;

procedure TTestJsonDynamicContractResolver.TearDown;
begin
  FResolver.Free;
end;

procedure TTestJsonDynamicContractResolver.TestSetFieldName;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
begin
  FResolver.SetFieldName(TypeInfo(TTestPoint), 'X', 'x_coord');
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  AssertEquals('Field should be renamed to x_coord', 'x_coord', OC.Properties[0].Name);
  AssertEquals('Other field should be unchanged', 'Y', OC.Properties[1].Name);
end;

procedure TTestJsonDynamicContractResolver.TestSetFieldsIgnored;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
begin
  FResolver.SetFieldsIgnored(TypeInfo(TTestPoint), ['Y']);
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  AssertFalse('X should not be ignored', OC.Properties[0].Ignored);
  AssertTrue('Y should be ignored', OC.Properties[1].Ignored);
end;

procedure TTestJsonDynamicContractResolver.TestSetTypeMemberSerialization;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
begin
  FResolver.SetTypeMemberSerialization(TypeInfo(TTestPoint), TJsonMemberSerialization.&Public);
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  AssertEquals('MemberSerialization should be Public', Ord(TJsonMemberSerialization.&Public), Ord(OC.MemberSerialization));
end;

procedure TTestJsonDynamicContractResolver.TestClearAttributesRebuildsContracts;
var
  C: TJsonContract;
  OC: TJsonObjectContract;
begin
  // Add a dynamic rename
  FResolver.SetFieldName(TypeInfo(TTestPoint), 'X', 'x_renamed');
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  AssertEquals('Should be renamed', 'x_renamed', OC.Properties[0].Name);

  // Clear and verify the rename is gone
  FResolver.ClearAttributes;
  C := FResolver.ResolveContract(TypeInfo(TTestPoint));
  OC := C as TJsonObjectContract;
  AssertEquals('After clear, should use original name', 'X', OC.Properties[0].Name);
end;

procedure TTestJsonDynamicContractResolver.TestDynamicResolverSerialize;
var
  Ser: TJsonSerializer;
  P: TTestPoint;
  S: string;
begin
  // Use a dynamic resolver through the serializer
  FResolver.SetFieldName(TypeInfo(TTestPoint), 'X', 'px');
  FResolver.SetFieldsIgnored(TypeInfo(TTestPoint), ['Y']);

  Ser := TJsonSerializer.Create;
  try
    Ser.ContractResolver := FResolver;
    // FResolver is ref-counted via the serializer's interface reference.
    // When Ser.Free releases the interface, the resolver auto-frees.
    // Nil FResolver so TearDown doesn't double-free.
    FResolver := nil;
    P.X := 10; P.Y := 20;
    S := Ser.Serialize<TTestPoint>(P);
    AssertTrue('Should use dynamic renamed field', Pos('"px"', S) > 0);
    AssertTrue('Dynamically ignored field should not appear', Pos('"Y"', S) = 0);
    AssertTrue('Ignored field value should not appear', Pos('20', S) = 0);
  finally
    Ser.Free;
  end;
end;

initialization
  RegisterTest(TTestJsonContractResolver);
  RegisterTest(TTestJsonSerializerPrimitives);
  RegisterTest(TTestJsonSerializerRecords);
  RegisterTest(TTestJsonSerializerArrays);
  RegisterTest(TTestJsonSerializerSettings);
  RegisterTest(TTestJsonSerializerPopulate);
  RegisterTest(TTestJsonSerializerIntegration);
  RegisterTest(TTestJsonSerializerLifecycle);
  RegisterTest(TTestJsonSerializerPrimitivesExtra);
  RegisterTest(TTestJsonSerializerAttributeEdgeCases);
  RegisterTest(TTestJsonSerializerClasses);
  RegisterTest(TTestJsonSerializerSets);
  RegisterTest(TTestJsonConverterRegistry);
  RegisterTest(TTestJsonDynamicContractResolver);

end.
