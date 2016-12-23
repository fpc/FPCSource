program trtti13;

uses
  TypInfo;

var
  error: LongInt = 0;

const
  RangedInt64Min = $FFFFFFFF00000000;
  RangedInt64Max = $100000000;

  RangedQWordMin = QWord($100000000);
  RangedQWordMax = QWord($8000000000000000);

type
  TRangedInt64 = RangedInt64Min..RangedInt64Max;
  TRangedQWord = RangedQWordMin..RangedQWordMax;

procedure TestOrdTypeInfo(aTI: PTypeInfo; aTypeKind: TTypeKind; aOrdType: TOrdType);
var
  td: PTypeData;
begin
  Inc(error);
  if aTI^.Kind <> aTypeKind then
    Halt(error);

  td := GetTypeData(aTI);

  Inc(error);
  if td^.OrdType <> aOrdType then
    Halt(error);
end;

procedure TestMinMax64(aTI: PTypeInfo; aMin, aMax: Int64);
var
  td: PTypeData;
begin
  td := GetTypeData(aTI);

  Inc(error);
  if (td^.OrdType=otSQWord) and (td^.MinInt64Value<>Int64(aMin)) then
    Halt(error);
  if (td^.OrdType=otUQWord) and (td^.MinQWordValue<>QWord(aMin)) then
    Halt(error);

  Inc(error);
  if (td^.OrdType=otSQWord) and (td^.MaxInt64Value<>Int64(aMax)) then
    Halt(error);
  if (td^.OrdType=otUQWord) and (td^.MaxQWordValue<>QWord(aMax)) then
    Halt(error);
end;

begin
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int8)), tkInteger, otSByte);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int16)), tkInteger, otSWord);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int32)), tkInteger, otSLong);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int64)), tkInt64, otSQWord);

  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt8)), tkInteger, otUByte);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt16)), tkInteger, otUWord);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt32)), tkInteger, otULong);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt64)), tkQWord, otUQWord);

  TestMinMax64(PTypeInfo(TypeInfo(Int64)), Low(Int64), High(Int64));
  TestMinMax64(PTypeInfo(TypeInfo(TRangedInt64)), RangedInt64Min, RangedInt64Max);
  TestMinMax64(PTypeInfo(TypeInfo(QWord)), Int64(Low(QWord)), Int64(High(QWord)));
  TestMinMax64(PTypeInfo(TypeInfo(TRangedQWord)), Int64(RangedQWordMin), Int64(RangedQWordMax));
end.
