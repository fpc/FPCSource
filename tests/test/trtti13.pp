program trtti13;

uses
  TypInfo;

var
  error: LongInt = 0;

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

begin
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int8)), tkInteger, otSByte);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int16)), tkInteger, otSWord);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int32)), tkInteger, otSLong);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(Int64)), tkInt64, otSQWord);

  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt8)), tkInteger, otUByte);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt16)), tkInteger, otUWord);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt32)), tkInteger, otULong);
  TestOrdTypeInfo(PTypeInfo(TypeInfo(UInt64)), tkQWord, otUQWord);

  Inc(error);
  if GetTypeData(PTypeInfo(TypeInfo(Int64)))^.MinInt64Value <> Low(Int64) then
    Halt(error);

  Inc(error);
  if GetTypeData(PTypeInfo(TypeInfo(Int64)))^.MaxInt64Value <> High(Int64) then
    Halt(error);

  Inc(error);
  if GetTypeData(PTypeInfo(TypeInfo(UInt64)))^.MinQWordValue <> Low(QWord) then
    Halt(error);

  Inc(error);
  if GetTypeData(PTypeInfo(TypeInfo(UInt64)))^.MaxQWordValue <> High(QWord) then
    Halt(error);
end.
