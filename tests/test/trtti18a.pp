program trtti18a;

uses
  typinfo;

type
  TTest1 = (
    tt1_1,
    tt1_2,
    tt1_3,
    tt1_4,
    tt1_5,
    tt1_6,
    tt1_7,
    tt1_8
  );
  TTests1 = set of TTest1;

  TTest2 = (
    tt2_1,
    tt2_2,
    tt2_3,
    tt2_4,
    tt2_5,
    tt2_6,
    tt2_7,
    tt2_8,
    tt2_9,
    tt2_10,
    tt2_11,
    tt2_12,
    tt2_13,
    tt2_14,
    tt2_15,
    tt2_16
  );
  TTests2 = set of TTest2;

  TTest3 = (
    tt3_1,
    tt3_2,
    tt3_3,
    tt3_4,
    tt3_5,
    tt3_6,
    tt3_7,
    tt3_8,
    tt3_9,
    tt3_10,
    tt3_11,
    tt3_12,
    tt3_13,
    tt3_14,
    tt3_15,
    tt3_16,
    tt3_17,
    tt3_18,
    tt3_19,
    tt3_20,
    tt3_21,
    tt3_22,
    tt3_23,
    tt3_24
  );
  TTests3 = set of TTest3;

  TTest4 = (
    tt4_1,
    tt4_2,
    tt4_3,
    tt4_4,
    tt4_5,
    tt4_6,
    tt4_7,
    tt4_8,
    tt4_9,
    tt4_10,
    tt4_11,
    tt4_12,
    tt4_13,
    tt4_14,
    tt4_15,
    tt4_16,
    tt4_17,
    tt4_18,
    tt4_19,
    tt4_20,
    tt4_21,
    tt4_22,
    tt4_23,
    tt4_24,
    tt4_25,
    tt4_26,
    tt4_27,
    tt4_28,
    tt4_29,
    tt4_30,
    tt4_31,
    tt4_32
  );
  TTests4 = set of TTest4;

  TTest5 = (
    tt5_1,
    tt5_2,
    tt5_3,
    tt5_4,
    tt5_5,
    tt5_6,
    tt5_7,
    tt5_8,
    tt5_9,
    tt5_10,
    tt5_11,
    tt5_12,
    tt5_13,
    tt5_14,
    tt5_15,
    tt5_16,
    tt5_17,
    tt5_18,
    tt5_19,
    tt5_20,
    tt5_21,
    tt5_22,
    tt5_23,
    tt5_24,
    tt5_25,
    tt5_26,
    tt5_27,
    tt5_28,
    tt5_29,
    tt5_30,
    tt5_31,
    tt5_32,
    tt5_33
  );
  TTests5 = set of TTest5;

var
  code: LongInt = 1;

procedure TestType(aTi: PTypeInfo; aOrdType: TOrdType; aSize: SizeInt);
var
  td: PTypeData;
begin
  td := GetTypeData(aTi);
  Writeln('Testing ', aTi^.Name, ' with ', td^.OrdType, ' and ', td^.SetSize, ' to ', aOrdType, ' and ', aSize);
  if td^.OrdType <> aOrdType then
    Halt(code);
  Inc(code);
  if td^.SetSize <> aSize then
    Halt(code);
  Inc(code);
end;

begin
  TestType(TypeInfo(TTests1), otULong, 4);
  TestType(TypeInfo(TTests2), otULong, 4);
  TestType(TypeInfo(TTests3), otULong, 4);
  TestType(TypeInfo(TTests4), otULong, 4);
  TestType(TypeInfo(TTests5), otUByte, 32);
  Writeln('ok');
end.
