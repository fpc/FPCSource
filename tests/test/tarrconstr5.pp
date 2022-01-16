program tarrconstr5;

{$mode objfpc}{$H+}

type
  TSet = set of (One, Two, Three);

  TLongIntArray = array of LongInt;
  TLongIntArrayArray = array of TLongIntArray;
  TSetArray  =array of TSet;
  TSetArrayArray = array of TSetArray;
  TStringArray = array of String;
  TStringArrayArray = array of TStringArray;
  TStringArrayArray2 = specialize TArray<specialize TArray<String>>;

var
  code: LongInt = 1;

procedure CheckLongInt(aActual, aExpected: array of LongInt);
var
  i: LongInt;
begin
  if Length(aActual) <> Length(aExpected) then
    Halt(code);
  Inc(code);
  for i := Low(aActual) to High(aActual) do
    if aActual[i] <> aExpected[i] then
      Halt(code);
  Inc(code);
end;

procedure CheckLongInt(aActual, aExpected: array of TLongIntArray);
var
  i: LongInt;
begin
  if Length(aActual) <> Length(aExpected) then
    Halt(code);
  Inc(code);
  for i := Low(aActual) to High(aActual) do
    CheckLongInt(aActual[i], aExpected[i]);
end;

procedure CheckSet(aActual, aExpected: array of TSet);
var
  i: LongInt;
begin
  if Length(aActual) <> Length(aExpected) then
    Halt(code);
  Inc(code);
  for i := Low(aActual) to High(aActual) do
    if aActual[i] <> aExpected[i] then
      Halt(code);
  Inc(code);
end;

procedure CheckSet(aActual, aExpected: array of TSetArray);
var
  i: LongInt;
begin
  if Length(aActual) <> Length(aExpected) then
    Halt(code);
  Inc(code);
  for i := Low(aActual) to High(aActual) do
    CheckSet(aActual[i], aExpected[i]);
end;

procedure CheckString(aActual, aExpected: array of String);
var
  i: LongInt;
begin
  if Length(aActual) <> Length(aExpected) then
    Halt(code);
  Inc(code);
  for i := Low(aActual) to High(aActual) do
    if aActual[i] <> aExpected[i] then
      Halt(code);
  Inc(code);
end;

procedure CheckString(aActual, aExpected: array of TStringArray);
var
  i: LongInt;
begin
  if Length(aActual) <> Length(aExpected) then
    Halt(code);
  Inc(code);
  for i := Low(aActual) to High(aActual) do
    CheckString(aActual[i], aExpected[i]);
end;

var
  la1: TLongIntArray;
  la2: array of LongInt;
  la3: specialize TArray<LongInt>;
  laa1: TLongIntArrayArray;
  laa2: array of TLongIntArray;
  laa3: array of array of LongInt;
  sa1: TSetArray;
  sa2: array of TSet;
  sa3: specialize TArray<TSet>;
  saa1: array of TSetArray;
  saa2: TSetArrayArray;
  stra1: TStringArray;
  stra2: array of String;
  stra3: specialize TArray<String>;
  straa1: array of TStringArray;
  straa2: TStringArrayArray;
  straa3: specialize TArray<TStringArray>;
  straa4: specialize TArray<specialize TArray<String>>;
begin
  la1 := [];
  CheckLongInt(la1, []);

  laa1 := [[]];
  CheckLongInt(laa1, [[]]);
  CheckLongInt(laa1, [nil]);

  laa1 := [nil];
  CheckLongInt(laa1, [[]]);
  CheckLongInt(laa1, [nil]);

  laa1 := [[], nil];
  CheckLongInt(laa1, [[], nil]);
  CheckLongInt(laa1, [[], []]);
  CheckLongInt(laa1, [nil, nil]);
  CheckLongInt(laa1, [nil, []]);

  laa1 := [nil, nil];
  CheckLongInt(laa1, [nil, nil]);
  CheckLongInt(laa1, [[], []]);
  CheckLongInt(laa1, [nil, []]);
  CheckLongInt(laa1, [[], nil]);

  laa1 := [[], []];
  CheckLongInt(laa1, [[], []]);
  CheckLongInt(laa1, [nil, nil]);
  CheckLongInt(laa1, [nil, []]);
  CheckLongInt(laa1, [[], nil]);

  laa1 := [nil, [], nil];
  CheckLongInt(laa1, [nil, [], nil]);
  CheckLongInt(laa1, [nil, nil, nil]);
  CheckLongInt(laa1, [[], [], []]);
  CheckLongInt(laa1, [[], nil, []]);

  la1 := [1, 3, 5];
  CheckLongInt(la1, [1, 3, 5]);

  la2 := [2, 4, 6];
  CheckLongInt(la2, [2, 4, 6]);

  la3 := [5, 6, 7, 8];
  CheckLongInt(la3, [5, 6, 7, 8]);

  laa1 := [la1, la2];
  CheckLongInt(laa1, [la1, la2]);

  laa2 := [la2, la1];
  CheckLongInt(laa2, [la2, la1]);

  laa3 := [la1, la3];
  CheckLongInt(laa3, [la1, la3]);

  laa1 := [[1, 3, 5], [2, 4, 6], [1, 3, 5]];
  CheckLongInt(laa1, [la1, la2, la1]);

  laa2 := [[2, 4, 6], [1, 3, 5], [5, 6, 7, 8]];
  CheckLongInt(laa2, [la2, la1, la3]);

  laa3 := [[5, 6, 7, 8]];
  CheckLongInt(laa3, [la3]);

  laa3 := [[], [], []];
  CheckLongInt(laa3, [nil, nil, nil]);

  sa1 := [[]];
  CheckSet(sa1, [[]]);

  sa1 := [[], []];
  CheckSet(sa1, [[], []]);

  saa1 := [[[]]];
  CheckSet(saa1, [[[]]]);

  saa1 := [nil];
  CheckSet(saa1, [nil]);

  saa1 := [[]];
  CheckSet(saa1, [[]]);

  sa1 := [[One], [Two, Three]];
  sa2 := [[]];
  sa3 := [[Three], []];

  saa1 := [[[One], [Two, Three]], [[]], [[Three], []]];
  CheckSet(saa1, [sa1, sa2, sa3]);

  sa1 := [[Two], [Three]];
  sa2 := [[One]];

  saa2 := [[[Two], [Three]], [[One]]];
  CheckSet(saa2, [sa1, sa2]);

  stra1 := ['Hello', 'World'];
  CheckString(stra1, ['Hello', 'World']);

  stra2 := ['Hello'];
  CheckString(stra2, ['Hello']);

  stra3 := [];
  CheckString(stra3, []);

  stra1 := ['Hello'];
  stra2 := ['World'];

  straa1 := [['Hello'], ['World']];
  CheckString(straa1, [stra1, stra2]);

  stra1 := ['Hello', 'FPC'];
  stra2 := [];
  stra3 := ['World'];

  straa2 := [['Hello', 'FPC'], [], ['World']];
  CheckString(straa2, [stra1, stra2, stra3]);

  straa3 := [[], ['World'], ['Hello', 'FPC']];
  CheckString(straa3, [stra2, stra3, stra1]);

  straa4 := [['World'], ['World']];
  CheckString(straa4, [stra3, stra3]);

  Writeln('ok');
end.
