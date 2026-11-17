{ SetRecordComparerManager: libraries supply the default comparers of record types, chained as memory
  managers are }
program trecordcmp2;

{$mode delphi}

uses
  TypInfo, Generics.Defaults, Generics.Collections;

type
  TKeyed = record
    Key: LongInt;
    Tag: LongInt;
  end;

  TOther = record
    Key: LongInt;
  end;

var
  gKeyedCmp: IComparer<TKeyed>;
  gKeyedEq: IEqualityComparer<TKeyed>;
  gOtherCmp: IComparer<TOther>;
  gOld: TRecordComparerManager;
  gFirstCalls, gSecondCalls: LongInt;

function MakeKeyed(aKey, aTag: LongInt): TKeyed;
begin
  Result.Key := aKey;
  Result.Tag := aTag;
end;

function MakeOther(aKey: LongInt): TOther;
begin
  Result.Key := aKey;
end;

function CompareKeyed(const aLeft, aRight: TKeyed): Integer;
begin
  if aLeft.Key < aRight.Key then
    Result := -1
  else if aLeft.Key > aRight.Key then
    Result := 1
  else
    Result := 0;
end;

function EqualKeyed(const aLeft, aRight: TKeyed): Boolean;
begin
  Result := aLeft.Key = aRight.Key;
end;

function HashKeyed(const aValue: TKeyed): UInt32;
begin
  Result := UInt32(aValue.Key) * 2654435761;
end;

function CompareOther(const aLeft, aRight: TOther): Integer;
begin
  if aLeft.Key < aRight.Key then
    Result := -1
  else if aLeft.Key > aRight.Key then
    Result := 1
  else
    Result := 0;
end;

{ the first library: answers for TKeyed }
function FirstLookup(aGInterface: TDefaultGenericInterface; aTypeInfo: PTypeInfo; aSize: SizeInt;
  aFactory: THashFactoryClass): Pointer;
begin
  Inc(gFirstCalls);
  Result := nil;
  if aTypeInfo <> TypeInfo(TKeyed) then
    Exit;
  case aGInterface of
    giComparer:
      Result := Pointer(gKeyedCmp);
    giEqualityComparer:
      Result := Pointer(gKeyedEq);
  end;
end;

{ the second library: answers for TOther and passes every other type on to the manager it replaced }
function SecondLookup(aGInterface: TDefaultGenericInterface; aTypeInfo: PTypeInfo; aSize: SizeInt;
  aFactory: THashFactoryClass): Pointer;
begin
  Inc(gSecondCalls);
  if (aTypeInfo = TypeInfo(TOther)) and (aGInterface = giComparer) then
    Result := Pointer(gOtherCmp)
  else if Assigned(gOld.Lookup) then
    Result := gOld.Lookup(aGInterface, aTypeInfo, aSize, aFactory)
  else
    Result := nil;
end;

procedure Check(aOk: Boolean; aCode: LongInt; const aWhat: string);
begin
  if not aOk then begin
    Writeln('FAIL ', aCode, ': ', aWhat);
    Halt(aCode);
  end;
end;

var
  lFirst, lSecond, lEmpty, lGot: TRecordComparerManager;
  lCmp: IComparer<TKeyed>;
  lEq: IEqualityComparer<TKeyed>;
  lOther: IComparer<TOther>;
  lList: TList<TKeyed>;
  lCompare: TComparisonFunc<TKeyed>;
  lEqual: TEqualityComparisonFunc<TKeyed>;
  lHash: THasherFunc<TKeyed>;
  lCompareOther: TComparisonFunc<TOther>;
begin
  lCompare := CompareKeyed;
  lEqual := EqualKeyed;
  lHash := HashKeyed;
  lCompareOther := CompareOther;
  gKeyedCmp := TComparer<TKeyed>.Construct(lCompare);
  gKeyedEq := TEqualityComparer<TKeyed>.Construct(lEqual, lHash);
  gOtherCmp := TComparer<TOther>.Construct(lCompareOther);

  { no manager at the start: the records are compared by their bytes; -1 is $FFFFFFFF, above 1 }
  GetRecordComparerManager(lGot);
  Check(not Assigned(lGot.Lookup), 1, 'no manager at the start');
  lCmp := TComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) > 0, 2, 'bytes before a manager');

  { the first library's manager }
  lFirst.Lookup := FirstLookup;
  SetRecordComparerManager(lFirst);
  lCmp := TComparer<TKeyed>.Default;
  lEq := TEqualityComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) < 0, 3, 'the manager''s order');
  Check(lEq.Equals(MakeKeyed(7, 1), MakeKeyed(7, 2)), 4, 'the manager''s equality');
  lList := TList<TKeyed>.Create;
  try
    lList.Add(MakeKeyed(3, 0));
    lList.Add(MakeKeyed(-2, 0));
    lList.Add(MakeKeyed(1, 0));
    lList.Sort;
    Check((lList[0].Key = -2) and (lList[1].Key = 1) and (lList[2].Key = 3), 5, 'TList.Sort through the manager');
  finally
    lList.Free;
  end;
  lOther := TComparer<TOther>.Default;
  Check(lOther.Compare(MakeOther(-1), MakeOther(1)) > 0, 6, 'a type the manager does not answer keeps its bytes');

  { the second library replaces it and keeps it as Old: both answer }
  lSecond.Lookup := SecondLookup;
  SetRecordComparerManager(lSecond, gOld);
  Check(@gOld.Lookup = @FirstLookup, 7, 'Old is the manager replaced');
  GetRecordComparerManager(lGot);
  Check(@lGot.Lookup = @SecondLookup, 8, 'Get returns the manager set');
  gFirstCalls := 0;
  gSecondCalls := 0;
  lOther := TComparer<TOther>.Default;
  Check(lOther.Compare(MakeOther(-1), MakeOther(1)) < 0, 9, 'the second manager answers its own type');
  lCmp := TComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) < 0, 10, 'the first answers through the second');
  Check((gSecondCalls = 2) and (gFirstCalls = 1), 11, 'the chain is asked in order');

  { the second library stops answering: it sets back the manager it replaced }
  SetRecordComparerManager(gOld);
  lOther := TComparer<TOther>.Default;
  Check(lOther.Compare(MakeOther(-1), MakeOther(1)) > 0, 12, 'the second manager removed');
  lCmp := TComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) < 0, 13, 'the first manager stays');

  { an empty manager: the bytes again }
  lEmpty.Lookup := nil;
  SetRecordComparerManager(lEmpty);
  lCmp := TComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) > 0, 14, 'bytes after the managers');
  Writeln('ok');
end.
