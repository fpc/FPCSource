{ SetRecordComparerManager: a library supplies the default comparers of a record type }
program trecordcmp1;

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

  TPair = array[0..1] of LongInt;

  TManaged = record
    Key: LongInt;
    Name: string;
  end;

  TVariantRec = record
    case Byte of
      0: (Key: LongInt);
      1: (Bytes: array[0..3] of Byte);
  end;

  TObj = object
    Key: LongInt;
  end;

var
  gComparer: IComparer<TKeyed>;
  gEquality: IEqualityComparer<TKeyed>;
  gCalls: array[TDefaultGenericInterface] of LongInt;
  gExtended: IExtendedEqualityComparer<TKeyed>;
  gSupplyExtended: Boolean = False;
  gWithFactory: LongInt;
  gLastFactory: THashFactoryClass;
  gWrongType: LongInt;
  gManagedCalls, gVariantCalls: LongInt;

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

{ the hash list as the dictionaries pass it: its first word holds how many hashes are asked for, and they
  follow it }
procedure HashListKeyed(const aValue: TKeyed; aHashList: PUInt32);
var
  lIdx: LongInt;
begin
  for lIdx := 1 to PUInt16(aHashList)^ do
    aHashList[lIdx] := UInt32(aValue.Key) xor (UInt32(lIdx) shl 24) xor $5EED0000;
end;

function HashListRight(const aHashList: array of UInt32): Boolean;
begin
  Result := (aHashList[0] = 2) and (aHashList[1] = (7 xor (1 shl 24) xor $5EED0000)) and
    (aHashList[2] = (7 xor (2 shl 24) xor $5EED0000)) and (aHashList[3] = $0BADF00D);
end;

function Lookup(aGInterface: TDefaultGenericInterface; aTypeInfo: PTypeInfo; aSize: SizeInt;
  aFactory: THashFactoryClass): Pointer;
begin
  Inc(gCalls[aGInterface]);
  gLastFactory := aFactory;
  if Assigned(aFactory) then
    Inc(gWithFactory);
  Result := nil;
  if aTypeInfo = TypeInfo(TManaged) then begin
    Inc(gManagedCalls);
    if aSize <> SizeOf(TManaged) then
      Inc(gWrongType);
  end else if aTypeInfo = TypeInfo(TVariantRec) then begin
    Inc(gVariantCalls);
    if aSize <> SizeOf(TVariantRec) then
      Inc(gWrongType);
  end;
  if aTypeInfo <> TypeInfo(TKeyed) then
    Exit;
  if aSize <> SizeOf(TKeyed) then
    Inc(gWrongType);
  case aGInterface of
    giComparer:
      Result := Pointer(gComparer);
    giEqualityComparer:
      Result := Pointer(gEquality);
    giExtendedEqualityComparer:
      if gSupplyExtended then
        Result := Pointer(gExtended);
  end;
end;

procedure Check(aOk: Boolean; aCode: LongInt; const aWhat: string);
begin
  if not aOk then begin
    Writeln('FAIL ', aCode, ': ', aWhat);
    Halt(aCode);
  end;
end;

var
  lList: TList<TKeyed>;
  lDict: TDictionary<TKeyed, LongInt>;
  lGI: TDefaultGenericInterface;
  lCompare: TComparisonFunc<TKeyed>;
  lEqual: TEqualityComparisonFunc<TKeyed>;
  lHash: THasherFunc<TKeyed>;
  lHashListFunc: TExtendedHasherFunc<TKeyed>;
  lHashList: array[0..3] of UInt32;
  lCmp: IComparer<TKeyed>;
  lEq: IEqualityComparer<TKeyed>;
  lExt: IExtendedEqualityComparer<TKeyed>;
  lOther: IComparer<TOther>;
  lIntCmp: IComparer<LongInt>;
  lIntEq: IEqualityComparer<LongInt>;
  lPairCmp: IComparer<TPair>;
  lPairEq: IEqualityComparer<TPair>;
  lPair, lPair2: TPair;
  lManagedCmp: IComparer<TManaged>;
  lVariantEq: IEqualityComparer<TVariantRec>;
  lObjCmp: IComparer<TObj>;
  lObj1, lObj2: TObj;
  lManager, lOld: TRecordComparerManager;
begin
  lCompare := CompareKeyed;
  lEqual := EqualKeyed;
  lHash := HashKeyed;
  gComparer := TComparer<TKeyed>.Construct(lCompare);
  gEquality := TEqualityComparer<TKeyed>.Construct(lEqual, lHash);
  lHashListFunc := HashListKeyed;
  gExtended := TExtendedEqualityComparer<TKeyed>.Construct(lEqual, lHash, lHashListFunc);

  { without a lookup the record is compared by its bytes: -1 is $FFFFFFFF, above 1 }
  lCmp := TComparer<TKeyed>.Default;
  lEq := TEqualityComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) > 0, 1, 'binary order before');
  Check(not lEq.Equals(MakeKeyed(7, 1), MakeKeyed(7, 2)), 2, 'binary equality before');

  lManager.Lookup := Lookup;
  SetRecordComparerManager(lManager, lOld);

  lCmp := TComparer<TKeyed>.Default;
  lEq := TEqualityComparer<TKeyed>.Default;
  Check(lCmp.Compare(MakeKeyed(-1, 0), MakeKeyed(1, 0)) < 0, 3, 'supplied order');
  Check(lEq.Equals(MakeKeyed(7, 1), MakeKeyed(7, 2)), 4, 'supplied equality');
  Check(lEq.GetHashCode(MakeKeyed(7, 1)) = lEq.GetHashCode(MakeKeyed(7, 2)), 5, 'supplied hash');

  lList := TList<TKeyed>.Create;
  try
    lList.Add(MakeKeyed(3, 0));
    lList.Add(MakeKeyed(-2, 0));
    lList.Add(MakeKeyed(1, 0));
    lList.Add(MakeKeyed(-5, 0));
    lList.Sort;
    Check((lList[0].Key = -5) and (lList[1].Key = -2) and (lList[2].Key = 1) and (lList[3].Key = 3), 6,
      'TList.Sort with the supplied comparer');
  finally
    lList.Free;
  end;

  { a dictionary asks through the overload that takes a hash factory }
  gWithFactory := 0;
  lDict := TDictionary<TKeyed, LongInt>.Create;
  try
    lDict.Add(MakeKeyed(7, 1), 70);
    Check(lDict.ContainsKey(MakeKeyed(7, 2)), 7, 'TDictionary with the supplied equality');
  finally
    lDict.Free;
  end;
  Check(gWithFactory > 0, 8, 'the factory is passed on');

  { nil from the lookup keeps the bytes: no extended comparer is supplied here }
  lExt := TExtendedEqualityComparer<TKeyed>.Default;
  Check(not lExt.Equals(MakeKeyed(7, 1), MakeKeyed(7, 2)), 9, 'extended falls back to the bytes');
  Check(gCalls[giExtendedEqualityComparer] > 0, 10, 'extended consults the lookup');
  gCalls[giExtendedEqualityComparer] := 0;
  gWithFactory := 0;
  lExt := TExtendedEqualityComparer<TKeyed>.Default(TDelphiQuadrupleHashFactory);
  Check((gCalls[giExtendedEqualityComparer] = 1) and (gWithFactory = 1), 18,
    'extended with a factory consults the lookup');
  Check(gLastFactory = TDelphiQuadrupleHashFactory, 19, 'the extended factory passed is the one asked for');

  { an extended comparer supplied: both overloads use it }
  gSupplyExtended := True;
  lExt := TExtendedEqualityComparer<TKeyed>.Default;
  Check(lExt.Equals(MakeKeyed(7, 1), MakeKeyed(7, 2)), 20, 'supplied extended equality');
  Check(lExt.GetHashCode(MakeKeyed(7, 1)) = lExt.GetHashCode(MakeKeyed(7, 2)), 21, 'supplied extended hash');
  lHashList[0] := 2;
  lHashList[1] := 0;
  lHashList[2] := 0;
  lHashList[3] := $0BADF00D;
  lExt.GetHashList(MakeKeyed(7, 1), @lHashList[0]);
  Check(HashListRight(lHashList), 22, 'supplied extended hash list');
  lExt := TExtendedEqualityComparer<TKeyed>.Default(TDelphiQuadrupleHashFactory);
  Check(lExt.Equals(MakeKeyed(7, 1), MakeKeyed(7, 2)), 23, 'supplied extended equality, factory overload');
  lHashList[0] := 2;
  lHashList[1] := 0;
  lHashList[2] := 0;
  lHashList[3] := $0BADF00D;
  lExt.GetHashList(MakeKeyed(7, 1), @lHashList[0]);
  Check(HashListRight(lHashList), 30, 'supplied extended hash list, factory overload');
  gSupplyExtended := False;
  lEq := TEqualityComparer<TKeyed>.Default(TxxHash32HashFactory);
  Check(gLastFactory = TxxHash32HashFactory, 24, 'the factory passed is the one asked for');

  lOther := TComparer<TOther>.Default;
  Check(lOther.Compare(MakeOther(-1), MakeOther(1)) > 0, 11, 'another record keeps its bytes');

  { a record with a managed field and a variant record reach the manager too, with their own type
    information and size }
  gManagedCalls := 0;
  gVariantCalls := 0;
  lManagedCmp := TComparer<TManaged>.Default;
  lVariantEq := TEqualityComparer<TVariantRec>.Default;
  Check(gManagedCalls = 1, 25, 'a record with a managed field reaches the manager');
  Check(gVariantCalls = 1, 26, 'a variant record reaches the manager');

  { a type that is not a record never reaches the lookup: a static array and an object, compared by their
    bytes, included }
  for lGI := Low(lGI) to High(lGI) do
    gCalls[lGI] := 0;
  lIntCmp := TComparer<LongInt>.Default;
  lIntEq := TEqualityComparer<LongInt>.Default;
  Check(lIntCmp.Compare(-1, 1) < 0, 12, 'LongInt');
  Check(lIntEq.Equals(5, 5), 13, 'LongInt equality');
  lPairCmp := TComparer<TPair>.Default;
  lPairEq := TEqualityComparer<TPair>.Default;
  lPair[0] := 1;
  lPair[1] := 2;
  lPair2[0] := 1;
  lPair2[1] := 3;
  Check(lPairCmp.Compare(lPair, lPair) = 0, 16, 'static array');
  Check(lPairEq.Equals(lPair, lPair), 17, 'static array equality');
  Check(lPairCmp.Compare(lPair, lPair2) < 0, 27, 'static array order by bytes');
  Check(not lPairEq.Equals(lPair, lPair2), 28, 'static array inequality by bytes');
  lObjCmp := TComparer<TObj>.Default;
  { -1 is $FFFFFFFF: above 1 by its bytes, below it as a number }
  lObj1.Key := -1;
  lObj2.Key := 1;
  Check(lObjCmp.Compare(lObj1, lObj2) > 0, 29, 'object order by bytes');
  for lGI := Low(lGI) to High(lGI) do
    Check(gCalls[lGI] = 0, 14, 'the lookup was asked about a type that is not a record');
  Check(gWrongType = 0, 15, 'the size passed');

  SetRecordComparerManager(lOld);
  Writeln('ok');
end.
