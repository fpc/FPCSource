{
  Tests of TDataSetField and nested TBufDataset datasets: all field types,
  blob data and the dataset operations on a nested dataset.
}
unit TestNestedDataSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, FmtBCD, DB, BufDataset, XMLDatapacketReader,
  fpcunit, testregistry, ToolsUnit;

type

  { TDerivedBufDataset }

  TDerivedBufDataset = class(TBufDataset);

  { TTestNestedDataSet }

  TTestNestedDataSet = class(TTestCase)
  private
    FParent: TBufDataset;
    FCalcCount: Integer;
    FFilterName: string;
    procedure AddAllTypeDefs(aDefs: TFieldDefs; aLevel: Integer);
    procedure AddSimpleDefs(aDefs: TFieldDefs);
    function CreateAllTypesParent: TBufDataset;
    function CreateSimpleParent: TBufDataset;
    function Nested: TBufDataset;
    function NestedOf(aDataSet: TDataSet; const aFieldName: string = 'DETAILS'): TBufDataset;
    function RowKey(aParentNo, aRow: Integer): Integer;
    procedure SetValue(aField: TField; aKey: Integer);
    function ExpectedString(aField: TField; aKey: Integer): UnicodeString;
    procedure CheckValue(aField: TField; aKey: Integer; const aWhere: string);
    procedure FillRows(aDataSet: TDataSet; aParentNo, aCount: Integer);
    procedure CheckRows(aDataSet: TDataSet; aParentNo, aCount: Integer; const aFieldName: string; const aWhere: string);
    procedure FillParents(aParents, aRows: Integer);
    procedure CheckParents(aParents, aRows: Integer; const aFieldName: string; const aWhere: string);
    procedure CheckType(const aFieldName: string);
    procedure AppendSimple(aDataSet: TDataSet; aNum: Integer; const aName: string);
    procedure AddSimpleParents(aCount: Integer);
    function NestedNames(aDataSet: TDataSet): string;
    function SaveAndLoad(aDataSet: TBufDataset; aFormat: TDataPacketFormat): TBufDataset;
    procedure DoCalcFields(DataSet: TDataSet);
    procedure DoFilterName(DataSet: TDataSet; var Accept: Boolean);
    function BlobBytes(aKey, aLength: Integer): TBytes;
    function CreateObjectFieldsDataset(aObjectView: Boolean): TBufDataset;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // structure and API
    procedure TestFieldClass;
    procedure TestFieldDefListAndFieldList;
    procedure TestNestedDataSetCreatedWhenUsed;
    procedure TestNestedFieldDefsFromChildDefs;
    procedure TestAssignDataSetField;
    procedure TestCircularLink;
    procedure TestNestedDataSetClass;
    procedure TestOpenCloseWithParent;
    procedure TestOpenWithoutParent;
    procedure TestFieldValueProperties;
    procedure TestReadOnlyParent;
    procedure TestReadOnlyDataSetField;
    procedure TestFreeParentFirst;
    procedure TestFreeNestedFirst;
    procedure TestObjectViewFalse;
    procedure TestPersistentFieldsInNested;
    procedure TestCalculatedFieldInNested;
    procedure TestLookupFieldInNested;
    procedure TestIsLinkedTo;
    // values of every field type
    procedure TestTypeString;
    procedure TestTypeFixedChar;
    procedure TestTypeWideString;
    procedure TestTypeFixedWideChar;
    procedure TestTypeSmallint;
    procedure TestTypeInteger;
    procedure TestTypeWord;
    procedure TestTypeLongWord;
    procedure TestTypeShortint;
    procedure TestTypeByte;
    procedure TestTypeLargeint;
    procedure TestTypeBoolean;
    procedure TestTypeFloat;
    procedure TestTypeExtended;
    procedure TestTypeSingle;
    procedure TestTypeCurrency;
    procedure TestTypeBCD;
    procedure TestTypeFmtBCD;
    procedure TestTypeDate;
    procedure TestTypeTime;
    procedure TestTypeDateTime;
    procedure TestTypeBytes;
    procedure TestTypeVarBytes;
    procedure TestTypeAutoInc;
    procedure TestTypeGuid;
    procedure TestTypeVariant;
    procedure TestTypeBlob;
    procedure TestTypeMemo;
    procedure TestTypeGraphic;
    procedure TestTypeFmtMemo;
    procedure TestTypeTypedBinary;
    procedure TestTypeParadoxOle;
    procedure TestTypeDBaseOle;
    procedure TestTypeOraBlob;
    procedure TestTypeOraClob;
    procedure TestTypeWideMemo;
    procedure TestTypeADT;
    procedure TestTypeArray;
    procedure TestTypeDataSet;
    procedure TestAllTypesTogether;
    procedure TestNullValues;
    procedure TestDifferentSetsPerParent;
    procedure TestEmptySetAndNull;
    // blobs
    procedure TestBlobStreamWriteRead;
    procedure TestBlobAsBytes;
    procedure TestBlobFieldLoadSaveStream;
    procedure TestBlobEmptyIsNull;
    procedure TestBlobAllByteValues;
    procedure TestBlobOneMegabyte;
    procedure TestBlobSixteenMegabytes;
    procedure TestMemoUTF8;
    procedure TestMemoCodePage;
    procedure TestWideMemoOutsideBMP;
    procedure TestSeveralBlobsPerRow;
    procedure TestBlobEditNestedCancel;
    procedure TestBlobEditParentCancel;
    procedure TestBlobEditParentPostScroll;
    procedure TestBlobRevertRecord;
    procedure TestBlobCancelUpdates;
    procedure TestBlobMergeChangeLog;
    procedure TestBlobCancelUpdatesThenMerge;
    procedure TestBlobStreamBinary;
    procedure TestBlobStreamXML;
    procedure TestBlobManyEdits;
    procedure TestBlobSecondLevel;
    // operations on the nested dataset
    procedure TestNavigation;
    procedure TestRecNo;
    procedure TestInsertPositions;
    procedure TestEditPostCancel;
    procedure TestDeletePositions;
    procedure TestDeleteAll;
    procedure TestDeleteEmpty;
    procedure TestBookmarks;
    procedure TestBookmarkAfterParentScroll;
    procedure TestLocate;
    procedure TestLocateOptions;
    procedure TestLookup;
    procedure TestFilterExpression;
    procedure TestOnFilterRecord;
    procedure TestIndexFieldNames;
    procedure TestAddIndex;
    procedure TestIndexAfterParentScroll;
    procedure TestIndexOnInsert;
    procedure TestNestedUpdateStatus;
    procedure TestNestedCancelUpdates;
    procedure TestNestedRevertRecord;
    procedure TestNestedApplyUpdates;
    procedure TestNestedFileName;
    procedure TestAutoIncShared;
    procedure TestRequiredField;
    procedure TestCalcFieldsAfterParentScroll;
    procedure TestDisableControlsNested;
    procedure TestDataLinkEvents;
    procedure TestNestedSaveToStream;
    procedure TestNestedLoadFromStream;
    procedure TestClearField;
    procedure TestManyRows;
    // interaction with the parent
    procedure TestEditPutsParentInEdit;
    procedure TestInsertPutsParentInEdit;
    procedure TestPostKeepsParentInEdit;
    procedure TestParentCancelKeepsNested;
    procedure TestParentPendingEditAndCancel;
    procedure TestNestedPostInNewParent;
    procedure TestParentCancelOfNewRecord;
    procedure TestScrollPostsNestedEdit;
    procedure TestNestedDeleteLeavesParentBrowse;
    procedure TestParentDelete;
    procedure TestParentCancelUpdates;
    procedure TestParentRevertRecord;
    procedure TestParentUpdateStatus;
    procedure TestParentMergeChangeLog;
    procedure TestFollowFilteredParent;
    procedure TestFollowIndexedParent;
    procedure TestFollowLocate;
    procedure TestParentDisableControls;
    procedure TestStreamBinaryAllTypes;
    procedure TestStreamXMLAllTypes;
    procedure TestStreamWithChangeLog;
    procedure TestStreamAutoInc;
    procedure TestTwoDataSetFields;
    procedure TestThreeLevels;
    // errors
    procedure TestDataSetFieldAsADTChild;
    procedure TestIndexOnDataSetField;
    procedure TestCopyFromDataset;
    procedure TestXMLObjectFields;
    procedure TestXMLObjectFieldsNested;
    procedure TestObjectFieldsStreamBinary;
  end;

implementation

const
  TextBlobTypes = [ftMemo, ftFmtMemo, ftOraClob];
  BinaryBlobTypes = [ftBlob, ftGraphic, ftTypedBinary, ftParadoxOle, ftDBaseOle, ftOraBlob];
  RowCount = 25;

var
  ISOFormat: TFormatSettings;

// The date of an ISO date string yyyy-mm-dd
function ISODate(const aValue: string): TDateTime;

begin
  Result := EncodeDate(StrToInt(Copy(aValue, 1, 4)), StrToInt(Copy(aValue, 6, 2)), StrToInt(Copy(aValue, 9, 2)));
end;


// A time value for key aKey
function KeyTime(aKey: Integer): TDateTime;

begin
  Result := EncodeTime(aKey mod 24, (aKey * 7) mod 60, (aKey * 13) mod 60, (aKey * 37) mod 1000);
end;


// A wide string with characters outside Latin-1 and outside the BMP
function WideText(aKey: Integer): UnicodeString;

begin
  Result := 'w' + IntToStr(aKey) + #$00E4#$20AC#$D83D#$DE00;
end;


{ TTestNestedDataSet }

procedure TTestNestedDataSet.SetUp;

begin
  FParent := nil;
  FCalcCount := 0;
end;


procedure TTestNestedDataSet.TearDown;

begin
  FreeAndNil(FParent);
end;


function TTestNestedDataSet.BlobBytes(aKey, aLength: Integer): TBytes;

var
  i: Integer;

begin
  SetLength(Result, aLength);
  for i := 0 to aLength - 1 do
    Result[i] := (aKey + i) mod 256;
end;


procedure TTestNestedDataSet.AddAllTypeDefs(aDefs: TFieldDefs; aLevel: Integer);

begin
  aDefs.Add('FSTRING', ftString, 10);
  aDefs.Add('FFIXEDCHAR', ftFixedChar, 10);
  aDefs.Add('FWIDESTRING', ftWideString, 20);
  aDefs.Add('FFIXEDWIDECHAR', ftFixedWideChar, 20);
  aDefs.Add('FSMALLINT', ftSmallint);
  aDefs.Add('FINTEGER', ftInteger);
  aDefs.Add('FWORD', ftWord);
  aDefs.Add('FLONGWORD', ftLongWord);
  aDefs.Add('FSHORTINT', ftShortint);
  aDefs.Add('FBYTE', ftByte);
  aDefs.Add('FLARGEINT', ftLargeint);
  aDefs.Add('FBOOLEAN', ftBoolean);
  aDefs.Add('FFLOAT', ftFloat);
  aDefs.Add('FEXTENDED', ftExtended);
  aDefs.Add('FSINGLE', ftSingle);
  aDefs.Add('FCURRENCY', ftCurrency);
  aDefs.Add('FBCD', ftBCD, 4);
  with aDefs.AddFieldDef do
    begin
    Name := 'FFMTBCD';
    DataType := ftFmtBCD;
    Size := 6;
    Precision := 18;
    end;
  aDefs.Add('FDATE', ftDate);
  aDefs.Add('FTIME', ftTime);
  aDefs.Add('FDATETIME', ftDateTime);
  aDefs.Add('FBYTES', ftBytes, 10);
  aDefs.Add('FVARBYTES', ftVarBytes, 20);
  aDefs.Add('FAUTOINC', ftAutoInc);
  aDefs.Add('FGUID', ftGuid, 38);
  aDefs.Add('FVARIANT', ftVariant);
  aDefs.Add('FBLOB', ftBlob);
  aDefs.Add('FMEMO', ftMemo);
  aDefs.Add('FGRAPHIC', ftGraphic);
  aDefs.Add('FFMTMEMO', ftFmtMemo);
  aDefs.Add('FTYPEDBINARY', ftTypedBinary);
  aDefs.Add('FPARADOXOLE', ftParadoxOle);
  aDefs.Add('FDBASEOLE', ftDBaseOle);
  aDefs.Add('FORABLOB', ftOraBlob);
  aDefs.Add('FORACLOB', ftOraClob);
  aDefs.Add('FWIDEMEMO', ftWideMemo);
  with aDefs.AddFieldDef do
    begin
    Name := 'FADT';
    DataType := ftADT;
    ChildDefs.Add('X', ftInteger);
    ChildDefs.Add('Y', ftFloat);
    end;
  with aDefs.AddFieldDef do
    begin
    Name := 'FARRAY';
    DataType := ftArray;
    Size := 3;
    ChildDefs.Add('E', ftInteger);
    end;
  if aLevel = 1 then
    with aDefs.AddFieldDef do
      begin
      Name := 'SUB';
      DataType := ftDataSet;
      ChildDefs.Add('SID', ftAutoInc);
      ChildDefs.Add('SNAME', ftString, 20);
      ChildDefs.Add('SBLOB', ftBlob);
      end;
end;


procedure TTestNestedDataSet.AddSimpleDefs(aDefs: TFieldDefs);

begin
  aDefs.Add('NID', ftAutoInc);
  aDefs.Add('NAME', ftString, 20);
  aDefs.Add('NUM', ftInteger);
  aDefs.Add('B', ftBlob);
end;


function TTestNestedDataSet.CreateAllTypesParent: TBufDataset;

begin
  Result := TBufDataset.Create(nil);
  Result.FieldDefs.Add('ID', ftInteger);
  Result.FieldDefs.Add('NAME', ftString, 20);
  Result.FieldDefs.Add('FBLOB', ftBlob);
  with Result.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    AddAllTypeDefs(ChildDefs, 1);
    end;
  Result.CreateDataset;
end;


function TTestNestedDataSet.CreateSimpleParent: TBufDataset;

begin
  Result := TBufDataset.Create(nil);
  Result.FieldDefs.Add('ID', ftInteger);
  Result.FieldDefs.Add('NAME', ftString, 20);
  with Result.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    AddSimpleDefs(ChildDefs);
    end;
  Result.CreateDataset;
end;


function TTestNestedDataSet.NestedOf(aDataSet: TDataSet; const aFieldName: string): TBufDataset;

begin
  Result := (aDataSet.FieldByName(aFieldName) as TDataSetField).NestedDataSet as TBufDataset;
end;


function TTestNestedDataSet.Nested: TBufDataset;

begin
  Result := NestedOf(FParent);
end;


function TTestNestedDataSet.RowKey(aParentNo, aRow: Integer): Integer;

begin
  Result := (aRow + aParentNo * 3) mod testValuesCount;
end;


procedure TTestNestedDataSet.SetValue(aField: TField; aKey: Integer);

var
  lGuid: TGuid;
  i: Integer;
  lBytes: TBytes;

begin
  case aField.DataType of
    ftString, ftFixedChar:
      aField.AsString := testStringValues[aKey];
    ftWideString, ftFixedWideChar:
      aField.AsUnicodeString := WideText(aKey);
    ftSmallint:
      aField.AsInteger := testSmallIntValues[aKey];
    ftInteger:
      aField.AsInteger := testIntValues[aKey];
    ftWord:
      aField.AsInteger := testWordValues[aKey];
    ftLongWord:
      aField.AsLongWord := testLongWordValues[aKey];
    ftShortint:
      aField.AsInteger := testShortintValues[aKey];
    ftByte:
      aField.AsInteger := testByteValues[aKey];
    ftLargeint:
      aField.AsLargeInt := testLargeIntValues[aKey];
    ftBoolean:
      aField.AsBoolean := testBooleanValues[aKey];
    ftFloat:
      aField.AsFloat := testFloatValues[aKey];
    ftExtended:
      aField.AsExtended := testFloatValues[aKey];
    ftSingle:
      aField.AsSingle := testSingleValues[aKey];
    ftCurrency, ftBCD:
      aField.AsCurrency := testCurrencyValues[aKey];
    ftFmtBCD:
      aField.AsBCD := StrToBCD(testFmtBCDValues[aKey], ISOFormat);
    ftDate:
      aField.AsDateTime := ISODate(testDateValues[aKey]);
    ftTime:
      aField.AsDateTime := KeyTime(aKey);
    ftDateTime:
      aField.AsDateTime := ISODate(testDateValues[aKey]) + KeyTime(aKey);
    ftBytes:
      aField.AsBytes := BlobBytes(aKey * 10, 10);
    ftVarBytes:
      aField.AsBytes := BlobBytes(aKey * 3, aKey mod 20 + 1);
    ftGuid:
      begin
      FillChar(lGuid, SizeOf(lGuid), 0);
      lGuid.D1 := aKey + 1;
      lGuid.D4[7] := aKey;
      TGuidField(aField).AsGuid := lGuid;
      end;
    ftVariant:
      case aKey mod 4 of
        0: aField.AsVariant := 'variant ' + testStringValues[aKey];
        1: aField.AsVariant := testIntValues[aKey];
        2: aField.AsVariant := testFloatValues[aKey];
        3: aField.AsVariant := testBooleanValues[aKey];
      end;
    ftWideMemo:
      aField.AsUnicodeString := WideText(aKey) + UnicodeString(StringOfChar('x', aKey * 50));
    ftADT:
      begin
      TADTField(aField).Fields[0].AsInteger := aKey * 10;
      TADTField(aField).Fields[1].AsFloat := aKey / 4;
      end;
    ftArray:
      for i := 0 to TArrayField(aField).FieldCount - 1 do
        TArrayField(aField).Fields[i].AsInteger := aKey + i;
  else
    if aField.DataType in TextBlobTypes then
      aField.AsString := 'memo ' + testStringValues[aKey] + StringOfChar('m', aKey * 40)
    else if aField.DataType in BinaryBlobTypes then
      begin
      lBytes := BlobBytes(aKey, (aKey * 37) mod 3000 + 1);
      aField.AsBytes := lBytes;
      end;
  end;
end;


function TTestNestedDataSet.ExpectedString(aField: TField; aKey: Integer): UnicodeString;

begin
  if aField.DataType in [ftWideString, ftFixedWideChar] then
    Result := WideText(aKey)
  else
    Result := UnicodeString(testStringValues[aKey]);
end;


procedure TTestNestedDataSet.CheckValue(aField: TField; aKey: Integer; const aWhere: string);

var
  lMsg: string;
  lGuid: TGuid;
  i: Integer;

  procedure CheckBytes(const aExpected: TBytes);
  var
    lActual: TBytes;
    j: Integer;
  begin
    lActual := aField.AsBytes;
    AssertEquals(lMsg + ' length', Length(aExpected), Length(lActual));
    for j := 0 to Length(aExpected) - 1 do
      if aExpected[j] <> lActual[j] then
        Fail(Format('%s byte %d: expected %d, got %d', [lMsg, j, aExpected[j], lActual[j]]));
  end;

begin
  lMsg := Format('%s %s key %d', [aWhere, aField.FieldName, aKey]);
  case aField.DataType of
    ftString, ftWideString:
      AssertEquals(lMsg, UnicodeString(ExpectedString(aField, aKey)), aField.AsUnicodeString);
    ftFixedChar, ftFixedWideChar:
      AssertEquals(lMsg, TrimRight(UnicodeString(ExpectedString(aField, aKey))), TrimRight(aField.AsUnicodeString));
    ftSmallint:
      AssertEquals(lMsg, testSmallIntValues[aKey], aField.AsInteger);
    ftInteger:
      AssertEquals(lMsg, testIntValues[aKey], aField.AsInteger);
    ftWord:
      AssertEquals(lMsg, testWordValues[aKey], aField.AsInteger);
    ftLongWord:
      AssertEquals(lMsg, testLongWordValues[aKey], aField.AsLongWord);
    ftShortint:
      AssertEquals(lMsg, testShortintValues[aKey], aField.AsInteger);
    ftByte:
      AssertEquals(lMsg, testByteValues[aKey], aField.AsInteger);
    ftLargeint:
      AssertEquals(lMsg, testLargeIntValues[aKey], aField.AsLargeInt);
    ftBoolean:
      AssertEquals(lMsg, testBooleanValues[aKey], aField.AsBoolean);
    ftFloat:
      AssertEquals(lMsg, testFloatValues[aKey], aField.AsFloat, 0);
    ftExtended:
      AssertEquals(lMsg, testFloatValues[aKey], aField.AsExtended, 0);
    ftSingle:
      AssertEquals(lMsg, testSingleValues[aKey], aField.AsSingle, 0);
    ftCurrency, ftBCD:
      AssertEquals(lMsg, testCurrencyValues[aKey], aField.AsCurrency, 0);
    ftFmtBCD:
      AssertEquals(lMsg, BCDToStr(StrToBCD(testFmtBCDValues[aKey], ISOFormat), ISOFormat),
        BCDToStr(aField.AsBCD, ISOFormat));
    ftDate:
      AssertEquals(lMsg, ISODate(testDateValues[aKey]), aField.AsDateTime, 0);
    ftTime:
      AssertEquals(lMsg, KeyTime(aKey), aField.AsDateTime, 1e-9);
    ftDateTime:
      AssertEquals(lMsg, ISODate(testDateValues[aKey]) + KeyTime(aKey), aField.AsDateTime, 1e-9);
    ftBytes:
      CheckBytes(BlobBytes(aKey * 10, 10));
    ftVarBytes:
      CheckBytes(BlobBytes(aKey * 3, aKey mod 20 + 1));
    ftGuid:
      begin
      FillChar(lGuid, SizeOf(lGuid), 0);
      lGuid.D1 := aKey + 1;
      lGuid.D4[7] := aKey;
      AssertEquals(lMsg, GUIDToString(lGuid), GUIDToString(TGuidField(aField).AsGuid));
      end;
    ftVariant:
      case aKey mod 4 of
        0: AssertEquals(lMsg, 'variant ' + testStringValues[aKey], VarToStr(aField.AsVariant));
        1: AssertEquals(lMsg, testIntValues[aKey], Integer(aField.AsVariant));
        2: AssertEquals(lMsg, testFloatValues[aKey], Double(aField.AsVariant), 0);
        3: AssertEquals(lMsg, testBooleanValues[aKey], Boolean(aField.AsVariant));
      end;
    ftWideMemo:
      AssertEquals(lMsg, WideText(aKey) + UnicodeString(StringOfChar('x', aKey * 50)), aField.AsUnicodeString);
    ftADT:
      begin
      AssertEquals(lMsg + ' X', aKey * 10, TADTField(aField).Fields[0].AsInteger);
      AssertEquals(lMsg + ' Y', aKey / 4, TADTField(aField).Fields[1].AsFloat, 0);
      end;
    ftArray:
      for i := 0 to TArrayField(aField).FieldCount - 1 do
        AssertEquals(Format('%s [%d]', [lMsg, i]), aKey + i, TArrayField(aField).Fields[i].AsInteger);
  else
    if aField.DataType in TextBlobTypes then
      AssertEquals(lMsg, 'memo ' + testStringValues[aKey] + StringOfChar('m', aKey * 40), aField.AsString)
    else if aField.DataType in BinaryBlobTypes then
      CheckBytes(BlobBytes(aKey, (aKey * 37) mod 3000 + 1));
  end;
end;


procedure TTestNestedDataSet.FillRows(aDataSet: TDataSet; aParentNo, aCount: Integer);

var
  lRow, i, j: Integer;
  lField: TField;
  lSub: TDataSet;

begin
  for lRow := 0 to aCount - 1 do
    begin
    aDataSet.Append;
    for i := 0 to aDataSet.Fields.Count - 1 do
      begin
      lField := aDataSet.Fields[i];
      if (lField.FieldKind = fkData) and not (lField.DataType in [ftAutoInc, ftDataSet]) then
        SetValue(lField, RowKey(aParentNo, lRow));
      end;
    aDataSet.Post;
    if aDataSet.FindField('SUB') <> nil then
      begin
      lSub := NestedOf(aDataSet, 'SUB');
      for j := 0 to lRow mod 3 - 1 do
        begin
        lSub.Append;
        lSub.FieldByName('SNAME').AsString := Format('s%d_%d_%d', [aParentNo, lRow, j]);
        lSub.FieldByName('SBLOB').AsBytes := BlobBytes(aParentNo + lRow + j, 100 * (j + 1));
        lSub.Post;
        end;
      end;
    end;
end;


procedure TTestNestedDataSet.CheckRows(aDataSet: TDataSet; aParentNo, aCount: Integer; const aFieldName: string; const aWhere: string);

var
  lRow, i, j: Integer;
  lField: TField;
  lSub: TDataSet;
  lWhere: string;

begin
  AssertEquals(aWhere + ' record count', aCount, aDataSet.RecordCount);
  aDataSet.First;
  for lRow := 0 to aCount - 1 do
    begin
    AssertFalse(aWhere + ' not at EOF', aDataSet.EOF);
    lWhere := Format('%s parent %d row %d', [aWhere, aParentNo, lRow]);
    for i := 0 to aDataSet.Fields.Count - 1 do
      begin
      lField := aDataSet.Fields[i];
      if (lField.FieldKind <> fkData) or (lField.DataType in [ftAutoInc, ftDataSet]) then
        Continue;
      if (aFieldName = '') or SameText(aFieldName, lField.FieldName) then
        CheckValue(lField, RowKey(aParentNo, lRow), lWhere);
      end;
    if (aDataSet.FindField('SUB') <> nil) and ((aFieldName = '') or SameText(aFieldName, 'SUB')) then
      begin
      lSub := NestedOf(aDataSet, 'SUB');
      AssertEquals(lWhere + ' SUB record count', lRow mod 3, lSub.RecordCount);
      lSub.First;
      for j := 0 to lRow mod 3 - 1 do
        begin
        AssertEquals(lWhere + ' SNAME', Format('s%d_%d_%d', [aParentNo, lRow, j]), lSub.FieldByName('SNAME').AsString);
        AssertEquals(lWhere + ' SBLOB length', 100 * (j + 1), Length(lSub.FieldByName('SBLOB').AsBytes));
        AssertEquals(lWhere + ' SBLOB first byte', (aParentNo + lRow + j) mod 256, lSub.FieldByName('SBLOB').AsBytes[0]);
        lSub.Next;
        end;
      end;
    aDataSet.Next;
    end;
  AssertTrue(aWhere + ' at EOF', aDataSet.EOF);
end;


procedure TTestNestedDataSet.FillParents(aParents, aRows: Integer);

var
  p: Integer;

begin
  FParent := CreateAllTypesParent;
  for p := 0 to aParents - 1 do
    begin
    FParent.Append;
    FParent.FieldByName('ID').AsInteger := p + 1;
    FParent.FieldByName('NAME').AsString := 'parent' + IntToStr(p + 1);
    FParent.FieldByName('FBLOB').AsBytes := BlobBytes(p, 500);
    FParent.Post;
    FillRows(Nested, p, aRows - p);
    if FParent.State in dsEditModes then
      FParent.Post;
    end;
end;


procedure TTestNestedDataSet.CheckParents(aParents, aRows: Integer; const aFieldName: string; const aWhere: string);

var
  p: Integer;
  lNested: TDataSet;

begin
  AssertEquals(aWhere + ' parent count', aParents, FParent.RecordCount);
  lNested := Nested;
  FParent.First;
  for p := 0 to aParents - 1 do
    begin
    AssertEquals(aWhere + ' parent ID', p + 1, FParent.FieldByName('ID').AsInteger);
    AssertEquals(aWhere + ' parent blob length', 500, Length(FParent.FieldByName('FBLOB').AsBytes));
    CheckRows(lNested, p, aRows - p, aFieldName, aWhere);
    FParent.Next;
    end;
end;


procedure TTestNestedDataSet.CheckType(const aFieldName: string);

begin
  FillParents(3, RowCount);
  FParent.Last;
  CheckParents(3, RowCount, aFieldName, 'after scrolling');
  FParent.MergeChangeLog;
  CheckParents(3, RowCount, aFieldName, 'after MergeChangeLog');
end;


procedure TTestNestedDataSet.AppendSimple(aDataSet: TDataSet; aNum: Integer; const aName: string);

begin
  aDataSet.Append;
  aDataSet.FieldByName('NAME').AsString := aName;
  aDataSet.FieldByName('NUM').AsInteger := aNum;
  aDataSet.Post;
end;


procedure TTestNestedDataSet.AddSimpleParents(aCount: Integer);

var
  i: Integer;

begin
  FParent := CreateSimpleParent;
  for i := 1 to aCount do
    FParent.AppendRecord([i, 'parent' + IntToStr(i)]);
  FParent.MergeChangeLog;
  FParent.First;
end;


function TTestNestedDataSet.NestedNames(aDataSet: TDataSet): string;

begin
  Result := '';
  aDataSet.First;
  while not aDataSet.EOF do
    begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + aDataSet.FieldByName('NAME').AsString;
    aDataSet.Next;
    end;
end;


function TTestNestedDataSet.SaveAndLoad(aDataSet: TBufDataset; aFormat: TDataPacketFormat): TBufDataset;

var
  lStream: TMemoryStream;

begin
  lStream := TMemoryStream.Create;
  try
    aDataSet.SaveToStream(lStream, aFormat);
    lStream.Position := 0;
    Result := TBufDataset.Create(nil);
    try
      Result.LoadFromStream(lStream, aFormat);
    except
      Result.Free;
      raise;
    end;
  finally
    lStream.Free;
  end;
end;


procedure TTestNestedDataSet.DoCalcFields(DataSet: TDataSet);

begin
  Inc(FCalcCount);
  DataSet.FieldByName('DOUBLED').AsInteger := DataSet.FieldByName('NUM').AsInteger * 2;
end;


procedure TTestNestedDataSet.DoFilterName(DataSet: TDataSet; var Accept: Boolean);

begin
  Accept := Pos(FFilterName, DataSet.FieldByName('NAME').AsString) > 0;
end;


{ structure and API }

procedure TTestNestedDataSet.TestFieldClass;

var
  lField: TField;

begin
  FParent := CreateSimpleParent;
  lField := FParent.FieldByName('DETAILS');
  AssertEquals('Field class', 'TDataSetField', lField.ClassName);
  AssertTrue('DataType', lField.DataType = ftDataSet);
  AssertFalse('IsBlob', lField.IsBlob);
  AssertEquals('DataSize', 0, lField.DataSize);
  AssertEquals('Child definitions', 4, FParent.FieldDefs[2].ChildDefs.Count);
end;


procedure TTestNestedDataSet.TestFieldDefListAndFieldList;

begin
  FParent := CreateSimpleParent;
  AssertEquals('FieldDefList does not list nested children', 3, FParent.FieldDefList.Count);
  AssertEquals('FieldList does not list nested fields', 3, FParent.FieldList.Count);
  AssertEquals('Fields', 3, FParent.Fields.Count);
end;


procedure TTestNestedDataSet.TestNestedDataSetCreatedWhenUsed;

var
  lField: TDataSetField;
  lNested: TDataSet;

begin
  FParent := CreateSimpleParent;
  lField := FParent.FieldByName('DETAILS') as TDataSetField;
  AssertEquals('No nested dataset before use', 0, FParent.NestedDataSets.Count);
  lNested := lField.NestedDataSet;
  AssertNotNull('Nested dataset', lNested);
  AssertEquals('Nested dataset class', 'TBufDataset', lNested.ClassName);
  AssertSame('DataSetField', lField, lNested.DataSetField);
  AssertEquals('Listed in NestedDataSets', 1, FParent.NestedDataSets.Count);
  AssertTrue('Opened with the parent', lNested.Active);
  AssertSame('Same nested dataset on next use', lNested, lField.NestedDataSet);
  AssertEquals('Fields of the field are the nested fields', 4, lField.Fields.Count);
  AssertSame('Fields of the field', lNested.Fields, lField.Fields);
  AssertSame('Owned by the field', lField, lNested.Owner);
end;


procedure TTestNestedDataSet.TestNestedFieldDefsFromChildDefs;

var
  lNested: TDataSet;
  lDefs: TFieldDefs;
  i: Integer;

begin
  FParent := CreateAllTypesParent;
  lNested := Nested;
  lDefs := FParent.FieldDefs[3].ChildDefs;
  AssertEquals('Definition count', lDefs.Count, lNested.FieldDefs.Count);
  for i := 0 to lDefs.Count - 1 do
    begin
    AssertEquals('Name ' + IntToStr(i), lDefs[i].Name, lNested.FieldDefs[i].Name);
    AssertTrue('DataType of ' + lDefs[i].Name, lDefs[i].DataType = lNested.FieldDefs[i].DataType);
    AssertEquals('Size of ' + lDefs[i].Name, lDefs[i].Size, lNested.FieldDefs[i].Size);
    AssertEquals('Child definitions of ' + lDefs[i].Name, lDefs[i].ChildDefs.Count, lNested.FieldDefs[i].ChildDefs.Count);
    end;
  AssertEquals('Precision', 18, lNested.FieldDefs.Find('FFMTBCD').Precision);
end;


procedure TTestNestedDataSet.TestAssignDataSetField;

var
  lNested: TBufDataset;
  lField: TDataSetField;

begin
  FParent := CreateSimpleParent;
  FParent.AppendRecord([1, 'one']);
  lField := FParent.FieldByName('DETAILS') as TDataSetField;
  lNested := TBufDataset.Create(nil);
  try
    lNested.DataSetField := lField;
    AssertTrue('Opened because the parent is open', lNested.Active);
    AssertSame('NestedDataSet', lNested, lField.NestedDataSet);
    AssertSame('Fields', lNested.Fields, lField.Fields);
    AssertEquals('NestedDataSets', 1, FParent.NestedDataSets.Count);
    AppendSimple(lNested, 1, 'a');
    AssertEquals('Row stored', 1, lNested.RecordCount);
    lNested.DataSetField := nil;
    AssertFalse('Closed when detached', lNested.Active);
    AssertEquals('Removed from NestedDataSets', 0, FParent.NestedDataSets.Count);
    lNested.DataSetField := lField;
    AssertEquals('Row still in the parent', 1, lNested.RecordCount);
    AssertEquals('Name', 'a', lNested.FieldByName('NAME').AsString);
  finally
    lNested.Free;
  end;
  AssertEquals('Removed when freed', 0, FParent.NestedDataSets.Count);
end;


procedure TTestNestedDataSet.TestCircularLink;

var
  lRaised: Boolean;

begin
  FParent := CreateSimpleParent;
  lRaised := False;
  try
    FParent.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('A dataset can not be nested in its own field', lRaised);
end;


procedure TTestNestedDataSet.TestNestedDataSetClass;

var
  lNested: TBufDataset;
  lRaised: Boolean;

begin
  FParent := CreateSimpleParent;
  FParent.NestedDataSetClass := TDerivedBufDataset;
  lNested := TBufDataset.Create(nil);
  try
    lRaised := False;
    try
      lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
    except
      on EDatabaseError do
        lRaised := True;
    end;
    AssertTrue('A nested dataset that does not inherit from NestedDataSetClass raises', lRaised);
  finally
    lNested.Free;
  end;
  AssertEquals('Created nested dataset has NestedDataSetClass', 'TDerivedBufDataset', Nested.ClassName);
end;


procedure TTestNestedDataSet.TestOpenCloseWithParent;

var
  lNested: TBufDataset;

begin
  AddSimpleParents(1);
  lNested := Nested;
  AppendSimple(lNested, 1, 'a');
  FParent.Post;
  FParent.Close;
  AssertFalse('Closed with the parent', lNested.Active);
  FParent.Open;
  AssertTrue('Opened with the parent', lNested.Active);
  AssertTrue('A reopened TBufDataset without file has no records', lNested.IsEmpty);
  FParent.AppendRecord([1, 'again']);
  AppendSimple(lNested, 2, 'b');
  AssertEquals('Rows after reopening', 'b', NestedNames(lNested));
end;


procedure TTestNestedDataSet.TestOpenWithoutParent;

var
  lNested: TBufDataset;
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  lNested := Nested;
  FParent.Close;
  lRaised := False;
  try
    lNested.Open;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('Opening a nested dataset of a closed parent raises', lRaised);
end;


procedure TTestNestedDataSet.TestFieldValueProperties;

var
  lField: TDataSetField;
  lValue: Variant;
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  lField := FParent.FieldByName('DETAILS') as TDataSetField;
  AssertTrue('IsNull without rows', lField.IsNull);
  AssertEquals('AsString', '(DataSet)', lField.AsString);
  AssertTrue('AsVariant without rows', VarIsNull(lField.AsVariant));
  AssertTrue('CanModify', lField.CanModify);
  AppendSimple(Nested, 5, 'five');
  AssertFalse('IsNull with rows', lField.IsNull);
  lValue := lField.AsVariant;
  AssertTrue('AsVariant is an array', VarIsArray(lValue));
  AssertEquals('AsVariant element', 'five', VarToStr(lValue[1]));
  lRaised := False;
  try
    lField.AsVariant := 1;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('Assigning a variant raises', lRaised);
  Nested.Delete;
  AssertTrue('IsNull after deleting the rows', lField.IsNull);
end;


procedure TTestNestedDataSet.TestReadOnlyParent;

var
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.ReadOnly := True;
  AssertFalse('Nested dataset of a read-only parent', Nested.CanModify);
  lRaised := False;
  try
    Nested.Edit;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('Editing raises', lRaised);
end;


procedure TTestNestedDataSet.TestReadOnlyDataSetField;

begin
  AddSimpleParents(1);
  FParent.FieldByName('DETAILS').ReadOnly := True;
  AssertFalse('Nested dataset of a read-only dataset field', Nested.CanModify);
end;


procedure TTestNestedDataSet.TestFreeParentFirst;

var
  lNested: TBufDataset;

begin
  AddSimpleParents(1);
  lNested := TBufDataset.Create(nil);
  try
    lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
    AppendSimple(lNested, 1, 'a');
    FreeAndNil(FParent);
    AssertNull('Detached when the parent is freed', lNested.DataSetField);
    AssertFalse('Closed when the parent is freed', lNested.Active);
  finally
    lNested.Free;
  end;
end;


procedure TTestNestedDataSet.TestFreeNestedFirst;

var
  lNested: TBufDataset;

begin
  AddSimpleParents(1);
  lNested := TBufDataset.Create(nil);
  lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
  AppendSimple(lNested, 1, 'a');
  lNested.Free;
  AssertEquals('Removed from NestedDataSets', 0, FParent.NestedDataSets.Count);
  AssertEquals('Rows stay in the parent', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestObjectViewFalse;

begin
  FParent := TBufDataset.Create(nil);
  FParent.ObjectView := False;
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'PT';
    DataType := ftADT;
    ChildDefs.Add('X', ftInteger);
    end;
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    AddSimpleDefs(ChildDefs);
    end;
  FParent.CreateDataset;
  AssertNull('ADT flattened', FParent.FindField('PT'));
  AssertNotNull('ADT member is a top-level field', FParent.FindField('PT.X'));
  AssertTrue('Dataset field is not flattened', FParent.FieldByName('DETAILS') is TDataSetField);
  AssertNull('Nested fields are not top-level fields', FParent.FindField('DETAILS.NAME'));
  FParent.AppendRecord([1]);
  AppendSimple(Nested, 1, 'a');
  AssertEquals('Nested rows', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestPersistentFieldsInNested;

var
  lNested: TBufDataset;
  lField: TField;

begin
  AddSimpleParents(1);
  lNested := TBufDataset.Create(nil);
  try
    lField := TStringField.Create(lNested);
    lField.FieldName := 'NAME';
    lField.Size := 20;
    lField.DataSet := lNested;
    lField := TIntegerField.Create(lNested);
    lField.FieldName := 'NUM';
    lField.DataSet := lNested;
    lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
    AssertEquals('Only the persistent fields', 2, lNested.Fields.Count);
    AppendSimple(lNested, 3, 'c');
    lNested.First;
    AssertEquals('Value', 3, lNested.FieldByName('NUM').AsInteger);
  finally
    lNested.Free;
  end;
  AssertEquals('Row in the created nested dataset', 'c', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestCalculatedFieldInNested;

var
  lNested: TBufDataset;
  lField: TField;
  i: Integer;

begin
  AddSimpleParents(1);
  lNested := TBufDataset.Create(nil);
  try
    lNested.FieldDefs.Assign(FParent.FieldDefs[2].ChildDefs);
    for i := 0 to lNested.FieldDefs.Count - 1 do
      lNested.FieldDefs[i].CreateField(lNested);
    lField := TIntegerField.Create(lNested);
    lField.FieldName := 'DOUBLED';
    lField.FieldKind := fkCalculated;
    lField.DataSet := lNested;
    lNested.OnCalcFields := @DoCalcFields;
    lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
    AppendSimple(lNested, 21, 'x');
    lNested.First;
    AssertEquals('Calculated value', 42, lNested.FieldByName('DOUBLED').AsInteger);
    AssertTrue('OnCalcFields called', FCalcCount > 0);
  finally
    lNested.Free;
  end;
end;


procedure TTestNestedDataSet.TestLookupFieldInNested;

var
  lLookup, lNested: TBufDataset;
  lField: TField;
  i: Integer;

begin
  AddSimpleParents(1);
  lLookup := TBufDataset.Create(nil);
  lNested := TBufDataset.Create(nil);
  try
    lLookup.FieldDefs.Add('NUM', ftInteger);
    lLookup.FieldDefs.Add('TEXT', ftString, 20);
    lLookup.CreateDataset;
    lLookup.AppendRecord([1, 'one']);
    lLookup.AppendRecord([2, 'two']);
    lNested.FieldDefs.Assign(FParent.FieldDefs[2].ChildDefs);
    for i := 0 to lNested.FieldDefs.Count - 1 do
      lNested.FieldDefs[i].CreateField(lNested);
    lField := TStringField.Create(lNested);
    lField.FieldName := 'NUMTEXT';
    lField.FieldKind := fkLookup;
    lField.Size := 20;
    lField.KeyFields := 'NUM';
    lField.LookupDataSet := lLookup;
    lField.LookupKeyFields := 'NUM';
    lField.LookupResultField := 'TEXT';
    lField.DataSet := lNested;
    lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
    AppendSimple(lNested, 2, 'b');
    lNested.First;
    AssertEquals('Lookup value', 'two', lNested.FieldByName('NUMTEXT').AsString);
  finally
    lNested.Free;
    lLookup.Free;
  end;
end;


procedure TTestNestedDataSet.TestIsLinkedTo;

var
  lSource: TDataSource;

begin
  AddSimpleParents(1);
  lSource := TDataSource.Create(nil);
  try
    lSource.DataSet := Nested;
    AssertTrue('Parent is linked to a data source of its nested dataset', FParent.IsLinkedTo(lSource));
    lSource.DataSet := FParent;
    AssertTrue('Linked to its own data source', FParent.IsLinkedTo(lSource));
    AssertFalse('Nested dataset is not linked to the parent data source', Nested.IsLinkedTo(lSource));
  finally
    lSource.Free;
  end;
end;


{ values of every field type }

procedure TTestNestedDataSet.TestTypeString;
begin
  CheckType('FSTRING');
end;

procedure TTestNestedDataSet.TestTypeFixedChar;
begin
  CheckType('FFIXEDCHAR');
end;

procedure TTestNestedDataSet.TestTypeWideString;
begin
  CheckType('FWIDESTRING');
end;

procedure TTestNestedDataSet.TestTypeFixedWideChar;
begin
  CheckType('FFIXEDWIDECHAR');
end;

procedure TTestNestedDataSet.TestTypeSmallint;
begin
  CheckType('FSMALLINT');
end;

procedure TTestNestedDataSet.TestTypeInteger;
begin
  CheckType('FINTEGER');
end;

procedure TTestNestedDataSet.TestTypeWord;
begin
  CheckType('FWORD');
end;

procedure TTestNestedDataSet.TestTypeLongWord;
begin
  CheckType('FLONGWORD');
end;

procedure TTestNestedDataSet.TestTypeShortint;
begin
  CheckType('FSHORTINT');
end;

procedure TTestNestedDataSet.TestTypeByte;
begin
  CheckType('FBYTE');
end;

procedure TTestNestedDataSet.TestTypeLargeint;
begin
  CheckType('FLARGEINT');
end;

procedure TTestNestedDataSet.TestTypeBoolean;
begin
  CheckType('FBOOLEAN');
end;

procedure TTestNestedDataSet.TestTypeFloat;
begin
  CheckType('FFLOAT');
end;

procedure TTestNestedDataSet.TestTypeExtended;
begin
  CheckType('FEXTENDED');
end;

procedure TTestNestedDataSet.TestTypeSingle;
begin
  CheckType('FSINGLE');
end;

procedure TTestNestedDataSet.TestTypeCurrency;
begin
  CheckType('FCURRENCY');
end;

procedure TTestNestedDataSet.TestTypeBCD;
begin
  CheckType('FBCD');
end;

procedure TTestNestedDataSet.TestTypeFmtBCD;
begin
  CheckType('FFMTBCD');
end;

procedure TTestNestedDataSet.TestTypeDate;
begin
  CheckType('FDATE');
end;

procedure TTestNestedDataSet.TestTypeTime;
begin
  CheckType('FTIME');
end;

procedure TTestNestedDataSet.TestTypeDateTime;
begin
  CheckType('FDATETIME');
end;

procedure TTestNestedDataSet.TestTypeBytes;
begin
  CheckType('FBYTES');
end;

procedure TTestNestedDataSet.TestTypeVarBytes;
begin
  CheckType('FVARBYTES');
end;

procedure TTestNestedDataSet.TestTypeAutoInc;

var
  lNested: TDataSet;
  lLast, p: Integer;

begin
  FillParents(3, 4);
  lNested := Nested;
  lLast := 0;
  FParent.First;
  for p := 0 to 2 do
    begin
    lNested.First;
    while not lNested.EOF do
      begin
      AssertTrue('AutoInc values increase over all sets', lNested.FieldByName('FAUTOINC').AsInteger > lLast);
      lLast := lNested.FieldByName('FAUTOINC').AsInteger;
      lNested.Next;
      end;
    FParent.Next;
    end;
  AssertEquals('Last AutoInc value', 4 + 3 + 2, lLast);
end;

procedure TTestNestedDataSet.TestTypeGuid;
begin
  CheckType('FGUID');
end;

procedure TTestNestedDataSet.TestTypeVariant;
begin
  CheckType('FVARIANT');
end;

procedure TTestNestedDataSet.TestTypeBlob;
begin
  CheckType('FBLOB');
end;

procedure TTestNestedDataSet.TestTypeMemo;
begin
  CheckType('FMEMO');
end;

procedure TTestNestedDataSet.TestTypeGraphic;
begin
  CheckType('FGRAPHIC');
end;

procedure TTestNestedDataSet.TestTypeFmtMemo;
begin
  CheckType('FFMTMEMO');
end;

procedure TTestNestedDataSet.TestTypeTypedBinary;
begin
  CheckType('FTYPEDBINARY');
end;

procedure TTestNestedDataSet.TestTypeParadoxOle;
begin
  CheckType('FPARADOXOLE');
end;

procedure TTestNestedDataSet.TestTypeDBaseOle;
begin
  CheckType('FDBASEOLE');
end;

procedure TTestNestedDataSet.TestTypeOraBlob;
begin
  CheckType('FORABLOB');
end;

procedure TTestNestedDataSet.TestTypeOraClob;
begin
  CheckType('FORACLOB');
end;

procedure TTestNestedDataSet.TestTypeWideMemo;
begin
  CheckType('FWIDEMEMO');
end;

procedure TTestNestedDataSet.TestTypeADT;
begin
  CheckType('FADT');
end;

procedure TTestNestedDataSet.TestTypeArray;
begin
  CheckType('FARRAY');
end;

procedure TTestNestedDataSet.TestTypeDataSet;
begin
  CheckType('SUB');
end;

procedure TTestNestedDataSet.TestAllTypesTogether;
begin
  CheckType('');
end;


procedure TTestNestedDataSet.TestNullValues;

var
  lNested: TDataSet;
  i, lNull: Integer;

begin
  FParent := CreateAllTypesParent;
  FParent.AppendRecord([1, 'one']);
  lNested := Nested;
  lNested.Append;
  lNested.Post;
  FParent.Post;
  FParent.First;
  lNested.First;
  AssertEquals('One row', 1, lNested.RecordCount);
  for i := 0 to lNested.Fields.Count - 1 do
    if not (lNested.Fields[i].DataType in [ftAutoInc, ftDataSet, ftADT, ftArray]) then
      AssertTrue('Null: ' + lNested.Fields[i].FieldName, lNested.Fields[i].IsNull);
  // one field set, all others null
  for i := 0 to lNested.Fields.Count - 1 do
    if not (lNested.Fields[i].DataType in [ftAutoInc, ftDataSet]) then
      begin
      lNested.Edit;
      SetValue(lNested.Fields[i], 7);
      lNested.Post;
      FParent.Post;
      FParent.First;
      CheckValue(lNested.Fields[i], 7, 'single value');
      lNested.Edit;
      lNested.Fields[i].Clear;
      lNested.Post;
      FParent.Post;
      end;
  lNull := 0;
  for i := 0 to lNested.Fields.Count - 1 do
    if not (lNested.Fields[i].DataType in [ftAutoInc, ftDataSet, ftADT, ftArray]) and lNested.Fields[i].IsNull then
      Inc(lNull);
  AssertTrue('Values cleared again', lNull > 30);
end;


procedure TTestNestedDataSet.TestDifferentSetsPerParent;

begin
  AddSimpleParents(3);
  AppendSimple(Nested, 1, 'a1');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 2, 'b1');
  AppendSimple(Nested, 3, 'b2');
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertEquals('Parent 1', 'a1', NestedNames(Nested));
  FParent.Next;
  AssertEquals('Parent 2', 'b1,b2', NestedNames(Nested));
  FParent.Next;
  AssertEquals('Parent 3', '', NestedNames(Nested));
  AssertTrue('Parent 3 field is null', FParent.FieldByName('DETAILS').IsNull);
end;


procedure TTestNestedDataSet.TestEmptySetAndNull;

var
  lField: TField;

begin
  AddSimpleParents(1);
  lField := FParent.FieldByName('DETAILS');
  AssertTrue('Null before rows', lField.IsNull);
  AppendSimple(Nested, 1, 'a');
  AssertFalse('Not null with a row', lField.IsNull);
  Nested.Delete;
  AssertTrue('Null after deleting the last row', lField.IsNull);
  AssertTrue('Nested dataset is empty', Nested.IsEmpty);
end;


{ blobs }

procedure TTestNestedDataSet.TestBlobStreamWriteRead;

var
  lStream: TStream;
  lData: TBytes;
  lRead: TBytes;

begin
  AddSimpleParents(2);
  lData := BlobBytes(3, 5000);
  Nested.Append;
  Nested.FieldByName('NAME').AsString := 'blob';
  lStream := Nested.CreateBlobStream(Nested.FieldByName('B'), bmWrite);
  try
    lStream.WriteBuffer(lData[0], Length(lData));
  finally
    lStream.Free;
  end;
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  lStream := Nested.CreateBlobStream(Nested.FieldByName('B'), bmRead);
  try
    AssertEquals('Size', 5000, lStream.Size);
    SetLength(lRead, lStream.Size);
    lStream.ReadBuffer(lRead[0], Length(lRead));
  finally
    lStream.Free;
  end;
  AssertEquals('Last byte', lData[4999], lRead[4999]);
  AssertEquals('Middle byte', lData[2500], lRead[2500]);
end;


procedure TTestNestedDataSet.TestBlobAsBytes;

begin
  AddSimpleParents(2);
  Nested.Append;
  Nested.FieldByName('B').AsBytes := BlobBytes(9, 300);
  Nested.Post;
  FParent.Post;
  FParent.Last;
  FParent.First;
  AssertEquals('Length', 300, Length(Nested.FieldByName('B').AsBytes));
  AssertEquals('Byte 299', (9 + 299) mod 256, Nested.FieldByName('B').AsBytes[299]);
end;


procedure TTestNestedDataSet.TestBlobFieldLoadSaveStream;

var
  lIn, lOut: TMemoryStream;
  lData: TBytes;

begin
  AddSimpleParents(2);
  lIn := TMemoryStream.Create;
  lOut := TMemoryStream.Create;
  try
    lData := BlobBytes(1, 20000);
    lIn.WriteBuffer(lData[0], Length(lData));
    Nested.Append;
    TBlobField(Nested.FieldByName('B')).LoadFromStream(lIn);
    Nested.Post;
    FParent.Post;
    FParent.Next;
    FParent.Prior;
    TBlobField(Nested.FieldByName('B')).SaveToStream(lOut);
    AssertEquals('Size', lIn.Size, lOut.Size);
    AssertTrue('Same data', CompareMem(lIn.Memory, lOut.Memory, lIn.Size));
  finally
    lOut.Free;
    lIn.Free;
  end;
end;


procedure TTestNestedDataSet.TestBlobEmptyIsNull;

begin
  AddSimpleParents(1);
  Nested.Append;
  Nested.FieldByName('NAME').AsString := 'x';
  Nested.Post;
  AssertTrue('Blob never set is null', Nested.FieldByName('B').IsNull);
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'data';
  Nested.Post;
  AssertFalse('Blob with data', Nested.FieldByName('B').IsNull);
  Nested.Edit;
  Nested.FieldByName('B').AsString := '';
  Nested.Post;
  FParent.Post;
  FParent.First;
  AssertTrue('Empty blob is null', Nested.FieldByName('B').IsNull);
end;


procedure TTestNestedDataSet.TestBlobAllByteValues;

var
  lData, lRead: TBytes;
  i: Integer;

begin
  AddSimpleParents(2);
  SetLength(lData, 256 * 4);
  for i := 0 to High(lData) do
    lData[i] := i mod 256;
  Nested.Append;
  Nested.FieldByName('B').AsBytes := lData;
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  lRead := Nested.FieldByName('B').AsBytes;
  AssertEquals('Length', Length(lData), Length(lRead));
  for i := 0 to High(lData) do
    AssertEquals('Byte ' + IntToStr(i), lData[i], lRead[i]);
end;


procedure TTestNestedDataSet.TestBlobOneMegabyte;

var
  lData: TBytes;

begin
  AddSimpleParents(2);
  lData := BlobBytes(5, 1024 * 1024);
  Nested.Append;
  Nested.FieldByName('B').AsBytes := lData;
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertTrue('Same data', CompareMem(@lData[0], @Nested.FieldByName('B').AsBytes[0], Length(lData)));
end;


procedure TTestNestedDataSet.TestBlobSixteenMegabytes;

var
  lData, lRead: TBytes;
  lLoaded: TBufDataset;

begin
  AddSimpleParents(2);
  lData := BlobBytes(11, 16 * 1024 * 1024);
  Nested.Append;
  Nested.FieldByName('B').AsBytes := lData;
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  lRead := Nested.FieldByName('B').AsBytes;
  AssertEquals('Length', Length(lData), Length(lRead));
  AssertTrue('Same data', CompareMem(@lData[0], @lRead[0], Length(lData)));
  lLoaded := SaveAndLoad(FParent, dfBinary);
  try
    lRead := NestedOf(lLoaded).FieldByName('B').AsBytes;
    AssertEquals('Length after streaming', Length(lData), Length(lRead));
    AssertTrue('Same data after streaming', CompareMem(@lData[0], @lRead[0], Length(lData)));
  finally
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestMemoUTF8;

var
  lText: UTF8String;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    ChildDefs.Add('M', ftMemo, 0, -1, False, False, 1, CP_UTF8);
    end;
  FParent.CreateDataset;
  FParent.AppendRecord([1]);
  FParent.AppendRecord([2]);
  FParent.First;
  lText := UTF8Encode(UnicodeString('Gr' + #$00FC#$00DF + 'e ' + #$4E16#$754C + ' ' + #$D83D#$DE00));
  Nested.Append;
  Nested.FieldByName('M').AsUTF8String := lText;
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertEquals('UTF-8 memo', lText, Nested.FieldByName('M').AsUTF8String);
end;


procedure TTestNestedDataSet.TestMemoCodePage;

var
  lText: RawByteString;
  lLoaded: TBufDataset;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    ChildDefs.Add('M', ftMemo, 0, -1, False, False, 1, 1252);
    end;
  FParent.CreateDataset;
  AssertEquals('Code page of the nested definition', 1252, Nested.FieldDefs[0].CodePage);
  FParent.AppendRecord([1]);
  lText := 'caf' + #$E9;
  SetCodePage(lText, 1252, False);
  Nested.Append;
  TMemoField(Nested.FieldByName('M')).AsAnsiString := lText;
  Nested.Post;
  FParent.Post;
  lLoaded := SaveAndLoad(FParent, dfBinary);
  try
    AssertEquals('Code page after streaming', 1252, NestedOf(lLoaded).FieldDefs[0].CodePage);
    AssertEquals('UTF-8 value after streaming', UTF8Encode(UnicodeString('caf' + #$00E9)),
      NestedOf(lLoaded).FieldByName('M').AsUTF8String);
  finally
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestWideMemoOutsideBMP;

var
  lText: UnicodeString;

begin
  FParent := CreateAllTypesParent;
  FParent.AppendRecord([1]);
  FParent.AppendRecord([2]);
  FParent.First;
  lText := WideText(3) + #$D834#$DD1E;
  Nested.Append;
  Nested.FieldByName('FWIDEMEMO').AsUnicodeString := lText;
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertEquals('Wide memo', lText, Nested.FieldByName('FWIDEMEMO').AsUnicodeString);
end;


procedure TTestNestedDataSet.TestSeveralBlobsPerRow;

begin
  FParent := CreateAllTypesParent;
  FParent.AppendRecord([1]);
  FParent.AppendRecord([2]);
  FParent.First;
  Nested.Append;
  Nested.FieldByName('FBLOB').AsBytes := BlobBytes(1, 100);
  Nested.FieldByName('FMEMO').AsString := 'memo';
  Nested.FieldByName('FGRAPHIC').AsBytes := BlobBytes(2, 200);
  Nested.FieldByName('FWIDEMEMO').AsUnicodeString := WideText(1);
  Nested.FieldByName('FORACLOB').AsString := 'clob';
  Nested.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertEquals('Blob', 100, Length(Nested.FieldByName('FBLOB').AsBytes));
  AssertEquals('Memo', 'memo', Nested.FieldByName('FMEMO').AsString);
  AssertEquals('Graphic', 200, Length(Nested.FieldByName('FGRAPHIC').AsBytes));
  AssertEquals('Graphic byte', 2, Nested.FieldByName('FGRAPHIC').AsBytes[0]);
  AssertEquals('Wide memo', WideText(1), Nested.FieldByName('FWIDEMEMO').AsUnicodeString);
  AssertEquals('Clob', 'clob', Nested.FieldByName('FORACLOB').AsString);
end;


procedure TTestNestedDataSet.TestBlobEditNestedCancel;

begin
  AddSimpleParents(1);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'old';
  Nested.Post;
  FParent.Post;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'new';
  Nested.Cancel;
  AssertEquals('Nested Cancel restores the blob', 'old', Nested.FieldByName('B').AsString);
  FParent.Cancel;
  FParent.First;
  AssertEquals('Blob after parent cancel', 'old', Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobEditParentCancel;

begin
  AddSimpleParents(1);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'old';
  Nested.Post;
  FParent.Post;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'new';
  Nested.Post;
  FParent.Cancel;
  Nested.First;
  AssertEquals('A posted nested change stays after the parent cancel', 'new', Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobEditParentPostScroll;

begin
  AddSimpleParents(2);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'first';
  Nested.Post;
  FParent.Post;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'second';
  Nested.Post;
  FParent.Post;
  FParent.Next;
  AssertTrue('Other parent has no rows', Nested.IsEmpty);
  FParent.First;
  AssertEquals('Blob after post and scroll', 'second', Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobRevertRecord;

begin
  AddSimpleParents(1);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'original';
  Nested.Post;
  FParent.Post;
  FParent.MergeChangeLog;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'changed';
  Nested.Post;
  FParent.Post;
  FParent.RevertRecord;
  AssertEquals('RevertRecord of the parent restores the blob', 'original', Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobCancelUpdates;

begin
  AddSimpleParents(2);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'original';
  Nested.Post;
  FParent.Post;
  FParent.MergeChangeLog;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'changed';
  Nested.Post;
  AppendSimple(Nested, 2, 'added');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 3, 'other');
  FParent.Post;
  FParent.CancelUpdates;
  FParent.First;
  AssertEquals('Rows of parent 1', '', Nested.FieldByName('NAME').AsString);
  AssertEquals('One row', 1, Nested.RecordCount);
  AssertEquals('Blob restored', 'original', Nested.FieldByName('B').AsString);
  FParent.Next;
  AssertTrue('Parent 2 has no rows', Nested.IsEmpty);
end;


procedure TTestNestedDataSet.TestBlobMergeChangeLog;

begin
  AddSimpleParents(1);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'v1';
  Nested.Post;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'v2';
  Nested.Post;
  FParent.Post;
  FParent.MergeChangeLog;
  AssertEquals('Change count', 0, FParent.ChangeCount);
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'v3';
  Nested.Post;
  FParent.Post;
  FParent.MergeChangeLog;
  FParent.First;
  AssertEquals('Blob after merging', 'v3', Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobCancelUpdatesThenMerge;

var
  lDS: TBufDataset;

begin
  lDS := TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('ID', ftInteger);
    lDS.FieldDefs.Add('B', ftBlob);
    lDS.CreateDataset;
    lDS.Append;
    lDS.FieldByName('ID').AsInteger := 1;
    lDS.FieldByName('B').AsString := 'original';
    lDS.Post;
    lDS.MergeChangeLog;
    lDS.Edit;
    lDS.FieldByName('B').AsString := 'changed';
    lDS.Post;
    lDS.CancelUpdates;
    lDS.MergeChangeLog;
    AssertEquals('Blob of a plain dataset after CancelUpdates and MergeChangeLog', 'original', lDS.FieldByName('B').AsString);
  finally
    lDS.Free;
  end;
  AddSimpleParents(1);
  Nested.Append;
  Nested.FieldByName('B').AsString := 'original';
  Nested.Post;
  FParent.Post;
  FParent.MergeChangeLog;
  Nested.Edit;
  Nested.FieldByName('B').AsString := 'changed';
  Nested.Post;
  FParent.Post;
  FParent.CancelUpdates;
  FParent.MergeChangeLog;
  FParent.First;
  AssertEquals('Nested blob after CancelUpdates and MergeChangeLog', 'original', Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobStreamBinary;

var
  lLoaded: TBufDataset;

begin
  AddSimpleParents(2);
  Nested.Append;
  Nested.FieldByName('NAME').AsString := 'b';
  Nested.FieldByName('B').AsBytes := BlobBytes(77, 70000);
  Nested.Post;
  FParent.Post;
  lLoaded := SaveAndLoad(FParent, dfBinary);
  try
    lLoaded.First;
    AssertEquals('Name', 'b', NestedOf(lLoaded).FieldByName('NAME').AsString);
    AssertEquals('Blob length', 70000, Length(NestedOf(lLoaded).FieldByName('B').AsBytes));
    AssertEquals('Blob byte', (77 + 69999) mod 256, NestedOf(lLoaded).FieldByName('B').AsBytes[69999]);
    lLoaded.Next;
    AssertTrue('Parent 2 empty', NestedOf(lLoaded).IsEmpty);
  finally
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestBlobStreamXML;

var
  lLoaded: TBufDataset;

begin
  AddSimpleParents(2);
  Nested.Append;
  Nested.FieldByName('NAME').AsString := 'b';
  Nested.FieldByName('B').AsBytes := BlobBytes(33, 7000);
  Nested.Post;
  FParent.Post;
  lLoaded := SaveAndLoad(FParent, dfXML);
  try
    lLoaded.First;
    AssertEquals('Name', 'b', NestedOf(lLoaded).FieldByName('NAME').AsString);
    AssertEquals('Blob length', 7000, Length(NestedOf(lLoaded).FieldByName('B').AsBytes));
    AssertEquals('Blob byte', (33 + 6999) mod 256, NestedOf(lLoaded).FieldByName('B').AsBytes[6999]);
  finally
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestBlobManyEdits;

var
  i: Integer;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  for i := 1 to 1000 do
    begin
    Nested.Edit;
    Nested.FieldByName('B').AsString := 'edit ' + IntToStr(i) + StringOfChar('e', i);
    Nested.Post;
    end;
  FParent.Post;
  FParent.First;
  AssertEquals('Last edit', 'edit 1000' + StringOfChar('e', 1000), Nested.FieldByName('B').AsString);
  FParent.MergeChangeLog;
  AssertEquals('Last edit after merge', 'edit 1000' + StringOfChar('e', 1000), Nested.FieldByName('B').AsString);
end;


procedure TTestNestedDataSet.TestBlobSecondLevel;

var
  lSub: TBufDataset;
  lLoaded: TBufDataset;

begin
  FParent := CreateAllTypesParent;
  FParent.AppendRecord([1]);
  FParent.AppendRecord([2]);
  FParent.First;
  Nested.Append;
  Nested.FieldByName('FSTRING').AsString := 'row';
  Nested.Post;
  lSub := NestedOf(Nested, 'SUB');
  lSub.Append;
  lSub.FieldByName('SBLOB').AsBytes := BlobBytes(4, 12345);
  lSub.Post;
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertEquals('Second level blob', 12345, Length(lSub.FieldByName('SBLOB').AsBytes));
  lLoaded := SaveAndLoad(FParent, dfBinary);
  try
    lLoaded.First;
    AssertEquals('Second level blob after streaming', 12345,
      Length(NestedOf(NestedOf(lLoaded), 'SUB').FieldByName('SBLOB').AsBytes));
  finally
    lLoaded.Free;
  end;
end;


{ operations on the nested dataset }

procedure TTestNestedDataSet.TestNavigation;

var
  i: Integer;

begin
  AddSimpleParents(1);
  for i := 1 to 5 do
    AppendSimple(Nested, i, 'n' + IntToStr(i));
  Nested.First;
  AssertTrue('BOF', Nested.BOF);
  AssertEquals('First', 1, Nested.FieldByName('NUM').AsInteger);
  Nested.Next;
  AssertEquals('Next', 2, Nested.FieldByName('NUM').AsInteger);
  Nested.Last;
  AssertTrue('EOF after Last', Nested.EOF);
  AssertEquals('Last', 5, Nested.FieldByName('NUM').AsInteger);
  Nested.Prior;
  AssertEquals('Prior', 4, Nested.FieldByName('NUM').AsInteger);
  AssertEquals('MoveBy', -2, Nested.MoveBy(-2));
  AssertEquals('After MoveBy', 2, Nested.FieldByName('NUM').AsInteger);
  AssertEquals('RecordCount', 5, Nested.RecordCount);
end;


procedure TTestNestedDataSet.TestRecNo;

var
  i: Integer;

begin
  AddSimpleParents(1);
  for i := 1 to 5 do
    AppendSimple(Nested, i, 'n' + IntToStr(i));
  Nested.First;
  AssertEquals('RecNo first', 1, Nested.RecNo);
  Nested.RecNo := 4;
  AssertEquals('Value after setting RecNo', 4, Nested.FieldByName('NUM').AsInteger);
  AssertEquals('RecNo', 4, Nested.RecNo);
end;


procedure TTestNestedDataSet.TestInsertPositions;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 2, 'b');
  AppendSimple(Nested, 4, 'd');
  Nested.First;
  Nested.Insert;
  Nested.FieldByName('NAME').AsString := 'a';
  Nested.Post;
  Nested.Last;
  Nested.Insert;
  Nested.FieldByName('NAME').AsString := 'c';
  Nested.Post;
  Nested.Append;
  Nested.FieldByName('NAME').AsString := 'e';
  Nested.Post;
  AssertEquals('Order', 'a,b,c,d,e', NestedNames(Nested));
  FParent.Post;
  FParent.First;
  AssertEquals('Order after reload', 'a,b,c,d,e', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestEditPostCancel;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'b';
  Nested.Post;
  AssertEquals('Posted', 'b', Nested.FieldByName('NAME').AsString);
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'c';
  Nested.Cancel;
  AssertEquals('Cancelled', 'b', Nested.FieldByName('NAME').AsString);
  FParent.Post;
  FParent.First;
  AssertEquals('After reload', 'b', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestDeletePositions;

var
  i: Integer;

begin
  AddSimpleParents(1);
  for i := 1 to 5 do
    AppendSimple(Nested, i, 'n' + IntToStr(i));
  Nested.First;
  Nested.Delete;
  Nested.RecNo := 2;
  Nested.Delete;
  Nested.Last;
  Nested.Delete;
  AssertEquals('Remaining', 'n2,n4', NestedNames(Nested));
  FParent.First;
  AssertEquals('Remaining after reload', 'n2,n4', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestDeleteAll;

var
  i: Integer;

begin
  AddSimpleParents(1);
  for i := 1 to 5 do
    AppendSimple(Nested, i, 'n' + IntToStr(i));
  Nested.First;
  while not Nested.IsEmpty do
    Nested.Delete;
  AssertEquals('RecordCount', 0, Nested.RecordCount);
  AssertTrue('Field is null', FParent.FieldByName('DETAILS').IsNull);
  AppendSimple(Nested, 9, 'again');
  AssertEquals('New row after deleting all', 'again', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestDeleteEmpty;

var
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  lRaised := False;
  try
    Nested.Delete;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('Delete in an empty nested dataset raises', lRaised);
end;


procedure TTestNestedDataSet.TestBookmarks;

var
  lMark2, lMark4: TBookmark;
  i: Integer;

begin
  AddSimpleParents(1);
  for i := 1 to 5 do
    AppendSimple(Nested, i, 'n' + IntToStr(i));
  Nested.RecNo := 2;
  lMark2 := Nested.GetBookmark;
  Nested.RecNo := 4;
  lMark4 := Nested.GetBookmark;
  try
    AssertTrue('Valid', Nested.BookmarkValid(lMark2));
    AssertEquals('Compare', -1, Nested.CompareBookmarks(lMark2, lMark4));
    Nested.GotoBookmark(lMark2);
    AssertEquals('After GotoBookmark', 2, Nested.FieldByName('NUM').AsInteger);
    Nested.GotoBookmark(lMark4);
    AssertEquals('After second GotoBookmark', 4, Nested.FieldByName('NUM').AsInteger);
  finally
    Nested.FreeBookmark(lMark2);
    Nested.FreeBookmark(lMark4);
  end;
end;


procedure TTestNestedDataSet.TestBookmarkAfterParentScroll;

var
  lMark: TBookmark;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  lMark := Nested.GetBookmark;
  try
    AssertTrue('Valid before the parent scrolls', Nested.BookmarkValid(lMark));
    FParent.Next;
    AssertFalse('Invalid after the parent scrolls', Nested.BookmarkValid(lMark));
  finally
    Nested.FreeBookmark(lMark);
  end;
end;


procedure TTestNestedDataSet.TestLocate;

var
  i: Integer;

begin
  AddSimpleParents(1);
  for i := 1 to 10 do
    AppendSimple(Nested, i * 10, 'name' + IntToStr(i));
  AssertTrue('Locate on an integer', Nested.Locate('NUM', 70, []));
  AssertEquals('Located row', 'name7', Nested.FieldByName('NAME').AsString);
  AssertTrue('Locate on two fields', Nested.Locate('NUM;NAME', VarArrayOf([30, 'name3']), []));
  AssertEquals('Located row on two fields', 30, Nested.FieldByName('NUM').AsInteger);
  AssertFalse('Locate of a missing value', Nested.Locate('NUM', 75, []));
end;


procedure TTestNestedDataSet.TestLocateOptions;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'Alpha');
  AppendSimple(Nested, 2, 'Beta');
  AppendSimple(Nested, 3, 'Gamma');
  AssertTrue('Case insensitive', Nested.Locate('NAME', 'beta', [loCaseInsensitive]));
  AssertEquals('Row', 2, Nested.FieldByName('NUM').AsInteger);
  AssertTrue('Partial key', Nested.Locate('NAME', 'Gam', [loPartialKey]));
  AssertEquals('Row', 3, Nested.FieldByName('NUM').AsInteger);
  AssertFalse('Case sensitive', Nested.Locate('NAME', 'alpha', []));
end;


procedure TTestNestedDataSet.TestLookup;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'one');
  AppendSimple(Nested, 2, 'two');
  AssertEquals('Lookup', 'two', VarToStr(Nested.Lookup('NUM', 2, 'NAME')));
  AssertTrue('Lookup of a missing value', VarIsNull(Nested.Lookup('NUM', 3, 'NAME')));
end;


procedure TTestNestedDataSet.TestFilterExpression;

var
  i: Integer;

begin
  AddSimpleParents(2);
  for i := 1 to 10 do
    AppendSimple(Nested, i, 'n' + IntToStr(i));
  FParent.Post;
  Nested.Filter := 'NUM > 7';
  Nested.Filtered := True;
  AssertEquals('Filtered rows', 'n8,n9,n10', NestedNames(Nested));
  AssertEquals('Filtered RecordCount', 3, Nested.RecordCount);
  FParent.Next;
  AssertEquals('Filter on the empty set', '', NestedNames(Nested));
  FParent.First;
  AssertEquals('Filter kept after the parent scrolls', 'n8,n9,n10', NestedNames(Nested));
  Nested.Filtered := False;
  AssertEquals('Unfiltered RecordCount', 10, Nested.RecordCount);
end;


procedure TTestNestedDataSet.TestOnFilterRecord;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'apple');
  AppendSimple(Nested, 2, 'banana');
  AppendSimple(Nested, 3, 'pear');
  FFilterName := 'a';
  Nested.OnFilterRecord := @DoFilterName;
  Nested.Filtered := True;
  AssertEquals('Filtered with OnFilterRecord', 'apple,banana,pear', NestedNames(Nested));
  FFilterName := 'an';
  Nested.Filtered := False;
  Nested.Filtered := True;
  AssertEquals('Filtered with OnFilterRecord', 'banana', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestIndexFieldNames;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 3, 'c');
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 2, 'b');
  Nested.IndexFieldNames := 'NUM';
  AssertEquals('Sorted', 'a,b,c', NestedNames(Nested));
  Nested.IndexFieldNames := '';
  AssertEquals('Unsorted', 'c,a,b', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestAddIndex;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 2, 'b');
  AppendSimple(Nested, 1, 'C');
  AppendSimple(Nested, 3, 'a');
  Nested.AddIndex('BYNUMDESC', 'NUM', [ixDescending], 'NUM');
  Nested.AddIndex('BYNAME', 'NAME', [ixCaseInsensitive]);
  Nested.IndexName := 'BYNUMDESC';
  AssertEquals('Descending', 'a,b,C', NestedNames(Nested));
  Nested.IndexName := 'BYNAME';
  AssertEquals('Case insensitive', 'a,b,C', NestedNames(Nested));
  Nested.IndexName := '';
  AssertEquals('Default order', 'b,C,a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestIndexAfterParentScroll;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 2, 'b');
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 9, 'z');
  AppendSimple(Nested, 8, 'y');
  FParent.Post;
  Nested.AddIndex('BYNUM', 'NUM', []);
  Nested.IndexName := 'BYNUM';
  AssertEquals('Sorted in parent 2', 'y,z', NestedNames(Nested));
  FParent.First;
  AssertEquals('Index kept after the parent scrolls', 'a,b', NestedNames(Nested));
  Nested.IndexFieldNames := 'NAME';
  FParent.Next;
  AssertEquals('IndexFieldNames kept after the parent scrolls', 'y,z', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestIndexOnInsert;

begin
  AddSimpleParents(1);
  Nested.IndexFieldNames := 'NUM';
  AppendSimple(Nested, 5, 'e');
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 3, 'c');
  AssertEquals('Sorted while inserting', 'a,c,e', NestedNames(Nested));
  FParent.Post;
  FParent.First;
  AssertEquals('Sorted after reload', 'a,c,e', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestNestedUpdateStatus;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  AssertTrue('Inserted', Nested.UpdateStatus = usInserted);
  AssertEquals('ChangeCount', 1, Nested.ChangeCount);
  FParent.Post;
  FParent.First;
  AssertTrue('Unmodified after reload', Nested.UpdateStatus = usUnmodified);
  AssertEquals('ChangeCount after reload', 0, Nested.ChangeCount);
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'b';
  Nested.Post;
  AssertTrue('Modified', Nested.UpdateStatus = usModified);
end;


procedure TTestNestedDataSet.TestNestedCancelUpdates;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.First;
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'changed';
  Nested.Post;
  AppendSimple(Nested, 2, 'added');
  Nested.CancelUpdates;
  AssertEquals('Nested CancelUpdates', 'a', NestedNames(Nested));
  FParent.Post;
  FParent.First;
  AssertEquals('Stored in the parent', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestNestedRevertRecord;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 2, 'b');
  FParent.Post;
  FParent.First;
  Nested.First;
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'x';
  Nested.Post;
  Nested.Next;
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'y';
  Nested.Post;
  Nested.RevertRecord;
  AssertEquals('Nested RevertRecord', 'x,b', NestedNames(Nested));
  FParent.Post;
  FParent.First;
  AssertEquals('Stored in the parent', 'x,b', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestNestedApplyUpdates;

var
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  lRaised := False;
  try
    Nested.ApplyUpdates;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('ApplyUpdates of a nested dataset raises', lRaised);
end;


procedure TTestNestedDataSet.TestNestedFileName;

var
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  Nested.Close;
  Nested.FileName := 'nested.dat';
  lRaised := False;
  try
    Nested.Open;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  Nested.FileName := '';
  AssertTrue('Opening a nested dataset with a file name raises', lRaised);
end;


procedure TTestNestedDataSet.TestAutoIncShared;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 2, 'b');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 3, 'c');
  AssertEquals('AutoInc continues in another set', 3, Nested.FieldByName('NID').AsInteger);
  FParent.Post;
  FParent.First;
  Nested.Last;
  AssertEquals('AutoInc in the first set', 2, Nested.FieldByName('NID').AsInteger);
end;


procedure TTestNestedDataSet.TestRequiredField;

var
  lRaised: Boolean;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    ChildDefs.Add('NAME', ftString, 20, True);
    end;
  FParent.CreateDataset;
  FParent.AppendRecord([1]);
  AssertTrue('Required copied', Nested.FieldByName('NAME').Required);
  Nested.Append;
  lRaised := False;
  try
    Nested.Post;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('Posting without a required value raises', lRaised);
  Nested.Cancel;
end;


procedure TTestNestedDataSet.TestCalcFieldsAfterParentScroll;

var
  lNested: TBufDataset;
  lField: TField;
  i: Integer;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 5, 'a');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 7, 'b');
  FParent.Post;
  lNested := TBufDataset.Create(nil);
  try
    lNested.FieldDefs.Assign(FParent.FieldDefs[2].ChildDefs);
    for i := 0 to lNested.FieldDefs.Count - 1 do
      lNested.FieldDefs[i].CreateField(lNested);
    lField := TIntegerField.Create(lNested);
    lField.FieldName := 'DOUBLED';
    lField.FieldKind := fkCalculated;
    lField.DataSet := lNested;
    lNested.OnCalcFields := @DoCalcFields;
    lNested.DataSetField := FParent.FieldByName('DETAILS') as TDataSetField;
    AssertEquals('Calculated in parent 2', 14, lNested.FieldByName('DOUBLED').AsInteger);
    FParent.First;
    AssertEquals('Calculated after the parent scrolls', 10, lNested.FieldByName('DOUBLED').AsInteger);
  finally
    lNested.Free;
  end;
end;


procedure TTestNestedDataSet.TestDisableControlsNested;

var
  i: Integer;

begin
  AddSimpleParents(1);
  Nested.DisableControls;
  try
    for i := 1 to 3 do
      AppendSimple(Nested, i, 'n' + IntToStr(i));
  finally
    Nested.EnableControls;
  end;
  AssertEquals('Rows', 'n1,n2,n3', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestDataLinkEvents;

var
  lSource: TDataSource;
  lLink: TTestDataLink;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  lSource := TDataSource.Create(nil);
  lLink := TTestDataLink.Create;
  try
    lSource.DataSet := Nested;
    lLink.DataSource := lSource;
    DataEvents := '';
    FParent.Next;
    AssertTrue('Data link of the nested dataset is told about the parent scroll: ' + DataEvents,
      Pos('DataSetChanged', DataEvents) > 0);
  finally
    lLink.Free;
    lSource.Free;
  end;
end;


procedure TTestNestedDataSet.TestNestedSaveToStream;

var
  lStream: TMemoryStream;
  lCopy: TBufDataset;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 2, 'b');
  lStream := TMemoryStream.Create;
  lCopy := TBufDataset.Create(nil);
  try
    Nested.SaveToStream(lStream, dfBinary);
    lStream.Position := 0;
    lCopy.LoadFromStream(lStream, dfBinary);
    AssertEquals('Rows of the saved nested dataset', 'a,b', NestedNames(lCopy));
  finally
    lCopy.Free;
    lStream.Free;
  end;
end;


procedure TTestNestedDataSet.TestNestedLoadFromStream;

var
  lStream: TMemoryStream;
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  lStream := TMemoryStream.Create;
  try
    Nested.SaveToStream(lStream, dfBinary);
    lStream.Position := 0;
    lRaised := False;
    try
      Nested.LoadFromStream(lStream, dfBinary);
    except
      on EDatabaseError do
        lRaised := True;
    end;
    AssertTrue('Loading a nested dataset from a stream raises', lRaised);
  finally
    lStream.Free;
  end;
end;


procedure TTestNestedDataSet.TestClearField;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 2, 'b');
  FParent.FieldByName('DETAILS').Clear;
  AssertTrue('Clear deletes the rows', Nested.IsEmpty);
  AssertTrue('Field is null', FParent.FieldByName('DETAILS').IsNull);
end;


procedure TTestNestedDataSet.TestManyRows;

var
  i: Integer;
  lStart: TDateTime;

begin
  AddSimpleParents(2);
  lStart := Now;
  Nested.DisableControls;
  try
    for i := 1 to 2000 do
      AppendSimple(Nested, i, 'n' + IntToStr(i));
  finally
    Nested.EnableControls;
  end;
  FParent.Post;
  FParent.Next;
  FParent.First;
  AssertEquals('RecordCount', 2000, Nested.RecordCount);
  Nested.Last;
  AssertEquals('Last row', 2000, Nested.FieldByName('NUM').AsInteger);
  AssertTrue('2000 rows in less than 60 seconds', (Now - lStart) * 86400 < 60);
end;


{ interaction with the parent }

procedure TTestNestedDataSet.TestEditPutsParentInEdit;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  AssertTrue('Parent in browse', FParent.State = dsBrowse);
  Nested.Edit;
  AssertTrue('Nested Edit puts the parent in edit mode', FParent.State = dsEdit);
  Nested.Cancel;
end;


procedure TTestNestedDataSet.TestInsertPutsParentInEdit;

begin
  AddSimpleParents(1);
  Nested.Append;
  AssertTrue('Nested Append puts the parent in edit mode', FParent.State = dsEdit);
  Nested.Cancel;
  AssertTrue('Parent stays in edit mode after the nested Cancel', FParent.State = dsEdit);
end;


procedure TTestNestedDataSet.TestPostKeepsParentInEdit;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  AssertTrue('Parent stays in edit mode after the nested Post', FParent.State = dsEdit);
  AssertFalse('Parent not modified by the nested Post', FParent.Modified);
end;


procedure TTestNestedDataSet.TestParentCancelKeepsNested;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  FParent.Cancel;
  AssertEquals('Posted nested rows stay after the parent cancel', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentPendingEditAndCancel;

begin
  AddSimpleParents(1);
  FParent.Edit;
  FParent.FieldByName('NAME').AsString := 'pending';
  AppendSimple(Nested, 1, 'a');
  AssertEquals('Pending parent value kept during the nested Post', 'pending', FParent.FieldByName('NAME').AsString);
  FParent.Cancel;
  AssertEquals('Parent value reverted', 'parent1', FParent.FieldByName('NAME').AsString);
  AssertEquals('Nested rows kept', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestNestedPostInNewParent;

begin
  AddSimpleParents(1);
  FParent.Append;
  FParent.FieldByName('ID').AsInteger := 2;
  AppendSimple(Nested, 1, 'a');
  AssertTrue('New parent posted and in edit mode again', FParent.State = dsEdit);
  AssertEquals('Parent records', 2, FParent.RecordCount);
  FParent.Post;
  FParent.First;
  FParent.Last;
  AssertEquals('Rows of the new parent', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentCancelOfNewRecord;

begin
  AddSimpleParents(1);
  FParent.Append;
  FParent.FieldByName('ID').AsInteger := 2;
  AssertTrue('No rows in a new parent record', Nested.IsEmpty);
  FParent.Cancel;
  AssertEquals('Parent records', 1, FParent.RecordCount);
end;


procedure TTestNestedDataSet.TestScrollPostsNestedEdit;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  Nested.Edit;
  Nested.FieldByName('NAME').AsString := 'pending';
  FParent.Next;
  FParent.First;
  AssertEquals('Nested edit posted when the parent scrolls', 'pending', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestNestedDeleteLeavesParentBrowse;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  AppendSimple(Nested, 2, 'b');
  FParent.Post;
  Nested.First;
  Nested.Delete;
  AssertTrue('Parent in browse mode after the nested Delete', FParent.State = dsBrowse);
  FParent.First;
  AssertEquals('Remaining row', 'b', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentDelete;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 2, 'b');
  FParent.Post;
  FParent.MergeChangeLog;
  FParent.First;
  FParent.Delete;
  AssertEquals('Parent records', 1, FParent.RecordCount);
  AssertEquals('Rows of the remaining parent', 'b', NestedNames(Nested));
  FParent.CancelUpdates;
  FParent.First;
  AssertEquals('Rows restored with the parent', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentCancelUpdates;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.MergeChangeLog;
  AppendSimple(Nested, 2, 'b');
  Nested.First;
  Nested.Delete;
  FParent.Post;
  AssertEquals('Changed rows', 'b', NestedNames(Nested));
  FParent.CancelUpdates;
  AssertEquals('Parent CancelUpdates', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentRevertRecord;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.Next;
  AppendSimple(Nested, 2, 'b');
  FParent.Post;
  FParent.RevertRecord;
  AssertEquals('Reverted parent', '', NestedNames(Nested));
  FParent.First;
  AssertEquals('Other parent kept', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentUpdateStatus;

begin
  AddSimpleParents(1);
  AssertTrue('Unmodified', FParent.UpdateStatus = usUnmodified);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  AssertTrue('Parent modified by the nested change', FParent.UpdateStatus = usModified);
  AssertEquals('ChangeCount', 1, FParent.ChangeCount);
end;


procedure TTestNestedDataSet.TestParentMergeChangeLog;

begin
  AddSimpleParents(1);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.MergeChangeLog;
  AssertEquals('ChangeCount', 0, FParent.ChangeCount);
  AssertTrue('Unmodified', FParent.UpdateStatus = usUnmodified);
  AssertEquals('Rows', 'a', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestFollowFilteredParent;

var
  i: Integer;

begin
  AddSimpleParents(4);
  for i := 1 to 4 do
    begin
    AppendSimple(Nested, i, 'of' + IntToStr(i));
    FParent.Post;
    FParent.Next;
    end;
  FParent.Filter := 'ID > 2';
  FParent.Filtered := True;
  FParent.First;
  AssertEquals('Filtered parent', 3, FParent.FieldByName('ID').AsInteger);
  AssertEquals('Rows of the current parent', 'of3', NestedNames(Nested));
  FParent.Next;
  AssertEquals('Rows of the next parent', 'of4', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestFollowIndexedParent;

var
  i: Integer;

begin
  AddSimpleParents(3);
  for i := 1 to 3 do
    begin
    AppendSimple(Nested, i, 'of' + IntToStr(i));
    FParent.Post;
    FParent.Next;
    end;
  FParent.AddIndex('DESC', 'ID', [ixDescending], 'ID');
  FParent.IndexName := 'DESC';
  FParent.First;
  AssertEquals('Indexed parent', 3, FParent.FieldByName('ID').AsInteger);
  AssertEquals('Rows of the current parent', 'of3', NestedNames(Nested));
  FParent.Last;
  AssertEquals('Rows of the last parent', 'of1', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestFollowLocate;

var
  i: Integer;

begin
  AddSimpleParents(3);
  for i := 1 to 3 do
    begin
    AppendSimple(Nested, i, 'of' + IntToStr(i));
    FParent.Post;
    FParent.Next;
    end;
  AssertTrue('Locate', FParent.Locate('ID', 2, []));
  AssertEquals('Rows after Locate', 'of2', NestedNames(Nested));
end;


procedure TTestNestedDataSet.TestParentDisableControls;

begin
  AddSimpleParents(2);
  AppendSimple(Nested, 1, 'a');
  FParent.Post;
  FParent.DisableControls;
  try
    FParent.Next;
    AssertTrue('Nested dataset follows the parent with disabled controls', Nested.IsEmpty);
    FParent.First;
    AssertEquals('Rows', 'a', NestedNames(Nested));
  finally
    FParent.EnableControls;
  end;
end;


procedure TTestNestedDataSet.TestStreamBinaryAllTypes;

var
  lLoaded: TBufDataset;
  lParent: TBufDataset;

begin
  FillParents(3, 10);
  lLoaded := SaveAndLoad(FParent, dfBinary);
  lParent := FParent;
  FParent := lLoaded;
  try
    CheckParents(3, 10, '', 'binary stream');
  finally
    FParent := lParent;
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestStreamXMLAllTypes;

var
  lLoaded: TBufDataset;
  lParent: TBufDataset;
  i: Integer;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  FParent.FieldDefs.Add('NAME', ftString, 20);
  FParent.FieldDefs.Add('FBLOB', ftBlob);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    ChildDefs.Add('FSTRING', ftString, 10);
    ChildDefs.Add('FWIDESTRING', ftWideString, 20);
    ChildDefs.Add('FINTEGER', ftInteger);
    ChildDefs.Add('FLARGEINT', ftLargeint);
    ChildDefs.Add('FBOOLEAN', ftBoolean);
    ChildDefs.Add('FCURRENCY', ftCurrency);
    ChildDefs.Add('FDATE', ftDate);
    ChildDefs.Add('FBLOB', ftBlob);
    ChildDefs.Add('FMEMO', ftMemo);
    ChildDefs.Add('FWIDEMEMO', ftWideMemo);
    with ChildDefs.AddFieldDef do
      begin
      Name := 'FADT';
      DataType := ftADT;
      ChildDefs.Add('X', ftInteger);
      ChildDefs.Add('Y', ftFloat);
      end;
    with ChildDefs.AddFieldDef do
      begin
      Name := 'FARRAY';
      DataType := ftArray;
      Size := 3;
      ChildDefs.Add('E', ftInteger);
      end;
    with ChildDefs.AddFieldDef do
      begin
      Name := 'SUB';
      DataType := ftDataSet;
      ChildDefs.Add('SID', ftAutoInc);
      ChildDefs.Add('SNAME', ftString, 20);
      ChildDefs.Add('SBLOB', ftBlob);
      end;
    end;
  FParent.CreateDataset;
  for i := 0 to 2 do
    begin
    FParent.Append;
    FParent.FieldByName('ID').AsInteger := i + 1;
    FParent.FieldByName('NAME').AsString := 'parent' + IntToStr(i + 1);
    FParent.FieldByName('FBLOB').AsBytes := BlobBytes(i, 500);
    FParent.Post;
    FillRows(Nested, i, 8 - i);
    FParent.Post;
    end;
  lLoaded := SaveAndLoad(FParent, dfXML);
  lParent := FParent;
  FParent := lLoaded;
  try
    CheckParents(3, 8, '', 'XML stream');
  finally
    FParent := lParent;
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestStreamWithChangeLog;

var
  lLoaded: TBufDataset;
  lFormat: TDataPacketFormat;

begin
  for lFormat in [dfBinary, dfXML] do
    begin
    FreeAndNil(FParent);
    AddSimpleParents(2);
    AppendSimple(Nested, 1, 'original');
    FParent.Post;
    FParent.MergeChangeLog;
    Nested.Edit;
    Nested.FieldByName('NAME').AsString := 'changed';
    Nested.Post;
    FParent.Post;
    lLoaded := SaveAndLoad(FParent, lFormat);
    try
      AssertEquals('ChangeCount after loading', 1, lLoaded.ChangeCount);
      lLoaded.First;
      AssertEquals('Changed rows after loading', 'changed', NestedNames(NestedOf(lLoaded)));
      lLoaded.CancelUpdates;
      lLoaded.First;
      AssertEquals('Original rows after CancelUpdates of the loaded dataset', 'original', NestedNames(NestedOf(lLoaded)));
    finally
      lLoaded.Free;
    end;
    end;
end;


procedure TTestNestedDataSet.TestStreamAutoInc;

var
  lLoaded: TBufDataset;
  lFormat: TDataPacketFormat;

begin
  for lFormat in [dfBinary, dfXML] do
    begin
    FreeAndNil(FParent);
    AddSimpleParents(2);
    AppendSimple(Nested, 1, 'a');
    AppendSimple(Nested, 2, 'b');
    FParent.Post;
    FParent.Next;
    AppendSimple(Nested, 3, 'c');
    FParent.Post;
    lLoaded := SaveAndLoad(FParent, lFormat);
    try
      lLoaded.First;
      AppendSimple(NestedOf(lLoaded), 4, 'd');
      AssertEquals('AutoInc continues after loading', 4, NestedOf(lLoaded).FieldByName('NID').AsInteger);
    finally
      lLoaded.Free;
    end;
    end;
end;


procedure TTestNestedDataSet.TestTwoDataSetFields;

var
  lOther: TBufDataset;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    AddSimpleDefs(ChildDefs);
    end;
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'OTHER';
    DataType := ftDataSet;
    ChildDefs.Add('TEXT', ftString, 20);
    end;
  FParent.CreateDataset;
  FParent.AppendRecord([1]);
  FParent.AppendRecord([2]);
  FParent.First;
  AppendSimple(Nested, 1, 'a');
  lOther := NestedOf(FParent, 'OTHER');
  lOther.AppendRecord(['text1']);
  FParent.Post;
  FParent.Next;
  lOther.AppendRecord(['text2']);
  FParent.Post;
  FParent.First;
  AssertEquals('DETAILS of parent 1', 'a', NestedNames(Nested));
  AssertEquals('OTHER of parent 1', 'text1', lOther.FieldByName('TEXT').AsString);
  FParent.Next;
  AssertTrue('DETAILS of parent 2', Nested.IsEmpty);
  AssertEquals('OTHER of parent 2', 'text2', lOther.FieldByName('TEXT').AsString);
  AssertEquals('Two nested datasets', 2, FParent.NestedDataSets.Count);
end;


procedure TTestNestedDataSet.TestThreeLevels;

var
  lLevel1, lLevel2, lLevel3: TBufDataset;
  lLoaded: TBufDataset;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    ChildDefs.Add('NAME', ftString, 20);
    with ChildDefs.AddFieldDef do
      begin
      Name := 'L2';
      DataType := ftDataSet;
      ChildDefs.Add('NAME', ftString, 20);
      with ChildDefs.AddFieldDef do
        begin
        Name := 'L3';
        DataType := ftDataSet;
        ChildDefs.Add('NAME', ftString, 20);
        ChildDefs.Add('B', ftBlob);
        end;
      end;
    end;
  FParent.CreateDataset;
  FParent.AppendRecord([1]);
  FParent.AppendRecord([2]);
  FParent.First;
  lLevel1 := Nested;
  lLevel1.AppendRecord(['l1']);
  lLevel2 := NestedOf(lLevel1, 'L2');
  lLevel2.AppendRecord(['l2']);
  lLevel3 := NestedOf(lLevel2, 'L3');
  lLevel3.Append;
  lLevel3.FieldByName('NAME').AsString := 'l3';
  lLevel3.FieldByName('B').AsString := 'deep blob';
  lLevel3.Post;
  FParent.Post;
  FParent.Next;
  AssertTrue('Parent 2 has no rows', lLevel1.IsEmpty);
  AssertTrue('Level 3 follows', lLevel3.IsEmpty);
  FParent.First;
  AssertEquals('Level 1', 'l1', NestedNames(lLevel1));
  lLevel1.First;
  AssertEquals('Level 2', 'l2', NestedNames(lLevel2));
  lLevel2.First;
  AssertEquals('Level 3', 'l3', NestedNames(lLevel3));
  AssertEquals('Level 3 blob', 'deep blob', lLevel3.FieldByName('B').AsString);
  lLoaded := SaveAndLoad(FParent, dfBinary);
  try
    lLoaded.First;
    AssertEquals('Level 3 after streaming', 'deep blob',
      NestedOf(NestedOf(NestedOf(lLoaded), 'L2'), 'L3').FieldByName('B').AsString);
  finally
    lLoaded.Free;
  end;
end;


{ errors }

procedure TTestNestedDataSet.TestDataSetFieldAsADTChild;

var
  lRaised: Boolean;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'PT';
    DataType := ftADT;
    with ChildDefs.AddFieldDef do
      begin
      Name := 'DS';
      DataType := ftDataSet;
      ChildDefs.Add('X', ftInteger);
      end;
    end;
  lRaised := False;
  try
    FParent.CreateDataset;
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('A dataset field as member of an ADT raises', lRaised);
end;


procedure TTestNestedDataSet.TestIndexOnDataSetField;

var
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  lRaised := False;
  try
    FParent.IndexFieldNames := 'DETAILS';
  except
    on EDatabaseError do
      lRaised := True;
  end;
  AssertTrue('An index on a dataset field raises', lRaised);
end;


procedure TTestNestedDataSet.TestCopyFromDataset;

var
  lCopy: TBufDataset;
  lRaised: Boolean;

begin
  AddSimpleParents(1);
  lCopy := TBufDataset.Create(nil);
  try
    lRaised := False;
    try
      lCopy.CopyFromDataset(FParent);
    except
      on EDatabaseError do
        lRaised := True;
    end;
    AssertTrue('CopyFromDataset of a dataset with a dataset field raises', lRaised);
  finally
    lCopy.Free;
  end;
end;


function TTestNestedDataSet.CreateObjectFieldsDataset(aObjectView: Boolean): TBufDataset;

begin
  Result := TBufDataset.Create(nil);
  Result.ObjectView := aObjectView;
  Result.FieldDefs.Add('ID', ftInteger);
  with Result.FieldDefs.AddFieldDef do
    begin
    Name := 'PT';
    DataType := ftADT;
    ChildDefs.Add('X', ftInteger);
    ChildDefs.Add('Y', ftString, 10);
    with ChildDefs.AddFieldDef do
      begin
      Name := 'TAGS';
      DataType := ftArray;
      Size := 2;
      ChildDefs.Add('T', ftString, 5);
      end;
    end;
  with Result.FieldDefs.AddFieldDef do
    begin
    Name := 'ARR';
    DataType := ftArray;
    Size := 3;
    ChildDefs.AddFieldDef.DataType := ftFloat;
    end;
  with Result.FieldDefs.AddFieldDef do
    begin
    Name := 'POINTS';
    DataType := ftArray;
    Size := 2;
    with ChildDefs.AddFieldDef do
      begin
      Name := 'P';
      DataType := ftADT;
      ChildDefs.Add('A', ftInteger);
      ChildDefs.Add('B', ftDate);
      end;
    end;
  Result.CreateDataset;
end;


procedure TTestNestedDataSet.TestXMLObjectFields;

var
  lDS, lLoaded: TBufDataset;
  lObjectView: Boolean;
  lWhere: string;

begin
  for lObjectView in [True, False] do
    begin
    lWhere := 'ObjectView ' + BoolToStr(lObjectView, True) + ': ';
    lDS := CreateObjectFieldsDataset(lObjectView);
    try
      lDS.Append;
      lDS.FieldByName('ID').AsInteger := 1;
      lDS.FieldList.FieldByName('PT.X').AsInteger := 42;
      lDS.FieldList.FieldByName('PT.Y').AsString := 'text';
      lDS.FieldList.FieldByName('PT.TAGS[1]').AsString := 'tag';
      lDS.FieldList.FieldByName('ARR[0]').AsFloat := 1.5;
      lDS.FieldList.FieldByName('ARR[2]').AsFloat := -3.25;
      lDS.FieldList.FieldByName('POINTS[1].A').AsInteger := 7;
      lDS.FieldList.FieldByName('POINTS[1].B').AsDateTime := EncodeDate(1917, 12, 29);
      lDS.Post;
      lDS.Append;
      lDS.FieldByName('ID').AsInteger := 2;
      lDS.Post;
      lLoaded := SaveAndLoad(lDS, dfXML);
      try
        AssertTrue(lWhere + 'Loaded dataset has object fields', lLoaded.FieldByName('PT') is TADTField);
        AssertEquals(lWhere + 'Definition count', 4, lLoaded.FieldDefs.Count);
        AssertTrue(lWhere + 'ADT definition', lLoaded.FieldDefs[1].DataType = ftADT);
        AssertEquals(lWhere + 'ADT members', 3, lLoaded.FieldDefs[1].ChildDefs.Count);
        AssertTrue(lWhere + 'Array definition', lLoaded.FieldDefs[2].DataType = ftArray);
        AssertEquals(lWhere + 'Array size', 3, lLoaded.FieldDefs[2].Size);
        lLoaded.First;
        AssertEquals(lWhere + 'PT.X', 42, lLoaded.FieldList.FieldByName('PT.X').AsInteger);
        AssertEquals(lWhere + 'PT.Y', 'text', lLoaded.FieldList.FieldByName('PT.Y').AsString);
        AssertTrue(lWhere + 'PT.TAGS[0] null', lLoaded.FieldList.FieldByName('PT.TAGS[0]').IsNull);
        AssertEquals(lWhere + 'PT.TAGS[1]', 'tag', lLoaded.FieldList.FieldByName('PT.TAGS[1]').AsString);
        AssertEquals(lWhere + 'ARR[0]', 1.5, lLoaded.FieldList.FieldByName('ARR[0]').AsFloat, 0);
        AssertTrue(lWhere + 'ARR[1] null', lLoaded.FieldList.FieldByName('ARR[1]').IsNull);
        AssertEquals(lWhere + 'ARR[2]', -3.25, lLoaded.FieldList.FieldByName('ARR[2]').AsFloat, 0);
        AssertTrue(lWhere + 'POINTS[0].A null', lLoaded.FieldList.FieldByName('POINTS[0].A').IsNull);
        AssertEquals(lWhere + 'POINTS[1].A', 7, lLoaded.FieldList.FieldByName('POINTS[1].A').AsInteger);
        AssertEquals(lWhere + 'POINTS[1].B', EncodeDate(1917, 12, 29), lLoaded.FieldList.FieldByName('POINTS[1].B').AsDateTime, 0);
        lLoaded.Next;
        AssertEquals(lWhere + 'Second record', 2, lLoaded.FieldByName('ID').AsInteger);
        AssertTrue(lWhere + 'ADT of the second record null', lLoaded.FieldList.FieldByName('PT.X').IsNull);
        AssertTrue(lWhere + 'Array of the second record null', lLoaded.FieldList.FieldByName('ARR[0]').IsNull);
      finally
        lLoaded.Free;
      end;
    finally
      lDS.Free;
    end;
    end;
end;


procedure TTestNestedDataSet.TestXMLObjectFieldsNested;

var
  lLoaded: TBufDataset;

begin
  FParent := TBufDataset.Create(nil);
  FParent.FieldDefs.Add('ID', ftInteger);
  with FParent.FieldDefs.AddFieldDef do
    begin
    Name := 'DETAILS';
    DataType := ftDataSet;
    with ChildDefs.AddFieldDef do
      begin
      Name := 'FADT';
      DataType := ftADT;
      ChildDefs.Add('X', ftInteger);
      ChildDefs.Add('Y', ftFloat);
      end;
    with ChildDefs.AddFieldDef do
      begin
      Name := 'FARRAY';
      DataType := ftArray;
      Size := 3;
      ChildDefs.Add('E', ftInteger);
      end;
    end;
  FParent.CreateDataset;
  FParent.AppendRecord([1]);
  Nested.Append;
  SetValue(Nested.FieldByName('FADT'), 5);
  SetValue(Nested.FieldByName('FARRAY'), 6);
  Nested.Post;
  Nested.Append;
  Nested.Post;
  FParent.Post;
  lLoaded := SaveAndLoad(FParent, dfXML);
  try
    lLoaded.First;
    AssertEquals('Nested records', 2, NestedOf(lLoaded).RecordCount);
    NestedOf(lLoaded).First;
    CheckValue(NestedOf(lLoaded).FieldByName('FADT'), 5, 'nested XML');
    CheckValue(NestedOf(lLoaded).FieldByName('FARRAY'), 6, 'nested XML');
    NestedOf(lLoaded).Next;
    AssertTrue('Nested ADT of the second row null', NestedOf(lLoaded).FieldByName('FADT').IsNull);
  finally
    lLoaded.Free;
  end;
end;


procedure TTestNestedDataSet.TestObjectFieldsStreamBinary;

var
  lDS, lLoaded: TBufDataset;

begin
  lDS := TBufDataset.Create(nil);
  try
    lDS.FieldDefs.Add('ID', ftInteger);
    with lDS.FieldDefs.AddFieldDef do
      begin
      Name := 'PT';
      DataType := ftADT;
      ChildDefs.Add('X', ftInteger);
      ChildDefs.Add('Y', ftString, 10);
      end;
    with lDS.FieldDefs.AddFieldDef do
      begin
      Name := 'ARR';
      DataType := ftArray;
      Size := 2;
      ChildDefs.Add('E', ftFloat);
      end;
    lDS.CreateDataset;
    lDS.Append;
    lDS.FieldByName('ID').AsInteger := 1;
    TADTField(lDS.FieldByName('PT')).Fields[0].AsInteger := 42;
    TADTField(lDS.FieldByName('PT')).Fields[1].AsString := 'text';
    TArrayField(lDS.FieldByName('ARR')).Fields[1].AsFloat := 2.5;
    lDS.Post;
    lLoaded := SaveAndLoad(lDS, dfBinary);
    try
      AssertEquals('ADT member X', 42, TADTField(lLoaded.FieldByName('PT')).Fields[0].AsInteger);
      AssertEquals('ADT member Y', 'text', TADTField(lLoaded.FieldByName('PT')).Fields[1].AsString);
      AssertTrue('Array element 0 null', TArrayField(lLoaded.FieldByName('ARR')).Fields[0].IsNull);
      AssertEquals('Array element 1', 2.5, TArrayField(lLoaded.FieldByName('ARR')).Fields[1].AsFloat, 0);
    finally
      lLoaded.Free;
    end;
  finally
    lDS.Free;
  end;
end;


initialization
  ISOFormat := DefaultFormatSettings;
  ISOFormat.DecimalSeparator := '.';
  ISOFormat.ThousandSeparator := #0;
  if uppercase(dbconnectorname) = 'BUFDATASET' then
    RegisterTest(TTestNestedDataSet);
end.
