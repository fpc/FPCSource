unit TestDBBasics;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  Classes, SysUtils, db, ToolsUnit;

type

  { TTestDBBasics }

  TTestDBBasics = class(TDBBasicsTestCase)
  private
    procedure TestfieldDefinition(AFieldType : TFieldType;ADatasize : integer;var ADS : TDataset; var AFld: TField);
    procedure TestcalculatedField_OnCalcfields(DataSet: TDataSet);

  published
    // fields
    procedure TestSetFieldValues;
    procedure TestGetFieldValues;

    procedure TestSupportIntegerFields;
    procedure TestSupportSmallIntFields;
    procedure TestSupportWordFields;
    procedure TestSupportStringFields;
    procedure TestSupportBooleanFields;
    procedure TestSupportFloatFields;
    procedure TestSupportLargeIntFields;
    procedure TestSupportDateFields;
    procedure TestSupportTimeFields;
    procedure TestSupportCurrencyFields;
    procedure TestSupportBCDFields;
    procedure TestSupportFmtBCDFields;
    procedure TestSupportFixedStringFields;
    procedure TestSupportBlobFields;
    procedure TestSupportMemoFields;

    procedure TestBlobBlobType; //bug 26064

    procedure TestCalculatedField;
    procedure TestCanModifySpecialFields;

    // dataset
    procedure TestDoubleClose;
    procedure TestFieldDefsUpdate;
    procedure TestAssignFieldftString;
    procedure TestAssignFieldftFixedChar;
    procedure TestSelectQueryBasics;
    procedure TestPostOnlyInEditState;
    procedure TestMove;                    // bug 5048
    procedure TestActiveBufferWhenClosed;
    procedure TestEOFBOFClosedDataset;
    procedure TestRecordcountAfterReopen;  // partly bug 8228
    procedure TestExceptionLocateClosed;    // bug 13938
    procedure TestDetectionNonMatchingDataset;
    // events
    procedure TestLayoutChangedEvents;
    procedure TestDataEventsResync;
    procedure TestdeFieldListChange;
  end;

  { TTestBufDatasetDBBasics }
{$ifdef fpc}
  TTestBufDatasetDBBasics = class(TDBBasicsTestCase)
  private
    procedure FTestXMLDatasetDefinition(ADataset : TDataset);
    procedure TestAddIndexFieldType(AFieldType : TFieldType; ActiveDS : boolean);
  published
    procedure TestFileNameProperty;
    procedure TestClientDatasetAsMemDataset;
    procedure TestSaveAsXML;
    procedure TestIsEmpty;
    procedure TestReadOnly;
  // cached updates
    procedure TestBufDatasetCancelUpd; //bug 6938
    procedure TestBufDatasetCancelUpd1;
    procedure TestMultipleDeleteUpdateBuffer;
    procedure TestDoubleDelete;
    procedure TestMergeChangeLog;
  // index tests
    procedure TestAddIndexInteger;
    procedure TestAddIndexSmallInt;
    procedure TestAddIndexBoolean;
    procedure TestAddIndexFloat;
    procedure TestAddIndexLargeInt;
    procedure TestAddIndexDateTime;
    procedure TestAddIndexCurrency;
    procedure TestAddIndexBCD;
    procedure TestAddIndexFmtBCD;

    procedure TestAddIndex;
    procedure TestAddDescIndex;
    procedure TestAddCaseInsIndex;
    procedure TestInactSwitchIndex;

    procedure TestAddIndexActiveDS;
    procedure TestAddIndexEditDS;

    procedure TestIndexFieldNames;
    procedure TestIndexFieldNamesActive;
    procedure TestIndexFieldNamesClosed; // bug 16695

    procedure TestIndexCurRecord;

    procedure TestAddDblIndex;
    procedure TestIndexEditRecord;
    procedure TestIndexAppendRecord;
  end;

{$endif fpc}

  TTestUniDirectionalDBBasics = class(TTestDBBasics)
  end;

  { TTestCursorDBBasics }

  TTestCursorDBBasics = class(TDBBasicsTestCase)
  private
    procedure TestOnFilterProc(DataSet: TDataSet; var Accept: Boolean); // Filters out all records with even ID
    procedure FTestDelete1(TestCancelUpdate : boolean);
    procedure FTestDelete2(TestCancelUpdate : boolean);
  published
    procedure TestCancelUpdDelete1;
    procedure TestCancelUpdDelete2;

    procedure TestAppendInsertRecord;

    procedure TestBookmarks;
    procedure TestBookmarkValid;

    procedure TestDelete1;
    procedure TestDelete2;

    procedure TestLocate;
    procedure TestLocateCaseIns;
    procedure TestLocateCaseInsInts;
    procedure TestLookup;

    procedure TestOnFilter;
    procedure TestIntFilter; //Integer range filter
    procedure TestNegativeIntFilter; //Negative integer filter; bug 25168
    procedure TestStringFilter; //String filter expressions

    procedure TestNullAtOpen;

    procedure TestAppendOnEmptyDataset;
    procedure TestInsertOnEmptyDataset;

    procedure TestFirst;
    procedure TestEofAfterFirst;           // bug 7211
    procedure TestLastAppendCancel;        // bug 5058
    procedure TestRecNo;                   // bug 5061
    procedure TestSetRecNo;                // bug 6919
    procedure TestBug7007;
    procedure TestBug6893;
    procedure TestRequired;
    procedure TestModified;
    // fields
    procedure TestFieldOldValueObsolete;
    procedure TestFieldOldValue;
    procedure TestChangeBlobFieldBeforePost; //bug 15376
  end;


  { TDBBasicsUniDirectionalTestSetup }
{$ifdef fpc}
  TDBBasicsUniDirectionalTestSetup = class(TDBBasicsTestSetup)
  protected
    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  end;
{$endif fpc}

implementation

uses
{$ifdef fpc}
  bufdataset,
  sqldb,
{$endif fpc}
  variants,
  strutils,
  FmtBCD;

type THackDataLink=class(TDataLink);

{ TTestCursorDBBasics }

procedure TTestCursorDBBasics.TestAppendOnEmptyDataset;
begin
  with DBConnector.GetNDataset(0) do
    begin
    open;
    CheckTrue(CanModify);
    CheckTrue(eof);
    CheckTrue(bof);
    append;
    FieldByName('id').AsInteger:=0;
    CheckFalse(Bof);
    CheckTrue(Eof);
    post;
    CheckFalse(eof);
    CheckFalse(bof);
    end;
end;

procedure TTestCursorDBBasics.TestInsertOnEmptyDataset;
begin
  with DBConnector.GetNDataset(0) do
    begin
    open;
    CheckTrue(CanModify);
    CheckTrue(eof);
    CheckTrue(bof);
    CheckTrue(IsEmpty);
    insert;
    FieldByName('id').AsInteger:=0;
    CheckTrue(Bof);
    CheckTrue(Eof);
    CheckFalse(IsEmpty);
    post;
    CheckFalse(IsEmpty);
    CheckFalse(eof);
    CheckFalse(bof);
    end;
end;

procedure TTestDBBasics.TestSelectQueryBasics;
var b : TFieldType;
begin
  with DBConnector.GetNDataset(1) do
    begin
    Open;

    if IsUniDirectional then
      CheckEquals(-1,RecNo)
    else
      CheckEquals(1,RecNo);
    CheckEquals(1,RecordCount);

    CheckEquals(2,FieldCount);

    CheckTrue(CompareText('ID',fields[0].FieldName)=0);
    CheckTrue(CompareText('ID',fields[0].DisplayName)=0);
    CheckTrue(ftInteger=fields[0].DataType, 'The datatype of the field ''ID'' is incorrect, it should be ftInteger');

    CheckTrue(CompareText('NAME',fields[1].FieldName)=0);
    CheckTrue(CompareText('NAME',fields[1].DisplayName)=0);
    CheckTrue(ftString=fields[1].DataType);

    CheckEquals(1,fields[0].Value);
    CheckEquals('TestName1',fields[1].Value);

    Close;
    end;
end;

procedure TTestDBBasics.TestPostOnlyInEditState;
begin
  with DBConnector.GetNDataset(1) do
    begin
    open;
    CheckException(Post,EDatabaseError,'Post was called in a non-edit state');
    end;
end;

procedure TTestDBBasics.TestMove;
var i,count      : integer;
    aDatasource  : TDataSource;
    aDatalink    : TDataLink;
    ABufferCount : Integer;

begin
  aDatasource := TDataSource.Create(nil);
  aDatalink := TTestDataLink.Create;
  try
    aDatalink.DataSource := aDatasource;
    ABufferCount := 11;
    aDatalink.BufferCount := ABufferCount;
    DataEvents := '';
    for count := 0 to 32 do
      begin
      aDatasource.DataSet := DBConnector.GetNDataset(count);
      with aDatasource.Dataset do
        begin
        i := 1;
        Open;
        CheckEquals('deUpdateState:0;',DataEvents);
        DataEvents := '';
        while not EOF do
          begin
          CheckEquals(i,fields[0].AsInteger);
          CheckEquals('TestName'+inttostr(i),fields[1].AsString);
          inc(i);

          Next;
          if (i > ABufferCount) and not EOF then
            CheckEquals('deCheckBrowseMode:0;deDataSetScroll:-1;DataSetScrolled:1;DataSetChanged;',DataEvents)
          else
            CheckEquals('deCheckBrowseMode:0;deDataSetScroll:0;DataSetScrolled:0;DataSetChanged;',DataEvents);
          DataEvents := '';
          end;
        CheckEquals(count,i-1);
        close;
        CheckEquals('deUpdateState:0;',DataEvents);
        DataEvents := '';
        end;
      end;
  finally
    aDatalink.Free;
    aDatasource.Free;
  end;
end;

procedure TTestDBBasics.TestActiveBufferWhenClosed;
begin
  with DBConnector.GetNDataset(0) do
    begin
{$ifdef fpc}
    AssertNull(ActiveBuffer);
{$endif fpc}
    open;
    CheckFalse(ActiveBuffer = nil,'Activebuffer of an empty dataset shouldn''t be nil');
    end;
end;

procedure TTestDBBasics.TestEOFBOFClosedDataset;
begin
  with DBConnector.GetNDataset(1) do
    begin
    CheckTrue(EOF);
    CheckTrue(BOF);
    open;
    CheckTrue(BOF, 'No BOF when opened non-empty dataset');
    CheckFalse(EOF, 'EOF after opened non-empty dataset');
    close;
    CheckTrue(EOF);
    CheckTrue(BOF);
    end;
end;

procedure TTestDBBasics.TestLayoutChangedEvents;
var aDatasource : TDataSource;
    aDatalink   : TDataLink;
    ds          : tdataset;

begin
  aDatasource := TDataSource.Create(nil);
  aDatalink := TTestDataLink.Create;
  try
    aDatalink.DataSource := aDatasource;
    ds := DBConnector.GetNDataset(6);
    aDatasource.DataSet:=ds;
    with ds do
      begin
      open;

      DataEvents := '';
      DisableControls;
      Active:=False;
      Active:=True;
      EnableControls;
      CheckEquals('deLayoutChange:0;DataSetChanged;',DataEvents);

      close;
      end;
  finally
    aDatasource.Free;
    aDatalink.Free;
  end;
end;

procedure TTestDBBasics.TestDataEventsResync;
var i,count     : integer;
    aDatasource : TDataSource;
    aDatalink   : TDataLink;
    ds          : tdataset;

begin
  aDatasource := TDataSource.Create(nil);
  aDatalink := TTestDataLink.Create;
  try
    aDatalink.DataSource := aDatasource;
    ds := DBConnector.GetNDataset(6);
    ds.BeforeScroll := DBConnector.DataEvent;
    with ds do
      begin
      aDatasource.DataSet := ds;
      Open;
      DataEvents := '';
      Resync([rmExact]);
      if IsUniDirectional then
        CheckEquals('',DataEvents)
      else
        CheckEquals('deDataSetChange:0;DataSetChanged;',DataEvents);
      DataEvents := '';
      Next;
      if IsUniDirectional then
        CheckEquals('deCheckBrowseMode:0;DataEvent;deDataSetScroll:-1;DataSetScrolled:1;DataSetChanged;',DataEvents)
      else
        CheckEquals('deCheckBrowseMode:0;DataEvent;deDataSetScroll:0;DataSetScrolled:1;DataSetChanged;',DataEvents);
      DataEvents := '';
      Close;
      end;
  finally
    aDatasource.Free;
    aDatalink.Free;
  end;
end;

procedure TTestDBBasics.TestdeFieldListChange;

var i,count     : integer;
    aDatasource : TDataSource;
    aDatalink   : TDataLink;
    ds          : TDataset;

begin
  aDatasource := TDataSource.Create(nil);
  aDatalink := TTestDataLink.Create;
  aDatalink.DataSource := aDatasource;
  ds := DBConnector.GetNDataset(1);
  with ds do
    begin
    aDatasource.DataSet := ds;
    DataEvents := '';
    Open;
    Fields.Add(TField.Create(ds));
    CheckEquals('deUpdateState:0;deFieldListChange:0;',DataEvents);
    DataEvents := '';
    Fields.Clear;
    CheckEquals('deFieldListChange:0;',DataEvents)
    end;
  aDatasource.Free;
  aDatalink.Free;
end;

procedure TTestDBBasics.TestRecordcountAfterReopen;
var
  datalink1: tdatalink;
  datasource1: tdatasource;
  query1: TDataSet;

begin
  query1:= DBConnector.GetNDataset(11);
  datalink1:= TDataLink.create;
  datasource1:= tdatasource.create(nil);
  try
    datalink1.datasource:= datasource1;
    datasource1.dataset:= query1;

    query1.active := true;
    query1.active := False;
    CheckEquals(0, THackDataLink(datalink1).RecordCount);
    query1.active := true;
    CheckTrue(THackDataLink(datalink1).RecordCount>0);
    query1.active := false;
  finally
    datalink1.free;
    datasource1.free;
  end;
end;

procedure TTestCursorDBBasics.TestLastAppendCancel;

var count : integer;

begin
  for count := 0 to 32 do with DBConnector.GetNDataset(count) do
    begin
    open;

    Last;
    Append;
    Cancel;

    CheckEquals(count,fields[0].asinteger);
    CheckEquals(count,RecordCount);

    Close;

    end;
end;

procedure TTestCursorDBBasics.TestRecNo;
var i       : longint;
    passed  : boolean;
begin
  with DBConnector.GetNDataset(0) do
    begin
    // Accessing RecNo on a closed dataset should raise an EDatabaseError or should
    // return 0
    passed := false;
    try
      i := recno;
    except on E: Exception do
      begin
      passed := E.classname = EDatabaseError.className
      end;
    end;
    if not passed then
      CheckEquals(0,RecNo,'Failed to get the RecNo from a closed dataset');

    // Accessing Recordcount on a closed dataset should raise an EDatabaseError or should
    // return 0
    passed := false;
    try
      i := recordcount;
    except on E: Exception do
      begin
      passed := E.classname = EDatabaseError.className
      end;
    end;
    if not passed then
      CheckEquals(0,RecNo,'Failed to get the Recordcount from a closed dataset');

    Open;

    CheckEquals(0,RecordCount);
    CheckEquals(0,RecNo);

    first;
    CheckEquals(0,RecordCount);
    CheckEquals(0,RecNo);

    last;
    CheckEquals(0,RecordCount);
    CheckEquals(0,RecNo);

    append;
    CheckEquals(0,RecNo);
    CheckEquals(0,RecordCount);

    first;
    CheckEquals(0,RecNo);
    CheckEquals(0,RecordCount);

    append;
    FieldByName('id').AsInteger := 1;
    CheckEquals(0,RecNo);
    CheckEquals(0,RecordCount);

    first;
    CheckEquals(1,RecNo);
    CheckEquals(1,RecordCount);

    last;
    CheckEquals(1,RecNo);
    CheckEquals(1,RecordCount);

    append;
    FieldByName('id').AsInteger := 1;
    CheckEquals(0,RecNo,'RecNo after 3rd Append');
    CheckEquals(1,RecordCount);

    Close;
    end;
end;

procedure TTestCursorDBBasics.TestSetRecNo;
begin
  with DBConnector.GetNDataset(15) do
    begin
    Open;
    RecNo := 1;
    CheckEquals(1,fields[0].AsInteger);
    CheckEquals(1,RecNo);

    RecNo := 2;
    CheckEquals(2,fields[0].AsInteger);
    CheckEquals(2,RecNo);

    RecNo := 8;
    CheckEquals(8,fields[0].AsInteger);
    CheckEquals(8,RecNo);

    RecNo := 15;
    CheckEquals(15,fields[0].AsInteger);
    CheckEquals(15,RecNo);

    RecNo := 3;
    CheckEquals(3,fields[0].AsInteger);
    CheckEquals(3,RecNo);

    RecNo := 14;
    CheckEquals(14,fields[0].AsInteger);
    CheckEquals(14,RecNo);

    RecNo := 15;
    CheckEquals(15,fields[0].AsInteger);
    CheckEquals(15,RecNo);

    // test for exceptions...
{    RecNo := 16;
    CheckEquals(15,fields[0].AsInteger);
    CheckEquals(15,RecNo);}

    Close;
    end;
end;

procedure TTestCursorDBBasics.TestRequired;
begin
  with DBConnector.GetNDataset(2) do
    begin
    Open;
    FieldByName('ID').Required := True;
    Append;
    CheckException(Post, EDatabaseError);
    FieldByName('ID').AsInteger := 1000;
    Post;
    Close;
    end;
end;

procedure TTestDBBasics.TestExceptionLocateClosed;
var passed: boolean;
begin
  with DBConnector.GetNDataset(15) do
    begin
    passed := false;
    try
      locate('name','TestName1',[]);
    except on E: Exception do
      begin
      passed := E.classname = EDatabaseError.className
      end;
    end;
    CheckTrue(passed);
    end;
end;

procedure TTestCursorDBBasics.TestModified;
begin
  // Tests TDataSet.Modified property
  with DBConnector.GetNDataset(true,1) as TDataset do
  begin
    Open;
    CheckFalse(Modified);

    Edit;
    CheckFalse(Modified, 'After Edit');
    Fields[1].AsString := Fields[1].AsString;
    CheckTrue(Modified, 'After change');
    Post;
    CheckFalse(Modified, 'After Post');

    Append;
    CheckFalse(Modified, 'After Append');
    Fields[0].AsInteger := 100;
    CheckTrue(Modified, 'After change');
    Cancel;
    CheckFalse(Modified, 'After Cancel');

    Close;
  end;
end;

procedure TTestDBBasics.TestDetectionNonMatchingDataset;
var
  F: TField;
  ds: tdataset;
begin
  // TDataset.Bindfields should detect problems when the underlying data does
  // not reflect the fields of the dataset. This test is to check if this is
  // really done.
  ds := DBConnector.GetNDataset(true,6);
  with ds do
    begin
    open;
    close;

    F := TStringField.Create(ds);
    F.FieldName:='DOES_NOT_EXIST';
    F.DataSet:=ds;
    F.Size:=50;

    CheckException(open,EDatabaseError);
    end;
end;

procedure TTestCursorDBBasics.TestAppendInsertRecord;
begin
  with DBConnector.GetNDataset(true,6) do
    begin
    open;
    // InsertRecord should insert a record, set the values, post the record and
    // make the new record active.
    InsertRecord([152,'TestInsRec']);
    CheckEquals(152,fields[0].AsInteger);
    CheckEquals('TestInsRec',fields[1].AsString);
    CheckTrue(state=dsBrowse);

    // AppendRecord should append a record, further the same as InsertRecord
    AppendRecord([151,'TestInsRec']);
    CheckEquals(151,fields[0].AsInteger);
    CheckEquals('TestInsRec',fields[1].AsString);
    CheckTrue(state=dsBrowse);
    next;
    CheckTrue(EOF);
    end;
end;

procedure TTestCursorDBBasics.TestBookmarks;
var BM1,BM2,BM3,BM4,BM5 : TBookmark;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
{$ifdef fpc}
    AssertNull(GetBookmark);
{$endif fpc}
    open;
    BM1:=GetBookmark; // id=1, BOF
    next;next;
    BM2:=GetBookmark; // id=3
    next;next;next;
    BM3:=GetBookmark; // id=6
    next;next;next;next;next;next;next;next;
    BM4:=GetBookmark; // id=14
    next;
    BM5:=GetBookmark; // id=14, EOF
    
    GotoBookmark(BM2);
    CheckEquals(3,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    CheckEquals(1,FieldByName('id').AsInteger);

    GotoBookmark(BM3);
    CheckEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM4);
    CheckEquals(14,FieldByName('id').AsInteger);

    GotoBookmark(BM3);
    CheckEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM5);
    CheckEquals(14,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    CheckEquals(1,FieldByName('id').AsInteger);

    next;
    delete;

    GotoBookmark(BM2);
    CheckEquals(3,FieldByName('id').AsInteger,'After #2 deleted');
    
    delete;delete;

    GotoBookmark(BM3);
    CheckEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    CheckEquals(1,FieldByName('id').AsInteger);
    insert;
    fieldbyname('id').AsInteger:=20;
    insert;
    fieldbyname('id').AsInteger:=21;
    insert;
    fieldbyname('id').AsInteger:=22;
    insert;
    fieldbyname('id').AsInteger:=23;
    post;
    
    GotoBookmark(BM3);
    CheckEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    CheckEquals(1,FieldByName('id').AsInteger);

    GotoBookmark(BM5);
    CheckEquals(14,FieldByName('id').AsInteger);
    end;
end;

procedure TTestCursorDBBasics.TestBookmarkValid;
var BM1,BM2,BM3,BM4,BM5 : TBookmark;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
    BM1 := Nil;
    CheckFalse(BookmarkValid(BM1));
    open;
    BM1:=GetBookmark; // id=1, BOF
    CheckTrue(BookmarkValid(BM1));
    next;next;
    BM2:=GetBookmark; // id=3
    CheckTrue(BookmarkValid(BM2));
    next;next;next;
    BM3:=GetBookmark; // id=6
    CheckTrue(BookmarkValid(BM3));
    next;next;next;next;next;next;next;next;
    BM4:=GetBookmark; // id=14
    CheckTrue(BookmarkValid(BM4));
    next;
    BM5:=GetBookmark; // id=14, EOF
    CheckTrue(BookmarkValid(BM5));

    CheckTrue(BookmarkValid(BM4));
    CheckTrue(BookmarkValid(BM3));
    CheckTrue(BookmarkValid(BM2));
    CheckTrue(BookmarkValid(BM1));
    GotoBookmark(BM2);
    CheckTrue(BookmarkValid(BM5));
    CheckTrue(BookmarkValid(BM4));
    CheckTrue(BookmarkValid(BM3));
    CheckTrue(BookmarkValid(BM2));
    CheckTrue(BookmarkValid(BM1));
    end;
end;

procedure TTestCursorDBBasics.TestLocate;
begin
  with DBConnector.GetNDataset(true,13) do
    begin
    open;
    CheckTrue(Locate('id',3,[]));

    CheckTrue(Locate('id',vararrayof([5]),[]));
    CheckEquals(5,FieldByName('id').AsInteger);

    CheckFalse(Locate('id',vararrayof([15]),[]));

    CheckTrue(Locate('id',vararrayof([13]),[]));
    CheckEquals(13,FieldByName('id').AsInteger);
    close;

    open;
    CheckTrue(Locate('id',vararrayof([12]),[]));
    CheckEquals(12,FieldByName('id').AsInteger);

    CheckTrue(Locate('id;name',vararrayof([4,'TestName4']),[]));
    CheckEquals(4,FieldByName('id').AsInteger);

    CheckFalse(Locate('id;name',vararrayof([4,'TestName5']),[]));
    end;
end;

procedure TTestCursorDBBasics.TestLocateCaseIns;
// Tests case insensitive locate, also partial key locate, both against string fields.
// Together with TestLocateCaseInsInts, checks 23509 DBF: locate with loPartialkey behaviour differs depending on index use
begin
  with DBConnector.GetNDataset(true,13) do
    begin
    open;
    CheckFalse(Locate('name',vararrayof(['TEstName5']),[]));
    CheckTrue(Locate('name',vararrayof(['TEstName5']),[loCaseInsensitive]));
    CheckEquals(5,FieldByName('id').AsInteger);

    CheckFalse(Locate('name',vararrayof(['TestN']),[]));
    CheckTrue(Locate('name',vararrayof(['TestN']),[loPartialKey]));

    CheckFalse(Locate('name',vararrayof(['TestNA']),[loPartialKey]));
    CheckTrue(Locate('name',vararrayof(['TestNA']),[loPartialKey, loCaseInsensitive]));
    close;
    end;
end;

procedure TTestCursorDBBasics.TestLocateCaseInsInts;
// Tests case insensitive locate, also partial key locate, both against integer fields.
// Together with TestLocateCaseIns, checks 23509 DBF: locate with loPartialkey behaviour differs depending on index use
begin
  with DBConnector.GetNDataset(true,13) do
    begin
    open;
    // To really test bug 23509: we should first have a record that matches greater than for non-string locate:
    first;
    insert;
    fieldbyname('id').AsInteger:=55;
    fieldbyname('name').AsString:='TestName55';
    post;
    first;

    CheckTrue(Locate('id',vararrayof([5]),[]));
    CheckEquals(5,FieldByName('id').AsInteger);
    first;

    CheckTrue(Locate('id',vararrayof([5]),[loCaseInsensitive]));
    CheckEquals(5,FieldByName('id').AsInteger);
    first;

    // Check specifying partial key doesn't influence search results
    CheckTrue(Locate('id',vararrayof([5]),[loPartialKey]));
    CheckEquals(5,FieldByName('id').AsInteger);
    first;

    CheckTrue(Locate('id',vararrayof([5]),[loPartialKey, loCaseInsensitive]));
    CheckEquals(5,FieldByName('id').AsInteger);

    close;
    end;
end;

procedure TTestCursorDBBasics.TestLookup;
var v: variant;
begin
  // Lookup doesn't move the record pointer of the dataset
  //  and no scroll events should be generated (only OnCalcFields when matched record is found)
  with DBConnector.GetNDataset(13) do
  begin
    Open;
    Next;
    CheckEquals('TestName5', Lookup('id',5,'name'));
    CheckTrue(Lookup('id',15,'name')=Null);
    v:=Lookup('id',7,'id;name');
    CheckEquals(7, v[0]);
    CheckEquals('TestName7', v[1]);
    // Lookup shouldn't change current record
    CheckEquals(2, FieldByName('id').AsInteger);
    Close;
  end;
end;

procedure TTestCursorDBBasics.TestFieldOldValueObsolete;
var v : variant;
    ds: TDataset;
begin
  // this test was created as reaction to AV bug found in TCustomBufDataset.GetFieldData
  // when retrieving OldValue (State=dsOldValue) of newly inserted or appended record.
  // In this case was CurrBuff set to nil (and not checked),
  // because OldValuesBuffer for just inserted record is nil. See rev.17704
  // (So purpose of this test isn't test InsertRecord on empty dataset or so)
  // Later was this test replaced by more complex TestOldValue (superset of old test),
  // but next to it was restored back also original test.
  // So now we have two tests which test same thing, where this 'old' one is subset of 'new' one
  // Ideal solution would be remove this 'old' test as it does not test anything what is not tested elsewhere ...
  ds := DBConnector.GetNDataset(0) as TDataset;
  ds.Open;
  ds.InsertRecord([0,'name']);
  v := VarToStr(ds.Fields[1].OldValue);
end;

procedure TTestCursorDBBasics.TestFieldOldValue;
var ds: TDataSet;
    OldValue: string;
    Fmemo: TField;
begin
  ds := DBConnector.GetFieldDataset;
  with ds do
  begin;
    Open;
    First;
    Next;
    OldValue := Fields[0].AsString;

    CheckEquals(OldValue, VarToStr(Fields[0].OldValue), 'Original value');  // unmodified original value
    CheckTrue(UpdateStatus=usUnmodified, 'Unmodified');

    Edit;
    Fields[0].AsInteger := -1;
    CheckEquals(OldValue, VarToStr(Fields[0].OldValue), 'Editing');  // dsEdit, there is no update-buffer yet
    Post;
    CheckEquals(OldValue, VarToStr(Fields[0].OldValue), 'Edited');  // there is already update-buffer
    CheckTrue(UpdateStatus=usModified, 'Modified');

    Append;
    Fields[0].AsInteger := -2;
    CheckTrue(VarIsNull(Fields[0].OldValue), 'Inserting'); // dsInsert, there is no update-buffer yet
    Post;
    CheckTrue(VarIsNull(Fields[0].OldValue), 'Inserted'); // there is already update-buffer
    CheckTrue(UpdateStatus=usInserted, 'Inserted');

    // Blobs are stored in a special way
    // Use TMemoField because it implements AsVariant as AsString
    First;
    Next;
    Fmemo := FieldByName('F'+FieldTypeNames[ftMemo]);
    OldValue := Fmemo.AsString;

    CheckEquals(OldValue, Fmemo.OldValue, 'Memo.OldValue');
    Edit;
    Fmemo.AsString := 'Changed Memo value';
    CheckEquals(OldValue, Fmemo.OldValue, 'Memo.OldValue before Post');
    Post;
    CheckEquals(OldValue, Fmemo.OldValue, 'Memo.OldValue after Post');
  end;
  if ds is TCustomBufDataset then
    with ds as TCustomBufDataset do
    begin
      MergeChangeLog;
      CheckEquals('Changed Memo value', Fmemo.OldValue, 'Memo.OldValue after MergeChangeLog');
    end;
end;

procedure TTestCursorDBBasics.TestChangeBlobFieldBeforePost;
// Edit memo fields should read back new contents even before post
// Bug 15376
// See also TTestFieldTypes.TestChangeBlob
var
  Fmemo: TField;
begin
  with DBConnector.GetFieldDataset do
    begin
    Open;
    Append;
    FieldByName('ID').AsInteger := -1; // Required - not null

    Fmemo := FieldByName('FMEMO');
    CheckTrue(Fmemo.IsNull, 'IsNull after Append');

    Fmemo.AsString:='MEMO1';
    CheckFalse(Fmemo.IsNull, 'IsNull after change');
    CheckEquals('MEMO1', Fmemo.AsString);

    Fmemo.Clear;
    CheckTrue(Fmemo.IsNull, 'IsNull after Clear');

    Fmemo.AsString:='MEMO2';
    CheckEquals('MEMO2', Fmemo.AsString);

    Fmemo.AsString:='';
    CheckTrue(Fmemo.IsNull, 'IsNull');

    Fmemo.AsString:='MEMO3';
    CheckEquals('MEMO3', Fmemo.AsString);
    Post;
    CheckEquals('MEMO3', Fmemo.AsString);
    Close;
    end;
end;

procedure TTestDBBasics.TestSetFieldValues;
var PassException : boolean;
begin
  with DBConnector.GetNDataset(true,11) do
    begin
    open;
    // First and Next methods are supported by UniDirectional datasets
    first;
    if IsUniDirectional then
      CheckException(Edit, EDatabaseError)
    else
      begin
      edit;
      FieldValues['id']:=5;
      post;
      CheckEquals('TestName1',FieldByName('name').AsString);
      CheckEquals(5,FieldByName('id').AsInteger);
      edit;
      FieldValues['name']:='FieldValuesTestName';
      post;
      CheckEquals('FieldValuesTestName',FieldByName('name').AsString);
      CheckEquals(5,FieldByName('id').AsInteger);
      edit;
      FieldValues['id;name']:= VarArrayOf([243,'ValuesTestName']);
      post;
      CheckEquals('ValuesTestName',FieldByName('name').AsString);
      CheckEquals(243,FieldByName('id').AsInteger);
    
      PassException:=false;
      try
        edit;
        FieldValues['id;name;fake']:= VarArrayOf([243,'ValuesTestName',4]);
      except
        on E: EDatabaseError do PassException := True;
      end;
      post;
      CheckTrue(PassException);
      end;
    end;
end;

procedure TTestDBBasics.TestGetFieldValues;
var AVar          : Variant;
    PassException : boolean;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
    open;
    AVar:=FieldValues['id'];
    CheckEquals(AVar,1);

    AVar:=FieldValues['name'];
    CheckEquals(AVar,'TestName1');

    AVar:=FieldValues['id;name'];
    CheckEquals(AVar[0],1);
    CheckEquals(AVar[1],'TestName1');

    AVar:=FieldValues['name;id;'];
    CheckEquals(AVar[1],1);
    CheckEquals(AVar[0],'TestName1');
    
    PassException:=false;
    try
      AVar:=FieldValues['name;id;fake'];
    except
      on E: EDatabaseError do PassException := True;
    end;
    CheckTrue(PassException);

    end;
end;

procedure TTestCursorDBBasics.TestDelete1;
begin
  FTestDelete1(false);
end;

procedure TTestCursorDBBasics.TestDelete2;
begin
  FTestDelete2(false);
end;

procedure TTestCursorDBBasics.TestCancelUpdDelete1;
begin
  FTestDelete1(true);
end;

procedure TTestCursorDBBasics.TestCancelUpdDelete2;
begin
  FTestDelete2(true);
end;

procedure TTestCursorDBBasics.FTestDelete1(TestCancelUpdate : boolean);
// Test the deletion of records, including the first and the last one
var i  : integer;
    ds : TDataset;
begin
  ds := DBConnector.GetNDataset(true,17);
  with ds do
    begin
    Open;

    for i := 0 to 16 do if i mod 4=0 then
      delete
    else
       next;

    First;
    for i := 0 to 16 do
      begin
      if i mod 4<>0 then
        begin
        CheckEquals(i+1,FieldByName('ID').AsInteger);
        CheckEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;
      end;
    end;

{$ifdef fpc}
  if TestCancelUpdate then
    begin
    if not (ds is TCustomBufDataset) then
      Ignore('This test only applies to TCustomBufDataset and descendents.');
    with TCustomBufDataset(ds) do
      begin
      CancelUpdates;

      First;
      for i := 0 to 16 do
        begin
        CheckEquals(i+1,FieldByName('ID').AsInteger);
        CheckEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;

      close;
      end;
    end;
{$endif}
end;

procedure TTestCursorDBBasics.FTestDelete2(TestCancelUpdate : boolean);
// Test the deletion of edited and appended records
var i : integer;
    ds : TDataset;
begin
  ds := DBConnector.GetNDataset(true,17);
  with ds do
    begin
    Open;

    for i := 0 to 16 do
      begin
      if i mod 4=0 then
        begin
        edit;
        fieldbyname('name').AsString:='this record will be gone soon';
        post;
        end;
      next;
      end;

    for i := 17 to 20 do
      begin
      append;
      fieldbyname('id').AsInteger:=i+1;
      fieldbyname('name').AsString:='TestName'+inttostr(i+1);
      post;
      end;

    first;
    for i := 0 to 20 do if i mod 4=0 then
      delete
    else
       next;

    First;
    i := 0;
    for i := 0 to 20 do
      begin
      if i mod 4<>0 then
        begin
        CheckEquals(i+1,FieldByName('ID').AsInteger);
        CheckEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;
      end;
    end;

{$ifdef fpc}
  if TestCancelUpdate then
    begin
    if not (ds is TCustomBufDataset) then
      Ignore('This test only applies to TCustomBufDataset and descendents.');
    with TCustomBufDataset(ds) do
      begin
      CancelUpdates;

      First;
      for i := 0 to 16 do
        begin
        CheckEquals(i+1,FieldByName('ID').AsInteger);
        CheckEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;

      close;
      end;
    end;
{$endif fpc}
end;

procedure TTestCursorDBBasics.TestOnFilterProc(DataSet: TDataSet; var Accept: Boolean);
var
  a : TDataSetState;
begin
  Accept := odd(Dataset.FieldByName('ID').AsInteger);
end;

procedure TTestCursorDBBasics.TestOnFilter;
// Tests OnFilterRecord filtering
var
  Counter : byte;
begin
  with DBConnector.GetNDataset(15) do
    begin
    OnFilterRecord := TestOnFilterProc;
    Filtered := True;
    Open;
    for Counter := 1 to 8 do
      begin
      CheckTrue(odd(FieldByName('ID').asinteger));
      next;
      end;
    CheckTrue(EOF);
    end;
end;

procedure TTestCursorDBBasics.TestIntFilter;
// Tests an integer range filter expression
var
  Counter : byte;
begin
  with DBConnector.GetNDataset(15) do
    begin
    Filtered := True;
    Filter := '(id>4) and (id<9)';
    Open;
    for Counter := 5 to 8 do
      begin
      CheckEquals(Counter, FieldByName('ID').AsInteger);
      Next;
      end;
    CheckTrue(EOF);

    Filter := '-id-ID=-4';
    CheckEquals(2, FieldByName('ID').AsInteger, 'Unary minus');
    Next;
    CheckTrue(EOF, 'Unary minus');

    Close;
    end;
end;

procedure TTestCursorDBBasics.TestNegativeIntFilter;
// Tests a negative integer range filter expression
var
  Counter : integer;
begin
  with DBConnector.GetNDataset(15) do
    begin
    // Change ID values to -1..-15 instead of positive
    Open;
    while not(EOF) do
      begin
      Edit;
      FieldByName('ID').AsInteger:=
        -1*(FieldByname('ID').AsInteger);
      Post;
      Next;
      end;

    // Regular filter with negative integer values
    Filtered := True;
    Filter := '(id>-9) and (id<-4)';
    First;
    for Counter := -5 downto -8 do
      begin
      CheckEquals(Counter,FieldByName('ID').AsInteger);
      Next;
      end;
    CheckTrue(EOF);

    // Filter with negative integer values and subtraction calculations
    Filtered := True;
    Filter := '(id>(-8-1)) and (id<(-3-1))';
    First;
    for Counter := -5 downto -8 do
      begin
      CheckEquals(Counter,FieldByName('ID').AsInteger);
      Next;
      end;
    CheckTrue(EOF);

    Close;
    end;
end;

procedure TTestCursorDBBasics.TestStringFilter;
// Tests string expression filters
var
  Counter : byte;
begin
  with DBConnector.GetNDataset(15) do
    begin
    Open;

    // Check equality
    Filter := '(name=''TestName3'')';
    Filtered := True;
    CheckFalse(EOF, 'Simple equality');
    CheckEquals(3,FieldByName('ID').asinteger,'Simple equality');
    CheckEquals('TestName3',FieldByName('NAME').asstring,'Simple equality');
    next;
    CheckTrue(EOF,'Simple equality');

    // Check partial compare
    Filter := '(name=''*Name5'')';
    CheckFalse(EOF, 'Partial compare');
    CheckEquals(5,FieldByName('ID').asinteger,'Partial compare');
    CheckEquals('TestName5',FieldByName('NAME').asstring,'Partial compare');
    next;
    CheckTrue(EOF,'Partial compare');

    // Check case-sensitivity
    Filter := '(name=''*name3'')';
    first;
    CheckTrue(EOF,'Case-sensitive search');

    FilterOptions:=[foCaseInsensitive];
    Filter := '(name=''testname3'')';
    first;
    CheckFalse(EOF,'Case-insensitive search');
    CheckEquals(3,FieldByName('ID').asinteger,'Case-insensitive search');
    CheckEquals('TestName3',FieldByName('NAME').asstring,'Case-insensitive search');
    next;
    CheckTrue(EOF);

    // Check case-insensitive partial compare
    Filter := '(name=''*name3'')';
    first;
    CheckFalse(EOF, 'Case-insensitive partial compare');
    CheckEquals(3,FieldByName('ID').asinteger, 'Case-insensitive partial compare');
    CheckEquals('TestName3',FieldByName('NAME').asstring, 'Case-insensitive partial compare');
    next;
    CheckTrue(EOF);

    // Multiple records with partial compare
    Filter := '(name=''*name*'')';
    first;
    CheckFalse(EOF,'Partial compare multiple records');
    CheckEquals(1,FieldByName('ID').asinteger,'Partial compare multiple records');
    CheckEquals('TestName1',FieldByName('NAME').asstring,'Partial compare multiple records');
    next;
    CheckFalse(EOF,'Partial compare multiple records');
    CheckEquals(2,FieldByName('ID').asinteger,'Partial compare multiple records');
    CheckEquals('TestName2',FieldByName('NAME').asstring,'Partial compare multiple records');

    // Invalid data with partial compare
    Filter := '(name=''*neme*'')';
    first;
    CheckTrue(EOF,'Invalid data, partial compare');

    // Multiple string filters
    Filter := '(name=''*a*'') and (name=''*m*'')';
    first;
    CheckFalse(EOF,'Multiple string filters');
    CheckEquals(1,FieldByName('ID').asinteger,'Multiple string filters');
    CheckEquals('TestName1',FieldByName('NAME').asstring,'Multiple string filters');
    next;
    CheckFalse(EOF,'Multiple string filters');
    CheckEquals(2,FieldByName('ID').asinteger,'Multiple string filters');
    CheckEquals('TestName2',FieldByName('NAME').asstring,'Multiple string filters');

    // Modify so we can use some tricky data
    Filter := ''; //show all records again and allow edits
    First;
    Edit;
    // Record 1=O'Malley
    FieldByName('NAME').AsString := 'O''Malley';
    Post;

    Next;
    Edit;
    // Record 2="Magic" Mushroom
    FieldByName('NAME').AsString := '"Magic" Mushroom';
    Post;

    Next;
    Edit;
    // Record 3=O'Malley's "Magic" Mushroom
    FieldByName('NAME').AsString := 'O''Malley''s "Magic" Mushroom';
    Post;

    // Test searching on " which can be a delimiter
    Filter := '(name=''*"Magic"*'')'; //should give record 2 and 3
    first;
    CheckFalse(EOF);
    CheckEquals(2,FieldByName('ID').asinteger,'Search for strings with ", partial compare');
    CheckEquals('"Magic" Mushroom',FieldByName('NAME').asstring,'Search for strings with ", partial compare');
    next;
    CheckFalse(EOF);
    CheckEquals(3,FieldByName('ID').asinteger,'Search for strings with ", partial compare');
    CheckEquals('O''Malley''s "Magic" Mushroom',FieldByName('NAME').asstring,'Search for strings with ", partial compare');

    // Search for strings with ' escaped, partial compare delimited by '
    Filter := '(name=''O''''Malley*'')'; //should give record 1 and 3
    first;
    CheckFalse(EOF);
    CheckEquals(1,FieldByName('ID').asinteger,'Search for strings with '' escaped, partial compare delimited by ''');
    CheckEquals('O''Malley',FieldByName('NAME').asstring,'Search for strings with '' escaped, partial compare delimited by ''');
    next;
    CheckFalse(EOF);
    CheckEquals(3,FieldByName('ID').asinteger,'Search for strings with '' escaped, partial compare delimited by ''');
    CheckEquals('O''Malley''s "Magic" Mushroom',FieldByName('NAME').asstring,'Search for strings with '' escaped, partial compare delimited by ''');

    Close;
    end;
end;

{$ifdef fpc}
procedure TTestBufDatasetDBBasics.TestIsEmpty;
begin
  with DBConnector.GetNDataset(True,1) as TCustomBufDataset do
    begin
    open;
    delete;
    Resync([]);
    ApplyUpdates;
    CheckTrue(IsEmpty);
    end;
end;

procedure TTestBufDatasetDBBasics.TestSaveAsXML;
var ds    : TDataset;
    LoadDs: TCustomBufDataset;
begin
  ds := DBConnector.GetNDataset(true,5);

  ds.open;
  TCustomBufDataset(ds).SaveToFile('test.xml');
  ds.close;

  LoadDs := TCustomBufDataset.Create(nil);
  try
    LoadDs.LoadFromFile('test.xml');
    FTestXMLDatasetDefinition(LoadDS);
  finally
    LoadDS.free;
  end;
end;

procedure TTestBufDatasetDBBasics.TestFileNameProperty;
var ds1,ds2: TDataset;
    LoadDs: TCustomBufDataset;
begin
  ds2 := nil;
  ds1 := DBConnector.GetNDataset(true,5);
  try
    ds1.open;
    TCustomBufDataset(ds1).FileName:='test.xml';
    ds1.close;

    ds2 := DBConnector.GetNDataset(True,7);
    TCustomBufDataset(ds2).FileName:='test.xml';
    ds2.Open;
    FTestXMLDatasetDefinition(Ds2);
  finally
    TCustomBufDataset(ds1).FileName:='';
    if assigned(ds2) then
      TCustomBufDataset(ds2).FileName:='';
  end;
end;

procedure TTestBufDatasetDBBasics.TestClientDatasetAsMemDataset;
var ds : TCustomBufDataset;
    i  : integer;
begin
  ds := TCustomBufDataset.Create(nil);
    try
    DS.FieldDefs.Add('ID',ftInteger);
    DS.FieldDefs.Add('NAME',ftString,50);
    DS.CreateDataset;
    DS.Open;
    for i := 1 to 10 do
      begin
      ds.Append;
      ds.FieldByName('ID').AsInteger := i;
      ds.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
      DS.Post;
      end;
    ds.first;
    for i := 1 to 10 do
      begin
      CheckEquals(i,ds.fieldbyname('ID').asinteger);
      CheckEquals('TestName' + inttostr(i),ds.fieldbyname('NAME').AsString);
      ds.next;
      end;
    CheckTrue(ds.EOF);
    DS.Close;

  finally
    ds.Free;
  end;
end;

procedure TTestBufDatasetDBBasics.TestBufDatasetCancelUpd;
var i : byte;
begin
  with DBConnector.GetNDataset(5) as TCustomBufDataset do
    begin
    open;
    next;
    next;

    edit;
    FieldByName('name').AsString := 'changed';
    post;
    next;
    delete;

    CancelUpdates;

    First;

    for i := 1 to 5 do
      begin
      CheckEquals(i,fields[0].AsInteger);
      CheckEquals('TestName'+inttostr(i),fields[1].AsString);
      Next;
      end;
    end;
end;

procedure TTestBufDatasetDBBasics.TestBufDatasetCancelUpd1;
var i : byte;
begin
  with DBConnector.GetNDataset(5) as TCustomBufDataset do
    begin
    open;
    next;
    next;

    delete;
    insert;
    FieldByName('id').AsInteger := 100;
    post;

    CancelUpdates;

    last;

    for i := 5 downto 1 do
      begin
      CheckEquals(i,fields[0].AsInteger);
      CheckEquals('TestName'+inttostr(i),fields[1].AsString);
      Prior;
      end;
    end;
end;

procedure TTestBufDatasetDBBasics.TestMultipleDeleteUpdateBuffer;
var ds    : TDataset;
begin
  ds := DBConnector.GetNDataset(true,5);

  ds.open;
  with TCustomBufDataset(ds) do
    begin
    CheckEquals(0,ChangeCount);
    edit;
    fieldbyname('id').asinteger := 500;
    fieldbyname('name').AsString := 'JoJo';
    post;
    CheckEquals(1,ChangeCount);
    next; next;
    Delete;
    CheckEquals(2,ChangeCount);
    Delete;
    CheckEquals(3,ChangeCount);
    CancelUpdates;
    end;
  ds.close;
end;

procedure TTestBufDatasetDBBasics.TestDoubleDelete;
var ds    : TCustomBufDataset;
begin
  ds := TCustomBufDataset(DBConnector.GetNDataset(true,5));

  with ds do
    begin
    open;
    next; next;
    Delete;
    Delete;

    first;
    CheckEquals(1,fieldbyname('id').AsInteger);
    next;
    CheckEquals(2,fieldbyname('id').AsInteger);
    next;
    CheckEquals(5,fieldbyname('id').AsInteger);

    CancelUpdates;

    first;
    CheckEquals(1,fieldbyname('id').AsInteger);
    next;
    CheckEquals(2,fieldbyname('id').AsInteger);
    next;
    CheckEquals(3,fieldbyname('id').AsInteger);
    next;
    CheckEquals(4,fieldbyname('id').AsInteger);
    next;
    CheckEquals(5,fieldbyname('id').AsInteger);
    end;
end;

procedure TTestBufDatasetDBBasics.TestReadOnly;
var
  ds: TCustomBufDataset;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    ReadOnly:=true;
    CheckFalse(CanModify);
    end;
end;

procedure TTestBufDatasetDBBasics.TestMergeChangeLog;
var
  ds: TCustomBufDataset;
  i: integer;
  s, FN: string;
begin
  ds := DBConnector.GetNDataset(5) as TCustomBufDataset;
  with ds do
    begin
    open;
    Edit;
    i := fields[0].AsInteger;
    s := fields[1].AsString;
    fields[0].AsInteger:=64;
    fields[1].AsString:='Changed1';
    Post;
    checkequals(fields[0].OldValue,i);
    checkequals(fields[1].OldValue,s);
    CheckEquals(ChangeCount,1);
    Next;
    Edit;
    i := fields[0].AsInteger;
    s := fields[1].AsString;
    fields[0].AsInteger:=23;
    fields[1].AsString:='Changed2';
    Post;
    checkequals(fields[0].OldValue,i);
    checkequals(fields[1].OldValue,s);
    CheckEquals(ChangeCount,2);
    MergeChangeLog;
    CheckEquals(ChangeCount,0);
    checkequals(fields[0].OldValue,23);
    checkequals(fields[1].OldValue,'Changed2');
    end;

  // Test handling of [Update]BlobBuffers in TBufDataset
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    // Testing scenario: read some records, so blob data are added into FBlobBuffers,
    // then update blob field, so element is added to FUpdateBlobBuffers, then read again some records
    // so next elements are added to FBlobBuffers, then again update blob field
    // DefaultBufferCount is 10
    PacketRecords:=1;
    Open;
    FN := 'F'+FieldTypeNames[ftBlob];
    First;     Edit; FieldByName(FN).AsString:='b01'; Post;
    RecNo:=11; Edit; FieldByName(FN).AsString:='b11'; Post;
    Next     ; Edit; FieldByName(FN).AsString:='b12'; Post;
    Last;
    MergeChangeLog;
    First;     CheckEquals('b01', FieldByName(FN).AsString);
    RecNo:=11; CheckEquals('b11', FieldByName(FN).AsString);
    Next;      CheckEquals('b12', FieldByName(FN).AsString);
    end;
end;

procedure TTestBufDatasetDBBasics.FTestXMLDatasetDefinition(ADataset: TDataset);
var i : integer;
begin
  CheckEquals(2,ADataset.FieldDefs.Count);
  CheckEquals(2,ADataset.Fields.Count);
  CheckTrue(SameText('ID',ADataset.Fields[0].FieldName));
  CheckTrue(SameText('NAME',ADataset.Fields[1].FieldName));
  CheckEquals(ord(ftString), ord(ADataset.Fields[1].DataType), 'Incorrect FieldType');
  i := 1;
  while not ADataset.EOF do
    begin
    CheckEquals('TestName'+inttostr(i),ADataset.FieldByName('name').AsString);
    ADataset.Next;
    inc(i);
    end;
end;

procedure TTestBufDatasetDBBasics.TestAddIndexFieldType(AFieldType: TFieldType; ActiveDS : boolean);
var ds : TCustomBufDataset;
    FList : TStringList;
    LastValue : Variant;
    StrValue : String;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    
    if not ActiveDS then
      begin
      AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
      IndexName:='testindex';
      end
    else
      MaxIndexesCount := 3;

    try
      open;
    except
      if not assigned(ds.FindField('F'+FieldTypeNames[AfieldType])) then
        Ignore('Fields of the type ' + FieldTypeNames[AfieldType] + ' are not supported by this type of dataset')
      else
        raise;
    end;

    if ActiveDS then
      begin
      if not assigned(ds.FindField('F'+FieldTypeNames[AfieldType])) then
        Ignore('Fields of the type ' + FieldTypeNames[AfieldType] + ' are not supported by this type of dataset');
      AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
      IndexName:='testindex';
      First;
      end;

    LastValue:=null;
    while not eof do
      begin
      if AFieldType=ftString then
        CheckTrue(AnsiCompareStr(VarToStr(LastValue),VarToStr(FieldByName('F'+FieldTypeNames[AfieldType]).AsString))<=0)
      else
        CheckTrue(LastValue<=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant);
      LastValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant;
      Next;
      end;

    while not bof do
      begin
      if AFieldType=ftString then
        CheckTrue(AnsiCompareStr(VarToStr(LastValue),VarToStr(FieldByName('F'+FieldTypeNames[AfieldType]).AsString))>=0)
      else
        CheckTrue(LastValue>=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant);
      LastValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant;
      Prior;
      end;
    end;
end;

procedure TTestBufDatasetDBBasics.TestAddIndexSmallInt;
begin
  TestAddIndexFieldType(ftSmallint,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexBoolean;
begin
  TestAddIndexFieldType(ftBoolean,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexFloat;
begin
  TestAddIndexFieldType(ftFloat,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexInteger;
begin
  TestAddIndexFieldType(ftInteger,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexLargeInt;
begin
  TestAddIndexFieldType(ftLargeint,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexDateTime;
begin
  TestAddIndexFieldType(ftDateTime,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexCurrency;
begin
  TestAddIndexFieldType(ftCurrency,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexBCD;
begin
  TestAddIndexFieldType(ftBCD,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexFmtBCD;
begin
  TestAddIndexFieldType(ftFmtBCD,False);
end;

procedure TTestBufDatasetDBBasics.TestAddIndex;
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
    FList := TStringList.Create;
    try
    FList.Sorted:=true;
    FList.CaseSensitive:=True;
    FList.Duplicates:=dupAccept;
    open;

    while not eof do
      begin
      flist.Add(FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Next;
      end;

    IndexName:='testindex';
    first;
    i:=0;

    while not eof do
      begin
      CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    while not bof do
      begin
      dec(i);
      CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Prior;
      end;
    finally
      flist.free;
    end;  
    end;
end;

procedure TTestBufDatasetDBBasics.TestAddDescIndex;
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[],'F'+FieldTypeNames[AfieldType]);
    FList := TStringList.Create;
    try
      FList.Sorted:=true;
      FList.CaseSensitive:=True;
      FList.Duplicates:=dupAccept;
      open;

      while not eof do
        begin
        flist.Add(FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
        Next;
        end;

      IndexName:='testindex';
      first;
      i:=FList.Count-1;

      while not eof do
        begin
        CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
        dec(i);
        Next;
        end;

      while not bof do
        begin
        inc(i);
        CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
        Prior;
        end;
    finally
      flist.free;
    end;  
    end;
end;

procedure TTestBufDatasetDBBasics.TestAddCaseInsIndex;
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[],'','F'+FieldTypeNames[AfieldType]);
    FList := TStringList.Create;
    try
      FList.Sorted:=true;
      FList.Duplicates:=dupAccept;
      open;

      while not eof do
        begin
        flist.Add(FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
        Next;
        end;

      IndexName:='testindex';
      first;
      i:=0;

      while not eof do
        begin
        CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
        inc(i);
        Next;
        end;

      while not bof do
        begin
        dec(i);
        CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
        Prior;
        end;
    finally
      FList.Free;
    end;  
    end;
end;

procedure TTestBufDatasetDBBasics.TestInactSwitchIndex;
// Test if the default-index is properly build when the active index is not
// the default-index while opening then dataset
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
    IndexName:='testindex';
    open;
    IndexName:=''; // This should set the default index (default_order)
    first;
    
    i := 0;

    while not eof do
      begin
      CheckEquals(testStringValues[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;
    end;
end;

procedure TTestBufDatasetDBBasics.TestAddIndexActiveDS;
var ds   : TCustomBufDataset;
    I    : integer;
begin
  TestAddIndexFieldType(ftString,true);
end;

procedure TTestBufDatasetDBBasics.TestAddIndexEditDS;
var ds        : TCustomBufDataset;
    I         : integer;
    LastValue : String;
begin
  ds := DBConnector.GetNDataset(True,5) as TCustomBufDataset;
  with ds do
    begin
    MaxIndexesCount:=3;
    open;
    edit;
    FieldByName('name').asstring := 'Zz';
    post;
    next;
    next;
    edit;
    FieldByName('name').asstring := 'aA';
    post;

    AddIndex('test','name',[]);

    first;
    ds.IndexName:='test';
    first;
    LastValue:='';
    while not eof do
      begin
      CheckTrue(AnsiCompareStr(LastValue,FieldByName('name').AsString)<=0);
      LastValue:=FieldByName('name').AsString;
      Next;
      end;
    end;
end;

procedure TTestBufDatasetDBBasics.TestIndexFieldNamesActive;
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    FList := TStringList.Create;
    try
    FList.Sorted:=true;
    FList.CaseSensitive:=True;
    FList.Duplicates:=dupAccept;
    open;

    while not eof do
      begin
      flist.Add(FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Next;
      end;

    IndexFieldNames:='F'+FieldTypeNames[AfieldType];
    first;
    i:=0;

    while not eof do
      begin
      CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    while not bof do
      begin
      dec(i);
      CheckEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Prior;
      end;

    CheckEquals('F'+FieldTypeNames[AfieldType],IndexFieldNames);

    IndexFieldNames:='ID';
    first;
    i:=0;

    while not eof do
      begin
      CheckEquals(testStringValues[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    CheckEquals('ID',IndexFieldNames);

    IndexFieldNames:='';
    first;
    i:=0;

    while not eof do
      begin
      CheckEquals(testStringValues[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    CheckEquals('',IndexFieldNames);
    finally
      flist.free;
    end;  

    end;
end;

procedure TTestBufDatasetDBBasics.TestIndexCurRecord;
// Test if the currentrecord stays the same after an index change
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    i : integer;
    OldID : Integer;
    OldStringValue : string;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
    open;

    for i := 0 to (testValuesCount div 3) do
      Next;

    OldID:=FieldByName('id').AsInteger;
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;

    IndexName:='testindex';

    CheckEquals(OldID,FieldByName('id').AsInteger);
    CheckEquals(OldStringValue,FieldByName('F'+FieldTypeNames[AfieldType]).AsString);

    next;
    CheckTrue(OldStringValue<=FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
    prior;
    prior;
    CheckTrue(OldStringValue>=FieldByName('F'+FieldTypeNames[AfieldType]).AsString);

    OldID:=FieldByName('id').AsInteger;
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;

    IndexName:='';

    CheckEquals(OldID,FieldByName('id').AsInteger);
    CheckEquals(OldStringValue,FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
    
    next;
    CheckEquals(OldID+1,FieldByName('ID').AsInteger);
    prior;
    prior;
    CheckEquals(OldID-1,FieldByName('ID').AsInteger);
    end;
end;

procedure TTestBufDatasetDBBasics.TestAddDblIndex;
var ds : TCustomBufDataset;
    LastInteger : Integer;
    LastString : string;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin

    AddIndex('testindex','F'+FieldTypeNames[ftString]+';F'+FieldTypeNames[ftInteger],[]);
    open;

    IndexName:='testindex';
    first;

    LastString:='';
    while not eof do
      begin
      CheckTrue(AnsiCompareStr(FieldByName('F'+FieldTypeNames[ftString]).AsString,LastString)>=0);
      LastString:= FieldByName('F'+FieldTypeNames[ftString]).AsString;

      LastInteger:=-MaxInt;
      while (FieldByName('F'+FieldTypeNames[ftString]).AsString=LastString) and not eof do
        begin
        CheckTrue(FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger>=LastInteger);
        LastInteger:=FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger;
        next;
        end;
      end;
    while not bof do
      begin
      CheckTrue(AnsiCompareStr(FieldByName('F'+FieldTypeNames[ftString]).AsString,LastString)<=0);
      LastString:= FieldByName('F'+FieldTypeNames[ftString]).AsString;

      LastInteger:=+MaxInt;
      while (FieldByName('F'+FieldTypeNames[ftString]).AsString=LastString) and not bof do
        begin
        CheckTrue(FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger<=LastInteger);
        LastInteger:=FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger;
        prior;
        end;
      end;
    end;
end;

procedure TTestBufDatasetDBBasics.TestIndexEditRecord;
// Tests index sorting for string field type by
// editing an existing record in the middle
// with a value at the end of the alphabet
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    i : integer;
    OldID : Integer;
    OldStringValue : string;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
    IndexName:='testindex';
    open; //Record 0
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;
    next; //Now on record 1
    CheckTrue(OldStringValue<=FieldByName('F'+FieldTypeNames[AfieldType]).AsString,'Record 0 must be smaller than record 1 with asc sorted index');
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;
    next; //Now on record 2
    CheckTrue(AnsiCompareStr(OldStringValue,FieldByName('F'+FieldTypeNames[AfieldType]).AsString)<=0,'Record 1 must be smaller than record 2 with asc sorted index');

    prior; //Now on record 1
    edit;
    FieldByName('F'+FieldTypeNames[AfieldType]).AsString := 'ZZZ'; //should be sorted last
    post;

    prior; // Now on record 0
    // Check ZZZ is sorted on/after record 0
    CheckTrue(AnsiCompareStr('ZZZ',FieldByName('F'+FieldTypeNames[AfieldType]).AsString)>=0, 'Prior>');
    next;
    next; // Now on record 2
    // Check ZZZ is sorted on/before record 2
    CheckTrue(AnsiCompareStr('ZZZ',FieldByName('F'+FieldTypeNames[AfieldType]).AsString)<=0, 'Next<');
    close;
    end;
end;

procedure TTestBufDatasetDBBasics.TestIndexAppendRecord;
var i: integer;
    LastValue: string;
begin
  // start with empty dataset
  with DBConnector.GetNDataset(true,0) as TCustomBufDataset do
  begin
    MaxIndexesCount:=4;
    // add index to closed dataset with no data
    AddIndex('testindex','NAME',[]);
    IndexName:='testindex';
    Open;
    // empty dataset and other than default index (default_order) active
    CheckTrue(BOF, 'No BOF when opening empty dataset');
    CheckTrue(EOF, 'No EOF when opening empty dataset');

    // append data at end
    for i:=20 downto 0 do
      AppendRecord([i, inttostr(i)]);
    // insert data at begining
    IndexName:='';
    First;
    for i:=21 to 22 do
      InsertRecord([i, inttostr(i)]);

    // swith to index and check if records are ordered
    IndexName := 'testindex';
    LastValue := '';
    First;
    for i:=22 downto 0 do
    begin
      CheckEquals(23-i, RecNo, 'testindex.RecNo:');
      CheckTrue(AnsiCompareStr(LastValue,Fields[1].AsString) < 0, 'testindex.LastValue>=CurrValue');
      LastValue := Fields[1].AsString;
      Next;
    end;
    CheckTrue(EOF, 'testindex.No EOF after last record');

    // switch back to default index (unordered)
    IndexName:='';
    First;
    for i:=22 downto 0 do
    begin
      CheckEquals(23-i, RecNo, 'index[0].RecNo:');
      CheckEquals(i, Fields[0].AsInteger, 'index[0].Fields[0].Value:');
      Next;
    end;
    CheckTrue(EOF, 'index[0].No EOF after last record');

    // add index to opened dataset with data
    AddIndex('testindex2','ID',[]);
    IndexName:='testindex2';
    First;
    for i:=0 to 22 do
    begin
      CheckEquals(1+i, RecNo, 'index2.RecNo:');
      CheckEquals(i, Fields[0].AsInteger, 'index2.Fields[0].Value:');
      Next;
    end;
    CheckTrue(EOF, 'index2.No EOF after last record');

    Close;
  end;
end;

procedure TTestBufDatasetDBBasics.TestIndexFieldNames;
var ds : TCustomBufDataset;
    AFieldType : TFieldType;
    PrevValue : String;
begin
  ds := DBConnector.GetFieldDataset as TCustomBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    
    IndexFieldNames:='F'+FieldTypeNames[AfieldType];

    open;
    PrevValue:='';
    while not eof do
      begin
      CheckTrue(AnsiCompareStr(FieldByName('F'+FieldTypeNames[AfieldType]).AsString,PrevValue)>=0);
      PrevValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;
      Next;
      end;

    CheckEquals('F'+FieldTypeNames[AfieldType],IndexFieldNames);

    end;
end;

procedure TTestBufDatasetDBBasics.TestIndexFieldNamesClosed;
var s : string;
    bufds: TCustomBufDataset;
begin
  bufds := DBConnector.GetNDataset(5) as TCustomBufDataset;
  s := bufds.IndexFieldNames;
  s := bufds.IndexName;
  bufds.CompareBookmarks(nil,nil);
end;
{$endif fpc}

procedure TTestCursorDBBasics.TestFirst;
var i : integer;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
    open;
    CheckEquals(1,FieldByName('ID').AsInteger);
    First;
    CheckEquals(1,FieldByName('ID').AsInteger);
    next;
    CheckEquals(2,FieldByName('ID').AsInteger);
    First;
    CheckEquals(1,FieldByName('ID').AsInteger);
    for i := 0 to 12 do
      next;
    CheckEquals(14,FieldByName('ID').AsInteger);
    First;
    CheckEquals(1,FieldByName('ID').AsInteger);
    close;
    end;
end;

procedure TTestCursorDBBasics.TestEofAfterFirst;
begin
  with DBConnector.GetNDataset(0) do
    begin
    open;
    CheckTrue(eof);
    CheckTrue(BOF);
    first;
    CheckTrue(eof);
    CheckTrue(BOF);
    end;
end;

procedure TTestDBBasics.TestfieldDefinition(AFieldType : TFieldType;ADatasize : integer;var ADS : TDataset; var AFld: TField);

var i          : byte;

begin
  ADS := DBConnector.GetFieldDataset;
  ADS.Open;

  AFld := ADS.FindField('F'+FieldTypeNames[AfieldType]);

{$ifdef fpc}
  if not assigned (AFld) then
    Ignore('Fields of the type ' + FieldTypeNames[AfieldType] + ' are not supported by this type of dataset');
{$endif fpc}
  CheckEquals(ord(AFieldType), ord(AFld.DataType), 'DataType');
  CheckEquals(ADatasize, AFld.DataSize, 'DataSize');
end;

procedure TTestDBBasics.TestSupportIntegerFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;
    DbfTableLevel: integer;

begin
  if (uppercase(dbconnectorname)='DBF') then
  begin
    DbfTableLevel:=strtointdef(dbconnectorparams,4);
    if not(DBFTableLevel in [7,30]) then
      Ignore('TDBF: only Visual Foxpro and DBase7 support full integer range.');
  end;

  TestfieldDefinition(ftInteger,4,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testIntValues[i],Fld.AsInteger);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportSmallIntFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  if (uppercase(dbconnectorname)='DBF') then
    Ignore('TDBF: Smallint support only from -999 to 9999');

  TestfieldDefinition(ftSmallint,2,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testSmallIntValues[i],Fld.AsInteger);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportWordFields;
var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftWord,2,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testWordValues[i],Fld.AsInteger);
    ds.Next;
    end;
  ds.close;
end;


procedure TTestDBBasics.TestSupportStringFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftString,11,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    if (uppercase(dbconnectorname)<>'DBF') then
      CheckEquals(testStringValues[i],Fld.AsString)
    else {DBF right-trims spaces in string fields }
      CheckEquals(TrimRight(testStringValues[i]),Fld.AsString);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportBooleanFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftBoolean,2,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testBooleanValues[i],Fld.AsBoolean);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportFloatFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftFloat,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testFloatValues[i],Fld.AsFloat);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportLargeIntFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftLargeint,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testLargeIntValues[i],Fld.AsLargeInt);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportDateFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftDate,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testDateValues[i], FormatDateTime('yyyy/mm/dd', Fld.AsDateTime, DBConnector.FormatSettings));
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportTimeFields;
var i          : byte;
    ds         : TDataset;
    Fld        : TField;
begin
  TestfieldDefinition(ftTime,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testTimeValues[i],DateTimeToTimeString(fld.AsDateTime));
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportCurrencyFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  if (uppercase(dbconnectorname)='DBF') then
    Ignore('This test does not apply to TDBF as they store currency in BCD fields.');

  TestfieldDefinition(ftCurrency,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testCurrencyValues[i],Fld.AsCurrency);
    CheckEquals(testCurrencyValues[i],Fld.AsFloat);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportBCDFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftBCD,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(CurrToStr(testCurrencyValues[i]),Fld.AsString);
    CheckEquals(testCurrencyValues[i],Fld.AsCurrency);
    CheckEquals(testCurrencyValues[i],Fld.AsFloat);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportFmtBCDFields;
var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftFMTBcd,sizeof(TBCD),ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testFmtBCDValues[i], BCDToStr(Fld.AsBCD,DBConnector.FormatSettings), 'AsBCD');
    CheckEquals(StrToFloat(testFmtBCDValues[i],DBConnector.FormatSettings), Fld.AsFloat, 1e-12, 'AsFloat');
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportFixedStringFields;
var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftFixedChar,11,ds,Fld);
  for i := 0 to testValuesCount-1 do
    begin
    if Fld.IsNull then // If the field is null, .AsString always returns an empty, non-padded string
      CheckEquals(testStringValues[i],Fld.AsString)
    else
{$ifdef fpc}
      CheckEquals(PadRight(testStringValues[i],10),Fld.AsString);
{$else fpc}
      CheckEquals(testStringValues[i],Fld.AsString);
{$endif fpc}
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportBlobFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;
begin
  TestfieldDefinition(ftBlob,0,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testValues[ftBlob,i],Fld.AsString);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportMemoFields;
var i          : byte;
    ds         : TDataset;
    Fld        : TField;
begin
  TestfieldDefinition(ftMemo,0,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    CheckEquals(testValues[ftMemo,i],Fld.AsString);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestBlobBlobType;
// Verifies whether all created blob types actually have blobtypes that fall
// into the blobtype range (subset of datatype enumeration)
var
  ds: TDataSet;
  i:integer;
begin
  ds := DBConnector.GetFieldDataset;
  with ds do
  begin;
    Open;
    for i:=0 to Fields.Count-1 do
      begin
      // This should only apply to blob types
      if Fields[i].DataType in [Low(TBlobType)..High(TBlobType)] then
        begin
          if not(TBlobField(Fields[i]).BlobType in [Low(TBlobType)..High(TBlobType)]) then
            fail('BlobType for field '+
              Fields[i].FieldName+' is not in blob type range. Actual value: '+
              inttostr(word(TBlobField(Fields[i]).BlobType)));
        end;
      end;
    Close;
  end;
end;

procedure TTestDBBasics.TestcalculatedField_OnCalcfields(DataSet: TDataSet);
begin
  case dataset.fieldbyname('ID').asinteger of
    1 : dataset.fieldbyname('CALCFLD').AsInteger := 5;
    2 : dataset.fieldbyname('CALCFLD').AsInteger := 70000;
    3 : dataset.fieldbyname('CALCFLD').Clear;
    4 : dataset.fieldbyname('CALCFLD').AsInteger := 1234;
    10 : dataset.fieldbyname('CALCFLD').Clear;
  else
    dataset.fieldbyname('CALCFLD').AsInteger := 1;
  end;
  CheckTrue(DataSet.State=dsCalcFields, 'State');
end;

procedure TTestDBBasics.TestCalculatedField;
var ds   : TDataset;
    AFld1, AFld2, AFld3 : Tfield;
begin
  ds := DBConnector.GetNDataset(5);
  with ds do
    begin
    AFld1 := TIntegerField.Create(ds);
    AFld1.FieldName := 'ID';
    AFld1.DataSet := ds;

    AFld2 := TStringField.Create(ds);
    AFld2.FieldName := 'NAME';
    AFld2.DataSet := ds;

    AFld3 := TIntegerField.Create(ds);
    AFld3.FieldName := 'CALCFLD';
    AFld3.DataSet := ds;
    Afld3.FieldKind := fkCalculated;

    CheckEquals(3,FieldCount);
    ds.OnCalcFields := TestcalculatedField_OnCalcfields;
    open;
    CheckEquals(1,FieldByName('ID').asinteger);
    CheckEquals(5,FieldByName('CALCFLD').asinteger);
    next;
    CheckEquals(70000,FieldByName('CALCFLD').asinteger);
    next;
    CheckTrue(FieldByName('CALCFLD').IsNull, '#3 Null');
    next;
    CheckEquals(1234,FieldByName('CALCFLD').AsInteger);
    if IsUniDirectional then
      // The CanModify property is always False, so attempts to put the dataset into edit mode always fail
      CheckException(Edit, EDatabaseError)
    else
      begin
      Edit;
      FieldByName('ID').AsInteger := 10;
      Post;
      CheckTrue(FieldByName('CALCFLD').IsNull, '#10 Null');
      end;
    close;
    AFld1.Free;
    AFld2.Free;
    AFld3.Free;
    end;
end;

procedure TTestDBBasics.TestCanModifySpecialFields;
var ds    : TDataset;
    lds   : TDataset;
    fld   : TField;
begin
  lds := DBConnector.GetNDataset(10);
  ds := DBConnector.GetNDataset(5);
  with ds do
    begin
    Fld := TIntegerField.Create(ds);
    Fld.FieldName:='ID';
    Fld.DataSet:=ds;

    Fld := TStringField.Create(ds);
    Fld.FieldName:='LookupFld';
    Fld.FieldKind:=fkLookup;
    Fld.DataSet:=ds;
    Fld.LookupDataSet:=lds;
    Fld.LookupResultField:='NAME';
    Fld.LookupKeyFields:='ID';
    Fld.KeyFields:='ID';

    lds.Open;
    Open;
    if IsUniDirectional then
      // The CanModify property is always False for UniDirectional datasets
      CheckFalse(FieldByName('ID').CanModify)
    else
      CheckTrue(FieldByName('ID').CanModify);
    CheckFalse(FieldByName('LookupFld').CanModify);
    CheckFalse(FieldByName('ID').ReadOnly);
    CheckFalse(FieldByName('LookupFld').ReadOnly);

    CheckEquals(1,FieldByName('ID').AsInteger);
    if IsUniDirectional then
      // Lookup fields are not supported by UniDirectional datasets
      CheckTrue(FieldByName('LookupFld').IsNull)
    else
      CheckEquals('TestName1',FieldByName('LookupFld').AsString);
    Next;
    Next;
    CheckEquals(3,FieldByName('ID').AsInteger);
    if IsUniDirectional then
      CheckTrue(FieldByName('LookupFld').IsNull)
    else
      CheckEquals('TestName3',FieldByName('LookupFld').AsString);

    Close;
    lds.Close;
    end;
end;

procedure TTestDBBasics.TestDoubleClose;
begin
  with DBConnector.GetNDataset(1) do
    begin
    close;
    close;
    open;
    close;
    close;
    end;
end;

procedure TTestDBBasics.TestFieldDefsUpdate;
begin
  // FieldDefs.Update is called also by Lazarus IDE Fields editor
  with DBConnector.GetNDataset(0) do
    begin
    // call Update on closed unprepared dataset
    FieldDefs.Update;
    CheckEquals(2, FieldDefs.Count);
    end;
end;

procedure TTestDBBasics.TestAssignFieldftString;
var AParam : TParam;
    AField : TField;
begin
  AParam := TParam.Create(nil);

  with DBConnector.GetNDataset(1) do
    begin
    open;
    AField := fieldbyname('name');
    AParam.AssignField(AField);
    CheckEquals(ord(ftString), ord(AParam.DataType), 'DataType');
    close;
    end;
  AParam.Free;
end;

procedure TTestDBBasics.TestAssignFieldftFixedChar;
var AParam : TParam;
    AField : TField;
begin
  AParam := TParam.Create(nil);
  with DBConnector.GetNDataset(1) do
    begin
    open;
    AField := fieldbyname('name');
    (AField as tstringfield).FixedChar := true;
    AParam.AssignField(AField);
    CheckEquals(ord(ftFixedChar), ord(AParam.DataType), 'DataType');
    close;
    end;
  AParam.Free;
end;

procedure TTestCursorDBBasics.TestBug7007;

var
  datalink1: tdatalink;
  datasource1: tdatasource;
  query1: TDataSet;

begin
  query1:= DBConnector.GetNDataset(6);
  datalink1:= TTestDataLink.create;
  datasource1:= tdatasource.create(nil);
  try
    datalink1.datasource:= datasource1;
    datasource1.dataset:= query1;
    datalink1.datasource:= datasource1;

    DataEvents := '';
    query1.open;
    datalink1.buffercount:= query1.recordcount;
    CheckEquals('deUpdateState:0;',DataEvents);
    CheckEquals(0, datalink1.ActiveRecord);
    CheckEquals(6, datalink1.RecordCount);
    CheckEquals(6, query1.RecordCount);
    CheckEquals(1, query1.RecNo);

    DataEvents := '';
    query1.append;
    CheckEquals('deCheckBrowseMode:0;deUpdateState:0;deDataSetChange:0;DataSetChanged;',DataEvents);
    CheckEquals(5, datalink1.ActiveRecord);
    CheckEquals(6, datalink1.RecordCount);
    CheckEquals(6, query1.RecordCount);
    CheckTrue(query1.RecNo in [0,7]);

    DataEvents := '';
    query1.cancel;
    CheckEquals('deCheckBrowseMode:0;deUpdateState:0;deDataSetChange:0;DataSetChanged;',DataEvents);
    CheckEquals(5, datalink1.ActiveRecord);
    CheckEquals(6, datalink1.RecordCount);
    CheckEquals(6, query1.RecordCount);
    CheckEquals(6, query1.RecNo);
  finally
    datalink1.free;
    datasource1.free;
  end;
end;

procedure TTestCursorDBBasics.TestBug6893;
var
  datalink1: tdatalink;
  datasource1: tdatasource;
  query1: TDataSet;

begin
  query1:= DBConnector.GetNDataset(25);
  datalink1:= TDataLink.create;
  datasource1:= tdatasource.create(nil);
  try
    datalink1.datasource:= datasource1;
    datasource1.dataset:= query1;

    datalink1.buffercount:= 5;
    query1.active := true;
    query1.MoveBy(20);
{$ifdef fpc}
    CheckEquals(5, THackDataLink(datalink1).Firstrecord);
    CheckEquals(4, datalink1.ActiveRecord);
    CheckEquals(21, query1.RecNo);

    query1.active := False;

    CheckEquals(0, THackDataLink(datalink1).Firstrecord);
    CheckEquals(0, datalink1.ActiveRecord);

    query1.active := true;

    CheckEquals(0, THackDataLink(datalink1).Firstrecord);
    CheckEquals(0, datalink1.ActiveRecord);
    CheckEquals(1, query1.RecNo);
{$endif fpc}

  finally
    datalink1.free;
    datasource1.free;
  end;
end;

procedure TTestCursorDBBasics.TestNullAtOpen;
begin
  with dbconnector.getndataset(0) do
    begin
    active:= true;
    CheckTrue(fieldbyname('id').IsNull,'Field isn''t NULL on a just-opened empty dataset');
    append;
    CheckTrue(fieldbyname('id').IsNull,'Field isn''t NULL after append on an empty dataset');
    fieldbyname('id').asinteger:= 123;
    cancel;
    CheckTrue(fieldbyname('id').IsNull,'Field isn''t NULL after cancel');
    end;
end;

{ TDBBasicsUniDirectionalTestSetup }
{$ifdef fpc}
procedure TDBBasicsUniDirectionalTestSetup.OneTimeSetup;
begin
  inherited OneTimeSetup;
  DBConnector.TestUniDirectional:=true;
end;

procedure TDBBasicsUniDirectionalTestSetup.OneTimeTearDown;
begin
  DBConnector.TestUniDirectional:=false;
  inherited OneTimeTearDown;
end;
{$endif fpc}


initialization
{$ifdef fpc}
  RegisterTestDecorator(TDBBasicsTestSetup, TTestDBBasics);
  RegisterTestDecorator(TDBBasicsTestSetup, TTestCursorDBBasics);

  // The SQL connectors are descendents of bufdataset and therefore benefit from testing:
  if (uppercase(dbconnectorname)='SQL') or (uppercase(dbconnectorname)='BUFDATASET') then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestBufDatasetDBBasics);
    RegisterTestDecorator(TDBBasicsUniDirectionalTestSetup, TTestUniDirectionalDBBasics);
    end;
{$else fpc}
  RegisterTest(TTestDBBasics.Suite);
{$endif fpc}
end.
