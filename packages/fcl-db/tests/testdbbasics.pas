unit TestDBBasics;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  fpcunit, testutils, testregistry, testdecorator,
  Classes, SysUtils, db;

type

  { TTestDBBasics }

  TTestDBBasics = class(TTestCase)
  private
    procedure TestOnFilterProc(DataSet: TDataSet; var Accept: Boolean);
    procedure TestfieldDefinition(AFieldType : TFieldType;ADatasize : integer;var ADS : TDataset; var AFld: TField);
    procedure TestcalculatedField_OnCalcfields(DataSet: TDataSet);

    procedure FTestDelete1(TestCancelUpdate : boolean);
    procedure FTestDelete2(TestCancelUpdate : boolean);
    procedure FTestXMLDatasetDefinition(ADataset : TDataset);
    procedure TestAddIndexFieldType(AFieldType : TFieldType; ActiveDS : boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFileNameProperty;
    procedure TestClientDatasetAsMemDataset;
    procedure TestCancelUpdDelete1;
    procedure TestCancelUpdDelete2;
    procedure TestSafeAsXML;
    procedure TestAppendInsertRecord;
    procedure TestBookmarks;
    procedure TestBookmarkValid;

    procedure TestLocate;

    procedure TestFirst;
    procedure TestDelete1;
    procedure TestDelete2;
    procedure TestIntFilter;
    procedure TestOnFilter;
    procedure TestStringFilter;

    procedure TestSetFieldValues;
    procedure TestGetFieldValues;

    procedure TestAddIndex;
    procedure TestAddDescIndex;
    procedure TestAddCaseInsIndex;
    procedure TestInactSwitchIndex;

    procedure TestAddIndexInteger;
    procedure TestAddIndexSmallInt;
    procedure TestAddIndexBoolean;
    procedure TestAddIndexFloat;
    procedure TestAddIndexLargeInt;
    procedure TestAddIndexDateTime;
    procedure TestAddIndexCurrency;
    procedure TestAddIndexBCD;

    procedure TestAddIndexActiveDS;
    procedure TestAddIndexEditDS;

    procedure TestIndexFieldNames;
    procedure TestIndexFieldNamesAct;
    
    procedure TestIndexCurRecord;

    procedure TestAddDblIndex;
    procedure TestIndexEditRecord;

    procedure TestNullAtOpen;

    procedure TestSupportIntegerFields;
    procedure TestSupportSmallIntFields;
    procedure TestSupportStringFields;
    procedure TestSupportBooleanFields;
    procedure TestSupportFloatFields;
    procedure TestSupportLargeIntFields;
    procedure TestSupportDateFields;
    procedure TestSupportCurrencyFields;
    procedure TestSupportBCDFields;

    procedure TestIsEmpty;
    procedure TestAppendOnEmptyDataset;
    procedure TestInsertOnEmptyDataset;

    procedure TestBufDatasetCancelUpd; //bug 6938
    procedure TestEofAfterFirst;           //bug 7211
    procedure TestBufDatasetCancelUpd1;
    procedure TestDoubleClose;
    procedure TestCalculatedField;
    procedure TestAssignFieldftString;
    procedure TestAssignFieldftFixedChar;
    procedure TestSelectQueryBasics;
    procedure TestPostOnlyInEditState;
    procedure TestMove;                    // bug 5048
    procedure TestActiveBufferWhenClosed;
    procedure TestEOFBOFClosedDataset;
    procedure TestDataEventsResync;
    procedure TestBug7007;
    procedure TestBug6893;
    procedure TestRecordcountAfterReopen;  // partly bug 8228
    procedure TestdeFieldListChange;
    procedure TestLastAppendCancel;        // bug 5058
    procedure TestRecNo;                   // bug 5061
    procedure TestSetRecNo;                // bug 6919
    procedure TestRequired;
  end;

  { TSQLTestSetup }

  TDBBasicsTestSetup = class(TTestSetup)
  protected
    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  end;

implementation

uses toolsunit, bufdataset, variants;

type THackDataLink=class(TdataLink);

procedure TTestDBBasics.TestIsEmpty;
begin
  if not (DBConnector.GetNDataset(5) is TBufDataset) then
    Ignore('This test only applies to TBufDataset and descendents.');
  with tbufdataset(DBConnector.GetNDataset(True,1)) do
    begin
    open;
    delete;
    refresh;
    applyupdates;
    AssertTrue(IsEmpty);

    end;
end;

procedure TTestDBBasics.TestAppendOnEmptyDataset;
begin
  with DBConnector.GetNDataset(0) do
    begin
    open;
    AssertTrue(CanModify);
    AssertTrue(eof);
    AssertTrue(bof);
    append;
    AssertFalse(Bof);
    AssertTrue(Eof);
    post;
    AssertFalse(eof);
    AssertFalse(bof);
    end;
end;

procedure TTestDBBasics.TestInsertOnEmptyDataset;
begin
  with DBConnector.GetNDataset(0) do
    begin
    open;
    AssertTrue(CanModify);
    AssertTrue(eof);
    AssertTrue(bof);
    AssertTrue(IsEmpty);
    insert;
    AssertTrue(Bof);
    AssertTrue(Eof);
    AssertFalse(IsEmpty);
    post;
    AssertFalse(IsEmpty);
    AssertFalse(eof);
    AssertFalse(bof);
    end;
end;

procedure TTestDBBasics.TestSelectQueryBasics;
var b : TFieldType;
begin
  with DBConnector.GetNDataset(1) do
    begin
    Open;

    AssertEquals(1,RecNo);
    AssertEquals(1,RecordCount);

    AssertEquals(2,FieldCount);

    AssertTrue(CompareText('ID',fields[0].FieldName)=0);
    AssertTrue(CompareText('ID',fields[0].DisplayName)=0);
    AssertTrue('The datatype of the field ''ID'' is incorrect, it should be ftInteger',ftInteger=fields[0].DataType);

    AssertTrue(CompareText('NAME',fields[1].FieldName)=0);
    AssertTrue(CompareText('NAME',fields[1].DisplayName)=0);
    AssertTrue(ftString=fields[1].DataType);

    AssertEquals(1,fields[0].Value);
    AssertEquals('TestName1',fields[1].Value);

    Close;
    end;
end;

procedure TTestDBBasics.TestPostOnlyInEditState;
begin
  with DBConnector.GetNDataset(1) do
    begin
    open;
    AssertException('Post was called in a non-edit state',EDatabaseError,Post);
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
      AssertEquals('deUpdateState:0;',DataEvents);
      DataEvents := '';
      while not EOF do
        begin
        AssertEquals(i,fields[0].AsInteger);
        AssertEquals('TestName'+inttostr(i),fields[1].AsString);
        inc(i);

        Next;
        if (i > ABufferCount) and not EOF then
          AssertEquals('deCheckBrowseMode:0;deDataSetScroll:-1;DataSetScrolled:1;',DataEvents)
        else
          AssertEquals('deCheckBrowseMode:0;deDataSetScroll:0;DataSetScrolled:0;',DataEvents);
        DataEvents := '';
        end;
      AssertEquals(count,i-1);
      close;
      AssertEquals('deUpdateState:0;',DataEvents);
      DataEvents := '';
      end;
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
    open;
    Fields.add(tfield.Create(DBConnector.GetNDataset(1)));
    AssertEquals('deUpdateState:0;deFieldListChange:0;',DataEvents);
    DataEvents := '';
    fields.Clear;
    AssertEquals('deFieldListChange:0;',DataEvents)
    end;
  aDatasource.Free;
  aDatalink.Free;
end;


procedure TTestDBBasics.TestActiveBufferWhenClosed;
begin
  with DBConnector.GetNDataset(0) do
    begin
    AssertNull(ActiveBuffer);
    open;
    AssertFalse('Activebuffer of an empty dataset shouldn''t be nil',ActiveBuffer = nil);
    end;
end;

procedure TTestDBBasics.TestEOFBOFClosedDataset;
begin
  with DBConnector.GetNDataset(1) do
    begin
    AssertTrue(EOF);
    AssertTrue(BOF);
    open;
    close;
    AssertTrue(EOF);
    AssertTrue(BOF);
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
  aDatalink.DataSource := aDatasource;
  ds := DBConnector.GetNDataset(6);
  ds.BeforeScroll := DBConnector.DataEvent;
  with ds do
    begin
    aDatasource.DataSet := ds;
    open;
    DataEvents := '';
    Resync([rmExact]);
    AssertEquals('deDataSetChange:0;',DataEvents);
    DataEvents := '';
    next;
    AssertEquals('deCheckBrowseMode:0;DataEvent;deDataSetScroll:0;DataSetScrolled:1;',DataEvents);
    close;
    end;
  aDatasource.Free;
  aDatalink.Free;
end;

procedure TTestDBBasics.TestLastAppendCancel;

var count : integer;

begin
  for count := 0 to 32 do with DBConnector.GetNDataset(count) do
    begin
    open;

    Last;
    Append;
    Cancel;

    AssertEquals(count,fields[0].asinteger);
    AssertEquals(count,RecordCount);

    Close;

    end;
end;

procedure TTestDBBasics.TestRecNo;
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
      AssertEquals('Failed to get the RecNo from a closed dataset',0,RecNo);

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
      AssertEquals('Failed to get the Recordcount from a closed dataset',0,RecNo);

    Open;

    AssertEquals(0,RecordCount);
    AssertEquals(0,RecNo);

    first;
    AssertEquals(0,RecordCount);
    AssertEquals(0,RecNo);

    last;
    AssertEquals(0,RecordCount);
    AssertEquals(0,RecNo);

    append;
    AssertEquals(0,RecNo);
    AssertEquals(0,RecordCount);

    first;
    AssertEquals(0,RecNo);
    AssertEquals(0,RecordCount);

    append;
    FieldByName('id').AsInteger := 1;
    AssertEquals(0,RecNo);
    AssertEquals(0,RecordCount);

    first;
    AssertEquals(1,RecNo);
    AssertEquals(1,RecordCount);

    last;
    AssertEquals(1,RecNo);
    AssertEquals(1,RecordCount);

    append;
    FieldByName('id').AsInteger := 1;
    AssertEquals(0,RecNo);
    AssertEquals(1,RecordCount);

    Close;
    end;
end;

procedure TTestDBBasics.TestSetRecNo;
begin
  with DBConnector.GetNDataset(15) do
    begin
    Open;
    RecNo := 1;
    AssertEquals(1,fields[0].AsInteger);
    AssertEquals(1,RecNo);

    RecNo := 2;
    AssertEquals(2,fields[0].AsInteger);
    AssertEquals(2,RecNo);

    RecNo := 8;
    AssertEquals(8,fields[0].AsInteger);
    AssertEquals(8,RecNo);

    RecNo := 15;
    AssertEquals(15,fields[0].AsInteger);
    AssertEquals(15,RecNo);

    RecNo := 3;
    AssertEquals(3,fields[0].AsInteger);
    AssertEquals(3,RecNo);

    RecNo := 14;
    AssertEquals(14,fields[0].AsInteger);
    AssertEquals(14,RecNo);

    RecNo := 15;
    AssertEquals(15,fields[0].AsInteger);
    AssertEquals(15,RecNo);

    // test for exceptions...
{    RecNo := 16;
    AssertEquals(15,fields[0].AsInteger);
    AssertEquals(15,RecNo);}

    Close;
    end;
end;

procedure TTestDBBasics.TestRequired;
begin
  with DBConnector.GetNDataset(2) do
    begin
    Open;
    FieldByName('ID').Required := True;
    Append;
    AssertException(EDatabaseError,Post);
    FieldByName('ID').AsInteger := 1000;
    Post;
    Close;
    end;
end;


procedure TTestDBBasics.SetUp;
begin
  DBConnector.StartTest;
end;

procedure TTestDBBasics.TearDown;
begin
  DBConnector.StopTest;
end;

procedure TTestDBBasics.TestSafeAsXML;
var ds    : TDataset;
    LoadDs: TBufDataset;
begin
  ds := DBConnector.GetNDataset(true,5);
  if not (ds is TBufDataset) then
    Ignore('This test only applies to TBufDataset and descendents.');

  ds.open;
  TBufDataset(ds).SaveToFile('test.xml');
  ds.close;

  LoadDs := TBufDataset.Create(nil);
  LoadDs.LoadFromFile('test.xml');
  FTestXMLDatasetDefinition(LoadDS);
end;

procedure TTestDBBasics.TestFileNameProperty;
var ds    : TDataset;
    LoadDs: TBufDataset;
begin
  ds := DBConnector.GetNDataset(true,5);
  if not (ds is TBufDataset) then
    Ignore('This test only applies to TBufDataset and descendents.');

  ds.open;
  TBufDataset(ds).FileName:='test.xml';
  ds.close;

  ds := DBConnector.GetNDataset(True,7);
  TBufDataset(ds).FileName:='test.xml';
  ds.Open;
  FTestXMLDatasetDefinition(Ds);
end;

procedure TTestDBBasics.TestClientDatasetAsMemDataset;
var ds : TBufDataset;
    i  : integer;
begin
  ds := TBufDataset.Create(nil);
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
    AssertEquals(i,ds.fieldbyname('ID').asinteger);
    AssertEquals('TestName' + inttostr(i),ds.fieldbyname('NAME').AsString);
    ds.next;
    end;
  AssertTrue(ds.EOF);
  DS.Close;
end;

procedure TTestDBBasics.TestAppendInsertRecord;
begin
  with DBConnector.GetNDataset(true,6) do
    begin
    open;
    // InsertRecord should insert a record, set the values, post the record and
    // make the new record active.
    InsertRecord([152,'TestInsRec']);
    AssertEquals(152,fields[0].AsInteger);
    AssertEquals('TestInsRec',fields[1].AsString);
    AssertTrue(state=dsBrowse);

    // AppendRecord should append a record, further the same as InsertRecord
    AppendRecord([151,'TestInsRec']);
    AssertEquals(151,fields[0].AsInteger);
    AssertEquals('TestInsRec',fields[1].AsString);
    AssertTrue(state=dsBrowse);
    next;
    AssertTrue(EOF);
    end;
end;

procedure TTestDBBasics.TestBookmarks;
var BM1,BM2,BM3,BM4,BM5 : TBookmark;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
    AssertNull(GetBookmark);
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
    AssertEquals(3,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    AssertEquals(1,FieldByName('id').AsInteger);

    GotoBookmark(BM3);
    AssertEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM4);
    AssertEquals(14,FieldByName('id').AsInteger);

    GotoBookmark(BM3);
    AssertEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM5);
    AssertEquals(14,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    AssertEquals(1,FieldByName('id').AsInteger);

    next;
    delete;

    GotoBookmark(BM2);
    AssertEquals(3,FieldByName('id').AsInteger);
    
    delete;delete;

    GotoBookmark(BM3);
    AssertEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    AssertEquals(1,FieldByName('id').AsInteger);
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
    AssertEquals(6,FieldByName('id').AsInteger);

    GotoBookmark(BM1);
    AssertEquals(1,FieldByName('id').AsInteger);

    GotoBookmark(BM5);
    AssertEquals(14,FieldByName('id').AsInteger);
    end;
end;

procedure TTestDBBasics.TestBookmarkValid;
var BM1,BM2,BM3,BM4,BM5 : TBookmark;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
    BM1 := Nil;
    AssertFalse(BookmarkValid(BM1));
    open;
    BM1:=GetBookmark; // id=1, BOF
    AssertTrue(BookmarkValid(BM1));
    next;next;
    BM2:=GetBookmark; // id=3
    AssertTrue(BookmarkValid(BM2));
    next;next;next;
    BM3:=GetBookmark; // id=6
    AssertTrue(BookmarkValid(BM3));
    next;next;next;next;next;next;next;next;
    BM4:=GetBookmark; // id=14
    AssertTrue(BookmarkValid(BM4));
    next;
    BM5:=GetBookmark; // id=14, EOF
    AssertTrue(BookmarkValid(BM5));

    AssertTrue(BookmarkValid(BM4));
    AssertTrue(BookmarkValid(BM3));
    AssertTrue(BookmarkValid(BM2));
    AssertTrue(BookmarkValid(BM1));
    GotoBookmark(BM2);
    AssertTrue(BookmarkValid(BM5));
    AssertTrue(BookmarkValid(BM4));
    AssertTrue(BookmarkValid(BM3));
    AssertTrue(BookmarkValid(BM2));
    AssertTrue(BookmarkValid(BM1));
    end;
end;

procedure TTestDBBasics.TestLocate;
begin
  with DBConnector.GetNDataset(true,13) do
    begin
    open;
    asserttrue(Locate('id',vararrayof([5]),[]));
    AssertEquals(5,FieldByName('id').AsInteger);
    AssertFalse(Locate('id',vararrayof([15]),[]));
    asserttrue(Locate('id',vararrayof([12]),[]));
    AssertEquals(12,FieldByName('id').AsInteger);
    close;
    open;
    asserttrue(Locate('id',vararrayof([12]),[]));
    AssertEquals(12,FieldByName('id').AsInteger);
    asserttrue(Locate('id;name',vararrayof([4,'TestName4']),[]));
    AssertEquals(4,FieldByName('id').AsInteger);

    assertFalse(Locate('id;name',vararrayof([4,'TestName5']),[]));

    end;
end;

procedure TTestDBBasics.TestSetFieldValues;
var PassException : boolean;
begin
  with DBConnector.GetNDataset(true,11) do
    begin
    open;
    first;
    edit;
    FieldValues['id']:=5;
    post;
    AssertEquals('TestName1',FieldByName('name').AsString);
    AssertEquals(5,FieldByName('id').AsInteger);
    edit;
    FieldValues['name']:='FieldValuesTestName';
    post;
    AssertEquals('FieldValuesTestName',FieldByName('name').AsString);
    AssertEquals(5,FieldByName('id').AsInteger);
    edit;
    FieldValues['id;name']:= VarArrayOf([243,'ValuesTestName']);
    post;
    AssertEquals('ValuesTestName',FieldByName('name').AsString);
    AssertEquals(243,FieldByName('id').AsInteger);
    
    PassException:=false;
    try
      edit;
      FieldValues['id;name;fake']:= VarArrayOf([243,'ValuesTestName',4]);
    except
      on E: EDatabaseError do PassException := True;
    end;
    post;
    AssertTrue(PassException);
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
    AssertEquals(AVar,1);

    AVar:=FieldValues['name'];
    AssertEquals(AVar,'TestName1');

    AVar:=FieldValues['id;name'];
    AssertEquals(AVar[0],1);
    AssertEquals(AVar[1],'TestName1');

    AVar:=FieldValues['name;id;'];
    AssertEquals(AVar[1],1);
    AssertEquals(AVar[0],'TestName1');
    
    PassException:=false;
    try
      AVar:=FieldValues['name;id;fake'];
    except
      on E: EDatabaseError do PassException := True;
    end;
    AssertTrue(PassException);

    end;
end;

procedure TTestDBBasics.TestFirst;
var i : integer;
begin
  with DBConnector.GetNDataset(true,14) do
    begin
    open;
    AssertEquals(1,FieldByName('ID').AsInteger);
    First;
    AssertEquals(1,FieldByName('ID').AsInteger);
    next;
    AssertEquals(2,FieldByName('ID').AsInteger);
    First;
    AssertEquals(1,FieldByName('ID').AsInteger);
    for i := 0 to 12 do
      next;
    AssertEquals(14,FieldByName('ID').AsInteger);
    First;
    AssertEquals(1,FieldByName('ID').AsInteger);
    close;
    end;
end;

procedure TTestDBBasics.TestDelete1;
begin
  FTestDelete1(false);
end;

procedure TTestDBBasics.TestDelete2;
begin
  FTestDelete2(false);
end;

procedure TTestDBBasics.TestCancelUpdDelete1;
begin
  FTestDelete1(true);
end;

procedure TTestDBBasics.TestCancelUpdDelete2;
begin
  FTestDelete2(true);
end;

procedure TTestDBBasics.FTestDelete1(TestCancelUpdate : boolean);
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
        AssertEquals(i+1,FieldByName('ID').AsInteger);
        AssertEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;
      end;
    end;
    
  if TestCancelUpdate then
    begin
    if not (ds is TBufDataset) then
      Ignore('This test only applies to TBufDataset and descendents.');
    with TBufDataset(ds) do
      begin
      CancelUpdates;

      First;
      for i := 0 to 16 do
        begin
        AssertEquals(i+1,FieldByName('ID').AsInteger);
        AssertEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;

      close;
      end;
    end;
end;

procedure TTestDBBasics.FTestDelete2(TestCancelUpdate : boolean);
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
        AssertEquals(i+1,FieldByName('ID').AsInteger);
        AssertEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;
      end;
    end;

  if TestCancelUpdate then
    begin
    if not (ds is TBufDataset) then
      Ignore('This test only applies to TBufDataset and descendents.');
    with TBufDataset(ds) do
      begin
      CancelUpdates;

      First;
      for i := 0 to 16 do
        begin
        AssertEquals(i+1,FieldByName('ID').AsInteger);
        AssertEquals('TestName'+inttostr(i+1),FieldByName('NAME').AsString);
        next;
        end;

      close;
      end;
    end;
end;

procedure TTestDBBasics.FTestXMLDatasetDefinition(ADataset: TDataset);
begin
  AssertEquals(2,ADataset.FieldDefs.Count);
  AssertEquals(5,ADataset.RecordCount);
  AssertEquals(2,ADataset.Fields.Count);
  AssertEquals('ID',ADataset.Fields[0].FieldName);
  AssertEquals('NAME',ADataset.Fields[1].FieldName);
  AssertTrue('Incorrect fieldtype',ADataset.fields[1].DataType=ftString);
  AssertEquals('TestName1',ADataset.FieldByName('name').AsString);
  ADataset.Next;
  AssertEquals('TestName2',ADataset.FieldByName('name').AsString);
end;

procedure TTestDBBasics.TestOnFilterProc(DataSet: TDataSet; var Accept: Boolean);

var a : TDataSetState;
begin
  Accept := odd(Dataset.FieldByName('ID').AsInteger);
end;

procedure TTestDBBasics.TestOnFilter;
var tel : byte;
begin
  with DBConnector.GetNDataset(15) do
    begin
    OnFilterRecord := TestOnFilterProc;
    Filtered := True;
    Open;
    for tel := 1 to 8 do
      begin
      AssertTrue(odd(FieldByName('ID').asinteger));
      next;
      end;
    AssertTrue(EOF);
    end;
end;

procedure TTestDBBasics.TestIntFilter;
var tel : byte;
begin
  with DBConnector.GetNDataset(15) do
    begin
    Filtered := True;
    Filter := '(id>4) and (id<9)';
    Open;
    for tel := 5 to 8 do
      begin
      AssertEquals(tel,FieldByName('ID').asinteger);
      next;
      end;
    AssertTrue(EOF);
    Close;
    end;
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
    AssertEquals(0, THackDataLink(datalink1).RecordCount);
    query1.active := true;
    AssertTrue(THackDataLink(datalink1).RecordCount>0);
    query1.active := false;
  finally
    datalink1.free;
    datasource1.free;
  end;
end;

procedure TTestDBBasics.TestStringFilter;
var tel : byte;
begin
  with DBConnector.GetNDataset(15) do
    begin
    Open;
    //FilterOptions := [foNoPartialCompare];
    //FilterOptions := [];
    Filter := '(name=''*Name3'')';
    Filtered := True;
    AssertFalse(EOF);
    AssertEquals(3,FieldByName('ID').asinteger);
    AssertEquals('TestName3',FieldByName('NAME').asstring);
    next;
    AssertTrue(EOF);
    Close;
    end;
end;

procedure TTestDBBasics.TestAddIndexFieldType(AFieldType: TFieldType; ActiveDS : boolean);
var ds : TBufDataset;
    FList : TStringList;
    LastValue : Variant;
    StrValue : String;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
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
        AssertTrue(AnsiCompareStr(VarToStr(LastValue),VarToStr(FieldByName('F'+FieldTypeNames[AfieldType]).AsString))<=0)
      else
        AssertTrue(LastValue<=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant);
      LastValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant;
      Next;
      end;

    while not bof do
      begin
      if AFieldType=ftString then
        AssertTrue(AnsiCompareStr(VarToStr(LastValue),VarToStr(FieldByName('F'+FieldTypeNames[AfieldType]).AsString))>=0)
      else
        AssertTrue(LastValue>=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant);
      LastValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsVariant;
      Prior;
      end;
    end;
end;

procedure TTestDBBasics.TestAddIndexSmallInt;
begin
  TestAddIndexFieldType(ftSmallint,False);
end;

procedure TTestDBBasics.TestAddIndexBoolean;
begin
  TestAddIndexFieldType(ftBoolean,False);
end;

procedure TTestDBBasics.TestAddIndexFloat;
begin
  TestAddIndexFieldType(ftFloat,False);
end;

procedure TTestDBBasics.TestAddIndexInteger;
begin
  TestAddIndexFieldType(ftInteger,False);
end;

procedure TTestDBBasics.TestAddIndexLargeInt;
begin
  TestAddIndexFieldType(ftLargeint,False);
end;

procedure TTestDBBasics.TestAddIndexDateTime;
begin
  TestAddIndexFieldType(ftDateTime,False);
end;

procedure TTestDBBasics.TestAddIndexCurrency;
begin
  TestAddIndexFieldType(ftCurrency,False);
end;

procedure TTestDBBasics.TestAddIndexBCD;
begin
  TestAddIndexFieldType(ftBCD,False);
end;

procedure TTestDBBasics.TestAddIndex;
var ds : TBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
    FList := TStringList.Create;
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
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    while not bof do
      begin
      dec(i);
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Prior;
      end;
    end;
end;

procedure TTestDBBasics.TestAddDescIndex;
var ds : TBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[],'F'+FieldTypeNames[AfieldType]);
    FList := TStringList.Create;
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
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      dec(i);
      Next;
      end;

    while not bof do
      begin
      inc(i);
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Prior;
      end;
    end;
end;

procedure TTestDBBasics.TestAddCaseInsIndex;
var ds : TBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin

    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[],'','F'+FieldTypeNames[AfieldType]);
    FList := TStringList.Create;
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
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    while not bof do
      begin
      dec(i);
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Prior;
      end;
    end;
end;

procedure TTestDBBasics.TestInactSwitchIndex;
// Test if the default-index is properly build when the active index is not
// the default-index while opening then dataset
var ds : TBufDataset;
    AFieldType : TFieldType;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
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
      AssertEquals(testStringValues[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;
    end;
end;

procedure TTestDBBasics.TestAddIndexActiveDS;
var ds   : TBufDataset;
    I    : integer;
begin
  TestAddIndexFieldType(ftString,true);
end;

procedure TTestDBBasics.TestAddIndexEditDS;
var ds        : TBufDataset;
    I         : integer;
    LastValue : String;
begin
  ds := DBConnector.GetNDataset(True,5) as TBufDataset;
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
    LastValue:=FieldByName('name').AsString;
    while not eof do
      begin
      AssertTrue(AnsiCompareStr(LastValue,FieldByName('name').AsString)<=0);
      Next;
      end;
    end;
end;

procedure TTestDBBasics.TestIndexFieldNamesAct;
var ds : TBufDataset;
    AFieldType : TFieldType;
    FList : TStringList;
    i : integer;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    FList := TStringList.Create;
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
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    while not bof do
      begin
      dec(i);
      AssertEquals(flist[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      Prior;
      end;

    AssertEquals('F'+FieldTypeNames[AfieldType],IndexFieldNames);

    IndexFieldNames:='ID';
    first;
    i:=0;

    while not eof do
      begin
      AssertEquals(testStringValues[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    AssertEquals('ID',IndexFieldNames);

    IndexFieldNames:='';
    first;
    i:=0;

    while not eof do
      begin
      AssertEquals(testStringValues[i],FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
      inc(i);
      Next;
      end;

    AssertEquals('',IndexFieldNames);

    end;
end;

procedure TTestDBBasics.TestIndexCurRecord;
// Test if the currentrecord stays the same after an index change
var ds : TBufDataset;
    AFieldType : TFieldType;
    i : integer;
    OldID : Integer;
    OldStringValue : string;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
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

    AssertEquals(OldID,FieldByName('id').AsInteger);
    AssertEquals(OldStringValue,FieldByName('F'+FieldTypeNames[AfieldType]).AsString);

    next;
    AssertTrue(OldStringValue<=FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
    prior;
    prior;
    AssertTrue(OldStringValue>=FieldByName('F'+FieldTypeNames[AfieldType]).AsString);

    OldID:=FieldByName('id').AsInteger;
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;

    IndexName:='';

    AssertEquals(OldID,FieldByName('id').AsInteger);
    AssertEquals(OldStringValue,FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
    
    next;
    AssertEquals(OldID+1,FieldByName('ID').AsInteger);
    prior;
    prior;
    AssertEquals(OldID-1,FieldByName('ID').AsInteger);
    end;
end;

procedure TTestDBBasics.TestAddDblIndex;
var ds : TBufDataset;
    LastInteger : Integer;
    LastString : string;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin

    AddIndex('testindex','F'+FieldTypeNames[ftString]+';F'+FieldTypeNames[ftInteger],[]);
    open;

    IndexName:='testindex';
    first;

    LastString:='';
    while not eof do
      begin
      AssertTrue(AnsiCompareStr(FieldByName('F'+FieldTypeNames[ftString]).AsString,LastString)>=0);
      LastString:= FieldByName('F'+FieldTypeNames[ftString]).AsString;

      LastInteger:=-MaxInt;
      while (FieldByName('F'+FieldTypeNames[ftString]).AsString=LastString) and not eof do
        begin
        AssertTrue(FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger>=LastInteger);
        LastInteger:=FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger;
        next;
        end;
      end;
    while not bof do
      begin
      AssertTrue(AnsiCompareStr(FieldByName('F'+FieldTypeNames[ftString]).AsString,LastString)<=0);
      LastString:= FieldByName('F'+FieldTypeNames[ftString]).AsString;

      LastInteger:=+MaxInt;
      while (FieldByName('F'+FieldTypeNames[ftString]).AsString=LastString) and not bof do
        begin
        AssertTrue(FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger<=LastInteger);
        LastInteger:=FieldByName('F'+FieldTypeNames[ftInteger]).AsInteger;
        prior;
        end;
      end;
    end;
end;

procedure TTestDBBasics.TestIndexEditRecord;
var ds : TBufDataset;
    AFieldType : TFieldType;
    i : integer;
    OldID : Integer;
    OldStringValue : string;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    AddIndex('testindex','F'+FieldTypeNames[AfieldType],[]);
    IndexName:='testindex';
    open;
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;
    next;
    AssertTrue(OldStringValue<=FieldByName('F'+FieldTypeNames[AfieldType]).AsString);
    OldStringValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;
    next;
    AssertTrue(AnsiCompareStr(OldStringValue,FieldByName('F'+FieldTypeNames[AfieldType]).AsString)<=0);
    prior;
    
    edit;
    FieldByName('F'+FieldTypeNames[AfieldType]).AsString := 'ZZZ';
    post;
    prior;
    AssertTrue(AnsiCompareStr('ZZZ',FieldByName('F'+FieldTypeNames[AfieldType]).AsString)>=0);
    next;
    next;
    AssertTrue(AnsiCompareStr('ZZZ',FieldByName('F'+FieldTypeNames[AfieldType]).AsString)<=0);
    close;
    end;
end;

procedure TTestDBBasics.TestIndexFieldNames;
var ds : TBufDataset;
    AFieldType : TFieldType;
    PrevValue : String;
begin
  ds := DBConnector.GetFieldDataset as TBufDataset;
  with ds do
    begin
    AFieldType:=ftString;
    
    IndexFieldNames:='F'+FieldTypeNames[AfieldType];

    open;
    PrevValue:='';
    while not eof do
      begin
      AssertTrue(AnsiCompareStr(FieldByName('F'+FieldTypeNames[AfieldType]).AsString,PrevValue)>=0);
      PrevValue:=FieldByName('F'+FieldTypeNames[AfieldType]).AsString;
      Next;
      end;

    AssertEquals('F'+FieldTypeNames[AfieldType],IndexFieldNames);

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

    AssertEquals(3,FieldCount);
    ds.OnCalcFields := TestcalculatedField_OnCalcfields;
    open;
    AssertEquals(1,FieldByName('ID').asinteger);
    AssertEquals(5,FieldByName('CALCFLD').asinteger);
    next;
    AssertEquals(70000,FieldByName('CALCFLD').asinteger);
    next;
    AssertEquals(true,FieldByName('CALCFLD').isnull);
    next;
    AssertEquals(1234,FieldByName('CALCFLD').AsInteger);
    edit;
    FieldByName('ID').AsInteger := 10;
    post;
    AssertEquals(true,FieldByName('CALCFLD').isnull);
    close;
    AFld1.Free;
    AFld2.Free;
    AFld3.Free;
    end;
end;

procedure TTestDBBasics.TestEofAfterFirst;
begin
  with DBConnector.GetNDataset(0) do
    begin
    open;
    AssertTrue(eof);
    AssertTrue(BOF);
    first;
    AssertTrue(eof);
    AssertTrue(BOF);
    end;
end;

procedure TTestDBBasics.TestfieldDefinition(AFieldType : TFieldType;ADatasize : integer;var ADS : TDataset; var AFld: TField);

var i          : byte;

begin
  ADS := DBConnector.GetFieldDataset;
  ADS.Open;

  AFld := ADS.FindField('F'+FieldTypeNames[AfieldType]);

  if not assigned (AFld) then
    Ignore('Fields of the type ' + FieldTypeNames[AfieldType] + ' are not supported by this type of dataset');
  AssertTrue(Afld.DataType = AFieldType);
  AssertEquals(ADatasize,Afld.DataSize );
end;

procedure TTestDBBasics.TestSupportIntegerFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftInteger,4,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    AssertEquals(testIntValues[i],Fld.AsInteger);
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportSmallIntFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftSmallint,2,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    AssertEquals(testSmallIntValues[i],Fld.AsInteger);
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
    AssertEquals(testStringValues[i],Fld.AsString);
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
    AssertEquals(testBooleanValues[i],Fld.AsBoolean);
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
    AssertEquals(testFloatValues[i],Fld.AsFloat);
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
    AssertEquals(testLargeIntValues[i],Fld.AsLargeInt);
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
    AssertEquals(testDateValues[i],FormatDateTime('yyyy/mm/dd',Fld.AsDateTime));
    ds.Next;
    end;
  ds.close;
end;

procedure TTestDBBasics.TestSupportCurrencyFields;

var i          : byte;
    ds         : TDataset;
    Fld        : TField;

begin
  TestfieldDefinition(ftCurrency,8,ds,Fld);

  for i := 0 to testValuesCount-1 do
    begin
    AssertEquals(testCurrencyValues[i],Fld.AsCurrency);
    AssertEquals(testCurrencyValues[i],Fld.AsFloat);
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
    AssertEquals(testCurrencyValues[i],Fld.AsCurrency);
    AssertEquals(testCurrencyValues[i],Fld.AsFloat);
    ds.Next;
    end;
  ds.close;
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
    AssertTrue(ftString=AParam.DataType);
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
    AssertTrue(ftFixedChar=AParam.DataType);
    close;
    end;
  AParam.Free;
end;

procedure TTestDBBasics.TestBufDatasetCancelUpd;
var i : byte;
begin
  if not (DBConnector.GetNDataset(5) is TBufDataset) then
    Ignore('This test only applies to TBufDataset and descendents.');
  with DBConnector.GetNDataset(5) as TBufDataset do
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
      AssertEquals(i,fields[0].AsInteger);
      AssertEquals('TestName'+inttostr(i),fields[1].AsString);
      Next;
      end;
    end;
end;

procedure TTestDBBasics.Testbug7007;

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
    AssertEquals('deUpdateState:0;',DataEvents);
    AssertEquals(0, datalink1.ActiveRecord);
    AssertEquals(6, datalink1.RecordCount);
    AssertEquals(6, query1.RecordCount);
    AssertEquals(1, query1.RecNo);

    DataEvents := '';
    query1.append;
    AssertEquals('deCheckBrowseMode:0;deUpdateState:0;deDataSetChange:0;',DataEvents);
    AssertEquals(5, datalink1.ActiveRecord);
    AssertEquals(6, datalink1.RecordCount);
    AssertEquals(6, query1.RecordCount);
    AssertTrue(query1.RecNo in [0,7]);

    DataEvents := '';
    query1.cancel;
    AssertEquals('deCheckBrowseMode:0;deUpdateState:0;deDataSetChange:0;',DataEvents);
    AssertEquals(5, datalink1.ActiveRecord);
    AssertEquals(6, datalink1.RecordCount);
    AssertEquals(6, query1.RecordCount);
    AssertEquals(6, query1.RecNo);
  finally
    datalink1.free;
    datasource1.free;
  end;
end;

procedure TTestDBBasics.TestBug6893;
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

    AssertEquals(5, THackDataLink(datalink1).Firstrecord);
    AssertEquals(4, datalink1.ActiveRecord);
    AssertEquals(21, query1.RecNo);

    query1.active := False;

    AssertEquals(0, THackDataLink(datalink1).Firstrecord);
    AssertEquals(0, datalink1.ActiveRecord);

    query1.active := true;

    AssertEquals(0, THackDataLink(datalink1).Firstrecord);
    AssertEquals(0, datalink1.ActiveRecord);
    AssertEquals(1, query1.RecNo);
    
  finally
    datalink1.free;
    datasource1.free;
  end;
end;

procedure TTestDBBasics.TestBufDatasetCancelUpd1;
var i : byte;
begin
  if not (DBConnector.GetNDataset(5) is TBufDataset) then
    Ignore('This test only applies to TBufDataset and descendents.');
  with DBConnector.GetNDataset(5) as TBufDataset do
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
      AssertEquals(i,fields[0].AsInteger);
      AssertEquals('TestName'+inttostr(i),fields[1].AsString);
      Prior;
      end;
    end;
end;

procedure TTestDBBasics.TestNullAtOpen;
begin
  with dbconnector.getndataset(0) do
    begin
    active:= true;
    AssertTrue('Field isn''t NULL on a just-opened empty dataset',fieldbyname('id').IsNull);
    append;
    AssertTrue('Field isn''t NULL after append on an empty dataset',fieldbyname('id').IsNull);
    fieldbyname('id').asinteger:= 123;
    cancel;
    AssertTrue('Field isn''t NULL after cancel',fieldbyname('id').IsNull);
    end;

end;

{ TSQLTestSetup }
procedure TDBBasicsTestSetup.OneTimeSetup;
begin
  InitialiseDBConnector;
end;

procedure TDBBasicsTestSetup.OneTimeTearDown;
begin
  FreeAndNil(DBConnector);
end;

initialization
  RegisterTestDecorator(TDBBasicsTestSetup, TTestDBBasics);
end.
