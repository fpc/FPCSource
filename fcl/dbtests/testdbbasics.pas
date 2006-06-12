unit TestDBBasics;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  fpcunit, testutils, testregistry, testdecorator,
  Classes, SysUtils;

type

  { TTestSQLMechanism }

  { TTestDBBasics }

  TTestDBBasics = class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RunTest; override;
  published
    procedure TestSelectQueryBasics;
    procedure TestPostOnlyInEditState;
    procedure TestMove;                    // bug 5048
    procedure TestActiveBufferWhenClosed;
    procedure TestEOFBOFClosedDataset;
    procedure TestdeFieldListChange;
    procedure TestLastAppendCancel;        // bug 5058
    procedure TestRecNo;                   // bug 5061

    procedure TestBufDatasetCancelUpdates; //bug 6938
    procedure TestBufDatasetCancelUpdates1;

  end;

  { TSQLTestSetup }

  TDBBasicsTestSetup = class(TTestSetup)
  protected

    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  end;

implementation

uses db, toolsunit;

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
    AssertTrue(CompareText('ID',fields[0].DisplayName)=0); // uitzoeken verschil displaylabel
    AssertTrue('The datatype of the field ''ID'' is incorrect, it should be ftInteger',ftInteger=fields[0].DataType);

    AssertTrue(CompareText('NAME',fields[1].FieldName)=0);
    AssertTrue(CompareText('NAME',fields[1].DisplayName)=0); // uitzoeken verschil displaylabel
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
{$IFDEF FPC}
    AssertException('Post was called in a non-edit state',EDatabaseError,@Post);
{$ELSE}
    AssertException('Post was called in a non-edit state',EDatabaseError,Post);
{$ENDIF}
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
  for count := 0 to 32 do with DBConnector.GetNDataset(count) do
    begin
    aDatasource.DataSet := DBConnector.GetNDataset(count);
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
        AssertEquals('deCheckBrowseMode:0;deDataSetScroll:-1;',DataEvents)
      else
        AssertEquals('deCheckBrowseMode:0;deDataSetScroll:0;',DataEvents);
      DataEvents := '';
      end;
    AssertEquals(count,i-1);
    close;
    AssertEquals('deUpdateState:0;',DataEvents);
    DataEvents := '';
    end;
end;

procedure TTestDBBasics.TestdeFieldListChange;

var i,count     : integer;
    aDatasource : TDataSource;
    aDatalink   : TDataLink;

begin
  aDatasource := TDataSource.Create(nil);
  aDatalink := TTestDataLink.Create;
  aDatalink.DataSource := aDatasource;
  with DBConnector.GetNDataset(1) do
    begin
    aDatasource.DataSet := DBConnector.GetNDataset(1);
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
{$IFDEF fpc}
    AssertNull(ActiveBuffer);
{$ENDIF}
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
begin
  with DBConnector.GetNDataset(0) do
    begin
    AssertEquals('Failed to get the RecNo from a closed dataset',0,RecNo);
    AssertEquals(0,RecordCount);

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


procedure TTestDBBasics.SetUp;
begin
  DBConnector.InitialiseDatasets;
end;

procedure TTestDBBasics.TearDown;
var count : integer;
begin
  DBConnector.FreeDatasets;
end;

procedure TTestDBBasics.RunTest;
begin
  inherited RunTest;
//  inherited RunTest;
//  inherited RunTest;
end;

procedure TTestDBBasics.TestBufDatasetCancelUpdates;
var i : byte;
begin
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

procedure TTestDBBasics.TestBufDatasetCancelUpdates1;
var i : byte;
begin
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
