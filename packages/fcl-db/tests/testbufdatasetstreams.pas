unit TestBufDatasetStreams;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  fpcunit, testutils, testregistry, testdecorator,
  Classes, SysUtils, db, BufDataset;

type

  { TTestBufDatasetStreams }

  TUpdDatasetProc = procedure(ADataset : TBufDataset) of object;

  TTestBufDatasetStreams = class(TTestCase)
  private
    procedure CompareDatasets(ADataset1, ADataset2 : TDataset);

    procedure TestChangesApplyUpdates(AUpdDatasetProc : TUpdDatasetProc; Inserts: Boolean=False);
    procedure TestChangesCancelUpdates(AUpdDatasetProc : TUpdDatasetProc);
    procedure TestChangesXML(AUpdDatasetProc : TUpdDatasetProc);

    procedure SimpleEditChange(ADataset: TBufDataset);
    procedure SimpleDeleteChange(ADataset: TBufDataset);
    procedure MoreDeletesChange(ADataset: TBufDataset);
    procedure SimpleInsertChange(ADataset: TBufDataset);
    procedure MoreInsertsChange(ADataset: TBufDataset);
    procedure SeveralEditsChange(ADataset: TBufDataset);
    procedure DeleteAllChange(ADataset: TBufDataset);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleEditCancelUpd;
    procedure TestSimpleDeleteCancelUpd;
    procedure TestMoreDeletesCancelUpd;
    procedure TestSimpleInsertCancelUpd;
    procedure MoreInsertsCancelUpd;
    procedure SeveralEditsCancelUpd;
    procedure DeleteAllCancelUpd;

    procedure TestSimpleEditApplUpd;
    procedure TestSimpleDeleteApplUpd;
    procedure TestMoreDeletesApplUpd;
    procedure TestSimpleInsertApplUpd;
    procedure MoreInsertsApplUpd;
    procedure SeveralEditsApplUpd;
    procedure DeleteAllApplUpd;

    procedure TestBasicsXML;
    procedure TestSimpleEditXML;
    procedure TestSimpleDeleteXML;
    procedure TestMoreDeletesXML;
    procedure TestSimpleInsertXML;
    procedure TestMoreInsertsXML;
    procedure TestSeveralEditsXML;
    procedure TestDeleteAllXML;

    procedure TestFileNameProperty;
  end;

  { TSQLTestSetup }

  TDBBasicsTestSetup = class(TTestSetup)
  protected
    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  end;

implementation

uses toolsunit, SQLDBToolsUnit, sqldb, XMLDatapacketReader;

procedure TTestBufDatasetStreams.CompareDatasets(ADataset1,
  ADataset2: TDataset);
begin
  ADataset1.First;
  ADataset2.First;
  while not ADataset1.EOF do
    begin
    AssertEquals(ADataset1.FieldByName('ID').AsInteger,ADataset2.FieldByName('ID').AsInteger);
    AssertEquals(ADataset1.FieldByName('NAME').AsString,ADataset2.FieldByName('NAME').AsString);
    ADataset1.Next;
    ADataset2.Next;
    end;
end;

procedure TTestBufDatasetStreams.TestChangesApplyUpdates(
  AUpdDatasetProc: TUpdDatasetProc; Inserts : Boolean);
var OrgDs,
    ChangedDs : TBufDataset;
begin
  OrgDs := DBConnector.GetNDataset(true,15) as TBufDataset;
  OrgDs.Open;
  AUpdDatasetProc(OrgDs);
  OrgDs.ApplyUpdates;

  if Inserts then
    begin
    ChangedDs := TSQLDBConnector(DBConnector).Query;
    TSQLQuery(ChangedDS).SQL.Text:='SELECT * FROM FPDEV WHERE (ID < 16) or (ID>100) ORDER BY ID';

    OrgDs.IndexFieldNames:='ID';
    OrgDs.First;
    end
  else
    ChangedDs := DBConnector.GetNDataset(true,15) as TBufDataset;
  ChangedDs.Open;
  CompareDatasets(OrgDs,ChangedDs);
end;

procedure TTestBufDatasetStreams.TestChangesCancelUpdates(
  AUpdDatasetProc: TUpdDatasetProc);
var OrgDs,
    ChangedDs : TBufDataset;
begin
  OrgDs := DBConnector.GetNDataset(true,15) as TBufDataset;
  OrgDs.Open;
  AUpdDatasetProc(OrgDs);
  OrgDs.CancelUpdates;

  ChangedDs := DBConnector.GetNDataset(true,15) as TBufDataset;
  ChangedDs.Open;
  CompareDatasets(OrgDs,ChangedDs);
end;

procedure TTestBufDatasetStreams.TestChangesXML(AUpdDatasetProc: TUpdDatasetProc);
var SaveDs,
    LoadDs : TBufDataset;
begin
  SaveDs := DBConnector.GetNDataset(true,15) as TBufDataset;
  SaveDs.Open;
  AUpdDatasetProc(SaveDs);
  SaveDs.SaveToFile('Basics.xml',dfXML);

  LoadDs := TBufDataset.Create(nil);
  LoadDs.LoadFromFile('Basics.xml');

  CompareDatasets(SaveDs,LoadDs);
  SaveDs.Close;

  SaveDs.Open;
  LoadDs.CancelUpdates;
  CompareDatasets(SaveDs,LoadDs);
  LoadDs.Free;
end;

procedure TTestBufDatasetStreams.SimpleEditChange(ADataset: TBufDataset);
begin
  ADataset.next;
  ADataset.edit;
  ADataset.FieldByName('name').AsString:='jojo';
  ADataset.Post;
end;

procedure TTestBufDatasetStreams.SimpleDeleteChange(ADataset: TBufDataset);
begin
  ADataset.Next;
  ADataset.Delete;
end;

procedure TTestBufDatasetStreams.MoreDeletesChange(ADataset: TBufDataset);
begin
  with ADataset do
    begin
    Delete;
    Next;
    // Next is to test what happens if the delete-buffer of one record is linked
    // to another deleted record
    Delete;
    Delete;
    Next;
    Next;
    Next;
    Next;
    // Next is to test two delete-update buffers which are linked to one record
    Delete;
    Prior;
    Delete;
    Last;
    Delete;
    end;
end;

procedure TTestBufDatasetStreams.SimpleInsertChange(ADataset: TBufDataset);
begin
  ADataset.next;
  ADataset.insert;
  ADataset.FieldByName('id').AsInteger:=500;
  ADataset.FieldByName('name').AsString:='TestName0500';
  ADataset.Post;
end;

procedure TTestBufDatasetStreams.MoreInsertsChange(ADataset: TBufDataset);
begin
  with ADataset do
    begin
    insert;
    FieldByName('id').AsInteger:=501;
    FieldByName('name').AsString:='TestName0501';
    Post;

    next;
    next;
    insert;
    FieldByName('id').AsInteger:=502;
    FieldByName('name').AsString:='TestName0502';
    Post;

    next;
    insert;
    FieldByName('id').AsInteger:=503;
    FieldByName('name').AsString:='TestName0503';
    Post;

    next;
    insert;
    FieldByName('id').AsInteger:=504;
    FieldByName('name').AsString:='TestName0504';
    Post;
    insert;
    FieldByName('id').AsInteger:=505;
    FieldByName('name').AsString:='TestName0505';
    Post;

    last;
    insert;
    FieldByName('id').AsInteger:=506;
    FieldByName('name').AsString:='TestName0506';
    Post;

    Append;
    FieldByName('id').AsInteger:=507;
    FieldByName('name').AsString:='TestName0507';
    Post;
    end;
end;

procedure TTestBufDatasetStreams.SeveralEditsChange(ADataset: TBufDataset);
begin
  with ADataset do
    begin
    edit;
    fieldbyname('id').asinteger := 500;
    fieldbyname('name').AsString := 'JoJo';
    post;

    next; next;
    Delete;
    Delete;

    next;next;
    insert;
    fieldbyname('id').asinteger := 501;
    post;
    end;
end;

procedure TTestBufDatasetStreams.DeleteAllChange(ADataset: TBufDataset);
begin
  with ADataset do
    while not eof do delete;
end;

procedure TTestBufDatasetStreams.SetUp;
begin
  DBConnector.StartTest;
end;

procedure TTestBufDatasetStreams.TearDown;
begin
  DBConnector.StopTest;
end;

procedure TTestBufDatasetStreams.TestSimpleEditCancelUpd;
begin
  TestChangesCancelUpdates(@SimpleEditChange);
end;

procedure TTestBufDatasetStreams.TestSimpleDeleteCancelUpd;
begin
  TestChangesCancelUpdates(@SimpleDeleteChange);
end;

procedure TTestBufDatasetStreams.TestMoreDeletesCancelUpd;
begin
  TestChangesCancelUpdates(@MoreDeletesChange);
end;

procedure TTestBufDatasetStreams.TestSimpleInsertCancelUpd;
begin
  TestChangesCancelUpdates(@SimpleInsertChange);
end;

procedure TTestBufDatasetStreams.MoreInsertsCancelUpd;
begin
  TestChangesCancelUpdates(@MoreInsertsChange);
end;

procedure TTestBufDatasetStreams.SeveralEditsCancelUpd;
begin
  TestChangesCancelUpdates(@SeveralEditsChange);
end;

procedure TTestBufDatasetStreams.DeleteAllCancelUpd;
begin
  TestChangesCancelUpdates(@DeleteAllChange);
end;

procedure TTestBufDatasetStreams.TestBasicsXML;
var SaveDs: TBufDataset;
    LoadDs: TBufDataset;
begin
  SaveDs := DBConnector.GetNDataset(true,15) as TBufDataset;
  SaveDs.Open;
  SaveDs.SaveToFile('Basics.xml',dfXML);

  LoadDs := TBufDataset.Create(nil);
  LoadDs.LoadFromFile('Basics.xml');
  CompareDatasets(SaveDs,LoadDs);
  LoadDs.Free;
end;

procedure TTestBufDatasetStreams.TestSimpleEditXML;
begin
  TestChangesXML(@SimpleEditChange);
end;

procedure TTestBufDatasetStreams.TestSimpleDeleteXML;
begin
  TestChangesXML(@SimpleDeleteChange);
end;

procedure TTestBufDatasetStreams.TestMoreDeletesXML;
begin
  TestChangesXML(@MoreDeletesChange);
end;

procedure TTestBufDatasetStreams.TestSimpleInsertXML;
begin
  TestChangesXML(@SimpleInsertChange);
end;

procedure TTestBufDatasetStreams.TestMoreInsertsXML;
begin
  TestChangesXML(@MoreInsertsChange);
end;

procedure TTestBufDatasetStreams.TestSeveralEditsXML;
begin
  TestChangesXML(@SeveralEditsChange);
end;

procedure TTestBufDatasetStreams.TestDeleteAllXML;
begin
  TestChangesXML(@DeleteAllChange);
end;

procedure TTestBufDatasetStreams.TestFileNameProperty;
var ds    : TDataset;
    LoadDs: TDataset;
begin
  ds := DBConnector.GetNDataset(true,5);

  ds.open;
  TBufDataset(ds).FileName:='test.dat';
  ds.close;

  LoadDs := DBConnector.GetNDataset(True,2);
  TBufDataset(LoadDs).FileName:='test.dat';
  LoadDs.Open;

  ds := DBConnector.GetNDataset(true,4);
  ds.Open;
  CompareDatasets(ds,LoadDs);
  ds.close;
  LoadDs.close;
end;

procedure TTestBufDatasetStreams.TestSimpleEditApplUpd;
begin
  TestChangesApplyUpdates(@SimpleEditChange);
end;

procedure TTestBufDatasetStreams.TestSimpleDeleteApplUpd;
begin
  TestChangesApplyUpdates(@SimpleDeleteChange);
end;

procedure TTestBufDatasetStreams.TestMoreDeletesApplUpd;
begin
  TestChangesApplyUpdates(@MoreDeletesChange);
end;

procedure TTestBufDatasetStreams.TestSimpleInsertApplUpd;
begin
  TestChangesApplyUpdates(@SimpleInsertChange,True);
end;

procedure TTestBufDatasetStreams.MoreInsertsApplUpd;
begin
  TestChangesApplyUpdates(@MoreInsertsChange,True);
end;

procedure TTestBufDatasetStreams.SeveralEditsApplUpd;
begin
  TestChangesApplyUpdates(@SeveralEditsChange, True);
end;

procedure TTestBufDatasetStreams.DeleteAllApplUpd;
begin
  TestChangesApplyUpdates(@DeleteAllChange, False);
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
  if uppercase(dbconnectorname)='SQL' then
    RegisterTestDecorator(TDBBasicsTestSetup, TTestBufDatasetStreams);
end.
