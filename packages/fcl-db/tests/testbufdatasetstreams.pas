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

  TUpdDatasetProc = procedure(ADataset : TCustomBufDataset) of object;

  TTestBufDatasetStreams = class(TTestCase)
  private
    procedure CompareDatasets(ADataset1, ADataset2 : TDataset);

    procedure TestChangesApplyUpdates(AUpdDatasetProc : TUpdDatasetProc; Inserts: Boolean=False);
    procedure TestChangesCancelUpdates(AUpdDatasetProc : TUpdDatasetProc);
    procedure TestChanges(AUpdDatasetProc : TUpdDatasetProc; AFormat : TDataPacketFormat=dfBinary);
    procedure TestChangesXML(AUpdDatasetProc : TUpdDatasetProc);

    procedure SimpleEditChange(ADataset: TCustomBufDataset);
    procedure SimpleDeleteChange(ADataset: TCustomBufDataset);
    procedure MoreDeletesChange(ADataset: TCustomBufDataset);
    procedure SimpleInsertChange(ADataset: TCustomBufDataset);
    procedure MoreInsertsChange(ADataset: TCustomBufDataset);
    procedure SeveralEditsChange(ADataset: TCustomBufDataset);
    procedure DeleteAllChange(ADataset: TCustomBufDataset);
    procedure DeleteAllInsertChange(ADataset: TCustomBufDataset);
    procedure NullInsertChange(ADataset: TCustomBufDataset);
    procedure NullEditChange(ADataset: TCustomBufDataset);
    procedure AppendDeleteChange(ADataset: TCustomBufDataset);
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
    procedure DeleteAllInsertCancelUpd;
    procedure AppendDeleteCancelUpd;

    procedure TestSimpleEditApplUpd;
    procedure TestSimpleDeleteApplUpd;
    procedure TestMoreDeletesApplUpd;
    procedure TestSimpleInsertApplUpd;
    procedure MoreInsertsApplUpd;
    procedure SeveralEditsApplUpd;
    procedure DeleteAllApplUpd;
    procedure DeleteAllInsertApplUpd;
    procedure NullInsertUpdateApplUpd;

    procedure TestBasicsXML;
    procedure TestSimpleEditXML;
    procedure TestSimpleDeleteXML;
    procedure TestMoreDeletesXML;
    procedure TestSimpleInsertXML;
    procedure TestMoreInsertsXML;
    procedure TestSeveralEditsXML;
    procedure TestDeleteAllXML;
    procedure TestDeleteAllInsertXML;

    procedure TestAppendDeleteBIN;

    procedure TestFileNameProperty;
    procedure TestXmlFileRecognition;
    procedure TestCloseDatasetNoConnection; // bug 17623
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
    ChangedDs : TCustomBufDataset;
begin
  OrgDs := DBConnector.GetNDataset(true,15) as TCustomBufDataset;
  OrgDs.Open;
  AUpdDatasetProc(OrgDs);
  OrgDs.ApplyUpdates;

  if Inserts then
    begin
    ChangedDs := TSQLDBConnector(DBConnector).Query;
    ChangedDs.Close;
    TSQLQuery(ChangedDS).SQL.Text:='SELECT * FROM FPDEV WHERE (ID < 16) or (ID>100) ORDER BY ID';

    OrgDs.IndexFieldNames:='ID';
    OrgDs.First;
    end
  else
    ChangedDs := DBConnector.GetNDataset(true,15) as TCustomBufDataset;
  ChangedDs.Open;
  CompareDatasets(OrgDs,ChangedDs);
end;

procedure TTestBufDatasetStreams.TestChangesCancelUpdates(
  AUpdDatasetProc: TUpdDatasetProc);
var OrgDs,
    ChangedDs : TCustomBufDataset;
begin
  OrgDs := DBConnector.GetNDataset(true,15) as TCustomBufDataset;
  OrgDs.Open;
  AUpdDatasetProc(OrgDs);
  OrgDs.CancelUpdates;

  ChangedDs := DBConnector.GetNDataset(true,15) as TCustomBufDataset;
  ChangedDs.Open;
  CompareDatasets(OrgDs,ChangedDs);
end;

procedure TTestBufDatasetStreams.TestChanges(AUpdDatasetProc: TUpdDatasetProc;
  AFormat: TDataPacketFormat);
var FileName: string;
    SaveDs,
    LoadDs : TCustomBufDataset;
begin
  case AFormat of
    dfBinary:  FileName := 'Basics.dat';
    else       FileName := 'Basics.xml';
  end;

  SaveDs := DBConnector.GetNDataset(true,15) as TCustomBufDataset;
  SaveDs.Open;
  AUpdDatasetProc(SaveDs);
  SaveDs.SaveToFile(FileName, AFormat);

  LoadDs := TCustomBufDataset.Create(nil);
  LoadDs.LoadFromFile(FileName);
  CompareDatasets(SaveDs,LoadDs);
  SaveDs.Close;

  SaveDs.Open;
  LoadDs.CancelUpdates;
  CompareDatasets(SaveDs,LoadDs);
  SaveDs.Close;

  LoadDs.Free;
  DeleteFile(FileName);
end;

procedure TTestBufDatasetStreams.TestChangesXML(AUpdDatasetProc: TUpdDatasetProc);
begin
  TestChanges(AUpdDatasetProc, dfXML);
end;


procedure TTestBufDatasetStreams.SimpleEditChange(ADataset: TCustomBufDataset);
begin
  ADataset.next;
  ADataset.edit;
  ADataset.FieldByName('name').AsString:='jojo';
  ADataset.Post;
end;

procedure TTestBufDatasetStreams.SimpleDeleteChange(ADataset: TCustomBufDataset);
begin
  ADataset.Next;
  ADataset.Delete;
end;

procedure TTestBufDatasetStreams.MoreDeletesChange(ADataset: TCustomBufDataset);
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

procedure TTestBufDatasetStreams.SimpleInsertChange(ADataset: TCustomBufDataset);
begin
  ADataset.next;
  ADataset.insert;
  ADataset.FieldByName('id').AsInteger:=500;
  ADataset.FieldByName('name').AsString:='TestName0500';
  ADataset.Post;
end;

procedure TTestBufDatasetStreams.MoreInsertsChange(ADataset: TCustomBufDataset);
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

procedure TTestBufDatasetStreams.SeveralEditsChange(ADataset: TCustomBufDataset);
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

procedure TTestBufDatasetStreams.DeleteAllChange(ADataset: TCustomBufDataset);
begin
  with ADataset do
    while not eof do delete;
end;

procedure TTestBufDatasetStreams.DeleteAllInsertChange(ADataset: TCustomBufDataset);
begin
  DeleteAllChange(ADataset);
  with ADataset do
    begin
    insert;
    fieldbyname('ID').AsInteger:=5;
    post;
    end;
end;

procedure TTestBufDatasetStreams.NullInsertChange(ADataset: TCustomBufDataset);
begin
  with ADataset do
  begin
    AssertTrue(Locate('ID',11,[]));
    Delete; //11
    Delete; //12
    Delete; //13
    Delete; //14
    Append;
    FieldByName('ID').AsInteger:=11;
    //FieldByName('NAME').Clear;
    Post;
    AppendRecord([12,'AfterNull']);
    AppendRecord([13,null]);
    AppendRecord([14,'AfterNull']);
    //Append; Post;
  end;
end;

procedure TTestBufDatasetStreams.NullEditChange(ADataset: TCustomBufDataset);
var i: integer;
begin
  //depends on procedure TTestBufDatasetStreams.NullInsertChange
  if ADataSet is TSQLQuery then
    with ADataset as TSQLQuery do
    begin
      AssertTrue(Locate('ID',11,[]));
      for i:=11 to 14 do
      begin
        Edit;
        FieldByName('NAME').AsString:='TestName'+inttostr(i);
        Post;
        Next;
      end;
      UpdateMode:=upWhereAll; //test when also null fields will be in where
    end;
end;

procedure TTestBufDatasetStreams.AppendDeleteChange(ADataset: TCustomBufDataset);
begin
  // Tests bugs #19593, #21994
  with ADataset do
  begin
    AppendRecord([16,'TestName16']);
    AppendRecord([17,'TestName17']);
    Prior;
    Prior;
    Delete;  // 15 update-buffer of deleted record is linked to 16
    Delete;  // 16 inserted-deleted and linked by 15
    AppendRecord([18,'TestName18']); // again append after delete
  end;
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

procedure TTestBufDatasetStreams.DeleteAllInsertCancelUpd;
begin
  TestChangesCancelUpdates(@DeleteAllInsertChange);
end;

procedure TTestBufDatasetStreams.AppendDeleteCancelUpd;
begin
  TestChangesCancelUpdates(@AppendDeleteChange);
end;

procedure TTestBufDatasetStreams.TestBasicsXML;
var SaveDs: TCustomBufDataset;
    LoadDs: TCustomBufDataset;
begin
  SaveDs := DBConnector.GetNDataset(true,15) as TCustomBufDataset;
  SaveDs.Open;
  SaveDs.SaveToFile('Basics.xml',dfXML);

  LoadDs := TCustomBufDataset.Create(nil);
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

procedure TTestBufDatasetStreams.TestDeleteAllInsertXML;
begin
  TestChangesXML(@DeleteAllInsertChange);
end;

procedure TTestBufDatasetStreams.TestAppendDeleteBIN;
begin
  TestChanges(@AppendDeleteChange);
end;

procedure TTestBufDatasetStreams.TestFileNameProperty;
var ds    : TDataset;
    LoadDs: TDataset;
begin
  ds := DBConnector.GetNDataset(true,5);

  ds.open;
  TCustomBufDataset(ds).FileName:='test.dat';
  ds.close;

  LoadDs := DBConnector.GetNDataset(True,2);
  TCustomBufDataset(LoadDs).FileName:='test.dat';
  LoadDs.Open;

  ds := DBConnector.GetNDataset(true,4);
  ds.Open;
  CompareDatasets(ds,LoadDs);
  ds.close;
  LoadDs.close;
end;

procedure TTestBufDatasetStreams.TestXmlFileRecognition;
var ds    : TDataset;
    LoadDs: TDataset;
begin
  ds := DBConnector.GetNDataset(true,5);

  ds.open;
  TCustomBufDataset(ds).SaveToFile('test.xml',dfXML);
  ds.close;

  LoadDs := DBConnector.GetNDataset(True,2);
  TCustomBufDataset(LoadDs).FileName:='test.xml';
  LoadDs.Open;

  ds := DBConnector.GetNDataset(true,4);
  ds.Open;
  CompareDatasets(ds,LoadDs);
  ds.close;
  LoadDs.close;
end;

procedure TTestBufDatasetStreams.TestCloseDatasetNoConnection;
var SaveDs: TCustomBufDataset;
    LoadDs: TCustomBufDataset;
    Conn: TSQLConnection;
begin
  SaveDs := DBConnector.GetNDataset(true,15) as TSQLQuery;
  SaveDs.Open;
  SaveDs.SaveToFile('Basics.xml',dfXML);
  SaveDs.Close;

  Conn := TSQLConnectionClass(TSQLDBConnector(DBConnector).Connection.ClassType).Create(nil);
  LoadDs := TSQLQuery.Create(nil);
  LoadDs.DataBase:=Conn;
  LoadDs.LoadFromFile('Basics.xml');
  LoadDs.Next;
  LoadDs.Close;
  LoadDs.Free;
  Conn.Free;
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

procedure TTestBufDatasetStreams.DeleteAllInsertApplUpd;
begin
  TestChangesApplyUpdates(@DeleteAllInsertChange, False);
end;

procedure TTestBufDatasetStreams.NullInsertUpdateApplUpd;
begin
  TestChangesApplyUpdates(@NullInsertChange, True);
  TestChangesApplyUpdates(@NullEditChange, True);
end;


procedure TTestBufDatasetStreams.SetUp;
begin
  DBConnector.StartTest;
end;

procedure TTestBufDatasetStreams.TearDown;
begin
  DBConnector.StopTest;
end;


initialization
  if uppercase(dbconnectorname)='SQL' then
    RegisterTestDecorator(TDBBasicsTestSetup, TTestBufDatasetStreams);
end.
