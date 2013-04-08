unit testspecifictdbf;

{
  Unit tests which are specific to the tdbf dbase/foxpro units.
}

{$IFDEF FPC}
{$mode Delphi}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  fpcunit, testutils, testregistry, testdecorator,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  Classes, SysUtils,
  ToolsUnit, dbf;

type

  { TTestSpecificTDBF }

  TTestSpecificTDBF = class(TTestCase)
  private
    procedure WriteReadbackTest(ADBFDataset: TDbf; AutoInc: boolean = false);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Verifies that requested tablelevel is delivered:
    procedure TestTableLevel;
    // Verifies that writing to memory and writing to disk results in the same data
    procedure TestMemoryDBFEqualsDiskDBF;
    // Create fields using indexdefs:
    procedure TestCreateDatasetFromFielddefs;
    // Specifying fields from field objects
    procedure TestCreateDatasetFromFields;
    // Tries to open a dbf that has not been activated, which should fail:
    procedure TestOpenNonExistingDataset_Fails;
    // Tests creating a new database with calculated/lookup fields
    procedure TestCreationDatasetWithCalcFields;
    procedure TestAutoIncField;
    // Tests findfirst moves to first record
    procedure TestFindFirst;
    // Tests findlast moves to last record
    procedure TestFindLast;
    // Tests findnext moves to next record
    procedure TestFindNext;
    // Tests findprior
    procedure TestFindPrior;
    // Tests writing and reading a memo field
    procedure TestMemo;
    // Tests string field with 254 characters (max for DBase IV)
    procedure TestLargeString;
  end;


implementation

uses
  variants,
  FmtBCD,
  db, dbf_common, DBFToolsUnit;

{ TTestSpecificTDBF }

procedure TTestSpecificTDBF.WriteReadbackTest(ADBFDataset: TDbf;
  AutoInc: boolean);
var
  i  : integer;
begin
  // Add sample data
  for i := 1 to 10 do
    begin
    ADBFDataset.Append;
    if not AutoInc then
      ADBFDataset.FieldByName('ID').AsInteger := i;
    ADBFDataset.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    ADBFDataset.Post;
    end;
  ADBFDataset.first;
  for i := 1 to 10 do
    begin
    CheckEquals(i,ADBFDataset.fieldbyname('ID').asinteger);
    CheckEquals('TestName' + inttostr(i),ADBFDataset.fieldbyname('NAME').AsString);
    ADBFDataset.next;
    end;
  CheckTrue(ADBFDataset.EOF);
end;


procedure TTestSpecificTDBF.SetUp;
begin
  DBConnector.StartTest;
end;

procedure TTestSpecificTDBF.TearDown;
begin
  DBConnector.StopTest;
end;

procedure TTestSpecificTDBF.TestTableLevel;
var
  ds : TDBF;
begin
  if ((DS as TDBFAutoClean).UserRequestedTableLevel=25) then
    ignore('Foxpro (tablelevel 25) may write data out in dBase IV (tablelevel 4) format.');
  ds := TDBFAutoClean.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);
  DS.CreateTable;
  DS.Open;
  CheckEquals((DS as TDBFAutoClean).UserRequestedTableLevel,DS.TableLevel,'User specified tablelevel should match dbf tablelevel.');
  DS.Close;
  ds.free;
end;

procedure TTestSpecificTDBF.TestMemoryDBFEqualsDiskDBF;
var
  dsfile: TDBF;
  dsmem: TDBF;
  backingstream: TMemoryStream;
  FileName: string;
  i: integer;
  thefile: TMemoryStream;
begin
  backingstream:=TMemoryStream.Create;
  thefile:=TMemoryStream.Create;
  dsmem:=TDBF.Create(nil);
  dsfile:=TDBF.Create(nil);
  FileName:=GetTempFileName;
  dsfile.FilePathFull:=ExtractFilePath(FileName);
  dsfile.TableName:=ExtractFileName(FileName);
  dsmem.TableName:=ExtractFileName(FileName);
  dsmem.Storage:=stoMemory;
  dsmem.UserStream:=backingstream;

  // A small number of fields but should be enough
  dsfile.FieldDefs.Add('ID',ftInteger);
  dsmem.FieldDefs.Add('ID',ftInteger);
  dsfile.FieldDefs.Add('NAME',ftString,50);
  dsmem.FieldDefs.Add('NAME',ftString,50);
  dsfile.CreateTable;
  dsmem.CreateTable;
  dsfile.Open;
  dsmem.Open;
  // Some sample data
  for i := 1 to 101 do
  begin
    dsfile.Append;
    dsmem.Append;
    dsfile.FieldByName('ID').AsInteger := i;
    dsmem.FieldByName('ID').AsInteger := i;
    dsfile.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    dsmem.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    dsfile.Post;
    dsmem.Post;
  end;

  // By closing, we update the number of records in the header
  dsfile.close;
  dsmem.close;
  dsfile.free;

  // Keep dsmem; load file into stream:
  thefile.LoadfromFile(FileName);
  deletefile(FileName);

  CheckEquals(backingstream.size,thefile.size,'Memory backed dbf should have same size as file-backed dbf');
  // Now compare stream contents - thereby comparing the file with backingstream
  CheckEquals(true,comparemem(thefile.Memory,backingstream.Memory,thefile.size),'Memory backed dbf data should be the same as file-backed dbf');
  backingstream.free;
  thefile.free;
end;

procedure TTestSpecificTDBF.TestCreateDatasetFromFielddefs;
var
  ds : TDBF;
begin
  ds := TDBFAutoClean.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);
  DS.FieldDefs.Add('NAME',ftString,50);
  DS.CreateTable;
  DS.Open;
  WriteReadbackTest(ds);
  DS.Close;
  ds.free;
end;

procedure TTestSpecificTDBF.TestCreateDatasetFromFields;
var
  ds : TDBF;
  f: TField;
begin
  ds := TDBFAutoClean.Create(nil);
  DS.CreateTable;
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;
  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;

  DS.Open;
  ds.free;
end;

procedure TTestSpecificTDBF.TestOpenNonExistingDataset_Fails;
var
  ds : TDBF;
  f: TField;
begin
  ds := TDBFAutoClean.Create(nil);
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;

  CheckException(ds.Open,EDbfError);
  ds.Free;

  ds := TDBFAutoClean.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);

  CheckException(ds.Open,EDbfError);
  ds.Free;
end;

procedure TTestSpecificTDBF.TestCreationDatasetWithCalcFields;
var
  ds : TDBF;
  f: TField;
  i: integer;
begin
  //todo: find out which tablelevels support calculated/lookup fields
  ds := TDBFAutoClean.Create(nil);
  try
    ds.FieldDefs.Add('ID',ftInteger);
    ds.FieldDefs.Add('NAME',ftString,50);
    ds.CreateTable;
    for i:=0 to ds.FieldDefs.Count-1 do
    begin
      ds.FieldDefs[i].CreateField(ds); // make fields persistent
    end;

    F := TStringField.Create(ds);
    F.FieldKind:=fkCalculated;
    F.FieldName:='NAME_CALC';
    F.DataSet:=ds;
    F.Size:=50;
    F.ProviderFlags:=[];

    F := TStringField.Create(ds);
    F.FieldKind:=fkLookup;
    F.FieldName:='NAME_LKP';
    F.LookupDataSet:=DBConnector.GetNDataset(5);
    F.KeyFields:='ID';
    F.LookupKeyFields:='ID';
    F.LookupResultField:='NAME';
    F.DataSet:=ds;
    F.Size:=50;

    DS.Open;
    WriteReadbackTest(ds);

    for i := 0 to ds.FieldDefs.Count-1 do
      begin
      CheckNotEquals(ds.FieldDefs[i].Name,'NAME_CALC');
      CheckNotEquals(ds.FieldDefs[i].Name,'NAME_LKP');
      end;
    DS.Close;
  finally
    ds.Free;
  end;
end;

procedure TTestSpecificTDBF.TestAutoIncField;
var
  ds : TDbf;
  f: TField;
begin
  ds := TDbfAutoClean.Create(nil);
  if ds.TableLevel<7 then
  begin
    Ignore('Autoinc fields are only supported in tablelevel 7 and higher');
  end;

  F := TAutoIncField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;

  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;

  DS.CreateTable;
  DS.Open;

  WriteReadbackTest(ds,True);
  DS.Close;
  ds.Free;
end;

procedure TTestSpecificTDBF.TestFindFirst;
const
  NumRecs=8;
var
  DS: TDataSet;
begin
  DS:=DBConnector.GetNDataset(NumRecs);
  DS.Open;
  DS.Last;
  CheckEquals(true,DS.FindFirst,'Findfirst should return true');
  CheckEquals(1,DS.fieldbyname('ID').asinteger);
end;

procedure TTestSpecificTDBF.TestFindLast;
const
  NumRecs=8;
var
  DS: TDataSet;
begin
  DS:=DBConnector.GetNDataset(NumRecs);
  DS.Open;
  DS.First;
  CheckEquals(true,DS.FindLast,'Findlast should return true');
  CheckEquals(NumRecs,DS.fieldbyname('ID').asinteger);
end;

procedure TTestSpecificTDBF.TestFindNext;
const
  NumRecs=8;
var
  DS: TDataSet;
begin
  DS:=DBConnector.GetNDataset(NumRecs);
  DS.Open;
  DS.First;
  CheckEquals(true,DS.FindNext,'FindNext should return true');
  CheckEquals(2,DS.fieldbyname('ID').asinteger);
end;

procedure TTestSpecificTDBF.TestFindPrior;
const
  NumRecs=8;
var
  DS: TDataSet;
begin
  DS:=DBConnector.GetNDataset(NumRecs);
  DS.Open;
  DS.Last;
  CheckEquals(true,DS.FindPrior,'FindPrior should return true');
  CheckEquals(NumRecs-1,DS.fieldbyname('ID').asinteger);
end;

procedure TTestSpecificTDBF.TestMemo;
var
  ds : TDBF;
begin
  ds := TDBFAutoClean.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);
  DS.FieldDefs.Add('NAME',ftMemo);
  DS.CreateTable;
  DS.Open;
  WriteReadbackTest(ds);
  DS.Close;
  ds.free;
end;

procedure TTestSpecificTDBF.TestLargeString;
var
  ds : TDBF;
  MaxStringSize: integer;
  TestValue: string;
begin
  ds := TDBFAutoClean.Create(nil);
  if (ds.TableLevel>=25) then
    // (Visual) FoxPro supports 32K
    MaxStringSize:=32767
  else
    // Dbase III..V,7
    MaxStringSize:=254;
  TestValue:=StringOfChar('a',MaxStringSize);

  DS.FieldDefs.Add('ID',ftInteger);
  DS.FieldDefs.Add('NAME',ftString,254);
  DS.CreateTable;
  DS.Open;

  // Write & readback test
  DS.Append;
  DS.FieldByName('ID').AsInteger := 1;
  DS.FieldByName('NAME').AsString := TestValue;
  DS.Post;

  DS.first;
  CheckEquals(1,DS.fieldbyname('ID').asinteger,'ID field must match record number');
  // If test fails, let's count the number of "a"s instead so we can report that instead of printing out the entire string
  CheckEquals(length(TestValue),length(DS.fieldbyname('NAME').AsString),'NAME field length must match test value length');
  CheckEquals(TestValue,DS.fieldbyname('NAME').AsString,'NAME field must match test value');
  DS.next;
  CheckTrue(DS.EOF,'Dataset EOF must be true');

  DS.Close;
  ds.free;
end;



initialization
{$ifdef fpc}
  if uppercase(dbconnectorname)='DBF' then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTDBF);
    end;
{$endif fpc}
end.
