unit testspecifictdbf;

{
  Unit tests which are specific to the tdbf dbase units.
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
  ds := TDBFAutoClean.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);
  DS.CreateTable;
  DS.Open;
  CheckEquals((DS as TDBFAutoClean).UserRequestedTableLevel,DS.TableLevel,'User specified tablelevel should match dbf tablelevel.');
  DS.Close;
  ds.free;
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



initialization
{$ifdef fpc}
  if uppercase(dbconnectorname)='DBF' then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTDBF);
    end;
{$endif fpc}
end.
