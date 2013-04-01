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
  db, dbf, dbf_common, ToolsUnit, DBFToolsUnit;

type

  { TTestSpecificTDBF }

  TTestSpecificTDBF = class(TTestCase)
  private
    function GetTableLevel: integer;
    procedure WriteReadbackTest(ADBFDataset: TDbf; AutoInc: boolean = false);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Create fields the old fashioned way:
    procedure CreateDatasetFromFielddefs;
    // Specifying fields from field objects
    procedure CreateDatasetFromFields;
    // Tries to open a dbf that has not been activated, which should fail:
    procedure OpenNonExistingDataset_Fails;
    procedure TestCreationDatasetWithCalcFields;
    procedure TestAutoIncField;
  end;


implementation

uses
  variants,
  FmtBCD;

{ TTestSpecificTDBF }

function TTestSpecificTDBF.GetTableLevel: integer;
var
  TableLevelProvided: integer;
begin
  TableLevelProvided:=StrToIntDef(dbconnectorparams,4);
  if not ((TableLevelProvided = 3) or (TableLevelProvided = 4) or (TableLevelProvided = 7) or (TableLevelProvided = 25)) then
  begin
    writeln('Invalid tablelevel specified in connectorparams= field. Aborting');
    exit;
  end;
  result := TableLevelProvided;
end;

procedure TTestSpecificTDBF.WriteReadbackTest(ADBFDataset: TDbf;
  AutoInc: boolean);
var
  i  : integer;
begin
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

procedure TTestSpecificTDBF.CreateDatasetFromFielddefs;
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

procedure TTestSpecificTDBF.CreateDatasetFromFields;
var
  ds : TDBF;
  f: TField;
begin
  ds := TDBFAutoClean.Create(nil);
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;
  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;
  DS.CreateTable;
  DS.Open;
  ds.free;
end;

procedure TTestSpecificTDBF.OpenNonExistingDataset_Fails;
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
    F := TIntegerField.Create(ds);
    F.FieldName:='ID';
    F.DataSet:=ds;

    F := TStringField.Create(ds);
    F.FieldName:='NAME';
    F.DataSet:=ds;
    F.Size:=50;

    F := TStringField.Create(ds);
    F.FieldKind:=fkCalculated;
    F.FieldName:='NAME_CALC';
    F.DataSet:=ds;
    F.Size:=50;

    F := TStringField.Create(ds);
    F.FieldKind:=fkLookup;
    F.FieldName:='NAME_LKP';
    F.LookupDataSet:=DBConnector.GetNDataset(5);
    F.KeyFields:='ID';
    F.LookupKeyFields:='ID';
    F.LookupResultField:='NAME';
    F.DataSet:=ds;
    F.Size:=50;

    DS.CreateTable;
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



initialization
{$ifdef fpc}

  if uppercase(dbconnectorname)='DBF' then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTDBF);
    end;
{$endif fpc}
end.
