unit TestSpecificTBufDataset;

{
  Unit tests which are specific to stand-alone TBufDataset-datasets. (So not
  for derived datasets like TQuery )
}

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  fpcunit, testutils, testregistry, testdecorator, BufDataset,
{$ELSE FPC}
  TestFramework,
{$ENDIF FPC}
  Classes, SysUtils, db, ToolsUnit;

type

  { TTestSpecificTBufDataset }

  TTestSpecificTBufDataset = class(TTestCase)
  private
    procedure TestDataset(ABufDataset: TBufDataset; AutoInc: boolean = false);
    function GetAutoIncDataset: TBufDataset;
    procedure IntTestAutoIncFieldStreaming(XML: boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreateDatasetFromFielddefs;
    procedure CreateDatasetFromFields;
    procedure TestOpeningNonExistingDataset;
    procedure TestCreationDatasetWithCalcFields;
    procedure TestAutoIncField;
    procedure TestAutoIncFieldStreaming;
    procedure TestAutoIncFieldStreamingXML;
  end;

implementation

uses
{$ifdef fpc}
//
{$endif fpc}
  variants,
  strutils,
  FmtBCD;

{ TTestSpecificTBufDataset }

procedure TTestSpecificTBufDataset.TestDataset(ABufDataset: TBufDataset;
  AutoInc: boolean);
var
  i  : integer;
begin
  for i := 1 to 10 do
    begin
    ABufDataset.Append;
    if not AutoInc then
      ABufDataset.FieldByName('ID').AsInteger := i;
    ABufDataset.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    ABufDataset.Post;
    end;
  ABufDataset.first;
  for i := 1 to 10 do
    begin
    CheckEquals(i,ABufDataset.fieldbyname('ID').asinteger);
    CheckEquals('TestName' + inttostr(i),ABufDataset.fieldbyname('NAME').AsString);
    ABufDataset.next;
    end;
  CheckTrue(ABufDataset.EOF);
end;

function TTestSpecificTBufDataset.GetAutoIncDataset: TBufDataset;
var
  ds : TBufDataset;
  f: TField;
begin
  ds := TBufDataset.Create(nil);
  F := TAutoIncField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;
  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;
  DS.CreateDataset;

  TestDataset(ds,True);
  result := ds;
end;

procedure TTestSpecificTBufDataset.IntTestAutoIncFieldStreaming(XML: boolean);
var
  ds : TBufDataset;
  fn: string;
begin
  ds := GetAutoIncDataset;
  fn := GetTempFileName;
  if xml then
    ds.SaveToFile(fn,dfXML)
  else
    ds.SaveToFile(fn);
  DS.Close;
  ds.Free;

  ds := TBufDataset.Create(nil);
  ds.LoadFromFile(fn);
  ds.Last;
  CheckEquals(10,ds.FieldByName('Id').AsInteger);
  ds.Append;
  ds.FieldByName('NAME').asstring := 'Test';
  ds.Post;
  CheckEquals(11,ds.FieldByName('Id').AsInteger);
  ds.Free;

  DeleteFile(fn);
end;

procedure TTestSpecificTBufDataset.SetUp;
begin
  DBConnector.StartTest(TestName);
end;

procedure TTestSpecificTBufDataset.TearDown;
begin
  DBConnector.StopTest(TestName);
end;

procedure TTestSpecificTBufDataset.CreateDatasetFromFielddefs;
var ds : TBufDataset;
begin
  ds := TBufDataset.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);
  DS.FieldDefs.Add('NAME',ftString,50);
  DS.CreateDataset;
  TestDataset(ds);
  DS.Close;
  DS.CreateDataset;
  TestDataset(ds);
end;

procedure TTestSpecificTBufDataset.CreateDatasetFromFields;
var ds : TBufDataset;
    f: TField;
begin
  ds := TBufDataset.Create(nil);
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;
  F := TStringField.Create(ds);
  F.FieldName:='NAME';
  F.DataSet:=ds;
  F.Size:=50;
  DS.CreateDataset;
  TestDataset(ds);
  DS.Close;
  DS.CreateDataset;
  TestDataset(ds);
end;

procedure TTestSpecificTBufDataset.TestOpeningNonExistingDataset;
var ds : TBufDataset;
    f: TField;
begin
  ds := TBufDataset.Create(nil);
  F := TIntegerField.Create(ds);
  F.FieldName:='ID';
  F.DataSet:=ds;

  CheckException(ds.Open,EDatabaseError);
  ds.Free;

  ds := TBufDataset.Create(nil);
  DS.FieldDefs.Add('ID',ftInteger);

  CheckException(ds.Open,EDatabaseError);
  ds.Free;
end;

procedure TTestSpecificTBufDataset.TestCreationDatasetWithCalcFields;
var ds : TBufDataset;
    f: TField;
    i: integer;
begin
  ds := TBufDataset.Create(nil);
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

    DS.CreateDataset;

    TestDataset(ds);

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

procedure TTestSpecificTBufDataset.TestAutoIncField;
var
  ds : TBufDataset;
begin
  ds := GetAutoIncDataset;
  DS.Close;
  ds.Free;
end;

procedure TTestSpecificTBufDataset.TestAutoIncFieldStreaming;
begin
  IntTestAutoIncFieldStreaming(false);
end;

procedure TTestSpecificTBufDataset.TestAutoIncFieldStreamingXML;
begin
  IntTestAutoIncFieldStreaming(true);
end;

initialization
{$ifdef fpc}

  if uppercase(dbconnectorname)='BUFDATASET' then
    begin
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTBufDataset);
    end;
{$endif fpc}
end.
