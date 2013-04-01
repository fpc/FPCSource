unit DBFToolsUnit;

{ Sets up dbf datasets for testing
Tests expect Get*Dataset to return a dataset with structure and test data, but closed.
Because of this, we use file-backed dbfs instead of memory backed dbfs
}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, toolsunit,
  db, Dbf;

type
{ TDBFDBConnector }

  TDBFDBConnector = class(TDBConnector)
  protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
  public
    function GetTraceDataset(AChange : Boolean) : TDataset; override;
  end;

  { TDbfTraceDataset }

  TDbfTraceDataset = class(Tdbf)
  protected
    procedure SetCurrentRecord(Index: Longint); override;
    procedure RefreshInternalCalcFields(Buffer: PChar); override;
    procedure InternalInitFieldDefs; override;
    procedure CalculateFields(Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
  end;

  { TDBFAutoClean }
  // DBF descendant that saves to a temp file and removes file when closed
  TDBFAutoClean=class(TDBF)
  public
    constructor Create;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation



{ TDBFAutoClean }

constructor TDBFAutoClean.Create;
var
  DBFFileName: string;
  TableLevelProvided: integer;
begin
  DBFFileName:=GetTempFileName;
  FilePathFull:=ExtractFilePath(DBFFileName);
  TableName := ExtractFileName(DBFFileName);
  // User can specify table level as a connector param, e.g.:
  // connectorparams=4
  // If none given, default to DBase IV
  TableLevelProvided:=StrToIntDef(dbconnectorparams,4);
  if not ((TableLevelProvided = 3) or (TableLevelProvided = 4) or (TableLevelProvided = 7) or (TableLevelProvided = 25)) then
  begin
    writeln('Invalid tablelevel specified in connectorparams= field. Aborting');
    exit;
  end;
  TableLevel := TableLevelProvided;
  CreateTable; //write out header to disk
end;

constructor TDBFAutoClean.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Create;
end;

destructor TDBFAutoClean.Destroy;
var
  FileName: string;
begin
  FileName:=AbsolutePath+TableName;
  inherited Destroy;
  deletefile(FileName);
end;


procedure TDBFDBConnector.CreateNDatasets;
begin
  // All datasets are created in InternalGet*Dataset
end;

procedure TDBFDBConnector.CreateFieldDataset;
begin
  // All datasets are created in InternalGet*Dataset
end;

procedure TDBFDBConnector.DropNDatasets;
begin
  // Nothing to be done here; the dataset is cleaned up in TDBFAutoClean.Destroy
end;

procedure TDBFDBConnector.DropFieldDataset;
begin
  // Nothing to be done here; the dataset is cleaned up in TDBFAutoClean.Destroy
end;

function TDBFDBConnector.InternalGetNDataset(n: integer): TDataset;
var
  countID: integer;
begin
  result:=(TDBFAutoClean.Create(nil) as TDataSet);
  with (result as TDBFAutoclean) do
    begin
    FieldDefs.Add('ID',ftInteger);
    FieldDefs.Add('NAME',ftString,50);
    CreateTable;
    Open;
    if n > 0 then for countId := 1 to n do
      begin
      Append;
      FieldByName('ID').AsInteger := countID;
      FieldByName('NAME').AsString := 'TestName'+inttostr(countID);
      // Explicitly call .post, since there could be a bug which disturbs
      // the automatic call to post. (example: when TDataset.DataEvent doesn't
      // work properly)
      Post;
      end;
    if state = dsinsert then
      Post;
    Close;
    end;
end;

function TDBFDBConnector.InternalGetFieldDataset: TDataSet;
var
  i : integer;
begin
  result:=(TDbfAutoClean.Create(nil) as TDataSet);
  with (result as TDBFAutoClean) do
    begin
    FieldDefs.Add('ID',ftInteger);
    FieldDefs.Add('FSTRING',ftString,10);
    FieldDefs.Add('FSMALLINT',ftSmallint);
    FieldDefs.Add('FINTEGER',ftInteger);
//    FieldDefs.Add('FWORD',ftWord);
    FieldDefs.Add('FBOOLEAN',ftBoolean);
    FieldDefs.Add('FFLOAT',ftFloat);
//    FieldDefs.Add('FCURRENCY',ftCurrency);
//    FieldDefs.Add('FBCD',ftBCD);
    FieldDefs.Add('FDATE',ftDate);
//    FieldDefs.Add('FTIME',ftTime);
    FieldDefs.Add('FDATETIME',ftDateTime);
    FieldDefs.Add('FLARGEINT',ftLargeint);
    CreateTable;
    Open;
    for i := 0 to testValuesCount-1 do
      begin
      Append;
      FieldByName('ID').AsInteger := i;
      FieldByName('FSTRING').AsString := testStringValues[i];
      FieldByName('FSMALLINT').AsInteger := testSmallIntValues[i];
      FieldByName('FINTEGER').AsInteger := testIntValues[i];
      FieldByName('FBOOLEAN').AsBoolean := testBooleanValues[i];
      FieldByName('FFLOAT').AsFloat := testFloatValues[i];
      FieldByName('FDATE').AsDateTime := StrToDate(testDateValues[i], 'yyyy/mm/dd', '-');
      FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
      Post;
      end;
    Close;
    end;
end;

function TDBFDBConnector.GetTraceDataset(AChange: Boolean): TDataset;
var ADS, AResDS : TDbf;
begin
  ADS := GetNDataset(AChange,15) as TDbf;
  AResDS := TDbfTraceDataset.Create(nil);
  AResDS.FilePath:=ADS.FilePath;
  AResDs.TableName:=ADS.TableName;
  Result:=AResDS;
end;

{ TDbfTraceDataset }

procedure TDbfTraceDataset.SetCurrentRecord(Index: Longint);
begin
  DataEvents := DataEvents + 'SetCurrentRecord' + ';';
  inherited SetCurrentRecord(Index);
end;

procedure TDbfTraceDataset.RefreshInternalCalcFields(Buffer: PChar);
begin
  DataEvents := DataEvents + 'RefreshInternalCalcFields' + ';';
  inherited RefreshInternalCalcFields(Buffer);
end;

procedure TDbfTraceDataset.InternalInitFieldDefs;
var i : integer;
    IntCalcFieldName : String;
begin
  // To fake an internal calculated field, set its fielddef InternalCalcField
  // property to true, before the dataset is opened.
  // This procedure takes care of setting the automatically created fielddef's
  // InternalCalcField property to true. (works for only one field)
  IntCalcFieldName:='';
  for i := 0 to FieldDefs.Count -1 do
    if fielddefs[i].InternalCalcField then IntCalcFieldName := FieldDefs[i].Name;
  inherited InternalInitFieldDefs;
  if IntCalcFieldName<>'' then with FieldDefs.find(IntCalcFieldName) do
    begin
    InternalCalcField := True;
    end;
end;

procedure TDbfTraceDataset.CalculateFields(Buffer: PChar);
begin
  DataEvents := DataEvents + 'CalculateFields' + ';';
  inherited CalculateFields(Buffer);
end;

procedure TDbfTraceDataset.ClearCalcFields(Buffer: PChar);
begin
  DataEvents := DataEvents + 'ClearCalcFields' + ';';
  inherited ClearCalcFields(Buffer);
end;

initialization
  RegisterClass(TDBFDBConnector);
end.

