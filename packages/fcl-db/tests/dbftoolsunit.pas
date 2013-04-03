unit DBFToolsUnit;

{ Sets up dbf datasets for testing
Tests expect Get*Dataset to return a dataset with structure and test data, but closed.
Because of this, we use file-backed dbfs instead of memory backed dbfs
}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

// If defined, do not delete the dbf files when done but print out location to stdout:
{.$DEFINE KEEPDBFFILES}

interface

uses
  Classes, SysUtils, toolsunit,
  DB, Dbf, dbf_common;

type
  { TDBFDBConnector }

  TDBFDBConnector = class(TDBConnector)
  protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    // InternalGetNDataset reroutes to ReallyInternalGetNDataset
    function InternalGetNDataset(n: integer): TDataset; override;
    function InternalGetFieldDataset: TDataSet; override;
    // GetNDataset allowing trace dataset if required;
    // if trace is on, use a TDbfTraceDataset instead of TDBFAutoClean
    function ReallyInternalGetNDataset(n: integer; Trace: boolean): TDataset;
  public
    function GetTraceDataset(AChange: boolean): TDataset; override;
  end;

  { TDBFAutoClean }
  // DBF descendant that saves to a temp file and removes file when closed
  TDBFAutoClean = class(TDBF)
  private
    FBackingStream: TMemoryStream;
    FCreatedBy: string;
  public
    // Keeps track of which function created the dataset, useful for troubleshooting
    property CreatedBy: string read FCreatedBy write FCreatedBy;
    constructor Create;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function UserRequestedTableLevel: integer;
  end;

  { TDbfTraceDataset }
  TDbfTraceDataset = class(TdbfAutoClean)
  protected
    procedure SetCurrentRecord(Index: longint); override;
    procedure RefreshInternalCalcFields(Buffer: PChar); override;
    procedure InternalInitFieldDefs; override;
    procedure CalculateFields(Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
  end;


implementation

uses
  FmtBCD;

{ TDBFAutoClean }

function TDBFAutoClean.UserRequestedTableLevel: integer;
  // User can specify table level as a connector param, e.g.:
  // connectorparams=4
  // If none given, default to DBase IV
var
  TableLevelProvided: integer;
begin
  TableLevelProvided := StrToIntDef(dbconnectorparams, 4);
  if not (TableLevelProvided in [3, 4, 5, 7, 
    TDBF_TABLELEVEL_FOXPRO, TDBF_TABLELEVEL_VISUALFOXPRO]) then
  begin
    Result := -1; // hope this crashes the tests so user is alerted.
    //Invalid tablelevel specified in connectorparams= field. Aborting
    exit;
  end;
  Result := TableLevelProvided;
end;

constructor TDBFAutoClean.Create;
begin
  FBackingStream:=TMemoryStream.Create;
  // Create a unique name:
  TableName := FormatDateTime('hhnnssz',Now())+'/'+inttostr(random(32767));
  TableLevel := UserRequestedTableLevel;
  Storage:=stoMemory;
  UserStream:=FBackingStream;
  CreateTable; //write out header to disk
end;

constructor TDBFAutoClean.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Create;
end;

destructor TDBFAutoClean.Destroy;
{$IFDEF KEEPDBFFILES}
var
  FileName: string;
{$ENDIF}
begin
  {$IFDEF KEEPDBFFILES}
  Close;
  FileName := GetTempFileName;
  FBackingStream.SaveToFile(FileName);
  writeln('TDBFAutoClean: file created by ',CreatedBy,' left file: ',FileName);
  {$ENDIF}
  inherited Destroy;
  FBackingStream.Free;
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
begin
  result:=ReallyInternalGetNDataset(n,false);
end;

function TDBFDBConnector.InternalGetFieldDataset: TDataSet;
var
  i: integer;
begin
  Result := (TDbfAutoClean.Create(nil) as TDataSet);
  with (Result as TDBFAutoClean) do
  begin
    CreatedBy:='InternalGetFieldDataset';
    FieldDefs.Add('ID', ftInteger);
    FieldDefs.Add('FSTRING', ftString, 10);
    FieldDefs.Add('FSMALLINT', ftSmallint);
    FieldDefs.Add('FINTEGER', ftInteger);
    FieldDefs.Add('FWORD', ftWord);
    FieldDefs.Add('FBOOLEAN', ftBoolean);
    FieldDefs.Add('FFLOAT', ftFloat);
    if (Result as TDBF).TableLevel >= 25 then
      FieldDefs.Add('FCURRENCY', ftCurrency);
    if (Result as TDBF).TableLevel >= 25 then
      FieldDefs.Add('FBCD', ftBCD);
    FieldDefs.Add('FDATE', ftDate);
    //    FieldDefs.Add('FTIME',ftTime);
    FieldDefs.Add('FDATETIME', ftDateTime);
    FieldDefs.Add('FLARGEINT', ftLargeint);
    FieldDefs.Add('FMEMO', ftMemo);
    CreateTable;
    Open;
    for i := 0 to testValuesCount - 1 do
    begin
      Append;
      FieldByName('ID').AsInteger := i;
      FieldByName('FSTRING').AsString := testStringValues[i];
      FieldByName('FSMALLINT').AsInteger := testSmallIntValues[i];
      FieldByName('FINTEGER').AsInteger := testIntValues[i];
      FieldByName('FBOOLEAN').AsBoolean := testBooleanValues[i];
      FieldByName('FFLOAT').AsFloat := testFloatValues[i];
      if (Result as TDBF).TableLevel >= 25 then
        FieldByName('FCURRENCY').AsCurrency := testCurrencyValues[i];
      if (Result as TDBF).TableLevel >= 25 then
        FieldByName('FBCD').AsBCD := StrToBCD(testFmtBCDValues[i], Self.FormatSettings);
      FieldByName('FDATE').AsDateTime := StrToDate(testDateValues[i], 'yyyy/mm/dd', '-');
      FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
      Post;
    end;
    Close;
  end;
end;

function TDBFDBConnector.ReallyInternalGetNDataset(n: integer; Trace: boolean): TDataset;
var
  countID: integer;
begin
  if Trace then
    Result := (TDbfTraceDataset.Create(nil) as TDataSet)
  else
    Result := (TDBFAutoClean.Create(nil) as TDataSet);
  with (Result as TDBFAutoclean) do
  begin
    CreatedBy:='InternalGetNDataset('+inttostr(n)+')';
    FieldDefs.Add('ID', ftInteger);
    FieldDefs.Add('NAME', ftString, 50);
    CreateTable;
    Open;
    if n > 0 then
      for countId := 1 to n do
      begin
        Append;
        FieldByName('ID').AsInteger := countID;
        FieldByName('NAME').AsString := 'TestName' + IntToStr(countID);
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

function TDBFDBConnector.GetTraceDataset(AChange: boolean): TDataset;
var
  ADS: TDataSet;
begin
  // Mimic TDBConnector.GetNDataset
  if AChange then FChangedDatasets[NForTraceDataset] := True;
  Result := ReallyInternalGetNDataset(NForTraceDataset,true);
  FUsedDatasets.Add(Result);
end;

{ TDbfTraceDataset }

procedure TDbfTraceDataset.SetCurrentRecord(Index: longint);
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
var
  i: integer;
  IntCalcFieldName: string;
begin
  // To fake an internal calculated field, set its fielddef InternalCalcField
  // property to true, before the dataset is opened.
  // This procedure takes care of setting the automatically created fielddef's
  // InternalCalcField property to true. (works for only one field)
  IntCalcFieldName := '';
  for i := 0 to FieldDefs.Count - 1 do
    if fielddefs[i].InternalCalcField then
      IntCalcFieldName := FieldDefs[i].Name;
  inherited InternalInitFieldDefs;
  if IntCalcFieldName <> '' then
    with FieldDefs.find(IntCalcFieldName) do
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
