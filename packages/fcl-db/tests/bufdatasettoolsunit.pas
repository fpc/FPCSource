unit BufDatasetToolsUnit;

{ Sets up bufdataset for testing.
Tests expect Get*Dataset to return a dataset with structure and test data, but closed.
A closed BufDataset normally has no data, so these tests won't work.

To circumvent this, this unit saves the dataset contents to file and reloads them on opening
using the BufDataset persistence mechanism.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  BufDataset;

type

  { TbufdatasetDBConnector }

  TbufdatasetDBConnector = class(TDBConnector)
  private
    FUniDirectional: boolean;
   protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
    procedure SetTestUniDirectional(const AValue: boolean); override;
    function GetTestUniDirectional: boolean; override;
  end;

implementation

uses
  StrUtils, FmtBCD;

type

  { TPersistentBufDataSet }

  TPersistentBufDataSet=class(TBufDataset)
    private
      TempFileName:string;
    protected
      procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;
    public
      destructor Destroy; override;
  end;

{ TPersistentBufDataSet }

procedure TPersistentBufDataSet.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
  Raise ENotImplemented.Create('LoadBlobIntoBuffer not implemented');
end;

destructor TPersistentBufDataSet.Destroy;
begin
  Close; // no locks on TempFileName
  DeleteFile(TempFileName);
  inherited Destroy;
end;

{ TbufdatasetDBConnector }

procedure TbufdatasetDBConnector.CreateNDatasets;
begin
  // All datasets are created in InternalGet*Dataset
end;

procedure TbufdatasetDBConnector.CreateFieldDataset;
begin
  // All datasets are created in InternalGet*Dataset
end;

procedure TbufdatasetDBConnector.DropNDatasets;
begin
  // All datasets are created in InternalGet*Dataset and cleaned up when destroyed
end;

procedure TbufdatasetDBConnector.DropFieldDataset;
begin
  // All datasets are created in InternalGet*Dataset and cleaned up when destroyed
end;

function TbufdatasetDBConnector.InternalGetNDataset(n: integer): TDataset;
var
  BufDataset : TPersistentBufDataSet;
  i : integer;
begin
  BufDataset := TPersistentBufDataSet.Create(nil);
  with BufDataset do
    begin
    Name := 'NDataset';
    FieldDefs.Add('ID',ftInteger);
    FieldDefs.Add('NAME',ftString,50);
    CreateDataset;
    Open;
    for i := 1 to n do
      begin
      Append;
      FieldByName('ID').AsInteger := i;
      FieldByName('NAME').AsString := 'TestName' + inttostr(i);
      Post;
      end;
    MergeChangeLog;
    TempFileName:=GetTempFileName;
    FileName:=TempFileName;
    Close; // Save data into file
    end;
  Result := BufDataset;
end;

function TbufdatasetDBConnector.InternalGetFieldDataset : TDataSet;


var BufDataset  : TPersistentBufDataSet;
    i      : integer;

begin
  // Values >= 24:00:00.000 can't be handled by StrToTime function
  testTimeValues[2] := '23:59:59.000';
  testTimeValues[3] := '23:59:59.003';

  BufDataset := TPersistentBufDataSet.Create(nil);
  with BufDataset do
    begin
    Name := 'FieldDataset';
    FieldDefs.Add('ID',ftInteger);
    FieldDefs.Add('FSTRING',ftString,10);
    FieldDefs.Add('FSMALLINT',ftSmallint);
    FieldDefs.Add('FINTEGER',ftInteger);
    FieldDefs.Add('FWORD',ftWord);
    FieldDefs.Add('FBOOLEAN',ftBoolean);
    FieldDefs.Add('FFLOAT',ftFloat);
    FieldDefs.Add('FCURRENCY',ftCurrency);
    FieldDefs.Add('FBCD',ftBCD);
    FieldDefs.Add('FDATE',ftDate);
    FieldDefs.Add('FTIME',ftTime);
    FieldDefs.Add('FDATETIME',ftDateTime);
    FieldDefs.Add('FVARBYTES',ftVarBytes,10);
    FieldDefs.Add('FBLOB',ftBlob);
    FieldDefs.Add('FMEMO',ftMemo);
    FieldDefs.Add('FFIXEDCHAR',ftFixedChar,10);
    FieldDefs.Add('FLARGEINT',ftLargeint);
    FieldDefs.Add('FVARIANT',ftVariant);
    FieldDefs.Add('FGUID',ftGuid,38);
    FieldDefs.Add('FFMTBCD',ftFmtBCD);
    FieldDefs.Add('FWIDESTRING',ftWideString,10);
    FieldDefs.Add('FFIXEDWIDECHAR',ftFixedWideChar,10);
    FieldDefs.Add('FWIDEMEMO',ftWideMemo);
    FieldDefs.Add('FLONGWORD',ftLongWord);
    FieldDefs.Add('FSHORTINT',ftShortInt);
    FieldDefs.Add('FBYTE',ftByte);
    FieldDefs.Add('FEXTENDED',ftExtended);
    FieldDefs.Add('FSINGLE',ftSingle);
    CreateDataset;
    Open;
    for i := 0 to testValuesCount-1 do
    begin
      Append;
      FieldByName('ID').AsInteger := i;
      FieldByName('FSTRING').AsString := testStringValues[i];
      FieldByName('FSMALLINT').AsInteger := testSmallIntValues[i];
      FieldByName('FINTEGER').AsInteger := testIntValues[i];
      FieldByName('FWORD').AsInteger := testWordValues[i];
      FieldByName('FBOOLEAN').AsBoolean := testBooleanValues[i];
      FieldByName('FFLOAT').AsFloat := testFloatValues[i];
      FieldByName('FCURRENCY').AsCurrency := testCurrencyValues[i];
      FieldByName('FBCD').AsCurrency := testCurrencyValues[i];
      FieldByName('FDATE').AsDateTime := StrToDateTime(testDateValues[i], Self.FormatSettings);
      FieldByName('FTIME').AsDateTime := StrToTime(testTimeValues[i], Self.FormatSettings);
      FieldByName('FDATETIME').AsDateTime := StrToDateTime(testValues[ftDateTime,i], Self.FormatSettings);
      FieldByName('FVARBYTES').AsString := testStringValues[i];
      FieldByName('FBLOB').AsString := testStringValues[i];
      FieldByName('FMEMO').AsString := testStringValues[i];
      FieldByName('FFIXEDCHAR').AsString := PadRight(testStringValues[i], 10);
      FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
      FieldByName('FVARIANT').AsString := testStringValues[i];
      FieldByName('FGUID').AsString := GuidToString(GUID_NULL);
      FieldByName('FFMTBCD').AsBCD := StrToBCD(testFmtBCDValues[i], Self.FormatSettings);
      FieldByName('FWIDESTRING').AsString := testStringValues[i];
      FieldByName('FFIXEDWIDECHAR').AsString := PadRight(testStringValues[i], 10);
      FieldByName('FWIDEMEMO').AsString := testStringValues[i];
      FieldByName('FLONGWORD').AsLongWord := testLongWordValues[i];
      FieldByName('FSHORTINT').AsInteger := testShortIntValues[i];
      FieldByName('FBYTE').AsInteger := testByteValues[i];
      FieldByName('FEXTENDED').AsExtended := testFloatValues[i];
      FieldByName('FSINGLE').AsSingle := testSingleValues[i];
      Post;
    end;
    MergeChangeLog;
    TempFileName:=GetTempFileName;
    FileName:=TempFileName;
    Close; // Save data into file
    // When data are loaded from file, bidirectional is checked
    // so unidirectional bufdataset can't be tested here
    UniDirectional := TestUniDirectional;
    end;
  Result := BufDataset;
end;

procedure TbufdatasetDBConnector.SetTestUniDirectional(const AValue: boolean);
begin
  FUniDirectional := AValue;
end;

function TbufdatasetDBConnector.GetTestUniDirectional: boolean;
begin
  Result := FUniDirectional;
end;

initialization
  RegisterClass(TbufdatasetDBConnector);
end.

