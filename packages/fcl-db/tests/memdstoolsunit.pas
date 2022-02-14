unit MemDSToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  MemDS;

type
{ TMemDSConnector }
  TMemDSDBConnector = class(TDBConnector)
  protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
  end;

implementation

uses
  StrUtils, FmtBCD;

{ TMemDSDBConnector }

procedure TMemDSDBConnector.CreateNDatasets;
begin
// All datasets only exist in memory, so nothing has to be done
end;

procedure TMemDSDBConnector.CreateFieldDataset;
begin
// All datasets only exist in memory, so nothing has to be done
end;

procedure TMemDSDBConnector.DropNDatasets;
begin
// All datasets only exist in memory, so nothing has to be done
end;

procedure TMemDSDBConnector.DropFieldDataset;
begin
// All datasets only exist in memory, so nothing has to be done
end;

function TMemDSDBConnector.InternalGetNDataset(n: integer): TDataset;
var MemDS  : TMemDataset;
    i      : integer;

begin
  MemDs := TMemDataset.Create(nil);
  MemDS.Name := 'NDataset';
  MemDS.FieldDefs.Add('ID',ftInteger);
  MemDS.FieldDefs.Add('NAME',ftString,50);
  MemDS.CreateTable;
  MemDS.Open;
  for i := 1 to n do
    begin
    memds.Append;
    memds.FieldByName('ID').AsInteger := i;
    memds.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    MemDS.Post;
    end;
  MemDS.Close;
  Result := MemDS;
end;

function TMemDSDBConnector.InternalGetFieldDataset : TDataSet;


var MemDS  : TMemDataset;
    i      : integer;

begin
  // Values >= 24:00:00.000 can't be handled by StrToTime function
  testTimeValues[2] := '23:59:59.000';
  testTimeValues[3] := '23:59:59.003';

  MemDS := TMemDataset.Create(nil);
  with MemDS do
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
    FieldDefs.Add('FFIXEDCHAR',ftFixedChar,10);
    FieldDefs.Add('FLARGEINT',ftLargeint);
    FieldDefs.Add('FFMTBCD',ftFmtBCD);
    FieldDefs.Add('FBLOB',ftBlob);
    FieldDefs.Add('FMEMO',ftMemo);
    FieldDefs.Add('FWIDESTRING',ftWideString);
    FieldDefs.Add('FFIXEDWIDECHAR',ftFixedWideChar);
    FieldDefs.Add('FWIDEMEMO',ftWideMemo);
    FieldDefs.Add('FLONGWORD',ftLongWord);
    FieldDefs.Add('FSHORTINT',ftShortInt);
    FieldDefs.Add('FBYTE',ftByte);
    FieldDefs.Add('FEXTENDED',ftExtended);
    FieldDefs.Add('FSINGLE',ftSingle);
    CreateTable;
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
      FieldByName('FFIXEDCHAR').AsString := PadRight(testStringValues[i], 10);
      FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
      FieldByName('FFMTBCD').AsBCD := StrToBCD(testFmtBCDValues[i], Self.FormatSettings);
      FieldByName('FBLOB').AsString := testValues[ftBlob, i];
      FieldByName('FMEMO').AsString := testValues[ftMemo, i];
      FieldByName('FWIDESTRING').AsWideString := testValues[ftWideString, i];
      FieldByName('FFIXEDWIDECHAR').AsWideString := testValues[ftFixedWideChar, i];
      FieldByName('FWIDEMEMO').AsWideString := testValues[ftWideMemo, i];
      FieldByName('FLONGWORD').AsLongWord := testLongWordValues[i];
      FieldByName('FSHORTINT').AsInteger := testShortIntValues[i];
      FieldByName('FBYTE').AsInteger := testByteValues[i];
      FieldByName('FEXTENDED').AsExtended := testFloatValues[i];
      FieldByName('FSINGLE').AsSingle := testSingleValues[i];
      Post;
      end;
    Close;
    end;
  Result := MemDS;
end;

initialization
  RegisterClass(TMemDSDBConnector);
end.

