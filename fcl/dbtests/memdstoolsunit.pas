unit MemDSToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  Memds;

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
  MemDs := TMemDataset.Create(nil);
  with MemDS do
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
    FieldDefs.Add('FTIME',ftTime);
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
      ShortDateFormat := 'yyyy-mm-dd';
      FieldByName('FDATE').AsDateTime := StrToDate(testDateValues[i]);
      FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
      Post;
      end;
    Close;
    end;
  Result := MemDS;
end;

initialization
  RegisterClass(TMemDSDBConnector);
end.

