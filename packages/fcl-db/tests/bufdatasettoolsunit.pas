unit BufDatasetToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  BufDataset;

type
{ TbufdatasetConnector }
  TbufdatasetDBConnector = class(TDBConnector)
  protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
  end;

implementation

{ TbufdatasetDBConnector }

procedure TbufdatasetDBConnector.CreateNDatasets;
begin
// All datasets only exist in memory, so nothing has to be done
end;

procedure TbufdatasetDBConnector.CreateFieldDataset;
begin
// All datasets only exist in memory, so nothing has to be done
end;

procedure TbufdatasetDBConnector.DropNDatasets;
begin
// All datasets only exist in memory, so nothing has to be done
end;

procedure TbufdatasetDBConnector.DropFieldDataset;
begin
// All datasets only exist in memory, so nothing has to be done
end;

function TbufdatasetDBConnector.InternalGetNDataset(n: integer): TDataset;
var BufDataset  : TBufDataset;
    i      : integer;

begin
  BufDataset := TBufDataset.Create(nil);
  BufDataset.FieldDefs.Add('ID',ftInteger);
  BufDataset.FieldDefs.Add('NAME',ftString,50);
  BufDataset.CreateDataset;
  BufDataset.Open;
  for i := 1 to n do
    begin
    BufDataset.Append;
    BufDataset.FieldByName('ID').AsInteger := i;
    BufDataset.FieldByName('NAME').AsString := 'TestName' + inttostr(i);
    BufDataset.Post;
    end;
  BufDataset.Close;
  Result := BufDataset;
end;

function TbufdatasetDBConnector.InternalGetFieldDataset : TDataSet;


var BufDataset  : TBufDataset;
    i      : integer;

begin
  BufDataset := TBufDataset.Create(nil);
  with BufDataset do
    begin
    //todo: this is based on memds.
    //check and add bufdataset supported fields
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
    CreateDataset;
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
  Result := BufDataset;
end;

initialization
  RegisterClass(TbufdatasetDBConnector);
end.

