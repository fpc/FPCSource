unit DBFToolsUnit;

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
  end;

implementation

procedure TDBFDBConnector.CreateNDatasets;
var countID,n : integer;
begin
  for n := 0 to MaxDataSet do
    begin
    with TDbf.Create(nil) do
      begin
      FilePath := dbname;
      TableName := 'fpdev_'+inttostr(n)+'.db';
      FieldDefs.Add('ID',ftInteger);
      FieldDefs.Add('NAME',ftString,50);
      CreateTable;
      Open;
      if n > 0 then for countId := 1 to n do
        begin
        Append;
        FieldByName('ID').AsInteger := countID;
        FieldByName('NAME').AsString := 'TestName'+inttostr(countID);
        end;
      if state = dsinsert then
        Post;
      Close;
      Free;
      end;
    end;
end;

procedure TDBFDBConnector.CreateFieldDataset;
var i : integer;
begin
  with TDbf.Create(nil) do
    begin
    FilePath := dbname;
    TableName := 'fpdev_field.db';
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
      ShortDateFormat := 'yyyy-mm-dd';
      FieldByName('FDATE').AsDateTime := StrToDate(testDateValues[i]);
      FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
      Post;
      end;
    Close;
    end;
end;

procedure TDBFDBConnector.DropNDatasets;
var n : integer;
begin
  for n := 0 to MaxDataSet do
    DeleteFile(ExtractFilePath(dbname)+PathDelim+'fpdev_'+inttostr(n)+'.db');
end;

procedure TDBFDBConnector.DropFieldDataset;
begin
  DeleteFile(ExtractFilePath(dbname)+PathDelim+'fpdev_field.db');
end;

function TDBFDBConnector.InternalGetNDataset(n: integer): TDataset;
begin
  Result := TDbf.Create(nil);
  with (result as TDbf) do
    begin
    FilePath := dbname;
    TableName := 'fpdev_'+inttostr(n)+'.db';
    end;
end;

function TDBFDBConnector.InternalGetFieldDataset: TDataSet;
begin
  Result := TDbf.Create(nil);
  with (result as TDbf) do
    begin
    FilePath := dbname;
    TableName := 'fpdev_field.db';
    end;
end;

initialization
  RegisterClass(TDBFDBConnector);
end.

