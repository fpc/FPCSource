unit Sqlite3DSToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit
  ,db, Sqlite3DS
  ;


const
  STestNotApplicable = 'This test does not apply to this sqlite3ds connection type';


type
  { TSqlite3DSDBConnector }

  TSqlite3DSDBConnector = class(TDBConnector)
  private
    FDataset: TSqlite3Dataset;
    Function CreateDataset: TSqlite3Dataset;
  protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
  public
    procedure TryDropIfExist(const ATableName : String);
    destructor Destroy; override;
    constructor Create; override;
    procedure ExecuteDirect(const SQL: string);
  end;


implementation

{ TSqlite3DSDBConnector }

function TSqlite3DSDBConnector.CreateDataset: TSqlite3Dataset;

begin
  Result := TSqlite3Dataset.create(nil);
  Result.FileName := dbname;
end;

procedure TSqlite3DSDBConnector.CreateNDatasets;
var CountID : Integer;
begin
  try
    TryDropIfExist('FPDEV');
    FDataset.ExecSQL('create table FPDEV (' +
                              '  ID INT NOT NULL,  ' +
                              '  NAME VARCHAR(50), ' +
                              '  PRIMARY KEY (ID)  ' +
                              ')');
    FDataset.ExecSQL('BEGIN;');
    for countID := 1 to MaxDataSet do
      FDataset.ExecSQL('insert into FPDEV (ID,NAME) ' +
                                'values ('+inttostr(countID)+',''TestName'+inttostr(countID)+''')');
    FDataset.ExecSQL('COMMIT;');
  except
    on E: Exception do begin
      if dblogfilename<>'' then
        LogMessage('Custom','Exception running CreateNDatasets: '+E.Message);
      FDataset.ExecSQL('ROLLBACK;');
    end;
  end;
end;

procedure TSqlite3DSDBConnector.CreateFieldDataset;
var
  FieldDataset: TSqlite3Dataset;
  i: Integer;

begin
  FieldDataset := CreateDataset;
  try
    TryDropIfExist('FPDEV_FIELD');
    with FieldDataset do
    begin
       TableName := 'FPDEV_FIELD';
       PrimaryKey := 'ID';
       FieldDefs.Add('ID', ftInteger);
       FieldDefs.Add('FSTRING', ftString, 10);
       //FieldDefs.Add('FSMALLINT', ftSmallint);
       FieldDefs.Add('FINTEGER', ftInteger);
       FieldDefs.Add('FWORD', ftWord);
       FieldDefs.Add('FBOOLEAN', ftBoolean);
       FieldDefs.Add('FFLOAT', ftFloat);
       FieldDefs.Add('FCURRENCY', ftCurrency);
       //FieldDefs.Add('FBCD', ftBCD);
       FieldDefs.Add('FDATE', ftDate);
       FieldDefs.Add('FDATETIME', ftDateTime);
       FieldDefs.Add('FLARGEINT', ftLargeint);
       FieldDefs.Add('FMEMO', ftMemo);
       if not CreateTable then
         raise Exception.Create('Error in CreateTable: ' + FieldDataset.ReturnString);
       Open;
       for i := 0 to testValuesCount - 1 do
       begin
         Append;
         FieldByName('ID').AsInteger := i;
         FieldByName('FSTRING').AsString := testStringValues[i];
         //FieldByName('FSMALLINT').AsInteger := testSmallIntValues[i];
         FieldByName('FINTEGER').AsInteger := testIntValues[i];
         FieldByName('FWORD').AsInteger := testWordValues[i];
         FieldByName('FBOOLEAN').AsBoolean := testBooleanValues[i];
         FieldByName('FFLOAT').AsFloat := testFloatValues[i];
         FieldByName('FCURRENCY').AsCurrency := testCurrencyValues[i];
         // work around missing TBCDField.AsBCD:
         //  FieldByName('FBCD').AsBCD := StrToBCD(testFmtBCDValues[i],Self.FormatSettings);
         FieldByName('FDATE').AsDateTime := StrToDate(testDateValues[i], 'yyyy/mm/dd', '-');
         FieldByName('FDATETIME').AsDateTime := StrToDateTime(testValues[ftDateTime,i], Self.FormatSettings);
         FieldByName('FLARGEINT').AsLargeInt := testLargeIntValues[i];
         FieldByName('FMEMO').AsString := testStringValues[i];
         Post;
       end;
       if not ApplyUpdates then
         raise Exception.Create('Error in ApplyUpdates: ' + FieldDataset.ReturnString);
       Destroy;
     end;
  except
    on E: Exception do begin
      if dblogfilename<>'' then
        LogMessage('Custom','Exception running CreateFieldDataset: '+E.Message);
      FDataset.ExecSQL('ROLLBACK;');
    end;
  end;
end;

procedure TSqlite3DSDBConnector.DropNDatasets;
begin
  try
    FDataset.ExecSQL('DROP TABLE FPDEV');
  Except
    on E: Exception do begin
      if dblogfilename<>'' then
        LogMessage('Custom','Exception running DropNDatasets: '+E.Message);
      FDataset.ExecSQL('ROLLBACK;');
    end;
  end;
end;

procedure TSqlite3DSDBConnector.DropFieldDataset;
begin
  try
    FDataset.ExecSQL('DROP TABLE FPDEV_FIELD');
  Except
    on E: Exception do begin
      if dblogfilename<>'' then
        LogMessage('Custom','Exception running DropFieldDataset: '+E.Message);
      FDataset.ExecSQL('ROLLBACK;');
    end;
  end;
end;

function TSqlite3DSDBConnector.InternalGetNDataset(n: integer): TDataset;
begin
  Result := CreateDataset;
  with (Result as TSqlite3Dataset) do
    begin
    sql := 'SELECT * FROM FPDEV WHERE ID < '+inttostr(n+1)+' ORDER BY ID';
    end;
end;

function TSqlite3DSDBConnector.InternalGetFieldDataset: TDataSet;
begin
  Result := CreateDataset;
  with (Result as TSqlite3Dataset) do
    begin
    sql := 'SELECT * FROM FPDEV_FIELD';
    end;
end;

procedure TSqlite3DSDBConnector.TryDropIfExist(const ATableName: String);
begin
  FDataset.ExecSQL('drop table if exists ' + ATableName);
end;

procedure TSqlite3DSDBConnector.ExecuteDirect(const SQL: string);
begin
  FDataset.ExecSQL(SQL);
end;

destructor TSqlite3DSDBConnector.Destroy;
begin
  inherited Destroy;
  FDataset.Destroy;
end;

constructor TSqlite3DSDBConnector.Create;
begin
  FDataset := CreateDataset;
  Inherited;
end;

initialization
  RegisterClass(TSqlite3DSDBConnector);
end.