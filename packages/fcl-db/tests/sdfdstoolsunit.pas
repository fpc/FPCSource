unit SdfDSToolsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, toolsunit,
  db,
  SdfData
  ;

type

  { TSdfDSDBConnector }

  TSdfDSDBConnector = class(TDBConnector)
    procedure SetNDatasetSchema(Schema : TStringList);
    procedure SetFieldDatasetSchema(Schema : TStringList);
  protected
    procedure CreateNDatasets; override;
    procedure CreateFieldDataset; override;
    procedure DropNDatasets; override;
    procedure DropFieldDataset; override;
    Function InternalGetNDataset(n : integer) : TDataset; override;
    Function InternalGetFieldDataset : TDataSet; override;
  end;

implementation

{ TSdfDSDBConnector }

procedure TSdfDSDBConnector.SetNDatasetSchema(Schema: TStringList);
begin
  Schema.Clear;
  Schema.Add('ID=5');
  Schema.Add('NAME=50');
end;

procedure TSdfDSDBConnector.SetFieldDatasetSchema(Schema: TStringList);
begin
  Schema.Clear;
  Schema.Add('ID=5');
  Schema.Add('FSTRING=10');
end;

procedure TSdfDSDBConnector.CreateNDatasets;
var countID,n : integer;
begin
  if dbname='' then raise Exception.Create('dbname variable not specified. You must specify name= in database.ini');
  for n := 0 to MaxDataSet do
    begin
    with TSdfDataSet.Create(nil) do
      begin
      FileName := dbname+PathDelim+'fpdev_'+inttostr(n)+'.dat';
      // Make sure the directory exists so we can write
      ForceDirectories(dbname);
      DeleteFile(FileName);
      FileMustExist:=False;
      
      SetNDatasetSchema(Schema);

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
      Close;
      Free;
      end;
    end;
end;

procedure TSdfDSDBConnector.CreateFieldDataset;
var i : integer;
begin
  if dbname='' then raise Exception.Create('dbname variable not specified. You must specify name= in database.ini');
  with TSdfDataSet.Create(nil) do
    begin
    FileName := dbname+PathDelim+'fpdev_field.dat';
    // Make sure the directory exists so we can write
    ForceDirectories(dbname);
    DeleteFile(FileName);
    FileMustExist:=False;
    
    SetFieldDatasetSchema(Schema);

    Open;
    for i := 0 to testValuesCount-1 do
      begin
      Append;
      FieldByName('ID').AsInteger := i;
      FieldByName('FSTRING').AsString := testStringValues[i];
      Post;
      end;
    Close;
    Free;
    end;
end;

procedure TSdfDSDBConnector.DropNDatasets;
var n : integer;
begin
  for n := 0 to MaxDataSet do
    DeleteFile(ExtractFilePath(dbname)+PathDelim+'fpdev_'+inttostr(n)+'.dat');
end;

procedure TSdfDSDBConnector.DropFieldDataset;
begin
  DeleteFile(ExtractFilePath(dbname)+PathDelim+'fpdev_field.dat');
end;

function TSdfDSDBConnector.InternalGetNDataset(n: integer): TDataset;
begin
  Result := TSdfDataSet.Create(nil);
  with (result as TSdfDataSet) do
    begin
    FileName := dbname+PathDelim+'fpdev_'+inttostr(n)+'.dat';
    SetNDatasetSchema(Schema);
    end;
end;

function TSdfDSDBConnector.InternalGetFieldDataset : TDataSet;
begin
  Result := TSdfDataSet.Create(nil);
  with (result as TSdfDataSet) do
    begin
    FileName := dbname+PathDelim+'fpdev_field.dat';
    SetFieldDatasetSchema(Schema);
    end;
end;

initialization
  RegisterClass(TSdfDSDBConnector);
end.

