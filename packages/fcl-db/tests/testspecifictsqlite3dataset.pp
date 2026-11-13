unit TestSpecificTSqlite3Dataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, ToolsUnit, Sqlite3DS;

type

  { TTestSpecificTSqlite3Dataset }

  TTestSpecificTSqlite3Dataset = class(TDBBasicsTestCase)
  private
    function CreateMemoryDataset: TSqlite3Dataset;
    procedure CheckBlobBytes(const aMsg: String; aField: TField; const aExpected: AnsiString);
  protected
  published
    procedure TestApplyUpdates;
    // Binary data in a BLOB column survives insert, edit and reopen
    procedure TestBinaryBlob;
  end;

implementation

uses
  testregistry;


{ TTestSpecificTSqlite3Dataset }

function TTestSpecificTSqlite3Dataset.CreateMemoryDataset: TSqlite3Dataset;
begin
  Result := TSqlite3Dataset.Create(nil);
  Result.FileName := ':memory:';
  Result.TableName := 'Test';
  Result.PrimaryKey := 'Id';
  Result.ExecSQL('Create Table Test (Id INTEGER PRIMARY KEY, Name VARCHAR);');
end;

procedure TTestSpecificTSqlite3Dataset.CheckBlobBytes(const aMsg: String; aField: TField; const aExpected: AnsiString);

var
  lStream: TBytesStream;

begin
  lStream := TBytesStream.Create;
  try
    TBlobField(aField).SaveToStream(lStream);
    CheckEquals(Length(aExpected), lStream.Size, aMsg + ': blob size');
    CheckTrue(CompareMem(lStream.Memory, PAnsiChar(aExpected), Length(aExpected)), aMsg + ': blob bytes');
  finally
    lStream.Free;
  end;
end;


procedure TTestSpecificTSqlite3Dataset.TestBinaryBlob;

var
  lDataset: TSqlite3Dataset;
  lData: AnsiString;
  lStream: TMemoryStream;
  i: Integer;

begin
  SetLength(lData, 257);
  lData[1] := #0;
  for i := 0 to 255 do
    lData[i + 2] := AnsiChar(i);
  lDataset := TSqlite3Dataset.Create(nil);
  try
    lDataset.FileName := ':memory:';
    lDataset.TableName := 'BlobTest';
    lDataset.PrimaryKey := 'Id';
    lDataset.FieldDefs.Add('Id', ftInteger);
    lDataset.FieldDefs.Add('Name', ftString, 10);
    lDataset.FieldDefs.Add('Data', ftBlob);
    CheckTrue(lDataset.CreateTable, 'CreateTable with a ftBlob field');
    lDataset.Open;
    CheckEquals(FieldTypeNames[ftBlob], FieldTypeNames[lDataset.FieldByName('Data').DataType], 'BLOB column is a ftBlob field');
    lStream := TMemoryStream.Create;
    try
      lStream.WriteBuffer(lData[1], Length(lData));
      lStream.Position := 0;
      lDataset.Append;
      lDataset.FieldByName('Id').AsInteger := 1;
      lDataset.FieldByName('Name').AsString := 'One';
      TBlobField(lDataset.FieldByName('Data')).LoadFromStream(lStream);
      lDataset.Post;
    finally
      lStream.Free;
    end;
    lDataset.AppendRecord([2, 'Two']);
    lDataset.First;
    CheckBlobBytes('After post', lDataset.FieldByName('Data'), lData);
    lDataset.ApplyUpdates;
    lDataset.Close;
    lDataset.Open;
    CheckEquals(2, lDataset.RecordCount, 'Record count after apply updates');
    CheckBlobBytes('After reopen', lDataset.FieldByName('Data'), lData);
    lDataset.Edit;
    lDataset.FieldByName('Name').AsString := 'Uno';
    lDataset.Post;
    CheckBlobBytes('After editing another field', lDataset.FieldByName('Data'), lData);
    lDataset.ApplyUpdates;
    lDataset.Close;
    lDataset.Open;
    CheckEquals('Uno', lDataset.FieldByName('Name').AsString, 'Edited field after reopen');
    CheckBlobBytes('After edit and reopen', lDataset.FieldByName('Data'), lData);
    lDataset.Next;
    CheckTrue(lDataset.FieldByName('Data').IsNull, 'Unassigned blob is NULL after reopen');
  finally
    lDataset.Free;
  end;
end;


procedure TTestSpecificTSqlite3Dataset.TestApplyUpdates;
var
  Dataset: TSqlite3Dataset;
  ExceptionRaised: Boolean;
begin
  Dataset := CreateMemoryDataset;
  try
    // insert
    Dataset.Open;
    CheckEquals(0, Dataset.RecordCount, 'Dataset should be empty');
    Dataset.Append;
    Dataset.FieldByName('Name').AsString := 'Luiz';
    Dataset.Post;
    Dataset.ApplyUpdates;
    Dataset.Close;
    Dataset.Open;
    CheckEquals(1, Dataset.RecordCount, 'Record count should increase after apply insert updates');
    CheckEquals('Luiz', Dataset.FieldByName('Name').AsString, 'Record field should be set');
    // update
    Dataset.Edit;
    Dataset.FieldByName('Name').AsString := 'New';
    Dataset.Post;
    Dataset.ApplyUpdates;
    Dataset.Close;
    Dataset.Open;
    CheckEquals(1, Dataset.RecordCount, 'Record count should not change after apply update updates');
    CheckEquals('New', Dataset.FieldByName('Name').AsString, 'Record field should be updated');
    //delete
    Dataset.Delete;
    Dataset.ApplyUpdates;
    Dataset.Close;
    Dataset.Open;
    CheckEquals(0, Dataset.RecordCount, 'Record count should decrease after apply delete updates');
  finally
    Dataset.Destroy;
  end;

  Dataset := CreateMemoryDataset;
  try
    Dataset.Open;
    CheckEquals(0, Dataset.RecordCount, 'Dataset should be empty');
    Dataset.Append;
    Dataset.FieldByName('Name').AsString := 'Luiz';
    Dataset.Post;
    Dataset.TableName := 'NonExistentTable';
    ExceptionRaised := False;
    try
      Dataset.ApplyUpdates;
    except
      ExceptionRaised := True;
    end;
    CheckTrue(ExceptionRaised, 'An exception should be raised if TableName is invalid');
  finally
    Dataset.Destroy;
  end;

  Dataset := CreateMemoryDataset;
  try
    Dataset.Open;
    Dataset.Append;
    Dataset.FieldByName('Name').AsString := 'Luiz';
    Dataset.Post;
    Dataset.ApplyUpdates;

    Dataset.Close;
    Dataset.PrimaryKey := '';
    Dataset.Open;
    Dataset.Edit;
    Dataset.FieldByName('Name').AsString := 'New';
    Dataset.Post;
    ExceptionRaised := False;
    try
      Dataset.ApplyUpdates;
    except
      ExceptionRaised := True;
    end;
    CheckTrue(ExceptionRaised, 'An exception should be raised if PrimaryKey is empty');

    Dataset.Close;
    Dataset.PrimaryKey := 'NonExistingKey';
    Dataset.Open;
    Dataset.Edit;
    Dataset.FieldByName('Name').AsString := 'New';
    Dataset.Post;
    ExceptionRaised := False;
    try
      Dataset.ApplyUpdates;
    except
      ExceptionRaised := True;
    end;
    CheckTrue(ExceptionRaised, 'An exception should be raised if PrimaryKey is invalid');
  finally
    Dataset.Destroy;
  end;
end;

initialization

  if UpperCase(dbconnectorname) = 'SQLITE3DS' then
    RegisterTestDecorator(TDBBasicsTestSetup, TTestSpecificTSqlite3Dataset);

end.

