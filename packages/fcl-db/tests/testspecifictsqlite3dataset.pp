unit TestSpecificTSqlite3Dataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ToolsUnit, Sqlite3DS;

type

  { TTestSpecificTSqlite3Dataset }

  TTestSpecificTSqlite3Dataset = class(TDBBasicsTestCase)
  private
    function CreateMemoryDataset: TSqlite3Dataset;
  protected
  published
    procedure TestApplyUpdates;
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

