unit TestSQLFieldTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  { TTestFieldTypes }

  TTestFieldTypes= class(TTestCase)
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
    procedure RunTest; override;
  published
    procedure TestInt;
  end; 

implementation

uses db,sqldbtoolsunit,toolsunit;

procedure TTestFieldTypes.TestInt;

const
  testValuesCount = 17;
  testValues : Array[0..testValuesCount-1] of integer = (-maxInt,-maxSmallint-1,-maxSmallint,-256,-255,-128,-127,-1,0,1,127,128,255,256,maxSmallint,maxSmallint+1,MaxInt);
 
var
  i : byte;

begin
  TSQLDBConnector(DBConnector).Connection.ExecuteDirect('create table FPDEV2 (ID INT NOT NULL)');

// Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
  TSQLDBConnector(DBConnector).Transaction.CommitRetaining;

  with TSQLDBConnector(DBConnector).Query do
    begin
    SQL.Clear;
    SQL.Add('select * from FPDEV2');

    Open;

    AssertEquals(1,FieldCount);
    AssertEquals('ID',fields[0].FieldName);
    AssertEquals('ID',fields[0].DisplayName);
    AssertEquals(4,fields[0].DataSize);
    AssertTrue(ftInteger=fields[0].DataType);

    Close;

    for i := 0 to testValuesCount-1 do
      TSQLDBConnector(DBConnector).Connection.ExecuteDirect('insert into FPDEV2 (id) values (' + inttostr(testValues[i]) + ')');

    Open;

    for i := 0 to testValuesCount-1 do
      begin
      AssertEquals(testValues[i],fields[0].Value);
      Next;
      end;
    end;
  TSQLDBConnector(DBConnector).Transaction.Rollback;
end;

procedure TTestFieldTypes.SetUp; 
begin
  InitialiseDBConnector;
end;

procedure TTestFieldTypes.TearDown; 
begin
  FreeAndNil(DBConnector);
end;

procedure TTestFieldTypes.RunTest;
begin
  if dbtype = 'interbase' then
    inherited RunTest;
end;

initialization

  RegisterTest(TTestFieldTypes); 
end.

