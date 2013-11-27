unit TestSQLDB;

{
  Unit tests which are specific to the sqlDB components like TSQLQuery, TSQLConnection.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  db;

type

  { TSQLDBTestCase }

  TSQLDBTestCase = class(TTestCase)
    protected
      procedure SetUp; override;
      procedure TearDown; override;
  end;

  { TTestTSQLQuery }

  TTestTSQLQuery = class(TSQLDBTestCase)
  private
  published
    procedure TestMasterDetail;
    procedure TestUpdateServerIndexDefs;
  end;

  { TTestTSQLConnection }

  TTestTSQLConnection = class(TSQLDBTestCase)
  private
  published
    procedure ReplaceMe;
  end;

  { TTestTSQLScript }

  TTestTSQLScript = class(TSQLDBTestCase)
  published
    procedure TestExecuteScript;
    procedure TestScriptColon; //bug 25334
    procedure TestUseCommit; //E.g. Firebird cannot use COMMIT RETAIN if mixing DDL and DML in a script
  end;

implementation

uses sqldbtoolsunit, toolsunit, sqldb;


{ TTestTSQLQuery }

procedure TTestTSQLQuery.TestMasterDetail;
var MasterQuery, DetailQuery: TSQLQuery;
    MasterSource: TDataSource;
begin
  with TSQLDBConnector(DBConnector) do
  try
    MasterQuery := GetNDataset(10) as TSQLQuery;
    MasterSource := TDatasource.Create(nil);
    MasterSource.DataSet := MasterQuery;
    DetailQuery := Query;
    DetailQuery.SQL.Text := 'select NAME from FPDEV where ID=:ID';
    DetailQuery.DataSource := MasterSource;

    MasterQuery.Open;
    DetailQuery.Open;
    CheckEquals('TestName1', DetailQuery.Fields[0].AsString);
    MasterQuery.MoveBy(3);
    CheckEquals('TestName4', DetailQuery.Fields[0].AsString);
  finally
    MasterSource.Free;
  end;
end;

procedure TTestTSQLQuery.TestUpdateServerIndexDefs;
var Q: TSQLQuery;
    name1, name2, name3: string;
begin
  // Test retrieval of information about indexes on unquoted and quoted table names
  //  (tests also case-sensitivity for DB's that support case-sensitivity of quoted identifiers)
  // For ODBC Firebird/Interbase we must define primary key as named constraint and
  //  in ODBC driver must be set: "quoted identifiers" and "sensitive identifier"
  // See also: TTestFieldTypes.TestUpdateIndexDefs
  with TSQLDBConnector(DBConnector) do
  begin
    // SQLite ignores case-sensitivity of quoted table names
    // MS SQL Server case-sensitivity of identifiers depends on the case-sensitivity of default collation of the database
    // MySQL case-sensitivity depends on case-sensitivity of server's file system
    if SQLServerType in [ssMSSQL,ssSQLite{$IFDEF WINDOWS},ssMySQL{$ENDIF}] then
      name1 := Connection.FieldNameQuoteChars[0]+'fpdev 2'+Connection.FieldNameQuoteChars[1]
    else
      name1 := 'FPDEV2';
    ExecuteDirect('create table '+name1+' (id integer not null, constraint PK_FPDEV21 primary key(id))');
    // same but quoted table name
    name2 := Connection.FieldNameQuoteChars[0]+'FPdev2'+Connection.FieldNameQuoteChars[1];
    ExecuteDirect('create table '+name2+' (ID2 integer not null, constraint PK_FPDEV22 primary key(ID2))');
    // embedded quote in table name
    if SQLServerType in [ssMySQL] then
      name3 := '`FPdev``2`'
    else
      name3 := Connection.FieldNameQuoteChars[0]+'FPdev""2'+Connection.FieldNameQuoteChars[1];
    ExecuteDirect('create table '+name3+' (Id3 integer not null, constraint PK_FPDEV23 primary key(Id3))');
    CommitDDL;
  end;

  try
    Q := TSQLDBConnector(DBConnector).Query;
    Q.SQL.Text:='select * from '+name1;
    Q.Prepare;
    Q.ServerIndexDefs.Update;
    CheckEquals(1, Q.ServerIndexDefs.Count);

    Q.SQL.Text:='select * from '+name2;
    Q.Prepare;
    Q.ServerIndexDefs.Update;
    CheckEquals(1, Q.ServerIndexDefs.Count, '2.1');
    CheckTrue(CompareText('ID2', Q.ServerIndexDefs[0].Fields)=0, '2.2'+Q.ServerIndexDefs[0].Fields);
    CheckTrue(Q.ServerIndexDefs[0].Options=[ixPrimary,ixUnique], '2.3');

    Q.SQL.Text:='select * from '+name3;
    Q.Prepare;
    Q.ServerIndexDefs.Update;
    CheckEquals(1, Q.ServerIndexDefs.Count, '3.1');
    CheckTrue(CompareText('ID3', Q.ServerIndexDefs[0].Fields)=0, '3.2');
    CheckTrue(Q.ServerIndexDefs[0].Options=[ixPrimary,ixUnique], '3.3');
  finally
    Q.UnPrepare;
    with TSQLDBConnector(DBConnector) do
    begin
      ExecuteDirect('DROP TABLE '+name1);
      ExecuteDirect('DROP TABLE '+name2);
      ExecuteDirect('DROP TABLE '+name3);
      CommitDDL;
    end;
  end;
end;

{ TTestTSQLConnection }

procedure TTestTSQLConnection.ReplaceMe;
begin
  // replace this procedure with any test for TSQLConnection
end;

{ TTestTSQLScript }

procedure TTestTSQLScript.TestExecuteScript;
var Ascript : TSQLScript;
begin
  Ascript := TSQLScript.Create(nil);
  try
    with Ascript do
      begin
      DataBase := TSQLDBConnector(DBConnector).Connection;
      Transaction := TSQLDBConnector(DBConnector).Transaction;
      Script.Clear;
      Script.Append('create table FPDEV_A (id int);');
      Script.Append('create table FPDEV_B (id int);');
      ExecuteScript;
      // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
      TSQLDBConnector(DBConnector).CommitDDL;
      end;
  finally
    AScript.Free;
    TSQLDBConnector(DBConnector).ExecuteDirect('drop table FPDEV_A');
    TSQLDBConnector(DBConnector).ExecuteDirect('drop table FPDEV_B');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).CommitDDL;
  end;
end;

procedure TTestTSQLScript.TestScriptColon;
// Bug 25334: TSQLScript incorrectly treats : in scripts as sqldb query parameter markers
// Firebird-only test; can be extended for other dbs that use : in SQL
var
  Ascript : TSQLScript;
begin
  if not(SQLConnType in [interbase]) then Ignore(STestNotApplicable);
  Ascript := TSQLScript.Create(nil);
  try
    with Ascript do
      begin
      DataBase := TSQLDBConnector(DBConnector).Connection;
      Transaction := TSQLDBConnector(DBConnector).Transaction;
      Script.Clear;
      UseSetTerm := true;
      // Example procedure that selects table names
      Script.Append(
        'SET TERM ^ ; '+LineEnding+
        'CREATE PROCEDURE FPDEV_TESTCOLON '+LineEnding+
        'RETURNS (tblname VARCHAR(31)) '+LineEnding+
        'AS '+LineEnding+
        'begin '+LineEnding+
        '/*  Show tables. Note statement uses colon */ '+LineEnding+
        'FOR '+LineEnding+
        '  SELECT RDB$RELATION_NAME  '+LineEnding+
        '    FROM RDB$RELATIONS '+LineEnding+
        '    ORDER BY RDB$RELATION_NAME '+LineEnding+
        '    INTO :tblname '+LineEnding+
        'DO  '+LineEnding+
        '  SUSPEND; '+LineEnding+
        'end^ '+LineEnding+
        'SET TERM ; ^'
        );
      ExecuteScript;
      // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
      TSQLDBConnector(DBConnector).CommitDDL;
      end;
  finally
    AScript.Free;
    TSQLDBConnector(DBConnector).ExecuteDirect('DROP PROCEDURE FPDEV_TESTCOLON');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    TSQLDBConnector(DBConnector).CommitDDL;
  end;
end;

procedure TTestTSQLScript.TestUseCommit;
// E.g. Firebird needs explicit COMMIT sometimes, e.g. if mixing DDL and DML
// statements in a script.
// Probably same as bug 17829 Error executing SQL script
const
  TestValue='Some text';
var
  Ascript : TSQLScript;
  CheckQuery : TSQLQuery;
begin
  Ascript := TSQLScript.Create(nil);
  try
    with Ascript do
      begin
      DataBase := TSQLDBConnector(DBConnector).Connection;
      Transaction := TSQLDBConnector(DBConnector).Transaction;
      Script.Clear;
      UseCommit:=true;
      // Example procedure that selects table names
      Script.Append('CREATE TABLE fpdev_scriptusecommit (logmessage VARCHAR(255));');
      Script.Append('COMMIT;'); //needed for table to show up
      Script.Append('INSERT INTO fpdev_scriptusecommit (logmessage) VALUES('''+TestValue+''');');
      Script.Append('COMMIT;');
      ExecuteScript;
      // This line should not run, as the commit above should have taken care of it:
      //TSQLDBConnector(DBConnector).CommitDDL;
      // Test whether second line of script executed, just to be sure
      CheckQuery:=TSQLDBConnector(DBConnector).Query;
      CheckQuery.SQL.Text:='SELECT logmessage FROM fpdev_scriptusecommit ';
      CheckQuery.Open;
      CheckEquals(TestValue, CheckQuery.Fields[0].AsString, 'Insert script line should have inserted '+TestValue);
      CheckQuery.Close;
      end;
  finally
    AScript.Free;
    TSQLDBConnector(DBConnector).ExecuteDirect('DROP TABLE fpdev_scriptusecommit');
    TSQLDBConnector(DBConnector).Transaction.Commit;
  end;
end;

{ TSQLDBTestCase }

procedure TSQLDBTestCase.SetUp;
begin
  inherited SetUp;
  InitialiseDBConnector;
  DBConnector.StartTest;
end;

procedure TSQLDBTestCase.TearDown;
begin
  DBConnector.StopTest;
  if assigned(DBConnector) then
    with TSQLDBConnector(DBConnector) do
      Transaction.Rollback;
  FreeDBConnector;
  inherited TearDown;
end;


initialization
  if uppercase(dbconnectorname)='SQL' then
  begin
    RegisterTest(TTestTSQLQuery);
    RegisterTest(TTestTSQLConnection);
    RegisterTest(TTestTSQLScript);
  end;
end.
