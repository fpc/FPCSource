unit TestSQLDB;

{
  Unit tests which are specific to the sqlDB components like TSQLQuery, TSQLConnection.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, sqldb, SysUtils, fpcunit, testregistry,
  sqldbtoolsunit,toolsunit, db;

type

  { TSQLDBTestCase }

  TSQLDBTestCase = class(TTestCase)
  private
    function GetSQLDBConnector: TSQLDBConnector;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
      Property SQLDBConnector : TSQLDBConnector Read GetSQLDBConnector;
  end;

  { TTestTSQLQuery }

  TTestTSQLQuery = class(TSQLDBTestCase)
  private
    FMyQ: TSQLQuery;
    procedure DoAfterPost(DataSet: TDataSet);
    Procedure DoApplyUpdates;
    Procedure SetQueryOptions;
    Procedure TrySetPacketRecords;
  Protected
    Procedure Setup; override;
  published
    procedure TestMasterDetail;
    procedure TestUpdateServerIndexDefs;
    Procedure TestKeepOpenOnCommit;
    Procedure TestKeepOpenOnCommitPacketRecords;
    Procedure TestCheckSettingsOnlyWhenInactive;
    Procedure TestAutoApplyUpdatesPost;
    Procedure TestAutoApplyUpdatesDelete;
    Procedure TestCheckRowsAffected;
    Procedure TestAutoCommit;
    Procedure TestRefreshSQL;
    Procedure TestGeneratedRefreshSQL;
    Procedure TestGeneratedRefreshSQL1Field;
    Procedure TestGeneratedRefreshSQLNoKey;
    Procedure TestRefreshSQLMultipleRecords;
    Procedure TestRefreshSQLNoRecords;
    Procedure TestFetchAutoInc;
  end;

  { TTestTSQLConnection }

  TTestTSQLConnection = class(TSQLDBTestCase)
  private
    procedure SetImplicit;
    procedure TestImplicitTransaction;
    procedure TestImplicitTransaction2;
    procedure TestImplicitTransactionNotAssignable;
    procedure TestImplicitTransactionOK;
    procedure TryOpen;
  published
    procedure TestUseImplicitTransaction;
    procedure TestUseExplicitTransaction;
    procedure TestExplicitConnect;
  end;

  { TTestTSQLScript }

  TTestTSQLScript = class(TSQLDBTestCase)
  published
    procedure TestExecuteScript;
    procedure TestScriptColon; //bug 25334
    procedure TestUseCommit; //E.g. Firebird cannot use COMMIT RETAIN if mixing DDL and DML in a script
  end;

implementation



{ TTestTSQLQuery }

procedure TTestTSQLQuery.TestMasterDetail;
var MasterQuery, DetailQuery: TSQLQuery;
    MasterSource: TDataSource;
begin
  with SQLDBConnector do
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
  with SQLDBConnector do
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
    Q := SQLDBConnector.Query;
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
    with SQLDBConnector do
    begin
      ExecuteDirect('DROP TABLE '+name1);
      ExecuteDirect('DROP TABLE '+name2);
      ExecuteDirect('DROP TABLE '+name3);
      CommitDDL;
    end;
  end;
end;

Procedure TTestTSQLQuery.TestKeepOpenOnCommit;
var Q: TSQLQuery;
    I: Integer;
begin
  // Test that for a SQL query with Options=sqoKeepOpenOnCommit, calling commit does not close the dataset.
  // Test also that an edit still works.
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    Transaction.Commit;
    for I:=1 to 20 do
      ExecuteDirect(Format('INSERT INTO testdiscon values (%d,''%.6d'')',[i,i]));
    Transaction.Commit;
    Q := SQLDBConnector.Query;
    Q.SQL.Text:='select * from testdiscon';
    Q.Options:=[sqoKeepOpenOnCommit];
    AssertEquals('PacketRecords forced to -1',-1,Q.PacketRecords);
    Q.Open;
    AssertEquals('Got all records',20,Q.RecordCount);
    Q.SQLTransaction.Commit;
    AssertTrue('Still open after transaction',Q.Active);
    // Now check editing
    Q.Locate('id',20,[]);
    Q.Edit;
    Q.FieldByName('a').AsString:='abc';
    Q.Post;
    AssertTrue('Have updates pending',Q.UpdateStatus=usModified);
    Q.ApplyUpdates;
    AssertTrue('Have no more updates pending',Q.UpdateStatus=usUnmodified);
    Q.Close;
    Q.SQL.Text:='select * from testdiscon where (id=20) and (a=''abc'')';
    Q.Open;
    AssertTrue('Have modified data record in database', not (Q.EOF AND Q.BOF));
    end;
end;

Procedure TTestTSQLQuery.TrySetPacketRecords;
begin
  FMyQ.PacketRecords:=10;
end;

Procedure TTestTSQLQuery.Setup;
begin
  inherited Setup;
  SQLDBConnector.Connection.Options:=[];
end;

Procedure TTestTSQLQuery.TestKeepOpenOnCommitPacketRecords;
begin
  with SQLDBConnector do
    begin
    FMyQ := SQLDBConnector.Query;
    FMyQ.Options:=[sqoKeepOpenOnCommit];
    AssertException('Cannot set PacketRecords when sqoKeepOpenOnCommit is active',EDatabaseError,@TrySetPacketRecords);
    end;
end;

Procedure TTestTSQLQuery.SetQueryOptions;
begin
  FMyQ.Options:=[sqoKeepOpenOnCommit];
end;

Procedure TTestTSQLQuery.TestCheckSettingsOnlyWhenInactive;
begin
  // Check that we can only set QueryOptions when the query is inactive.
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    Transaction.COmmit;
     ExecuteDirect(Format('INSERT INTO testdiscon values (%d,''%.6d'')',[1,1]));
    Transaction.COmmit;
    FMyQ := SQLDBConnector.Query;
    FMyQ.SQL.Text:='select * from testdiscon';
    FMyQ := SQLDBConnector.Query;
    FMyQ.OPen;
    AssertException('Cannot set packetrecords when sqoDisconnected is active',EDatabaseError,@SetQueryOptions);
    end;
end;

procedure TTestTSQLQuery.DoAfterPost(DataSet: TDataSet);
begin
  AssertTrue('Have modifications in after post',FMyq.UpdateStatus=usModified)
end;

Procedure TTestTSQLQuery.TestAutoApplyUpdatesPost;
var Q: TSQLQuery;
    I: Integer;
begin
  // Test that if sqoAutoApplyUpdates is in QueryOptions, then POST automatically does an ApplyUpdates
  // Test also that POST afterpost event is backwards compatible.
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    Transaction.COmmit;
    for I:=1 to 2 do
      ExecuteDirect(Format('INSERT INTO testdiscon values (%d,''%.6d'')',[i,i]));
    Transaction.COmmit;
    Q := SQLDBConnector.Query;
    FMyQ:=Q; // so th event handler can reach it.
    Q.SQL.Text:='select * from testdiscon';
    Q.Options:=[sqoAutoApplyUpdates];
    // We must test that in AfterPost, the modification is still there, for backwards compatibilty
    Q.AfterPost:=@DoAfterPost;
    Q.Open;
    AssertEquals('Got all records',2,Q.RecordCount);
    // Now check editing
    Q.Locate('id',2,[]);
    Q.Edit;
    Q.FieldByName('a').AsString:='abc';
    Q.Post;
    AssertTrue('Have no more updates pending',Q.UpdateStatus=usUnmodified);
    Q.Close;
    Q.SQL.Text:='select * from testdiscon where (id=2) and (a=''abc'')';
    Q.Open;
    AssertTrue('Have modified data record in database',not (Q.EOF AND Q.BOF));
    end;

end;

Procedure TTestTSQLQuery.TestAutoApplyUpdatesDelete;

var Q: TSQLQuery;
    I: Integer;
begin
  // Test that if sqoAutoApplyUpdates is in QueryOptions, then Delete automatically does an ApplyUpdates
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    Transaction.COmmit;
    for I:=1 to 2 do
      ExecuteDirect(Format('INSERT INTO testdiscon values (%d,''%.6d'')',[i,i]));
    Transaction.COmmit;
    Q := SQLDBConnector.Query;
    FMyQ:=Q; // so th event handler can reach it.
    Q.SQL.Text:='select * from testdiscon';
    Q.Options:=[sqoAutoApplyUpdates];
    // We must test that in AfterPost, the modification is still there, for backwards compatibilty
    Q.AfterPost:=@DoAfterPost;
    Q.Open;
    AssertEquals('Got all records',2,Q.RecordCount);
    // Now check editing
    Q.Locate('id',2,[]);
    Q.Delete;
    AssertTrue('Have no more updates pending',Q.UpdateStatus=usUnmodified);
    Q.Close;
    Q.SQL.Text:='select * from testdiscon where (id=2)';
    Q.Open;
    AssertTrue('Data record is deleted in database', (Q.EOF AND Q.BOF));
    end;
end;

Procedure TTestTSQLQuery.DoApplyUpdates;

begin
  FMyQ.ApplyUpdates();
end;

Procedure TTestTSQLQuery.TestCheckRowsAffected;
var Q: TSQLQuery;
    I: Integer;
begin
  // Test that if sqoAutoApplyUpdates is in QueryOptions, then Delete automatically does an ApplyUpdates
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    Transaction.COmmit;
    for I:=1 to 2 do
      ExecuteDirect(Format('INSERT INTO testdiscon values (%d,''%.6d'')',[i,i]));
    Transaction.COmmit;
    SQLDBConnector.Connection.Options:=[scoApplyUpdatesChecksRowsAffected];
    Q := SQLDBConnector.Query;
    Q.SQL.Text:='select * from testdiscon';
    Q.DeleteSQL.Text:='delete from testdiscon';
    Q.Open;
    AssertEquals('Got all records',2,Q.RecordCount);
    // Now check editing
    Q.Delete;
    FMyQ:=Q;
    AssertException('Rowsaffected > 1 raises exception',EUpdateError,@DoApplyUpdates);
    end;
end;

Procedure TTestTSQLQuery.TestAutoCommit;
var
  I : Integer;
begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    if Transaction.Active then
      Transaction.Commit;

    Query.Options:=[sqoAutoCommit];
    for I:=1 to 2 do
      begin
      Query.SQL.Text:=Format('INSERT INTO testdiscon values (%d,''%.6d'');',[i,i]);
      Query.Prepare;
      Query.ExecSQL;
      // We do not commit anything explicitly.
      end;

    AssertFalse('Transaction is still active after expected auto commit', Transaction.Active);

    Connection.Close;
    Connection.Open;

    Query.SQL.Text:='SELECT COUNT(*) from testdiscon';
    Query.Open;
    AssertEquals('Records haven''t been committed to database', 2, Query.Fields[0].AsInteger);
    end;
end;

Procedure TTestTSQLQuery.TestRefreshSQL;
var
  Q: TSQLQuery;

begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdefval');
    ExecuteDirect('create table testdefval (id integer not null, a varchar(10) default ''abcde'', constraint pk_testdefval primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    end;
  Q:=SQLDBConnector.Query;
  Q.SQL.Text:='select * from testdefval';
  Q.InsertSQL.Text:='insert into testdefval (id) values (:id)';
  Q.RefreshSQL.Text:='SELECT a FROM testdefval WHERE (id=:id)';
  Q.Open;
  Q.Insert;
  Q.FieldByName('id').AsInteger:=1;
  Q.Post;
  AssertTrue('Field value has not been fetched after post',Q.FieldByName('a').IsNull);
  Q.ApplyUpdates(0);
  AssertEquals('Still on correct field',1,Q.FieldByName('id').AsInteger);
  AssertEquals('Field value has been fetched from the database ','abcde',Q.FieldByName('a').AsString);
end;

Procedure TTestTSQLQuery.TestGeneratedRefreshSQL;

var
  Q: TSQLQuery;

begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdefval');
    ExecuteDirect('create table testdefval (id integer not null, a varchar(10) default ''abcde'', b varchar(5) default ''fgh'', constraint pk_testdefval primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    end;
  Q:=SQLDBConnector.Query;
  Q.SQL.Text:='select * from testdefval';
  Q.InsertSQL.Text:='insert into testdefval (id) values (:id)';
  Q.Open;
  With Q.FieldByName('id') do
    ProviderFlags:=ProviderFlags+[pfInKey];
  With Q.FieldByName('a') do
    ProviderFlags:=ProviderFlags+[pfRefreshOnInsert,pfRefreshOnUpdate];
  With Q.FieldByName('b') do
    ProviderFlags:=ProviderFlags+[pfRefreshOnInsert,pfRefreshOnUpdate];
  Q.Insert;
  Q.FieldByName('id').AsInteger:=1;
  Q.Post;
  AssertTrue('Field value has not been fetched after post',Q.FieldByName('a').IsNull);
  Q.ApplyUpdates(0);
  AssertEquals('Still on correct field',1,Q.FieldByName('id').AsInteger);
  AssertEquals('Field value has been fetched from the database ','abcde',Q.FieldByName('a').AsString);
  AssertEquals('Field value has been fetched from the database ','fgh',Q.FieldByName('b').AsString);
end;

Procedure TTestTSQLQuery.TestGeneratedRefreshSQL1Field;
var
  Q: TSQLQuery;

begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdefval');
    ExecuteDirect('create table testdefval (id integer not null, a varchar(10) default ''abcde'', b varchar(5) default ''fgh'', constraint pk_testdefval primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    end;
  Q:=SQLDBConnector.Query;
  Q.SQL.Text:='select * from testdefval';
  Q.InsertSQL.Text:='insert into testdefval (id) values (:id)';
  Q.Open;
  With Q.FieldByName('id') do
    ProviderFlags:=ProviderFlags+[pfInKey];
  With Q.FieldByName('a') do
    ProviderFlags:=ProviderFlags+[pfRefreshOnInsert,pfRefreshOnUpdate];
  Q.Insert;
  Q.FieldByName('id').AsInteger:=1;
  Q.Post;
  AssertTrue('Field value has not been fetched after post',Q.FieldByName('a').IsNull);
  Q.ApplyUpdates(0);
  AssertEquals('Still on correct field',1,Q.FieldByName('id').AsInteger);
  AssertEquals('Field value a has been fetched from the database ','abcde',Q.FieldByName('a').AsString);
  AssertEquals('Field value b has NOT been fetched from the database ','',Q.FieldByName('b').AsString);
end;

Procedure TTestTSQLQuery.TestGeneratedRefreshSQLNoKey;
begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdefval');
    ExecuteDirect('create table testdefval (id integer not null, a varchar(10) default ''abcde'', b varchar(5) default ''fgh'', constraint pk_testdefval primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    end;
  FMyQ:=SQLDBConnector.Query;
  FMyQ.SQL.Text:='select * from testdefval';
  FMyQ.InsertSQL.Text:='insert into testdefval (id) values (:id)';
  FMyQ.Open;
  With FMyQ.FieldByName('id') do
    ProviderFlags:=ProviderFlags-[pfInKey];
  With FMyQ.FieldByName('a') do
    ProviderFlags:=ProviderFlags+[pfRefreshOnInsert,pfRefreshOnUpdate];
  FMyQ.Insert;
  FMyQ.FieldByName('id').AsInteger:=1;
  FMyQ.Post;
  AssertException('Cannot refresh without primary key',EUpdateError,@DoApplyUpdates);
end;

Procedure TTestTSQLQuery.TestRefreshSQLMultipleRecords;

begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdefval');
    ExecuteDirect('create table testdefval (id integer not null, a varchar(10) default ''abcde'', b varchar(5) default ''fgh'', constraint pk_testdefval primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    ExecuteDirect('insert into testdefval (id) values (123)');
    if Transaction.Active then
      Transaction.Commit;
    end;
  FMyQ:=SQLDBConnector.Query;
  FMyQ.SQL.Text:='select * from testdefval';
  FMyQ.InsertSQL.Text:='insert into testdefval (id) values (:id)';
  FMyQ.RefreshSQL.Text:='select * from testdefval';
  FMyQ.Open;
  With FMyQ.FieldByName('id') do
    ProviderFlags:=ProviderFlags+[pfInKey];
  With FMyQ.FieldByName('a') do
    ProviderFlags:=ProviderFlags+[pfRefreshOnInsert,pfRefreshOnUpdate];
  FMyQ.Insert;
  FMyQ.FieldByName('id').AsInteger:=1;
  FMyQ.Post;
  AssertException('Multiple records returned by RefreshSQL gives an error',EUpdateError,@DoApplyUpdates);
end;

Procedure TTestTSQLQuery.TestRefreshSQLNoRecords;
begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdefval');
    ExecuteDirect('create table testdefval (id integer not null, a varchar(10) default ''abcde'', b varchar(5) default ''fgh'', constraint pk_testdefval primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    ExecuteDirect('insert into testdefval (id) values (123)');
    if Transaction.Active then
      Transaction.Commit;
    end;
  FMyQ:=SQLDBConnector.Query;
  FMyQ.SQL.Text:='select * from testdefval';
  FMyQ.InsertSQL.Text:='insert into testdefval (id) values (:id)';
  FMyQ.RefreshSQL.Text:='select * from testdefval where 1=2';
  FMyQ.Open;
  With FMyQ.FieldByName('id') do
    ProviderFlags:=ProviderFlags+[pfInKey];
  With FMyQ.FieldByName('a') do
    ProviderFlags:=ProviderFlags+[pfRefreshOnInsert,pfRefreshOnUpdate];
  FMyQ.Insert;
  FMyQ.FieldByName('id').AsInteger:=1;
  FMyQ.Post;
  AssertException('No records returned by RefreshSQL gives an error',EUpdateError,@DoApplyUpdates);
end;

Procedure TTestTSQLQuery.TestFetchAutoInc;
var datatype: string;
    id: largeint;
begin
  with SQLDBConnector do
    begin
    if not (sqLastInsertID in Connection.ConnOptions) then
      Ignore(STestNotApplicable);
    case SQLServerType of
      ssMySQL:
        datatype := 'integer auto_increment';
      ssMSSQL, ssSybase:
        datatype := 'integer identity';
      ssSQLite:
        datatype := 'integer';
      else
        Ignore(STestNotApplicable);
    end;
    TryDropIfExist('FPDEV2');
    ExecuteDirect('create table FPDEV2 (id '+datatype+' primary key, f varchar(5))');
    CommitDDL;
    end;

  with SQLDBConnector.Query do
    begin
    SQL.Text:='select * from FPDEV2';
    Open;
    Insert;
    FieldByName('f').AsString:='a';
    Post;  // #1 record
    Append;
    FieldByName('f').AsString:='b';
    Post;  // #2 record
    AssertTrue('ID field is not null after Post', FieldByName('id').IsNull);
    First; // #1 record
    ApplyUpdates(0);
    AssertTrue('ID field is still null after ApplyUpdates', Not FieldByName('id').IsNull);
    // Should be 1 after the table was created, but this is not guaranteed... So we just test positive values.
    id := FieldByName('id').AsLargeInt;
    AssertTrue('ID field has not positive value', id>0);
    Next;  // #2 record
    AssertTrue('Next ID value is not greater than previous', FieldByName('id').AsLargeInt>id);
    end;
end;


{ TTestTSQLConnection }

procedure TTestTSQLConnection.TestImplicitTransaction;

Var
  T : TSQLTransaction;

begin
  T:=TSQLTransaction.Create(Nil);
  try
    T.Options:=[stoUseImplicit];
    T.DataBase:=SQLDBConnector.Connection;
  finally
    T.Free;
  end;
end;

procedure TTestTSQLConnection.TestImplicitTransaction2;

Var
  T : TSQLTransaction;

begin
  T:=TSQLTransaction.Create(Nil);
  try
    T.Options:=[stoUseImplicit];
    SQLDBConnector.Connection.Transaction:=T;
  finally
    T.Free;
  end;
end;

procedure TTestTSQLConnection.SetImplicit;

begin
  SQLDBConnector.Transaction.Options:=[stoUseImplicit];
end;

procedure TTestTSQLConnection.TestImplicitTransactionNotAssignable;

begin
  AssertException('Cannot set toUseImplicit option if database does not allow it',EDatabaseError,@SetImplicit);
  AssertException('Cannot assign database to transaction with toUseImplicit, if database does not allow it',EDatabaseError,@TestImplicitTransaction);
  AssertException('Cannot assign transaction with toUseImplicit to database, if database does not allow it',EDatabaseError,@TestImplicitTransaction2);
end;

procedure TTestTSQLConnection.TestImplicitTransactionOK;


var
  Q : TSQLQuery;
  T : TSQLTransaction;
  I : Integer;
begin
  with SQLDBConnector do
    begin
    TryDropIfExist('testdiscon');
    ExecuteDirect('create table testdiscon (id integer not null, a varchar(10), constraint pk_testdiscon primary key(id))');
    if Transaction.Active then
      Transaction.Commit;
    end;
  SetImplicit;
  Q:=SQLDBConnector.Query;
  for I:=1 to 2 do
    begin
    Q.SQL.Text:=Format('INSERT INTO testdiscon values (%d,''%.6d'');',[i,i]);
    Q.Prepare;
    Q.ExecSQL;
    // We do not commit anything explicitly.
    end;
  Q:=Nil;
  T:=Nil;
  try
    T:=TSQLTransaction.Create(Nil);
    Q:=TSQLQuery.Create(Nil);
    Q.Transaction:=T;
    Q.Database:=SQLDBConnector.Connection;
    T.Database:=SQLDBConnector.Connection;
    Q.SQL.text:='SELECT COUNT(*) from testdiscon';
    Q.Open;
    AssertEquals('Records have been committed to database',2,Q.Fields[0].AsInteger);
  finally
    Q.Free;
    T.Free;
  end;
end;

procedure TTestTSQLConnection.TestUseImplicitTransaction;
begin
  if (sqImplicitTransaction in SQLDBConnector.Connection.ConnOptions) then
    TestImplicitTransactionOK
  else
    TestImplicitTransactionNotAssignable;
end;

procedure TTestTSQLConnection.TryOpen;

begin
  SQLDBConnector.Query.Open;
end;

procedure TTestTSQLConnection.TestUseExplicitTransaction;
begin
  SQLDBConnector.Transaction.Active:=False;
  SQLDBConnector.Transaction.Options:=[stoExplicitStart];
  SQLDBConnector.Query.SQL.Text:='select * from FPDEV';
  AssertException('toExplicitStart raises exception on implicit start',EDatabaseError,@TryOpen)
end;

procedure TTestTSQLConnection.TestExplicitConnect;
begin
  SQLDBConnector.Transaction.Active:=False;
  SQLDBConnector.Connection.Options:=[scoExplicitConnect];
  SQLDBConnector.Connection.Connected:=False;
  SQLDBConnector.Query.SQL.Text:='select * from FPDEV';
  AssertException('toExplicitStart raises exception on implicit start',EDatabaseError,@TryOpen)
end;

{ TTestTSQLScript }

procedure TTestTSQLScript.TestExecuteScript;
var Ascript : TSQLScript;
begin
  Ascript := TSQLScript.Create(nil);
  try
    with Ascript do
      begin
      DataBase := SQLDBConnector.Connection;
      Transaction := SQLDBConnector.Transaction;
      Script.Clear;
      Script.Append('create table FPDEV_A (id int);');
      Script.Append('create table FPDEV_B (id int);');
      ExecuteScript;
      // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
      SQLDBConnector.CommitDDL;
      end;
  finally
    AScript.Free;
    SQLDBConnector.ExecuteDirect('drop table FPDEV_A');
    SQLDBConnector.ExecuteDirect('drop table FPDEV_B');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    SQLDBConnector.CommitDDL;
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
      DataBase := SQLDBConnector.Connection;
      Transaction := SQLDBConnector.Transaction;
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
      SQLDBConnector.CommitDDL;
      end;
  finally
    AScript.Free;
    SQLDBConnector.ExecuteDirect('DROP PROCEDURE FPDEV_TESTCOLON');
    // Firebird/Interbase need a commit after a DDL statement. Not necessary for the other connections
    SQLDBConnector.CommitDDL;
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
      DataBase := SQLDBConnector.Connection;
      Transaction := SQLDBConnector.Transaction;
      Script.Clear;
      UseCommit:=true;
      // Example procedure that selects table names
      Script.Append('CREATE TABLE fpdev_scriptusecommit (logmessage VARCHAR(255));');
      Script.Append('COMMIT;'); //needed for table to show up
      Script.Append('INSERT INTO fpdev_scriptusecommit (logmessage) VALUES('''+TestValue+''');');
      Script.Append('COMMIT;');
      ExecuteScript;
      // This line should not run, as the commit above should have taken care of it:
      //SQLDBConnector.CommitDDL;
      // Test whether second line of script executed, just to be sure
      CheckQuery:=SQLDBConnector.Query;
      CheckQuery.SQL.Text:='SELECT logmessage FROM fpdev_scriptusecommit ';
      CheckQuery.Open;
      CheckEquals(TestValue, CheckQuery.Fields[0].AsString, 'Insert script line should have inserted '+TestValue);
      CheckQuery.Close;
      end;
  finally
    AScript.Free;
    SQLDBConnector.ExecuteDirect('DROP TABLE fpdev_scriptusecommit');
    SQLDBConnector.Transaction.Commit;
  end;
end;

{ TSQLDBTestCase }

function TSQLDBTestCase.GetSQLDBConnector: TSQLDBConnector;
begin
  Result:=DBConnector as TSQLDBConnector;
end;

procedure TSQLDBTestCase.SetUp;
begin
  inherited SetUp;
  InitialiseDBConnector;
  DBConnector.StartTest(TestName);
end;

procedure TSQLDBTestCase.TearDown;
begin
  DBConnector.StopTest(TestName);
  if assigned(DBConnector) then
    with SQLDBConnector do
      if Assigned(Transaction) and Transaction.Active and not (stoUseImplicit in Transaction.Options) then
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
