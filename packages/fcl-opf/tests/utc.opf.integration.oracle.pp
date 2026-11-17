unit utc.opf.integration.oracle;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, db, sqldb,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Consts, Opf.Fields, Opf.Driver, Opf.Sql, Opf.DDL, Opf.Executor, Opf.Transaction,
  Opf.Manager, Opf.Registry, Opf.Persister.RTTI, Opf.Persister.Intf,
  Opf.Relations,
  utc.opf.executor,
  utc.opf.integration.base,
  utc.opf.integration.common;

Type

  { TOracleTestIntegration }
  TOracleTestIntegration = class(TTestIntegrationCommon)
  protected
    procedure CreateTestTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestAutoInc }
  TOracleTestAutoInc = class(TTestAutoIncCommon)
  protected
    procedure CreateAutoIncTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestSequence }
  TOracleTestSequence = class(TTestSequenceCommon)
  protected
    procedure CreateSequenceTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestCascadeDelete }
  TOracleTestCascadeDelete = class(TTestCascadeDeleteCommon)
  protected
    procedure CreateCascadeTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestInterfacePersister }
  TOracleTestInterfacePersister = class(TTestInterfacePersisterCommon)
  protected
    procedure CreateProductTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestExplicitTransaction }
  TOracleTestExplicitTransaction = class(TTestExplicitTransactionCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestDDLExec }
  TOracleTestDDLExec = class(TTestDDLExecCommon)
  protected
    function GetDDLTableName: String; override;
    function GetManualTableName: String; override;
    function GetManualDDL: String; override;
    procedure CleanupDDLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateTableIfNotExistsTwice;
  end;

  { TOracleTestManagerSave }
  TOracleTestManagerSave = class(TTestManagerSaveCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestManagerDelete }
  TOracleTestManagerDelete = class(TTestManagerDeleteCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestManagerLoad }
  TOracleTestManagerLoad = class(TTestManagerLoadCommon)
  protected
    procedure CreateLoadTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestManagerLoadList }
  TOracleTestManagerLoadList = class(TTestManagerLoadListCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestMappedCRUD }
  TOracleTestMappedCRUD = class(TTestMappedCRUDCommon)
  protected
    procedure CreateMappedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestJsonBlobCRUD }
  TOracleTestJsonBlobCRUD = class(TTestJsonBlobCRUDCommon)
  protected
    procedure CreateJsonContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestUUID }
  TOracleTestUUID = class(TTestUUIDCommon)
  protected
    procedure CreateUUIDTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestCustomGen }
  TOracleTestCustomGen = class(TTestCustomGenCommon)
  protected
    procedure CreateCustomGenTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestLoadChildren }
  TOracleTestLoadChildren = class(TTestLoadChildrenCommon)
  protected
    procedure CreateLoadChildrenTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestCustomSQL }
  TOracleTestCustomSQL = class(TTestCustomSQLCommon)
  protected
    procedure CreateCustomSQLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestExtendedTypesCRUD }
  TOracleTestExtendedTypesCRUD = class(TTestExtendedTypesCRUDCommon)
  protected
    procedure CreateExtendedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestErrorDiagnostics }
  TOracleTestErrorDiagnostics = class(TTestErrorDiagnosticsCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestNullableRoundTrip }
  TOracleTestNullableRoundTrip = class(TTestNullableRoundTripCommon)
  protected
    procedure CreateNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestIOPFNullableRoundTrip }
  TOracleTestIOPFNullableRoundTrip = class(TTestIOPFNullableRoundTripCommon)
  protected
    procedure CreateIOPFNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestM2MLoadAssociations }
  TOracleTestM2MLoadAssociations = class(TTestM2MLoadAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestM2MAssociations }
  TOracleTestM2MAssociations = class(TTestM2MAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TOracleTestM2MCascadeDelete }
  TOracleTestM2MCascadeDelete = class(TTestM2MCascadeDeleteCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses Opf.ListReg, Opf.M2M;

{ Drops a sequence, ignoring the error for a missing one }
procedure DropSequence(H: TDatabaseSetupHelper; C: TSQLConnection; const aName: String);
begin
  try
    H.ExecDDL(C, 'DROP SEQUENCE ' + aName);
  except
    if C.Transaction.Active then
      C.Transaction.Rollback;
  end;
end;

{ ===== TOracleTestIntegration ===== }
procedure TOracleTestIntegration.CreateTestTable;
begin DeleteTables(['TestIntPerson']); ExecDDL('CREATE TABLE TestIntPerson (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(255), Age NUMBER(10))'); end;
procedure TOracleTestIntegration.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestIntegration.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestAutoInc ===== }
procedure TOracleTestAutoInc.CreateAutoIncTable;
begin DeleteTables(['TestAutoInc']); ExecDDL('CREATE TABLE TestAutoInc (ID NUMBER(10) GENERATED BY DEFAULT ON NULL AS IDENTITY PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestAutoInc.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestAutoInc.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestSequence ===== }
procedure TOracleTestSequence.CreateSequenceTable;
var Conn: TOPConnection; Field: TOPFieldMap;
begin
  DeleteTables(['TestSequence']);
  DropSequence(FHelper, FConn, 'opf_test_seq');
  ExecDDL('CREATE SEQUENCE opf_test_seq START WITH 1');
  ExecDDL('CREATE TABLE TestSequence (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  OPRegistry.RegisterPersister(TTestSequenceContact, TRTTIPersister);
  OPRegistry.FindPersister(TTestSequenceContact).TableName:='TestSequence';
  Field:=OPRegistry.FindPersister(TTestSequenceContact).FieldMap.FindByPropertyName('ID');
  Field.Options:=Field.Options + [foInKey];
  Field.SequenceName:='opf_test_seq';
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;
procedure TOracleTestSequence.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestSequence.TearDown;
begin
  inherited TearDown;
  if Assigned(FConn) and FConn.Connected then
  begin
    FHelper.DeleteTables(FConn, ['TestSequence']);
    DropSequence(FHelper, FConn, 'opf_test_seq');
  end;
  TearDownDatabase;
end;

{ ===== TOracleTestCascadeDelete ===== }
procedure TOracleTestCascadeDelete.CreateCascadeTables;
begin
  DeleteTables(['TestCascadeGrandchild', 'TestCascadeChild', 'TestCascadeParent']);
  ExecDDL('CREATE TABLE TestCascadeParent (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestCascadeChild (ID NUMBER(10) PRIMARY KEY, ParentID NUMBER(10), Val VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestCascadeGrandchild (ID NUMBER(10) PRIMARY KEY, ChildID NUMBER(10), Data VARCHAR2(4000))');
end;
procedure TOracleTestCascadeDelete.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestCascadeDelete.TearDown;
begin
  inherited TearDown;
  DeleteTables(['TestCascadeGrandchild', 'TestCascadeChild', 'TestCascadeParent']);
  TearDownDatabase;
end;

{ ===== TOracleTestInterfacePersister ===== }
procedure TOracleTestInterfacePersister.CreateProductTable;
begin DeleteTables(['TestIntfProduct']); ExecDDL('CREATE TABLE TestIntfProduct (ID NUMBER(10) GENERATED BY DEFAULT ON NULL AS IDENTITY PRIMARY KEY, Name VARCHAR2(255), Price BINARY_DOUBLE)'); end;
procedure TOracleTestInterfacePersister.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestInterfacePersister.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestExplicitTransaction ===== }
procedure TOracleTestExplicitTransaction.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestExplicitTransaction.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestExplicitTransaction.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestDDLExec ===== }
function TOracleTestDDLExec.GetDDLTableName: String; begin Result:='testddlora'; end;
function TOracleTestDDLExec.GetManualTableName: String; begin Result:='testmanualora'; end;
function TOracleTestDDLExec.GetManualDDL: String; begin Result:='CREATE TABLE testmanualora (ID NUMBER(10) PRIMARY KEY, Val VARCHAR2(4000))'; end;
procedure TOracleTestDDLExec.CleanupDDLTables; begin DeleteTables(['testddlora', 'testmanualora']); end;
procedure TOracleTestDDLExec.TestCreateTableIfNotExistsTwice;
begin
  Ignore('IF NOT EXISTS not supported by the Oracle dialect');
end;

procedure TOracleTestDDLExec.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestDDLExec.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestManagerSave ===== }
procedure TOracleTestManagerSave.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestManagerSave.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestManagerSave.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestManagerDelete ===== }
procedure TOracleTestManagerDelete.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestManagerDelete.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestManagerDelete.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestManagerLoad ===== }
procedure TOracleTestManagerLoad.CreateLoadTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestManagerLoad.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestManagerLoad.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestManagerLoadList ===== }
procedure TOracleTestManagerLoadList.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestManagerLoadList.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestManagerLoadList.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestMappedCRUD ===== }
procedure TOracleTestMappedCRUD.CreateMappedTable;
begin DeleteTables(['MappedContact']); ExecDDL('CREATE TABLE MappedContact (contact_id NUMBER(10) PRIMARY KEY, full_name VARCHAR2(4000))'); end;
procedure TOracleTestMappedCRUD.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestMappedCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestJsonBlobCRUD ===== }
procedure TOracleTestJsonBlobCRUD.CreateJsonContactTable;
begin DeleteTables(['TestJsonContact']); ExecDDL('CREATE TABLE TestJsonContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000), Address CLOB)'); end;
procedure TOracleTestJsonBlobCRUD.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestJsonBlobCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestUUID ===== }
procedure TOracleTestUUID.CreateUUIDTable;
begin DeleteTables(['TestUUID']); ExecDDL('CREATE TABLE TestUUID (ID VARCHAR2(36) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestUUID.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestUUID.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestCustomGen ===== }
procedure TOracleTestCustomGen.CreateCustomGenTable;
begin DeleteTables(['TestCustomGen']); ExecDDL('CREATE TABLE TestCustomGen (ID VARCHAR2(36) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestCustomGen.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestCustomGen.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestLoadChildren ===== }
procedure TOracleTestLoadChildren.CreateLoadChildrenTables;
begin
  DeleteTables(['TestAddress', 'TestContactWithChildren']);
  ExecDDL('CREATE TABLE TestContactWithChildren (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestAddress (ID NUMBER(10) PRIMARY KEY, ContactID NUMBER(10), Street VARCHAR2(4000))');
end;
procedure TOracleTestLoadChildren.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestLoadChildren.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestCustomSQL ===== }
procedure TOracleTestCustomSQL.CreateCustomSQLTables;
begin
  DeleteTables(['TestCustomSQLContact', 'TestCustomSQLDept']);
  ExecDDL('CREATE TABLE TestCustomSQLDept (ID NUMBER(10) PRIMARY KEY, DeptName VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestCustomSQLContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000), DeptID NUMBER(10))');
end;
procedure TOracleTestCustomSQL.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestCustomSQL.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestExtendedTypesCRUD ===== }
procedure TOracleTestExtendedTypesCRUD.CreateExtendedTable;
begin DeleteTables(['TestExtendedTypeContact']); ExecDDL('CREATE TABLE TestExtendedTypeContact (ID NUMBER(10) PRIMARY KEY, Color NUMBER(10), Flags NUMBER(10), Extra VARCHAR2(4000))'); end;
procedure TOracleTestExtendedTypesCRUD.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestExtendedTypesCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestErrorDiagnostics ===== }
procedure TOracleTestErrorDiagnostics.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))'); end;
procedure TOracleTestErrorDiagnostics.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestErrorDiagnostics.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestNullableRoundTrip ===== }
procedure TOracleTestNullableRoundTrip.CreateNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestNullableInt (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000), Age NUMBER(10))';
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.ExecSQL; FConn.Transaction.Commit;
  finally Q.Free; end;
end;
procedure TOracleTestNullableRoundTrip.SetUp; begin SetUpDatabase(TOracleSetupHelper); DeleteTables(['TestNullableInt']); inherited SetUp; end;
procedure TOracleTestNullableRoundTrip.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestIOPFNullableRoundTrip ===== }
procedure TOracleTestIOPFNullableRoundTrip.CreateIOPFNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestIntNullableObj (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000), Age NUMBER(10))';
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.ExecSQL; FConn.Transaction.Commit;
  finally Q.Free; end;
end;
procedure TOracleTestIOPFNullableRoundTrip.SetUp; begin SetUpDatabase(TOracleSetupHelper); DeleteTables(['TestIntNullableObj']); inherited SetUp; end;
procedure TOracleTestIOPFNullableRoundTrip.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestM2MLoadAssociations ===== }
procedure TOracleTestM2MLoadAssociations.CreateM2MTables;
begin
  DeleteTables(['student_course', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestCourse (ID NUMBER(10) PRIMARY KEY, Title VARCHAR2(4000))');
  ExecDDL('CREATE TABLE student_course (student_id NUMBER(10), course_id NUMBER(10), PRIMARY KEY(student_id, course_id))');
end;
procedure TOracleTestM2MLoadAssociations.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestM2MLoadAssociations.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestM2MAssociations ===== }
procedure TOracleTestM2MAssociations.CreateM2MTables;
begin
  DeleteTables(['student_course', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestCourse (ID NUMBER(10) PRIMARY KEY, Title VARCHAR2(4000))');
  ExecDDL('CREATE TABLE student_course (student_id NUMBER(10), course_id NUMBER(10), PRIMARY KEY(student_id, course_id))');
end;
procedure TOracleTestM2MAssociations.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestM2MAssociations.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TOracleTestM2MCascadeDelete ===== }
procedure TOracleTestM2MCascadeDelete.CreateM2MTables;
begin
  DeleteTables(['student_club', 'student_course', 'TestClub', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  ExecDDL('CREATE TABLE TestCourse (ID NUMBER(10) PRIMARY KEY, Title VARCHAR2(4000))');
  ExecDDL('CREATE TABLE student_course (student_id NUMBER(10), course_id NUMBER(10), PRIMARY KEY(student_id, course_id))');
  ExecDDL('CREATE TABLE TestClub (ID NUMBER(10) PRIMARY KEY, Name VARCHAR2(4000))');
  ExecDDL('CREATE TABLE student_club (student_id NUMBER(10), club_id NUMBER(10), PRIMARY KEY(student_id, club_id))');
end;
procedure TOracleTestM2MCascadeDelete.SetUp; begin SetUpDatabase(TOracleSetupHelper); inherited SetUp; end;
procedure TOracleTestM2MCascadeDelete.TearDown; begin inherited TearDown; TearDownDatabase; end;

initialization
  RegisterTests('Oracle',[TOracleTestIntegration,
  TOracleTestAutoInc,
  TOracleTestSequence,
  TOracleTestCascadeDelete,
  TOracleTestInterfacePersister,
  TOracleTestExplicitTransaction,
  TOracleTestDDLExec,
  TOracleTestManagerSave,
  TOracleTestManagerDelete,
  TOracleTestManagerLoad,
  TOracleTestManagerLoadList,
  TOracleTestMappedCRUD,
  TOracleTestJsonBlobCRUD,
  TOracleTestUUID,
  TOracleTestCustomGen,
  TOracleTestLoadChildren,
  TOracleTestCustomSQL,
  TOracleTestExtendedTypesCRUD,
  TOracleTestErrorDiagnostics,
  TOracleTestNullableRoundTrip,
  TOracleTestIOPFNullableRoundTrip,
  TOracleTestM2MLoadAssociations,
  TOracleTestM2MAssociations,
  TOracleTestM2MCascadeDelete]);

end.
