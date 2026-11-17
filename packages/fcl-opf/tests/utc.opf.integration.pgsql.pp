unit utc.opf.integration.pgsql;

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

  { TPgSQLTestIntegration }
  TPgSQLTestIntegration = class(TTestIntegrationCommon)
  protected
    procedure CreateTestTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestAutoInc }
  TPgSQLTestAutoInc = class(TTestAutoIncCommon)
  protected
    procedure CreateAutoIncTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestSequence }
  TPgSQLTestSequence = class(TTestSequenceCommon)
  protected
    procedure CreateSequenceTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestCascadeDelete }
  TPgSQLTestCascadeDelete = class(TTestCascadeDeleteCommon)
  protected
    procedure CreateCascadeTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestInterfacePersister }
  TPgSQLTestInterfacePersister = class(TTestInterfacePersisterCommon)
  protected
    procedure CreateProductTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestExplicitTransaction }
  TPgSQLTestExplicitTransaction = class(TTestExplicitTransactionCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestDDLExec }
  TPgSQLTestDDLExec = class(TTestDDLExecCommon)
  protected
    function GetDDLTableName: String; override;
    function GetManualTableName: String; override;
    function GetManualDDL: String; override;
    procedure CleanupDDLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestManagerSave }
  TPgSQLTestManagerSave = class(TTestManagerSaveCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestManagerDelete }
  TPgSQLTestManagerDelete = class(TTestManagerDeleteCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestManagerLoad }
  TPgSQLTestManagerLoad = class(TTestManagerLoadCommon)
  protected
    procedure CreateLoadTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestManagerLoadList }
  TPgSQLTestManagerLoadList = class(TTestManagerLoadListCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestMappedCRUD }
  TPgSQLTestMappedCRUD = class(TTestMappedCRUDCommon)
  protected
    procedure CreateMappedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestJsonBlobCRUD }
  TPgSQLTestJsonBlobCRUD = class(TTestJsonBlobCRUDCommon)
  protected
    procedure CreateJsonContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestUUID }
  TPgSQLTestUUID = class(TTestUUIDCommon)
  protected
    procedure CreateUUIDTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestCustomGen }
  TPgSQLTestCustomGen = class(TTestCustomGenCommon)
  protected
    procedure CreateCustomGenTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestLoadChildren }
  TPgSQLTestLoadChildren = class(TTestLoadChildrenCommon)
  protected
    procedure CreateLoadChildrenTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestCustomSQL }
  TPgSQLTestCustomSQL = class(TTestCustomSQLCommon)
  protected
    procedure CreateCustomSQLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestExtendedTypesCRUD }
  TPgSQLTestExtendedTypesCRUD = class(TTestExtendedTypesCRUDCommon)
  protected
    procedure CreateExtendedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestErrorDiagnostics }
  TPgSQLTestErrorDiagnostics = class(TTestErrorDiagnosticsCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestNullableRoundTrip }
  TPgSQLTestNullableRoundTrip = class(TTestNullableRoundTripCommon)
  protected
    procedure CreateNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestIOPFNullableRoundTrip }
  TPgSQLTestIOPFNullableRoundTrip = class(TTestIOPFNullableRoundTripCommon)
  protected
    procedure CreateIOPFNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestM2MLoadAssociations }
  TPgSQLTestM2MLoadAssociations = class(TTestM2MLoadAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestM2MAssociations }
  TPgSQLTestM2MAssociations = class(TTestM2MAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TPgSQLTestM2MCascadeDelete }
  TPgSQLTestM2MCascadeDelete = class(TTestM2MCascadeDeleteCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses Opf.ListReg, Opf.M2M;

{ Standard PgSQL SetUp/TearDown pattern }

procedure PgDDL(H: TDatabaseSetupHelper; C: TSQLConnection; const S: String);
begin H.ExecDDL(C, S); end;

{ ===== TPgSQLTestIntegration ===== }
procedure TPgSQLTestIntegration.CreateTestTable;
begin DeleteTables(['TestIntPerson']); ExecDDL('CREATE TABLE TestIntPerson (ID INTEGER PRIMARY KEY, Name VARCHAR(255), Age INTEGER)'); end;
procedure TPgSQLTestIntegration.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestIntegration.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestAutoInc ===== }
procedure TPgSQLTestAutoInc.CreateAutoIncTable;
begin DeleteTables(['TestAutoInc']); ExecDDL('CREATE TABLE TestAutoInc (ID SERIAL PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestAutoInc.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestAutoInc.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestSequence ===== }
procedure TPgSQLTestSequence.CreateSequenceTable;
var Conn: TOPConnection; Field: TOPFieldMap;
begin
  DeleteTables(['TestSequence']);
  ExecDDL('DROP SEQUENCE IF EXISTS opf_test_seq');
  ExecDDL('CREATE SEQUENCE opf_test_seq START 1');
  ExecDDL('CREATE TABLE TestSequence (ID INTEGER PRIMARY KEY, Name TEXT)');
  OPRegistry.RegisterPersister(TTestSequenceContact, TRTTIPersister);
  OPRegistry.FindPersister(TTestSequenceContact).TableName:='TestSequence';
  Field:=OPRegistry.FindPersister(TTestSequenceContact).FieldMap.FindByPropertyName('ID');
  Field.Options:=Field.Options + [foInKey];
  Field.SequenceName:='opf_test_seq';
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
end;
procedure TPgSQLTestSequence.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestSequence.TearDown;
begin
  inherited TearDown;
  if Assigned(FConn) and FConn.Connected then
  begin
    FHelper.ExecDDL(FConn, 'DROP TABLE IF EXISTS TestSequence');
    FHelper.ExecDDL(FConn, 'DROP SEQUENCE IF EXISTS opf_test_seq');
  end;
  TearDownDatabase;
end;

{ ===== TPgSQLTestCascadeDelete ===== }
procedure TPgSQLTestCascadeDelete.CreateCascadeTables;
begin
  DeleteTables(['TestCascadeGrandchild', 'TestCascadeChild', 'TestCascadeParent']);
  ExecDDL('CREATE TABLE TestCascadeParent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCascadeChild (ID INTEGER PRIMARY KEY, ParentID INTEGER, Val TEXT)');
  ExecDDL('CREATE TABLE TestCascadeGrandchild (ID INTEGER PRIMARY KEY, ChildID INTEGER, Data TEXT)');
end;
procedure TPgSQLTestCascadeDelete.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestCascadeDelete.TearDown;
begin
  inherited TearDown;
  DeleteTables(['TestCascadeGrandchild', 'TestCascadeChild', 'TestCascadeParent']);
  TearDownDatabase;
end;

{ ===== TPgSQLTestInterfacePersister ===== }
procedure TPgSQLTestInterfacePersister.CreateProductTable;
begin DeleteTables(['TestIntfProduct']); ExecDDL('CREATE TABLE TestIntfProduct (ID SERIAL PRIMARY KEY, Name VARCHAR(255), Price DOUBLE PRECISION)'); end;
procedure TPgSQLTestInterfacePersister.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestInterfacePersister.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestExplicitTransaction ===== }
procedure TPgSQLTestExplicitTransaction.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestExplicitTransaction.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestExplicitTransaction.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestDDLExec ===== }
function TPgSQLTestDDLExec.GetDDLTableName: String; begin Result:='testddlpg'; end;
function TPgSQLTestDDLExec.GetManualTableName: String; begin Result:='testmanualpg'; end;
function TPgSQLTestDDLExec.GetManualDDL: String; begin Result:='CREATE TABLE testmanualpg (ID INTEGER PRIMARY KEY, Val TEXT)'; end;
procedure TPgSQLTestDDLExec.CleanupDDLTables; begin DeleteTables(['testddlpg', 'testmanualpg']); end;
procedure TPgSQLTestDDLExec.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestDDLExec.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestManagerSave ===== }
procedure TPgSQLTestManagerSave.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestManagerSave.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestManagerSave.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestManagerDelete ===== }
procedure TPgSQLTestManagerDelete.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestManagerDelete.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestManagerDelete.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestManagerLoad ===== }
procedure TPgSQLTestManagerLoad.CreateLoadTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestManagerLoad.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestManagerLoad.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestManagerLoadList ===== }
procedure TPgSQLTestManagerLoadList.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestManagerLoadList.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestManagerLoadList.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestMappedCRUD ===== }
procedure TPgSQLTestMappedCRUD.CreateMappedTable;
begin DeleteTables(['MappedContact']); ExecDDL('CREATE TABLE MappedContact (contact_id INTEGER PRIMARY KEY, full_name TEXT)'); end;
procedure TPgSQLTestMappedCRUD.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestMappedCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestJsonBlobCRUD ===== }
procedure TPgSQLTestJsonBlobCRUD.CreateJsonContactTable;
begin DeleteTables(['TestJsonContact']); ExecDDL('CREATE TABLE TestJsonContact (ID INTEGER PRIMARY KEY, Name TEXT, Address TEXT)'); end;
procedure TPgSQLTestJsonBlobCRUD.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestJsonBlobCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestUUID ===== }
procedure TPgSQLTestUUID.CreateUUIDTable;
begin DeleteTables(['TestUUID']); ExecDDL('CREATE TABLE TestUUID (ID VARCHAR(36) PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestUUID.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestUUID.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestCustomGen ===== }
procedure TPgSQLTestCustomGen.CreateCustomGenTable;
begin DeleteTables(['TestCustomGen']); ExecDDL('CREATE TABLE TestCustomGen (ID VARCHAR(36) PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestCustomGen.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestCustomGen.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestLoadChildren ===== }
procedure TPgSQLTestLoadChildren.CreateLoadChildrenTables;
begin
  DeleteTables(['TestAddress', 'TestContactWithChildren']);
  ExecDDL('CREATE TABLE TestContactWithChildren (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestAddress (ID INTEGER PRIMARY KEY, ContactID INTEGER, Street TEXT)');
end;
procedure TPgSQLTestLoadChildren.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestLoadChildren.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestCustomSQL ===== }
procedure TPgSQLTestCustomSQL.CreateCustomSQLTables;
begin
  DeleteTables(['TestCustomSQLContact', 'TestCustomSQLDept']);
  ExecDDL('CREATE TABLE TestCustomSQLDept (ID INTEGER PRIMARY KEY, DeptName TEXT)');
  ExecDDL('CREATE TABLE TestCustomSQLContact (ID INTEGER PRIMARY KEY, Name TEXT, DeptID INTEGER)');
end;
procedure TPgSQLTestCustomSQL.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestCustomSQL.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestExtendedTypesCRUD ===== }
procedure TPgSQLTestExtendedTypesCRUD.CreateExtendedTable;
begin DeleteTables(['TestExtendedTypeContact']); ExecDDL('CREATE TABLE TestExtendedTypeContact (ID INTEGER PRIMARY KEY, Color INTEGER, Flags INTEGER, Extra TEXT)'); end;
procedure TPgSQLTestExtendedTypesCRUD.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestExtendedTypesCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestErrorDiagnostics ===== }
procedure TPgSQLTestErrorDiagnostics.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;
procedure TPgSQLTestErrorDiagnostics.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestErrorDiagnostics.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestNullableRoundTrip ===== }
procedure TPgSQLTestNullableRoundTrip.CreateNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestNullableInt (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)';
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.ExecSQL; FConn.Transaction.Commit;
  finally Q.Free; end;
end;
procedure TPgSQLTestNullableRoundTrip.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); DeleteTables(['TestNullableInt']); inherited SetUp; end;
procedure TPgSQLTestNullableRoundTrip.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestIOPFNullableRoundTrip ===== }
procedure TPgSQLTestIOPFNullableRoundTrip.CreateIOPFNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestIntNullableObj (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)';
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.ExecSQL; FConn.Transaction.Commit;
  finally Q.Free; end;
end;
procedure TPgSQLTestIOPFNullableRoundTrip.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); DeleteTables(['TestIntNullableObj']); inherited SetUp; end;
procedure TPgSQLTestIOPFNullableRoundTrip.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestM2MLoadAssociations ===== }
procedure TPgSQLTestM2MLoadAssociations.CreateM2MTables;
begin
  DeleteTables(['student_course', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id))');
end;
procedure TPgSQLTestM2MLoadAssociations.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestM2MLoadAssociations.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestM2MAssociations ===== }
procedure TPgSQLTestM2MAssociations.CreateM2MTables;
begin
  DeleteTables(['student_course', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id))');
end;
procedure TPgSQLTestM2MAssociations.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestM2MAssociations.TearDown; begin inherited TearDown; TearDownDatabase; end;

{ ===== TPgSQLTestM2MCascadeDelete ===== }
procedure TPgSQLTestM2MCascadeDelete.CreateM2MTables;
begin
  DeleteTables(['student_club', 'student_course', 'TestClub', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id))');
  ExecDDL('CREATE TABLE TestClub (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE student_club (student_id INTEGER, club_id INTEGER, PRIMARY KEY(student_id, club_id))');
end;
procedure TPgSQLTestM2MCascadeDelete.SetUp; begin SetUpDatabase(TPgSQLSetupHelper); inherited SetUp; end;
procedure TPgSQLTestM2MCascadeDelete.TearDown; begin inherited TearDown; TearDownDatabase; end;

initialization
  RegisterTests('Postgres',[TPgSQLTestIntegration,
  TPgSQLTestAutoInc,
  TPgSQLTestSequence,
  TPgSQLTestCascadeDelete,
  TPgSQLTestInterfacePersister,
  TPgSQLTestExplicitTransaction,
  TPgSQLTestDDLExec,
  TPgSQLTestManagerSave,
  TPgSQLTestManagerDelete,
  TPgSQLTestManagerLoad,
  TPgSQLTestManagerLoadList,
  TPgSQLTestMappedCRUD,
  TPgSQLTestJsonBlobCRUD,
  TPgSQLTestUUID,
  TPgSQLTestCustomGen,
  TPgSQLTestLoadChildren,
  TPgSQLTestCustomSQL,
  TPgSQLTestExtendedTypesCRUD,
  TPgSQLTestErrorDiagnostics,
  TPgSQLTestNullableRoundTrip,
  TPgSQLTestIOPFNullableRoundTrip,
  TPgSQLTestM2MLoadAssociations,
  TPgSQLTestM2MAssociations,
  TPgSQLTestM2MCascadeDelete]);

end.
