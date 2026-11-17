unit utc.opf.integration.mysql;

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

  TMySQLTestIntegration = class(TTestIntegrationCommon)
  protected procedure CreateTestTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestAutoInc = class(TTestAutoIncCommon)
  protected procedure CreateAutoIncTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestCascadeDelete = class(TTestCascadeDeleteCommon)
  protected procedure CreateCascadeTables; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestInterfacePersister = class(TTestInterfacePersisterCommon)
  protected procedure CreateProductTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestExplicitTransaction = class(TTestExplicitTransactionCommon)
  protected procedure CreateContactTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestDDLExec = class(TTestDDLExecCommon)
  protected
    function GetDDLTableName: String; override;
    function GetManualTableName: String; override;
    function GetManualDDL: String; override;
    procedure CleanupDDLTables; override;
    procedure SetUp; override; procedure TearDown; override;
  end;

  TMySQLTestManagerSave = class(TTestManagerSaveCommon)
  protected procedure CreateContactTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestManagerDelete = class(TTestManagerDeleteCommon)
  protected procedure CreateContactTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestManagerLoad = class(TTestManagerLoadCommon)
  protected procedure CreateLoadTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestManagerLoadList = class(TTestManagerLoadListCommon)
  protected procedure CreateContactTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestMappedCRUD = class(TTestMappedCRUDCommon)
  protected procedure CreateMappedTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestJsonBlobCRUD = class(TTestJsonBlobCRUDCommon)
  protected procedure CreateJsonContactTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestUUID = class(TTestUUIDCommon)
  protected procedure CreateUUIDTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestCustomGen = class(TTestCustomGenCommon)
  protected procedure CreateCustomGenTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestLoadChildren = class(TTestLoadChildrenCommon)
  protected procedure CreateLoadChildrenTables; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestCustomSQL = class(TTestCustomSQLCommon)
  protected procedure CreateCustomSQLTables; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestExtendedTypesCRUD = class(TTestExtendedTypesCRUDCommon)
  protected procedure CreateExtendedTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestErrorDiagnostics = class(TTestErrorDiagnosticsCommon)
  protected procedure CreateContactTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestNullableRoundTrip = class(TTestNullableRoundTripCommon)
  protected procedure CreateNullableTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestIOPFNullableRoundTrip = class(TTestIOPFNullableRoundTripCommon)
  protected procedure CreateIOPFNullableTable; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestM2MLoadAssociations = class(TTestM2MLoadAssociationsCommon)
  protected procedure CreateM2MTables; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestM2MAssociations = class(TTestM2MAssociationsCommon)
  protected procedure CreateM2MTables; override; procedure SetUp; override; procedure TearDown; override; end;

  TMySQLTestM2MCascadeDelete = class(TTestM2MCascadeDeleteCommon)
  protected procedure CreateM2MTables; override; procedure SetUp; override; procedure TearDown; override; end;

implementation

uses Opf.ListReg, Opf.M2M;

{ MySQL: tables use ENGINE=InnoDB for FK/transaction support }

procedure TMySQLTestIntegration.CreateTestTable;
begin DeleteTables(['TestIntPerson']); ExecDDL('CREATE TABLE TestIntPerson (ID INTEGER PRIMARY KEY, Name VARCHAR(255), Age INTEGER) ENGINE=InnoDB'); end;
procedure TMySQLTestIntegration.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestIntegration.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestAutoInc.CreateAutoIncTable;
begin DeleteTables(['TestAutoInc']); ExecDDL('CREATE TABLE TestAutoInc (ID INT AUTO_INCREMENT PRIMARY KEY, Name TEXT) ENGINE=InnoDB'); end;
procedure TMySQLTestAutoInc.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestAutoInc.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestCascadeDelete.CreateCascadeTables;
begin
  DeleteTables(['TestCascadeGrandchild', 'TestCascadeChild', 'TestCascadeParent']);
  ExecDDL('CREATE TABLE TestCascadeParent (ID INTEGER PRIMARY KEY, Name VARCHAR(100)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestCascadeChild (ID INTEGER PRIMARY KEY, ParentID INTEGER, Val VARCHAR(100)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestCascadeGrandchild (ID INTEGER PRIMARY KEY, ChildID INTEGER, Data VARCHAR(100)) ENGINE=InnoDB');
end;
procedure TMySQLTestCascadeDelete.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestCascadeDelete.TearDown;
begin inherited TearDown; DeleteTables(['TestCascadeGrandchild', 'TestCascadeChild', 'TestCascadeParent']); TearDownDatabase; end;

procedure TMySQLTestInterfacePersister.CreateProductTable;
begin DeleteTables(['TestIntfProduct']); ExecDDL('CREATE TABLE TestIntfProduct (ID INT AUTO_INCREMENT PRIMARY KEY, Name VARCHAR(255), Price DOUBLE) ENGINE=InnoDB'); end;
procedure TMySQLTestInterfacePersister.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestInterfacePersister.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestExplicitTransaction.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name VARCHAR(100)) ENGINE=InnoDB'); end;
procedure TMySQLTestExplicitTransaction.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestExplicitTransaction.TearDown; begin inherited TearDown; TearDownDatabase; end;

function TMySQLTestDDLExec.GetDDLTableName: String; begin Result:='TestDDLMy'; end;
function TMySQLTestDDLExec.GetManualTableName: String; begin Result:='TestManualMy'; end;
function TMySQLTestDDLExec.GetManualDDL: String; begin Result:='CREATE TABLE TestManualMy (ID INTEGER PRIMARY KEY, Val TEXT)'; end;
procedure TMySQLTestDDLExec.CleanupDDLTables; begin DeleteTables(['TestDDLMy', 'TestManualMy']); end;
procedure TMySQLTestDDLExec.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestDDLExec.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestManagerSave.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestManagerSave.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestManagerSave.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestManagerDelete.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestManagerDelete.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestManagerDelete.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestManagerLoad.CreateLoadTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestManagerLoad.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestManagerLoad.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestManagerLoadList.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestManagerLoadList.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestManagerLoadList.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestMappedCRUD.CreateMappedTable;
begin DeleteTables(['MappedContact']); ExecDDL('CREATE TABLE MappedContact (contact_id INTEGER PRIMARY KEY, full_name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestMappedCRUD.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestMappedCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestJsonBlobCRUD.CreateJsonContactTable;
begin DeleteTables(['TestJsonContact']); ExecDDL('CREATE TABLE TestJsonContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255), Address TEXT) ENGINE=InnoDB'); end;
procedure TMySQLTestJsonBlobCRUD.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestJsonBlobCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestUUID.CreateUUIDTable;
begin DeleteTables(['TestUUID']); ExecDDL('CREATE TABLE TestUUID (ID VARCHAR(36) PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestUUID.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestUUID.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestCustomGen.CreateCustomGenTable;
begin DeleteTables(['TestCustomGen']); ExecDDL('CREATE TABLE TestCustomGen (ID VARCHAR(36) PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestCustomGen.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestCustomGen.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestLoadChildren.CreateLoadChildrenTables;
begin
  DeleteTables(['TestAddress', 'TestContactWithChildren']);
  ExecDDL('CREATE TABLE TestContactWithChildren (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestAddress (ID INTEGER PRIMARY KEY, ContactID INTEGER, Street VARCHAR(255)) ENGINE=InnoDB');
end;
procedure TMySQLTestLoadChildren.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestLoadChildren.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestCustomSQL.CreateCustomSQLTables;
begin
  DeleteTables(['TestCustomSQLContact', 'TestCustomSQLDept']);
  ExecDDL('CREATE TABLE TestCustomSQLDept (ID INTEGER PRIMARY KEY, DeptName VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestCustomSQLContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255), DeptID INTEGER) ENGINE=InnoDB');
end;
procedure TMySQLTestCustomSQL.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestCustomSQL.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestExtendedTypesCRUD.CreateExtendedTable;
begin DeleteTables(['TestExtendedTypeContact']); ExecDDL('CREATE TABLE TestExtendedTypeContact (ID INTEGER PRIMARY KEY, Color INTEGER, Flags INTEGER, Extra TEXT) ENGINE=InnoDB'); end;
procedure TMySQLTestExtendedTypesCRUD.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestExtendedTypesCRUD.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestErrorDiagnostics.CreateContactTable;
begin DeleteTables(['TestMgrSaveContact']); ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB'); end;
procedure TMySQLTestErrorDiagnostics.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestErrorDiagnostics.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestNullableRoundTrip.CreateNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestNullableInt (ID INTEGER PRIMARY KEY, Name VARCHAR(255), Age INTEGER) ENGINE=InnoDB';
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.ExecSQL; FConn.Transaction.Commit;
  finally Q.Free; end;
end;
procedure TMySQLTestNullableRoundTrip.SetUp; begin SetUpDatabase(TMySQLSetupHelper); DeleteTables(['TestNullableInt']); inherited SetUp; end;
procedure TMySQLTestNullableRoundTrip.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestIOPFNullableRoundTrip.CreateIOPFNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestIntNullableObj (ID INTEGER PRIMARY KEY, Name VARCHAR(255), Age INTEGER) ENGINE=InnoDB';
    if not FConn.Transaction.Active then FConn.Transaction.StartTransaction;
    Q.ExecSQL; FConn.Transaction.Commit;
  finally Q.Free; end;
end;
procedure TMySQLTestIOPFNullableRoundTrip.SetUp; begin SetUpDatabase(TMySQLSetupHelper); DeleteTables(['TestIntNullableObj']); inherited SetUp; end;
procedure TMySQLTestIOPFNullableRoundTrip.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestM2MLoadAssociations.CreateM2MTables;
begin
  DeleteTables(['student_course', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id)) ENGINE=InnoDB');
end;
procedure TMySQLTestM2MLoadAssociations.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestM2MLoadAssociations.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestM2MAssociations.CreateM2MTables;
begin
  DeleteTables(['student_course', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id)) ENGINE=InnoDB');
end;
procedure TMySQLTestM2MAssociations.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestM2MAssociations.TearDown; begin inherited TearDown; TearDownDatabase; end;

procedure TMySQLTestM2MCascadeDelete.CreateM2MTables;
begin
  DeleteTables(['student_club', 'student_course', 'TestClub', 'TestCourse', 'TestStudent']);
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE TestClub (ID INTEGER PRIMARY KEY, Name VARCHAR(255)) ENGINE=InnoDB');
  ExecDDL('CREATE TABLE student_club (student_id INTEGER, club_id INTEGER, PRIMARY KEY(student_id, club_id)) ENGINE=InnoDB');
end;
procedure TMySQLTestM2MCascadeDelete.SetUp; begin SetUpDatabase(TMySQLSetupHelper); inherited SetUp; end;
procedure TMySQLTestM2MCascadeDelete.TearDown; begin inherited TearDown; TearDownDatabase; end;

initialization
  RegisterTests('MySQL',[TMySQLTestIntegration,
  TMySQLTestAutoInc,
  TMySQLTestCascadeDelete,
  TMySQLTestInterfacePersister,
  TMySQLTestExplicitTransaction,
  TMySQLTestDDLExec,
  TMySQLTestManagerSave,
  TMySQLTestManagerDelete,
  TMySQLTestManagerLoad,
  TMySQLTestManagerLoadList,
  TMySQLTestMappedCRUD,
  TMySQLTestJsonBlobCRUD,
  TMySQLTestUUID,
  TMySQLTestCustomGen,
  TMySQLTestLoadChildren,
  TMySQLTestCustomSQL,
  TMySQLTestExtendedTypesCRUD,
  TMySQLTestErrorDiagnostics,
  TMySQLTestNullableRoundTrip,
  TMySQLTestIOPFNullableRoundTrip,
  TMySQLTestM2MLoadAssociations,
  TMySQLTestM2MAssociations,
  TMySQLTestM2MCascadeDelete]);

end.
