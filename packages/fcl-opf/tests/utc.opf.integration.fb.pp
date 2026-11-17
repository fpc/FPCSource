unit utc.opf.integration.fb;

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

  TFirebirdTestIntegration = class(TTestIntegrationCommon)
  protected
    procedure CreateTestTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestAutoInc = class(TTestAutoIncCommon)
  protected
    procedure CreateAutoIncTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestSequence = class(TTestSequenceCommon)
  protected
    procedure CreateSequenceTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestCascadeDelete = class(TTestCascadeDeleteCommon)
  protected
    procedure CreateCascadeTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestInterfacePersister = class(TTestInterfacePersisterCommon)
  protected
    procedure CreateProductTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestExplicitTransaction = class(TTestExplicitTransactionCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestDDLExec = class(TTestDDLExecCommon)
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

  TFirebirdTestManagerSave = class(TTestManagerSaveCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestManagerDelete = class(TTestManagerDeleteCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestManagerLoad = class(TTestManagerLoadCommon)
  protected
    procedure CreateLoadTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestManagerLoadList = class(TTestManagerLoadListCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestMappedCRUD = class(TTestMappedCRUDCommon)
  protected
    procedure CreateMappedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestJsonBlobCRUD = class(TTestJsonBlobCRUDCommon)
  protected
    procedure CreateJsonContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestUUID = class(TTestUUIDCommon)
  protected
    procedure CreateUUIDTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestCustomGen = class(TTestCustomGenCommon)
  protected
    procedure CreateCustomGenTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestLoadChildren = class(TTestLoadChildrenCommon)
  protected
    procedure CreateLoadChildrenTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestCustomSQL = class(TTestCustomSQLCommon)
  protected
    procedure CreateCustomSQLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestExtendedTypesCRUD = class(TTestExtendedTypesCRUDCommon)
  protected
    procedure CreateExtendedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestErrorDiagnostics = class(TTestErrorDiagnosticsCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestNullableRoundTrip = class(TTestNullableRoundTripCommon)
  protected
    procedure CreateNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestIOPFNullableRoundTrip = class(TTestIOPFNullableRoundTripCommon)
  protected
    procedure CreateIOPFNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestM2MLoadAssociations = class(TTestM2MLoadAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestM2MAssociations = class(TTestM2MAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TFirebirdTestM2MCascadeDelete = class(TTestM2MCascadeDeleteCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses Opf.ListReg, Opf.M2M;

{ Firebird DDL: each statement in its own transaction via ExecDDL (error suppressed for DROP) }

procedure TFirebirdTestIntegration.CreateTestTable;
begin
  ExecDDL('DROP TABLE TestIntPerson'); 
  ExecDDL('CREATE TABLE TestIntPerson (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255), Age INTEGER)'); 
end;

procedure TFirebirdTestIntegration.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp; 
end;

procedure TFirebirdTestIntegration.TearDown;

begin
  inherited TearDown; 
  TearDownDatabase; 
end;

procedure TFirebirdTestAutoInc.CreateAutoIncTable;
begin
  ExecDDL('DROP TRIGGER TestAutoInc_BI');
  ExecDDL('DROP TABLE TestAutoInc');
  ExecDDL('DROP GENERATOR GEN_TestAutoInc_ID');
  ExecDDL('CREATE TABLE TestAutoInc (ID INTEGER PRIMARY KEY, Name VARCHAR(255))');
  ExecDDL('CREATE GENERATOR GEN_TestAutoInc_ID');
  ExecDDL('CREATE TRIGGER TestAutoInc_BI FOR TestAutoInc ACTIVE BEFORE INSERT POSITION 0 AS BEGIN IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(GEN_TestAutoInc_ID, 1); END');
end;

procedure TFirebirdTestAutoInc.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestAutoInc.TearDown;

begin
  inherited TearDown;
  ExecDDL('DROP TRIGGER TestAutoInc_BI');
  ExecDDL('DROP TABLE TestAutoInc');
  ExecDDL('DROP GENERATOR GEN_TestAutoInc_ID');
  TearDownDatabase;
end;

procedure TFirebirdTestSequence.CreateSequenceTable;
var 
  Conn: TOPConnection; 
  Field: TOPFieldMap;
begin
  ExecDDL('DROP TABLE TestSequence');
  ExecDDL('DROP GENERATOR opf_test_seq');
  ExecDDL('CREATE GENERATOR opf_test_seq');
  ExecDDL('CREATE TABLE TestSequence (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
  OPRegistry.RegisterPersister(TTestSequenceContact, TRTTIPersister);
  OPRegistry.FindPersister(TTestSequenceContact).TableName:='TestSequence';
  Field:=OPRegistry.FindPersister(TTestSequenceContact).FieldMap.FindByPropertyName('ID');
  Field.Options:=Field.Options + [foInKey];
  Field.SequenceName:='opf_test_seq';
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; 
  Conn.IsDefault:=True;
end;

procedure TFirebirdTestSequence.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestSequence.TearDown;
begin
  inherited TearDown;
  ExecDDL('DROP TABLE TestSequence');
  ExecDDL('DROP GENERATOR opf_test_seq');
  TearDownDatabase;
end;

procedure TFirebirdTestCascadeDelete.CreateCascadeTables;

begin
  ExecDDL('DROP TABLE TestCascadeGrandchild');
  ExecDDL('DROP TABLE TestCascadeChild');
  ExecDDL('DROP TABLE TestCascadeParent');
  ExecDDL('CREATE TABLE TestCascadeParent (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(100))');
  ExecDDL('CREATE TABLE TestCascadeChild (ID INTEGER NOT NULL PRIMARY KEY, ParentID INTEGER, Val VARCHAR(100))');
  ExecDDL('CREATE TABLE TestCascadeGrandchild (ID INTEGER NOT NULL PRIMARY KEY, ChildID INTEGER, Data VARCHAR(100))');
end;

procedure TFirebirdTestCascadeDelete.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestCascadeDelete.TearDown;
begin
  inherited TearDown;
  ExecDDL('DROP TABLE TestCascadeGrandchild');
  ExecDDL('DROP TABLE TestCascadeChild');
  ExecDDL('DROP TABLE TestCascadeParent');
  TearDownDatabase;
end;

procedure TFirebirdTestInterfacePersister.CreateProductTable;
begin
  ExecDDL('DROP TABLE TestIntfProduct');
  ExecDDL('DROP GENERATOR GEN_TestIntfProduct_ID');
  ExecDDL('CREATE TABLE TestIntfProduct (ID INTEGER PRIMARY KEY, Name VARCHAR(255), Price DOUBLE PRECISION)');
  ExecDDL('CREATE GENERATOR GEN_TestIntfProduct_ID');
  ExecDDL('CREATE TRIGGER TestIntfProduct_BI FOR TestIntfProduct ACTIVE BEFORE INSERT POSITION 0 AS BEGIN IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(GEN_TestIntfProduct_ID, 1); END');
end;

procedure TFirebirdTestInterfacePersister.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestInterfacePersister.TearDown;
begin
  inherited TearDown;
  ExecDDL('DROP TABLE TestIntfProduct');
  ExecDDL('DROP GENERATOR GEN_TestIntfProduct_ID');
  TearDownDatabase;
end;

procedure TFirebirdTestExplicitTransaction.CreateContactTable;
begin
  ExecDDL('DROP TABLE TestMgrSaveContact');
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(100))');
end;
procedure TFirebirdTestExplicitTransaction.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestExplicitTransaction.TearDown;
begin
  inherited TearDown;
  ExecDDL('DROP TABLE TestMgrSaveContact');
  TearDownDatabase;
end;

function TFirebirdTestDDLExec.GetDDLTableName: String;

begin
  Result:='TestDDLFB';
end;

function TFirebirdTestDDLExec.GetManualTableName: String;

begin
  Result:='TestManualFB';
end;

function TFirebirdTestDDLExec.GetManualDDL: String;

begin
  Result:='CREATE TABLE TestManualFB (ID INTEGER NOT NULL PRIMARY KEY, Val VARCHAR(100))';
end;

procedure TFirebirdTestDDLExec.CleanupDDLTables;
begin
  ExecDDL('DROP TABLE TestDDLFB');
  ExecDDL('DROP TABLE TestManualFB');
end;

procedure TFirebirdTestDDLExec.TestCreateTableIfNotExistsTwice;
begin
  Ignore('IF NOT EXISTS not supported on Firebird');
end;

procedure TFirebirdTestDDLExec.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestDDLExec.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestManagerSave.CreateContactTable;

begin
  ExecDDL('DROP TABLE TestMgrSaveContact');
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;

procedure TFirebirdTestManagerSave.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestManagerSave.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestManagerDelete.CreateContactTable;
begin
  ExecDDL('DROP TABLE TestMgrSaveContact');
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;

procedure TFirebirdTestManagerDelete.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestManagerDelete.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestManagerLoad.CreateLoadTable;
begin
  ExecDDL('DROP TABLE TestMgrSaveContact');
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;

procedure TFirebirdTestManagerLoad.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestManagerLoad.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestManagerLoadList.CreateContactTable;
begin
  ExecDDL('DROP TABLE TestMgrSaveContact');
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;
procedure TFirebirdTestManagerLoadList.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestManagerLoadList.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestMappedCRUD.CreateMappedTable;
begin
  ExecDDL('DROP TABLE MappedContact');
  ExecDDL('CREATE TABLE MappedContact (contact_id INTEGER NOT NULL PRIMARY KEY, full_name VARCHAR(255))');
end;
procedure TFirebirdTestMappedCRUD.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestMappedCRUD.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestJsonBlobCRUD.CreateJsonContactTable;
begin
  ExecDDL('DROP TABLE TestJsonContact');
  ExecDDL('CREATE TABLE TestJsonContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255), Address VARCHAR(4000))');
end;

procedure TFirebirdTestJsonBlobCRUD.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestJsonBlobCRUD.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestUUID.CreateUUIDTable;
begin
  ExecDDL('DROP TABLE TestUUID'); 
  ExecDDL('CREATE TABLE TestUUID (ID VARCHAR(36) NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;

procedure TFirebirdTestUUID.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestUUID.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestCustomGen.CreateCustomGenTable;
begin
  ExecDDL('DROP TABLE TestCustomGen'); 
  ExecDDL('CREATE TABLE TestCustomGen (ID VARCHAR(36) NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;

procedure TFirebirdTestCustomGen.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestCustomGen.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestLoadChildren.CreateLoadChildrenTables;
begin
  ExecDDL('DROP TABLE TestAddress');
  ExecDDL('DROP TABLE TestContactWithChildren');
  ExecDDL('CREATE TABLE TestContactWithChildren (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
  ExecDDL('CREATE TABLE TestAddress (ID INTEGER NOT NULL PRIMARY KEY, ContactID INTEGER, Street VARCHAR(255))');
end;

procedure TFirebirdTestLoadChildren.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestLoadChildren.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestCustomSQL.CreateCustomSQLTables;
begin
  ExecDDL('DROP TABLE TestCustomSQLContact');
  ExecDDL('DROP TABLE TestCustomSQLDept');
  ExecDDL('CREATE TABLE TestCustomSQLDept (ID INTEGER NOT NULL PRIMARY KEY, DeptName VARCHAR(255))');
  ExecDDL('CREATE TABLE TestCustomSQLContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255), DeptID INTEGER)');
end;
procedure TFirebirdTestCustomSQL.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestCustomSQL.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestExtendedTypesCRUD.CreateExtendedTable;

begin
  ExecDDL('DROP TABLE TestExtendedTypeContact'); 
  ExecDDL('CREATE TABLE TestExtendedTypeContact (ID INTEGER NOT NULL PRIMARY KEY, Color INTEGER, Flags INTEGER, Extra VARCHAR(255))');
end;

procedure TFirebirdTestExtendedTypesCRUD.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestExtendedTypesCRUD.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestErrorDiagnostics.CreateContactTable;

begin
  ExecDDL('DROP TABLE TestMgrSaveContact'); 
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
end;

procedure TFirebirdTestErrorDiagnostics.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestErrorDiagnostics.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestNullableRoundTrip.CreateNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; 
    Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestNullableInt (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255), Age INTEGER)';
    if not FConn.Transaction.Active then 
      FConn.Transaction.StartTransaction;
    Q.ExecSQL; 
    FConn.Transaction.Commit;
  finally 
    Q.Free;
  end;
end;
procedure TFirebirdTestNullableRoundTrip.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  ExecDDL('DROP TABLE TestNullableInt');
  inherited SetUp;
end;

procedure TFirebirdTestNullableRoundTrip.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestIOPFNullableRoundTrip.CreateIOPFNullableTable;
var Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=FConn; 
    Q.Transaction:=FConn.Transaction;
    Q.SQL.Text:='CREATE TABLE TestIntNullableObj (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255), Age INTEGER)';
    if not FConn.Transaction.Active then 
      FConn.Transaction.StartTransaction;
    Q.ExecSQL; 
    FConn.Transaction.Commit;
  finally 
    Q.Free;
  end;
end;

procedure TFirebirdTestIOPFNullableRoundTrip.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper); 
  ExecDDL('DROP TABLE TestIntNullableObj');
  inherited SetUp;
end;

procedure TFirebirdTestIOPFNullableRoundTrip.TearDown;

begin
  inherited TearDown; 
  TearDownDatabase;
end;

procedure TFirebirdTestM2MLoadAssociations.CreateM2MTables;
begin
  ExecDDL('DROP TABLE student_course');
  ExecDDL('DROP TABLE TestCourse');
  ExecDDL('DROP TABLE TestStudent');
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER NOT NULL PRIMARY KEY, Title VARCHAR(255))');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER NOT NULL, course_id INTEGER NOT NULL, PRIMARY KEY(student_id, course_id))');
end;

procedure TFirebirdTestM2MLoadAssociations.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestM2MLoadAssociations.TearDown;

begin
  inherited TearDown; 
  TearDownDatabase;
end;

procedure TFirebirdTestM2MAssociations.CreateM2MTables;
begin
  ExecDDL('DROP TABLE student_course');
  ExecDDL('DROP TABLE TestCourse');
  ExecDDL('DROP TABLE TestStudent');
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER NOT NULL PRIMARY KEY, Title VARCHAR(255))');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER NOT NULL, course_id INTEGER NOT NULL, PRIMARY KEY(student_id, course_id))');
end;
procedure TFirebirdTestM2MAssociations.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestM2MAssociations.TearDown;

begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TFirebirdTestM2MCascadeDelete.CreateM2MTables;
begin
  ExecDDL('DROP TABLE student_club');
  ExecDDL('DROP TABLE student_course');
  ExecDDL('DROP TABLE TestClub');
  ExecDDL('DROP TABLE TestCourse');
  ExecDDL('DROP TABLE TestStudent');
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER NOT NULL PRIMARY KEY, Title VARCHAR(255))');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER NOT NULL, course_id INTEGER NOT NULL, PRIMARY KEY(student_id, course_id))');
  ExecDDL('CREATE TABLE TestClub (ID INTEGER NOT NULL PRIMARY KEY, Name VARCHAR(255))');
  ExecDDL('CREATE TABLE student_club (student_id INTEGER NOT NULL, club_id INTEGER NOT NULL, PRIMARY KEY(student_id, club_id))');
end;

procedure TFirebirdTestM2MCascadeDelete.SetUp;

begin
  SetUpDatabase(TFirebirdSetupHelper);
  inherited SetUp;
end;

procedure TFirebirdTestM2MCascadeDelete.TearDown;

begin
  inherited TearDown; 
  TearDownDatabase;
end;

initialization
  RegisterTests('Firebird',[TFirebirdTestIntegration,
    TFirebirdTestAutoInc,
    TFirebirdTestSequence,
    TFirebirdTestCascadeDelete,
    TFirebirdTestInterfacePersister,
    TFirebirdTestExplicitTransaction,
    TFirebirdTestDDLExec,
    TFirebirdTestManagerSave,
    TFirebirdTestManagerDelete,
    TFirebirdTestManagerLoad,
    TFirebirdTestManagerLoadList,
    TFirebirdTestMappedCRUD,
    TFirebirdTestJsonBlobCRUD,
    TFirebirdTestUUID,
    TFirebirdTestCustomGen,
    TFirebirdTestLoadChildren,
    TFirebirdTestCustomSQL,
    TFirebirdTestExtendedTypesCRUD,
    TFirebirdTestErrorDiagnostics,
    TFirebirdTestNullableRoundTrip,
    TFirebirdTestIOPFNullableRoundTrip,
    TFirebirdTestM2MLoadAssociations,
    TFirebirdTestM2MAssociations,
    TFirebirdTestM2MCascadeDelete]);

end.
