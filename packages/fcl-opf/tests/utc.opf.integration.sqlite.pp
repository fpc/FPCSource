unit utc.opf.integration.sqlite;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, db, sqldb,
  sqlite3conn,
  fpcunit, testregistry,
{$ENDIF}
  contnrs, typinfo, Variants,
  Opf.Types, Opf.Fields, Opf.Driver, Opf.Sql, Opf.DDL, Opf.Executor, Opf.Transaction, Opf.Manager,
  Opf.Persister.Intf, Opf.Logging,
  Opf.Persister.RTTI,
  Opf.Nullable,
  Opf.M2M, utc.opf.executor,
  utc.opf.integration.base,
  utc.opf.integration.common;

Type

  { ===== SQLite leaf classes ===== }

  { TSQLiteTestIntegration }

  TSQLiteTestIntegration = class(TTestIntegrationCommon)
  protected
    procedure CreateTestTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSelectWithWhere;
  end;

  { TSQLiteTestManagerSave }

  TSQLiteTestManagerSave = class(TTestManagerSaveCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestManagerDelete }

  TSQLiteTestManagerDelete = class(TTestManagerDeleteCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestCascadeDelete }

  TSQLiteTestCascadeDelete = class(TTestCascadeDeleteCommon)
  protected
    procedure CreateCascadeTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestManagerLoad }

  TSQLiteTestManagerLoad = class(TTestManagerLoadCommon)
  protected
    procedure CreateLoadTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestManagerLoadList }

  TSQLiteTestManagerLoadList = class(TTestManagerLoadListCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestMappedCRUD }

  TSQLiteTestMappedCRUD = class(TTestMappedCRUDCommon)
  protected
    procedure CreateMappedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestJsonBlobCRUD }

  TSQLiteTestJsonBlobCRUD = class(TTestJsonBlobCRUDCommon)
  protected
    procedure CreateJsonContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestAutoInc }

  TSQLiteTestAutoInc = class(TTestAutoIncCommon)
  protected
    procedure CreateAutoIncTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestUUID }

  TSQLiteTestUUID = class(TTestUUIDCommon)
  protected
    procedure CreateUUIDTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestCustomGen }

  TSQLiteTestCustomGen = class(TTestCustomGenCommon)
  protected
    procedure CreateCustomGenTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestLoadChildren }

  TSQLiteTestLoadChildren = class(TTestLoadChildrenCommon)
  protected
    procedure CreateLoadChildrenTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestLazyLoadChildren }

  TSQLiteTestLazyLoadChildren = class(TTestLazyLoadChildrenCommon)
  protected
    procedure CreateLazyTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestLazyM2M }

  TSQLiteTestLazyM2M = class(TTestLazyM2MCommon)
  protected
    procedure CreateLazyM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestInterfacePersister }

  TSQLiteTestInterfacePersister = class(TTestInterfacePersisterCommon)
  protected
    procedure CreateProductTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestExplicitTransaction }

  TSQLiteTestExplicitTransaction = class(TTestExplicitTransactionCommon)
  private
    FLog: TStringList;
    procedure CaptureLog(Sender: TObject; aLevel: TLogKind; const aMsg: string);
    procedure StartLogCapture(aKinds: TLogKinds);
    procedure StopLogCapture;
    function CountLogged(const aPrefix, aText: string): Integer;
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExplicitTransactionAtomicDelete;
    procedure TestImplicitSkipsWhenExplicitActive;
    procedure TestFailedImplicitCommitNotMasked;
    procedure TestFailedSaveLoggedOnce;
    procedure TestRaiseIsTracedAndFailureLogged;
  end;

  { TSQLiteTestCustomSQL }

  TSQLiteTestCustomSQL = class(TTestCustomSQLCommon)
  protected
    procedure CreateCustomSQLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestExtendedTypesCRUD }

  TSQLiteTestExtendedTypesCRUD = class(TTestExtendedTypesCRUDCommon)
  protected
    procedure CreateExtendedTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSaveLoadSetEnumAsString;
  end;

  { TSQLiteTestErrorDiagnostics }

  TSQLiteTestErrorDiagnostics = class(TTestErrorDiagnosticsCommon)
  protected
    procedure CreateContactTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestNullableRoundTrip }

  TSQLiteTestNullableRoundTrip = class(TTestNullableRoundTripCommon)
  protected
    procedure CreateNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestIOPFNullableRoundTrip }

  TSQLiteTestIOPFNullableRoundTrip = class(TTestIOPFNullableRoundTripCommon)
  protected
    procedure CreateIOPFNullableTable; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestSaveList }

  TSQLiteTestSaveList = class(TTestSaveListCommon)
  protected
    procedure CreateSaveListTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestDDLExec }

  TSQLiteTestDDLExec = class(TTestDDLExecCommon)
  protected
    function GetDDLTableName: String; override;
    function GetManualTableName: String; override;
    function GetManualDDL: String; override;
    procedure CleanupDDLTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestM2MLoadAssociations }

  TSQLiteTestM2MLoadAssociations = class(TTestM2MLoadAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestM2MAssociations }

  TSQLiteTestM2MAssociations = class(TTestM2MAssociationsCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSQLiteTestM2MCascadeDelete }

  TSQLiteTestM2MCascadeDelete = class(TTestM2MCascadeDeleteCommon)
  protected
    procedure CreateM2MTables; override;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { ===== Optimistic locking fixture classes ===== }

  { TTestVerInt: version column of kind tkInteger. }

  TTestVerInt = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FContent: String;
    FVersion: Integer;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property Content: String read FContent write FContent;
    property Version: Integer read FVersion write FVersion;
  end;

  { TTestVerInt64: version column of kind tkInt64. }

  TTestVerInt64 = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FContent: String;
    FVersion: Int64;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Content: String read FContent write FContent;
    property Version: Int64 read FVersion write FVersion;
  end;

  { TTestVerStr: version column of kind tkAString, filled with a UUID. }

  TTestVerStr = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FContent: String;
    FVersion: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Content: String read FContent write FContent;
    property Version: String read FVersion write FVersion;
  end;

  { TTestVerShort: version column of kind tkSString. }

  TTestVerShort = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FContent: String;
    FVersion: ShortString;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Content: String read FContent write FContent;
    property Version: ShortString read FVersion write FVersion;
  end;

  { TTestVerUni: version column of kind tkUString. }

  TTestVerUni = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FContent: String;
    FVersion: UnicodeString;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Content: String read FContent write FContent;
    property Version: UnicodeString read FVersion write FVersion;
  end;

  { TTestVerNone: no version column at all. }

  TTestVerNone = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FContent: String;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    FState: TObjectPersistenceState;
  published
    property ID: Integer read FID write FID;
    property Content: String read FContent write FContent;
  end;

  { TTestVersionConcurrency — standalone, owns an in-memory SQLite database }

  TTestVersionConcurrency = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    procedure ExecDirect(const aSQL: UTF8String);
    function ReadString(const aSQL: UTF8String): String;
    function ReadInt64(const aSQL: UTF8String): Int64;
    procedure RegisterFixture(aClass: TClass; const aDDL: UTF8String; aVersioned: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIntegerVersion;
    procedure TestInt64Version;
    procedure TestUUIDVersion;
    procedure TestShortStringVersion;
    procedure TestUnicodeStringVersion;
    procedure TestConcurrentModificationUnicodeString;
    procedure TestConcurrentModification;
    procedure TestConcurrentModificationString;
    procedure TestPartialUpdateWithVersionField;
    procedure TestRollbackForcesFullUpdate;
    procedure TestNoVersionField;
  end;

  { TTestBackendSwitch — standalone, not inheriting from common }

  TTestBackendSwitch = class(TTestCase)
  private
    FConnA: TSQLite3Connection;
    FConnB: TSQLite3Connection;
    procedure CreateContactTable(aConn: TSQLite3Connection);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSwitchConnectionCRUD;
  end;

implementation

uses Opf.Registry, Opf.ListReg, Opf.Relations, Opf.ChangeTracking, Opf.Snapshot, Opf.Consts;

{ Helper: execute DDL via ExecDDL on the helper }

{ ===== TSQLiteTestIntegration ===== }

procedure TSQLiteTestIntegration.CreateTestTable;
begin
  ExecDDL('CREATE TABLE TestIntPerson (ID INTEGER PRIMARY KEY, Name TEXT, Age INTEGER)');
end;

procedure TSQLiteTestIntegration.SetUp;
begin
  SetUpDatabase(TSQLiteSetupHelper);
  inherited SetUp;
end;

procedure TSQLiteTestIntegration.TearDown;
begin
  inherited TearDown;
  TearDownDatabase;
end;

procedure TSQLiteTestIntegration.TestSelectWithWhere;
var SelectSQL, WhereSQL, SQL: UTF8String;
begin
  DirectInsert(1, 'Alice', 30);
  DirectInsert(2, 'Bob', 25);
  FObj.ID:=2; FObj.Name:=''; FObj.Age:=0;
  SelectSQL:=OPSQLBuilder.GetSelectSQL('TestIntPerson', FFieldMap.AsArray);
  WhereSQL:=OPSQLBuilder.GetWhereKey('TestIntPerson', FFieldMap.AsArray);
  SQL:=SelectSQL + ' WHERE ' + WhereSQL;
  FExecutor.QuerySQL(FObj, FPersister, FFieldMap, SQL, FConn);
  AssertEquals('ID loaded', 2, FObj.ID);
  AssertEquals('Name loaded', 'Bob', FObj.Name);
  AssertEquals('Age loaded', 25, FObj.Age);
end;

{ ===== TSQLiteTestManagerSave ===== }

procedure TSQLiteTestManagerSave.CreateContactTable;
begin ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestManagerSave.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestManagerSave.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestManagerDelete ===== }

procedure TSQLiteTestManagerDelete.CreateContactTable;
begin ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestManagerDelete.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestManagerDelete.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestCascadeDelete ===== }

procedure TSQLiteTestCascadeDelete.CreateCascadeTables;
begin
  ExecDDL('CREATE TABLE TestCascadeParent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCascadeChild (ID INTEGER PRIMARY KEY, ParentID INTEGER, Val TEXT)');
  ExecDDL('CREATE TABLE TestCascadeGrandchild (ID INTEGER PRIMARY KEY, ChildID INTEGER, Data TEXT)');
end;

procedure TSQLiteTestCascadeDelete.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestCascadeDelete.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestManagerLoad ===== }

procedure TSQLiteTestManagerLoad.CreateLoadTable;
begin ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestManagerLoad.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestManagerLoad.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestManagerLoadList ===== }

procedure TSQLiteTestManagerLoadList.CreateContactTable;
begin ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestManagerLoadList.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestManagerLoadList.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestMappedCRUD ===== }

procedure TSQLiteTestMappedCRUD.CreateMappedTable;
begin ExecDDL('CREATE TABLE MappedContact (contact_id INTEGER PRIMARY KEY, full_name TEXT)'); end;

procedure TSQLiteTestMappedCRUD.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestMappedCRUD.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestJsonBlobCRUD ===== }

procedure TSQLiteTestJsonBlobCRUD.CreateJsonContactTable;
begin ExecDDL('CREATE TABLE TestJsonContact (ID INTEGER PRIMARY KEY, Name TEXT, Address TEXT)'); end;

procedure TSQLiteTestJsonBlobCRUD.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestJsonBlobCRUD.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestAutoInc ===== }

procedure TSQLiteTestAutoInc.CreateAutoIncTable;
begin ExecDDL('CREATE TABLE TestAutoInc (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT)'); end;

procedure TSQLiteTestAutoInc.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestAutoInc.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestUUID ===== }

procedure TSQLiteTestUUID.CreateUUIDTable;
begin ExecDDL('CREATE TABLE TestUUID (ID TEXT PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestUUID.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestUUID.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestCustomGen ===== }

procedure TSQLiteTestCustomGen.CreateCustomGenTable;
begin ExecDDL('CREATE TABLE TestCustomGen (ID TEXT PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestCustomGen.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestCustomGen.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestLoadChildren ===== }

procedure TSQLiteTestLoadChildren.CreateLoadChildrenTables;
begin
  ExecDDL('CREATE TABLE TestContactWithChildren (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestAddress (ID INTEGER PRIMARY KEY, ContactID INTEGER, Street TEXT)');
end;

procedure TSQLiteTestLoadChildren.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestLoadChildren.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestLazyLoadChildren ===== }

procedure TSQLiteTestLazyLoadChildren.CreateLazyTables;
begin
  ExecDDL('CREATE TABLE TestLazyDepartment (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestLazyEmployee (ID INTEGER PRIMARY KEY, DepartmentID INTEGER, EmpName TEXT)');
end;

procedure TSQLiteTestLazyLoadChildren.SetUp;
begin
  SetUpDatabase(TSQLiteSetupHelper);
  inherited SetUp;
end;

procedure TSQLiteTestLazyLoadChildren.TearDown;
begin
  inherited TearDown;
  TearDownDatabase;
end;

{ ===== TSQLiteTestLazyM2M ===== }

procedure TSQLiteTestLazyM2M.CreateLazyM2MTables;
begin
  ExecDDL('CREATE TABLE TestLazyStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestLazyCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE lazy_student_course (student_id INTEGER, course_id INTEGER)');
end;

procedure TSQLiteTestLazyM2M.SetUp;
begin
  SetUpDatabase(TSQLiteSetupHelper);
  inherited SetUp;
end;

procedure TSQLiteTestLazyM2M.TearDown;
begin
  inherited TearDown;
  TearDownDatabase;
end;

{ ===== TSQLiteTestInterfacePersister ===== }

procedure TSQLiteTestInterfacePersister.CreateProductTable;
begin ExecDDL('CREATE TABLE TestIntfProduct (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT, Price REAL)'); end;

procedure TSQLiteTestInterfacePersister.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestInterfacePersister.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestExplicitTransaction ===== }

procedure TSQLiteTestExplicitTransaction.CreateContactTable;
begin ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestExplicitTransaction.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestExplicitTransaction.TearDown;
begin inherited TearDown; TearDownDatabase; end;

procedure TSQLiteTestExplicitTransaction.TestExplicitTransactionAtomicDelete;
var ObjSave, ObjDel: TTestMgrSaveContact;
begin
  DirectInsertContact(10, 'Existing');
  OPManager.BeginTransaction;
  ObjSave:=TTestMgrSaveContact.Create;
  try ObjSave.ID:=20; ObjSave.Name:='New'; ObjSave.FState:=opsNew; OPManager.Save(ObjSave); finally ObjSave.Free; end;
  ObjDel:=TTestMgrSaveContact.Create;
  try ObjDel.ID:=10; ObjDel.Name:='Existing'; ObjDel.FState:=opsClean; OPManager.Delete(ObjDel); finally ObjDel.Free; end;
  OPManager.Commit;
  AssertEquals('One row after atomic save+delete', 1, CountContactRows);
end;

procedure TSQLiteTestExplicitTransaction.TestImplicitSkipsWhenExplicitActive;
var Obj: TTestMgrSaveContact;
begin
  OPManager.BeginTransaction;
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  OPManager.Rollback;
  AssertEquals('Zero rows proves implicit did not commit', 0, CountContactRows);
end;

procedure TSQLiteTestExplicitTransaction.TestFailedImplicitCommitNotMasked;
var Obj: TTestMgrSaveContact;
    Msg: String;
begin
  // A deferred foreign key is only checked at COMMIT, so the implicit commit of Save fails.
  ExecDDL('DROP TABLE TestMgrSaveContact');
  ExecDDL('CREATE TABLE TestMgrParent (ID INTEGER PRIMARY KEY)');
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY '+
    'REFERENCES TestMgrParent(ID) DEFERRABLE INITIALLY DEFERRED, Name TEXT)');
  FConn.Close;
  FConn.Params.Values['foreign_keys']:='ON';
  FConn.Open;
  Msg:='';
  Obj:=TTestMgrSaveContact.Create;
  try
    Obj.ID:=1; Obj.Name:='Orphan'; Obj.FState:=opsNew;
    try
      OPManager.Save(Obj);
      Fail('Save with a dangling deferred foreign key should raise');
    except
      on E: EAssertionFailedError do raise;
      on E: Exception do Msg:=E.Message;
    end;
  finally
    Obj.Free;
  end;
  AssertTrue('Commit error propagates, got: '+Msg, Pos('FOREIGN KEY', UpperCase(Msg))>0);
  AssertFalse('No transaction left open', OPManager.InTransaction);
end;

procedure TSQLiteTestExplicitTransaction.CaptureLog(Sender: TObject; aLevel: TLogKind; const aMsg: string);
begin
  FLog.Add(Copy(GetEnumName(TypeInfo(TLogKind), Ord(aLevel)), 3, MaxInt)+': '+aMsg);
end;

procedure TSQLiteTestExplicitTransaction.StartLogCapture(aKinds: TLogKinds);
begin
  FLog:=TStringList.Create;
  OPLogger.LogKinds:=aKinds;
  OPLogger.OnLog:=@CaptureLog;
end;

procedure TSQLiteTestExplicitTransaction.StopLogCapture;
begin
  OPLogger.OnLog:=Nil;
  OPLogger.LogKinds:=[lkWarning, lkError, lkInfo];
  FreeAndNil(FLog);
end;

function TSQLiteTestExplicitTransaction.CountLogged(const aPrefix, aText: string): Integer;
var I: Integer;
begin
  Result:=0;
  for I:=0 to FLog.Count-1 do
    if (Pos(aPrefix+': ', FLog[I])=1) and ((aText='') or (Pos(aText, FLog[I])>0)) then
      Inc(Result);
end;

procedure TSQLiteTestExplicitTransaction.TestFailedSaveLoggedOnce;
var Obj: TTestMgrSaveContact;
begin
  DirectInsertContact(1, 'Alice');
  StartLogCapture([lkWarning, lkError]);
  try
    Obj:=TTestMgrSaveContact.Create;
    try
      // Duplicate primary key; Save(Obj) nests Save(Obj, State), both roll back
      Obj.ID:=1; Obj.Name:='Bob'; Obj.FState:=opsNew;
      try
        OPManager.Save(Obj);
        Fail('Save with a duplicate key should raise');
      except
        on E: EAssertionFailedError do raise;
        on E: Exception do ;
      end;
    finally
      Obj.Free;
    end;
    AssertEquals('Failure logged exactly once: '+FLog.Text, 1, CountLogged('Error', 'Save failed'));
    AssertEquals('Only one error entry: '+FLog.Text, 1, CountLogged('Error', ''));
  finally
    StopLogCapture;
  end;
end;

procedure TSQLiteTestExplicitTransaction.TestRaiseIsTracedAndFailureLogged;
var Obj: TTestMgrSaveContact;
begin
  StartLogCapture([lkError, lkTrace]);
  try
    Obj:=TTestMgrSaveContact.Create;
    try
      Obj.ID:=42; Obj.FState:=opsKey;
      try
        OPManager.Load(Obj);
        Fail('Load of a missing row should raise');
      except
        on E: EAssertionFailedError do raise;
        on E: EOPF do ;
      end;
    finally
      Obj.Free;
    end;
    AssertEquals('Raise site traced: '+FLog.Text, 1, CountLogged('Trace', 'TOPManager.Load:'));
    AssertEquals('Failure logged once: '+FLog.Text, 1, CountLogged('Error', 'Load failed'));
  finally
    StopLogCapture;
  end;
end;

{ ===== TSQLiteTestCustomSQL ===== }

procedure TSQLiteTestCustomSQL.CreateCustomSQLTables;
begin
  ExecDDL('CREATE TABLE TestCustomSQLContact (ID INTEGER PRIMARY KEY, Name TEXT, DeptID INTEGER)');
  ExecDDL('CREATE TABLE TestCustomSQLDept (ID INTEGER PRIMARY KEY, DeptName TEXT)');
end;

procedure TSQLiteTestCustomSQL.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestCustomSQL.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestExtendedTypesCRUD ===== }

procedure TSQLiteTestExtendedTypesCRUD.CreateExtendedTable;
begin ExecDDL('CREATE TABLE TestExtendedTypeContact (ID INTEGER PRIMARY KEY, Color INTEGER, Flags INTEGER, Extra TEXT)'); end;

procedure TSQLiteTestExtendedTypesCRUD.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestExtendedTypesCRUD.TearDown;
begin inherited TearDown; TearDownDatabase; end;

procedure TSQLiteTestExtendedTypesCRUD.TestSaveLoadSetEnumAsString;
var Obj, Loaded: TTestExtendedTypeContact;
    PD: TPersisterDef; FM: TOPFieldMap;
    T: TSQLTransaction; Q: TSQLQuery;
    Conn: TOPConnection;
begin
  PD:=OPRegistry.FindPersister(TTestExtendedTypeContact);
  FM:=PD.FieldMap.FindByPropertyName('Color'); FM.Options:=FM.Options + [foAsString];
  FM:=PD.FieldMap.FindByPropertyName('Flags'); FM.Options:=FM.Options + [foAsString];
  TearDown;
  SetUpDatabase(TSQLiteSetupHelper);
  ExecDDL('CREATE TABLE TestExtendedTypeContact (ID INTEGER PRIMARY KEY, Color TEXT, Flags TEXT, Extra TEXT)');
  OPRegistry.RegisterPersister(TTestExtendedTypeContact, TRTTIPersister);
  PD:=OPRegistry.FindPersister(TTestExtendedTypeContact);
  FM:=PD.FieldMap.FindByPropertyName('Color'); FM.Options:=FM.Options + [foAsString];
  FM:=PD.FieldMap.FindByPropertyName('Flags'); FM.Options:=FM.Options + [foAsString];
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.SingleConnection:=FConn; Conn.IsDefault:=True;
  Obj:=TTestExtendedTypeContact.Create;
  try
    Obj.ID:=1; Obj.Color:=ecBlue; Obj.Flags:=[efItalic]; Obj.Extra:=Integer(42);
    OPManager.Save(Obj);
  finally Obj.Free; end;
  Loaded:=TTestExtendedTypeContact.Create;
  try
    Loaded.ID:=1; Loaded.FState:=opsKey; OPManager.Load(Loaded);
    AssertEquals('Color as string round-trip', Ord(ecBlue), Ord(Loaded.Color));
    AssertTrue('efItalic in Flags', efItalic in Loaded.Flags);
    AssertFalse('efBold not in Flags', efBold in Loaded.Flags);
  finally Loaded.Free; end;
end;

{ ===== TSQLiteTestErrorDiagnostics ===== }

procedure TSQLiteTestErrorDiagnostics.CreateContactTable;
begin ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)'); end;

procedure TSQLiteTestErrorDiagnostics.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestErrorDiagnostics.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestNullableRoundTrip ===== }

procedure TSQLiteTestNullableRoundTrip.CreateNullableTable;
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

procedure TSQLiteTestNullableRoundTrip.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestNullableRoundTrip.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestIOPFNullableRoundTrip ===== }

procedure TSQLiteTestIOPFNullableRoundTrip.CreateIOPFNullableTable;
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

procedure TSQLiteTestIOPFNullableRoundTrip.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestIOPFNullableRoundTrip.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestSaveList ===== }

procedure TSQLiteTestSaveList.CreateSaveListTables;
begin
  ExecDDL('CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)');
end;

procedure TSQLiteTestSaveList.SetUp;
begin
  SetUpDatabase(TSQLiteSetupHelper);
  inherited SetUp;
end;

procedure TSQLiteTestSaveList.TearDown;
begin
  inherited TearDown;
  TearDownDatabase;
end;

{ ===== TSQLiteTestDDLExec ===== }

function TSQLiteTestDDLExec.GetDDLTableName: String; begin Result:='TestDDL'; end;
function TSQLiteTestDDLExec.GetManualTableName: String; begin Result:='TestManual'; end;
function TSQLiteTestDDLExec.GetManualDDL: String; begin Result:='CREATE TABLE TestManual (ID INTEGER PRIMARY KEY, Val TEXT)'; end;

procedure TSQLiteTestDDLExec.CleanupDDLTables;
begin DeleteTables(['TestDDL', 'TestManual']); end;

procedure TSQLiteTestDDLExec.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestDDLExec.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestM2MLoadAssociations ===== }

procedure TSQLiteTestM2MLoadAssociations.CreateM2MTables;
begin
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id))');
end;

procedure TSQLiteTestM2MLoadAssociations.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestM2MLoadAssociations.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestM2MAssociations ===== }

procedure TSQLiteTestM2MAssociations.CreateM2MTables;
begin
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id))');
end;

procedure TSQLiteTestM2MAssociations.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestM2MAssociations.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== TSQLiteTestM2MCascadeDelete ===== }

procedure TSQLiteTestM2MCascadeDelete.CreateM2MTables;
begin
  ExecDDL('CREATE TABLE TestStudent (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE TestCourse (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecDDL('CREATE TABLE student_course (student_id INTEGER, course_id INTEGER, PRIMARY KEY(student_id, course_id))');
  ExecDDL('CREATE TABLE TestClub (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecDDL('CREATE TABLE student_club (student_id INTEGER, club_id INTEGER, PRIMARY KEY(student_id, club_id))');
end;

procedure TSQLiteTestM2MCascadeDelete.SetUp;
begin SetUpDatabase(TSQLiteSetupHelper); inherited SetUp; end;

procedure TSQLiteTestM2MCascadeDelete.TearDown;
begin inherited TearDown; TearDownDatabase; end;

{ ===== Optimistic locking fixture classes ===== }

const
  SDDLVerInt = 'CREATE TABLE TestVerInt (ID INTEGER PRIMARY KEY, Title TEXT, Content TEXT, Version INTEGER)';
  SDDLVerInt64 = 'CREATE TABLE TestVerInt64 (ID INTEGER PRIMARY KEY, Content TEXT, Version BIGINT)';
  SDDLVerStr = 'CREATE TABLE TestVerStr (ID INTEGER PRIMARY KEY, Content TEXT, Version TEXT)';
  SDDLVerShort = 'CREATE TABLE TestVerShort (ID INTEGER PRIMARY KEY, Content TEXT, Version TEXT)';
  SDDLVerUni = 'CREATE TABLE TestVerUni (ID INTEGER PRIMARY KEY, Content TEXT, Version TEXT)';
  SDDLVerNone = 'CREATE TABLE TestVerNone (ID INTEGER PRIMARY KEY, Content TEXT)';

function TTestVerInt.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TTestVerInt.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TTestVerInt64.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TTestVerInt64.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TTestVerStr.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TTestVerStr.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TTestVerShort.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TTestVerShort.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TTestVerUni.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TTestVerUni.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


function TTestVerNone.GetPersistenceState: TObjectPersistenceState;

begin
  Result:=FState;
end;


procedure TTestVerNone.SetPersistenceState(aValue: TObjectPersistenceState);

begin
  FState:=aValue;
end;


{ ===== TTestVersionConcurrency ===== }

procedure TTestVersionConcurrency.SetUp;
var
  lConn: TOPConnection;
begin
  inherited SetUp;
  FConn:=TSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(FConn);
  FConn.Transaction:=FTrans;
  FConn.Open;
  OPSQLBuilder.SQLDialect:=dSQLite;
  TOPManager.ClearInstance;
  lConn:=OPManager.Connections.Add as TOPConnection;
  lConn.SingleConnection:=FConn;
  lConn.IsDefault:=True;
end;


procedure TTestVersionConcurrency.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  TOPListRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  FTrans:=Nil;
  if Assigned(FConn) then
    begin
    if FConn.Connected then
      FConn.Close;
    FreeAndNil(FConn);
    end;
  inherited TearDown;
end;


procedure TTestVersionConcurrency.ExecDirect(const aSQL: UTF8String);
var
  lQuery: TSQLQuery;
begin
  lQuery:=TSQLQuery.Create(Nil);
  try
    lQuery.Database:=FConn;
    lQuery.Transaction:=FTrans;
    lQuery.SQL.Text:=aSQL;
    if not FTrans.Active then
      FTrans.StartTransaction;
    lQuery.ExecSQL;
    FTrans.Commit;
  finally
    lQuery.Free;
  end;
end;


function TTestVersionConcurrency.ReadString(const aSQL: UTF8String): String;
var
  lQuery: TSQLQuery;
begin
  lQuery:=TSQLQuery.Create(Nil);
  try
    lQuery.Database:=FConn;
    lQuery.Transaction:=FTrans;
    lQuery.SQL.Text:=aSQL;
    if not FTrans.Active then
      FTrans.StartTransaction;
    lQuery.Open;
    try
      AssertFalse('No row returned by '+aSQL, lQuery.EOF);
      Result:=lQuery.Fields[0].AsString;
    finally
      lQuery.Close;
      FTrans.Commit;
    end;
  finally
    lQuery.Free;
  end;
end;


function TTestVersionConcurrency.ReadInt64(const aSQL: UTF8String): Int64;
var
  lQuery: TSQLQuery;
begin
  lQuery:=TSQLQuery.Create(Nil);
  try
    lQuery.Database:=FConn;
    lQuery.Transaction:=FTrans;
    lQuery.SQL.Text:=aSQL;
    if not FTrans.Active then
      FTrans.StartTransaction;
    lQuery.Open;
    try
      AssertFalse('No row returned by '+aSQL, lQuery.EOF);
      Result:=lQuery.Fields[0].AsLargeInt;
    finally
      lQuery.Close;
      FTrans.Commit;
    end;
  finally
    lQuery.Free;
  end;
end;


procedure TTestVersionConcurrency.RegisterFixture(aClass: TClass;
  const aDDL: UTF8String; aVersioned: Boolean);
var
  lDef: TPersisterDef;
  lField: TOPFieldMap;
begin
  lDef:=OPRegistry.RegisterPersister(aClass, TRTTIPersister);
  if aVersioned then
    begin
    lField:=lDef.FieldMap.FindByPropertyName('Version');
    AssertNotNull('Version field mapped for '+aClass.ClassName, lField);
    lField.Options:=lField.Options+[foVersion];
    end;
  ExecDirect(aDDL);
end;


procedure TTestVersionConcurrency.TestIntegerVersion;
var
  lObj: TTestVerInt;
begin
  RegisterFixture(TTestVerInt, SDDLVerInt, True);
  lObj:=TTestVerInt.Create;
  try
    lObj.ID:=1;
    lObj.Title:='Doc';
    lObj.Content:='First';
    lObj.Version:=3;
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  AssertEquals('Version stored on insert', Int64(3), ReadInt64('SELECT Version FROM TestVerInt WHERE ID=1'));
  lObj:=TTestVerInt.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    AssertEquals('Version loaded', 3, lObj.Version);
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    OPManager.Save(lObj);
    AssertEquals('Version incremented', 4, lObj.Version);
  finally
    lObj.Free;
  end;
  AssertEquals('Version stored on update', Int64(4), ReadInt64('SELECT Version FROM TestVerInt WHERE ID=1'));
  AssertEquals('Content stored on update', 'Second', ReadString('SELECT Content FROM TestVerInt WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestInt64Version;
const
  BigVersion = Int64(5000000000);
var
  lObj: TTestVerInt64;
begin
  RegisterFixture(TTestVerInt64, SDDLVerInt64, True);
  lObj:=TTestVerInt64.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.Version:=BigVersion;
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  AssertEquals('Version stored on insert', BigVersion, ReadInt64('SELECT Version FROM TestVerInt64 WHERE ID=1'));
  lObj:=TTestVerInt64.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    AssertEquals('Version loaded without truncation', BigVersion, lObj.Version);
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    OPManager.Save(lObj);
    AssertEquals('Version incremented', BigVersion+1, lObj.Version);
  finally
    lObj.Free;
  end;
  AssertEquals('Version stored on update', BigVersion+1, ReadInt64('SELECT Version FROM TestVerInt64 WHERE ID=1'));
  AssertEquals('Content stored on update', 'Second', ReadString('SELECT Content FROM TestVerInt64 WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestUUIDVersion;
var
  lObj: TTestVerStr;
  lFirst, lSecond: String;
begin
  RegisterFixture(TTestVerStr, SDDLVerStr, True);
  lObj:=TTestVerStr.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
    lFirst:=lObj.Version;
  finally
    lObj.Free;
  end;
  AssertEquals('UUID generated on insert', 36, Length(lFirst));
  AssertEquals('UUID stored on insert', lFirst, ReadString('SELECT Version FROM TestVerStr WHERE ID=1'));
  lObj:=TTestVerStr.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    AssertEquals('UUID loaded', lFirst, lObj.Version);
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    OPManager.Save(lObj);
    lSecond:=lObj.Version;
  finally
    lObj.Free;
  end;
  AssertEquals('Fresh UUID generated on update', 36, Length(lSecond));
  AssertTrue('UUID changed on update', lFirst<>lSecond);
  AssertEquals('UUID stored on update', lSecond, ReadString('SELECT Version FROM TestVerStr WHERE ID=1'));
  AssertEquals('Content stored on update', 'Second', ReadString('SELECT Content FROM TestVerStr WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestShortStringVersion;
var
  lObj: TTestVerShort;
  lFirst, lSecond: String;
begin
  RegisterFixture(TTestVerShort, SDDLVerShort, True);
  lObj:=TTestVerShort.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
    lFirst:=lObj.Version;
  finally
    lObj.Free;
  end;
  AssertEquals('UUID generated on insert', 36, Length(lFirst));
  AssertEquals('UUID stored on insert', lFirst, ReadString('SELECT Version FROM TestVerShort WHERE ID=1'));
  lObj:=TTestVerShort.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    AssertEquals('UUID loaded', lFirst, String(lObj.Version));
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    OPManager.Save(lObj);
    lSecond:=lObj.Version;
  finally
    lObj.Free;
  end;
  AssertEquals('Fresh UUID generated on update', 36, Length(lSecond));
  AssertTrue('UUID changed on update', lFirst<>lSecond);
  AssertEquals('UUID stored on update', lSecond, ReadString('SELECT Version FROM TestVerShort WHERE ID=1'));
  AssertEquals('Content stored on update', 'Second', ReadString('SELECT Content FROM TestVerShort WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestConcurrentModification;
var
  lObj: TTestVerInt;
  lRaised: Boolean;
  lMessage: String;
begin
  RegisterFixture(TTestVerInt, SDDLVerInt, True);
  lObj:=TTestVerInt.Create;
  try
    lObj.ID:=1;
    lObj.Title:='Doc';
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  lRaised:=False;
  lMessage:='';
  lObj:=TTestVerInt.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    { direct SQL outside the ORM changes the row and its version }
    ExecDirect('UPDATE TestVerInt SET Content=''Outside'', Version=99 WHERE ID=1');
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    try
      OPManager.Save(lObj);
    except
      on E: EOPF do
        begin
        lRaised:=True;
        lMessage:=E.Message;
        end;
    end;
  finally
    lObj.Free;
  end;
  AssertTrue('EOPF raised on stale version', lRaised);
  AssertTrue('Concurrency conflict reported: '+lMessage,
    Pos(Format(SErrConcurrencyConflict, ['TTestVerInt']), lMessage)>0);
  AssertEquals('Row untouched by conflicting update', 'Outside',
    ReadString('SELECT Content FROM TestVerInt WHERE ID=1'));
  AssertEquals('Version untouched by conflicting update', Int64(99),
    ReadInt64('SELECT Version FROM TestVerInt WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestUnicodeStringVersion;
var
  lObj: TTestVerUni;
  lFirst, lSecond: UnicodeString;
begin
  RegisterFixture(TTestVerUni, SDDLVerUni, True);
  lObj:=TTestVerUni.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
    lFirst:=lObj.Version;
  finally
    lObj.Free;
  end;
  AssertEquals('UUID generated on insert', 36, Length(lFirst));
  AssertEquals('UUID stored on insert', String(lFirst), ReadString('SELECT Version FROM TestVerUni WHERE ID=1'));
  lObj:=TTestVerUni.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    AssertEquals('UUID loaded', String(lFirst), String(lObj.Version));
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    OPManager.Save(lObj);
    lSecond:=lObj.Version;
  finally
    lObj.Free;
  end;
  AssertEquals('Fresh UUID generated on update', 36, Length(lSecond));
  AssertTrue('UUID changed on update', lFirst<>lSecond);
  AssertEquals('UUID stored on update', String(lSecond), ReadString('SELECT Version FROM TestVerUni WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestConcurrentModificationUnicodeString;
var
  lObj: TTestVerUni;
  lRaised: Boolean;
begin
  RegisterFixture(TTestVerUni, SDDLVerUni, True);
  lObj:=TTestVerUni.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  lRaised:=False;
  lObj:=TTestVerUni.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    ExecDirect('UPDATE TestVerUni SET Content=''Outside'', Version=''outside-version'' WHERE ID=1');
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    try
      OPManager.Save(lObj);
    except
      on E: EOPF do
        lRaised:=True;
    end;
  finally
    lObj.Free;
  end;
  AssertTrue('EOPF raised on stale version', lRaised);
  AssertEquals('Row untouched by conflicting update', 'Outside',
    ReadString('SELECT Content FROM TestVerUni WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestConcurrentModificationString;
var
  lObj: TTestVerStr;
  lRaised: Boolean;
  lMessage: String;
begin
  RegisterFixture(TTestVerStr, SDDLVerStr, True);
  lObj:=TTestVerStr.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  lRaised:=False;
  lMessage:='';
  lObj:=TTestVerStr.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    { direct SQL outside the ORM changes the row and its version }
    ExecDirect('UPDATE TestVerStr SET Content=''Outside'', Version=''outside-version'' WHERE ID=1');
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    try
      OPManager.Save(lObj);
    except
      on E: EOPF do
        begin
        lRaised:=True;
        lMessage:=E.Message;
        end;
    end;
  finally
    lObj.Free;
  end;
  AssertTrue('EOPF raised on stale version', lRaised);
  AssertTrue('Concurrency conflict reported: '+lMessage,
    Pos(Format(SErrConcurrencyConflict, ['TTestVerStr']), lMessage)>0);
  AssertEquals('Row untouched by conflicting update', 'Outside',
    ReadString('SELECT Content FROM TestVerStr WHERE ID=1'));
  AssertEquals('Version untouched by conflicting update', 'outside-version',
    ReadString('SELECT Version FROM TestVerStr WHERE ID=1'));
end;


procedure TTestVersionConcurrency.TestPartialUpdateWithVersionField;
var
  lTracker: TOPSnapshotManager;
  lObj: TTestVerInt;
begin
  RegisterFixture(TTestVerInt, SDDLVerInt, True);
  lTracker:=TOPSnapshotManager.Create(Nil);
  try
    lTracker.Manager:=OPManager;
    lTracker.Mode:=smExplicit;
    lTracker.RegisterClass(TTestVerInt);
    AssertTrue('Tracker attached to the manager under test',
      Assigned(OPManager.ChangeTracker));
    lObj:=TTestVerInt.Create;
    try
      lObj.ID:=1;
      lObj.Title:='Doc';
      lObj.Content:='First';
      lObj.Version:=3;
      lObj.FState:=opsNew;
      OPManager.Save(lObj);
    finally
      lObj.Free;
    end;
    lObj:=TTestVerInt.Create;
    try
      lObj.ID:=1;
      lObj.FState:=opsKey;
      OPManager.Load(lObj);
      { Title is not in the reduced field map, so a partial update leaves it alone }
      ExecDirect('UPDATE TestVerInt SET Title=''Outside'' WHERE ID=1');
      lObj.Content:='Second';
      lObj.FState:=opsUpdate;
      OPManager.Save(lObj);
      AssertEquals('Version incremented', 4, lObj.Version);
    finally
      lObj.Free;
    end;
    AssertEquals('Title left out of the partial update', 'Outside',
      ReadString('SELECT Title FROM TestVerInt WHERE ID=1'));
    AssertEquals('Content stored on update', 'Second',
      ReadString('SELECT Content FROM TestVerInt WHERE ID=1'));
    AssertEquals('Version stored on update', Int64(4),
      ReadInt64('SELECT Version FROM TestVerInt WHERE ID=1'));
  finally
    lTracker.Free;
  end;
end;


procedure TTestVersionConcurrency.TestRollbackForcesFullUpdate;
var
  lTracker: TOPSnapshotManager;
  lObj: TTestVerInt;
begin
  RegisterFixture(TTestVerInt, SDDLVerInt, False);
  lTracker:=TOPSnapshotManager.Create(Nil);
  try
    lTracker.Mode:=smExplicit;
    lTracker.RegisterClass(TTestVerInt);
    AssertTrue('Tracker attached to the manager under test',
      Assigned(OPManager.ChangeTracker));
    lObj:=TTestVerInt.Create;
    try
      lObj.ID:=1;
      lObj.Title:='Doc';
      lObj.Content:='First';
      lObj.FState:=opsNew;
      OPManager.Save(lObj);
      OPManager.BeginTransaction;
      lObj.Content:='Second';
      lObj.FState:=opsUpdate;
      OPManager.Save(lObj);
      OPManager.Rollback;
      { Changes a column behind the ORM's back. }
      ExecDirect('UPDATE TestVerInt SET Title=''Outside'' WHERE ID=1');
      lObj.FState:=opsUpdate;
      OPManager.Save(lObj);
    finally
      lObj.Free;
    end;
    AssertEquals('Unchanged column carried by the full update', 'Doc',
      ReadString('SELECT Title FROM TestVerInt WHERE ID=1'));
    AssertEquals('Changed column reaches the database', 'Second',
      ReadString('SELECT Content FROM TestVerInt WHERE ID=1'));
  finally
    lTracker.Free;
  end;
end;


procedure TTestVersionConcurrency.TestNoVersionField;
var
  lObj: TTestVerNone;
  lPlain: TTestVerInt;
begin
  RegisterFixture(TTestVerNone, SDDLVerNone, False);
  lObj:=TTestVerNone.Create;
  try
    lObj.ID:=1;
    lObj.Content:='First';
    lObj.FState:=opsNew;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  lObj:=TTestVerNone.Create;
  try
    lObj.ID:=1;
    lObj.FState:=opsKey;
    OPManager.Load(lObj);
    lObj.Content:='Second';
    lObj.FState:=opsUpdate;
    OPManager.Save(lObj);
  finally
    lObj.Free;
  end;
  AssertEquals('Plain update ran', 'Second', ReadString('SELECT Content FROM TestVerNone WHERE ID=1'));
  { a class that has a Version property but no foVersion option takes the same plain path }
  RegisterFixture(TTestVerInt, SDDLVerInt, False);
  lPlain:=TTestVerInt.Create;
  try
    lPlain.ID:=1;
    lPlain.Title:='Doc';
    lPlain.Content:='First';
    lPlain.Version:=3;
    lPlain.FState:=opsNew;
    OPManager.Save(lPlain);
  finally
    lPlain.Free;
  end;
  lPlain:=TTestVerInt.Create;
  try
    lPlain.ID:=1;
    lPlain.FState:=opsKey;
    OPManager.Load(lPlain);
    lPlain.Content:='Second';
    lPlain.FState:=opsUpdate;
    OPManager.Save(lPlain);
    AssertEquals('Version not incremented without foVersion', 3, lPlain.Version);
  finally
    lPlain.Free;
  end;
  AssertEquals('Version column not incremented without foVersion', Int64(3),
    ReadInt64('SELECT Version FROM TestVerInt WHERE ID=1'));
end;


{ ===== TTestBackendSwitch ===== }

procedure TTestBackendSwitch.CreateContactTable(aConn: TSQLite3Connection);
var Q: TSQLQuery; T: TSQLTransaction;
begin
  T:=TSQLTransaction.Create(Nil);
  try
    T.Database:=aConn; aConn.Transaction:=T;
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=aConn; Q.Transaction:=T;
      Q.SQL.Text:='CREATE TABLE TestMgrSaveContact (ID INTEGER PRIMARY KEY, Name TEXT)';
      T.StartTransaction; Q.ExecSQL; T.Commit;
    finally Q.Free; end;
  finally aConn.Transaction:=Nil; T.Free; end;
end;

procedure TTestBackendSwitch.SetUp;
var Conn: TOPConnection;
begin
  inherited SetUp;
  FConnA:=TSQLite3Connection.Create(Nil); FConnA.DatabaseName:=':memory:'; FConnA.Open;
  FConnB:=TSQLite3Connection.Create(Nil); FConnB.DatabaseName:=':memory:'; FConnB.Open;
  OPRegistry.RegisterPersister(TTestMgrSaveContact, TRTTIPersister);
  OPSQLBuilder.SQLDialect:=dSQLite;
  TOPManager.ClearInstance;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.Name:='BackendA'; Conn.SingleConnection:=FConnA; Conn.IsDefault:=True;
  Conn:=OPManager.Connections.Add as TOPConnection;
  Conn.Name:='BackendB'; Conn.SingleConnection:=FConnB;
  CreateContactTable(FConnA); CreateContactTable(FConnB);
end;

procedure TTestBackendSwitch.TearDown;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  TOPListRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  if Assigned(FConnA) then begin if FConnA.Connected then FConnA.Close; FreeAndNil(FConnA); end;
  if Assigned(FConnB) then begin if FConnB.Connected then FConnB.Close; FreeAndNil(FConnB); end;
  inherited TearDown;
end;

procedure TTestBackendSwitch.TestSwitchConnectionCRUD;
var Obj, Loaded: TTestMgrSaveContact; Raised: Boolean;
begin
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=1; Obj.Name:='Alice'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  (OPManager.Connections[1] as TOPConnection).IsDefault:=True;
  Obj:=TTestMgrSaveContact.Create;
  try Obj.ID:=2; Obj.Name:='Bob'; Obj.FState:=opsNew; OPManager.Save(Obj); finally Obj.Free; end;
  Loaded:=TTestMgrSaveContact.Create;
  try
    Loaded.ID:=2; Loaded.FState:=opsKey; OPManager.Load(Loaded);
    AssertEquals('Loaded from B', 'Bob', Loaded.Name);
  finally Loaded.Free; end;
  Loaded:=TTestMgrSaveContact.Create;
  try
    Loaded.ID:=1; Loaded.FState:=opsKey;
    Raised:=False;
    try OPManager.Load(Loaded); except on E: EOPF do Raised:=True; end;
    AssertTrue('Should not find Alice on backend B', Raised);
  finally Loaded.Free; end;
  (OPManager.Connections[0] as TOPConnection).IsDefault:=True;
  Loaded:=TTestMgrSaveContact.Create;
  try
    Loaded.ID:=1; Loaded.FState:=opsKey; OPManager.Load(Loaded);
    AssertEquals('Loaded from A', 'Alice', Loaded.Name);
  finally Loaded.Free; end;
end;

initialization

  RegisterTests('SQLite',[TSQLiteTestIntegration,
    TSQLiteTestManagerSave,
    TSQLiteTestManagerDelete,
    TSQLiteTestCascadeDelete,
    TSQLiteTestManagerLoad,
    TSQLiteTestMappedCRUD,
    TSQLiteTestJsonBlobCRUD,
    TSQLiteTestManagerLoadList,
    TSQLiteTestErrorDiagnostics,
    TSQLiteTestAutoInc,
    TSQLiteTestUUID,
    TSQLiteTestCustomGen,
    TSQLiteTestLoadChildren,
    TSQLiteTestLazyLoadChildren,
    TSQLiteTestLazyM2M,
    TSQLiteTestInterfacePersister,
    TSQLiteTestExplicitTransaction,
    TSQLiteTestCustomSQL,
    TTestBackendSwitch,
    TTestVersionConcurrency,
    TSQLiteTestExtendedTypesCRUD,
    TSQLiteTestNullableRoundTrip,
    TSQLiteTestIOPFNullableRoundTrip,
    TSQLiteTestM2MLoadAssociations,
    TSQLiteTestM2MAssociations,
    TSQLiteTestM2MCascadeDelete,
    TSQLiteTestSaveList,
    TSQLiteTestDDLExec]);

end.
