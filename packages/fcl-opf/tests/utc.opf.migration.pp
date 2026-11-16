unit utc.opf.migration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, db, sqldb, sqlite3conn,
  fpcunit, testregistry,
  Opf.Migration, Opf.Manager, Opf.Registry, Opf.Fields, Opf.Relations, Opf.M2M,
  Opf.DDL, Opf.Schema;

type

  { TSQLiteMigrationTest }

  TSQLiteMigrationTest = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FTracker: TOPMigrationTracker;
    procedure ExecSQL(const aSQL: String);
    function QueryInt(const aSQL: String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHasMigrationTableFalse;
    procedure TestEnsureMigrationTable;
    procedure TestEnsureMigrationTableIdempotent;
    procedure TestGetCurrentVersionEmpty;
    procedure TestGetCurrentVersionNoTable;
    procedure TestRecordMigration;
    procedure TestGetCurrentVersionAfterRecord;
    procedure TestRecordMigrationMultiple;
    procedure TestGetMigrationHistory;
  end;

  { TSQLiteMigrateSchemaTest }

  { TTestMigrationHelper }

  TTestMigrationHelper = class(TObject)
  private
    FExecuted: Boolean;
    FExecutionOrder: Integer;
    FOrderCounter: PInteger;
    FRaiseException: Boolean;
  public
    constructor Create;
    procedure StepInsertRow(aConnection: TSQLConnection; aVersion: Integer);
    procedure StepTrackOrder(aConnection: TSQLConnection; aVersion: Integer);
    procedure StepRaise(aConnection: TSQLConnection; aVersion: Integer);
    property Executed: Boolean read FExecuted;
    property ExecutionOrder: Integer read FExecutionOrder;
    property OrderCounter: PInteger read FOrderCounter write FOrderCounter;
  end;

  { TSQLiteMigrateSchemaTest }

  TSQLiteMigrateSchemaTest = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FManager: TOPManager;
    FRegistry: TOPRegistry;
    FRelReg: TOPRelationRegistry;
    FM2MReg: TOPManyToManyRegistry;
    procedure ExecSQL(const aSQL: String);
    function QueryInt(const aSQL: String): Integer;
    function QueryStr(const aSQL: String): String;
    function ColumnExists(const aTable, aColumn: String): Boolean;
    function TableExists(const aTable: String): Boolean;
    procedure RegisterSimpleTable(const aTableName: UTF8String;
      const aFieldNames: array of UTF8String;
      const aFieldKinds: array of TTypeKind);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMigrateSchemaNoChanges;
    procedure TestMigrateSchemaAddColumn;
    procedure TestMigrateSchemaDropColumnFiltered;
    procedure TestMigrateSchemaDropColumnAllowed;
    procedure TestMigrateSchemaDropTableFiltered;
    procedure TestMigrateSchemaDryRun;
    procedure TestMigrateSchemaVersionTracking;
    procedure TestCreateSchemaRecordsVersion;
    // Custom migration step tests
    procedure TestRegisterMigration;
    procedure TestCustomStepExecutes;
    procedure TestCustomStepOrder;
    procedure TestCustomStepNoMatchSkipped;
    procedure TestCustomStepDryRunSkipped;
    procedure TestCustomStepWithDDL;
    procedure TestCustomStepExceptionRollback;
  end;

implementation

{ TSQLiteMigrationTest }

procedure TSQLiteMigrationTest.ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := aSQL;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TSQLiteMigrationTest.QueryInt(const aSQL: String): Integer;
var
  Q: TSQLQuery;
begin
  Result := 0;
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := aSQL;
    Q.Open;
    if (not Q.EOF) and (not Q.Fields[0].IsNull) then
      Result := Q.Fields[0].AsInteger;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TSQLiteMigrationTest.SetUp;
begin
  FConn := TSQLite3Connection.Create(nil);
  FTrans := TSQLTransaction.Create(nil);
  FConn.DatabaseName := ':memory:';
  FConn.Transaction := FTrans;
  FConn.Open;
  FTracker := TOPMigrationTracker.Create;
  FTracker.Connection := FConn;
end;

procedure TSQLiteMigrationTest.TearDown;
begin
  FreeAndNil(FTracker);
  if Assigned(FTrans) and FTrans.Active then
    FTrans.Rollback;
  if Assigned(FConn) and FConn.Connected then
    FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
end;

{ Tests }

procedure TSQLiteMigrationTest.TestHasMigrationTableFalse;
begin
  AssertFalse('Empty database should not have migration table',
    FTracker.HasMigrationTable);
end;

procedure TSQLiteMigrationTest.TestEnsureMigrationTable;
begin
  FTracker.EnsureMigrationTable;
  AssertTrue('Migration table should exist after EnsureMigrationTable',
    FTracker.HasMigrationTable);
end;

procedure TSQLiteMigrationTest.TestEnsureMigrationTableIdempotent;
begin
  FTracker.EnsureMigrationTable;
  FTracker.EnsureMigrationTable;
  AssertTrue('Migration table should exist after calling EnsureMigrationTable twice',
    FTracker.HasMigrationTable);
end;

procedure TSQLiteMigrationTest.TestGetCurrentVersionEmpty;
begin
  FTracker.EnsureMigrationTable;
  AssertEquals('Empty migration table should return version 0',
    0, FTracker.GetCurrentVersion);
end;

procedure TSQLiteMigrationTest.TestGetCurrentVersionNoTable;
begin
  AssertEquals('No migration table should return version 0',
    0, FTracker.GetCurrentVersion);
end;

procedure TSQLiteMigrationTest.TestRecordMigration;
var
  Count: Integer;
begin
  FTracker.EnsureMigrationTable;
  FTracker.RecordMigration(1, 'Initial schema');
  FTrans.Commit;
  Count := QueryInt('SELECT COUNT(*) FROM ' + cMigrationTable);
  AssertEquals('Should have 1 migration record', 1, Count);
  AssertEquals('Version should be 1',
    1, QueryInt('SELECT Version FROM ' + cMigrationTable));
end;

procedure TSQLiteMigrationTest.TestGetCurrentVersionAfterRecord;
begin
  FTracker.EnsureMigrationTable;
  FTracker.RecordMigration(1, 'v1');
  FTrans.Commit;
  FTracker.RecordMigration(3, 'v3');
  FTrans.Commit;
  AssertEquals('Current version should be 3 (max)',
    3, FTracker.GetCurrentVersion);
end;

procedure TSQLiteMigrationTest.TestRecordMigrationMultiple;
var
  Count: Integer;
begin
  FTracker.EnsureMigrationTable;
  FTracker.RecordMigration(1, 'First');
  FTracker.RecordMigration(2, 'Second');
  FTracker.RecordMigration(3, 'Third');
  FTrans.Commit;
  Count := QueryInt('SELECT COUNT(*) FROM ' + cMigrationTable);
  AssertEquals('Should have 3 migration records', 3, Count);
end;

procedure TSQLiteMigrationTest.TestGetMigrationHistory;
var
  History: TOPMigrationArray;
begin
  FTracker.EnsureMigrationTable;
  FTracker.RecordMigration(1, 'First migration');
  FTracker.RecordMigration(2, 'Second migration');
  FTracker.RecordMigration(3, 'Third migration');
  FTrans.Commit;
  History := FTracker.GetMigrationHistory;
  AssertEquals('History should have 3 entries', 3, Length(History));
  AssertEquals('First entry version', 1, History[0].Version);
  AssertEquals('First entry description', 'First migration', History[0].Description);
  AssertTrue('First entry should have AppliedAt', History[0].AppliedAt <> '');
  AssertEquals('Second entry version', 2, History[1].Version);
  AssertEquals('Second entry description', 'Second migration', History[1].Description);
  AssertEquals('Third entry version', 3, History[2].Version);
  AssertEquals('Third entry description', 'Third migration', History[2].Description);
end;

{ TSQLiteMigrateSchemaTest }

procedure TSQLiteMigrateSchemaTest.ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  Trans:=TSQLTransaction.Create(nil);
  try
    Trans.Database:=FConn;
    Q:=TSQLQuery.Create(nil);
    try
      Q.Database:=FConn;
      Q.Transaction:=Trans;
      Trans.StartTransaction;
      Q.SQL.Text:=aSQL;
      Q.ExecSQL;
      Trans.Commit;
    finally
      Q.Free;
    end;
  finally
    Trans.Free;
  end;
end;

function TSQLiteMigrateSchemaTest.QueryInt(const aSQL: String): Integer;
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  Result:=0;
  Trans:=TSQLTransaction.Create(nil);
  try
    Trans.Database:=FConn;
    Q:=TSQLQuery.Create(nil);
    try
      Q.Database:=FConn;
      Q.Transaction:=Trans;
      Trans.StartTransaction;
      Q.SQL.Text:=aSQL;
      Q.Open;
      if (not Q.EOF) and (not Q.Fields[0].IsNull) then
        Result:=Q.Fields[0].AsInteger;
      Q.Close;
      Trans.Commit;
    finally
      Q.Free;
    end;
  finally
    if Trans.Active then
      Trans.Rollback;
    Trans.Free;
  end;
end;

function TSQLiteMigrateSchemaTest.QueryStr(const aSQL: String): String;
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  Result:='';
  Trans:=TSQLTransaction.Create(nil);
  try
    Trans.Database:=FConn;
    Q:=TSQLQuery.Create(nil);
    try
      Q.Database:=FConn;
      Q.Transaction:=Trans;
      Trans.StartTransaction;
      Q.SQL.Text:=aSQL;
      Q.Open;
      if (not Q.EOF) and (not Q.Fields[0].IsNull) then
        Result:=Q.Fields[0].AsString;
      Q.Close;
      Trans.Commit;
    finally
      Q.Free;
    end;
  finally
    if Trans.Active then
      Trans.Rollback;
    Trans.Free;
  end;
end;

function TSQLiteMigrateSchemaTest.ColumnExists(const aTable, aColumn: String): Boolean;
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  Result:=False;
  Trans:=TSQLTransaction.Create(nil);
  try
    Trans.Database:=FConn;
    Q:=TSQLQuery.Create(nil);
    try
      Q.Database:=FConn;
      Q.Transaction:=Trans;
      Trans.StartTransaction;
      Q.SQL.Text:='PRAGMA table_info(' + aTable + ')';
      Q.Open;
      while not Q.EOF do
      begin
        if SameText(Q.FieldByName('name').AsString, aColumn) then
        begin
          Result:=True;
          Break;
        end;
        Q.Next;
      end;
      Q.Close;
      Trans.Commit;
    finally
      Q.Free;
    end;
  finally
    if Trans.Active then
      Trans.Rollback;
    Trans.Free;
  end;
end;

function TSQLiteMigrateSchemaTest.TableExists(const aTable: String): Boolean;
begin
  Result:=QueryInt('SELECT COUNT(*) FROM sqlite_master WHERE type=''table'' AND name=''' + aTable + '''') > 0;
end;

procedure TSQLiteMigrateSchemaTest.RegisterSimpleTable(const aTableName: UTF8String;
  const aFieldNames: array of UTF8String;
  const aFieldKinds: array of TTypeKind);
var
  I: Integer;
  PD: TPersisterDef;
  FM: TOPFieldMap;
begin
  PD:=FRegistry.RegisterPersister(TObject, nil);
  PD.TableName:=aTableName;
  PD.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  for I:=0 to High(aFieldNames) do
  begin
    FM:=TOPFieldMap(PD.FieldMap.Add);
    FM.FieldName:=aFieldNames[I];
    FM.PropertyName:=aFieldNames[I];
    FM.PropertyKind:=aFieldKinds[I];
    if I = 0 then
      FM.Options:=[foInKey, foInSelect]
    else
      FM.Options:=[foInSelect, foInInsert, foInUpdate];
  end;
end;

procedure TSQLiteMigrateSchemaTest.SetUp;
begin
  FConn:=TSQLite3Connection.Create(nil);
  FTrans:=TSQLTransaction.Create(nil);
  FConn.DatabaseName:=':memory:';
  FConn.Transaction:=FTrans;
  FConn.Open;
  FRegistry:=TOPRegistry.Create(nil);
  FRelReg:=TOPRelationRegistry.Create(nil);
  FRelReg.Registry:=FRegistry;
  FM2MReg:=TOPManyToManyRegistry.Create(nil);
  FM2MReg.Registry:=FRegistry;
  FManager:=TOPManager.Create(nil);
  FManager.Registry:=FRegistry;
  FManager.RelationRegistry:=FRelReg;
  FManager.M2MRegistry:=FM2MReg;
end;

procedure TSQLiteMigrateSchemaTest.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FM2MReg);
  FreeAndNil(FRelReg);
  FreeAndNil(FRegistry);
  if Assigned(FTrans) and FTrans.Active then
    FTrans.Rollback;
  if Assigned(FConn) and FConn.Connected then
    FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
end;

{ TSQLiteMigrateSchemaTest - Tests }

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaNoChanges;
var
  Res: TOPMigrationResult;
begin
  // Register a simple table and create schema
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  FManager.CreateSchema(FConn);
  // Migrate same model — should find 0 diffs
  Res:=FManager.MigrateSchema(FConn, 2);
  AssertEquals('No diffs expected', 0, Res.DiffsFound);
  AssertEquals('No statements executed', 0, Res.StatementsExecuted);
  AssertEquals('Version before should be 1', 1, Res.VersionBefore);
  AssertEquals('Version after should be 2', 2, Res.VersionAfter);
end;

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaAddColumn;
var
  Res: TOPMigrationResult;
  PD: TPersisterDef;
  FM: TOPFieldMap;
begin
  // Register and create v1 schema
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  FManager.CreateSchema(FConn);
  // Add a field to simulate model evolution
  PD:=FRegistry.FindPersister(TObject);
  FM:=TOPFieldMap(PD.FieldMap.Add);
  FM.FieldName:='Age';
  FM.PropertyName:='Age';
  FM.PropertyKind:=tkInteger;
  FM.Options:=[foInSelect, foInInsert, foInUpdate];
  // Migrate to v2
  Res:=FManager.MigrateSchema(FConn, 2);
  AssertTrue('Should find diffs', Res.DiffsFound > 0);
  AssertTrue('Should execute statements', Res.StatementsExecuted > 0);
  // Verify column was added
  AssertTrue('Age column should exist', ColumnExists('Person', 'Age'));
end;

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaDropColumnFiltered;
var
  Res: TOPMigrationResult;
begin
  // Register a table with just ID and Name
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  // Create schema via manager (creates proper types), then add extra column
  FManager.CreateSchema(FConn);
  ExecSQL('ALTER TABLE Person ADD COLUMN ExtraCol TEXT');
  // Migrate to v2 — sdkDropColumn should be filtered out by default
  Res:=FManager.MigrateSchema(FConn, 2);
  AssertTrue('Should detect diffs (drop column)', Res.DiffsFound > 0);
  // ExtraCol should still exist (filtered out)
  AssertTrue('ExtraCol should still exist (filtered)', ColumnExists('Person', 'ExtraCol'));
end;

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaDropColumnAllowed;
var
  Res: TOPMigrationResult;
begin
  // Register a table with just ID and Name
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  // Create schema via manager, then add extra column
  FManager.CreateSchema(FConn);
  ExecSQL('ALTER TABLE Person ADD COLUMN ExtraCol TEXT');
  // Migrate with moAllowDropColumn — column should be dropped
  Res:=FManager.MigrateSchema(FConn, 2, [moAllowDropColumn]);
  AssertTrue('Should detect diffs', Res.DiffsFound > 0);
  AssertTrue('Should execute statements', Res.StatementsExecuted > 0);
  // ExtraCol should be gone
  AssertFalse('ExtraCol should be dropped', ColumnExists('Person', 'ExtraCol'));
end;

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaDropTableFiltered;
var
  Res: TOPMigrationResult;
begin
  // Register a table
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  // Create schema via manager, then add an extra table
  FManager.CreateSchema(FConn);
  ExecSQL('CREATE TABLE ExtraTable (ID INTEGER PRIMARY KEY, Data TEXT)');
  // Migrate to v2 — sdkDropTable should be filtered out by default
  Res:=FManager.MigrateSchema(FConn, 2);
  AssertTrue('Should detect diffs (drop table)', Res.DiffsFound > 0);
  // ExtraTable should still exist
  AssertTrue('ExtraTable should still exist (filtered)', TableExists('ExtraTable'));
end;

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaDryRun;
var
  Res: TOPMigrationResult;
  PD: TPersisterDef;
  FM: TOPFieldMap;
begin
  // Register and create v1 schema
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  FManager.CreateSchema(FConn);
  // Add a field
  PD:=FRegistry.FindPersister(TObject);
  FM:=TOPFieldMap(PD.FieldMap.Add);
  FM.FieldName:='Email';
  FM.PropertyName:='Email';
  FM.PropertyKind:=tkUString;
  FM.Options:=[foInSelect, foInInsert, foInUpdate];
  // Dry-run migrate
  Res:=FManager.MigrateSchema(FConn, 2, [moDryRun]);
  AssertTrue('Should find diffs', Res.DiffsFound > 0);
  AssertEquals('No statements executed in dry-run', 0, Res.StatementsExecuted);
  AssertTrue('SQL array should be populated', Length(Res.SQL) > 0);
  // Column should NOT exist (dry-run)
  AssertFalse('Email column should not exist (dry-run)', ColumnExists('Person', 'Email'));
  // Migration should NOT be recorded
  AssertEquals('Version should still be 1', 1,
    QueryInt('SELECT MAX(Version) FROM ' + cMigrationTable));
end;

procedure TSQLiteMigrateSchemaTest.TestMigrateSchemaVersionTracking;
var
  Res: TOPMigrationResult;
  PD: TPersisterDef;
  FM: TOPFieldMap;
begin
  // Register and create v1 schema
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  FManager.CreateSchema(FConn);
  // Add a field and migrate to v2
  PD:=FRegistry.FindPersister(TObject);
  FM:=TOPFieldMap(PD.FieldMap.Add);
  FM.FieldName:='Age';
  FM.PropertyName:='Age';
  FM.PropertyKind:=tkInteger;
  FM.Options:=[foInSelect, foInInsert, foInUpdate];
  Res:=FManager.MigrateSchema(FConn, 2);
  AssertEquals('Version before should be 1', 1, Res.VersionBefore);
  AssertEquals('Version after should be 2', 2, Res.VersionAfter);
  // Verify migration recorded
  AssertEquals('Current version should be 2', 2,
    QueryInt('SELECT MAX(Version) FROM ' + cMigrationTable));
  // Migrate again — should be no-op
  Res:=FManager.MigrateSchema(FConn, 2);
  AssertEquals('No diffs on re-migration', 0, Res.DiffsFound);
  AssertEquals('No statements on re-migration', 0, Res.StatementsExecuted);
  AssertEquals('Version before should be 2', 2, Res.VersionBefore);
end;

procedure TSQLiteMigrateSchemaTest.TestCreateSchemaRecordsVersion;
begin
  // Register and create schema
  RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
  FManager.CreateSchema(FConn);
  // Verify migration table exists and has version 1
  AssertTrue('Migration table should exist', TableExists(cMigrationTable));
  AssertEquals('Version should be 1', 1,
    QueryInt('SELECT MAX(Version) FROM ' + cMigrationTable));
end;

{ TTestMigrationHelper }

constructor TTestMigrationHelper.Create;
begin
  inherited Create;
  FExecuted:=False;
  FExecutionOrder:=0;
  FOrderCounter:=nil;
  FRaiseException:=False;
end;

procedure TTestMigrationHelper.StepInsertRow(aConnection: TSQLConnection; aVersion: Integer);
var
  Q: TSQLQuery;
begin
  FExecuted:=True;
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=aConnection;
    Q.Transaction:=aConnection.Transaction;
    Q.SQL.Text:='INSERT INTO TestData (Value) VALUES (:V)';
    Q.Params.ParamByName('V').AsInteger:=aVersion;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TTestMigrationHelper.StepTrackOrder(aConnection: TSQLConnection; aVersion: Integer);
begin
  FExecuted:=True;
  if Assigned(FOrderCounter) then
  begin
    Inc(FOrderCounter^);
    FExecutionOrder:=FOrderCounter^;
  end;
end;

procedure TTestMigrationHelper.StepRaise(aConnection: TSQLConnection; aVersion: Integer);
var
  Q: TSQLQuery;
begin
  FExecuted:=True;
  // Insert a row that should be rolled back
  Q:=TSQLQuery.Create(nil);
  try
    Q.Database:=aConnection;
    Q.Transaction:=aConnection.Transaction;
    Q.SQL.Text:='INSERT INTO TestData (Value) VALUES (999)';
    Q.ExecSQL;
  finally
    Q.Free;
  end;
  raise Exception.Create('Deliberate test exception');
end;

{ Custom migration step tests }

procedure TSQLiteMigrateSchemaTest.TestRegisterMigration;
var
  Helper: TTestMigrationHelper;
  Res: TOPMigrationResult;
begin
  // Register a step and verify it doesn't crash
  Helper:=TTestMigrationHelper.Create;
  try
    FManager.RegisterMigration(1, 2, @Helper.StepInsertRow, 'Test step');
    // Register schema and create to get to version 1
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    // Migrate with no schema changes — step should still be registered
    Res:=FManager.MigrateSchema(FConn, 2, [moDryRun]);
    // Dry-run should report 0 statements but acknowledge custom steps exist
    AssertEquals('No statements in dry-run', 0, Res.StatementsExecuted);
  finally
    Helper.Free;
  end;
end;

procedure TSQLiteMigrateSchemaTest.TestCustomStepExecutes;
var
  Helper: TTestMigrationHelper;
  Res: TOPMigrationResult;
  RowCount: Integer;
begin
  Helper:=TTestMigrationHelper.Create;
  try
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    // Create test table for the custom step to insert into
    ExecSQL('CREATE TABLE TestData (Value INTEGER)');
    // Register custom step for version 1 -> 2
    FManager.RegisterMigration(1, 2, @Helper.StepInsertRow, 'Insert test row');
    // Migrate
    Res:=FManager.MigrateSchema(FConn, 2);
    // Verify custom step executed
    AssertTrue('Helper should be marked as executed', Helper.Executed);
    AssertEquals('CustomStepsExecuted should be 1', 1, Res.CustomStepsExecuted);
    // Verify row was inserted
    RowCount:=QueryInt('SELECT COUNT(*) FROM TestData');
    AssertEquals('Should have 1 row in TestData', 1, RowCount);
    AssertEquals('Row value should be target version', 2,
      QueryInt('SELECT Value FROM TestData'));
  finally
    Helper.Free;
  end;
end;

procedure TSQLiteMigrateSchemaTest.TestCustomStepOrder;
var
  Helper1, Helper2: TTestMigrationHelper;
  Res: TOPMigrationResult;
  Counter: Integer;
begin
  Counter:=0;
  Helper1:=TTestMigrationHelper.Create;
  Helper2:=TTestMigrationHelper.Create;
  try
    Helper1.OrderCounter:=@Counter;
    Helper2.OrderCounter:=@Counter;
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    // Register two steps for the same transition
    FManager.RegisterMigration(1, 2, @Helper1.StepTrackOrder, 'First step');
    FManager.RegisterMigration(1, 2, @Helper2.StepTrackOrder, 'Second step');
    Res:=FManager.MigrateSchema(FConn, 2);
    // Both should have executed
    AssertTrue('Helper1 should be executed', Helper1.Executed);
    AssertTrue('Helper2 should be executed', Helper2.Executed);
    AssertEquals('CustomStepsExecuted should be 2', 2, Res.CustomStepsExecuted);
    // Verify registration order
    AssertEquals('Helper1 should execute first', 1, Helper1.ExecutionOrder);
    AssertEquals('Helper2 should execute second', 2, Helper2.ExecutionOrder);
  finally
    Helper1.Free;
    Helper2.Free;
  end;
end;

procedure TSQLiteMigrateSchemaTest.TestCustomStepNoMatchSkipped;
var
  Helper: TTestMigrationHelper;
  Res: TOPMigrationResult;
begin
  Helper:=TTestMigrationHelper.Create;
  try
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    // Register a step for version 3 -> 4 (won't match 1 -> 2)
    FManager.RegisterMigration(3, 4, @Helper.StepTrackOrder, 'Unmatched step');
    Res:=FManager.MigrateSchema(FConn, 2);
    // Step should NOT have executed
    AssertFalse('Helper should not be executed', Helper.Executed);
    AssertEquals('CustomStepsExecuted should be 0', 0, Res.CustomStepsExecuted);
  finally
    Helper.Free;
  end;
end;

procedure TSQLiteMigrateSchemaTest.TestCustomStepDryRunSkipped;
var
  Helper: TTestMigrationHelper;
  Res: TOPMigrationResult;
begin
  Helper:=TTestMigrationHelper.Create;
  try
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    ExecSQL('CREATE TABLE TestData (Value INTEGER)');
    // Register step for 1 -> 2
    FManager.RegisterMigration(1, 2, @Helper.StepInsertRow, 'Should not run');
    Res:=FManager.MigrateSchema(FConn, 2, [moDryRun]);
    // Step should NOT have executed
    AssertFalse('Helper should not be executed in dry-run', Helper.Executed);
    AssertEquals('CustomStepsExecuted should be 0', 0, Res.CustomStepsExecuted);
    // TestData should be empty
    AssertEquals('TestData should be empty', 0, QueryInt('SELECT COUNT(*) FROM TestData'));
  finally
    Helper.Free;
  end;
end;

procedure TSQLiteMigrateSchemaTest.TestCustomStepWithDDL;
var
  Helper: TTestMigrationHelper;
  Res: TOPMigrationResult;
  PD: TPersisterDef;
  FM: TOPFieldMap;
begin
  Helper:=TTestMigrationHelper.Create;
  try
    // Create v1 schema
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    // Create the test data table for custom step
    ExecSQL('CREATE TABLE TestData (Value INTEGER)');
    // Evolve model — add Age column
    PD:=FRegistry.FindPersister(TObject);
    FM:=TOPFieldMap(PD.FieldMap.Add);
    FM.FieldName:='Age';
    FM.PropertyName:='Age';
    FM.PropertyKind:=tkInteger;
    FM.Options:=[foInSelect, foInInsert, foInUpdate];
    // Register custom step to populate data after DDL
    FManager.RegisterMigration(1, 2, @Helper.StepInsertRow, 'Populate after DDL');
    Res:=FManager.MigrateSchema(FConn, 2);
    // Verify DDL happened
    AssertTrue('Age column should exist', ColumnExists('Person', 'Age'));
    AssertTrue('Should have DDL diffs', Res.DiffsFound > 0);
    AssertTrue('Should have executed statements', Res.StatementsExecuted > 0);
    // Verify custom step also ran
    AssertTrue('Helper should be executed', Helper.Executed);
    AssertEquals('CustomStepsExecuted should be 1', 1, Res.CustomStepsExecuted);
    AssertEquals('TestData should have 1 row', 1, QueryInt('SELECT COUNT(*) FROM TestData'));
  finally
    Helper.Free;
  end;
end;

procedure TSQLiteMigrateSchemaTest.TestCustomStepExceptionRollback;
var
  Helper: TTestMigrationHelper;
  Res: TOPMigrationResult;
  ExceptionRaised: Boolean;
begin
  Helper:=TTestMigrationHelper.Create;
  try
    RegisterSimpleTable('Person', ['ID', 'Name'], [tkInteger, tkUString]);
    FManager.CreateSchema(FConn);
    ExecSQL('CREATE TABLE TestData (Value INTEGER)');
    // Register a step that raises an exception
    FManager.RegisterMigration(1, 2, @Helper.StepRaise, 'Failing step');
    ExceptionRaised:=False;
    try
      Res:=FManager.MigrateSchema(FConn, 2);
    except
      on E: Exception do
      begin
        ExceptionRaised:=True;
        AssertEquals('Exception message', 'Deliberate test exception', E.Message);
      end;
    end;
    AssertTrue('Exception should have propagated', ExceptionRaised);
    // CustomStepsExecuted should not have been incremented (exception before Inc)
    AssertEquals('CustomStepsExecuted should be 0', 0, Res.CustomStepsExecuted);
    // The row inserted by the failing step should be rolled back
    AssertEquals('TestData should be empty (rolled back)', 0,
      QueryInt('SELECT COUNT(*) FROM TestData'));
    // Migration should NOT be recorded (step 10 never reached)
    AssertEquals('Version should still be 1', 1,
      QueryInt('SELECT MAX(Version) FROM ' + cMigrationTable));
  finally
    Helper.Free;
  end;
end;

initialization
  RegisterTest(TSQLiteMigrationTest);
  RegisterTest(TSQLiteMigrateSchemaTest);

end.
