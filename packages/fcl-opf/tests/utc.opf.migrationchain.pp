unit utc.opf.migrationchain;

{ Migration: chained custom steps, version recording per step group,
  tracking table per dialect, transaction policy, schema comparison of
  types and nullability, and a create/migrate round trip without differences. }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb, Data.SqlDb.Sqlite3,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, DB, sqldb, sqlite3conn,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Relations, Opf.M2M, Opf.Driver, Opf.DDL,
  Opf.Schema, Opf.Migration, Opf.Persister.RTTI;

type

  { TChainMigrator: exposes the chain planning }

  TChainMigrator = class(TOPSchemaMigrator)
  public
    function Chain(aFrom, aTo: Integer): TOPMigrationStepArray;
    function Builder(aConnection: TSQLConnection): TOPDDLBuilder;
  end;

  { TTestMigrationChainPlan: no database }

  TTestMigrationChainPlan = class(TTestCase)
  private
    FMigrator: TChainMigrator;
    procedure Step(aConnection: TSQLConnection; aVersion: Integer);
    procedure AssertChainRaises(const aMsg: String; aFrom, aTo: Integer);
    function Describe(const aSteps: TOPMigrationStepArray): String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLinearChain;
    procedure TestPartialChainFromCurrentVersion;
    procedure TestSchemaOnlyTail;
    procedure TestStepsAboveTargetIgnored;
    procedure TestSeveralStepsSameTransitionInOrder;
    procedure TestNoSteps;
    procedure TestGapRaises;
    procedure TestAmbiguousRaises;
    procedure TestOvershootRaises;
    procedure TestBackwardsRaises;
    procedure TestOldStepsBelowCurrentIgnored;
    procedure TestFreshDatabaseStartsAtVersionOne;
    procedure TestStepFromZeroUsedOnFreshDatabase;
  end;

  { TMigItem: model for the SQLite tests }

  TMigItem = class(TPersistent)
  private
    FAmount: Currency;
    FBig: Int64;
    FCode: String;
    FCreated: TDateTime;
    FDay: TDate;
    FID: Integer;
    FName: String;
    FRatio: Double;
    FActive: Boolean;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Code: String read FCode write FCode;
    property Big: Int64 read FBig write FBig;
    property Created: TDateTime read FCreated write FCreated;
    property Day: TDate read FDay write FDay;
    property Amount: Currency read FAmount write FAmount;
    property Ratio: Double read FRatio write FRatio;
    property Active: Boolean read FActive write FActive;
  end;

  { TMigParent, TMigChild: a relation for the foreign key tests }

  TMigParent = class(TPersistent)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TMigChild = class(TPersistent)
  private
    FID: Integer;
    FOtherID: Integer;
    FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property OtherID: Integer read FOtherID write FOtherID;
  end;

  { TTestMigrationChainRun: SQLite }

  TTestMigrationChainRun = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FMigrator: TOPSchemaMigrator;
    FCalls: TStringList;
    FFailAt: Integer;
    FLog: TStringList;
    procedure Exec(const aSQL: String);
    function Scalar(const aSQL: String): Integer;
    function History: String;
    procedure Step12(aConnection: TSQLConnection; aVersion: Integer);
    procedure Step23(aConnection: TSQLConnection; aVersion: Integer);
    procedure Step34(aConnection: TSQLConnection; aVersion: Integer);
    procedure StepCommitsThenFails(aConnection: TSQLConnection; aVersion: Integer);
    procedure RegisterSteps;
    procedure RegisterRelation;
    procedure DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
    function Logged(const aText: String): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChainRunsAllSteps;
    procedure TestChainRecordsEachVersion;
    procedure TestStepsReceiveTheirVersion;
    procedure TestFailureInMiddleKeepsEarlierVersions;
    procedure TestRerunAfterFailureResumes;
    procedure TestGapRaisesBeforeChanges;
    procedure TestDryRunPlansWithoutExecuting;
    procedure TestAlreadyAtTarget;
    procedure TestCustomTrackingTableNeverDropped;
    procedure TestPolicyRaiseWithCallerTransaction;
    procedure TestPolicyCommitCaller;
    procedure TestLegacyTrackingTableWithID;
    procedure TestDuplicateVersionRejected;
    procedure TestCreateThenMigrateFindsNoDifferences;
    procedure TestGenerateSQLDoesNotCreateTrackingTable;
    // Review 2
    procedure TestTrackingColumnsQuotedWithAlwaysQuote;
    procedure TestFreshInstallWithSteps;
    procedure TestCreateSchemaRepeatable;
    procedure TestDryRunCreatesNoTrackingTable;
    procedure TestAppendCharSchemaHasNoDifferences;
    procedure TestGenerateSQLStopsBeforeCustomSteps;
    procedure TestGenerateSQLStepFromCurrentVersion;
    procedure TestExecDDLWithActiveCallerTransaction;
    procedure TestAddUniqueColumn;
    procedure TestCreateSchemaWithRelation;
    procedure TestRollbackFailureKeepsStepError;
    procedure TestKeepIndexes;
  end;

  { TTestMigrationStatements: generated migration statements, no database }

  TTestMigrationStatements = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FBuilder: TOPDDLBuilder;
    function Field(aKind: TTypeKind; aOptions: TOPFieldOptions): TOPFieldMap;
    function Diff(aKind: TSchemaDiffKind; const aTable, aColumn: UTF8String): TOPSchemaDiffArray;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMySQLWidenAutoIncKeepsAutoIncrement;
    procedure TestMySQLModifyKeepsDefault;
    procedure TestPostgresWidenAutoIncAltersSequence;
    procedure TestPostgresNarrowKeyNoSequenceChange;
    procedure TestNewTableKeepsPKConstraintName;
    procedure TestForceQuotesMigration;
    procedure TestSQLServerUnicodeStrings;
    procedure TestSQLiteAddUniqueColumn;
  end;

  { TTestTrackingTableDDL }

  TTestTrackingTableDDL = class(TTestCase)
  private
    function CreateSQL(aDialect: TSQLDialect; const aName: UTF8String = ''): UTF8String;
  published
    procedure TestFirebirdQuotesInternalName;
    procedure TestFirebirdCustomNameUnquoted;
    procedure TestNoTextType;
    procedure TestVersionIsPrimaryKey;
    procedure TestSQLiteUnquoted;
  end;

  { TTestSchemaTypeCompare }

  TTestSchemaTypeCompare = class(TTestCase)
  private
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FBuilder: TOPDDLBuilder;
    FSchema: TOPSchemaInfo;
    procedure AddColumn(const aName, aType: UTF8String; aNullable: Boolean;
      aMaxLen: Integer = 0; aPrec: Integer = 0; aScale: Integer = 0);
    function Diffs(const aIgnore: array of UTF8String): TOPSchemaDiffArray;
    function CountKind(const aDiffs: TOPSchemaDiffArray; aKind: TSchemaDiffKind; const aColumn: UTF8String = ''): Integer;
    procedure BuildPGTable(aNameLen: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParseSQLType;
    procedure TestCanonicalNames;
    procedure TestPostgresNamesMatch;
    procedure TestVarcharLengthMismatch;
    procedure TestDecimalPrecisionMismatch;
    procedure TestPrimaryKeyReportedNullable;
    procedure TestRequiredColumnNullableIsDiff;
    procedure TestIgnoredTableNotDropped;
    procedure TestUnignoredTableDropped;
    // Review 2
    procedure TestAppendCharNamesMatch;
    procedure TestDropTablesReferencingFirst;
    procedure TestChangedRelationDropsOldForeignKeyFirst;
    procedure TestSQLiteInt64AutoIncNoTypeDiff;
  end;

implementation

uses Opf.Sql;

{ TChainMigrator }

function TChainMigrator.Chain(aFrom, aTo: Integer): TOPMigrationStepArray;
begin
  Result:=BuildStepChain(aFrom, aTo);
end;

function TChainMigrator.Builder(aConnection: TSQLConnection): TOPDDLBuilder;
begin
  Result:=CreateDDLBuilder(aConnection);
end;

{ TTestMigrationChainPlan }

procedure TTestMigrationChainPlan.Step(aConnection: TSQLConnection; aVersion: Integer);
begin
end;

procedure TTestMigrationChainPlan.AssertChainRaises(const aMsg: String; aFrom, aTo: Integer);
var
  Raised: Boolean;
begin
  Raised:=False;
  try
    FMigrator.Chain(aFrom, aTo);
  except
    on EOPF do
      Raised:=True;
  end;
  AssertTrue(aMsg, Raised);
end;

function TTestMigrationChainPlan.Describe(const aSteps: TOPMigrationStepArray): String;
var
  S: TOPMigrationStepDef;
begin
  Result:='';
  for S in aSteps do
  begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+IntToStr(S.FromVersion)+'>'+IntToStr(S.ToVersion)+S.Description;
  end;
end;

procedure TTestMigrationChainPlan.SetUp;
begin
  inherited SetUp;
  FMigrator:=TChainMigrator.Create(Nil);
end;

procedure TTestMigrationChainPlan.TearDown;
begin
  FreeAndNil(FMigrator);
  inherited TearDown;
end;

procedure TTestMigrationChainPlan.TestLinearChain;
begin
  FMigrator.RegisterMigration(3, 4, @Step);
  FMigrator.RegisterMigration(1, 2, @Step);
  FMigrator.RegisterMigration(2, 3, @Step);
  AssertEquals('Ordered by version, not registration', '1>2,2>3,3>4', Describe(FMigrator.Chain(1, 4)));
end;

procedure TTestMigrationChainPlan.TestPartialChainFromCurrentVersion;
begin
  FMigrator.RegisterMigration(1, 2, @Step);
  FMigrator.RegisterMigration(2, 3, @Step);
  FMigrator.RegisterMigration(3, 4, @Step);
  AssertEquals('From the current version', '2>3,3>4', Describe(FMigrator.Chain(2, 4)));
end;

procedure TTestMigrationChainPlan.TestSchemaOnlyTail;
begin
  FMigrator.RegisterMigration(1, 2, @Step);
  AssertEquals('Rest is schema only', '1>2', Describe(FMigrator.Chain(1, 5)));
end;

procedure TTestMigrationChainPlan.TestStepsAboveTargetIgnored;
begin
  FMigrator.RegisterMigration(1, 2, @Step);
  FMigrator.RegisterMigration(5, 6, @Step);
  AssertEquals('Later steps ignored', '1>2', Describe(FMigrator.Chain(1, 3)));
end;

procedure TTestMigrationChainPlan.TestSeveralStepsSameTransitionInOrder;
begin
  FMigrator.RegisterMigration(1, 2, @Step, 'a');
  FMigrator.RegisterMigration(1, 2, @Step, 'b');
  AssertEquals('Registration order within a transition', '1>2a,1>2b', Describe(FMigrator.Chain(1, 2)));
end;

procedure TTestMigrationChainPlan.TestNoSteps;
begin
  AssertEquals('Nothing to run', 0, Length(FMigrator.Chain(1, 3)));
end;

procedure TTestMigrationChainPlan.TestGapRaises;
begin
  FMigrator.RegisterMigration(1, 2, @Step);
  FMigrator.RegisterMigration(3, 4, @Step);
  AssertChainRaises('No step from 2, but one from 3', 1, 4);
end;

procedure TTestMigrationChainPlan.TestAmbiguousRaises;
begin
  FMigrator.RegisterMigration(2, 3, @Step);
  FMigrator.RegisterMigration(2, 4, @Step);
  AssertChainRaises('Two targets from version 2', 2, 4);
end;

procedure TTestMigrationChainPlan.TestOvershootRaises;
begin
  FMigrator.RegisterMigration(2, 5, @Step);
  AssertChainRaises('Step beyond the target', 2, 4);
end;

procedure TTestMigrationChainPlan.TestBackwardsRaises;
begin
  FMigrator.RegisterMigration(2, 1, @Step);
  AssertChainRaises('Step to a lower version', 2, 4);
end;

procedure TTestMigrationChainPlan.TestOldStepsBelowCurrentIgnored;
begin
  FMigrator.RegisterMigration(1, 2, @Step);
  FMigrator.RegisterMigration(3, 4, @Step);
  AssertEquals('Steps below the current version are history', '3>4', Describe(FMigrator.Chain(3, 4)));
end;

procedure TTestMigrationChainPlan.TestFreshDatabaseStartsAtVersionOne;
begin
  FMigrator.RegisterMigration(1, 2, @Step);
  FMigrator.RegisterMigration(2, 3, @Step);
  // Version 0: no version recorded, the schema is created from the model
  AssertEquals('Fresh database: from version 1', '1>2,2>3', Describe(FMigrator.Chain(0, 3)));
  AssertEquals('Fresh database, target 1: no steps', 0, Length(FMigrator.Chain(0, 1)));
end;

procedure TTestMigrationChainPlan.TestStepFromZeroUsedOnFreshDatabase;
begin
  FMigrator.RegisterMigration(0, 1, @Step, 'seed');
  FMigrator.RegisterMigration(1, 2, @Step);
  AssertEquals('A step from 0 runs', '0>1seed,1>2', Describe(FMigrator.Chain(0, 2)));
end;

{ TTestMigrationChainRun }

procedure TTestMigrationChainRun.Exec(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FConn.Transaction;
    if not FConn.Transaction.Active then
      FConn.Transaction.StartTransaction;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
    FConn.Transaction.Commit;
  finally
    Q.Free;
  end;
end;

function TTestMigrationChainRun.Scalar(const aSQL: String): Integer;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:=aSQL;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestMigrationChainRun.History: String;
var
  T: TOPMigrationTracker;
  R: TOPMigrationRecord;
begin
  Result:='';
  T:=TOPMigrationTracker.Create;
  try
    T.Connection:=FConn;
    T.Dialect:=dSQLite;
    T.TableName:=FMigrator.MigrationTableName;
    for R in T.GetMigrationHistory do
    begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+IntToStr(R.Version);
    end;
  finally
    T.Free;
  end;
end;

procedure TTestMigrationChainRun.Step12(aConnection: TSQLConnection; aVersion: Integer);
var
  Q: TSQLQuery;
begin
  FCalls.Add('1>2@'+IntToStr(aVersion));
  // Data change inside the step transaction
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConnection;
    Q.Transaction:=aConnection.Transaction;
    Q.SQL.Text:='INSERT INTO StepLog (Step) VALUES (12)';
    Q.ExecSQL;
  finally
    Q.Free;
  end;
  if FFailAt=2 then
    raise Exception.Create('Step 1>2 failed');
end;

procedure TTestMigrationChainRun.Step23(aConnection: TSQLConnection; aVersion: Integer);
var
  Q: TSQLQuery;
begin
  FCalls.Add('2>3@'+IntToStr(aVersion));
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConnection;
    Q.Transaction:=aConnection.Transaction;
    Q.SQL.Text:='INSERT INTO StepLog (Step) VALUES (23)';
    Q.ExecSQL;
  finally
    Q.Free;
  end;
  if FFailAt=3 then
    raise Exception.Create('Step 2>3 failed');
end;

procedure TTestMigrationChainRun.Step34(aConnection: TSQLConnection; aVersion: Integer);
begin
  FCalls.Add('3>4@'+IntToStr(aVersion));
end;

procedure TTestMigrationChainRun.StepCommitsThenFails(aConnection: TSQLConnection; aVersion: Integer);
var
  Q: TSQLQuery;
begin
  // Ends the transaction behind sqldb's back: the rollback after the error fails
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConnection;
    Q.Transaction:=aConnection.Transaction;
    Q.SQL.Text:='COMMIT';
    Q.ExecSQL;
  finally
    Q.Free;
  end;
  raise Exception.Create('Step failed on purpose');
end;

procedure TTestMigrationChainRun.RegisterRelation;
var
  D: TPersisterDef;
begin
  D:=FRegistry.RegisterPersister(TMigParent, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  D.FieldMap.FindByPropertyName('Name').MaxSize:=50;
  D:=FRegistry.RegisterPersister(TMigChild, TRTTIPersister);
  D.FieldMap.FindByPropertyName('ID').Options:=D.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  FRelations.RegisterRelationship(TMigParent, TMigChild, 'ParentID');
end;

procedure TTestMigrationChainRun.DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
begin
  FLog.Add(Msg);
end;

function TTestMigrationChainRun.Logged(const aText: String): Boolean;
var
  S: String;
begin
  for S in FLog do
    if Pos(aText, S)>0 then
      Exit(True);
  Result:=False;
end;

procedure TTestMigrationChainRun.RegisterSteps;
begin
  FMigrator.RegisterMigration(1, 2, @Step12, 'one to two');
  FMigrator.RegisterMigration(2, 3, @Step23, 'two to three');
  FMigrator.RegisterMigration(3, 4, @Step34, 'three to four');
end;

procedure TTestMigrationChainRun.SetUp;
var
  F: TOPFieldMap;
  D: TPersisterDef;
begin
  inherited SetUp;
  FCalls:=TStringList.Create;
  FLog:=TStringList.Create;
  FFailAt:=0;
  FConn:=TSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FRegistry:=TOPRegistry.Create(Nil);
  D:=FRegistry.RegisterPersister(TMigItem, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  F:=D.FieldMap.FindByPropertyName('Name');
  F.MaxSize:=50;
  F.Options:=F.Options+[foRequired];
  F:=D.FieldMap.FindByPropertyName('Code');
  F.MaxSize:=10;
  F.MinSize:=10;
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  FMigrator:=TOPSchemaMigrator.Create(Nil);
  FMigrator.Registry:=FRegistry;
  FMigrator.RelationRegistry:=FRelations;
  FMigrator.M2MRegistry:=FM2M;
  FMigrator.SQLDialect:=dSQLite;
  // A table the custom steps write to, outside the model: kept out of the
  // comparison by naming it like an internal table
  Exec('CREATE TABLE _opf_StepLog (Step INTEGER)');
end;

procedure TTestMigrationChainRun.TearDown;
begin
  FreeAndNil(FMigrator);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  FreeAndNil(FCalls);
  FreeAndNil(FLog);
  inherited TearDown;
end;

procedure TTestMigrationChainRun.TestChainRunsAllSteps;
var
  R: TOPMigrationResult;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  // StepLog is not in the model: the comparison would offer to drop it,
  // which is not allowed by default
  R:=FMigrator.MigrateSchema(FConn, 4);
  AssertEquals('All steps, in order', '1>2@2,2>3@3,3>4@4', FCalls.CommaText);
  AssertEquals('Steps counted', 3, R.CustomStepsExecuted);
  AssertEquals('Version after', 4, R.VersionAfter);
end;

procedure TTestMigrationChainRun.TestChainRecordsEachVersion;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  FMigrator.MigrateSchema(FConn, 4);
  AssertEquals('Each version recorded', '1,2,3,4', History);
end;

procedure TTestMigrationChainRun.TestStepsReceiveTheirVersion;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  FMigrator.MigrateSchema(FConn, 3);
  AssertEquals('Up to the target only', '1>2@2,2>3@3', FCalls.CommaText);
  AssertEquals('History', '1,2,3', History);
end;

procedure TTestMigrationChainRun.TestFailureInMiddleKeepsEarlierVersions;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  FFailAt:=3;
  try
    FMigrator.MigrateSchema(FConn, 4);
    Fail('Failing step must raise');
  except
    on E: Exception do
      AssertTrue('Step error: '+E.Message, Pos('2>3 failed', E.Message)>0);
  end;
  AssertEquals('Version 2 recorded, 3 not', '1,2', History);
  AssertEquals('Data of 1>2 kept, of 2>3 rolled back', 12, Scalar('SELECT SUM(Step) FROM StepLog'));
end;

procedure TTestMigrationChainRun.TestRerunAfterFailureResumes;
var
  R: TOPMigrationResult;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  FFailAt:=3;
  try
    FMigrator.MigrateSchema(FConn, 4);
  except
    on Exception do ;
  end;
  FFailAt:=0;
  FCalls.Clear;
  R:=FMigrator.MigrateSchema(FConn, 4);
  AssertEquals('Resumes at the failed step', '2>3@3,3>4@4', FCalls.CommaText);
  AssertEquals('Version before', 2, R.VersionBefore);
  AssertEquals('History', '1,2,3,4', History);
  AssertEquals('1>2 ran once, 2>3 once', 12+23, Scalar('SELECT SUM(Step) FROM StepLog'));
end;

procedure TTestMigrationChainRun.TestGapRaisesBeforeChanges;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  FMigrator.RegisterMigration(1, 2, @Step12);
  FMigrator.RegisterMigration(3, 4, @Step34);
  try
    FMigrator.MigrateSchema(FConn, 4);
    Fail('Gap must raise');
  except
    on EOPF do ;
  end;
  AssertEquals('Nothing ran', 0, FCalls.Count);
  AssertEquals('Nothing recorded', '1', History);
end;

procedure TTestMigrationChainRun.TestDryRunPlansWithoutExecuting;
var
  R: TOPMigrationResult;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  R:=FMigrator.MigrateSchema(FConn, 4, [moDryRun]);
  AssertEquals('Planned steps', 3, Length(R.PlannedSteps));
  AssertEquals('Nothing ran', 0, FCalls.Count);
  AssertEquals('Version unchanged', 1, R.VersionAfter);
  AssertEquals('History unchanged', '1', History);
end;

procedure TTestMigrationChainRun.TestAlreadyAtTarget;
var
  R: TOPMigrationResult;
begin
  FMigrator.CreateSchema(FConn);
  R:=FMigrator.MigrateSchema(FConn, 1);
  AssertEquals('Before', 1, R.VersionBefore);
  AssertEquals('After', 1, R.VersionAfter);
end;

procedure TTestMigrationChainRun.TestCustomTrackingTableNeverDropped;
var
  R: TOPMigrationResult;
begin
  FMigrator.MigrationTableName:='schema_version';
  FMigrator.CreateSchema(FConn);
  R:=FMigrator.MigrateSchema(FConn, 2, [moAllowDropTable]);
  AssertEquals('Tracking table still there', 1, Scalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE name=''schema_version'''));
  AssertEquals('History kept', '1,2', History);
  AssertEquals('Version after', 2, R.VersionAfter);
end;

procedure TTestMigrationChainRun.TestPolicyRaiseWithCallerTransaction;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  AssertTrue('Raise is the default', FMigrator.TransactionPolicy=mtpRaise);
  FTrans.StartTransaction;
  try
    FMigrator.MigrateSchema(FConn, 2);
    Fail('Active caller transaction must raise');
  except
    on EOPF do ;
  end;
  AssertTrue('Caller transaction left alone', FTrans.Active);
  FTrans.Rollback;
end;

procedure TTestMigrationChainRun.TestPolicyCommitCaller;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  FMigrator.CreateSchema(FConn);
  FMigrator.TransactionPolicy:=mtpCommitCaller;
  FTrans.StartTransaction;
  FMigrator.MigrateSchema(FConn, 2);
  AssertEquals('Migrated', '1,2', History);
end;

procedure TTestMigrationChainRun.TestLegacyTrackingTableWithID;
begin
  Exec('ALTER TABLE _opf_StepLog RENAME TO StepLog');
  // As created by earlier versions
  Exec('CREATE TABLE _opf_migrations (ID INTEGER PRIMARY KEY, Version INTEGER NOT NULL, '+
       'AppliedAt TEXT NOT NULL, Description TEXT)');
  Exec('INSERT INTO _opf_migrations (Version, AppliedAt, Description) VALUES (1, ''2025-01-01 00:00:00'', ''Initial'')');
  FMigrator.Registry:=FRegistry;
  FMigrator.CreateSchema(FConn);
  RegisterSteps;
  FMigrator.MigrateSchema(FConn, 3);
  AssertEquals('Old table used as it is', 1, Scalar(
    'SELECT COUNT(*) FROM pragma_table_info(''_opf_migrations'') WHERE name=''ID'''));
  AssertTrue('Versions recorded: '+History, Pos('2,3', History)>0);
end;

procedure TTestMigrationChainRun.TestDuplicateVersionRejected;
var
  T: TOPMigrationTracker;
  Raised: Boolean;
begin
  FMigrator.CreateSchema(FConn);
  T:=TOPMigrationTracker.Create;
  try
    T.Connection:=FConn;
    T.Dialect:=dSQLite;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Raised:=False;
    try
      T.RecordMigration(1, 'again');
    except
      on Exception do
        Raised:=True;
    end;
    FTrans.Rollback;
    AssertTrue('Version is the primary key', Raised);
  finally
    T.Free;
  end;
end;

procedure TTestMigrationChainRun.TestCreateThenMigrateFindsNoDifferences;
var
  R: TOPMigrationResult;
begin
  FMigrator.CreateSchema(FConn);
  R:=FMigrator.MigrateSchema(FConn, 2, [moDryRun]);
  AssertEquals('No differences after CreateSchema: '+string(Concat(R.SQL, [''])[0]), 0, R.DiffsFound);
end;

procedure TTestMigrationChainRun.TestGenerateSQLDoesNotCreateTrackingTable;
var
  S: TUTF8StringArray;
begin
  S:=FMigrator.GenerateMigrationSQL(FConn, 2);
  AssertEquals('No tracking table created', 0, Scalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE name=''_opf_migrations'''));
  AssertTrue('Script creates it', (Length(S)>0) and (Pos('CREATE TABLE _opf_migrations', S[0])=1));
  AssertTrue('Script records the version', Pos('INSERT INTO _opf_migrations', S[High(S)])=1);
end;

procedure TTestMigrationChainRun.TestTrackingColumnsQuotedWithAlwaysQuote;
begin
  FMigrator.ReservedWordStrategy:=rwsAlwaysQuote;
  FConn.OnLog:=@DoLog;
  FConn.LogEvents:=LogAllEvents;
  FMigrator.CreateSchema(FConn);
  FMigrator.MigrateSchema(FConn, 2);
  // The columns are created as "Version", ...: a bare Version is another
  // name where unquoted names are folded (PostgreSQL, Firebird, Oracle)
  AssertTrue('Created quoted', Logged('"Version" INT NOT NULL'));
  AssertTrue('Read quoted', Logged('SELECT MAX("Version")'));
  AssertTrue('Inserted quoted', Logged('("Version", "AppliedAt", "Description")'));
  AssertFalse('No bare name', Logged('MAX(Version)'));
  AssertEquals('History', '1,2', History);
end;

procedure TTestMigrationChainRun.TestFreshInstallWithSteps;
var
  R: TOPMigrationResult;
begin
  // The standard startup call on an empty database, with registered steps
  FMigrator.RegisterMigration(1, 2, @Step34, 'one to two');
  R:=FMigrator.MigrateSchema(FConn, 2);
  AssertEquals('Step ran', 1, FCalls.Count);
  AssertEquals('Version after', 2, R.VersionAfter);
  AssertEquals('Table created', 1, Scalar('SELECT COUNT(*) FROM sqlite_master WHERE name=''MigItem'''));
  AssertEquals('History', '2', History);
end;

procedure TTestMigrationChainRun.TestCreateSchemaRepeatable;
var
  D: TPersisterDef;
  Idx: TOPIndexDefArray;
begin
  D:=FRegistry.FindPersister(TMigItem);
  SetLength(Idx, 1);
  Idx[0].IndexName:='IX_MigItem_Name';
  Idx[0].FieldNames:='Name';
  D.Indexes:=Idx;
  FMigrator.CreateSchema(FConn, [coIfNotExists]);
  // Again on the existing database: no error, version 1 recorded once
  FMigrator.CreateSchema(FConn, [coIfNotExists]);
  AssertEquals('History', '1', History);
end;

procedure TTestMigrationChainRun.TestDryRunCreatesNoTrackingTable;
var
  R: TOPMigrationResult;
begin
  R:=FMigrator.MigrateSchema(FConn, 1, [moDryRun]);
  AssertTrue('Differences found', R.DiffsFound>0);
  AssertEquals('No tracking table created', 0, Scalar(
    'SELECT COUNT(*) FROM sqlite_master WHERE name=''_opf_migrations'''));
  AssertEquals('No table created', 0, Scalar('SELECT COUNT(*) FROM sqlite_master WHERE name=''MigItem'''));
end;

procedure TTestMigrationChainRun.TestAppendCharSchemaHasNoDifferences;
var
  D: TPersisterDef;
  R: TOPMigrationResult;
begin
  D:=FRegistry.FindPersister(TMigItem);
  D.TableName:='Order';
  D.FieldMap.FindByPropertyName('Code').FieldName:='Group';
  FMigrator.ReservedWordStrategy:=rwsAppendChar;
  FMigrator.CreateSchema(FConn);
  AssertEquals('Created as Order_', 1, Scalar('SELECT COUNT(*) FROM sqlite_master WHERE name=''Order_'''));
  R:=FMigrator.MigrateSchema(FConn, 2, [moDryRun, moAllowDropTable, moAllowDropColumn]);
  AssertEquals('No differences: '+string(Concat(R.SQL, [''])[0]), 0, R.DiffsFound);
end;

procedure TTestMigrationChainRun.TestGenerateSQLStopsBeforeCustomSteps;
var
  S: TUTF8StringArray;
begin
  // Fresh database: the script creates the schema (version 1)
  FMigrator.RegisterMigration(1, 2, @Step34);
  S:=FMigrator.GenerateMigrationSQL(FConn, 3);
  AssertTrue('Schema created', (Length(S)>2) and (Pos('CREATE TABLE MigItem', S[1])=1));
  // Step 1>2 cannot run in a script: version 1 is recorded, not 3
  AssertTrue('Records version 1: '+S[High(S)], Pos('VALUES (1,', S[High(S)])>0);
  AssertEquals('Nothing ran', 0, FCalls.Count);
end;

procedure TTestMigrationChainRun.TestGenerateSQLStepFromCurrentVersion;
var
  S: TUTF8StringArray;
  I: Integer;
begin
  FMigrator.CreateSchema(FConn);
  FMigrator.RegisterMigration(1, 2, @Step34);
  S:=FMigrator.GenerateMigrationSQL(FConn, 2);
  // The first step starts at the current version: nothing to record
  for I:=0 to High(S) do
    AssertTrue('No version recorded: '+S[I], Pos('INSERT', S[I])=0);
end;

procedure TTestMigrationChainRun.TestExecDDLWithActiveCallerTransaction;
var
  B: TOPDDLBuilder;
  Msg: String;
begin
  B:=TOPDDLBuilder.Create(Nil);
  try
    B.SQLDialect:=dSQLite;
    FTrans.StartTransaction;
    Msg:='';
    try
      B.ExecDDL(FConn, 'CREATE TABLE ActiveTrans (A INT)');
    except
      on E: EOPFDDL do
        Msg:=E.Message;
    end;
    // SQLite has one transaction per connection: a clear error, nothing changed
    AssertTrue('Clear error: '+Msg, Pos('is active', Msg)>0);
    AssertTrue('Caller transaction still active', FTrans.Active);
    AssertTrue('Connection keeps its transaction', FConn.Transaction=FTrans);
    FTrans.Rollback;
  finally
    B.Free;
  end;
end;

procedure TTestMigrationChainRun.TestAddUniqueColumn;
var
  D: TPersisterDef;
  R: TOPMigrationResult;
  Raised: Boolean;
begin
  FMigrator.CreateSchema(FConn);
  Exec('ALTER TABLE MigItem DROP COLUMN Code');
  D:=FRegistry.FindPersister(TMigItem);
  D.FieldMap.FindByPropertyName('Code').Options:=D.FieldMap.FindByPropertyName('Code').Options+[foUnique];
  // SQLite: "Cannot add a UNIQUE column"; added, then a unique index
  R:=FMigrator.MigrateSchema(FConn, 2);
  AssertEquals('Column and index', 2, R.StatementsExecuted);
  Exec('INSERT INTO MigItem (Name, Code) VALUES (''a'', ''X'')');
  Raised:=False;
  try
    Exec('INSERT INTO MigItem (Name, Code) VALUES (''b'', ''X'')');
  except
    on Exception do
      Raised:=True;
  end;
  if FConn.Transaction.Active then
    FConn.Transaction.Rollback;
  AssertTrue('Values are unique', Raised);
  R:=FMigrator.MigrateSchema(FConn, 3, [moDryRun]);
  AssertEquals('The unique index is not a difference', 0, R.DiffsFound);
end;

procedure TTestMigrationChainRun.TestCreateSchemaWithRelation;
var
  R: TOPMigrationResult;
begin
  RegisterRelation;
  // Default schema options include foreign keys: SQLite cannot ALTER TABLE
  // ADD CONSTRAINT, they are declared in CREATE TABLE
  FMigrator.CreateSchema(FConn);
  AssertEquals('Foreign key created', 1, Scalar('SELECT COUNT(*) FROM pragma_foreign_key_list(''MigChild'')'));
  R:=FMigrator.MigrateSchema(FConn, 2, [moDryRun]);
  AssertEquals('No differences', 0, R.DiffsFound);
end;

procedure TTestMigrationChainRun.TestRollbackFailureKeepsStepError;
var
  Msg: String;
begin
  FMigrator.CreateSchema(FConn);
  FMigrator.RegisterMigration(1, 2, @StepCommitsThenFails);
  Msg:='';
  try
    FMigrator.MigrateSchema(FConn, 2);
  except
    on E: Exception do
      Msg:=E.Message;
  end;
  AssertTrue('The step error is reported: '+Msg, Pos('Step failed on purpose', Msg)>0);
  AssertTrue('Connection transaction restored', FConn.Transaction=FTrans);
end;

procedure TTestMigrationChainRun.TestKeepIndexes;
var
  R: TOPMigrationResult;
begin
  FMigrator.CreateSchema(FConn);
  Exec('CREATE INDEX IX_Extra ON MigItem (Name)');
  R:=FMigrator.MigrateSchema(FConn, 2, [moDryRun]);
  AssertEquals('Drop candidate by default', 1, Length(R.SQL));
  FMigrator.MigrateSchema(FConn, 2, [moKeepIndexes]);
  AssertEquals('Index kept', 1, Scalar('SELECT COUNT(*) FROM sqlite_master WHERE name=''IX_Extra'''));
  AssertEquals('Migrated', '1,2', History);
end;

{ TTestTrackingTableDDL }

function TTestTrackingTableDDL.CreateSQL(aDialect: TSQLDialect; const aName: UTF8String): UTF8String;
var
  T: TOPMigrationTracker;
begin
  T:=TOPMigrationTracker.Create;
  try
    T.Dialect:=aDialect;
    T.TableName:=aName;
    Result:=T.GetCreateTableSQL;
  finally
    T.Free;
  end;
end;

procedure TTestTrackingTableDDL.TestFirebirdQuotesInternalName;
var
  S: UTF8String;
begin
  S:=CreateSQL(dFirebird3);
  AssertTrue('Quoted: '+S, Pos('CREATE TABLE "_opf_migrations"', S)=1);
end;

procedure TTestTrackingTableDDL.TestFirebirdCustomNameUnquoted;
begin
  AssertTrue(Pos('CREATE TABLE schema_version', CreateSQL(dFirebird3, 'schema_version'))=1);
end;

procedure TTestTrackingTableDDL.TestNoTextType;
var
  D: TSQLDialect;
  S: UTF8String;
begin
  for D in [dFirebird3, dPostGres, dMySQL, dSQLServer, dOracle] do
  begin
    S:=CreateSQL(D);
    AssertTrue('No TEXT type for '+D.ToString+': '+S, Pos(' TEXT', S)=0);
    if D=dSQLServer then
      AssertTrue('NVARCHAR(30) for '+D.ToString, Pos('AppliedAt NVARCHAR(30) NOT NULL', S)>0)
    else
      AssertTrue('VARCHAR(30) for '+D.ToString, Pos('AppliedAt VARCHAR(30) NOT NULL', S)>0);
  end;
end;

procedure TTestTrackingTableDDL.TestVersionIsPrimaryKey;
var
  S: UTF8String;
begin
  S:=CreateSQL(dPostGres);
  AssertTrue('Version NOT NULL: '+S, Pos('Version INT NOT NULL', S)>0);
  AssertTrue('Primary key on Version: '+S, Pos('PRIMARY KEY(Version)', S)>0);
  AssertTrue('No ID column: '+S, Pos('ID ', S)=0);
end;

procedure TTestTrackingTableDDL.TestSQLiteUnquoted;
begin
  AssertTrue(Pos('CREATE TABLE _opf_migrations', CreateSQL(dSQLite))=1);
end;

{ TTestSchemaTypeCompare }

procedure TTestSchemaTypeCompare.AddColumn(const aName, aType: UTF8String; aNullable: Boolean;
  aMaxLen: Integer; aPrec: Integer; aScale: Integer);
var
  L: Integer;
begin
  L:=Length(FSchema.Tables[0].Columns);
  SetLength(FSchema.Tables[0].Columns, L+1);
  FSchema.Tables[0].Columns[L].ColumnName:=aName;
  FSchema.Tables[0].Columns[L].DataType:=aType;
  FSchema.Tables[0].Columns[L].IsNullable:=aNullable;
  FSchema.Tables[0].Columns[L].MaxLength:=aMaxLen;
  FSchema.Tables[0].Columns[L].NumericPrecision:=aPrec;
  FSchema.Tables[0].Columns[L].NumericScale:=aScale;
end;

function TTestSchemaTypeCompare.Diffs(const aIgnore: array of UTF8String): TOPSchemaDiffArray;
begin
  Result:=TOPSchemaDiff.Compare(FRegistry, FRelations, FM2M, FSchema, FBuilder, aIgnore);
end;

function TTestSchemaTypeCompare.CountKind(const aDiffs: TOPSchemaDiffArray; aKind: TSchemaDiffKind;
  const aColumn: UTF8String): Integer;
var
  D: TOPSchemaDiffItem;
begin
  Result:=0;
  for D in aDiffs do
    if (D.Kind=aKind) and ((aColumn='') or SameText(D.ColumnName, aColumn)) then
      Inc(Result);
end;

{ The table as PostgreSQL reports a table created from the TMigItem model }

procedure TTestSchemaTypeCompare.BuildPGTable(aNameLen: Integer);
begin
  SetLength(FSchema.Tables, 1);
  FSchema.Tables[0].TableName:='migitem';
  FSchema.Tables[0].PrimaryKey:=['id'];
  AddColumn('id', 'integer', False);
  AddColumn('name', 'character varying', False, aNameLen);
  AddColumn('code', 'character', True, 10);
  AddColumn('big', 'bigint', True);
  AddColumn('created', 'timestamp without time zone', True);
  AddColumn('day', 'date', True);
  AddColumn('amount', 'numeric', True, 0, 18, 4);
  AddColumn('ratio', 'double precision', True);
  AddColumn('active', 'boolean', True);
end;

procedure TTestSchemaTypeCompare.SetUp;
var
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  D:=FRegistry.RegisterPersister(TMigItem, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options+[foInKey, foAutoInc]-[foInInsert];
  F:=D.FieldMap.FindByPropertyName('Name');
  F.MaxSize:=50;
  F.Options:=F.Options+[foRequired];
  F:=D.FieldMap.FindByPropertyName('Code');
  F.MaxSize:=10;
  F.MinSize:=10;
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  FBuilder:=TOPDDLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dPostGres;
  FBuilder.Registry:=FRegistry;
  FSchema:=TOPSchemaInfo.Create;
end;

procedure TTestSchemaTypeCompare.TearDown;
begin
  FreeAndNil(FSchema);
  FreeAndNil(FBuilder);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

procedure TTestSchemaTypeCompare.TestParseSQLType;
var
  B: UTF8String;
  P1, P2: Integer;
begin
  ParseSQLType('NUMERIC( 12 , 4 )', B, P1, P2);
  AssertEquals('Base', 'NUMERIC', B);
  AssertEquals('Precision', 12, P1);
  AssertEquals('Scale', 4, P2);
  ParseSQLType('varchar(50)', B, P1, P2);
  AssertEquals('Uppercased', 'VARCHAR', B);
  AssertEquals('Length', 50, P1);
  AssertEquals('No second', 0, P2);
  ParseSQLType('DOUBLE PRECISION', B, P1, P2);
  AssertEquals('Multi word', 'DOUBLE PRECISION', B);
  ParseSQLType('BLOB SUB_TYPE 1', B, P1, P2);
  AssertEquals('Firebird text blob', 'BLOB SUB_TYPE 1', B);
  ParseSQLType('TIMESTAMP(3) WITHOUT TIME ZONE', B, P1, P2);
  AssertEquals('Suffix kept', 'TIMESTAMP WITHOUT TIME ZONE', B);
  AssertEquals('Fraction digits', 3, P1);
end;

procedure TTestSchemaTypeCompare.TestCanonicalNames;
begin
  AssertEquals('INT4', 'INT', CanonicalTypeName('INT4'));
  AssertEquals('SERIAL', 'INT', CanonicalTypeName('SERIAL'));
  AssertEquals('BIGSERIAL', 'BIGINT', CanonicalTypeName('BIGSERIAL'));
  AssertEquals('CHARACTER VARYING', 'VARCHAR', CanonicalTypeName('CHARACTER VARYING'));
  AssertEquals('BPCHAR', 'CHAR', CanonicalTypeName('BPCHAR'));
  AssertEquals('NUMERIC', 'DECIMAL', CanonicalTypeName('NUMERIC'));
  AssertEquals('DATETIME2', 'TIMESTAMP', CanonicalTypeName('DATETIME2'));
  AssertEquals('TIMESTAMP WITHOUT TIME ZONE', 'TIMESTAMP', CanonicalTypeName('TIMESTAMP WITHOUT TIME ZONE'));
  AssertEquals('BLOB SUB_TYPE 1', 'TEXT', CanonicalTypeName('BLOB SUB_TYPE 1'));
  AssertEquals('BYTEA', 'BLOB', CanonicalTypeName('BYTEA'));
  AssertEquals('DOUBLE PRECISION', 'FLOAT', CanonicalTypeName('DOUBLE PRECISION'));
end;

procedure TTestSchemaTypeCompare.TestPostgresNamesMatch;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(50);
  D:=Diffs([]);
  AssertEquals('No type differences', 0, CountKind(D, sdkAlterColumnType));
  AssertEquals('No nullability differences', 0, CountKind(D, sdkAlterColumnNullable));
end;

procedure TTestSchemaTypeCompare.TestVarcharLengthMismatch;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(40);
  D:=Diffs([]);
  AssertEquals('Length change found', 1, CountKind(D, sdkAlterColumnType, 'Name'));
  AssertEquals('Only that one', 1, CountKind(D, sdkAlterColumnType));
end;

procedure TTestSchemaTypeCompare.TestDecimalPrecisionMismatch;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(50);
  FSchema.Tables[0].Columns[6].NumericScale:=2;
  D:=Diffs([]);
  AssertEquals('Scale change found', 1, CountKind(D, sdkAlterColumnType, 'Amount'));
end;

procedure TTestSchemaTypeCompare.TestPrimaryKeyReportedNullable;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(50);
  // SQLite reports primary key columns as nullable
  FSchema.Tables[0].Columns[0].IsNullable:=True;
  D:=Diffs([]);
  AssertEquals('Primary key column counts as NOT NULL', 0, CountKind(D, sdkAlterColumnNullable, 'ID'));
end;

procedure TTestSchemaTypeCompare.TestRequiredColumnNullableIsDiff;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(50);
  FSchema.Tables[0].Columns[1].IsNullable:=True;
  D:=Diffs([]);
  AssertEquals('Required column found nullable', 1, CountKind(D, sdkAlterColumnNullable, 'Name'));
end;

procedure TTestSchemaTypeCompare.TestIgnoredTableNotDropped;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(50);
  SetLength(FSchema.Tables, 2);
  FSchema.Tables[1].TableName:='schema_version';
  D:=Diffs(['SCHEMA_VERSION']);
  AssertEquals('Ignored, case insensitive', 0, CountKind(D, sdkDropTable));
end;

procedure TTestSchemaTypeCompare.TestUnignoredTableDropped;
var
  D: TOPSchemaDiffArray;
begin
  BuildPGTable(50);
  SetLength(FSchema.Tables, 2);
  FSchema.Tables[1].TableName:='schema_version';
  D:=Diffs([]);
  AssertEquals('Unknown table is a drop candidate', 1, CountKind(D, sdkDropTable));
end;

procedure TTestSchemaTypeCompare.TestAppendCharNamesMatch;
var
  D: TOPSchemaDiffArray;
  PD: TPersisterDef;
begin
  PD:=FRegistry.FindPersister(TMigItem);
  PD.TableName:='Order';
  PD.FieldMap.FindByPropertyName('Code').FieldName:='Group';
  FBuilder.Driver.ReservedWordStrategy:=rwsAppendChar;
  BuildPGTable(50);
  // As created: Order_, Name_ and Group_ (NAME is reserved in PostgreSQL)
  FSchema.Tables[0].TableName:='order_';
  FSchema.Tables[0].Columns[1].ColumnName:='name_';
  FSchema.Tables[0].Columns[2].ColumnName:='group_';
  D:=Diffs([]);
  AssertEquals('Table found', 0, CountKind(D, sdkAddTable));
  AssertEquals('Table not dropped', 0, CountKind(D, sdkDropTable));
  AssertEquals('Column found', 0, CountKind(D, sdkAddColumn));
  AssertEquals('Column not dropped', 0, CountKind(D, sdkDropColumn));
end;

procedure TTestSchemaTypeCompare.TestDropTablesReferencingFirst;
var
  D: TOPSchemaDiffArray;
  I, PosParent, PosChild: Integer;
begin
  BuildPGTable(50);
  SetLength(FSchema.Tables, 3);
  // Name order would drop the referenced table first
  FSchema.Tables[1].TableName:='aparent';
  FSchema.Tables[2].TableName:='bchild';
  SetLength(FSchema.Tables[2].ForeignKeys, 1);
  FSchema.Tables[2].ForeignKeys[0].ConstraintName:='fk_b_a';
  FSchema.Tables[2].ForeignKeys[0].ColumnName:='aid';
  FSchema.Tables[2].ForeignKeys[0].ReferencedTable:='aparent';
  FSchema.Tables[2].ForeignKeys[0].ReferencedColumn:='id';
  D:=Diffs([]);
  PosParent:=-1;
  PosChild:=-1;
  for I:=0 to High(D) do
    if D[I].Kind=sdkDropTable then
      if D[I].TableName='aparent' then
        PosParent:=I
      else if D[I].TableName='bchild' then
        PosChild:=I;
  AssertTrue('Both dropped', (PosParent>=0) and (PosChild>=0));
  AssertTrue('Referencing table first', PosChild<PosParent);
end;

procedure TTestSchemaTypeCompare.TestChangedRelationDropsOldForeignKeyFirst;
var
  D: TOPSchemaDiffArray;
  PD: TPersisterDef;
  Rel: TOPRelationDef;
  SQL: TUTF8StringArray;
  I, PosDrop, PosAdd: Integer;
begin
  BuildPGTable(50);
  PD:=FRegistry.RegisterPersister(TMigParent, TRTTIPersister);
  PD.FieldMap.FindByPropertyName('ID').Options:=PD.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  PD.FieldMap.FindByPropertyName('Name').MaxSize:=50;
  PD:=FRegistry.RegisterPersister(TMigChild, TRTTIPersister);
  PD.FieldMap.FindByPropertyName('ID').Options:=PD.FieldMap.FindByPropertyName('ID').Options+[foInKey];
  Rel:=FRelations.RegisterRelationship(TMigParent, TMigChild, 'ParentID');
  Rel.ConstraintName:='fk_rel';
  SetLength(FSchema.Tables, 3);
  FSchema.Tables[1].TableName:='migparent';
  FSchema.Tables[1].PrimaryKey:=['id'];
  FSchema.Tables[2].TableName:='migchild';
  FSchema.Tables[2].PrimaryKey:=['id'];
  // The relation used to be on OtherID, with the same constraint name
  SetLength(FSchema.Tables[2].ForeignKeys, 1);
  FSchema.Tables[2].ForeignKeys[0].ConstraintName:='fk_rel';
  FSchema.Tables[2].ForeignKeys[0].ColumnName:='otherid';
  FSchema.Tables[2].ForeignKeys[0].ReferencedTable:='migparent';
  FSchema.Tables[2].ForeignKeys[0].ReferencedColumn:='id';
  D:=Diffs([]);
  AssertEquals('Old foreign key dropped', 1, CountKind(D, sdkDropForeignKey));
  AssertEquals('New foreign key added', 1, CountKind(D, sdkAddForeignKey));
  SQL:=FBuilder.GenerateMigrationSQL(D, FRegistry, FRelations, FM2M);
  PosDrop:=-1;
  PosAdd:=-1;
  for I:=0 to High(SQL) do
    if Pos('DROP CONSTRAINT fk_rel', SQL[I])>0 then
      PosDrop:=I
    else if Pos('ADD CONSTRAINT fk_rel', SQL[I])>0 then
      PosAdd:=I;
  AssertTrue('Both statements', (PosDrop>=0) and (PosAdd>=0));
  AssertTrue('Same name: dropped before it is added', PosDrop<PosAdd);
end;

procedure TTestSchemaTypeCompare.TestSQLiteInt64AutoIncNoTypeDiff;
var
  D: TOPSchemaDiffArray;
begin
  FBuilder.SQLDialect:=dSQLite;
  FRegistry.FindPersister(TMigItem).FieldMap.FindByPropertyName('ID').PropertyKind:=tkInt64;
  BuildPGTable(50);
  // INTEGER PRIMARY KEY, as created for an auto-increment key on SQLite
  FSchema.Tables[0].Columns[0].DataType:='INTEGER';
  D:=Diffs([]);
  AssertEquals('The rowid is 64-bit: no type difference', 0, CountKind(D, sdkAlterColumnType, 'ID'));
end;

{ TTestMigrationStatements }

function TTestMigrationStatements.Field(aKind: TTypeKind; aOptions: TOPFieldOptions): TOPFieldMap;
begin
  Result:=FRegistry.FindPersister(TMigItem).FieldMap.FindByPropertyName('ID');
  Result.PropertyKind:=aKind;
  Result.Options:=aOptions;
end;

function TTestMigrationStatements.Diff(aKind: TSchemaDiffKind; const aTable, aColumn: UTF8String): TOPSchemaDiffArray;
begin
  Result:=nil;
  SetLength(Result, 1);
  Result[0]:=Default(TOPSchemaDiffItem);
  Result[0].Kind:=aKind;
  Result[0].TableName:=aTable;
  Result[0].ColumnName:=aColumn;
end;

procedure TTestMigrationStatements.SetUp;
begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(Nil);
  FRegistry.RegisterPersister(TMigItem, TRTTIPersister);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  FBuilder:=TOPDDLBuilder.Create(Nil);
  FBuilder.Registry:=FRegistry;
end;

procedure TTestMigrationStatements.TearDown;
begin
  FreeAndNil(FBuilder);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  inherited TearDown;
end;

procedure TTestMigrationStatements.TestMySQLWidenAutoIncKeepsAutoIncrement;
var
  S: UTF8String;
begin
  FBuilder.SQLDialect:=dMySQL;
  S:=FBuilder.GetAlterColumnTypeSQL('MigItem', 'ID', Field(tkInt64, [foInKey, foAutoInc, foInSelect]));
  // MODIFY redefines the column: without AUTO_INCREMENT the key is no longer generated
  AssertEquals('MODIFY with AUTO_INCREMENT',
    'ALTER TABLE MigItem MODIFY COLUMN ID BIGINT NOT NULL AUTO_INCREMENT', S);
end;

procedure TTestMigrationStatements.TestMySQLModifyKeepsDefault;
var
  F: TOPFieldMap;
begin
  FBuilder.SQLDialect:=dMySQL;
  F:=FRegistry.FindPersister(TMigItem).FieldMap.FindByPropertyName('Big');
  F.DefaultValue:=5;
  AssertEquals('Type change keeps the default',
    'ALTER TABLE MigItem MODIFY COLUMN Big BIGINT NULL DEFAULT 5',
    FBuilder.GetAlterColumnTypeSQL('MigItem', 'Big', F));
  AssertEquals('Null change keeps the default',
    'ALTER TABLE MigItem MODIFY COLUMN Big BIGINT NOT NULL DEFAULT 5',
    FBuilder.GetAlterColumnNullableSQL('MigItem', 'Big', False, F));
end;

procedure TTestMigrationStatements.TestPostgresWidenAutoIncAltersSequence;
var
  SQL: TUTF8StringArray;
begin
  FBuilder.SQLDialect:=dPostGres;
  Field(tkInt64, [foInKey, foAutoInc, foInSelect]);
  SQL:=FBuilder.GenerateMigrationSQL(Diff(sdkAlterColumnType, 'MigItem', 'ID'), FRegistry, FRelations, FM2M);
  AssertEquals('Two statements', 2, Length(SQL));
  AssertEquals('Column type', 'ALTER TABLE MigItem ALTER COLUMN ID TYPE BIGINT', SQL[0]);
  // The SERIAL sequence is AS integer: it would still overflow at 2^31
  AssertTrue('Sequence found by the column: '+SQL[1],
    Pos('pg_get_serial_sequence(''MigItem'', ''id'')', SQL[1])>0);
  AssertTrue('Sequence widened: '+SQL[1], Pos('ALTER SEQUENCE ''||s||'' AS BIGINT', SQL[1])>0);
  AssertEquals('No colon: sqldb would see a parameter', 0, Pos(':', SQL[1]));
end;

procedure TTestMigrationStatements.TestPostgresNarrowKeyNoSequenceChange;
begin
  FBuilder.SQLDialect:=dPostGres;
  AssertEquals('32-bit key', '', FBuilder.GetAlterAutoIncTypeSQL('MigItem', 'ID',
    Field(tkInteger, [foInKey, foAutoInc, foInSelect])));
  AssertEquals('Not auto-increment', '', FBuilder.GetAlterAutoIncTypeSQL('MigItem', 'ID',
    Field(tkInt64, [foInKey, foInSelect])));
  FBuilder.SQLDialect:=dMySQL;
  AssertEquals('Not PostgreSQL', '', FBuilder.GetAlterAutoIncTypeSQL('MigItem', 'ID',
    Field(tkInt64, [foInKey, foAutoInc, foInSelect])));
end;

procedure TTestMigrationStatements.TestNewTableKeepsPKConstraintName;
var
  SQL: TUTF8StringArray;
begin
  FBuilder.SQLDialect:=dPostGres;
  Field(tkInteger, [foInKey, foInSelect, foInInsert]);
  FRegistry.FindPersister(TMigItem).PKConstraintName:='pk_mig_custom';
  SQL:=FBuilder.GenerateMigrationSQL(Diff(sdkAddTable, 'MigItem', ''), FRegistry, FRelations, FM2M);
  AssertTrue('Named primary key: '+SQL[0], Pos('CONSTRAINT pk_mig_custom PRIMARY KEY', SQL[0])>0);
end;

procedure TTestMigrationStatements.TestForceQuotesMigration;
var
  M: TChainMigrator;
  B: TOPDDLBuilder;
  SQL: TUTF8StringArray;
begin
  M:=TChainMigrator.Create(Nil);
  try
    M.Registry:=FRegistry;
    M.SQLDialect:=dPostGres;
    M.CreateOptions:=[coForceQuotes];
    B:=M.Builder(Nil);
    try
      SQL:=B.GenerateMigrationSQL(Diff(sdkAddColumn, 'MigItem', 'Code'), FRegistry, FRelations, FM2M);
      // CREATE TABLE "MigItem" keeps the case: ALTER TABLE MigItem is another table
      AssertTrue('Quoted like the schema: '+SQL[0], Pos('ALTER TABLE "MigItem" ADD COLUMN "Code"', SQL[0])=1);
    finally
      B.Free;
    end;
  finally
    M.Free;
  end;
end;

procedure TTestMigrationStatements.TestSQLServerUnicodeStrings;
var
  F: TOPFieldMap;
begin
  FBuilder.SQLDialect:=dSQLServer;
  F:=FRegistry.FindPersister(TMigItem).FieldMap.FindByPropertyName('Name');
  F.MaxSize:=50;
  AssertEquals('Unicode by default', 'NVARCHAR(50)', FBuilder.GetPropertyDefSQL('MigItem', F));
  (FBuilder.Driver as TSQLDriverSQLServer).UnicodeStrings:=False;
  AssertEquals('Code page', 'VARCHAR(50)', FBuilder.GetPropertyDefSQL('MigItem', F));
  (FBuilder.Driver as TSQLDriverSQLServer).UnicodeStrings:=True;
  F.MinSize:=50;
  AssertEquals('Unicode fixed', 'NCHAR(50)', FBuilder.GetPropertyDefSQL('MigItem', F));
end;

procedure TTestMigrationStatements.TestSQLiteAddUniqueColumn;
var
  SQL: TUTF8StringArray;
  F: TOPFieldMap;
begin
  FBuilder.SQLDialect:=dSQLite;
  F:=FRegistry.FindPersister(TMigItem).FieldMap.FindByPropertyName('Code');
  F.MaxSize:=10;
  F.Options:=F.Options+[foUnique];
  SQL:=FBuilder.GenerateMigrationSQL(Diff(sdkAddColumn, 'MigItem', 'Code'), FRegistry, FRelations, FM2M);
  AssertEquals('Two statements', 2, Length(SQL));
  AssertEquals('Column without UNIQUE', 'ALTER TABLE MigItem ADD COLUMN Code VARCHAR(10)', SQL[0]);
  AssertEquals('Unique index', 'CREATE UNIQUE INDEX uq_MigItem_Code ON MigItem (Code)', SQL[1]);
end;

initialization
  RegisterTests([TTestMigrationChainPlan, TTestMigrationChainRun, TTestTrackingTableDDL, TTestSchemaTypeCompare,
    TTestMigrationStatements]);
end.
