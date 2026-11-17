{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Database schema migration

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Migration;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, db, sqldb,
{$ENDIF}
  Opf.Fields, Opf.DDL, Opf.Driver, Opf.Registry, Opf.Relations, Opf.M2M;

const
  cMigrationTable = '_opf_migrations';

type

  TOPMigrationRecord = record
    Version: Integer;
    AppliedAt: UTF8String;
    Description: UTF8String;
  end;

  TOPMigrationArray = array of TOPMigrationRecord;

  { moLogSQL: log the generated statements (lkSQL) before they are executed.
      To see them without executing them, use moDryRun (it logs them too).
    moDryRun: compute the statements and the custom steps, change nothing
      (not even the tracking table is created).
    moAllowDropColumn, moAllowDropTable: also drop columns and tables that
      the model does not have; without them these are skipped.
    moAllowNarrowing: apply column type changes that can lose data (shorter
      VARCHAR, fewer DECIMAL digits, another type family). Without it they are
      skipped with a warning.
    moKeepIndexes: do not drop indexes that the model does not declare
      (e.g. created by hand or by another application). Without it, as in
      earlier versions, they are dropped. }
  TMigrateOption = (moLogSQL, moDryRun, moAllowDropColumn, moAllowDropTable, moAllowNarrowing,
    moKeepIndexes);
  TMigrateOptions = set of TMigrateOption;

  TGenerateSQLOption = (gsoIncludeVersionTracking);
  TGenerateSQLOptions = set of TGenerateSQLOption;

  TOPMigrationStep = procedure(aConnection: TSQLConnection; aVersion: Integer) of object;

  TOPMigrationStepDef = record
    FromVersion: Integer;
    ToVersion: Integer;
    Step: TOPMigrationStep;
    Description: UTF8String;
  end;
  TOPMigrationStepArray = array of TOPMigrationStepDef;

  TOPMigrationResult = record
    DiffsFound: Integer;
    StatementsExecuted: Integer;
    CustomStepsExecuted: Integer;
    SQL: TUTF8StringArray;
    VersionBefore: Integer;
    // The version recorded in the database after the call
    VersionAfter: Integer;
    // The custom steps that (would) run, in order
    PlannedSteps: TOPMigrationStepArray;
  end;

  { What to do when the connection's own transaction is active when the
    migrator needs the connection:
    mtpCommitCaller: commit it, and log a warning
    mtpRaise: raise an exception, nothing is changed (default) }
  TOPMigrationTransactionPolicy = (mtpCommitCaller, mtpRaise);

  { TOPMigrationTracker:
    Records the applied versions in a table (one row per version, the version
    is the primary key). The table is created with the column types of the
    dialect; an existing table is used as it is, so a table created by an older
    version (with an ID column) keeps working. }

  TOPMigrationTracker = class(TObject)
  private
    FConnection: TSQLConnection;
    FTableName: UTF8String;
    FDialect: TSQLDialect;
    FBuilder: TOPDDLBuilder;
    FReservedWordStrategy: TReservedWordStrategy;
    function GetTableName: UTF8String;
    function GetBuilder: TOPDDLBuilder;
    // A column of the tracking table, written as in GetCreateTableSQL
    function ColumnName(const aName: UTF8String): UTF8String;
  public
    // Frees the tracker
    destructor Destroy; override;
    // The table name as it must appear in SQL (quoted when needed)
    function QuotedTableName: UTF8String;
    // True if the version tracking table exists in the database
    function HasMigrationTable: Boolean;
    // Creates the version tracking table if it does not exist yet
    procedure EnsureMigrationTable;
    // The highest version recorded in the database, or 0 if there is none
    function GetCurrentVersion: Integer;
    // Insert a version row, in the connection's current transaction
    procedure RecordMigration(aVersion: Integer; const aDescription: UTF8String);
    // Insert a version row in aTransaction
    procedure RecordMigration(aVersion: Integer; const aDescription: UTF8String;
      aTransaction: TSQLTransaction);
    // INSERT statement that records a version, with the values in the text
    function GetRecordMigrationSQL(aVersion: Integer; const aDescription: UTF8String): UTF8String;
    // All recorded versions, ordered by version
    function GetMigrationHistory: TOPMigrationArray;
    // CREATE TABLE statement for the tracking table
    function GetCreateTableSQL: UTF8String;
    // The database connection that holds the tracking table
    property Connection: TSQLConnection read FConnection write FConnection;
    { Table name for migration tracking. Defaults to cMigrationTable ('_opf_migrations'). }
    property TableName: UTF8String read GetTableName write FTableName;
    // Dialect for the table definition and identifier quoting
    property Dialect: TSQLDialect read FDialect write FDialect;
    // How names that are reserved words in SQL are written
    property ReservedWordStrategy: TReservedWordStrategy read FReservedWordStrategy write FReservedWordStrategy;
  end;

  { TOPSchemaMigrator }

  TOPSchemaMigrator = class(TComponent)
  private
    FMigrationTableName: UTF8String;
    FMigrationSteps: TOPMigrationStepArray;
    FRegistry: TOPRegistry;
    FRelationRegistry: TOPRelationRegistry;
    FM2MRegistry: TOPManyToManyRegistry;
    FSQLDialect: TSQLDialect;
    FSQLDialectSet: Boolean;
    FTransactionPolicy: TOPMigrationTransactionPolicy;
    FReservedWordStrategy: TReservedWordStrategy;
    FCreateOptions: TSQLCreateOptions;
    // Version recorded inside the DDL transaction (DoRecordPendingVersion)
    FPendingTracker: TOPMigrationTracker;
    FPendingVersion: Integer;
    FPendingDescription: UTF8String;
    procedure DoRecordPendingVersion(aConnection: TSQLConnection; aTransaction: TSQLTransaction);
    function GetEffectiveTableName: UTF8String;
    procedure SetSQLDialect(aValue: TSQLDialect);
  protected
    procedure CheckRegistries; virtual;
    { Creates a builder for changes to the database structure of this connection; the caller frees it }
    function CreateDDLBuilder(aConnection: TSQLConnection): TOPDDLBuilder; virtual;
    { The dialect: SQLDialect when it was set, otherwise detected from the connection. }
    function ResolveDialect(aConnection: TSQLConnection): TSQLDialect; virtual;
    { The connection's own transaction is active: apply TransactionPolicy. }
    procedure ReleaseCallerTransaction(aConnection: TSQLConnection); virtual;
    { Find registered custom migration steps matching the version transition exactly. }
    function FindMatchingSteps(aFromVersion, aToVersion: Integer): TOPMigrationStepArray; virtual;
    { The steps to run to go from aFromVersion to aToVersion, in order: steps are
      followed from version to version (1->2, 2->3, ...). Raises when steps that
      start from the same version end at different versions, when a step goes
      beyond aToVersion, or when a version in between has no step while later
      steps exist. Steps above aToVersion are ignored. From version 0 (a
      fresh database, created from the model like CreateSchema creates
      version 1) the chain starts at version 1, unless a step starts at 0. }
    function BuildStepChain(aFromVersion, aToVersion: Integer): TOPMigrationStepArray; virtual;
    { Introspect DB schema, compare with registered model, filter destructive
      operations, generate migration SQL. Returns the SQL statements and
      populates Result.DiffsFound and Result.SQL. }
    procedure ComputeSchemaDiffs(aConnection: TSQLConnection;
      aOptions: TMigrateOptions; var aResult: TOPMigrationResult); virtual;
    { Execute the generated DDL statements. With transactional DDL (PostgreSQL,
      SQLite) they run in one transaction: a failure leaves the schema as it was.
      aRecordVersion > 0: record that version in the same transaction.
      Populates Result.StatementsExecuted. }
    procedure ExecuteDDLStatements(aConnection: TSQLConnection;
      var aResult: TOPMigrationResult; aRecordVersion: Integer = 0); virtual;
    { Run user-registered Pascal migration callbacks. The steps of one version
      transition run in one transaction, together with recording the version
      they lead to: a failing step leaves the database at the previous version,
      and a later run continues there. Each step receives the version its
      transition leads to. Populates Result.CustomStepsExecuted. }
    procedure ExecuteCustomSteps(aConnection: TSQLConnection;
      const aSteps: TOPMigrationStepArray; aTargetVersion: Integer;
      var aResult: TOPMigrationResult); virtual;
    { Record the migration version in the tracking table, unless it is already
      at that version (custom steps record their versions themselves). }
    procedure RecordMigrationVersion(aConnection: TSQLConnection;
      aTargetVersion: Integer); virtual;
    { Create and configure a migration tracker for the given connection. Caller frees. }
    function CreateTracker(aConnection: TSQLConnection): TOPMigrationTracker; virtual;
  public
    // Creates a schema migrator
    constructor Create(AOwner: TComponent); override;
    // Adds a custom step that brings the database from one version to another
    procedure RegisterMigration(aFromVersion, aToVersion: Integer;
      aStep: TOPMigrationStep; const aDescription: UTF8String = '');
    { Creates all tables of the model in the database and records version 1,
      unless a version is already recorded (a repeated call with coIfNotExists).
      On SQLite the foreign keys are part of CREATE TABLE. }
    procedure CreateSchema(aConnection: TSQLConnection;
      aCreateOptions: TSQLCreateOptions = [];
      aSchemaOptions: TSchemaCreateOptions = [scoForeignKeys, scoIndexes, scoJunctionTables]);
    { Bring the database to aTargetVersion: schema changes first (computed
      against the current model), then the custom steps from the current
      version up to aTargetVersion, then the target version is recorded.
      Note that custom steps run after the schema changes: a step cannot use a
      column that the current model no longer has. }
    function MigrateSchema(aConnection: TSQLConnection;
      aTargetVersion: Integer;
      aOptions: TMigrateOptions = []): TOPMigrationResult;
    { Returns the SQL statements that would bring the database to aTargetVersion,
      without running them. Custom steps cannot be part of the script: when
      steps are due, the script records the version where the first one
      starts (a warning is logged), and MigrateSchema runs the steps later. }
    function GenerateMigrationSQL(aConnection: TSQLConnection;
      aTargetVersion: Integer;
      aOptions: TMigrateOptions = [];
      aGenerateOptions: TGenerateSQLOptions = [gsoIncludeVersionTracking]): TUTF8StringArray;
  published
    { Migration table name. Defaults to cMigrationTable ('_opf_migrations').
      Set before calling CreateSchema/MigrateSchema/GenerateMigrationSQL. }
    property MigrationTableName: UTF8String read FMigrationTableName write FMigrationTableName;
    // The registered classes whose tables make up the schema
    property Registry: TOPRegistry read FRegistry write FRegistry;
    // The registered relations between classes, used for foreign keys
    property RelationRegistry: TOPRelationRegistry read FRelationRegistry write FRelationRegistry;
    // The registered many-to-many relations, used for link tables
    property M2MRegistry: TOPManyToManyRegistry read FM2MRegistry write FM2MRegistry;
    { The kind of database; when not set it is found from the connection }
    property SQLDialect: TSQLDialect read FSQLDialect write SetSQLDialect stored FSQLDialectSet;
    // What to do when the connection's own transaction is active
    property TransactionPolicy: TOPMigrationTransactionPolicy read FTransactionPolicy
      write FTransactionPolicy default mtpRaise;
    { Must match the strategy the application's SQL and DDL builders use, so
      generated statements name the columns the same way. }
    property ReservedWordStrategy: TReservedWordStrategy read FReservedWordStrategy
      write FReservedWordStrategy default rwsNone;
    { Options for creating the schema, also used by MigrateSchema and
      GenerateMigrationSQL. Set coForceQuotes when the schema was created
      with it (it is added to the options passed to CreateSchema): all names
      in migration statements are then quoted too, so they match the
      case-sensitive names of such a schema. }
    property CreateOptions: TSQLCreateOptions read FCreateOptions write FCreateOptions default [];
  end;

implementation

uses Opf.Schema, Opf.Sql, Opf.Logging, Opf.Types, Opf.Consts, Opf.Transaction;

const
  // Width of the AppliedAt column: 'yyyy-mm-dd hh:nn:ss' plus room
  cAppliedAtLength = 30;
  cDescriptionLength = 255;

{ Commit the connection's transaction when this code started it (it was not
  active before): reading metadata must not leave a transaction open, but
  must not commit work of the caller either. }

procedure EndOwnTransaction(aConnection: TSQLConnection; aWasActive: Boolean);
begin
  if (not aWasActive) and Assigned(aConnection.Transaction)
     and aConnection.Transaction.Active then
    aConnection.Transaction.Commit;
end;

function TransactionActive(aConnection: TSQLConnection): Boolean;
begin
  Result:=Assigned(aConnection.Transaction) and aConnection.Transaction.Active;
end;

{ Within a migrator method, after ReleaseCallerTransaction: an active
  connection transaction can only have been started by reading metadata
  (sqldb itself starts it, e.g. to read index definitions). }

procedure CommitStray(aConnection: TSQLConnection);
begin
  if TransactionActive(aConnection) then
    aConnection.Transaction.Commit;
end;

{ In a finally block, after an error: roll back without raising, so the
  original error is kept and the connection's transaction can be restored.
  A failed rollback leaves the transaction marked inactive. }

procedure RollbackQuietly(aTrans: TOPFTransaction);
begin
  if not aTrans.Active then
    Exit;
  try
    aTrans.Rollback;
  except
    on E: Exception do
    begin
      {$IFNDEF OPF_SKIP_LOGGING}
      OPLogger.LogWarning(nil, 'Migration: rollback failed: %s', [E.Message]);
      {$ENDIF}
    end;
  end;
  if aTrans.Active then
    aTrans.ForceInactive;
end;

{ TOPMigrationTracker }

destructor TOPMigrationTracker.Destroy;
begin
  FreeAndNil(FBuilder);
  inherited Destroy;
end;

function TOPMigrationTracker.GetTableName: UTF8String;
begin
  if FTableName = '' then
    Result := cMigrationTable
  else
    Result := FTableName;
end;

function TOPMigrationTracker.GetBuilder: TOPDDLBuilder;
begin
  if FBuilder=Nil then
  begin
    FBuilder:=TOPDDLBuilder.Create(nil);
    FBuilder.SQLDialect:=FDialect;
  end
  else if FBuilder.SQLDialect<>FDialect then
    FBuilder.SQLDialect:=FDialect;
  FBuilder.Driver.ReservedWordStrategy:=FReservedWordStrategy;
  Result:=FBuilder;
end;

function TOPMigrationTracker.ColumnName(const aName: UTF8String): UTF8String;
begin
  // rwsAlwaysQuote creates "Version": a bare Version would be another name
  // on databases that fold unquoted names
  Result:=GetBuilder.MaybeQuoteIdentifier(aName);
end;

function TOPMigrationTracker.QuotedTableName: UTF8String;
begin
  // Quoted when the name is not a regular identifier for the dialect,
  // e.g. _opf_migrations on Firebird
  Result:=GetBuilder.MaybeQuoteIdentifier(TableName, itTable);
end;

function TOPMigrationTracker.HasMigrationTable: Boolean;
var
  Tables: TStringList;
  I: Integer;
  TN: UTF8String;
  WasActive: Boolean;
begin
  Tables := TStringList.Create;
  try
    WasActive := TransactionActive(FConnection);
    FConnection.GetTableNames(Tables);
    EndOwnTransaction(FConnection, WasActive);
    TN := UpperCase(TableName);
    Result := False;
    for I := 0 to Tables.Count - 1 do
      if UpperCase(Trim(Tables[I])) = TN then
      begin
        Result := True;
        Break;
      end;
  finally
    Tables.Free;
  end;
end;

function TOPMigrationTracker.GetCreateTableSQL: UTF8String;
var
  Map: TOPFieldMapList;
  F: TOPFieldMap;
begin
  Map := TOPFieldMapList.Create(TOPFieldMap);
  try
    // The version is the key: recording a version twice fails
    F := Map.AddField('Version', 'Version', [foInKey, foRequired, foInSelect, foInInsert]);
    F.PropertyKind := tkInteger;
    F := Map.AddField('AppliedAt', 'AppliedAt', [foRequired, foInSelect, foInInsert]);
    F.PropertyKind := tkAString;
    F.MaxSize := cAppliedAtLength;
    F := Map.AddField('Description', 'Description', [foInSelect, foInInsert]);
    F.PropertyKind := tkAString;
    F.MaxSize := cDescriptionLength;
    Result := GetBuilder.GetCreateSQL(TableName, Map.AsArray);
  finally
    Map.Free;
  end;
end;

procedure TOPMigrationTracker.EnsureMigrationTable;
begin
  if HasMigrationTable then
    Exit;
  GetBuilder.ExecDDL(FConnection, GetCreateTableSQL);
end;

function TOPMigrationTracker.GetCurrentVersion: Integer;
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  Result := 0;
  if not HasMigrationTable then
    Exit;
  Trans := TSQLTransaction.Create(nil);
  try
    Trans.Database := FConnection;
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := FConnection;
      Q.Transaction := Trans;
      // No index definitions: sqldb would read them in the connection's transaction
      Q.UsePrimaryKeyAsKey := False;
      Q.SQL.Text := 'SELECT MAX(' + ColumnName('Version') + ') FROM ' + QuotedTableName;
      Trans.StartTransaction;
      Q.Open;
      if (not Q.EOF) and (not Q.Fields[0].IsNull) then
        Result := Q.Fields[0].AsInteger;
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

procedure TOPMigrationTracker.RecordMigration(aVersion: Integer; const aDescription: UTF8String);
begin
  RecordMigration(aVersion, aDescription, FConnection.Transaction);
end;

procedure TOPMigrationTracker.RecordMigration(aVersion: Integer; const aDescription: UTF8String;
  aTransaction: TSQLTransaction);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := aTransaction;
    // Explicit column list: also works for an older table with an ID column
    Q.SQL.Text := 'INSERT INTO ' + QuotedTableName +
      ' (' + ColumnName('Version') + ', ' + ColumnName('AppliedAt') + ', ' + ColumnName('Description') +
      ') VALUES (:Version, :AppliedAt, :Description)';
    Q.Params.ParamByName('Version').AsInteger := aVersion;
    Q.Params.ParamByName('AppliedAt').AsString := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
    Q.Params.ParamByName('Description').AsString := Copy(aDescription, 1, cDescriptionLength);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

function TOPMigrationTracker.GetRecordMigrationSQL(aVersion: Integer; const aDescription: UTF8String): UTF8String;
begin
  Result := Format('INSERT INTO %s (%s, %s, %s) VALUES (%d, %s, %s)',
    [QuotedTableName, ColumnName('Version'), ColumnName('AppliedAt'), ColumnName('Description'),
     aVersion, GetBuilder.Driver.QuoteStringLiteral(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)),
     GetBuilder.Driver.QuoteStringLiteral(Copy(aDescription, 1, cDescriptionLength))]);
end;

function TOPMigrationTracker.GetMigrationHistory: TOPMigrationArray;
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
  Len: Integer;
begin
  Result := nil;
  if not HasMigrationTable then
    Exit;
  Len := 0;
  Trans := TSQLTransaction.Create(nil);
  try
    Trans.Database := FConnection;
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := FConnection;
      Q.Transaction := Trans;
      Q.UsePrimaryKeyAsKey := False;
      Q.SQL.Text := 'SELECT ' + ColumnName('Version') + ', ' + ColumnName('AppliedAt') + ', ' +
        ColumnName('Description') + ' FROM ' + QuotedTableName + ' ORDER BY ' + ColumnName('Version');
      Trans.StartTransaction;
      Q.Open;
      while not Q.EOF do
      begin
        SetLength(Result, Len + 1);
        Result[Len].Version := Q.Fields[0].AsInteger;
        Result[Len].AppliedAt := Trim(Q.Fields[1].AsString);
        Result[Len].Description := Q.Fields[2].AsString;
        Inc(Len);
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

{ TOPSchemaMigrator }

constructor TOPSchemaMigrator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransactionPolicy := mtpRaise;
end;

procedure TOPSchemaMigrator.CheckRegistries;
begin
  if FRegistry = nil then
    raise Exception.Create('TOPSchemaMigrator: Registry not assigned');
  if FRelationRegistry = nil then
    raise Exception.Create('TOPSchemaMigrator: RelationRegistry not assigned');
  if FM2MRegistry = nil then
    raise Exception.Create('TOPSchemaMigrator: M2MRegistry not assigned');
end;

procedure TOPSchemaMigrator.SetSQLDialect(aValue: TSQLDialect);
begin
  FSQLDialect := aValue;
  FSQLDialectSet := True;
end;

function TOPSchemaMigrator.ResolveDialect(aConnection: TSQLConnection): TSQLDialect;
begin
  if FSQLDialectSet then
    Exit(FSQLDialect);
  // Detection needs an open connection to tell Firebird 3+ from older versions
  if not aConnection.Connected then
    aConnection.Open;
  Result := DetectDialectFromConnection(aConnection);
end;

procedure TOPSchemaMigrator.ReleaseCallerTransaction(aConnection: TSQLConnection);
begin
  if not TransactionActive(aConnection) then
    Exit;
  if FTransactionPolicy = mtpRaise then
    raise EOPF.CreateFmt(SErrMigrationCallerTransaction, [aConnection.Name]);
  {$IFNDEF OPF_SKIP_LOGGING}
  OPLogger.LogWarning(Self, 'Migration: committing the active transaction of connection %s',
    [aConnection.Name]);
  {$ENDIF}
  aConnection.Transaction.Commit;
end;

function TOPSchemaMigrator.GetEffectiveTableName: UTF8String;
begin
  if FMigrationTableName = '' then
    Result := cMigrationTable
  else
    Result := FMigrationTableName;
end;

function TOPSchemaMigrator.CreateTracker(aConnection: TSQLConnection): TOPMigrationTracker;
begin
  Result := TOPMigrationTracker.Create;
  Result.Connection := aConnection;
  Result.TableName := GetEffectiveTableName;
  Result.Dialect := ResolveDialect(aConnection);
  Result.ReservedWordStrategy := FReservedWordStrategy;
end;

function TOPSchemaMigrator.CreateDDLBuilder(aConnection: TSQLConnection): TOPDDLBuilder;
begin
  Result := TOPDDLBuilder.Create(nil);
  try
    Result.SQLDialect := ResolveDialect(aConnection);
    Result.Driver.ReservedWordStrategy := FReservedWordStrategy;
    Result.Registry := FRegistry;
    // A schema created with quoted names is changed with quoted names
    if coForceQuotes in FCreateOptions then
      Result.Options := Result.Options + [sboQuoteIdentifiers];
  except
    Result.Free;
    raise;
  end;
end;

function TOPSchemaMigrator.FindMatchingSteps(aFromVersion, aToVersion: Integer): TOPMigrationStepArray;
var
  I, Len: Integer;
begin
  Result := nil;
  Len := 0;
  for I := 0 to Length(FMigrationSteps) - 1 do
    if (FMigrationSteps[I].FromVersion = aFromVersion) and
       (FMigrationSteps[I].ToVersion = aToVersion) then
    begin
      SetLength(Result, Len + 1);
      Result[Len] := FMigrationSteps[I];
      Inc(Len);
    end;
end;

function TOPSchemaMigrator.BuildStepChain(aFromVersion, aToVersion: Integer): TOPMigrationStepArray;
var
  V, I, NextV, Len, NextFrom: Integer;
  Found: Boolean;
begin
  Result := nil;
  Len := 0;
  V := aFromVersion;
  { Version 0 is a database without recorded version (a fresh install): the
    schema is created from the model, like CreateSchema does for version 1.
    Without a step from version 0, the chain starts at version 1. }
  if V = 0 then
  begin
    Found := False;
    for I := 0 to Length(FMigrationSteps) - 1 do
      if FMigrationSteps[I].FromVersion = 0 then
        Found := True;
    if not Found then
      V := 1;
  end;
  while V < aToVersion do
  begin
    // All steps leaving V, in registration order: they must agree on the target
    Found := False;
    NextV := 0;
    for I := 0 to Length(FMigrationSteps) - 1 do
      if FMigrationSteps[I].FromVersion = V then
      begin
        if not Found then
          NextV := FMigrationSteps[I].ToVersion
        else if FMigrationSteps[I].ToVersion <> NextV then
          raise EOPF.CreateFmt(SErrMigrationAmbiguousStep,
            [V, NextV, FMigrationSteps[I].ToVersion]);
        Found := True;
      end;
    if not Found then
    begin
      // No step from V: fine when no step lies ahead, the rest is schema only
      NextFrom := MaxInt;
      for I := 0 to Length(FMigrationSteps) - 1 do
        if (FMigrationSteps[I].FromVersion > V) and (FMigrationSteps[I].FromVersion < aToVersion)
           and (FMigrationSteps[I].FromVersion < NextFrom) then
          NextFrom := FMigrationSteps[I].FromVersion;
      if NextFrom <> MaxInt then
        raise EOPF.CreateFmt(SErrMigrationStepGap, [V, NextFrom]);
      Break;
    end;
    if NextV <= V then
      raise EOPF.CreateFmt(SErrMigrationStepBackwards, [V, NextV]);
    if NextV > aToVersion then
      raise EOPF.CreateFmt(SErrMigrationStepOvershoot, [V, NextV, aToVersion]);
    for I := 0 to Length(FMigrationSteps) - 1 do
      if FMigrationSteps[I].FromVersion = V then
      begin
        SetLength(Result, Len + 1);
        Result[Len] := FMigrationSteps[I];
        Inc(Len);
      end;
    V := NextV;
  end;
end;

procedure TOPSchemaMigrator.ComputeSchemaDiffs(aConnection: TSQLConnection;
  aOptions: TMigrateOptions; var aResult: TOPMigrationResult);
var
  Builder: TOPDDLBuilder;
  Schema: TOPSchemaInfo;
  Diffs, Filtered: TOPSchemaDiffArray;
  I, Len: Integer;
  Dialect: TSQLDialect;
begin
  Schema := nil;
  Builder := CreateDDLBuilder(aConnection);
  try
    CommitStray(aConnection);
    Schema := Builder.IntrospectSchema(aConnection);
    CommitStray(aConnection);
    // The tracking table is never part of the model
    Diffs := TOPSchemaDiff.Compare(FRegistry, FRelationRegistry, FM2MRegistry, Schema, Builder,
      [GetEffectiveTableName]);
    aResult.DiffsFound := Length(Diffs);
    Dialect := Builder.SQLDialect;
    { Filter destructive operations }
    Filtered := nil;
    Len := 0;
    for I := 0 to Length(Diffs) - 1 do
    begin
      if (Diffs[I].Kind = sdkDropColumn) and not (moAllowDropColumn in aOptions) then
        Continue;
      if (Diffs[I].Kind = sdkDropTable) and not (moAllowDropTable in aOptions) then
        Continue;
      if (Diffs[I].Kind = sdkDropIndex) and (moKeepIndexes in aOptions) then
        Continue;
      // SQLite cannot add or drop a constraint of an existing table
      if (Dialect = dSQLite) and (Diffs[I].Kind in [sdkAddForeignKey, sdkDropForeignKey]) then
      begin
        {$IFNDEF OPF_SKIP_LOGGING}
        OPLogger.LogWarning(Self, 'MigrateSchema: SQLite cannot change foreign key %s of table %s',
          [Diffs[I].ColumnName, Diffs[I].TableName]);
        {$ENDIF}
        Continue;
      end;
      // Changes the database cannot make: skipped, so the rest still applies
      if ((Diffs[I].Kind = sdkAlterColumnType) and not Builder.Driver.CanAlterColumnType)
         or ((Diffs[I].Kind = sdkAlterColumnNullable) and not Builder.Driver.CanAlterNullability) then
      begin
        {$IFNDEF OPF_SKIP_LOGGING}
        OPLogger.LogWarning(Self, 'MigrateSchema: %s cannot change column %s of table %s (%s)',
          [Dialect.ToString, Diffs[I].ColumnName, Diffs[I].TableName, Diffs[I].Detail]);
        {$ENDIF}
        Continue;
      end;
      // A type change that can lose data needs moAllowNarrowing
      if (Diffs[I].Kind = sdkAlterColumnType) and Diffs[I].Narrowing
         and not (moAllowNarrowing in aOptions) then
      begin
        {$IFNDEF OPF_SKIP_LOGGING}
        OPLogger.LogWarning(Self, 'MigrateSchema: skipped narrowing change of column %s of table %s (%s): use moAllowNarrowing',
          [Diffs[I].ColumnName, Diffs[I].TableName, Diffs[I].Detail]);
        {$ENDIF}
        Continue;
      end;
      SetLength(Filtered, Len + 1);
      Filtered[Len] := Diffs[I];
      Inc(Len);
    end;
    {$IFNDEF OPF_SKIP_LOGGING}
    if OPLogger.IsKindActive(lkInfo) then
      OPLogger.LogInfo(nil, 'MigrateSchema: %d diffs found, %d after filtering',
        [Length(Diffs), Length(Filtered)]);
    {$ENDIF}
    aResult.SQL := Builder.GenerateMigrationSQL(Filtered, FRegistry, FRelationRegistry, FM2MRegistry);
  finally
    Schema.Free;
    Builder.Free;
  end;
end;

procedure TOPSchemaMigrator.DoRecordPendingVersion(aConnection: TSQLConnection;
  aTransaction: TSQLTransaction);
begin
  // aTransaction: the connection's own transaction may be another one
  if Assigned(FPendingTracker) and (FPendingVersion > 0) then
    FPendingTracker.RecordMigration(FPendingVersion, FPendingDescription, aTransaction);
end;

procedure TOPSchemaMigrator.ExecuteDDLStatements(aConnection: TSQLConnection;
  var aResult: TOPMigrationResult; aRecordVersion: Integer);
var
  Builder: TOPDDLBuilder;
begin
  if (Length(aResult.SQL) = 0) and (aRecordVersion <= 0) then
    Exit;
  CommitStray(aConnection);
  Builder := CreateDDLBuilder(aConnection);
  try
    if aRecordVersion > 0 then
      FPendingTracker := CreateTracker(aConnection);
    try
      FPendingVersion := aRecordVersion;
      FPendingDescription := Format('Migration to version %d', [aRecordVersion]);
      Builder.ExecDDLList(aConnection, aResult.SQL, @DoRecordPendingVersion);
      aResult.StatementsExecuted := Length(aResult.SQL);
    finally
      FreeAndNil(FPendingTracker);
      FPendingVersion := 0;
    end;
  finally
    Builder.Free;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if OPLogger.IsKindActive(lkInfo) then
    OPLogger.LogInfo(nil, 'MigrateSchema: %d statements executed',
      [aResult.StatementsExecuted]);
  {$ENDIF}
end;

procedure TOPSchemaMigrator.ExecuteCustomSteps(aConnection: TSQLConnection;
  const aSteps: TOPMigrationStepArray; aTargetVersion: Integer;
  var aResult: TOPMigrationResult);
var
  I, GroupStart, GroupEnd, ToVersion: Integer;
  SavedTrans: TSQLTransaction;
  StepTrans: TOPFTransaction;
  Tracker: TOPMigrationTracker;
  Desc: UTF8String;
begin
  if Length(aSteps) = 0 then
    Exit;
  {$IFNDEF OPF_SKIP_LOGGING}
  if OPLogger.IsKindActive(lkInfo) then
    OPLogger.LogInfo(nil, 'MigrateSchema: executing %d custom migration steps',
      [Length(aSteps)]);
  {$ENDIF}
  Tracker := CreateTracker(aConnection);
  try
    GroupStart := 0;
    while GroupStart < Length(aSteps) do
    begin
      // One group: the consecutive steps of the same version transition
      ToVersion := aSteps[GroupStart].ToVersion;
      GroupEnd := GroupStart;
      while (GroupEnd + 1 < Length(aSteps))
            and (aSteps[GroupEnd + 1].FromVersion = aSteps[GroupStart].FromVersion) do
        Inc(GroupEnd);
      CommitStray(aConnection);
      SavedTrans := aConnection.Transaction;
      StepTrans := TOPFTransaction.Create(nil);
      try
        StepTrans.Database := aConnection;
        aConnection.Transaction := StepTrans;
        StepTrans.StartTransaction;
        for I := GroupStart to GroupEnd do
        begin
          if aSteps[I].Description <> '' then
            Desc := aSteps[I].Description
          else
            Desc := '(no description)';
          {$IFNDEF OPF_SKIP_LOGGING}
          if OPLogger.IsKindActive(lkInfo) then
            OPLogger.LogInfo(nil, 'MigrateSchema: custom step %d/%d (%d -> %d): %s',
              [I + 1, Length(aSteps), aSteps[I].FromVersion, aSteps[I].ToVersion, Desc]);
          {$ENDIF}
          aSteps[I].Step(aConnection, ToVersion);
        end;
        // Same transaction: the version is recorded if and only if the steps succeed
        Tracker.RecordMigration(ToVersion, Format('Migration to version %d', [ToVersion]));
        StepTrans.Commit;
      finally
        RollbackQuietly(StepTrans);
        aConnection.Transaction := SavedTrans;
        StepTrans.Free;
      end;
      Inc(aResult.CustomStepsExecuted, GroupEnd - GroupStart + 1);
      aResult.VersionAfter := ToVersion;
      GroupStart := GroupEnd + 1;
    end;
  finally
    Tracker.Free;
  end;
end;

procedure TOPSchemaMigrator.RecordMigrationVersion(aConnection: TSQLConnection;
  aTargetVersion: Integer);
var
  Tracker: TOPMigrationTracker;
  SavedTrans: TSQLTransaction;
  RecTrans: TOPFTransaction;
begin
  Tracker := CreateTracker(aConnection);
  try
    // Custom steps may already have recorded it
    if Tracker.GetCurrentVersion >= aTargetVersion then
      Exit;
    CommitStray(aConnection);
    SavedTrans := aConnection.Transaction;
    RecTrans := TOPFTransaction.Create(nil);
    try
      RecTrans.Database := aConnection;
      aConnection.Transaction := RecTrans;
      RecTrans.StartTransaction;
      Tracker.RecordMigration(aTargetVersion,
        Format('Migration to version %d', [aTargetVersion]));
      RecTrans.Commit;
    finally
      RollbackQuietly(RecTrans);
      aConnection.Transaction := SavedTrans;
      RecTrans.Free;
    end;
  finally
    Tracker.Free;
  end;
end;

procedure TOPSchemaMigrator.RegisterMigration(aFromVersion, aToVersion: Integer;
  aStep: TOPMigrationStep; const aDescription: UTF8String);
var
  Len: Integer;
begin
  if aStep = nil then
    raise EOPF.Create('RegisterMigration: Migration step callback is nil');
  Len:=Length(FMigrationSteps);
  SetLength(FMigrationSteps, Len+1);
  FMigrationSteps[Len].FromVersion:=aFromVersion;
  FMigrationSteps[Len].ToVersion:=aToVersion;
  FMigrationSteps[Len].Step:=aStep;
  FMigrationSteps[Len].Description:=aDescription;
end;

procedure TOPSchemaMigrator.CreateSchema(aConnection: TSQLConnection;
  aCreateOptions: TSQLCreateOptions;
  aSchemaOptions: TSchemaCreateOptions);
var
  Builder: TOPDDLBuilder;
  Statements: TUTF8StringArray;
begin
  CheckRegistries;
  ReleaseCallerTransaction(aConnection);
  // The same dialect as MigrateSchema uses, so a later migration finds no differences
  Builder := CreateDDLBuilder(aConnection);
  try
    FPendingTracker := CreateTracker(aConnection);
    try
      Statements := Builder.GetCreateSchemaSQL(FRegistry, FRelationRegistry, FM2MRegistry,
        aCreateOptions + FCreateOptions, aSchemaOptions);
      if not FPendingTracker.HasMigrationTable then
      begin
        Statements := Concat(Statements, [FPendingTracker.GetCreateTableSQL]);
        FPendingVersion := 1;
      end
      // Repeated (coIfNotExists): version 1 is recorded once
      else if FPendingTracker.GetCurrentVersion < 1 then
        FPendingVersion := 1
      else
        FPendingVersion := 0;
      CommitStray(aConnection);
      // With transactional DDL: all tables and version 1, or nothing
      FPendingDescription := 'Initial schema creation';
      Builder.ExecDDLList(aConnection, Statements, @DoRecordPendingVersion);
    finally
      FreeAndNil(FPendingTracker);
      FPendingVersion := 0;
    end;
  finally
    Builder.Free;
  end;
end;

function TOPSchemaMigrator.MigrateSchema(aConnection: TSQLConnection;
  aTargetVersion: Integer;
  aOptions: TMigrateOptions): TOPMigrationResult;
var
  Tracker: TOPMigrationTracker;
  CurrentVer: Integer;
  {$IFNDEF OPF_SKIP_LOGGING}
  I: Integer;
  {$ENDIF}
begin
  CheckRegistries;
  // Before anything else: the migrator uses its own transactions
  ReleaseCallerTransaction(aConnection);
  Result.DiffsFound := 0;
  Result.StatementsExecuted := 0;
  Result.CustomStepsExecuted := 0;
  Result.SQL := nil;
  Result.VersionBefore := 0;
  Result.VersionAfter := 0;
  Result.PlannedSteps := nil;

  Tracker := CreateTracker(aConnection);
  try
    // A dry run changes nothing: a missing tracking table means version 0
    if not (moDryRun in aOptions) then
      Tracker.EnsureMigrationTable;
    CurrentVer := Tracker.GetCurrentVersion;
    Result.VersionBefore := CurrentVer;
    Result.VersionAfter := CurrentVer;

    {$IFNDEF OPF_SKIP_LOGGING}
    if OPLogger.IsKindActive(lkInfo) then
      OPLogger.LogInfo(nil, 'MigrateSchema: current version %d, target version %d',
        [CurrentVer, aTargetVersion]);
    {$ENDIF}

    if CurrentVer >= aTargetVersion then
    begin
      {$IFNDEF OPF_SKIP_LOGGING}
      if OPLogger.IsKindActive(lkInfo) then
        OPLogger.LogInfo(nil, 'MigrateSchema: already at version %d, no migration needed',
          [CurrentVer]);
      {$ENDIF}
      Exit;
    end;

    // Before anything is changed: an inconsistent set of steps raises here
    Result.PlannedSteps := BuildStepChain(CurrentVer, aTargetVersion);
    ComputeSchemaDiffs(aConnection, aOptions, Result);

    {$IFNDEF OPF_SKIP_LOGGING}
    if (moLogSQL in aOptions) or (moDryRun in aOptions) then
      if OPLogger.IsKindActive(lkSQL) then
        for I := 0 to Length(Result.SQL) - 1 do
          OPLogger.Log(nil, lkSQL, Result.SQL[I]);
    {$ENDIF}

    if moDryRun in aOptions then
    begin
      {$IFNDEF OPF_SKIP_LOGGING}
      if OPLogger.IsKindActive(lkInfo) then
        OPLogger.LogInfo(nil, 'MigrateSchema: dry-run complete, %d statements generated, %d custom steps planned',
          [Length(Result.SQL), Length(Result.PlannedSteps)]);
      {$ENDIF}
      Exit;
    end;

    // Without custom steps the version is recorded with the schema changes
    if Length(Result.PlannedSteps) = 0 then
      ExecuteDDLStatements(aConnection, Result, aTargetVersion)
    else
      ExecuteDDLStatements(aConnection, Result);
    ExecuteCustomSteps(aConnection, Result.PlannedSteps, aTargetVersion, Result);
    RecordMigrationVersion(aConnection, aTargetVersion);
    Result.VersionAfter := aTargetVersion;
  finally
    Tracker.Free;
  end;
end;

function TOPSchemaMigrator.GenerateMigrationSQL(aConnection: TSQLConnection;
  aTargetVersion: Integer;
  aOptions: TMigrateOptions;
  aGenerateOptions: TGenerateSQLOptions): TUTF8StringArray;
var
  Tracker: TOPMigrationTracker;
  Res: TOPMigrationResult;
  Len, CurrentVer, RecordVer: Integer;
  Steps: TOPMigrationStepArray;
begin
  CheckRegistries;
  ReleaseCallerTransaction(aConnection);
  Result := nil;
  Tracker := CreateTracker(aConnection);
  try
    // Only generates SQL: the tracking table is created by the script when missing
    if Tracker.HasMigrationTable then
      CurrentVer := Tracker.GetCurrentVersion
    else
    begin
      CurrentVer := 0;
      if gsoIncludeVersionTracking in aGenerateOptions then
      begin
        SetLength(Result, 1);
        Result[0] := Tracker.GetCreateTableSQL;
      end;
    end;
    if CurrentVer >= aTargetVersion then
    begin
      Result := nil;
      Exit;
    end;
    Res := Default(TOPMigrationResult);
    // Raises for an inconsistent set of steps, like MigrateSchema
    Steps := BuildStepChain(CurrentVer, aTargetVersion);
    ComputeSchemaDiffs(aConnection, aOptions, Res);
    Result := Concat(Result, Res.SQL);
    { Custom steps cannot be part of a script: the version recorded is the one
      where the first of them starts. MigrateSchema runs them later. }
    RecordVer := aTargetVersion;
    if Length(Steps) > 0 then
    begin
      RecordVer := Steps[0].FromVersion;
      {$IFNDEF OPF_SKIP_LOGGING}
      OPLogger.LogWarning(Self, 'GenerateMigrationSQL: %d custom steps (from version %d) are not part of the script: '+
        'the script records version %d, MigrateSchema must run the steps', [Length(Steps), RecordVer, RecordVer]);
      {$ENDIF}
    end;
    { Append version tracking INSERT if requested }
    if (gsoIncludeVersionTracking in aGenerateOptions) and (RecordVer > CurrentVer) then
    begin
      Len := Length(Result);
      SetLength(Result, Len + 1);
      Result[Len] := Tracker.GetRecordMigrationSQL(RecordVer,
        Format('Migration to version %d', [RecordVer]));
    end;
  finally
    Tracker.Free;
  end;
end;

end.
