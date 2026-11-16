{ demo_version_uuid - Optimistic concurrency with UUID version column
  ======================================================================
  Demonstrates optimistic concurrency control using a UUID string version
  column. Each Save generates a new UUID. The WHERE clause checks the old
  UUID to detect concurrent modifications.

  UUID versions are useful when version numbers might be meaningful in a
  different context (e.g., API versioning) or when you want non-sequential,
  opaque version tokens.

  Compile:  lazbuild demo_version_uuid.lpi
  Run:      ./demo_version_uuid
}
program demo_version_uuid;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.DDL,
  Opf.Persister.Rtti,
  Opf.Types,
  Opf.Logging;

const
  DB_FILE = 'demo_version_uuid.db';

type
  { TSetting:
    has a string Version column with foVersion.
    The ORM auto-generates a UUID on INSERT and regenerates on each UPDATE.
    UPDATE includes WHERE Version=:OldUUID for conflict detection. }

  TSetting = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FKey: String;
    FValue: String;
    FVersion: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Key: String read FKey write FKey;
    property Value: String read FValue write FValue;
    property Version: String read FVersion write FVersion;
  end;

function TSetting.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TSetting.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.ExecSQL;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
  VersionField: TOPFieldMap;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  Def := OPRegistry.RegisterPersister(TSetting, TRTTIPersister);

  { Mark the Version field with foVersion.
    Since it's a string type, the ORM uses UUID generation instead of incrementing. }
  VersionField := Def.FieldMap.FindByPropertyName('Version');
  VersionField.Options := VersionField.Options + [foVersion];

  OPSQLBuilder.SQLDialect := dSQLite;
  ExecSQL(OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray));

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  OPManager.Logger := TOPLogger.Instance;
  TOPLogger(TOPLogger.Instance).LogKinds := [lkSQL];
  TOPLogger(TOPLogger.Instance).LogToConsole := True;

  WriteLn('Database setup complete. Version field: String (UUID) with foVersion.');
  WriteLn;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  SetupTrans.Free;
  if Conn.Connected then
    Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

var
  Setting: TSetting;
  OldVersion: String;
begin
  SetupDatabase;
  try
    { Insert: Version auto-set to a new UUID }
    WriteLn('--- INSERT (Version auto-set to UUID) ---');
    Setting := TSetting.Create;
    try
      Setting.ID := 1;
      Setting.Key := 'app.theme';
      Setting.Value := 'dark';
      Setting.FState := opsNew;
      OPManager.Save(Setting);
      WriteLn('Saved: Key="', Setting.Key, '" Value="', Setting.Value, '"');
      WriteLn('  Version=', Setting.Version);
    finally
      Setting.Free;
    end;
    WriteLn;

    { Normal update: Version becomes a new UUID }
    WriteLn('--- UPDATE (Version regenerated) ---');
    Setting := TSetting.Create;
    try
      Setting.ID := 1;
      OPManager.Load(Setting);
      OldVersion := Setting.Version;
      WriteLn('Loaded:  Version=', OldVersion);
      Setting.Value := 'light';
      Setting.FState := opsUpdate;
      OPManager.Save(Setting);
      WriteLn('Saved:   Version=', Setting.Version);
      WriteLn('  (old and new UUIDs are different: ', OldVersion <> Setting.Version, ')');
    finally
      Setting.Free;
    end;
    WriteLn;

    { Conflict situation: }
    WriteLn('--- CONFLICT SIMULATION ---');
    Setting := TSetting.Create;
    try
      { Load as "User A" }
      Setting.ID := 1;
      OPManager.Load(Setting);
      OldVersion := Setting.Version;
      WriteLn('User A loaded: Version=', OldVersion);

      { Simulate another user modifying the record directly }
      WriteLn('Simulating User B saving via direct SQL...');
      ExecSQL('UPDATE Setting SET Value=''contrast'', Version=''user-b-version'' WHERE ID=1');

      { User A tries to save - should fail because Version in DB changed }
      Setting.Value := 'solarized';
      Setting.FState := opsUpdate;
      WriteLn('User A saving with stale Version...');
      try
        OPManager.Save(Setting);
        WriteLn('ERROR: Should have raised EOPFConcurrency!');
      except
        on E: EOPF do
          WriteLn('CAUGHT: ', E.Message);
      end;
    finally
      Setting.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
