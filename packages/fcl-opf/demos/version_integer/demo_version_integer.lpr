{ demo_version_integer - Optimistic concurrency with integer version column
  ============================================================================
  Demonstrates optimistic concurrency control using an auto-incrementing
  integer version column. When two users load the same record and both try
  to save, the second save detects the version mismatch and raises
  EOPFConcurrency.

  Compile:  lazbuild demo_version_integer.lpi
  Run:      ./demo_version_integer
}
program demo_version_integer;

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
  DB_FILE = 'demo_version_integer.db';

type
  { TDocument:
    has an integer Version column with foVersion.
    The ORM auto-sets Version=1 on INSERT and increments on each UPDATE.
    UPDATE includes WHERE Version=:OldVersion for conflict detection. }

  TDocument = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FContent: String;
    FVersion: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property Content: String read FContent write FContent;
    property Version: Integer read FVersion write FVersion;
  end;

function TDocument.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TDocument.SetPersistenceState(aValue: TObjectPersistenceState);
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

  Def := OPRegistry.RegisterPersister(TDocument, TRTTIPersister);

  { Mark the Version field with foVersion.
    This tells the ORM to auto-manage it for optimistic concurrency. }
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

  WriteLn('Database setup complete. Version field: Integer with foVersion.');
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
  Doc: TDocument;
  UserA, UserB: TDocument;
begin
  SetupDatabase;
  try
    { Insert: Version auto-set to 1 }
    WriteLn('--- INSERT (Version auto-set to 1) ---');
    Doc := TDocument.Create;
    try
      Doc.ID := 1;
      Doc.Title := 'Design Doc';
      Doc.Content := 'Initial draft.';
      Doc.FState := opsNew;
      OPManager.Save(Doc);
      WriteLn('Saved: ID=', Doc.ID, ' Title="', Doc.Title,
        '" Version=', Doc.Version);
    finally
      Doc.Free;
    end;
    WriteLn;

    { Normal update: Version increments to 2 }
    WriteLn('--- UPDATE (Version increments 1 -> 2) ---');
    Doc := TDocument.Create;
    try
      Doc.ID := 1;
      OPManager.Load(Doc);
      WriteLn('Loaded: Version=', Doc.Version);
      Doc.Content := 'Revised draft.';
      Doc.FState := opsUpdate;
      OPManager.Save(Doc);
      WriteLn('Saved:  Version=', Doc.Version, ' (auto-incremented)');
    finally
      Doc.Free;
    end;
    WriteLn;

    { Conflict situation:
      Two "users" load the same record (both see Version=2).
      User A saves first (succeeds, Version becomes 3).
      User B tries to save (fails - Version in DB is now 3, not 2). }
    WriteLn('--- CONFLICT SIMULATION ---');
    UserA := TDocument.Create;
    UserB := TDocument.Create;
    try
      { Both load the same record }
      UserA.ID := 1;
      OPManager.Load(UserA);
      UserB.ID := 1;
      OPManager.Load(UserB);
      WriteLn('User A loaded: Version=', UserA.Version);
      WriteLn('User B loaded: Version=', UserB.Version);
      WriteLn;

      { User A saves first }
      UserA.Title := 'Design Doc (A edit)';
      UserA.FState := opsUpdate;
      OPManager.Save(UserA);
      WriteLn('User A saved:  Version=', UserA.Version, ' (success)');
      WriteLn;

      { User B tries to save - this should fail }
      UserB.Title := 'Design Doc (B edit)';
      UserB.FState := opsUpdate;
      WriteLn('User B saving with stale Version=', UserB.Version, '...');
      try
        OPManager.Save(UserB);
        WriteLn('ERROR: Should have raised EOPFConcurrency!');
      except
        on E: EOPF do
          WriteLn('CAUGHT: ', E.Message);
      end;
    finally
      UserB.Free;
      UserA.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
