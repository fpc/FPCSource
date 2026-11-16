{ demo_multidb - Switching database backends
  =============================================
  This demo shows how to register multiple named database connections
  and switch between them at runtime. Each connection is isolated --
  data saved on one backend is not visible on the other.

  In production, these connections would point to different database
  engines (PostgreSQL, MySQL, SQLite, etc.). Here we use two separate
  SQLite databases to demonstrate the concept portably.

  Key concepts:
  - Named connections via OPManager.Connections.Add
  - IsDefault designates which connection is currently active
  - Switching IsDefault changes which DB receives operations
  - Data is fully isolated between backends

  Compile:  fpc demo_multidb.lpr -Fu../../src
  Run:      ./demo_multidb
}
program demo_multidb;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  { ORM units }
  Opf.Manager,         { TOPManager, TOPConnection -- connection management }
  Opf.Registry,        { TOPRegistry -- class registration }
  Opf.Fields,          { TOPFieldMap -- field mappings }
  Opf.Sql,             { TOPSQLBuilder -- SQL generation }
  Opf.Driver,
  Opf.Persister.Rtti;  { TRTTIPersister -- automatic RTTI-based mapping }

const
  DB_FILE_A = 'demo_multidb_a.db';
  DB_FILE_B = 'demo_multidb_b.db';

type
  { TUser -- simple model class for demonstrating multi-DB operations }

  TUser = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

function TUser.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TUser.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  ConnA, ConnB: TSQLite3Connection;
  TransA, TransB: TSQLTransaction;

{ Helper: execute DDL on a specific connection }
procedure ExecSQLOn(aConn: TSQLite3Connection; aTrans: TSQLTransaction;
  const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := aConn;
    Q.Transaction := aTrans;
    Q.SQL.Text := aSQL;
    if not aTrans.Active then
      aTrans.StartTransaction;
    Q.ExecSQL;
    aTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
begin
  { Clean up old DB files }
  if FileExists(DB_FILE_A) then DeleteFile(DB_FILE_A);
  if FileExists(DB_FILE_B) then DeleteFile(DB_FILE_B);

  { Create two SQLite connections -- simulating two different backends.
    In production, ConnA might be PostgreSQL and ConnB might be MySQL. }
  ConnA := TSQLite3Connection.Create(nil);
  ConnA.DatabaseName := DB_FILE_A;
  TransA := TSQLTransaction.Create(ConnA);
  ConnA.Transaction := TransA;
  ConnA.Open;

  ConnB := TSQLite3Connection.Create(nil);
  ConnB.DatabaseName := DB_FILE_B;
  TransB := TSQLTransaction.Create(ConnB);
  ConnB.Transaction := TransB;
  ConnB.Open;

  { Register the model class (only once -- registration is global) }
  OPRegistry.RegisterPersister(TUser, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;

  { Create the same table on both backends }
  ExecSQLOn(ConnA, TransA, 'CREATE TABLE UserAccount (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQLOn(ConnB, TransB, 'CREATE TABLE UserAccount (ID INTEGER PRIMARY KEY, Name TEXT)');

  { Override the default table name (TUser -> 'User' would conflict with
    SQL reserved word in some dialects) }
  OPRegistry.FindPersister(TUser).TableName := 'UserAccount';

  { Register both connections with the ORM manager.
    Each connection gets a Name for identification.
    IsDefault = True means this connection is used for all operations. }
  TOPManager.ClearInstance;

  { Backend A -- the default connection }
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.Name := 'BackendA';
  OpConn.SingleConnection := ConnA;
  OpConn.IsDefault := True;   { <-- This is the active backend }

  { Backend B -- registered but not active yet }
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.Name := 'BackendB';
  OpConn.SingleConnection := ConnB;
  { IsDefault not set -- defaults to False }

  WriteLn('Database setup complete.');
  WriteLn('  Backend A: ', DB_FILE_A, ' (default)');
  WriteLn('  Backend B: ', DB_FILE_B);
  WriteLn;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  if Assigned(TransA) then begin TransA.Free; TransA := nil; end;
  if Assigned(ConnA) then
  begin
    if ConnA.Connected then ConnA.Close;
    ConnA.Free;
  end;
  if Assigned(TransB) then begin TransB.Free; TransB := nil; end;
  if Assigned(ConnB) then
  begin
    if ConnB.Connected then ConnB.Close;
    ConnB.Free;
  end;
  if FileExists(DB_FILE_A) then DeleteFile(DB_FILE_A);
  if FileExists(DB_FILE_B) then DeleteFile(DB_FILE_B);
end;

var
  User: TUser;
  Loaded: TUser;
  AllUsers: TObjectList;
  I: Integer;
begin
  SetupDatabase;
  try
    { Step 1: Save Alice on Backend A (the current default) }
    WriteLn('--- Step 1: Save on Backend A (default) ---');
    User := TUser.Create;
    try
      User.ID := 1;
      User.Name := 'Alice';
      User.FState := opsNew;
      OPManager.Save(User);
      WriteLn('Saved Alice (ID=', User.ID, ') on Backend A');
    finally
      User.Free;
    end;
    WriteLn;

    { Step 2: Switch default to Backend B
      Setting IsDefault on another connection makes it the active one.
      All subsequent ORM operations will use Backend B. }
    WriteLn('--- Step 2: Switch to Backend B ---');
    (OPManager.Connections[1] as TOPConnection).IsDefault := True;
    WriteLn('Default connection switched to Backend B');
    WriteLn;

    { === Step 3: Save Bob on Backend B === }
    WriteLn('--- Step 3: Save on Backend B ---');
    User := TUser.Create;
    try
      User.ID := 2;
      User.Name := 'Bob';
      User.FState := opsNew;
      OPManager.Save(User);
      WriteLn('Saved Bob (ID=', User.ID, ') on Backend B');
    finally
      User.Free;
    end;
    WriteLn;

    { Step 4: Verify data isolation
      Bob should be on Backend B, Alice should NOT be on Backend B. }
    WriteLn('--- Step 4: Verify isolation on Backend B ---');
    AllUsers := TObjectList.Create(True);
    try
      OPManager.Load(AllUsers, TUser);
      WriteLn('Users on Backend B: ', AllUsers.Count);
      for I := 0 to AllUsers.Count - 1 do
      begin
        User := TUser(AllUsers[I]);
        WriteLn('  ID=', User.ID, ' Name="', User.Name, '"');
      end;
      WriteLn('  (Alice is NOT here -- she is on Backend A)');
    finally
      AllUsers.Free;
    end;
    WriteLn;

    { Step 5: Switch back to Backend A
      Verify Alice is still on Backend A. }
    WriteLn('--- Step 5: Switch back to Backend A ---');
    (OPManager.Connections[0] as TOPConnection).IsDefault := True;
    WriteLn('Default connection switched back to Backend A');

    Loaded := TUser.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Loaded from Backend A: ID=', Loaded.ID, ' Name="', Loaded.Name, '"');
      WriteLn('  Alice is still here -- data persists independently per backend');
    finally
      Loaded.Free;
    end;

    AllUsers := TObjectList.Create(True);
    try
      OPManager.Load(AllUsers, TUser);
      WriteLn('Users on Backend A: ', AllUsers.Count);
      for I := 0 to AllUsers.Count - 1 do
      begin
        User := TUser(AllUsers[I]);
        WriteLn('  ID=', User.ID, ' Name="', User.Name, '"');
      end;
    finally
      AllUsers.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
