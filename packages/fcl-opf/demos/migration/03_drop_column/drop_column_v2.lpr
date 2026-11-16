{ 03_drop_column — v2: Drop Nickname column from Person table
  Demonstrates: MigrateSchema with moAllowDropColumn to remove a column.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program drop_column_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  { v2 model: Nickname removed }
  TPerson = class(TObject, IOPPersistenceState)
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

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Manager: TOPManager;
  Registry: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
  Res: TOPMigrationResult;
  P: TPerson;
  I: Integer;

begin
  if not FileExists(DB_FILE) then
  begin
    WriteLn('ERROR: Run v1 first to create ', DB_FILE);
    Halt(1);
  end;

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  try
    Conn.DatabaseName := DB_FILE;
    Conn.Transaction := Trans;
    Conn.Open;

    Registry := TOPRegistry.Create(nil);
    RelReg := TOPRelationRegistry.Create(nil);
    RelReg.Registry := Registry;
    M2MReg := TOPManyToManyRegistry.Create(nil);
    M2MReg.Registry := Registry;
    Manager := TOPManager.Create(nil);
    try
      Manager.Registry := Registry;
      Manager.RelationRegistry := RelReg;
      Manager.M2MRegistry := M2MReg;
      with Manager.Connections.Add as TOPConnection do
      begin
        SingleConnection := Conn;
        IsDefault := True;
      end;

      Registry.RegisterPersister(TPerson, TRTTIPersister);

      WriteLn('v2: Migrating schema from version 1 to 2 (drop Nickname)...');
      Res := Manager.MigrateSchema(Conn, 2, [moAllowDropColumn]);

      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Verify existing data survived (Name column intact) }
      P := TPerson.Create;
      try
        P.ID := 1;
        Manager.Load(P);
        WriteLn('  Loaded Person #1: Name="', P.Name, '"');

        P.ID := 2;
        Manager.Load(P);
        WriteLn('  Loaded Person #2: Name="', P.Name, '"');
      finally
        P.Free;
      end;

      WriteLn('  Migration complete. Nickname column dropped; Name data intact.');
    finally
      Manager.Free;
      M2MReg.Free;
      RelReg.Free;
      Registry.Free;
    end;
  finally
    if Trans.Active then Trans.Rollback;
    Conn.Close;
    Trans.Free;
    Conn.Free;
  end;

  DeleteFile(DB_FILE);
end.
