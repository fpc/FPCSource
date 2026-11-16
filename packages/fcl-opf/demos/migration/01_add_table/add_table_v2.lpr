{ 01_add_table — v2: Add Address table to existing schema
  Demonstrates: MigrateSchema detecting a new table and creating it.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program add_table_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  { Same as v1 }
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

  { NEW in v2 }
  TAddress = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FStreet: String;
    FCity: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Street: String read FStreet write FStreet;
    property City: String read FCity write FCity;
  end;

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;
function TAddress.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TAddress.SetPersistenceState(aValue: TObjectPersistenceState);
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
  A: TAddress;
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

      { Register v2 model: Person + Address }
      Registry.RegisterPersister(TPerson, TRTTIPersister);
      Registry.RegisterPersister(TAddress, TRTTIPersister);

      { Migrate to version 2 }
      WriteLn('v2: Migrating schema from version 1 to 2...');
      Res := Manager.MigrateSchema(Conn, 2);

      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Verify: insert into new Address table }
      A := TAddress.Create;
      try
        A.ID := 1; A.Street := '123 Main St'; A.City := 'Springfield';
        A.FState := opsNew;
        Manager.Save(A);
        WriteLn('  Inserted Address: Street="', A.Street, '" City="', A.City, '"');
      finally
        A.Free;
      end;

      WriteLn('  Migration complete. Address table created alongside existing Person data.');
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
