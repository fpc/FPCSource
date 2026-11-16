{ 04_add_relation — v2: Add DepartmentID column to Person
  Demonstrates: MigrateSchema adding a foreign key column to link
  Person to Department. On SQLite, the FK column is added without a
  DB-level constraint (SQLite limitation), but the ORM tracks the
  relationship at the application level.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program add_relation_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  TDepartment = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FDeptName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property DeptName: String read FDeptName write FDeptName;
  end;

  { v2 model: Person now has DepartmentID to reference Department }
  TPerson = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FDepartmentID: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property DepartmentID: Integer read FDepartmentID write FDepartmentID;
  end;

function TDepartment.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TDepartment.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
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

      { Register v2 model: add DepartmentID column (no DB-level FK on SQLite) }
      Registry.RegisterPersister(TDepartment, TRTTIPersister);
      Registry.RegisterPersister(TPerson, TRTTIPersister);

      WriteLn('v2: Migrating schema from version 1 to 2 (add DepartmentID column)...');
      Res := Manager.MigrateSchema(Conn, 2);

      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Update Alice to belong to Engineering department }
      P := TPerson.Create;
      try
        P.ID := 1;
        Manager.Load(P);
        WriteLn('  Loaded Person #1: Name="', P.Name, '" DepartmentID=', P.DepartmentID);

        P.DepartmentID := 1;
        P.FState := opsUpdate;
        Manager.Save(P);
        WriteLn('  Updated Person #1: DepartmentID set to 1 (Engineering)');

        { Verify the update }
        P.Name := '';
        P.DepartmentID := 0;
        Manager.Load(P);
        WriteLn('  Verified Person #1: Name="', P.Name, '" DepartmentID=', P.DepartmentID);
      finally
        P.Free;
      end;

      WriteLn('  Migration complete. DepartmentID column added to Person.');
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
