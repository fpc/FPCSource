{ 05_drop_relation — v2: Remove DepartmentID column from Person
  Demonstrates: MigrateSchema with moAllowDropColumn to drop the FK column,
  effectively severing the relationship between Person and Department.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program drop_relation_v2;

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

  { v2 model: Person no longer has DepartmentID — relation removed }
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
  D: TDepartment;
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

      { Register v2 model: no DepartmentID column }
      Registry.RegisterPersister(TDepartment, TRTTIPersister);
      Registry.RegisterPersister(TPerson, TRTTIPersister);

      WriteLn('v2: Migrating schema from version 1 to 2 (drop DepartmentID column)...');
      Res := Manager.MigrateSchema(Conn, 2, [moAllowDropColumn]);

      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Verify existing data is intact }
      P := TPerson.Create;
      try
        P.ID := 1;
        Manager.Load(P);
        WriteLn('  Loaded Person #1: Name="', P.Name, '"');
      finally
        P.Free;
      end;

      D := TDepartment.Create;
      try
        D.ID := 1;
        Manager.Load(D);
        WriteLn('  Loaded Department #1: DeptName="', D.DeptName, '"');
      finally
        D.Free;
      end;

      WriteLn('  Migration complete. DepartmentID column dropped; both tables intact.');
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
