{ 05_drop_relation — v1: Create Person with DepartmentID linking to Department
  The DepartmentID column acts as a logical foreign key (application-level).
  Compile: fpc -Fu../../../src v1.pas
  Run:     ./v1                                                }
program drop_relation_v1;

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
  D: TDepartment;
  P: TPerson;

begin
  if FileExists(DB_FILE) then DeleteFile(DB_FILE);

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

      { Register v1 model: Person has DepartmentID referencing Department }
      Registry.RegisterPersister(TDepartment, TRTTIPersister);
      Registry.RegisterPersister(TPerson, TRTTIPersister);

      Manager.CreateSchema(Conn);
      WriteLn('v1: Schema created at version 1');
      WriteLn('  Table: Department (ID, DeptName)');
      WriteLn('  Table: Person (ID, Name, DepartmentID)');

      D := TDepartment.Create;
      try
        D.ID := 1; D.DeptName := 'Engineering'; D.FState := opsNew;
        Manager.Save(D);
      finally
        D.Free;
      end;

      P := TPerson.Create;
      try
        P.ID := 1; P.Name := 'Alice'; P.DepartmentID := 1; P.FState := opsNew;
        Manager.Save(P);
      finally
        P.Free;
      end;

      WriteLn('  Inserted: Department "Engineering", Person "Alice" (DepartmentID=1)');
      WriteLn('  Database saved to: ', DB_FILE);
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
end.
