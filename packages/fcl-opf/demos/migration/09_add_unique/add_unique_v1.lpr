{ 09_add_unique — v1: Create schema with Person(ID, Name)
  Demonstrates: Initial schema without unique constraint.
  Compile: fpc -Fu../../../src v1.pas
  Run:     ./v1                                                }
program add_unique_v1;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  TPerson = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FEmail: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Email: String read FEmail write FEmail;
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

      Registry.RegisterPersister(TPerson, TRTTIPersister);

      Manager.CreateSchema(Conn);
      WriteLn('v1: Schema created at version 1');
      WriteLn('  Table: Person (ID, Name, Email) — no unique constraint');

      { Insert persons with DUPLICATE emails — this is allowed in v1 }
      P := TPerson.Create;
      try
        P.ID := 1; P.Name := 'Alice'; P.Email := 'alice@test.com';
        P.FState := opsNew;
        Manager.Save(P);

        P.ID := 2; P.Name := 'Bob'; P.Email := 'alice@test.com';
        P.FState := opsNew;
        Manager.Save(P);

        P.ID := 3; P.Name := 'Charlie'; P.Email := 'charlie@test.com';
        P.FState := opsNew;
        Manager.Save(P);
      finally
        P.Free;
      end;

      WriteLn('  Inserted: Alice (alice@test.com)');
      WriteLn('  Inserted: Bob   (alice@test.com) — duplicate email, allowed!');
      WriteLn('  Inserted: Charlie (charlie@test.com)');
      WriteLn('  Database saved to: ', DB_FILE);
    finally
      Manager.Free; M2MReg.Free; RelReg.Free; Registry.Free;
    end;
  finally
    if Trans.Active then Trans.Rollback;
    Conn.Close; Trans.Free; Conn.Free;
  end;
end.
