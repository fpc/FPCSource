{ 08_custom_step — v1: Create Person table with FullName field
  Compile: fpc -Fu../../../src v1.pas
  Run:     ./v1                                                }
program custom_step_v1;

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
    FFullName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property FullName: String read FFullName write FFullName;
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

      { Register v1 model: Person with FullName }
      Registry.RegisterPersister(TPerson, TRTTIPersister);

      { Create schema — version 1 }
      Manager.CreateSchema(Conn);
      WriteLn('v1: Schema created at version 1');
      WriteLn('  Table: Person (ID, FullName)');

      { Insert sample data }
      P := TPerson.Create;
      try
        P.ID := 1; P.FullName := 'Alice Smith'; P.FState := opsNew;
        Manager.Save(P);
        P.ID := 2; P.FullName := 'Bob Jones'; P.FState := opsNew;
        Manager.Save(P);
      finally
        P.Free;
      end;

      WriteLn('  Inserted: "Alice Smith", "Bob Jones"');
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
