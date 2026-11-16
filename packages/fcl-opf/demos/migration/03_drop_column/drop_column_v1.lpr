{ 03_drop_column — v1: Create schema with Person(ID, Name, Nickname)
  Compile: fpc -Fu../../../src v1.pas
  Run:     ./v1                                                }
program drop_column_v1;

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
    FNickname: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Nickname: String read FNickname write FNickname;
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
      WriteLn('  Table: Person (ID, Name, Nickname)');

      P := TPerson.Create;
      try
        P.ID := 1; P.Name := 'Alice'; P.Nickname := 'Ali'; P.FState := opsNew;
        Manager.Save(P);
        P.ID := 2; P.Name := 'Bob'; P.Nickname := 'Bobby'; P.FState := opsNew;
        Manager.Save(P);
      finally
        P.Free;
      end;
      WriteLn('  Inserted: Alice (Ali), Bob (Bobby)');
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
