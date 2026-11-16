{ 06_add_m2m — v1: Create Person and Skill tables (no M2M)
  Compile: fpc -Fu../../../src v1.pas
  Run:     ./v1                                                }
program add_m2m_v1;

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
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TSkill = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FSkillName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property SkillName: String read FSkillName write FSkillName;
  end;

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;
function TSkill.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TSkill.SetPersistenceState(aValue: TObjectPersistenceState);
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
  S: TSkill;

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

      { Register v1 model: Person and Skill, no M2M }
      Registry.RegisterPersister(TPerson, TRTTIPersister);
      Registry.RegisterPersister(TSkill, TRTTIPersister);

      { Create schema — version 1 }
      Manager.CreateSchema(Conn);
      WriteLn('v1: Schema created at version 1');
      WriteLn('  Tables: Person (ID, Name), Skill (ID, SkillName)');

      { Insert sample persons }
      P := TPerson.Create;
      try
        P.ID := 1; P.Name := 'Alice'; P.FState := opsNew;
        Manager.Save(P);
        P.ID := 2; P.Name := 'Bob'; P.FState := opsNew;
        Manager.Save(P);
      finally
        P.Free;
      end;

      { Insert sample skills }
      S := TSkill.Create;
      try
        S.ID := 1; S.SkillName := 'Pascal'; S.FState := opsNew;
        Manager.Save(S);
        S.ID := 2; S.SkillName := 'SQL'; S.FState := opsNew;
        Manager.Save(S);
        S.ID := 3; S.SkillName := 'Python'; S.FState := opsNew;
        Manager.Save(S);
      finally
        S.Free;
      end;

      WriteLn('  Inserted: Alice, Bob');
      WriteLn('  Inserted: Pascal, SQL, Python');
      WriteLn('  No M2M junction table in this version.');
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
