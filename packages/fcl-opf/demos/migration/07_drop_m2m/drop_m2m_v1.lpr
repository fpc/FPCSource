{ 07_drop_m2m — v1: Create Person, Skill, and PersonSkill junction table
  Compile: fpc -Fu../../../src v1.pas
  Run:     ./v1                                                }
program drop_m2m_v1;

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

procedure ExecSQL(aConn: TSQLite3Connection; const aSQL: String);
var
  Q: TSQLQuery;
  T: TSQLTransaction;
begin
  T := TSQLTransaction.Create(nil);
  try
    T.Database := aConn;
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := aConn;
      Q.Transaction := T;
      T.StartTransaction;
      Q.SQL.Text := aSQL;
      Q.ExecSQL;
      T.Commit;
    finally
      Q.Free;
    end;
  finally
    if T.Active then T.Rollback;
    T.Free;
  end;
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

      { Register v1 model: Person + Skill + M2M junction }
      Registry.RegisterPersister(TPerson, TRTTIPersister);
      Registry.RegisterPersister(TSkill, TRTTIPersister);
      M2MReg.RegisterManyToMany(TPerson, TSkill, 'PersonSkill', 'PersonID', 'SkillID');

      { Create schema — version 1 }
      Manager.CreateSchema(Conn);
      WriteLn('v1: Schema created at version 1');
      WriteLn('  Tables: Person, Skill, PersonSkill (junction)');

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
      finally
        S.Free;
      end;
    finally
      Manager.Free;
      M2MReg.Free;
      RelReg.Free;
      Registry.Free;
    end;

    { Insert junction rows via raw SQL }
    ExecSQL(Conn, 'INSERT INTO PersonSkill (PersonID, SkillID) VALUES (1, 1)');
    ExecSQL(Conn, 'INSERT INTO PersonSkill (PersonID, SkillID) VALUES (1, 2)');
    ExecSQL(Conn, 'INSERT INTO PersonSkill (PersonID, SkillID) VALUES (2, 1)');

    WriteLn('  Inserted: Alice, Bob');
    WriteLn('  Inserted: Pascal, SQL');
    WriteLn('  Junction rows: Alice->Pascal, Alice->SQL, Bob->Pascal');
    WriteLn('  Database saved to: ', DB_FILE);
  finally
    if Trans.Active then Trans.Rollback;
    Conn.Close;
    Trans.Free;
    Conn.Free;
  end;
end.
