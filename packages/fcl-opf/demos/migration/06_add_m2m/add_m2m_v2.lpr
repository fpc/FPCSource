{ 06_add_m2m — v2: Add M2M junction table PersonSkill
  Demonstrates: MigrateSchema creating a junction table for a new M2M relationship.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program add_m2m_v2;

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

procedure QueryAndShow(aConn: TSQLite3Connection; const aSQL, aLabel: String);
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
      Q.Open;
      WriteLn('  ', aLabel, ':');
      while not Q.EOF do
      begin
        WriteLn('    ', Q.Fields[0].AsString, ' | ', Q.Fields[1].AsString);
        Q.Next;
      end;
      Q.Close;
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
  Res: TOPMigrationResult;
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

      { Register v2 model: Person + Skill + M2M junction }
      Registry.RegisterPersister(TPerson, TRTTIPersister);
      Registry.RegisterPersister(TSkill, TRTTIPersister);
      M2MReg.RegisterManyToMany(TPerson, TSkill, 'PersonSkill', 'PersonID', 'SkillID');

      { Migrate to version 2 }
      WriteLn('v2: Migrating schema from version 1 to 2...');
      Res := Manager.MigrateSchema(Conn, 2);

      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  Version: ', Res.VersionBefore, ' -> ', Res.VersionAfter);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Verify existing data is intact }
      QueryAndShow(Conn, 'SELECT ID, Name FROM Person ORDER BY ID', 'Person data');
      QueryAndShow(Conn, 'SELECT ID, SkillName FROM Skill ORDER BY ID', 'Skill data');

      WriteLn('  Migration complete. PersonSkill junction table created.');
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
