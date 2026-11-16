(*
  demo_lazy_m2m_auto - Lazy loading of M:N associations via automatic setup
  =======================================================================
  This demo shows how TOPLazy fields for M:N associations are automatically
  wired by the Manager on FPC 3.3.1+ using extended RTTI. No IOPLazyInit
  interface is needed.

  Requirements:
  - FPC trunk (3.3.1+)
  - {$RTTI EXPLICIT FIELDS} on the class containing the lazy field
  - Use TOPLazyObjectList from Opf.Proxy (has RTTI for inner fields)

  Compile:  fpc demo_lazy_m2m_auto.lpr -Fu../../src
  Run:      ./demo_lazy_m2m_auto
*)

program demo_lazy_m2m_auto;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.M2M,
  Opf.Relations,
  Opf.Proxy,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_lazy_m2m_auto.db';

type
  { TSkill - the associated entity }
  TSkill = class(TObject, IOPPersistenceState)
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

  { TDeveloper - has lazy M:N skills, auto-setup by Manager }
  {$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TDeveloper = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FHandle: String;
    FState: TObjectPersistenceState;
    FSkillList: TObjectList;
    FLazySkills: TOPLazyObjectList;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetSkillList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Handle: String read FHandle write FHandle;
    property SkillList: TObjectList read GetSkillList;
  end;

{ Implementations }

function TSkill.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TSkill.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TDeveloper.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TDeveloper.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TDeveloper.Create;
begin
  inherited Create;
  FSkillList := TObjectList.Create(True);
  FLazySkills.Init(FSkillList);
end;

destructor TDeveloper.Destroy;
begin
  FSkillList.Free;
  inherited Destroy;
end;

function TDeveloper.GetSkillList: TObjectList;
begin
  Result := FLazySkills.Value;
end;

{ Database helpers }

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := Trans;
    Q.SQL.Text := aSQL;
    if not Trans.Active then
      Trans.StartTransaction;
    Q.ExecSQL;
    Trans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
  Idx: Integer;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  Trans := TSQLTransaction.Create(Conn);
  Conn.Transaction := Trans;
  Conn.Open;

  ExecSQL('CREATE TABLE Developer (ID INTEGER PRIMARY KEY, Handle TEXT)');
  ExecSQL('CREATE TABLE Skill (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQL('CREATE TABLE dev_skill (dev_id INTEGER, skill_id INTEGER)');

  Def := OPRegistry.RegisterPersister(TDeveloper, TRTTIPersister);
  Idx := Def.FieldMap.IndexOfPropertyName('SkillList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TSkill, TRTTIPersister);

  OPM2MRegistry.RegisterManyToMany(
    TDeveloper, TSkill, 'dev_skill', 'dev_id', 'skill_id',
    False, 'SkillList', '');

  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPManyToManyRegistry.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  Trans.Free;
  Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ Main }

var
  Dev: TDeveloper;
  Sk: TSkill;
  I: Integer;
begin
  SetupDatabase;
  try
    WriteLn('--- INSERT ---');
    Dev := TDeveloper.Create;
    try
      Dev.ID := 1;
      Dev.Handle := 'lazarus_dev';
      Dev.FState := opsNew;
      OPManager.Save(Dev);
      WriteLn('Saved developer: "', Dev.Handle, '"');
    finally
      Dev.Free;
    end;

    Sk := TSkill.Create;
    try
      Sk.ID := 10;
      Sk.Name := 'Pascal';
      Sk.FState := opsNew;
      OPManager.Save(Sk);
    finally
      Sk.Free;
    end;

    Sk := TSkill.Create;
    try
      Sk.ID := 11;
      Sk.Name := 'SQL';
      Sk.FState := opsNew;
      OPManager.Save(Sk);
    finally
      Sk.Free;
    end;

    ExecSQL('INSERT INTO dev_skill (dev_id, skill_id) VALUES (1, 10)');
    ExecSQL('INSERT INTO dev_skill (dev_id, skill_id) VALUES (1, 11)');
    WriteLn('Created 2 skill associations');
    WriteLn;

    WriteLn('--- AUTOMATIC LAZY LOAD M:N (no IOPLazyInit) ---');
    Dev := TDeveloper.Create;
    try
      Dev.ID := 1;
      Dev.FState := opsKey;
      OPManager.Load(Dev);
      WriteLn('Loaded developer: "', Dev.Handle, '"');
      WriteLn('Skills loaded yet? ', Dev.FLazySkills.IsLoaded);

      WriteLn('Accessing SkillList...');
      WriteLn('Skill count: ', Dev.SkillList.Count);
      WriteLn('Skills loaded now? ', Dev.FLazySkills.IsLoaded);
      for I := 0 to Dev.SkillList.Count - 1 do
      begin
        Sk := TSkill(Dev.SkillList[I]);
        WriteLn('  [', I, '] "', Sk.Name, '"');
      end;
    finally
      Dev.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
