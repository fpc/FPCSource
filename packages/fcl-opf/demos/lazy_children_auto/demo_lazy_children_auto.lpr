{ demo_lazy_children_auto - Lazy loading of 1:N children via automatic setup
  =========================================================================
  This demo shows how TOPLazy<T> fields are automatically wired by the
  Manager on FPC 3.3.1+ (trunk) using extended RTTI. No IOPLazyInit
  interface is needed - the Manager discovers lazy fields and matches
  them against registered relationships.

  Requires: FPC trunk (3.3.1+)

  Compile:  fpc demo_lazy_children_auto.lpr -Fu../../src
  Run:      ./demo_lazy_children_auto
}
program demo_lazy_children_auto;

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
  Opf.Relations,
  Opf.Proxy,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_lazy_children_auto.db';

type
  TTask = class;

  (* Use TOPLazyObjectList from Opf.Proxy - it has {$RTTI EXPLICIT} applied,
    so the Manager can discover its inner fields (FValue, FLoaded, FOnLoad). *)

  (* 
    TProject - parent with a lazy task list.
    No IOPLazyInit needed - the Manager sets up the loader automatically
    by matching the TOPLazy field's FValue against the backing field.
    The {$RTTI EXPLICIT} directive is REQUIRED for auto-discovery of the
    lazy record AND the class fields. 
  *)
  {$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TProject = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FState: TObjectPersistenceState;
    FTaskList: TObjectList;
    FLazyTasks: TOPLazyObjectList;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetTaskList: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    { TaskList is published so the Manager can resolve it via RTTI.
      The getter returns FLazyTasks.Value, triggering lazy load. }
    property TaskList: TObjectList read GetTaskList;
  end;

  { TTask - child entity with FK to TProject }
  TTask = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FProjectID: Integer;
    FDescription: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property ProjectID: Integer read FProjectID write FProjectID;
    property Description: String read FDescription write FDescription;
  end;

{ TProject implementation }

function TProject.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TProject.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TProject.Create;
begin
  inherited Create;
  FTaskList := TObjectList.Create(True);
  { Init the lazy record with the list - the Manager will set the loader }
  FLazyTasks.Init(FTaskList);
end;

destructor TProject.Destroy;
begin
  FTaskList.Free;
  inherited Destroy;
end;

function TProject.GetTaskList: TObjectList;
begin
  Result := FLazyTasks.Value;
end;

{ TTask implementation }

function TTask.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTask.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
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

  ExecSQL('CREATE TABLE Project (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecSQL('CREATE TABLE Task (ID INTEGER PRIMARY KEY, ProjectID INTEGER, Description TEXT)');

  Def := OPRegistry.RegisterPersister(TProject, TRTTIPersister);
  Idx := Def.FieldMap.IndexOfPropertyName('TaskList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);

  OPRegistry.RegisterPersister(TTask, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TProject, TTask, 'ProjectID', False, 'TaskList');

  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
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
  Proj: TProject;
  T: TTask;
  I: Integer;
begin
  SetupDatabase;
  try
    WriteLn('--- INSERT ---');
    Proj := TProject.Create;
    try
      Proj.ID := 1;
      Proj.Title := 'Build ORM';
      Proj.FState := opsNew;
      OPManager.Save(Proj);
      WriteLn('Saved project: ID=', Proj.ID, ' Title="', Proj.Title, '"');
    finally
      Proj.Free;
    end;

    T := TTask.Create;
    try
      T.ID := 10;
      T.ProjectID := 1;
      T.Description := 'Implement lazy loading';
      T.FState := opsNew;
      OPManager.Save(T);
    finally
      T.Free;
    end;

    T := TTask.Create;
    try
      T.ID := 11;
      T.ProjectID := 1;
      T.Description := 'Write tests';
      T.FState := opsNew;
      OPManager.Save(T);
    finally
      T.Free;
    end;

    T := TTask.Create;
    try
      T.ID := 12;
      T.ProjectID := 1;
      T.Description := 'Write demos';
      T.FState := opsNew;
      OPManager.Save(T);
    finally
      T.Free;
    end;
    WriteLn('Saved 3 tasks for project 1');
    WriteLn;

    { Load project:
      tasks are not yet loaded.
      Note: TProject does not implement IOPLazyInit.
      The Manager discovers the TOPLazy field via extended RTTI and
      matches it to the registered relationship automatically. }
    WriteLn('--- AUTOMATIC LAZY LOAD (no IOPLazyInit) ---');
    Proj := TProject.Create;
    try
      Proj.ID := 1;
      Proj.FState := opsKey;
      OPManager.Load(Proj);
      WriteLn('Loaded project: ID=', Proj.ID, ' Title="', Proj.Title, '"');
      WriteLn('Tasks loaded yet? ', Proj.FLazyTasks.IsLoaded);

      WriteLn('Accessing TaskList...');
      WriteLn('Task count: ', Proj.TaskList.Count);
      WriteLn('Tasks loaded now? ', Proj.FLazyTasks.IsLoaded);
      for I := 0 to Proj.TaskList.Count - 1 do
      begin
        T := TTask(Proj.TaskList[I]);
        WriteLn('  [', I, '] ID=', T.ID, ' "', T.Description, '"');
      end;
    finally
      Proj.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
