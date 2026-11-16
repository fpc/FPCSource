{ demo_soft_delete - Soft delete with automatic load filtering
  =============================================================
  This demo shows how foSoftDelete marks records as deleted without
  physically removing them. The ORM automatically:

  - Replaces DELETE with UPDATE (setting the soft-delete column)
  - Filters soft-deleted records out of Load, LoadChildren, LoadAssociations
  - Propagates soft delete to cascade-deleted children

  Part 1: Boolean soft delete - a simple TRUE/FALSE column
  Part 2: DateTime soft delete - stores the deletion timestamp
  Part 3: Criteria bypass - Load(list) with criteria sees all records
  Part 4: Cascade soft delete - parent + child both soft-deleted

  Compile:  lazbuild demo_soft_delete.lpi
  Run:      ./demo_soft_delete
}
program demo_soft_delete;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Types,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Relations,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_soft_delete.db';

type

  { Part 1: Boolean soft delete ===
    Deleted is a Boolean column. FALSE/NULL = active, TRUE = deleted. }

  TArticle = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FDeleted: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  { Part 2: DateTime soft delete ===
    DeletedAt is a TDateTime column. NULL = active, timestamp = deleted. }

  TComment = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FText: String;
    FDeletedAt: TDateTime;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Text: String read FText write FText;
    property DeletedAt: TDateTime read FDeletedAt write FDeletedAt;
  end;

  { Part 4: Cascade - Parent with children ===
    Both parent and child have foSoftDelete. }

  TProject = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FDeleted: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TTask = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FProjectID: Integer;
    FDescription: String;
    FDeleted: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property ProjectID: Integer read FProjectID write FProjectID;
    property Description: String read FDescription write FDescription;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

{ --- IOPPersistenceState implementations --- }

function TArticle.GetPersistenceState: TObjectPersistenceState;
begin Result := FState; end;
procedure TArticle.SetPersistenceState(aValue: TObjectPersistenceState);
begin FState := aValue; end;

function TComment.GetPersistenceState: TObjectPersistenceState;
begin Result := FState; end;
procedure TComment.SetPersistenceState(aValue: TObjectPersistenceState);
begin FState := aValue; end;

function TProject.GetPersistenceState: TObjectPersistenceState;
begin Result := FState; end;
procedure TProject.SetPersistenceState(aValue: TObjectPersistenceState);
begin FState := aValue; end;

function TTask.GetPersistenceState: TObjectPersistenceState;
begin Result := FState; end;
procedure TTask.SetPersistenceState(aValue: TObjectPersistenceState);
begin FState := aValue; end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.ExecSQL;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

function QueryInt(const aSQL: String): Integer;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := aSQL;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
  Field: TOPFieldMap;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  OPSQLBuilder.SQLDialect := dSQLite;

  { --- Register TArticle with Boolean soft delete --- }
  Def := OPRegistry.RegisterPersister(TArticle, TRTTIPersister);
  Def.TableName := 'Articles';
  Field := Def.FieldMap.FindByPropertyName('Deleted');
  Field.Options := Field.Options + [foSoftDelete];
  { Note: no foRequired, so NULL also means "not deleted" }

  { --- Register TComment with DateTime soft delete --- }
  Def := OPRegistry.RegisterPersister(TComment, TRTTIPersister);
  Def.TableName := 'Comments';
  Field := Def.FieldMap.FindByPropertyName('DeletedAt');
  Field.Options := Field.Options + [foSoftDelete] - [foInInsert, foInUpdate];
  { Remove foInInsert/foInUpdate so the column is not included in regular
    INSERT/UPDATE SQL - it stays NULL in the DB until a soft delete sets it.
    The soft-delete UPDATE uses its own SQL, not the normal Update path. }

  { --- Register TProject + TTask with cascade soft delete --- }
  Def := OPRegistry.RegisterPersister(TProject, TRTTIPersister);
  Def.TableName := 'Projects';
  Field := Def.FieldMap.FindByPropertyName('Deleted');
  Field.Options := Field.Options + [foSoftDelete];

  Def := OPRegistry.RegisterPersister(TTask, TRTTIPersister);
  Def.TableName := 'Tasks';
  Field := Def.FieldMap.FindByPropertyName('Deleted');
  Field.Options := Field.Options + [foSoftDelete];

  { Register parent-child relationship for cascade }
  OPRelationRegistry.RegisterRelationship(TProject, TTask, 'ProjectID');

  { Create tables }
  ExecSQL('CREATE TABLE Articles (ID INTEGER PRIMARY KEY, Title TEXT, Deleted BOOLEAN)');
  ExecSQL('CREATE TABLE Comments (ID INTEGER PRIMARY KEY, Text TEXT, DeletedAt REAL)');
  ExecSQL('CREATE TABLE Projects (ID INTEGER PRIMARY KEY, Name TEXT, Deleted BOOLEAN)');
  ExecSQL('CREATE TABLE Tasks (ID INTEGER PRIMARY KEY, ProjectID INTEGER, Description TEXT, Deleted BOOLEAN)');

  { Configure ORM manager }
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  WriteLn('Database setup complete.');
  WriteLn;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  if Assigned(SetupTrans) then
  begin
    SetupTrans.Free;
    SetupTrans := nil;
  end;
  if Assigned(Conn) then
  begin
    if Conn.Connected then
      Conn.Close;
    Conn.Free;
  end;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

var
  Article: TArticle;
  Comment: TComment;
  Project: TProject;
  Task: TTask;
  AllArticles: TObjectList;
  AllTasks: TObjectList;
  I, RowCount: Integer;
begin
  SetupDatabase;
  try
    { Part 1: Boolean soft delete }
    WriteLn('Part 1: Boolean soft delete ===');
    WriteLn('Deleted column: FALSE/NULL = active, TRUE = soft-deleted.');
    WriteLn;

    { Insert 3 articles }
    for I := 1 to 3 do
    begin
      Article := TArticle.Create;
      try
        Article.ID := I;
        Article.Title := 'Article ' + IntToStr(I);
        Article.FState := opsNew;
        OPManager.Save(Article);
      finally
        Article.Free;
      end;
    end;
    WriteLn('Inserted 3 articles.');

    { Soft-delete article 2 }
    Article := TArticle.Create;
    try
      Article.ID := 2;
      OPManager.Load(Article);
      WriteLn('Soft-deleting: "', Article.Title, '"');
      OPManager.Delete(Article);
      WriteLn('  After delete: Deleted=', Article.Deleted,
              ', State=opsDeleted: ', Ord(Article.FState) = Ord(opsDeleted));
    finally
      Article.Free;
    end;
    WriteLn;

    { Verify: record still exists in DB but is filtered from Load }
    RowCount := QueryInt('SELECT COUNT(*) FROM Articles');
    WriteLn('Rows in Articles table (raw SQL): ', RowCount, '  (all 3 still there)');

    AllArticles := TObjectList.Create(True);
    try
      OPManager.Load(AllArticles, TArticle);
      WriteLn('OPManager.Load (no criteria): ', AllArticles.Count, ' articles  (soft-deleted filtered out)');
      for I := 0 to AllArticles.Count - 1 do
        WriteLn('  [', I, '] "', TArticle(AllArticles[I]).Title, '"');
    finally
      AllArticles.Free;
    end;
    WriteLn;

    { Verify: Load(single) on deleted record raises }
    Article := TArticle.Create;
    try
      Article.ID := 2;
      try
        OPManager.Load(Article);
        WriteLn('ERROR: Should not reach here');
      except
        on E: EOPF do
          WriteLn('Load(single) on soft-deleted record: raises as expected');
      end;
    finally
      Article.Free;
    end;
    WriteLn;

    { Part 2: DateTime soft delete }
    WriteLn('Part 2: DateTime soft delete ===');
    WriteLn('DeletedAt column: NULL = active, timestamp = when deleted.');
    WriteLn;

    for I := 1 to 3 do
    begin
      Comment := TComment.Create;
      try
        Comment.ID := I;
        Comment.Text := 'Comment ' + IntToStr(I);
        Comment.FState := opsNew;
        OPManager.Save(Comment);
      finally
        Comment.Free;
      end;
    end;
    WriteLn('Inserted 3 comments.');

    Comment := TComment.Create;
    try
      Comment.ID := 1;
      OPManager.Load(Comment);
      WriteLn('Soft-deleting: "', Comment.Text, '"');
      OPManager.Delete(Comment);
      WriteLn('  DeletedAt = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Comment.DeletedAt));
    finally
      Comment.Free;
    end;

    AllArticles := TObjectList.Create(True);
    try
      OPManager.Load(AllArticles, TComment);
      WriteLn('Load (no criteria): ', AllArticles.Count, ' comments visible');
    finally
      AllArticles.Free;
    end;
    WriteLn;

    { Part 3: Criteria bypass }
    WriteLn('Part 3: Criteria bypass ===');
    WriteLn('Load(list) WITH criteria returns ALL records (including deleted).');
    WriteLn('This gives you full control when you need to see deleted records.');
    WriteLn;

    AllArticles := TObjectList.Create(True);
    try
      OPManager.Load(AllArticles, TArticle, 'ID > 0');
      WriteLn('Load with criteria "ID > 0": ', AllArticles.Count, ' articles  (includes soft-deleted)');
      for I := 0 to AllArticles.Count - 1 do
      begin
        Article := TArticle(AllArticles[I]);
        WriteLn('  [', I, '] "', Article.Title, '" Deleted=', Article.Deleted);
      end;
    finally
      AllArticles.Free;
    end;
    WriteLn;

    { Part 4: Cascade soft delete }
    WriteLn('Part 4: Cascade soft delete');
    WriteLn('Both parent (Project) and child (Task) have foSoftDelete.');
    WriteLn('Deleting the parent soft-deletes children too.');
    WriteLn;

    { Create a project with 3 tasks }
    Project := TProject.Create;
    try
      Project.ID := 1;
      Project.Name := 'Website Redesign';
      Project.FState := opsNew;
      OPManager.Save(Project);
    finally
      Project.Free;
    end;

    for I := 1 to 3 do
    begin
      Task := TTask.Create;
      try
        Task.ID := I;
        Task.ProjectID := 1;
        Task.Description := 'Task ' + IntToStr(I);
        Task.FState := opsNew;
        OPManager.Save(Task);
      finally
        Task.Free;
      end;
    end;
    WriteLn('Created project "Website Redesign" with 3 tasks.');

    { Soft-delete the project - cascade soft-deletes children }
    Project := TProject.Create;
    try
      Project.ID := 1;
      OPManager.Load(Project);
      WriteLn('Soft-deleting project "', Project.Name, '"...');
      OPManager.Delete(Project);
    finally
      Project.Free;
    end;

    { Verify: parent and children all still in DB }
    RowCount := QueryInt('SELECT COUNT(*) FROM Projects');
    WriteLn('  Projects in DB (raw SQL): ', RowCount);
    RowCount := QueryInt('SELECT COUNT(*) FROM Tasks');
    WriteLn('  Tasks in DB (raw SQL):    ', RowCount, '  (all still there)');

    { Verify: all soft-deleted via ORM }
    RowCount := QueryInt('SELECT COUNT(*) FROM Tasks WHERE Deleted = 1');
    WriteLn('  Tasks with Deleted=TRUE:  ', RowCount, '  (cascade soft-deleted)');

    { Verify: LoadChildren returns empty (all children soft-deleted) }
    Project := TProject.Create;
    try
      Project.ID := 1;
      // Use criteria to load the soft-deleted project
      AllArticles := TObjectList.Create(True);
      try
        OPManager.Load(AllArticles, TProject, 'ID = 1');
        if AllArticles.Count > 0 then
        begin
          AllTasks := TObjectList.Create(True);
          try
            OPManager.LoadChildren(TProject(AllArticles[0]), TTask, AllTasks);
            WriteLn('  LoadChildren on soft-deleted project: ', AllTasks.Count, ' tasks visible  (all filtered)');
          finally
            AllTasks.Free;
          end;
        end;
      finally
        AllArticles.Free;
      end;
    finally
      Project.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
