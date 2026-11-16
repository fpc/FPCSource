{ demo_lazy_m2m_interface - Lazy loading of M:N associations via IOPLazyInit
  ==========================================================================
  This demo shows how to use TOPLazy<T> with the IOPLazyInit interface
  (FPC 3.2.2 compatible) to defer loading of many-to-many associations
  until the property is first accessed.

  Compile:  fpc demo_lazy_m2m_interface.lpr -Fu../../src
  Run:      ./demo_lazy_m2m_interface
}
program demo_lazy_m2m_interface;

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
  DB_FILE = 'demo_lazy_m2m_interface.db';

type
  TOPLazyObjectList = specialize TOPLazy<TObjectList>;

  { TTag:
    the associated entity (no lazy loading on this side) }
  TTag = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FLabel_: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Label_: String read FLabel_ write FLabel_;
  end;

  { TArticle:
    has lazy M:N tags via IOPLazyInit }
  TArticle = class(TObject, IOPPersistenceState, IOPLazyInit)
  private
    FID: Integer;
    FTitle: String;
    FState: TObjectPersistenceState;
    FTagList: TObjectList;
    FLazyTags: TOPLazyObjectList;
    FManager: TObject;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetTagList: TObjectList;
    procedure DoLoadTags(aTarget: TObject);
    procedure InitLazyLoaders(aManager: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property TagList: TObjectList read GetTagList;
  end;

{ Implementations }

function TTag.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TTag.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TArticle.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TArticle.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TArticle.Create;
begin
  inherited Create;
  FTagList := TObjectList.Create(True);
  FLazyTags.Init(FTagList);
end;

destructor TArticle.Destroy;
begin
  FTagList.Free;
  inherited Destroy;
end;

function TArticle.GetTagList: TObjectList;
begin
  Result := FLazyTags.Value;
end;

procedure TArticle.DoLoadTags(aTarget: TObject);
begin
  TOPManager(FManager).LoadAssociations(Self, TTag, aTarget);
end;

procedure TArticle.InitLazyLoaders(aManager: TObject);
begin
  FManager := aManager;
  FLazyTags.SetLoader(@DoLoadTags);
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

  ExecSQL('CREATE TABLE Article (ID INTEGER PRIMARY KEY, Title TEXT)');
  ExecSQL('CREATE TABLE Tag (ID INTEGER PRIMARY KEY, Label_ TEXT)');
  ExecSQL('CREATE TABLE article_tag (article_id INTEGER, tag_id INTEGER)');

  Def := OPRegistry.RegisterPersister(TArticle, TRTTIPersister);
  Idx := Def.FieldMap.IndexOfPropertyName('TagList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);
  OPRegistry.RegisterPersister(TTag, TRTTIPersister);

  OPM2MRegistry.RegisterManyToMany(
    TArticle, TTag, 'article_tag', 'article_id', 'tag_id',
    False, 'TagList', '');

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
  Art: TArticle;
  Tag: TTag;
  I: Integer;
begin
  SetupDatabase;
  try
    WriteLn('--- INSERT ---');
    Art := TArticle.Create;
    try
      Art.ID := 1;
      Art.Title := 'Lazy Loading in ORM';
      Art.FState := opsNew;
      OPManager.Save(Art);
      WriteLn('Saved article: "', Art.Title, '"');
    finally
      Art.Free;
    end;

    Tag := TTag.Create;
    try
      Tag.ID := 10;
      Tag.Label_ := 'orm';
      Tag.FState := opsNew;
      OPManager.Save(Tag);
    finally
      Tag.Free;
    end;

    Tag := TTag.Create;
    try
      Tag.ID := 11;
      Tag.Label_ := 'pascal';
      Tag.FState := opsNew;
      OPManager.Save(Tag);
    finally
      Tag.Free;
    end;

    Tag := TTag.Create;
    try
      Tag.ID := 12;
      Tag.Label_ := 'lazy';
      Tag.FState := opsNew;
      OPManager.Save(Tag);
    finally
      Tag.Free;
    end;

    { Create associations via junction table }
    ExecSQL('INSERT INTO article_tag (article_id, tag_id) VALUES (1, 10)');
    ExecSQL('INSERT INTO article_tag (article_id, tag_id) VALUES (1, 11)');
    ExecSQL('INSERT INTO article_tag (article_id, tag_id) VALUES (1, 12)');
    WriteLn('Created 3 tag associations');
    WriteLn;

    { Load article - tags are NOT loaded yet }
    WriteLn('--- LAZY LOAD M:N ---');
    Art := TArticle.Create;
    try
      Art.ID := 1;
      Art.FState := opsKey;
      OPManager.Load(Art);
      WriteLn('Loaded article: "', Art.Title, '"');
      WriteLn('Tags loaded yet? ', Art.FLazyTags.IsLoaded);

      WriteLn('Accessing TagList...');
      WriteLn('Tag count: ', Art.TagList.Count);
      WriteLn('Tags loaded now? ', Art.FLazyTags.IsLoaded);
      for I := 0 to Art.TagList.Count - 1 do
      begin
        Tag := TTag(Art.TagList[I]);
        WriteLn('  [', I, '] "', Tag.Label_, '"');
      end;
    finally
      Art.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
