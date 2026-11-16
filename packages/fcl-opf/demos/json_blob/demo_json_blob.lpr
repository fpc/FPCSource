{ demo_json_blob - Class-typed properties stored as JSON columns
  ================================================================
  This demo shows how the ORM automatically serializes class-typed
  properties to JSON when the nested class is NOT registered with
  its own persister.

  When TUserProfile has a published property "Settings: TSettings",
  and TSettings is NOT registered with OPRegistry, the RTTI persister
  stores the entire TSettings object as a JSON string in a single
  database column. On load, it deserializes the JSON back into the object.

  This is useful for storing configuration, preferences, or any
  structured data that doesn't need its own table.

  Compile:  fpc demo_json_blob.lpr -Fu../../src
  Run:      ./demo_json_blob
}
program demo_json_blob;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_json_blob.db';

type
  { TSettings:
    a nested class that will be stored as JSON.
    It descends from TPersistent so the JSON serializer can
    discover its published properties via RTTI.
    We do NOT register this class with the ORM - that's the key:
    unregistered class-typed properties are serialized to JSON. }
  TSettings = class(TPersistent)
  private
    FTheme: String;
    FFontSize: Integer;
    FDarkMode: Boolean;
  published
    property Theme: String read FTheme write FTheme;
    property FontSize: Integer read FFontSize write FFontSize;
    property DarkMode: Boolean read FDarkMode write FDarkMode;
  end;

  { TUserProfile:
    The main persisted class.
    The "Settings" property is a class-typed (TSettings) published property.
    Because TSettings is not registered as a persisted class, the ORM
    serializes it to JSON on Save and deserializes it on Load. }
  TUserProfile = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FUserName: String;
    FSettings: TSettings;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property UserName: String read FUserName write FUserName;
    property Settings: TSettings read FSettings write FSettings;
  end;

function TUserProfile.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TUserProfile.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TUserProfile.Create;
begin
  inherited Create;
  { The Settings object must be created by the owner.
    The ORM will populate its properties on Load, but it does NOT
    create the object itself. }
  FSettings := TSettings.Create;
end;

destructor TUserProfile.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

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

{ Read a column value directly from the database to inspect the stored JSON }
function DirectSelect(aID: Integer; const aColumn: String): String;
var
  Q: TSQLQuery;
begin
  Result := '';
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := 'SELECT ' + aColumn + ' FROM UserProfile WHERE ID=:ID';
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.Params.ParamByName('ID').AsInteger := aID;
    Q.Open;
    if not Q.EOF then
      Result := Q.Fields[0].AsString;
    Q.Close;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  { The "Settings" column is TEXT - it will store JSON. }
  ExecSQL('CREATE TABLE UserProfile (ID INTEGER PRIMARY KEY, UserName TEXT, Settings TEXT)');

  { Register TUserProfile - but NOT TSettings.
    Because TSettings has no persister registration, the RTTI persister
    treats the Settings property as a JSON blob column. }
  OPRegistry.RegisterPersister(TUserProfile, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
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
  Profile, Loaded: TUserProfile;
begin
  SetupDatabase;
  try
    { Save with JSON blob:
      The Settings object's published properties (Theme, FontSize, DarkMode)
      are serialized to a JSON string and stored in the "Settings" column. }
    WriteLn('--- SAVE ---');
    Profile := TUserProfile.Create;
    try
      Profile.ID := 1;
      Profile.UserName := 'alice';
      Profile.Settings.Theme := 'Ocean Blue';
      Profile.Settings.FontSize := 14;
      Profile.Settings.DarkMode := True;
      Profile.FState := opsNew;
      OPManager.Save(Profile);
      WriteLn('Saved profile: UserName="', Profile.UserName, '"');
      WriteLn('  Settings: Theme="', Profile.Settings.Theme,
              '" FontSize=', Profile.Settings.FontSize,
              ' DarkMode=', Profile.Settings.DarkMode);
    finally
      Profile.Free;
    end;
    WriteLn;

    { Inspect the raw JSON stored in the database:
      This shows what the ORM actually writes to the TEXT column. }
    WriteLn('--- RAW JSON IN DATABASE ---');
    WriteLn('Settings column value: ', DirectSelect(1, 'Settings'));
    WriteLn;

    { Load with JSON blob:
      On load, the ORM reads the JSON string from the "Settings" column
      and deserializes it back into the TSettings object. }
    WriteLn('--- LOAD ---');
    Loaded := TUserProfile.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Loaded profile: UserName="', Loaded.UserName, '"');
      WriteLn('  Settings: Theme="', Loaded.Settings.Theme,
              '" FontSize=', Loaded.Settings.FontSize,
              ' DarkMode=', Loaded.Settings.DarkMode);
    finally
      Loaded.Free;
    end;
    WriteLn;

    { Round-trip verification:
      Save a second profile, load it back, verify all values match. }
    WriteLn('--- ROUND-TRIP ---');
    Profile := TUserProfile.Create;
    try
      Profile.ID := 2;
      Profile.UserName := 'bob';
      Profile.Settings.Theme := 'Sunset';
      Profile.Settings.FontSize := 18;
      Profile.Settings.DarkMode := False;
      Profile.FState := opsNew;
      OPManager.Save(Profile);
      WriteLn('Saved:  Theme="', Profile.Settings.Theme,
              '" FontSize=', Profile.Settings.FontSize,
              ' DarkMode=', Profile.Settings.DarkMode);
    finally
      Profile.Free;
    end;

    Loaded := TUserProfile.Create;
    try
      Loaded.ID := 2;
      OPManager.Load(Loaded);
      WriteLn('Loaded: Theme="', Loaded.Settings.Theme,
              '" FontSize=', Loaded.Settings.FontSize,
              ' DarkMode=', Loaded.Settings.DarkMode);
    finally
      Loaded.Free;
    end;
    WriteLn;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
