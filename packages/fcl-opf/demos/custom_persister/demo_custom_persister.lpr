{ demo_custom_persister - Custom persister class
  =================================================
  This demo shows how to create a custom persister by subclassing TOPPersister.
  A custom persister gives you full control over how object properties map
  to database columns, including type coercion and computed fields.

  When to subclass TOPPersister:
  - You need type coercion (e.g., store an integer enum as text in the DB)
  - You have computed or derived fields
  - You want control that neither RTTI nor IOPFieldProvider provides

  Difference from the other persistence paths:
  - RTTI path (TRTTIPersister): automatic, requires published properties
  - Interface path (TInterfacePersister): manual mapping, defined in the model class
  - Custom path (TOPPersister subclass): manual mapping, defined externally

  Compile:  fpc demo_custom_persister.lpr -Fu../../src
  Run:      ./demo_custom_persister
}
program demo_custom_persister;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs, typinfo,
  { ORM units }
  Opf.Manager,         { TOPManager - central facade for all persistence operations }
  Opf.Registry,        { TOPRegistry, TOPPersister - class registration and base persister }
  Opf.Fields,          { TOPFieldMap - field mapping between objects and database columns }
  Opf.Sql,             { TOPSQLBuilder - SQL generation with dialect support }
  Opf.Driver,
  Opf.Persister.Rtti;  { TRTTIPersister - for reference/comparison }

const
  DB_FILE = 'demo_custom_persister.db';

type
  { Log severity levels as an integer enum.
    The custom persister stores these as text ('INFO', 'WARN', 'ERROR')
    in the database, demonstrating type coercion that the RTTI persister
    cannot do automatically. }
  TLogLevel = (llInfo, llWarn, llError);

const
  LOG_LEVEL_NAMES: array[TLogLevel] of String = ('INFO', 'WARN', 'ERROR');

type
  { TLogEntry:
    model class with IOPPersistenceState.
    Properties are published so the custom persister can use RTTI helpers
    to access them, but the mapping logic is fully custom. }

  TLogEntry = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTimestamp: TDateTime;
    FMessage: String;
    FLevel: TLogLevel;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    property Message: String read FMessage write FMessage;
    property Level: TLogLevel read FLevel write FLevel;
  end;

  { TLogPersister
    custom persister subclass.
    Override 3 methods:
      PopulateFieldMap  -- define which fields exist and their types
      ApplyProperty     -- read DB field values into object properties
      PersistProperty   -- write object properties to DB parameters

    This persister stores the Level enum as text in the DB, which the
    standard RTTI persister would store as an integer. }

  TLogPersister = class(TOPPersister)
  public
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    function ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean; override;
    function PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean; override;
  end;

{ IOPPersistenceState implementation -- required for Save/Load/Delete }
function TLogEntry.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TLogEntry.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ PopulateFieldMap:
  define the field mappings for TLogEntry.
  Called once during RegisterPersister. Each field gets a DB column name,
  object property name, and type info for SQL generation. }
procedure TLogPersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  { ID: auto-increment primary key }
  F := aMap.AddField('ID', 'ID');
  F.Options := F.Options + [foInKey, foAutoInc] - [foInInsert];
  F.PropertyTypeInfo := TypeInfo(Integer);

  { Timestamp: stored as REAL in SQLite (Julian date format) }
  F := aMap.AddField('Timestamp', 'Timestamp');
  F.PropertyTypeInfo := TypeInfo(Double);

  { Message: standard text field }
  F := aMap.AddField('Message', 'Message');
  F.PropertyTypeInfo := TypeInfo(String);

  { Level: stored as TEXT in the DB, but it's a TLogLevel enum in the object.
    We set PropertyTypeInfo to String because the DB column is TEXT.
    The type coercion (enum <-> text) happens in ApplyProperty/PersistProperty. }
  F := aMap.AddField('Level', 'Level');
  F.PropertyTypeInfo := TypeInfo(String);
end;

{ ApplyProperty -- read a DB field value into the object property.
  This is where type coercion happens: the Level column is TEXT in the DB,
  but we convert it to a TLogLevel enum on the object. }
function TLogPersister.ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean;
var
  Entry: TLogEntry;
  LevelStr: String;
  L: TLogLevel;
begin
  Result := True;
  Entry := Instance as TLogEntry;

  if aFieldMap.PropertyName = 'ID' then
  begin
    if aField.IsNull then
      Entry.FID := 0
    else
      Entry.FID := aField.AsInteger;
  end
  else if aFieldMap.PropertyName = 'Timestamp' then
  begin
    if aField.IsNull then
      Entry.FTimestamp := 0
    else
      Entry.FTimestamp := aField.AsFloat;
  end
  else if aFieldMap.PropertyName = 'Message' then
  begin
    if aField.IsNull then
      Entry.FMessage := ''
    else
      Entry.FMessage := aField.AsString;
  end
  else if aFieldMap.PropertyName = 'Level' then
  begin
    { Type coercion: convert text ('INFO','WARN','ERROR') to TLogLevel enum }
    if aField.IsNull then
      Entry.FLevel := llInfo
    else
    begin
      LevelStr := aField.AsString;
      Entry.FLevel := llInfo;  { default }
      for L := Low(TLogLevel) to High(TLogLevel) do
        if SameText(LevelStr, LOG_LEVEL_NAMES[L]) then
        begin
          Entry.FLevel := L;
          Break;
        end;
    end;
  end
  else
    Result := False;
end;

{ PersistProperty:
  write an object property to a DB parameter.
  The Level enum is converted to its text name before writing. }
function TLogPersister.PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
var
  Entry: TLogEntry;
begin
  Result := True;
  Entry := Instance as TLogEntry;

  if aFieldMap.PropertyName = 'ID' then
    aParam.AsInteger := Entry.FID
  else if aFieldMap.PropertyName = 'Timestamp' then
    aParam.AsFloat := Entry.FTimestamp
  else if aFieldMap.PropertyName = 'Message' then
    aParam.AsString := Entry.FMessage
  else if aFieldMap.PropertyName = 'Level' then
    { Type coercion: convert TLogLevel enum to text for storage }
    aParam.AsString := LOG_LEVEL_NAMES[Entry.FLevel]
  else
    Result := False;
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

  { Register TLogEntry with the custom TLogPersister.
    The ORM calls TLogPersister.PopulateFieldMap, ApplyProperty, PersistProperty
    instead of using RTTI or IOPFieldProvider. }
  OPRegistry.RegisterPersister(TLogEntry, TLogPersister);

  OPSQLBuilder.SQLDialect := dSQLite;

  { Create the table. Note: Level is TEXT, not INTEGER.
    The custom persister handles the enum <-> text conversion. }
  ExecSQL('CREATE TABLE LogEntry ' +
    '(ID INTEGER PRIMARY KEY, Timestamp REAL, Message TEXT, Level TEXT)');

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  WriteLn('Database setup complete. Table "LogEntry" created.');
  WriteLn;
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

{ Helper: format a TDateTime for display }
function FormatTS(aTS: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', aTS);
end;

var
  Entry: TLogEntry;
  Loaded: TLogEntry;
  AllEntries: TObjectList;
  I: Integer;
begin
  SetupDatabase;
  try
    { Insert log entries with different severity levels:
      The custom persister converts the TLogLevel enum to text ('INFO',
      'WARN', 'ERROR') when writing to the database. }
    WriteLn('--- SAVE (INSERT with auto-inc and type coercion) ---');

    Entry := TLogEntry.Create;
    try
      Entry.Timestamp := Now;
      Entry.Message := 'Application started';
      Entry.Level := llInfo;
      Entry.FState := opsNew;
      OPManager.Save(Entry);
      WriteLn('Saved: ID=', Entry.ID, ' Level=', LOG_LEVEL_NAMES[Entry.Level],
              ' Message="', Entry.Message, '"');
    finally
      Entry.Free;
    end;

    Entry := TLogEntry.Create;
    try
      Entry.Timestamp := Now;
      Entry.Message := 'Disk space running low';
      Entry.Level := llWarn;
      Entry.FState := opsNew;
      OPManager.Save(Entry);
      WriteLn('Saved: ID=', Entry.ID, ' Level=', LOG_LEVEL_NAMES[Entry.Level],
              ' Message="', Entry.Message, '"');
    finally
      Entry.Free;
    end;

    Entry := TLogEntry.Create;
    try
      Entry.Timestamp := Now;
      Entry.Message := 'Database connection failed';
      Entry.Level := llError;
      Entry.FState := opsNew;
      OPManager.Save(Entry);
      WriteLn('Saved: ID=', Entry.ID, ' Level=', LOG_LEVEL_NAMES[Entry.Level],
              ' Message="', Entry.Message, '"');
    finally
      Entry.Free;
    end;
    WriteLn;

    { Verify type coercion works in reverse:
      The custom persister reads the text 'ERROR' from the DB and converts
      it back to the TLogLevel enum value llError. }
    WriteLn('--- LOAD (with type coercion from TEXT to enum) ---');
    Loaded := TLogEntry.Create;
    try
      Loaded.ID := 3;
      OPManager.Load(Loaded);
      WriteLn('Loaded: ID=', Loaded.ID,
              ' Level=', LOG_LEVEL_NAMES[Loaded.Level],
              ' (enum ord=', Ord(Loaded.Level), ')',
              ' Message="', Loaded.Message, '"');
      WriteLn('  Type coercion: DB stores "ERROR" (text), object has llError (enum)');
    finally
      Loaded.Free;
    end;
    WriteLn;

    { Show all log entries }
    WriteLn('--- LOAD LIST ---');
    AllEntries := TObjectList.Create(True);
    try
      OPManager.Load(AllEntries, TLogEntry);
      WriteLn('Total log entries: ', AllEntries.Count);
      for I := 0 to AllEntries.Count - 1 do
      begin
        Entry := TLogEntry(AllEntries[I]);
        WriteLn('  [', I, '] ID=', Entry.ID,
                ' Time=', FormatTS(Entry.Timestamp),
                ' Level=', LOG_LEVEL_NAMES[Entry.Level],
                ' Msg="', Entry.Message, '"');
      end;
    finally
      AllEntries.Free;
    end;
    WriteLn;

    { Remove a log entry}
    WriteLn('--- DELETE ---');
    Entry := TLogEntry.Create;
    try
      Entry.ID := 1;
      OPManager.Load(Entry);
      WriteLn('Deleting: ID=', Entry.ID, ' Message="', Entry.Message, '"');
      OPManager.Delete(Entry);
      WriteLn('Deleted successfully.');
    finally
      Entry.Free;
    end;

    { Verify deletion }
    AllEntries := TObjectList.Create(True);
    try
      OPManager.Load(AllEntries, TLogEntry);
      WriteLn('Entries remaining: ', AllEntries.Count);
    finally
      AllEntries.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
