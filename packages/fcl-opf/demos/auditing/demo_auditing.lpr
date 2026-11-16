{ demo_auditing - Automatic auditing columns and lifecycle hooks
  ================================================================
  This demo shows two related features of fcl-opf:

  1. Automatic auditing columns (foCreatedAt, foUpdatedAt, foCreatedBy,
     foUpdatedBy) that are populated automatically during Save, without
     any manual code in each Save call.

  2. Entity lifecycle hooks (BeforeSave, AfterSave, AfterLoad, BeforeDelete,
     AfterDelete) that fire at key persistence points - either as published
     events (for datamodule point-and-click) or as protected virtual methods
     (for subclass overrides).

  Part 1: Timestamp auditing - foCreatedAt / foUpdatedAt
  Part 2: User tracking - foCreatedBy / foUpdatedBy with NumericalID
  Part 3: User tracking - foCreatedBy / foUpdatedBy with StringID
  Part 4: Lifecycle hooks - event-based notification on Save/Load/Delete

  Compile:  lazbuild demo_auditing.lpi
  Run:      ./demo_auditing
}
program demo_auditing;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  { ORM units }
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_auditing.db';

type

  { Part 1 & 2: 
    Model with timestamp and integer user auditing
    CreatedAt/UpdatedAt are TDateTime (float) fields.
    CreatedBy/UpdatedBy are Integer fields mapped to a numerical user ID. }

  TDocument = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
    FCreatedBy: Integer;
    FUpdatedBy: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
    property CreatedBy: Integer read FCreatedBy write FCreatedBy;
    property UpdatedBy: Integer read FUpdatedBy write FUpdatedBy;
  end;

  { Part 3: 
    Model with string user auditing
    CreatedByUser/UpdatedByUser are String fields mapped to a username. }

  TLogEntry = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FMessage: String;
    FCreatedAt: TDateTime;
    FCreatedByUser: String;
    FUpdatedByUser: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Message: String read FMessage write FMessage;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property CreatedByUser: String read FCreatedByUser write FCreatedByUser;
    property UpdatedByUser: String read FUpdatedByUser write FUpdatedByUser;
  end;

  { Part 4: 
    Helper class for lifecycle hook event handlers
    Holds event handler methods that match TOPSaveEvent / TOPObjectEvent. }

  THookLogger = class
    procedure OnBeforeSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
    procedure OnAfterSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
    procedure OnAfterLoad(Sender: TObject; aObject: TObject);
    procedure OnBeforeDelete(Sender: TObject; aObject: TObject);
    procedure OnAfterDelete(Sender: TObject; aObject: TObject);
  end;

{  IOPPersistenceState implementations  }

function TDocument.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TDocument.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TLogEntry.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TLogEntry.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{  THookLogger event handlers  }

procedure THookLogger.OnBeforeSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
const
  StateNames: array[TObjectPersistenceState] of String =
    ('opsClean', 'opsKey', 'opsNew', 'opsUpdate', 'opsDelete', 'opsDeleted');
begin
  WriteLn('  [Hook] BeforeSave: ', aObject.ClassName, ' state=', StateNames[aState]);
end;

procedure THookLogger.OnAfterSave(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState);
const
  StateNames: array[TObjectPersistenceState] of String =
    ('opsClean', 'opsKey', 'opsNew', 'opsUpdate', 'opsDelete', 'opsDeleted');
begin
  WriteLn('  [Hook] AfterSave:  ', aObject.ClassName, ' state=', StateNames[aState]);
end;

procedure THookLogger.OnAfterLoad(Sender: TObject; aObject: TObject);
begin
  WriteLn('  [Hook] AfterLoad:  ', aObject.ClassName);
end;

procedure THookLogger.OnBeforeDelete(Sender: TObject; aObject: TObject);
begin
  WriteLn('  [Hook] BeforeDelete: ', aObject.ClassName);
end;

procedure THookLogger.OnAfterDelete(Sender: TObject; aObject: TObject);
begin
  WriteLn('  [Hook] AfterDelete:  ', aObject.ClassName);
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

  {  Register TDocument 
    Configure audit field options on the field map entries. }
  Def := OPRegistry.RegisterPersister(TDocument, TRTTIPersister);
  Def.TableName := 'Documents';

  { Mark timestamp fields }
  Field := Def.FieldMap.FindByPropertyName('CreatedAt');
  Field.Options := Field.Options + [foCreatedAt];

  Field := Def.FieldMap.FindByPropertyName('UpdatedAt');
  Field.Options := Field.Options + [foUpdatedAt, foRequired];
  { foRequired on UpdatedAt means: set on insert and update.
    Without foRequired, UpdatedAt would only be set on update. }

  { Mark user tracking fields }
  Field := Def.FieldMap.FindByPropertyName('CreatedBy');
  Field.Options := Field.Options + [foCreatedBy];

  Field := Def.FieldMap.FindByPropertyName('UpdatedBy');
  Field.Options := Field.Options + [foUpdatedBy, foRequired];

  { Register TLogEntry:  String-based user tracking with timestamp. }
  Def := OPRegistry.RegisterPersister(TLogEntry, TRTTIPersister);
  Def.TableName := 'LogEntries';

  Field := Def.FieldMap.FindByPropertyName('CreatedAt');
  Field.Options := Field.Options + [foCreatedAt];

  Field := Def.FieldMap.FindByPropertyName('CreatedByUser');
  Field.Options := Field.Options + [foCreatedBy];

  Field := Def.FieldMap.FindByPropertyName('UpdatedByUser');
  Field.Options := Field.Options + [foUpdatedBy];
  { No foRequired here: UpdatedByUser is set ONLY on update, not on insert. }

  { Create tables }
  ExecSQL('CREATE TABLE Documents (ID INTEGER PRIMARY KEY, Title TEXT, ' +
          'CreatedAt REAL, UpdatedAt REAL, CreatedBy INTEGER, UpdatedBy INTEGER)');
  ExecSQL('CREATE TABLE LogEntries (ID INTEGER PRIMARY KEY, Message TEXT, ' +
          'CreatedAt REAL, CreatedByUser TEXT, UpdatedByUser TEXT)');

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
  Doc: TDocument;
  Loaded: TDocument;
  Entry: TLogEntry;
  HookLogger: THookLogger;
begin
  SetupDatabase;
  try
    { Part 1: 
      Automatic timestamps (foCreatedAt / foUpdatedAt)
    }
    WriteLn('=== Part 1: Automatic timestamps ===');
    WriteLn('foCreatedAt is set on INSERT only.');
    WriteLn('foUpdatedAt + foRequired is set on BOTH insert and update.');
    WriteLn;

    { Configure the audit info - timestamps use local time by default }
    OPManager.AuditInfo.TimestampKind := tkLocal;
    { Set the current user as a numerical ID }
    OPManager.AuditInfo.NumericalID := 100;

    Doc := TDocument.Create;
    try
      Doc.ID := 1;
      Doc.Title := 'Design Specification';
      Doc.FState := opsNew;
      OPManager.Save(Doc);
      WriteLn('INSERT:');
      WriteLn('  Title     = "', Doc.Title, '"');
      WriteLn('  CreatedAt = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Doc.CreatedAt));
      WriteLn('  UpdatedAt = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Doc.UpdatedAt));
      WriteLn('  CreatedBy = ', Doc.CreatedBy);
      WriteLn('  UpdatedBy = ', Doc.UpdatedBy);
      WriteLn('  (Both timestamps set on insert because UpdatedAt has foRequired)');
    finally
      Doc.Free;
    end;
    WriteLn;

    { Simulate a short pause so timestamps differ }
    Sleep(100);

    { Update the document - switch to a different user }
    OPManager.AuditInfo.NumericalID := 200;

    Doc := TDocument.Create;
    try
      Doc.ID := 1;
      OPManager.Load(Doc);
      WriteLn('After LOAD:');
      WriteLn('  CreatedAt = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Doc.CreatedAt));
      WriteLn('  CreatedBy = ', Doc.CreatedBy, '  (user 100 - the original creator)');

      Doc.Title := 'Design Specification v2';
      Doc.FState := opsUpdate;
      OPManager.Save(Doc);
      WriteLn;
      WriteLn('After UPDATE (by user 200):');
      WriteLn('  Title     = "', Doc.Title, '"');
      WriteLn('  CreatedAt = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Doc.CreatedAt),
              '  (unchanged - foCreatedAt only fires on insert)');
      WriteLn('  UpdatedAt = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Doc.UpdatedAt),
              '  (updated to now)');
      WriteLn('  CreatedBy = ', Doc.CreatedBy,
              '  (unchanged - foCreatedBy only fires on insert)');
      WriteLn('  UpdatedBy = ', Doc.UpdatedBy,
              '  (updated to user 200)');
    finally
      Doc.Free;
    end;
    WriteLn;

    { Part 2: UTC timestamps }
    WriteLn('=== Part 2: UTC timestamps ===');
    WriteLn('Switch TimestampKind to tkUTC for UTC-based auditing.');
    WriteLn;

    OPManager.AuditInfo.TimestampKind := tkUTC;

    Doc := TDocument.Create;
    try
      Doc.ID := 2;
      Doc.Title := 'UTC Document';
      Doc.FState := opsNew;
      OPManager.Save(Doc);
      WriteLn('INSERT with tkUTC:');
      WriteLn('  CreatedAt (UTC) = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Doc.CreatedAt));
      WriteLn('  Local Now       = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    finally
      Doc.Free;
    end;
    WriteLn;

    { Reset to local time for the rest }
    OPManager.AuditInfo.TimestampKind := tkLocal;

    { Part 3: String-based user tracking (foCreatedBy / foUpdatedBy) }
    WriteLn('=== Part 3: String-based user tracking ===');
    WriteLn('AuditInfo.StringID sets the user identity as a string.');
    WriteLn('foUpdatedBy WITHOUT foRequired: not set on insert, only on update.');
    WriteLn;

    { Switch from numerical to string user identity }
    OPManager.AuditInfo.StringID := 'alice';

    Entry := TLogEntry.Create;
    try
      Entry.ID := 1;
      Entry.Message := 'System started';
      Entry.FState := opsNew;
      OPManager.Save(Entry);
      WriteLn('INSERT:');
      WriteLn('  Message       = "', Entry.Message, '"');
      WriteLn('  CreatedAt     = ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Entry.CreatedAt));
      WriteLn('  CreatedByUser = "', Entry.CreatedByUser, '"');
      WriteLn('  UpdatedByUser = "', Entry.UpdatedByUser, '"',
              '  (empty - foUpdatedBy without foRequired skips insert)');
    finally
      Entry.Free;
    end;
    WriteLn;

    { Update as a different user }
    OPManager.AuditInfo.StringID := 'bob';

    Entry := TLogEntry.Create;
    try
      Entry.ID := 1;
      OPManager.Load(Entry);
      Entry.Message := 'System started (edited)';
      Entry.FState := opsUpdate;
      OPManager.Save(Entry);
      WriteLn('After UPDATE (by bob):');
      WriteLn('  Message       = "', Entry.Message, '"');
      WriteLn('  CreatedByUser = "', Entry.CreatedByUser, '"',
              '  (still alice - foCreatedBy only fires on insert)');
      WriteLn('  UpdatedByUser = "', Entry.UpdatedByUser, '"',
              '  (bob - set on update)');
    finally
      Entry.Free;
    end;
    WriteLn;

    { Part 4: 
      Entity lifecycle hooks.
      Switch back to numerical user ID for TDocument's integer fields 
    }
    OPManager.AuditInfo.NumericalID := 300;

    WriteLn('=== Part 4: Entity lifecycle hooks ===');
    WriteLn('Published events fire at key persistence points.');
    WriteLn('Assign handlers to BeforeSave, AfterSave, AfterLoad,');
    WriteLn('BeforeDelete, AfterDelete on the manager.');
    WriteLn;

    HookLogger := THookLogger.Create;
    try
      { Assign event handlers - in a Lazarus datamodule, you would do
        this with point-and-click in the Object Inspector. }
      OPManager.BeforeSave := @HookLogger.OnBeforeSave;
      OPManager.AfterSave := @HookLogger.OnAfterSave;
      OPManager.AfterLoad := @HookLogger.OnAfterLoad;
      OPManager.BeforeDelete := @HookLogger.OnBeforeDelete;
      OPManager.AfterDelete := @HookLogger.OnAfterDelete;

      WriteLn('Saving a new document...');
      Doc := TDocument.Create;
      try
        Doc.ID := 3;
        Doc.Title := 'Hook Demo';
        Doc.FState := opsNew;
        OPManager.Save(Doc);
        WriteLn('  Title="', Doc.Title, '"');
      finally
        Doc.Free;
      end;
      WriteLn;

      WriteLn('Loading the document...');
      Loaded := TDocument.Create;
      try
        Loaded.ID := 3;
        OPManager.Load(Loaded);
        WriteLn('  Title="', Loaded.Title, '"');
      finally
        Loaded.Free;
      end;
      WriteLn;

      WriteLn('Deleting the document...');
      Doc := TDocument.Create;
      try
        Doc.ID := 3;
        OPManager.Load(Doc);
        OPManager.Delete(Doc);
        WriteLn('  Deleted.');
      finally
        Doc.Free;
      end;
      WriteLn;

      { Clear hooks }
      OPManager.BeforeSave := nil;
      OPManager.AfterSave := nil;
      OPManager.AfterLoad := nil;
      OPManager.BeforeDelete := nil;
      OPManager.AfterDelete := nil;
    finally
      HookLogger.Free;
    end;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
