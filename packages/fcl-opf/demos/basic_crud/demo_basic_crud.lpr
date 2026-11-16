{ demo_basic_crud - Register a class, Save, Load, Update, Delete
  ================================================================
  This demo shows the fundamental CRUD (Create, Read, Update, Delete)
  operations of the fcl-opf object persistence framework.

  Compile:  fpc demo_basic_crud.lpr -Fu../../src
  Run:      ./demo_basic_crud
}
program demo_basic_crud;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  { ORM units }
  Opf.Manager,        { TOPManager - central facade for all persistence operations }
  Opf.Registry,       { TOPRegistry - class-to-persister registration }
  Opf.Fields,         { TOPFieldMap - field mapping between objects and database columns }
  Opf.Sql,            { TOPSQLBuilder - SQL generation with dialect support }
  Opf.Driver,
  Opf.DDL,            { TOPDDLBuilder - DDL (CREATE TABLE) generation }
  Opf.Persister.Rtti; { TRTTIPersister - automatic RTTI-based property mapping }

const
  DB_FILE = 'demo_basic_crud.db';

{ ---- Model class ----
  Any class with published properties can be persisted.
  The class must implement IOPPersistenceState so the ORM can track
  whether the object is new, clean (loaded), updated, or deleted.
  A property named 'ID' is automatically recognized as the primary key. }
type
  TContact = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FEmail: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Email: String read FEmail write FEmail;
  end;

{ IOPPersistenceState implementation:  
  the ORM uses this to decide whether Save should INSERT (opsNew) or UPDATE (opsUpdate). }
function TContact.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TContact.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;

{ Helper: 
  execute a SQL statement directly against the database.
  Used here only to create the table; all CRUD goes through the ORM.
  The transaction is created once and reused for DDL operations. }
var
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

{ Set up the SQLite database and configure the ORM. }
procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
begin
  { Delete old DB file if it exists }
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  { Create and open a SQLite connection.
    SQLite creates the database file automatically when opened. }
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  { SQLite requires a transaction object assigned before opening. }
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  { Step 1: Register the class with the RTTI persister.
    TRTTIPersister automatically discovers published properties and maps
    them to database columns with the same name. }
  Def := OPRegistry.RegisterPersister(TContact, TRTTIPersister);

  { Step 2: Set the SQL dialect so the ORM generates SQLite-compatible SQL. }
  OPSQLBuilder.SQLDialect := dSQLite;

  { Step 3: Create the database table using the DDL builder.
    GetCreateSQL generates a CREATE TABLE statement from the field map,
    including a PRIMARY KEY constraint for fields marked foInKey. }
  ExecSQL(OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray));

  { Step 4: Configure the ORM manager with our database connection. }
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  WriteLn('Database setup complete. Table "', Def.TableName, '" created.');
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

{ Main demo }
var
  Contact: TContact;
  Loaded: TContact;
  AllContacts: TObjectList;
  I: Integer;
begin
  SetupDatabase;
  try
    { CREATE (INSERT):
      Create a new object, set its state to opsNew, and call Save.
      The ORM generates an INSERT statement and executes it. }
    WriteLn('--- CREATE ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      Contact.Name := 'Alice';
      Contact.Email := 'alice@example.com';
      Contact.FState := opsNew;  { Tell the ORM this is a new object }
      OPManager.Save(Contact);
      WriteLn('Saved: ID=', Contact.ID, ' Name="', Contact.Name, '" Email="', Contact.Email, '"');
      WriteLn('State after save: opsClean = ', Ord(Contact.FState) = Ord(opsClean));
    finally
      Contact.Free;
    end;
    WriteLn;

    { READ (SELECT by PK)
      Create an empty object, set its PK, and call Load.
      The ORM generates a SELECT WHERE ID=:ID and populates the object. }
    WriteLn('--- READ ---');
    Loaded := TContact.Create;
    try
      Loaded.ID := 1;   { Set the primary key value to look up }
      OPManager.Load(Loaded);
      WriteLn('Loaded: ID=', Loaded.ID, ' Name="', Loaded.Name, '" Email="', Loaded.Email, '"');
    finally
      Loaded.Free;
    end;
    WriteLn;

    { UPDATE
      Load an object, modify it, and call Save again.
      After Load, the state is opsClean. Change the state to opsUpdate
      to tell the ORM to generate an UPDATE statement. }
    WriteLn('--- UPDATE ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      OPManager.Load(Contact);
      WriteLn('Before update: Name="', Contact.Name, '"');
      Contact.Name := 'Alice Smith';
      Contact.FState := opsUpdate;  { Tell the ORM this object has changes }
      OPManager.Save(Contact);
      WriteLn('After update:  Name="', Contact.Name, '"');
    finally
      Contact.Free;
    end;
    WriteLn;

    { Verify the update persisted }
    Loaded := TContact.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Verified reload: Name="', Loaded.Name, '"');
    finally
      Loaded.Free;
    end;
    WriteLn;

    { Load list
      Insert a second contact, then load all contacts into a list.
      The ORM creates objects and populates them from each result row. }
    WriteLn('--- LOAD LIST ---');
    Contact := TContact.Create;
    try
      Contact.ID := 2;
      Contact.Name := 'Bob';
      Contact.Email := 'bob@example.com';
      Contact.FState := opsNew;
      OPManager.Save(Contact);
    finally
      Contact.Free;
    end;
    AllContacts := TObjectList.Create(True);
    try
      OPManager.Load(AllContacts, TContact);
      WriteLn('Total contacts: ', AllContacts.Count);
      for I := 0 to AllContacts.Count - 1 do
      begin
        Contact := TContact(AllContacts[I]);
        WriteLn('  [', I, '] ID=', Contact.ID, ' Name="', Contact.Name, '" Email="', Contact.Email, '"');
      end;
    finally
      AllContacts.Free;
    end;
    WriteLn;

    { DELETE
      Load an object and call Delete. The ORM generates a DELETE WHERE ID=:ID. }
    WriteLn('--- DELETE ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      OPManager.Load(Contact);
      WriteLn('Deleting: ID=', Contact.ID, ' Name="', Contact.Name, '"');
      OPManager.Delete(Contact);
      WriteLn('Deleted successfully.');
    finally
      Contact.Free;
    end;

    { Verify deletion }
    AllContacts := TObjectList.Create(True);
    try
      OPManager.Load(AllContacts, TContact);
      WriteLn('Contacts remaining after delete: ', AllContacts.Count);
      for I := 0 to AllContacts.Count - 1 do
      begin
        Contact := TContact(AllContacts[I]);
        WriteLn('  [', I, '] ID=', Contact.ID, ' Name="', Contact.Name, '"');
      end;
    finally
      AllContacts.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
