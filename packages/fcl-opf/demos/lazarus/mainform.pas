unit mainform;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  opfdatamodule;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnRunDemo: TButton;
    MemoOutput: TMemo;
    procedure BtnRunDemoClick(Sender: TObject);
  private
    procedure RunCRUDDemo;
  end;

var
  MainFormInstance: TMainForm;

implementation

uses
  db, sqldb, sqlite3conn, contnrs,
  Opf.Manager, Opf.Registry, Opf.Sql, Opf.Driver, Opf.DDL, Opf.Persister.Rtti;

{$R *.lfm}

const
  DB_FILE = 'demo_lazarus.db';

type
  { TContact - simple model class for the CRUD demo.
    Implements IOPPersistenceState so the ORM can track object state.
    The 'ID' property is auto-detected as the primary key. }
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

function TContact.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TContact.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ Helpers }

var
  Memo: TMemo;

procedure Log(const aMsg: String);
begin
  Memo.Lines.Add(aMsg);
end;

procedure LogContact(const aPrefix: String; C: TContact);
begin
  Log(aPrefix + 'ID=' + IntToStr(C.ID) +
      ' Name="' + C.Name + '" Email="' + C.Email + '"');
end;

{ SetupDatabase - create SQLite connection, register model, create table,
  and wire the connection to the design-time TOPConnection entry. }
procedure SetupDatabase(out aConn: TSQLite3Connection; out aTrans: TSQLTransaction);
var
  Q: TSQLQuery;
  Def: TPersisterDef;
  OpConn: TOPConnection;
  DM: TOPFDataModule;
begin
  DM := OPFDataModuleInstance;

  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  aConn := TSQLite3Connection.Create(nil);
  aTrans := TSQLTransaction.Create(nil);
  aConn.DatabaseName := DB_FILE;
  aConn.Transaction := aTrans;
  aConn.Open;
  Log('1. SQLite connection opened: ' + DB_FILE);

  { Register model class at runtime (cannot be design-time) }
  if DM.OPRegistry1.IndexOfPersister(TContact) = -1 then
    Def := DM.OPRegistry1.RegisterPersister(TContact, TRTTIPersister)
  else
    Def := DM.OPRegistry1.FindPersister(TContact);
  Log('2. TContact registered (table: ' + Def.TableName + ')');

  DM.OPSQLBuilder1.SQLDialect := dSQLite;

  { Create table using the DDL builder }
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := aConn;
    Q.Transaction := aTrans;
    Q.SQL.Text := OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray);
    if not aTrans.Active then
      aTrans.StartTransaction;
    Q.ExecSQL;
    aTrans.Commit;
    Log('3. Table "' + Def.TableName + '" created');
  finally
    Q.Free;
  end;

  { Wire real connection to design-time TOPConnection }
  OpConn := DM.OPManager1.Connections[0] as TOPConnection;
  OpConn.SingleConnection := aConn;
  Log('4. Connection assigned to TOPConnection "' + OpConn.Name + '"');
end;

procedure TeardownDatabase(aConn: TSQLite3Connection; aTrans: TSQLTransaction);
var
  OpConn: TOPConnection;
begin
  OpConn := OPFDataModuleInstance.OPManager1.Connections[0] as TOPConnection;
  OpConn.SingleConnection := nil;
  aTrans.Free;
  if aConn.Connected then
    aConn.Close;
  aConn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ TMainForm }

procedure TMainForm.BtnRunDemoClick(Sender: TObject);
begin
  MemoOutput.Clear;
  BtnRunDemo.Enabled := False;
  try
    RunCRUDDemo;
  finally
    BtnRunDemo.Enabled := True;
  end;
end;

procedure TMainForm.RunCRUDDemo;
var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Mgr: TOPManager;
  Contact: TContact;
  AllContacts: TObjectList;
  I: Integer;
begin
  Memo := MemoOutput;
  Log('=== Lazarus Design-Time OPF Demo ===');
  Log('');

  SetupDatabase(Conn, Trans);
  try
    Mgr := OPFDataModuleInstance.OPManager1;
    Log('');

    { CREATE }
    Log('--- CREATE ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      Contact.Name := 'Alice';
      Contact.Email := 'alice@example.com';
      Contact.FState := opsNew;
      Mgr.Save(Contact);
      LogContact('Saved: ', Contact);
    finally
      Contact.Free;
    end;

    { READ }
    Log('--- READ ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      Mgr.Load(Contact);
      LogContact('Loaded: ', Contact);
    finally
      Contact.Free;
    end;

    { UPDATE }
    Log('--- UPDATE ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      Mgr.Load(Contact);
      Log('Before: Name="' + Contact.Name + '"');
      Contact.Name := 'Alice Smith';
      Contact.FState := opsUpdate;
      Mgr.Save(Contact);
      Log('After:  Name="' + Contact.Name + '"');
    finally
      Contact.Free;
    end;

    { LIST (insert second contact, then load all) }
    Log('--- LIST ---');
    Contact := TContact.Create;
    try
      Contact.ID := 2;
      Contact.Name := 'Bob';
      Contact.Email := 'bob@example.com';
      Contact.FState := opsNew;
      Mgr.Save(Contact);
    finally
      Contact.Free;
    end;
    AllContacts := TObjectList.Create(True);
    try
      Mgr.Load(AllContacts, TContact);
      Log('Total contacts: ' + IntToStr(AllContacts.Count));
      for I := 0 to AllContacts.Count - 1 do
        LogContact('  [' + IntToStr(I) + '] ', TContact(AllContacts[I]));
    finally
      AllContacts.Free;
    end;

    { DELETE }
    Log('--- DELETE ---');
    Contact := TContact.Create;
    try
      Contact.ID := 1;
      Mgr.Load(Contact);
      Log('Deleting: ID=' + IntToStr(Contact.ID) + ' Name="' + Contact.Name + '"');
      Mgr.Delete(Contact);
      Log('Deleted successfully.');
    finally
      Contact.Free;
    end;

    Log('');
    Log('=== Demo completed successfully ===');
  finally
    TeardownDatabase(Conn, Trans);
  end;
end;

end.
