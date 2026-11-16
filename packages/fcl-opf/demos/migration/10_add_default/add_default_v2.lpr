{ 10_add_default — v2: Add Status column with DEFAULT 'active'
  Demonstrates: MigrateSchema adding a column with a default value.
  Existing rows receive the default automatically.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program add_default_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, Variants, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  TPerson = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FStatus: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Status: String read FStatus write FStatus;
  end;

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ Helper to query all rows as a formatted string }
function QueryAll(aConn: TSQLite3Connection; const aSQL: String): String;
var
  Q: TSQLQuery;
  T: TSQLTransaction;
  I: Integer;
begin
  Result := '';
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
      while not Q.EOF do
      begin
        if Result <> '' then Result := Result + LineEnding;
        Result := Result + '    ';
        for I := 0 to Q.FieldCount - 1 do
        begin
          if I > 0 then Result := Result + ' | ';
          Result := Result + Q.Fields[I].FieldName + '=' +
            Q.Fields[I].AsString;
        end;
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
  PD: TPersisterDef;
  FM: TOPFieldMap;
  P: TPerson;
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

    { Show current state: no Status column }
    WriteLn('v2: Current data (before migration):');
    WriteLn(QueryAll(Conn, 'SELECT ID, Name FROM Person ORDER BY ID'));

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

      Registry.RegisterPersister(TPerson, TRTTIPersister);

      { Set default value on the Status field.
        After RegisterPersister, the field map is already populated
        via RTTI. We just find the field and set its DefaultValue. }
      PD := Registry.FindPersister(TPerson);
      FM := TOPFieldMap(PD.FieldMap.FindByFieldName('Status'));
      FM.DefaultValue := 'active';

      WriteLn;
      WriteLn('v2: Migrating schema from version 1 to 2...');
      WriteLn('  Adding column Status with DEFAULT ''active''');
      Res := Manager.MigrateSchema(Conn, 2);

      WriteLn;
      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Verify: existing rows should have the default value }
      WriteLn;
      WriteLn('v2: Data after migration (existing rows get default):');
      WriteLn(QueryAll(Conn,
        'SELECT ID, Name, Status FROM Person ORDER BY ID'));

      { Verify via ORM Load }
      WriteLn;
      WriteLn('v2: ORM Load verification:');
      P := TPerson.Create;
      try
        P.ID := 1;
        Manager.Load(P);
        WriteLn('    Person #1: Name="', P.Name, '" Status="', P.Status, '"');

        P.ID := 2;
        Manager.Load(P);
        WriteLn('    Person #2: Name="', P.Name, '" Status="', P.Status, '"');

        P.ID := 3;
        Manager.Load(P);
        WriteLn('    Person #3: Name="', P.Name, '" Status="', P.Status, '"');
      finally
        P.Free;
      end;

      { Insert a new person — Status should get the default if not set }
      WriteLn;
      WriteLn('v2: Inserting new person with explicit Status...');
      P := TPerson.Create;
      try
        P.ID := 4; P.Name := 'Diana'; P.Status := 'inactive';
        P.FState := opsNew;
        Manager.Save(P);
        WriteLn('    Inserted Diana with Status="inactive"');
      finally
        P.Free;
      end;

      WriteLn;
      WriteLn('v2: Final data:');
      WriteLn(QueryAll(Conn,
        'SELECT ID, Name, Status FROM Person ORDER BY ID'));

      WriteLn;
      WriteLn('  Migration complete. Status column added with default ''active''.');
    finally
      Manager.Free; M2MReg.Free; RelReg.Free; Registry.Free;
    end;
  finally
    if Trans.Active then Trans.Rollback;
    Conn.Close; Trans.Free; Conn.Free;
  end;

  DeleteFile(DB_FILE);
end.
