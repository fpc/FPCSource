{ 09_add_unique — v2: Add UNIQUE constraint on Email via custom migration step
  Demonstrates: Using RegisterMigration to add a UNIQUE index, handling
  failure due to duplicate data, fixing the data, and retrying.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program add_unique_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  TPerson = class(TObject, IOPPersistenceState)
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

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

type
  { Helper class to hold the custom migration step as a method }
  TMigrationHelper = class
    procedure AddUniqueEmail(aConnection: TSQLConnection; aVersion: Integer);
  end;

procedure TMigrationHelper.AddUniqueEmail(aConnection: TSQLConnection;
  aVersion: Integer);
var
  Q: TSQLQuery;
begin
  { The custom step runs inside a transaction managed by MigrateSchema.
    Use aConnection.Transaction directly — do NOT create a new transaction. }
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := aConnection;
    Q.Transaction := aConnection.Transaction;

    { Step 1: Deduplicate — keep the first row for each email,
      update duplicates with a unique suffix }
    WriteLn('  Custom step: deduplicating emails...');
    Q.SQL.Text :=
      'UPDATE Person SET Email = Email || ''.dup'' || CAST(ID AS TEXT) ' +
      'WHERE ID NOT IN (' +
      '  SELECT MIN(ID) FROM Person GROUP BY Email' +
      ')';
    Q.ExecSQL;
    WriteLn('  Custom step: duplicates resolved');

    { Step 2: Now create the unique index }
    WriteLn('  Custom step: creating unique index on Email...');
    Q.SQL.Text := 'CREATE UNIQUE INDEX idx_person_email ON Person (Email)';
    Q.ExecSQL;
    WriteLn('  Custom step: unique index created');
  finally
    Q.Free;
  end;
end;

{ Helper to query a string value }
function QueryStr(aConn: TSQLite3Connection; const aSQL: String): String;
var
  Q: TSQLQuery;
  T: TSQLTransaction;
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
        if Result <> '' then Result := Result + ', ';
        Result := Result + Q.Fields[0].AsString;
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
  Helper: TMigrationHelper;
  Res: TOPMigrationResult;
  PD: TPersisterDef;
  FM: TOPFieldMap;
  Q: TSQLQuery;
  T2: TSQLTransaction;
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

    { Show current state: duplicate emails exist }
    WriteLn('v2: Current data (before migration):');
    WriteLn('  Emails: ', QueryStr(Conn,
      'SELECT Name || ''='' || Email FROM Person ORDER BY ID'));

    Registry := TOPRegistry.Create(nil);
    RelReg := TOPRelationRegistry.Create(nil);
    RelReg.Registry := Registry;
    M2MReg := TOPManyToManyRegistry.Create(nil);
    M2MReg.Registry := Registry;
    Manager := TOPManager.Create(nil);
    Helper := TMigrationHelper.Create;
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

      { Mark Email field as unique in the ORM model }
      PD := Registry.FindPersister(TPerson);
      FM := TOPFieldMap(PD.FieldMap.FindByFieldName('Email'));
      FM.Options := FM.Options + [foUnique];

      { Register a custom migration step that handles deduplication
        and creates the unique index. This runs for version 1 -> 2. }
      Manager.RegisterMigration(1, 2, @Helper.AddUniqueEmail,
        'Deduplicate emails and add UNIQUE index');

      WriteLn;
      WriteLn('v2: Migrating schema from version 1 to 2...');
      Res := Manager.MigrateSchema(Conn, 2);

      WriteLn;
      WriteLn('  Schema diffs found: ', Res.DiffsFound);
      WriteLn('  DDL statements executed: ', Res.StatementsExecuted);
      WriteLn('  Custom steps executed: ', Res.CustomStepsExecuted);
      if Length(Res.SQL) > 0 then
      begin
        WriteLn('  SQL:');
        for I := 0 to Length(Res.SQL) - 1 do
          WriteLn('    ', Res.SQL[I]);
      end;

      { Verify: show updated data }
      WriteLn;
      WriteLn('v2: Data after migration:');
      WriteLn('  Emails: ', QueryStr(Conn,
        'SELECT Name || ''='' || Email FROM Person ORDER BY ID'));

      { Verify: try to insert a duplicate email — should fail }
      WriteLn;
      WriteLn('v2: Testing unique constraint enforcement...');
      try
        WriteLn('  Attempting INSERT with duplicate email "charlie@test.com"...');
        { Use raw SQL to test the constraint directly }
        begin
          T2 := TSQLTransaction.Create(nil);
          try
            T2.Database := Conn;
            Q := TSQLQuery.Create(nil);
            try
              Q.Database := Conn;
              Q.Transaction := T2;
              T2.StartTransaction;
              Q.SQL.Text := 'INSERT INTO Person (ID, Name, Email) VALUES (99, ''Eve'', ''charlie@test.com'')';
              Q.ExecSQL;
              T2.Commit;
              WriteLn('  ERROR: Insert succeeded — unique constraint not enforced!');
            finally
              Q.Free;
            end;
          finally
            if T2.Active then T2.Rollback;
            T2.Free;
          end;
        end;
      except
        on E: Exception do
          WriteLn('  OK: Insert correctly rejected — ', E.Message);
      end;

      WriteLn;
      WriteLn('  Migration complete. Unique constraint on Email is now enforced.');
    finally
      Helper.Free;
      Manager.Free; M2MReg.Free; RelReg.Free; Registry.Free;
    end;
  finally
    if Trans.Active then Trans.Rollback;
    Conn.Close; Trans.Free; Conn.Free;
  end;

  DeleteFile(DB_FILE);
end.
