{ 08_custom_step — v2: Add FirstName + LastName columns, custom step splits FullName
  Demonstrates: MigrateSchema with a custom migration step callback.
  The DDL migration adds FirstName and LastName columns.
  The custom step reads FullName, splits at space, populates the new columns.
  Compile: fpc -Fu../../../src v2.pas
  Run:     ./v2  (after running v1 first)                      }
program custom_step_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti;

const
  DB_FILE = 'example.db';

type
  { v2 model: FullName kept, FirstName and LastName added }
  TPerson = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FFullName: String;
    FFirstName: String;
    FLastName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property FullName: String read FFullName write FFullName;
    property FirstName: String read FFirstName write FFirstName;
    property LastName: String read FLastName write FLastName;
  end;

  TMigrationHelper = class
    procedure SplitNames(aConnection: TSQLConnection; aVersion: Integer);
  end;

function TPerson.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;
procedure TPerson.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

procedure TMigrationHelper.SplitNames(aConnection: TSQLConnection; aVersion: Integer);
var
  QRead, QWrite: TSQLQuery;
  Full, First, Last: String;
  SpacePos: Integer;
  PersonID: Integer;
begin
  { Note: MigrateSchema already wraps custom steps in a transaction,
    so we must NOT start our own transaction here. }
  QRead := TSQLQuery.Create(nil);
  QWrite := TSQLQuery.Create(nil);
  try
    QRead.Database := aConnection;
    QRead.Transaction := aConnection.Transaction;
    QWrite.Database := aConnection;
    QWrite.Transaction := aConnection.Transaction;

    { Read all existing rows }
    QRead.SQL.Text := 'SELECT ID, FullName FROM Person';
    QRead.Open;
    while not QRead.EOF do
    begin
      PersonID := QRead.FieldByName('ID').AsInteger;
      Full := QRead.FieldByName('FullName').AsString;

      { Split at first space }
      SpacePos := Pos(' ', Full);
      if SpacePos > 0 then
      begin
        First := Copy(Full, 1, SpacePos - 1);
        Last := Copy(Full, SpacePos + 1, Length(Full));
      end
      else
      begin
        First := Full;
        Last := '';
      end;

      { Update the row }
      QWrite.SQL.Text := 'UPDATE Person SET FirstName = :First, LastName = :Last WHERE ID = :ID';
      QWrite.Params.ParamByName('First').AsString := First;
      QWrite.Params.ParamByName('Last').AsString := Last;
      QWrite.Params.ParamByName('ID').AsInteger := PersonID;
      QWrite.ExecSQL;

      QRead.Next;
    end;
    QRead.Close;
  finally
    QWrite.Free;
    QRead.Free;
  end;
end;

procedure QueryAndShow(aConn: TSQLite3Connection; const aSQL, aLabel: String);
var
  Q: TSQLQuery;
  T: TSQLTransaction;
begin
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
      WriteLn('  ', aLabel, ':');
      while not Q.EOF do
      begin
        WriteLn('    ID=', Q.FieldByName('ID').AsString,
                ' FullName="', Q.FieldByName('FullName').AsString, '"',
                ' FirstName="', Q.FieldByName('FirstName').AsString, '"',
                ' LastName="', Q.FieldByName('LastName').AsString, '"');
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
  I: Integer;

begin
  if not FileExists(DB_FILE) then
  begin
    WriteLn('ERROR: Run v1 first to create ', DB_FILE);
    Halt(1);
  end;

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Helper := TMigrationHelper.Create;
  try
    Conn.DatabaseName := DB_FILE;
    Conn.Transaction := Trans;
    Conn.Open;

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

      { Register v2 model: Person with FullName + FirstName + LastName }
      Registry.RegisterPersister(TPerson, TRTTIPersister);

      { Register custom migration step }
      Manager.RegisterMigration(1, 2, @Helper.SplitNames,
        'Split FullName into FirstName + LastName');

      { Migrate to version 2 }
      WriteLn('v2: Migrating schema from version 1 to 2...');
      Res := Manager.MigrateSchema(Conn, 2);

      WriteLn('  Diffs found: ', Res.DiffsFound);
      WriteLn('  Statements executed: ', Res.StatementsExecuted);
      WriteLn('  Custom steps executed: ', Res.CustomStepsExecuted);
      WriteLn('  Version: ', Res.VersionBefore, ' -> ', Res.VersionAfter);
      WriteLn('  SQL:');
      for I := 0 to Length(Res.SQL) - 1 do
        WriteLn('    ', Res.SQL[I]);

      { Show the split data }
      QueryAndShow(Conn, 'SELECT ID, FullName, FirstName, LastName FROM Person ORDER BY ID',
        'Person data after migration');

      WriteLn('  Migration complete. FirstName and LastName populated from FullName.');
    finally
      Manager.Free;
      M2MReg.Free;
      RelReg.Free;
      Registry.Free;
    end;
  finally
    if Trans.Active then Trans.Rollback;
    Conn.Close;
    Trans.Free;
    Conn.Free;
    Helper.Free;
  end;

  DeleteFile(DB_FILE);
end.
