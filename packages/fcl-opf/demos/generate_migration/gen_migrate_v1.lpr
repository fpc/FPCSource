{ gen_migrate_v1 - Create initial v1 database
  =============================================
  Creates an SQLite database with a simple schema (Customer table)
  and inserts sample data. The migration tracker records version 1.

  Run this first, then run gen_migrate_v2 to generate the update SQL.

  Compile:  fpc gen_migrate_v1.lpr -Fu../../src
  Run:      ./gen_migrate_v1
}
program gen_migrate_v1;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql, Opf.DDL, Opf.Driver,
  Opf.Persister.Rtti;

const
  DB_FILE = 'gen_migrate_demo.db';

type
  { TCustomer - v1 schema: ID, Name }
  TCustomer = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

function TCustomer.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCustomer.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  OpConn: TOPConnection;
  C: TCustomer;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(Conn);
  Conn.DatabaseName := DB_FILE;
  Conn.Transaction := Trans;
  Conn.Open;

  { Register v1 model }
  OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  { Create schema - records version 1 in _opf_migrations }
  WriteLn('Creating v1 schema...');
  OPManager.CreateSchema(Conn, [coIfNotExists]);
  WriteLn('Schema created. Migration version: 1');

  { Insert sample data }
  C := TCustomer.Create;
  try
    C.ID := 1;
    C.Name := 'Alice';
    C.FState := opsNew;
    OPManager.Save(C);
  finally
    C.Free;
  end;

  C := TCustomer.Create;
  try
    C.ID := 2;
    C.Name := 'Bob';
    C.FState := opsNew;
    OPManager.Save(C);
  finally
    C.Free;
  end;

  WriteLn('Inserted 2 customers.');
  WriteLn;
  WriteLn('Database created: ', DB_FILE);
  WriteLn('Now run gen_migrate_v2 to generate the migration SQL.');

  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  Conn.Close;
  Conn.Free;
end.
