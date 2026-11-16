{ gen_migrate_v2 - Generate migration SQL from v1 to v2
  =====================================================
  Opens the v1 database created by gen_migrate_v1, registers the v2
  model (Customer with added Email and Country columns, plus a new
  Order table), and uses GenerateMigrationSQL to produce a SQL script
  that can be executed externally (e.g., by an installer or DBA).

  The SQL is written to gen_migrate_v1_to_v2.sql.

  Compile:  fpc gen_migrate_v2.lpr -Fu../../src
  Run:      ./gen_migrate_v2
}
program gen_migrate_v2;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql, Opf.Driver,
  Opf.Migration, Opf.Relations,
  Opf.Persister.Rtti;

const
  DB_FILE = 'gen_migrate_demo.db';
  SQL_FILE = 'gen_migrate_v1_to_v2.sql';

type
  { TCustomer - v2 schema: added Email and Country columns }
  TCustomer = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FEmail: String;
    FCountry: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Email: String read FEmail write FEmail;
    property Country: String read FCountry write FCountry;
  end;

  { TCustOrder - new table in v2 }
  TCustOrder = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FCustomerID: Integer;
    FProduct: String;
    FAmount: Currency;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property CustomerID: Integer read FCustomerID write FCustomerID;
    property Product: String read FProduct write FProduct;
    property Amount: Currency read FAmount write FAmount;
  end;

function TCustomer.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCustomer.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TCustOrder.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCustOrder.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  OpConn: TOPConnection;
  SQL: TUTF8StringArray;
  F: TextFile;
  I: Integer;
begin
  if not FileExists(DB_FILE) then
  begin
    WriteLn('Error: ', DB_FILE, ' not found. Run gen_migrate_v1 first.');
    Halt(1);
  end;

  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(Conn);
  Conn.DatabaseName := DB_FILE;
  Conn.Transaction := Trans;
  Conn.Open;

  { Register v2 model - the DESIRED state }
  OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  OPRegistry.RegisterPersister(TCustOrder, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TCustomer, TCustOrder, 'CustomerID');

  OPSQLBuilder.SQLDialect := dSQLite;

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  { Generate migration SQL without executing }
  WriteLn('Generating migration SQL from v1 to v2...');
  WriteLn;

  SQL := OPManager.GenerateMigrationSQL(Conn, 2, [], [gsoIncludeVersionTracking]);

  if Length(SQL) = 0 then
  begin
    WriteLn('No migration needed - schema is already up to date.');
    Halt(0);
  end;

  { Display to console }
  WriteLn('Generated ', Length(SQL), ' SQL statements:');
  WriteLn('----------------------------------------');
  for I := 0 to Length(SQL) - 1 do
    WriteLn(SQL[I], ';');
  WriteLn('----------------------------------------');
  WriteLn;

  { Write to file }
  AssignFile(F, SQL_FILE);
  Rewrite(F);
  WriteLn(F, '-- Migration script: v1 -> v2');
  WriteLn(F, '-- Generated: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn(F, '-- Database: ', DB_FILE);
  WriteLn(F, '');
  for I := 0 to Length(SQL) - 1 do
    WriteLn(F, SQL[I], ';');
  CloseFile(F);

  WriteLn('SQL script written to: ', SQL_FILE);
  WriteLn;
  WriteLn('To apply manually:');
  WriteLn('  sqlite3 ', DB_FILE, ' < ', SQL_FILE);

  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
  TOPRegistry.ClearInstance;
  Conn.Close;
  Conn.Free;
end.
