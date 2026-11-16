{ demo_migration - Incremental adoption: replacing hand-written SQL with fcl-opf
  =================================================================================
  This demo shows how to incrementally adopt an ORM in an existing codebase.
  It walks through four phases:

  Phase 1 (Legacy):      Hand-written SQL with TSQLQuery -- the "before" state
  Phase 2 (ORM adoption): Same operations via OPManager.Save/Load/Delete
  Phase 3 (Coexistence):  Legacy SQL and ORM working on the same table
  Phase 4 (Custom SQL):   OPManager.LoadSQL for complex queries the ORM can't generate

  Key insight: you don't have to convert everything at once. The ORM and
  direct sqldb operations can coexist on the same connection and table.

  Compile:  fpc demo_migration.lpr -Fu../../src
  Run:      ./demo_migration
}
program demo_migration;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  { ORM units }
  Opf.Manager,         { TOPManager - central facade }
  Opf.Registry,        { TOPRegistry - class registration }
  Opf.Fields,          { TOPFieldMap - field mappings }
  Opf.Sql,             { TOPSQLBuilder - SQL generation }
  Opf.Driver,
  Opf.Persister.Rtti;  { TRTTIPersister - automatic RTTI-based mapping }

const
  DB_FILE = 'demo_migration.db';

type
  { TCustomer:
    The model class used in all four phases.
    The same class works with both hand-written SQL and the ORM. }

  TCustomer = class(TObject, IOPPersistenceState)
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
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  { Create the customer table -- used by all phases }
  ExecSQL('CREATE TABLE Customer (ID INTEGER PRIMARY KEY, Name TEXT, Email TEXT)');

  WriteLn('Database setup complete. Table "Customer" created.');
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

{ Phase 1: Legacy approach -- hand-written SQL
  This is the "before" state: all SQL is written manually.
  Every operation requires creating TSQLQuery, setting parameters,
  handling transactions, and reading result sets field by field. }
procedure Phase1_LegacySQL;
var
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  WriteLn('=== Phase 1: Legacy approach (hand-written SQL) ===');
  WriteLn('All SQL is written manually with TSQLQuery.');
  WriteLn;

  Trans := TSQLTransaction.Create(nil);
  try
    Trans.Database := Conn;

    { manual parameter binding }
    WriteLn('  INSERT with manual parameter binding:');
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := Conn;
      Q.Transaction := Trans;
      Q.SQL.Text := 'INSERT INTO Customer (ID, Name, Email) VALUES (:ID, :Name, :Email)';
      Trans.StartTransaction;
      Q.Params.ParamByName('ID').AsInteger := 1;
      Q.Params.ParamByName('Name').AsString := 'Alice';
      Q.Params.ParamByName('Email').AsString := 'alice@example.com';
      Q.ExecSQL;
      Trans.Commit;
      WriteLn('    Inserted: ID=1 Name="Alice" Email="alice@example.com"');
    finally
      Q.Free;
    end;

    { manual result set reading }
    WriteLn('  SELECT with manual result set reading:');
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := Conn;
      Q.Transaction := Trans;
      Q.SQL.Text := 'SELECT ID, Name, Email FROM Customer WHERE ID = :ID';
      Trans.StartTransaction;
      Q.Params.ParamByName('ID').AsInteger := 1;
      Q.Open;
      if not Q.EOF then
        WriteLn('    Loaded: ID=', Q.FieldByName('ID').AsInteger,
                ' Name="', Q.FieldByName('Name').AsString, '"',
                ' Email="', Q.FieldByName('Email').AsString, '"');
      Q.Close;
      Trans.Commit;
    finally
      Q.Free;
    end;

    { manual parameter binding }
    WriteLn('  UPDATE with manual parameter binding:');
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := Conn;
      Q.Transaction := Trans;
      Q.SQL.Text := 'UPDATE Customer SET Name = :Name WHERE ID = :ID';
      Trans.StartTransaction;
      Q.Params.ParamByName('Name').AsString := 'Alice Smith';
      Q.Params.ParamByName('ID').AsInteger := 1;
      Q.ExecSQL;
      Trans.Commit;
      WriteLn('    Updated: ID=1 Name="Alice Smith"');
    finally
      Q.Free;
    end;

    { manual parameter binding }
    WriteLn('  DELETE with manual parameter binding:');
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := Conn;
      Q.Transaction := Trans;
      Q.SQL.Text := 'DELETE FROM Customer WHERE ID = :ID';
      Trans.StartTransaction;
      Q.Params.ParamByName('ID').AsInteger := 1;
      Q.ExecSQL;
      Trans.Commit;
      WriteLn('    Deleted: ID=1');
    finally
      Q.Free;
    end;
  finally
    Trans.Free;
  end;

  WriteLn;
  WriteLn('  That was a lot of boilerplate for 4 operations!');
  WriteLn;
end;

{ Phase 2: ORM adoption
  Same operations using the ORM. Dramatically less code.
  The class is registered once, then Save/Load/Delete handle everything. }
procedure Phase2_ORMAdoption;
var
  OpConn: TOPConnection;
  Customer: TCustomer;
  Loaded: TCustomer;
begin
  WriteLn('=== Phase 2: ORM adoption ===');
  WriteLn('Same operations via OPManager.Save/Load/Delete.');
  WriteLn;

  { One-time setup: register the class and configure the manager }
  OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  { INSERT via ORM just Create, set properties, Save }
  WriteLn('  INSERT via ORM:');
  Customer := TCustomer.Create;
  try
    Customer.ID := 10;
    Customer.Name := 'Bob';
    Customer.Email := 'bob@example.com';
    Customer.FState := opsNew;
    OPManager.Save(Customer);
    WriteLn('    Saved: ID=', Customer.ID, ' Name="', Customer.Name, '"');
  finally
    Customer.Free;
  end;

  { SELECT via ORM set ID, call Load }
  WriteLn('  SELECT via ORM:');
  Loaded := TCustomer.Create;
  try
    Loaded.ID := 10;
    OPManager.Load(Loaded);
    WriteLn('    Loaded: ID=', Loaded.ID, ' Name="', Loaded.Name,
            '" Email="', Loaded.Email, '"');
  finally
    Loaded.Free;
  end;

  { UPDATE via ORM modify, set opsUpdate, Save }
  WriteLn('  UPDATE via ORM:');
  Customer := TCustomer.Create;
  try
    Customer.ID := 10;
    OPManager.Load(Customer);
    Customer.Name := 'Bob Jones';
    Customer.FState := opsUpdate;
    OPManager.Save(Customer);
    WriteLn('    Updated: ID=', Customer.ID, ' Name="', Customer.Name, '"');
  finally
    Customer.Free;
  end;

  { DELETE via ORM Load, Delete }
  WriteLn('  DELETE via ORM:');
  Customer := TCustomer.Create;
  try
    Customer.ID := 10;
    OPManager.Load(Customer);
    OPManager.Delete(Customer);
    WriteLn('    Deleted: ID=10');
  finally
    Customer.Free;
  end;

  WriteLn;
  WriteLn('  Same result, much less code!');
  WriteLn;
end;

{ Phase 3: Coexistence
  Legacy SQL and ORM operations on the same table and connection.
  This proves you can adopt the ORM incrementally without rewriting
  everything at once. }
procedure Phase3_Coexistence;
var
  Customer: TCustomer;
  Loaded: TCustomer;
  Q: TSQLQuery;
  Trans: TSQLTransaction;
begin
  WriteLn('=== Phase 3: Coexistence (ORM + legacy SQL) ===');
  WriteLn('ORM and hand-written SQL work on the same table.');
  WriteLn;

  { Insert with ORM, query with raw SQL }
  WriteLn('  Insert with ORM:');
  Customer := TCustomer.Create;
  try
    Customer.ID := 20;
    Customer.Name := 'Carol';
    Customer.Email := 'carol@example.com';
    Customer.FState := opsNew;
    OPManager.Save(Customer);
    WriteLn('    ORM saved: ID=', Customer.ID, ' Name="', Customer.Name, '"');
  finally
    Customer.Free;
  end;

  WriteLn('  Query with raw SQL (same table, same connection):');
  Trans := TSQLTransaction.Create(nil);
  try
    Trans.Database := Conn;
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := Conn;
      Q.Transaction := Trans;
      Q.SQL.Text := 'SELECT ID, Name, Email FROM Customer WHERE ID = 20';
      Trans.StartTransaction;
      Q.Open;
      if not Q.EOF then
        WriteLn('    Raw SQL found: ID=', Q.FieldByName('ID').AsInteger,
                ' Name="', Q.FieldByName('Name').AsString,
                '" Email="', Q.FieldByName('Email').AsString, '"');
      Q.Close;
      Trans.Commit;
    finally
      Q.Free;
    end;
  finally
    Trans.Free;
  end;
  WriteLn;

  { Insert with raw SQL, load with ORM }
  WriteLn('  Insert with raw SQL:');
  Trans := TSQLTransaction.Create(nil);
  try
    Trans.Database := Conn;
    Q := TSQLQuery.Create(nil);
    try
      Q.Database := Conn;
      Q.Transaction := Trans;
      Q.SQL.Text := 'INSERT INTO Customer (ID, Name, Email) VALUES (:ID, :Name, :Email)';
      Trans.StartTransaction;
      Q.Params.ParamByName('ID').AsInteger := 21;
      Q.Params.ParamByName('Name').AsString := 'Dave';
      Q.Params.ParamByName('Email').AsString := 'dave@example.com';
      Q.ExecSQL;
      Trans.Commit;
      WriteLn('    Raw SQL inserted: ID=21 Name="Dave"');
    finally
      Q.Free;
    end;
  finally
    Trans.Free;
  end;

  WriteLn('  Load with ORM (same row inserted by raw SQL):');
  Loaded := TCustomer.Create;
  try
    Loaded.ID := 21;
    OPManager.Load(Loaded);
    WriteLn('    ORM loaded: ID=', Loaded.ID, ' Name="', Loaded.Name,
            '" Email="', Loaded.Email, '"');
  finally
    Loaded.Free;
  end;

  WriteLn;
  WriteLn('  ORM and raw SQL coexist seamlessly!');
  WriteLn;
end;

{ Phase 4: Custom SQL escape hatch
  Use OPManager.LoadSQL for complex queries that the ORM's built-in
  Load method can't generate (filtered queries, JOINs, aggregates).
  The ORM maps the result set to objects, but you write the SQL. }
procedure Phase4_CustomSQL;
var
  Customer: TCustomer;
  AllCustomers: TObjectList;
  I: Integer;
begin
  WriteLn('=== Phase 4: Custom SQL escape hatch ===');
  WriteLn('OPManager.LoadSQL for queries the ORM can''t generate.');
  WriteLn;

  { Insert a few more customers for filtering }
  Customer := TCustomer.Create;
  try
    Customer.ID := 30;
    Customer.Name := 'Eve';
    Customer.Email := 'eve@example.com';
    Customer.FState := opsNew;
    OPManager.Save(Customer);
  finally
    Customer.Free;
  end;

  { LoadSQL with a single object - filtered by custom WHERE }
  WriteLn('  LoadSQL: single object with custom WHERE:');
  Customer := TCustomer.Create;
  try
    OPManager.LoadSQL(Customer,
      'SELECT ID, Name, Email FROM Customer WHERE Name = :name',
      ['name', 'Carol']);
    WriteLn('    Found: ID=', Customer.ID, ' Name="', Customer.Name,
            '" Email="', Customer.Email, '"');
  finally
    Customer.Free;
  end;

  { LoadSQL with a list - custom WHERE returning multiple rows }
  WriteLn('  LoadSQL: list with custom WHERE:');
  AllCustomers := TObjectList.Create(True);
  try
    OPManager.LoadSQL(AllCustomers, TCustomer,
      'SELECT ID, Name, Email FROM Customer WHERE ID >= :minid ORDER BY Name',
      ['minid', 20]);
    WriteLn('    Results (ID >= 20, ordered by Name):');
    for I := 0 to AllCustomers.Count - 1 do
    begin
      Customer := TCustomer(AllCustomers[I]);
      WriteLn('      ID=', Customer.ID, ' Name="', Customer.Name,
              '" Email="', Customer.Email, '"');
    end;
  finally
    AllCustomers.Free;
  end;

  { LoadSQL without parameters }
  WriteLn('  LoadSQL: all rows without parameters:');
  AllCustomers := TObjectList.Create(True);
  try
    OPManager.LoadSQL(AllCustomers, TCustomer,
      'SELECT ID, Name, Email FROM Customer ORDER BY ID');
    WriteLn('    Total customers in table: ', AllCustomers.Count);
    for I := 0 to AllCustomers.Count - 1 do
    begin
      Customer := TCustomer(AllCustomers[I]);
      WriteLn('      ID=', Customer.ID, ' Name="', Customer.Name, '"');
    end;
  finally
    AllCustomers.Free;
  end;

  WriteLn;
  WriteLn('  LoadSQL gives you full SQL power with ORM result mapping.');
  WriteLn;
end;

begin
  SetupDatabase;
  try
    Phase1_LegacySQL;
    Phase2_ORMAdoption;
    Phase3_Coexistence;
    Phase4_CustomSQL;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
