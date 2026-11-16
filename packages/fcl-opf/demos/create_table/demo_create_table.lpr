{ demo_create_table - DDL generation from field maps
  ====================================================
  This demo shows how the ORM can auto-generate CREATE TABLE SQL
  from the registered field mappings of a class. This is useful for
  rapid prototyping, migrations, and understanding how field types
  map to SQL column types.

  Key concepts:
  - OPDDLBuilder.GetCreateSQL generates DDL from field map arrays
  - PropertyTypeInfo determines the SQL column type
  - TSQLCreateOptions control formatting (quotes, newlines)
  - SQLDialect affects type keywords (e.g., BOOLEAN vs SMALLINT)

  Compile:  fpc demo_create_table.lpr -Fu../../src
  Run:      ./demo_create_table
}
program demo_create_table;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  { ORM units }
  Opf.Manager,         { TOPManager -- central facade }
  Opf.Registry,        { TOPRegistry -- class registration }
  Opf.Fields,          { TOPFieldMap -- field mapping }
  Opf.Sql,             { TOPSQLBuilder, TSQLDialect -- SQL generation }
  Opf.Driver,
  Opf.DDL,             { TOPDDLBuilder -- DDL (CREATE TABLE) generation }
  Opf.Persister.Rtti;  { TRTTIPersister -- automatic RTTI-based mapping }

const
  DB_FILE = 'demo_create_table.db';

type
  { TInventoryItem:
    model class with diverse property types to show
    how each Pascal type maps to a SQL column type in the DDL builder. }

  TInventoryItem = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FQuantity: Integer;
    FPrice: Double;
    FActive: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    { Integer -> INT }
    property ID: Integer read FID write FID;
    { String -> VARCHAR(255) }
    property Name: String read FName write FName;
    { Integer -> INT }
    property Quantity: Integer read FQuantity write FQuantity;
    { Double (tkFloat) -> FLOAT }
    property Price: Double read FPrice write FPrice;
    { Boolean -> BOOLEAN (SQLite) or SMALLINT (vanilla) }
    property Active: Boolean read FActive write FActive;
  end;

function TInventoryItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TInventoryItem.SetPersistenceState(aValue: TObjectPersistenceState);
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

  { Register the class so its field map is populated via RTTI }
  OPRegistry.RegisterPersister(TInventoryItem, TRTTIPersister);

  { Set dialect to SQLite for DDL generation }
  OPSQLBuilder.SQLDialect := dSQLite;
  OPDDLBuilder.SQLDialect := dSQLite;

  WriteLn('Database setup complete.');
  WriteLn;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPDDLBuilder.ClearInstance;
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
  Def: TPersisterDef;
  DDL: UTF8String;
  OpConn: TOPConnection;
  Item: TInventoryItem;
  Loaded: TInventoryItem;
begin
  SetupDatabase;
  try
    { Retrieve the persister definition to access the field map }
    Def := OPRegistry.FindPersister(TInventoryItem);
    WriteLn('Registered class: ', Def.InstanceClass.ClassName);
    WriteLn('Table name: ', Def.TableName);
    WriteLn('Field count: ', Def.FieldMap.Count);
    WriteLn;

    { DDL Generation: Default output
      GetCreateSQL takes the table name and field map array.
      It generates a CREATE TABLE statement with column types
      derived from the PropertyTypeInfo of each field. }
    WriteLn('--- DDL: Default (SQLite dialect) ---');
    DDL := OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray);
    WriteLn(DDL);
    WriteLn;

    { DDL Generation: With forced quotes
      coForceQuotes wraps all identifiers in double quotes.
      Useful when table/column names conflict with SQL reserved words. }
    WriteLn('--- DDL: With coForceQuotes ---');
    DDL := OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray,
      [coForceQuotes]);
    WriteLn(DDL);
    WriteLn;

    { DDL Generation: With forced newlines
      coForceNewLine adds line breaks between columns for readability. }
    WriteLn('--- DDL: With coForceNewLine ---');
    DDL := OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray,
      [coForceNewLine]);
    WriteLn(DDL);
    WriteLn;

    { DDL Generation: Both options  }
    WriteLn('--- DDL: With coForceQuotes + coForceNewLine ---');
    DDL := OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray,
      [coForceQuotes, coForceNewLine]);
    WriteLn(DDL);
    WriteLn;

    { DDL Generation: Different dialect
      Changing SQLDialect affects the type keywords.
      E.g., PostgreSQL uses BOOLEAN vs vanilla uses SMALLINT for tkBool. }
    WriteLn('--- DDL: PostgreSQL dialect (for comparison) ---');
    OPDDLBuilder.SQLDialect := dPostGres;
    DDL := OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray,
      [coForceNewLine]);
    WriteLn(DDL);
    WriteLn;

    { Reset to SQLite for actual table creation }
    OPDDLBuilder.SQLDialect := dSQLite;

    { Execute DDL and verify with CRUD:
      Use the DDL builder to create the actual table, then verify
      with ORM operations. }
    WriteLn('--- Execute DDL and verify with CRUD ---');
    OPDDLBuilder.SQLDialect := dSQLite;
    ExecSQL(OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray));
    WriteLn('Table "', Def.TableName, '" created successfully.');

    { Configure ORM manager }
    TOPManager.ClearInstance;
    OpConn := OPManager.Connections.Add as TOPConnection;
    OpConn.SingleConnection := Conn;
    OpConn.IsDefault := True;

    { Insert a test record }
    Item := TInventoryItem.Create;
    try
      Item.ID := 1;
      Item.Name := 'Laptop';
      Item.Quantity := 50;
      Item.Price := 999.99;
      Item.Active := True;
      Item.FState := opsNew;
      OPManager.Save(Item);
      WriteLn('Inserted: ID=', Item.ID, ' Name="', Item.Name,
              '" Qty=', Item.Quantity, ' Price=', Item.Price:0:2,
              ' Active=', Item.Active);
    finally
      Item.Free;
    end;

    { Load it back to verify }
    Loaded := TInventoryItem.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Loaded:   ID=', Loaded.ID, ' Name="', Loaded.Name,
              '" Qty=', Loaded.Quantity, ' Price=', Loaded.Price:0:2,
              ' Active=', Loaded.Active);
    finally
      Loaded.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
