{ demo_conventions - Convention-based naming and auto-mapping
  =============================================================
  This demo shows how the ORM maps classes to tables and properties
  to columns using naming conventions - zero configuration required.

  The conventions:
  - Table name = class name minus the leading 'T' (TProduct → "Product")
  - Column name = property name (Price → "Price")
  - A property named 'ID' is automatically detected as the primary key

  Compile:  fpc demo_conventions.lpr -Fu../../src
  Run:      ./demo_conventions
}
program demo_conventions;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_conventions.db';

type
  { TProduct - a simple model class.
    Convention: table name will be "Product" (class name minus 'T').
    Convention: each published property maps to a column with the same name.
    Convention: the "ID" property is automatically marked as the primary key. }
  TProduct = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FPrice: Currency;
    FInStock: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Price: Currency read FPrice write FPrice;
    property InStock: Boolean read FInStock write FInStock;
  end;

function TProduct.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TProduct.SetPersistenceState(aValue: TObjectPersistenceState);
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

  { Register the class - zero configuration, just the class and persister type.
    The RTTI persister auto-discovers all published properties. }
  OPRegistry.RegisterPersister(TProduct, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;

  { Create table matching the convention-derived names.
    "Product" = TProduct minus 'T'. Columns match property names exactly. }
  ExecSQL('CREATE TABLE Product ' +
    '(ID INTEGER PRIMARY KEY, Name TEXT, Price REAL, InStock INTEGER)');

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
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
  Def: TPersisterDef;
  FM: TOPFieldMap;
  I: Integer;
  Prod, Loaded: TProduct;
  AllProducts: TObjectList;
begin
  SetupDatabase;
  try
    { Inspect the convention-derived field map
      After registration, the persister definition contains the auto-generated
      field map. Let's inspect what the ORM discovered from TProduct. }
    WriteLn('=== Convention-Based Field Map ===');
    WriteLn;
    Def := OPRegistry.FindPersister(TProduct);
    WriteLn('Table name (convention): "', Def.TableName, '"');
    WriteLn('  Rule: class name "TProduct" minus leading "T" = "Product"');
    WriteLn;
    WriteLn('Field mappings (', Def.FieldMap.Count, ' fields):');
    for I := 0 to Def.FieldMap.Count - 1 do
    begin
      FM := Def.FieldMap[I];
      Write('  Property "', FM.PropertyName, '" -> Column "', FM.FieldName, '"');
      if foInKey in FM.Options then
        Write('  [PRIMARY KEY]');
      if foInSelect in FM.Options then
        Write('  [SELECT]');
      if foInInsert in FM.Options then
        Write('  [INSERT]');
      if foInUpdate in FM.Options then
        Write('  [UPDATE]');
      WriteLn;
    end;
    WriteLn;
    WriteLn('  Rule: property name = column name (no transformation)');
    WriteLn('  Rule: "ID" property automatically gets foInKey (primary key)');
    WriteLn;

    { Verify CRUD works with zero configuration 
      No table name override, no field mapping - pure convention. }
    WriteLn('=== CRUD with Convention Mapping ===');
    WriteLn;

    { Insert }
    Prod := TProduct.Create;
    try
      Prod.ID := 1;
      Prod.Name := 'Widget';
      Prod.Price := 9.99;
      Prod.InStock := True;
      Prod.FState := opsNew;
      OPManager.Save(Prod);
      WriteLn('Saved: ID=', Prod.ID, ' Name="', Prod.Name,
              '" Price=', CurrToStr(Prod.Price), ' InStock=', Prod.InStock);
    finally
      Prod.Free;
    end;

    Prod := TProduct.Create;
    try
      Prod.ID := 2;
      Prod.Name := 'Gadget';
      Prod.Price := 24.50;
      Prod.InStock := False;
      Prod.FState := opsNew;
      OPManager.Save(Prod);
      WriteLn('Saved: ID=', Prod.ID, ' Name="', Prod.Name,
              '" Price=', CurrToStr(Prod.Price), ' InStock=', Prod.InStock);
    finally
      Prod.Free;
    end;
    WriteLn;

    { Load by PK - convention maps ID to the WHERE clause }
    Loaded := TProduct.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Loaded by PK: ID=', Loaded.ID, ' Name="', Loaded.Name,
              '" Price=', CurrToStr(Loaded.Price), ' InStock=', Loaded.InStock);
    finally
      Loaded.Free;
    end;
    WriteLn;

    { Load list - all rows }
    AllProducts := TObjectList.Create(True);
    try
      OPManager.Load(AllProducts, TProduct);
      WriteLn('All products (', AllProducts.Count, '):');
      for I := 0 to AllProducts.Count - 1 do
      begin
        Prod := TProduct(AllProducts[I]);
        WriteLn('  [', I, '] ID=', Prod.ID, ' Name="', Prod.Name,
                '" Price=', CurrToStr(Prod.Price), ' InStock=', Prod.InStock);
      end;
    finally
      AllProducts.Free;
    end;
    WriteLn;

    { Delete - convention maps PK for the WHERE clause }
    Prod := TProduct.Create;
    try
      Prod.ID := 2;
      OPManager.Load(Prod);
      OPManager.Delete(Prod);
      WriteLn('Deleted product ID=2');
    finally
      Prod.Free;
    end;

    AllProducts := TObjectList.Create(True);
    try
      OPManager.Load(AllProducts, TProduct);
      WriteLn('Products remaining: ', AllProducts.Count);
    finally
      AllProducts.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
