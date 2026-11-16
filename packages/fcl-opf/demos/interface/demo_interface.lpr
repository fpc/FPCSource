{ demo_interface - Interface-driven persister (no RTTI)
  ======================================================
  This demo shows how to use the IOPFieldProvider interface to define
  field mappings manually instead of relying on RTTI (published properties).

  Why use the interface path instead of RTTI?
  - Performance: no RTTI lookups at runtime
  - Control: properties do NOT need to be 'published'
  - Flexibility: the class does not need to descend from TPersistent

  The IOPFieldProvider contract has 3 methods:
    GetFieldDefs  - define field mappings (name, type, options)
    ApplyField    - read a DB field value into an object property
    PersistField  - write an object property to a DB parameter

  Compile:  fpc demo_interface.lpr -Fu../../src
  Run:      ./demo_interface
}
program demo_interface;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs, typinfo,
  { ORM units }
  Opf.Manager,         { TOPManager -- central facade for all persistence operations }
  Opf.Registry,        { TOPRegistry -- class-to-persister registration }
  Opf.Fields,          { TOPFieldMap -- field mapping between objects and database columns }
  Opf.Sql,             { TOPSQLBuilder -- SQL generation with dialect support }
  Opf.Driver,
  Opf.Persister.Intf;  { TInterfacePersister -- interface-driven field mapping }

const
  DB_FILE = 'demo_interface.db';

type
  { TProduct:
    A model class using IOPFieldProvider + IOPPersistenceState.
    Note: properties are PUBLIC, not published. This demonstrates that RTTI
    is completely bypassed -- the interface provides all field mapping info. }

  TProduct = class(TObject, IOPFieldProvider, IOPPersistenceState)
  private
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    { Public fields: NOT published. RTTI is not used. }
    FID: Integer;
    FName: UTF8String;
    FPrice: Double;

    { IOPFieldProvider: define field mappings, read from DB, write to DB }
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;

    property ID: Integer read FID write FID;
    property Name: UTF8String read FName write FName;
    property Price: Double read FPrice write FPrice;
  end;

{ IOPPersistenceState implementation:
  the ORM uses this to decide  whether Save should INSERT (opsNew) or UPDATE (opsUpdate).
  Every model class MUST implement this interface for Save/Load/Delete. }
function TProduct.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TProduct.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ GetFieldDefs: 
  called once during RegisterPersister to define the field mappings. 
  Each field needs:
    - A field name (DB column) and property name (object side)
    - PropertyTypeInfo: tells the ORM the data type (for SQL generation)
    - Options: foInKey marks the primary key, foAutoInc means the DB
      generates the value, removing foInInsert excludes it from INSERT }
procedure TProduct.GetFieldDefs(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  { ID field: auto-increment primary key.
    foAutoInc + foInKey = DB generates the ID on INSERT.
    We remove foInInsert so the INSERT SQL omits the ID column. }
  F := aMap.AddField('ID', 'ID');
  F.Options := F.Options + [foInKey, foAutoInc] - [foInInsert];
  F.PropertyTypeInfo := TypeInfo(Integer);

  { Name field: standard text field, included in SELECT/INSERT/UPDATE }
  F := aMap.AddField('Name', 'Name');
  F.PropertyTypeInfo := TypeInfo(UTF8String);

  { Price field: floating point, included in SELECT/INSERT/UPDATE }
  F := aMap.AddField('Price', 'Price');
  F.PropertyTypeInfo := TypeInfo(Double);
end;

{ ApplyField:
  Called by the ORM when reading a result row from the DB.
  Dispatch on aFieldMap.PropertyName to set the correct object property.
  aField is a TField from the query result set. }
function TProduct.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result := True;
  { Handle NULL values by resetting to defaults }
  if aField.IsNull then
  begin
    if aFieldMap.PropertyName = 'ID' then
      FID := 0
    else if aFieldMap.PropertyName = 'Name' then
      FName := ''
    else if aFieldMap.PropertyName = 'Price' then
      FPrice := 0.0
    else
      Result := False;
    Exit;
  end;
  { Read the field value using the appropriate type accessor }
  if aFieldMap.PropertyName = 'ID' then
    FID := aField.AsInteger
  else if aFieldMap.PropertyName = 'Name' then
    FName := aField.AsString
  else if aFieldMap.PropertyName = 'Price' then
    FPrice := aField.AsFloat
  else
    Result := False;  { Unknown field -- let the ORM handle it }
end;

{ PersistField:
  Called by the ORM when writing object data to a SQL parameter (for INSERT or UPDATE). 
  Dispatch on PropertyName and use the appropriate param type accessor. }
function TProduct.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result := True;
  if aFieldMap.PropertyName = 'ID' then
    aParam.AsInteger := FID
  else if aFieldMap.PropertyName = 'Name' then
    aParam.AsString := FName
  else if aFieldMap.PropertyName = 'Price' then
    aParam.AsFloat := FPrice
  else
    Result := False;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;

{ Helper: execute a SQL statement directly against the database.
  Used here only to create the table; all CRUD goes through the ORM. }
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

  { Register TProduct with TInterfacePersister instead of TRTTIPersister.
    TInterfacePersister calls TProduct.GetFieldDefs to build the field map,
    and TProduct.ApplyField/PersistField for data transfer. }
  OPRegistry.RegisterPersister(TProduct, TInterfacePersister);

  OPSQLBuilder.SQLDialect := dSQLite;

  { Create the table. ID is INTEGER PRIMARY KEY so SQLite assigns
    auto-incrementing rowid values automatically. }
  ExecSQL('CREATE TABLE Product (ID INTEGER PRIMARY KEY, Name TEXT, Price REAL)');

  { Configure the ORM manager with our database connection }
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  WriteLn('Database setup complete. Table "Product" created.');
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

var
  Prod: TProduct;
  Loaded: TProduct;
  AllProducts: TObjectList;
  I: Integer;
begin
  SetupDatabase;
  try
    { INSERT with auto-increment:
      Set state to opsNew and call Save. The ORM generates an INSERT
      that omits the ID column (foAutoInc - foInInsert). After Save,
      the ID is populated from SQLite's last_insert_rowid(). }
    WriteLn('--- SAVE (INSERT with auto-inc) ---');
    Prod := TProduct.Create;
    try
      Prod.Name := 'Widget';
      Prod.Price := 9.99;
      Prod.FState := opsNew;
      OPManager.Save(Prod);
      WriteLn('Saved: ID=', Prod.ID, ' Name="', Prod.Name,
              '" Price=', Prod.Price:0:2);
      WriteLn('  ID was auto-generated by SQLite (foAutoInc)');
    finally
      Prod.Free;
    end;

    { Insert a second product }
    Prod := TProduct.Create;
    try
      Prod.Name := 'Gadget';
      Prod.Price := 24.50;
      Prod.FState := opsNew;
      OPManager.Save(Prod);
      WriteLn('Saved: ID=', Prod.ID, ' Name="', Prod.Name,
              '" Price=', Prod.Price:0:2);
    finally
      Prod.Free;
    end;
    WriteLn;

    {SELECT by PK:
      Set the ID, then call Load. The ORM generates SELECT ... WHERE ID=:ID
      and calls ApplyField for each column in the result. }
    WriteLn('--- LOAD ---');
    Loaded := TProduct.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Loaded: ID=', Loaded.ID, ' Name="', Loaded.Name,
              '" Price=', Loaded.Price:0:2);
    finally
      Loaded.Free;
    end;
    WriteLn;

    { UPDATE:
      Load a product, modify it, set state to opsUpdate, and Save again.
      The ORM generates an UPDATE statement. }
    WriteLn('--- UPDATE ---');
    Prod := TProduct.Create;
    try
      Prod.ID := 1;
      OPManager.Load(Prod);
      WriteLn('Before: Name="', Prod.Name, '" Price=', Prod.Price:0:2);
      Prod.Name := 'Super Widget';
      Prod.Price := 14.99;
      Prod.FState := opsUpdate;
      OPManager.Save(Prod);
      WriteLn('After:  Name="', Prod.Name, '" Price=', Prod.Price:0:2);
    finally
      Prod.Free;
    end;

    { Verify the update }
    Loaded := TProduct.Create;
    try
      Loaded.ID := 1;
      OPManager.Load(Loaded);
      WriteLn('Verified: Name="', Loaded.Name, '" Price=', Loaded.Price:0:2);
    finally
      Loaded.Free;
    end;
    WriteLn;

    { Load all products into a TObjectList. }
    WriteLn('--- LOAD LIST ---');
    AllProducts := TObjectList.Create(True);
    try
      OPManager.Load(AllProducts, TProduct);
      WriteLn('Total products: ', AllProducts.Count);
      for I := 0 to AllProducts.Count - 1 do
      begin
        Prod := TProduct(AllProducts[I]);
        WriteLn('  [', I, '] ID=', Prod.ID, ' Name="', Prod.Name,
                '" Price=', Prod.Price:0:2);
      end;
    finally
      AllProducts.Free;
    end;
    WriteLn;

    { Load a product and call Delete. The ORM generates DELETE WHERE ID=:ID. }
    WriteLn('--- DELETE ---');
    Prod := TProduct.Create;
    try
      Prod.ID := 2;
      OPManager.Load(Prod);
      WriteLn('Deleting: ID=', Prod.ID, ' Name="', Prod.Name, '"');
      OPManager.Delete(Prod);
      WriteLn('Deleted successfully.');
    finally
      Prod.Free;
    end;

    { Verify deletion }
    AllProducts := TObjectList.Create(True);
    try
      OPManager.Load(AllProducts, TProduct);
      WriteLn('Products remaining: ', AllProducts.Count);
      for I := 0 to AllProducts.Count - 1 do
      begin
        Prod := TProduct(AllProducts[I]);
        WriteLn('  [', I, '] ID=', Prod.ID, ' Name="', Prod.Name, '"');
      end;
    finally
      AllProducts.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
