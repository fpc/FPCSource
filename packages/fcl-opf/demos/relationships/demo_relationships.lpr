{ demo_relationships - Parent-child relationships and cascade delete
  ====================================================================
  This demo shows how to:
  - Register a parent-child relationship between two classes
  - Load child objects for a parent (LoadChildren)
  - Cascade delete: deleting a parent automatically deletes its children

  Compile:  fpc demo_relationships.lpr -Fu../../src
  Run:      ./demo_relationships
}
program demo_relationships;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Relations,       { OPRelationRegistry - parent-child relationship registration }
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_relationships.db';

type
  { TCustomer:
    Parent class.
    Has an ID and a Name. The AddressList property holds child objects
    but is NOT a database column - it's populated by LoadChildren. }
  TCustomer = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FAddressList: TObjectList;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    destructor Destroy; override;
    property AddressList: TObjectList read FAddressList;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TOrder:
    Child class.
    Has a CustomerID foreign key pointing to TCustomer.ID.
    The ORM uses this FK to load children and cascade deletes. }
  TOrder = class(TObject, IOPPersistenceState)
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

{ IOPPersistenceState implementations }

function TCustomer.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCustomer.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TCustomer.Create;
begin
  inherited Create;
  FAddressList := TObjectList.Create(True);
end;

destructor TCustomer.Destroy;
begin
  FAddressList.Free;
  inherited Destroy;
end;

function TOrder.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TOrder.SetPersistenceState(aValue: TObjectPersistenceState);
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

function CountRows(const aTable: String): Integer;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := SetupTrans;
    Q.SQL.Text := 'SELECT COUNT(*) FROM ' + aTable;
    if not SetupTrans.Active then
      SetupTrans.StartTransaction;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
    SetupTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure SetupDatabase;
var
  OpConn: TOPConnection;
  Def: TPersisterDef;
  Idx: Integer;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  { Create tables }
  ExecSQL('CREATE TABLE Customer (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQL('CREATE TABLE CustOrder (ID INTEGER PRIMARY KEY, CustomerID INTEGER, Product TEXT, Amount REAL)');

  { Register both classes with the RTTI persister }
  Def := OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  { The AddressList property is a TObjectList - the RTTI persister maps it
    as a field, but it's not a database column. Remove it from the field map. }
  Idx := Def.FieldMap.IndexOfPropertyName('AddressList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);

  { Override table name for TOrder because "Order" is a SQL reserved word.
    The convention would map TOrder → "Order", which causes syntax errors.
    We override to "CustOrder" instead. }
  OPRegistry.RegisterPersister(TOrder, TRTTIPersister).TableName := 'CustOrder';

  { Register the parent-child relationship.
    Parameters:
      1. Parent class: TCustomer
      2. Child class: TOrder
      3. Foreign key field name: 'CustomerID' (column in TOrder's table that references TCustomer.ID)
      4. DB cascade: False - the ORM handles cascade deletes in application code,
         not via database-level ON DELETE CASCADE. When True, the ORM skips
         application-level child deletion, trusting the database to handle it. }
  OPRelationRegistry.RegisterRelationship(
    TCustomer, TOrder, 'CustomerID', False);

  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRelationRegistry.ClearInstance;
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
  Cust: TCustomer;
  Ord1, Ord2: TOrder;
  I: Integer;
begin
  SetupDatabase;
  try
    { === Insert a customer and two orders === }
    WriteLn('--- INSERT ---');
    Cust := TCustomer.Create;
    try
      Cust.ID := 1;
      Cust.Name := 'Alice';
      Cust.FState := opsNew;
      OPManager.Save(Cust);
      WriteLn('Saved customer: ID=', Cust.ID, ' Name="', Cust.Name, '"');
    finally
      Cust.Free;
    end;

    Ord1 := TOrder.Create;
    try
      Ord1.ID := 10;
      Ord1.CustomerID := 1;  { FK pointing to Alice }
      Ord1.Product := 'Widget';
      Ord1.Amount := 19.99;
      Ord1.FState := opsNew;
      OPManager.Save(Ord1);
      WriteLn('Saved order: ID=', Ord1.ID, ' CustomerID=', Ord1.CustomerID,
              ' Product="', Ord1.Product, '"');
    finally
      Ord1.Free;
    end;

    Ord2 := TOrder.Create;
    try
      Ord2.ID := 11;
      Ord2.CustomerID := 1;  { FK pointing to Alice }
      Ord2.Product := 'Gadget';
      Ord2.Amount := 49.50;
      Ord2.FState := opsNew;
      OPManager.Save(Ord2);
      WriteLn('Saved order: ID=', Ord2.ID, ' CustomerID=', Ord2.CustomerID,
              ' Product="', Ord2.Product, '"');
    finally
      Ord2.Free;
    end;
    WriteLn;

    { Load children: retrieve orders for a customer:
      LoadChildren populates the customer's AddressList with TOrder objects
      by querying: SELECT * FROM "Order" WHERE CustomerID = :CustomerID }
    WriteLn('--- LOAD CHILDREN ---');
    Cust := TCustomer.Create;
    try
      Cust.ID := 1;
      Cust.FState := opsKey;
      OPManager.Load(Cust);
      WriteLn('Customer: ID=', Cust.ID, ' Name="', Cust.Name, '"');

      { LoadChildren with explicit child class and list:
        The ORM queries: SELECT * FROM CustOrder WHERE CustomerID = 1
        and creates TOrder objects, adding them to the provided list. }
      OPManager.LoadChildren(Cust, TOrder, Cust.AddressList);
      WriteLn('Orders loaded: ', Cust.AddressList.Count);
      for I := 0 to Cust.AddressList.Count - 1 do
      begin
        Ord1 := TOrder(Cust.AddressList[I]);
        WriteLn('  [', I, '] ID=', Ord1.ID, ' Product="', Ord1.Product,
                '" Amount=', CurrToStr(Ord1.Amount));
      end;
    finally
      Cust.Free;
    end;
    WriteLn;

    { Cascade delete:
      When you delete a parent, the ORM automatically deletes all children
      that are registered in a relationship (with DBCascade=False, the ORM
      handles the cascade in application code). }
    WriteLn('--- CASCADE DELETE ---');
    WriteLn('Before delete: Customers=', CountRows('Customer'), ' Orders=', CountRows('CustOrder'));

    Cust := TCustomer.Create;
    try
      Cust.ID := 1;
      Cust.FState := opsKey;
      OPManager.Load(Cust);
      WriteLn('Deleting customer: ID=', Cust.ID, ' Name="', Cust.Name, '"');
      { Delete cascades: ORM first deletes all TOrder records where CustomerID=1,
        then deletes the TCustomer record. }
      OPManager.Delete(Cust);
      WriteLn('Customer and orders deleted.');
    finally
      Cust.Free;
    end;

    WriteLn('After delete:  Customers=', CountRows('Customer'), ' Orders=', CountRows('CustOrder'));
    WriteLn;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
