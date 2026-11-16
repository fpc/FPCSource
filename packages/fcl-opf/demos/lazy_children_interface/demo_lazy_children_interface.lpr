{ demo_lazy_children_interface - Lazy loading of 1:N children via IOPLazyInit
  ===========================================================================
  This demo shows how to use TOPLazy<T> with the IOPLazyInit interface
  (FPC 3.2.2 compatible) to defer loading of child objects until the
  property is first accessed.

  Key points:
  - The parent class implements IOPLazyInit
  - InitLazyLoaders is called automatically by the Manager after Load
  - The property getter returns FLazy.Value, which triggers loading on first access

  Compile:  fpc demo_lazy_children_interface.lpr -Fu../../src
  Run:      ./demo_lazy_children_interface
}
program demo_lazy_children_interface;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.Relations,
  Opf.Proxy,
  Opf.Persister.Rtti;

const
  DB_FILE = 'demo_lazy_children_interface.db';

type
  { Forward declare the child class so the parent can reference it }
  TInvoice = class;

  { Specialize the lazy record for TObjectList }
  TOPLazyObjectList = specialize TOPLazy<TObjectList>;

  { TCustomer - parent with lazy child loading via IOPLazyInit.
    The Invoices property getter triggers loading on first access. }
  TCustomer = class(TObject, IOPPersistenceState, IOPLazyInit)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    FInvoiceList: TObjectList;
    FLazyInvoices: TOPLazyObjectList;
    FManager: TObject;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetInvoiceList: TObjectList;
    procedure DoLoadInvoices(aTarget: TObject);
    { IOPLazyInit - called by Manager after Load }
    procedure InitLazyLoaders(aManager: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property InvoiceList: TObjectList read GetInvoiceList;
  end;

  { TInvoice - child entity with FK to TCustomer }
  TInvoice = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FCustomerID: Integer;
    FAmount: Currency;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property CustomerID: Integer read FCustomerID write FCustomerID;
    property Amount: Currency read FAmount write FAmount;
  end;

{ TCustomer implementation }

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
  FInvoiceList := TObjectList.Create(True);
  FLazyInvoices.Init(FInvoiceList);
end;

destructor TCustomer.Destroy;
begin
  FInvoiceList.Free;
  inherited Destroy;
end;

function TCustomer.GetInvoiceList: TObjectList;
begin
  { This triggers the lazy loader on first access }
  Result := FLazyInvoices.Value;
end;

procedure TCustomer.DoLoadInvoices(aTarget: TObject);
begin
  { The actual load callback - uses the stored manager reference }
  TOPManager(FManager).LoadChildren(Self, TInvoice, aTarget);
end;

procedure TCustomer.InitLazyLoaders(aManager: TObject);
begin
  { Called by the Manager after Load. Store manager ref and wire up the loader. }
  FManager := aManager;
  FLazyInvoices.SetLoader(@DoLoadInvoices);
end;

{ TInvoice implementation }

function TInvoice.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TInvoice.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ Database helpers }

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;

procedure ExecSQL(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := Trans;
    Q.SQL.Text := aSQL;
    if not Trans.Active then
      Trans.StartTransaction;
    Q.ExecSQL;
    Trans.Commit;
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
  Trans := TSQLTransaction.Create(Conn);
  Conn.Transaction := Trans;
  Conn.Open;

  ExecSQL('CREATE TABLE Customer (ID INTEGER PRIMARY KEY, Name TEXT)');
  ExecSQL('CREATE TABLE Invoice (ID INTEGER PRIMARY KEY, CustomerID INTEGER, Amount REAL)');

  Def := OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  Idx := Def.FieldMap.IndexOfPropertyName('InvoiceList');
  if Idx >= 0 then
    Def.FieldMap.Delete(Idx);

  OPRegistry.RegisterPersister(TInvoice, TRTTIPersister);
  OPRelationRegistry.RegisterRelationship(TCustomer, TInvoice, 'CustomerID', False, 'InvoiceList');

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
  Trans.Free;
  Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

{ Main }

var
  Cust: TCustomer;
  Inv: TInvoice;
  I: Integer;
begin
  SetupDatabase;
  try
    { Insert test data }
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

    Inv := TInvoice.Create;
    try
      Inv.ID := 10;
      Inv.CustomerID := 1;
      Inv.Amount := 99.95;
      Inv.FState := opsNew;
      OPManager.Save(Inv);
    finally
      Inv.Free;
    end;

    Inv := TInvoice.Create;
    try
      Inv.ID := 11;
      Inv.CustomerID := 1;
      Inv.Amount := 250.00;
      Inv.FState := opsNew;
      OPManager.Save(Inv);
    finally
      Inv.Free;
    end;
    WriteLn('Saved 2 invoices for customer 1');
    WriteLn;

    { Load customer - invoices are NOT loaded yet }
    WriteLn('--- LAZY LOAD ---');
    Cust := TCustomer.Create;
    try
      Cust.ID := 1;
      Cust.FState := opsKey;
      OPManager.Load(Cust);
      WriteLn('Loaded customer: ID=', Cust.ID, ' Name="', Cust.Name, '"');
      WriteLn('Invoices loaded yet? ', Cust.FLazyInvoices.IsLoaded);

      { Accessing InvoiceList triggers lazy loading }
      WriteLn('Accessing InvoiceList...');
      WriteLn('Invoice count: ', Cust.InvoiceList.Count);
      WriteLn('Invoices loaded now? ', Cust.FLazyInvoices.IsLoaded);
      for I := 0 to Cust.InvoiceList.Count - 1 do
      begin
        Inv := TInvoice(Cust.InvoiceList[I]);
        WriteLn('  [', I, '] ID=', Inv.ID, ' Amount=', CurrToStr(Inv.Amount));
      end;
    finally
      Cust.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
