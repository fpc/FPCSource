{ demo_dirty_explicit - Dirty tracking with global snapshot manager (explicit mode)
  ====================================================================================
  Demonstrates dirty tracking where ONLY explicitly registered classes are tracked.
  Use smExplicit mode when you want fine-grained control over which classes
  get partial UPDATEs. Unregistered classes still get full UPDATEs.

  Compile:  lazbuild demo_dirty_explicit.lpi
  Run:      ./demo_dirty_explicit
}
program demo_dirty_explicit;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager,
  Opf.Registry,
  Opf.Fields,
  Opf.Sql,
  Opf.Driver,
  Opf.DDL,
  Opf.Persister.Rtti,
  Opf.Snapshot,
  Opf.SnapshotHooks,
  Opf.Logging;

const
  DB_FILE = 'demo_dirty_explicit.db';

type
  { TCustomer - will be REGISTERED for dirty tracking }
  TCustomer = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCity: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property City: String read FCity write FCity;
  end;

  { TInvoice - will NOT be registered, gets full UPDATEs }
  TInvoice = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FCustomerID: Integer;
    FTotal: Double;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property CustomerID: Integer read FCustomerID write FCustomerID;
    property Total: Double read FTotal write FTotal;
  end;

function TCustomer.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TCustomer.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

function TInvoice.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TInvoice.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  SetupTrans: TSQLTransaction;
  SnapshotMgr: TOPSnapshotManager;

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
  CustDef, InvDef: TPersisterDef;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  CustDef := OPRegistry.RegisterPersister(TCustomer, TRTTIPersister);
  InvDef := OPRegistry.RegisterPersister(TInvoice, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;
  ExecSQL(OPDDLBuilder.GetCreateSQL(CustDef.TableName, CustDef.FieldMap.AsArray));
  ExecSQL(OPDDLBuilder.GetCreateSQL(InvDef.TableName, InvDef.FieldMap.AsArray));

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  OPManager.Logger := TOPLogger.Instance;
  TOPLogger(TOPLogger.Instance).LogKinds := [lkSQL];
  TOPLogger(TOPLogger.Instance).LogToConsole := True;

  { Create snapshot manager in EXPLICIT mode.
    Only classes added via RegisterClass get dirty tracking. }
  SnapshotMgr := TOPSnapshotManager.Create(nil);
  SnapshotMgr.Mode := smExplicit;
  SnapshotMgr.RegisterClass(TCustomer);
  { TInvoice is NOT registered - it will always get full UPDATEs }

  WriteLn('Database setup complete.');
  WriteLn('  TCustomer: dirty tracking ENABLED (registered)');
  WriteLn('  TInvoice:  dirty tracking DISABLED (not registered)');
  WriteLn;
end;

procedure TeardownDatabase;
begin
  SnapshotMgr.Free;
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  SetupTrans.Free;
  if Conn.Connected then
    Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

var
  Customer: TCustomer;
  Order: TInvoice;
begin
  SetupDatabase;
  try
    { INSERT both }
    WriteLn('--- INSERT customer and order ---');
    Customer := TCustomer.Create;
    try
      Customer.ID := 1;
      Customer.Name := 'Alice';
      Customer.City := 'New York';
      Customer.FState := opsNew;
      OPManager.Save(Customer);
      WriteLn('Customer saved: ID=', Customer.ID, ' Name="', Customer.Name, '"');
    finally
      Customer.Free;
    end;
    Order := TInvoice.Create;
    try
      Order.ID := 1;
      Order.CustomerID := 1;
      Order.Total := 99.50;
      Order.FState := opsNew;
      OPManager.Save(Order);
      WriteLn('Order saved: ID=', Order.ID, ' Total=', Order.Total:0:2);
    finally
      Order.Free;
    end;
    WriteLn;

    { UPDATE customer - partial UPDATE (only City in SET) }
    WriteLn('--- UPDATE customer (change City only) ---');
    WriteLn('Expected: partial UPDATE with only City in SET clause');
    Customer := TCustomer.Create;
    try
      Customer.ID := 1;
      OPManager.Load(Customer);
      Customer.City := 'Boston';
      Customer.FState := opsUpdate;
      OPManager.Save(Customer);
    finally
      Customer.Free;
    end;
    WriteLn;

    { UPDATE order - full UPDATE (TInvoice not registered for tracking) }
    WriteLn('--- UPDATE order (change Total only) ---');
    WriteLn('Expected: full UPDATE with ALL columns in SET clause');
    Order := TInvoice.Create;
    try
      Order.ID := 1;
      OPManager.Load(Order);
      Order.Total := 149.75;
      Order.FState := opsUpdate;
      OPManager.Save(Order);
    finally
      Order.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
    WriteLn('Notice: TCustomer UPDATE had only City; TInvoice UPDATE had all columns.');
  finally
    TeardownDatabase;
  end;
end.
