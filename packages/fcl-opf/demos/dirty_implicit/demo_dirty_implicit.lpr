{ demo_dirty_implicit - Dirty tracking with global snapshot manager (implicit mode)
  ===================================================================================
  Demonstrates dirty tracking where ALL classes are automatically tracked.
  The snapshot manager is created with default smImplicit mode - no per-class
  registration needed. Only changed columns appear in UPDATE statements.

  Compile:  lazbuild demo_dirty_implicit.lpi
  Run:      ./demo_dirty_implicit
}
program demo_dirty_implicit;

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
  DB_FILE = 'demo_dirty_implicit.db';

type
  TProduct = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FPrice: Double;
    FStock: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Stock: Integer read FStock write FStock;
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
  Def: TPersisterDef;
begin
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);

  Conn := TSQLite3Connection.Create(nil);
  Conn.DatabaseName := DB_FILE;
  SetupTrans := TSQLTransaction.Create(Conn);
  Conn.Transaction := SetupTrans;
  Conn.Open;

  Def := OPRegistry.RegisterPersister(TProduct, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;
  ExecSQL(OPDDLBuilder.GetCreateSQL(Def.TableName, Def.FieldMap.AsArray));

  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;

  { Enable SQL logging so we can see partial UPDATE statements }
  OPManager.Logger := TOPLogger.Instance;
  TOPLogger(TOPLogger.Instance).LogKinds := [lkSQL];
  TOPLogger(TOPLogger.Instance).LogToConsole := True;

  { Create the snapshot manager.
    Default mode is smImplicit - ALL classes are tracked automatically.
    It auto-attaches to the default OPManager. }
  SnapshotMgr := TOPSnapshotManager.Create(nil);

  WriteLn('Database setup complete. Dirty tracking: IMPLICIT mode (all classes tracked).');
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
  Product: TProduct;
begin
  SetupDatabase;
  try
    { INSERT a new product }
    WriteLn('--- INSERT (new product) ---');
    Product := TProduct.Create;
    try
      Product.ID := 1;
      Product.Name := 'Widget';
      Product.Price := 9.99;
      Product.Stock := 100;
      Product.FState := opsNew;
      OPManager.Save(Product);
      WriteLn('Saved: ID=', Product.ID, ' Name="', Product.Name,
        '" Price=', Product.Price:0:2, ' Stock=', Product.Stock);
    finally
      Product.Free;
    end;
    WriteLn;

    { Load, change ONE field, save - should produce partial UPDATE }
    WriteLn('--- PARTIAL UPDATE (change Price only) ---');
    Product := TProduct.Create;
    try
      Product.ID := 1;
      OPManager.Load(Product);
      WriteLn('Loaded:  Price=', Product.Price:0:2, ' Stock=', Product.Stock);
      Product.Price := 12.99;
      Product.FState := opsUpdate;
      WriteLn('Changed: Price=', Product.Price:0:2, ' (Stock unchanged)');
      WriteLn('SQL generated (observe only Price in SET clause):');
      OPManager.Save(Product);
    finally
      Product.Free;
    end;
    WriteLn;

    { Load, change NOTHING, save - should skip UPDATE entirely }
    WriteLn('--- NO-CHANGE SAVE (should skip UPDATE) ---');
    Product := TProduct.Create;
    try
      Product.ID := 1;
      OPManager.Load(Product);
      Product.FState := opsUpdate;
      WriteLn('Saving without changes...');
      OPManager.Save(Product);
      WriteLn('No SQL generated - UPDATE skipped.');
    finally
      Product.Free;
    end;
    WriteLn;

    { Load, change multiple fields - should include all changed columns }
    WriteLn('--- MULTI-FIELD UPDATE (change Name and Stock) ---');
    Product := TProduct.Create;
    try
      Product.ID := 1;
      OPManager.Load(Product);
      Product.Name := 'Super Widget';
      Product.Stock := 50;
      Product.FState := opsUpdate;
      WriteLn('Changed: Name="', Product.Name, '" Stock=', Product.Stock);
      WriteLn('SQL generated (observe Name and Stock in SET clause):');
      OPManager.Save(Product);
    finally
      Product.Free;
    end;

    WriteLn;
    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
