{ demo_save_list - Save an entire list in one call
  ================================================
  This demo shows how OPManager.Save(aList) detects a registered list
  and saves all items based on their persistence state:
  - opsNew    -> INSERT
  - opsUpdate -> UPDATE
  - opsClean  -> skipped (no changes)
  - opsDelete -> DELETE

  The entire operation is wrapped in a single transaction.

  Compile:  fpc demo_save_list.lpr -Fu../../src
  Run:      ./demo_save_list
}
program demo_save_list;

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
  DB_FILE = 'demo_save_list.db';

type
  { TProduct }
  TProduct = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FPrice: Currency;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Price: Currency read FPrice write FPrice;
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

function CountRows: Integer;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := Conn;
    Q.Transaction := Trans;
    Q.SQL.Text := 'SELECT COUNT(*) FROM Product';
    if not Trans.Active then
      Trans.StartTransaction;
    Q.Open;
    Result := Q.Fields[0].AsInteger;
    Q.Close;
    Trans.Commit;
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
  Trans := TSQLTransaction.Create(Conn);
  Conn.Transaction := Trans;
  Conn.Open;

  ExecSQL('CREATE TABLE Product (ID INTEGER PRIMARY KEY, Name TEXT, Price REAL)');

  OPRegistry.RegisterPersister(TProduct, TRTTIPersister);
  OPSQLBuilder.SQLDialect := dSQLite;
  TOPManager.ClearInstance;
  OpConn := OPManager.Connections.Add as TOPConnection;
  OpConn.SingleConnection := Conn;
  OpConn.IsDefault := True;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  Trans.Free;
  Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

procedure PrintProducts;
var
  List: TObjectList;
  P: TProduct;
  I: Integer;
begin
  List := TObjectList.Create(True);
  try
    OPManager.Load(List, TProduct);
    WriteLn('  Products in database: ', List.Count);
    for I := 0 to List.Count - 1 do
    begin
      P := TProduct(List[I]);
      WriteLn('    [', P.ID, '] ', P.Name, ' - ', CurrToStr(P.Price));
    end;
  finally
    List.Free;
  end;
end;

var
  Products: TObjectList;
  P: TProduct;
begin
  SetupDatabase;
  try
    { Step 1: Insert multiple items via Save(list) }
    WriteLn('--- BATCH INSERT via Save(list) ---');
    Products := TObjectList.Create(True);
    try
      P := TProduct.Create;
      P.ID := 1;
      P.Name := 'Widget';
      P.Price := 9.99;
      P.FState := opsNew;
      Products.Add(P);

      P := TProduct.Create;
      P.ID := 2;
      P.Name := 'Gadget';
      P.Price := 24.50;
      P.FState := opsNew;
      Products.Add(P);

      P := TProduct.Create;
      P.ID := 3;
      P.Name := 'Doohickey';
      P.Price := 4.75;
      P.FState := opsNew;
      Products.Add(P);

      OPManager.Save(Products);
      WriteLn('Saved 3 new products in one call.');
    finally
      Products.Free;
    end;
    PrintProducts;
    WriteLn;

    { Step 2: Mixed operations via Save(list) }
    WriteLn('--- MIXED SAVE (insert + update + delete) ---');
    Products := TObjectList.Create(True);
    try
      { Update Widget price }
      P := TProduct.Create;
      P.ID := 1;
      P.FState := opsKey;
      OPManager.Load(P);
      P.Name := 'Super Widget';
      P.Price := 14.99;
      P.FState := opsUpdate;
      Products.Add(P);

      { Delete Doohickey }
      P := TProduct.Create;
      P.ID := 3;
      P.FState := opsDelete;
      Products.Add(P);

      { Insert new item }
      P := TProduct.Create;
      P.ID := 4;
      P.Name := 'Thingamajig';
      P.Price := 39.95;
      P.FState := opsNew;
      Products.Add(P);

      { Clean item - should be skipped }
      P := TProduct.Create;
      P.ID := 2;
      P.FState := opsKey;
      OPManager.Load(P);
      { State is opsClean - no changes, Save will skip this }
      Products.Add(P);

      WriteLn('Saving list with 4 items:');
      WriteLn('  ID=1 opsUpdate (rename + price change)');
      WriteLn('  ID=3 opsDelete');
      WriteLn('  ID=4 opsNew');
      WriteLn('  ID=2 opsClean (skipped)');
      OPManager.Save(Products);
      WriteLn('Done.');
    finally
      Products.Free;
    end;
    PrintProducts;
    WriteLn;

    WriteLn('Demo completed successfully.');
  finally
    TeardownDatabase;
  end;
end.
