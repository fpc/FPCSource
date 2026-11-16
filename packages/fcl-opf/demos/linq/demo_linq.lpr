{ demo_linq - LINQ-style query expressions
  =============================================
  This demo shows the operator-based query syntax that provides
  a natural Pascal alternative to the fluent TOPCriteria builder.

  Instead of:
    C := TOPCriteria.Create;
    C.Where('Name', opEqual, 'Alice').AddAnd('Age', opGreater, 18);
    OPManager.Load(List, TPerson, C);
    C.Free;

  You write:
    OPManager.Load(List, TPerson,
      (Prop('Name') = 'Alice') and (Prop('Age') > 18));

  No object to create or free - COM interface refcounting manages
  everything automatically. Property names are resolved to database
  column names at query time.

  Compile:  lazbuild demo_linq.lpi
  Run:      ./demo_linq
}
program demo_linq;

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
  Opf.Persister.Rtti,
  Opf.Linq;

const
  DB_FILE = 'demo_linq.db';

type

  TProduct = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCategory: String;
    FPrice: Double;
    FInStock: Boolean;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Category: String read FCategory write FCategory;
    property Price: Double read FPrice write FPrice;
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

procedure InsertProduct(aID: Integer; const aName, aCat: String; aPrice: Double; aInStock: Boolean);
var
  P: TProduct;
begin
  P := TProduct.Create;
  try
    P.ID := aID;
    P.Name := aName;
    P.Category := aCat;
    P.Price := aPrice;
    P.InStock := aInStock;
    P.FState := opsNew;
    OPManager.Save(P);
  finally
    P.Free;
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
  OPSQLBuilder.SQLDialect := dSQLite;
  OPRegistry.RegisterPersister(TProduct, TRTTIPersister);
  ExecSQL('CREATE TABLE Product (ID INTEGER PRIMARY KEY, Name TEXT, ' +
          'Category TEXT, Price REAL, InStock BOOLEAN)');
  TOPManager.ClearInstance;
  (OPManager.Connections.Add as TOPConnection).SingleConnection := Conn;
  (OPManager.Connections[0] as TOPConnection).IsDefault := True;
end;

procedure SeedData;
begin
  InsertProduct(1,  'Pascal Handbook',     'Books',       45.99, True);
  InsertProduct(2,  'Delphi Cookbook',      'Books',       39.99, True);
  InsertProduct(3,  'USB Keyboard',        'Electronics', 29.99, True);
  InsertProduct(4,  'Mechanical Keyboard', 'Electronics', 89.99, False);
  InsertProduct(5,  'Mouse Pad',           'Electronics', 12.99, True);
  InsertProduct(6,  'FPC T-Shirt',         'Clothing',    24.99, True);
  InsertProduct(7,  'Lazarus Hoodie',      'Clothing',    59.99, False);
  InsertProduct(8,  'Code Coffee Mug',     'Kitchen',     15.99, True);
  InsertProduct(9,  'Algorithm Poster',    'Books',       19.99, True);
  InsertProduct(10, 'Monitor Stand',       'Electronics', 79.99, True);
  WriteLn('Seeded 10 products.');
  WriteLn;
end;

procedure PrintList(aList: TObjectList; const aTitle: String);
var
  I: Integer;
  P: TProduct;
begin
  WriteLn(aTitle, ' (', aList.Count, ' results):');
  for I := 0 to aList.Count - 1 do
  begin
    P := TProduct(aList[I]);
    WriteLn(Format('  [%d] %-22s %-12s $%6.2f  %s',
      [P.ID, P.Name, P.Category, P.Price,
       BoolToStr(P.InStock, 'In Stock', 'Out of Stock')]));
  end;
  WriteLn;
end;

procedure TeardownDatabase;
begin
  TOPManager.ClearInstance;
  TOPRegistry.ClearInstance;
  SetupTrans.Free;
  Conn.Close;
  Conn.Free;
  if FileExists(DB_FILE) then
    DeleteFile(DB_FILE);
end;

var
  List: TObjectList;
begin
  SetupDatabase;
  try
    SeedData;

    { 1. Simple equality }
    WriteLn('=== 1. Simple equality ===');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        Prop('Category') = 'Books');
      PrintList(List, 'Category = Books');
    finally
      List.Free;
    end;

    { 2. AND with comparison }
    WriteLn('=== 2. AND with comparison ===');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        (Prop('InStock') = True) and (Prop('Price') < Double(50)));
      PrintList(List, 'InStock AND Price < 50');
    finally
      List.Free;
    end;

    { 3. OR with parentheses }
    WriteLn('=== 3. OR with parentheses ===');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        (Prop('Category') = 'Books') or (Prop('Category') = 'Kitchen'));
      PrintList(List, 'Books OR Kitchen');
    finally
      List.Free;
    end;

    { 4. Nested: AND + OR }
    WriteLn('=== 4. Nested AND + OR ===');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        (Prop('InStock') = True)
        and ((Prop('Category') = 'Electronics') or (Prop('Category') = 'Books')));
      PrintList(List, 'InStock AND (Electronics OR Books)');
    finally
      List.Free;
    end;

    { 5. Contains (LIKE %...%) }
    WriteLn('=== 5. Contains ===');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        Prop('Name').Contains('oo'));
      PrintList(List, 'Name contains "oo"');
    finally
      List.Free;
    end;

    { 6. NOT }
    WriteLn('=== 6. NOT ===');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        not (Prop('InStock') = True));
      PrintList(List, 'NOT InStock');
    finally
      List.Free;
    end;

    { 7. Complex realistic query }
    WriteLn('=== 7. Complex realistic query ===');
    WriteLn('InStock AND (Electronics OR Clothing) AND Price >= 25');
    List := TObjectList.Create(True);
    try
      OPManager.Load(List, TProduct,
        (Prop('InStock') = True)
        and ((Prop('Category') = 'Electronics') or (Prop('Category') = 'Clothing'))
        and (Prop('Price') >= Double(25)));
      PrintList(List, 'Complex query');
    finally
      List.Free;
    end;

    WriteLn('Demo completed successfully.');
    WriteLn;
    WriteLn('Note: No TOPCriteria objects were created or freed manually.');
    WriteLn('COM interface refcounting handles all memory automatically.');
  finally
    TeardownDatabase;
  end;
end.
