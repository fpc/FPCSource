{ demo_factory_rtti - Automatic constructor discovery via extended RTTI
  ========================================================================
  Demonstrates that when a class is registered with TExtRTTIPersister and
  has a custom parameterless constructor, the factory is automatically
  registered so the custom constructor is called when loading lists.

  Key requirements:
  - FPC trunk (3.3.1+) - extended RTTI required
  - {$RTTI EXPLICIT METHODS([vcPublic])} on the class - enables constructor discovery
  - The class must have a parameterless constructor that differs from TObject.Create

  Without the RTTI factory, aClass.Create via TClass calls TObject.Create
  (which is NOT virtual in FPC), skipping any custom initialization.
  The factory discovers and calls the correct constructor.

  Compile: fpc -Fu../../src demo_factory_rtti.lpr
  Run:     ./demo_factory_rtti
}
program demo_factory_rtti;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$modeswitch prefixedattributes}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql, Opf.Driver,
  Opf.Relations, Opf.M2M, Opf.Persister.ExtRTTI, Opf.Factory;

type

  { TProduct - has a custom constructor that sets defaults.
    The {$RTTI EXPLICIT} directive enables extended RTTI so
    TExtRTTIPersister.AfterRegister can discover the constructor. }

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished])}
  TProduct = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FCategory: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create;
    property Category: String read FCategory write FCategory;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TSimpleItem - does NOT have a custom constructor.
    No factory is auto-registered; uses default TObject.Create. }

  TSimpleItem = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FTitle: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Title: String read FTitle write FTitle;
  end;

{ TProduct }

constructor TProduct.Create;
begin
  inherited Create;
  FCategory := 'Uncategorized';
  WriteLn('    [TProduct.Create called - FCategory set to "Uncategorized"]');
end;

function TProduct.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TProduct.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

{ TSimpleItem }

function TSimpleItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TSimpleItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Manager: TOPManager;
  Registry: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
  P: TProduct;
  S: TSimpleItem;
  Products: TObjectList;
  Items: TObjectList;
  I: Integer;

begin
  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  try
    Conn.DatabaseName := ':memory:';
    Conn.Transaction := Trans;
    Conn.Open;

    Registry := TOPRegistry.Create(nil);
    RelReg := TOPRelationRegistry.Create(nil);
    RelReg.Registry := Registry;
    M2MReg := TOPManyToManyRegistry.Create(nil);
    M2MReg.Registry := Registry;
    Manager := TOPManager.Create(nil);
    try
      Manager.Registry := Registry;
      Manager.RelationRegistry := RelReg;
      Manager.M2MRegistry := M2MReg;
      with Manager.Connections.Add as TOPConnection do
      begin
        SingleConnection := Conn;
        IsDefault := True;
      end;

      { Register with TExtRTTIPersister - triggers AfterRegister
        which discovers TProduct's custom constructor via RTTI }
      WriteLn('=== Registering classes ===');
      WriteLn;
      WriteLn('Registering TProduct with TExtRTTIPersister...');
      Registry.RegisterPersister(TProduct, TExtRTTIPersister);
      if OPFactory.HasFactory(TProduct) then
        WriteLn('  -> Factory auto-registered (custom constructor discovered)')
      else
        WriteLn('  -> No factory registered (constructor not discovered)');
      WriteLn;

      WriteLn('Registering TSimpleItem with TExtRTTIPersister...');
      Registry.RegisterPersister(TSimpleItem, TExtRTTIPersister);
      if OPFactory.HasFactory(TSimpleItem) then
        WriteLn('  -> Factory auto-registered')
      else
        WriteLn('  -> No factory registered (only has TObject.Create)');
      WriteLn;

      Manager.CreateSchema(Conn);

      { Insert products }
      WriteLn('=== Inserting data ===');
      for I := 1 to 3 do
      begin
        P := TProduct.Create;
        try
          P.ID := I;
          P.Name := Format('Product-%d', [I]);
          P.FState := opsNew;
          Manager.Save(P);
        finally
          P.Free;
        end;
      end;
      WriteLn;

      { Insert simple items }
      for I := 1 to 2 do
      begin
        S := TSimpleItem.Create;
        try
          S.ID := I;
          S.Title := Format('Item-%d', [I]);
          S.FState := opsNew;
          Manager.Save(S);
        finally
          S.Free;
        end;
      end;

      { Load products - custom constructor runs for each }
      WriteLn('=== Loading products (custom constructor fires) ===');
      Products := TObjectList.Create(True);
      try
        Manager.Load(Products, TProduct);
        WriteLn;
        WriteLn('  Loaded ', Products.Count, ' products:');
        for I := 0 to Products.Count - 1 do
        begin
          P := TProduct(Products[I]);
          WriteLn('    ID=', P.ID, ' Name="', P.Name, '" Category="', P.Category, '"');
        end;
        WriteLn;
        WriteLn('  Note: Category="Uncategorized" proves the custom constructor ran.');
        WriteLn('  Without the RTTI factory, Category would be empty.');
      finally
        Products.Free;
      end;
      WriteLn;

      { Load simple items - default constructor, no factory }
      WriteLn('=== Loading simple items (default TObject.Create) ===');
      Items := TObjectList.Create(True);
      try
        Manager.Load(Items, TSimpleItem);
        WriteLn('  Loaded ', Items.Count, ' items:');
        for I := 0 to Items.Count - 1 do
        begin
          S := TSimpleItem(Items[I]);
          WriteLn('    ID=', S.ID, ' Title="', S.Title, '"');
        end;
      finally
        Items.Free;
      end;

    finally
      Manager.Free;
      M2MReg.Free;
      RelReg.Free;
      Registry.Free;
    end;
  finally
    if Trans.Active then
      Trans.Rollback;
    Conn.Close;
    Trans.Free;
    Conn.Free;
  end;
end.
