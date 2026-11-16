{ demo_factory_explicit - Register a custom factory function for object creation
  ==================================================================================
  Demonstrates registering an explicit factory callback (both plain function and
  method of object) with TOPClassFactory. When loading a list, the factory is
  called instead of the default aClass.Create.

  Key concepts:
  - TOPFactoryFunc (plain function) and TOPFactoryMethod (method of object)
  - TOPFactoryContext provides reason, list object, owner, and property name
  - Hierarchy lookup: a factory for TAnimal also creates TDog (descendant)
  - Fallback: classes without a factory use aClass.Create

  Compile: fpc -Fu../../src demo_factory_explicit.lpr
  Run:     ./demo_factory_explicit
}
program demo_factory_explicit;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

uses
  SysUtils, Classes, db, sqldb, sqlite3conn, contnrs,
  Opf.Manager, Opf.Registry, Opf.Fields, Opf.Sql, Opf.Driver,
  Opf.Relations, Opf.M2M, Opf.Persister.Rtti, Opf.Factory;

type

  { TAnimal - base class }

  TAnimal = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FTag: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property Tag: String read FTag write FTag;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TDog - descendant, inherits factory from TAnimal }

  TDog = class(TAnimal)
  end;

  { TAnimalCounter - stateful factory using method of object }

  TAnimalCounter = class
  private
    FCount: Integer;
  public
    constructor Create;
    function CreateAnimal(aClass: TClass; aContext: TOPFactoryContext): TObject;
    property Count: Integer read FCount;
  end;

function TAnimal.GetPersistenceState: TObjectPersistenceState;
begin
  Result := FState;
end;

procedure TAnimal.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState := aValue;
end;

constructor TAnimalCounter.Create;
begin
  inherited Create;
  FCount := 0;
end;

function TAnimalCounter.CreateAnimal(aClass: TClass; aContext: TOPFactoryContext): TObject;
begin
  Inc(FCount);
  Result := aClass.Create;
  TAnimal(Result).Tag := Format('animal-%d', [FCount]);
  WriteLn('    Factory called: created ', aClass.ClassName,
    ' #', FCount,
    ' (reason: ', Ord(aContext.Reason), ')');
end;

var
  Conn: TSQLite3Connection;
  Trans: TSQLTransaction;
  Manager: TOPManager;
  Registry: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
  Counter: TAnimalCounter;
  A: TAnimal;
  Animals: TObjectList;
  I: Integer;
  OpConn: TOPConnection;

begin
  Conn := TSQLite3Connection.Create(nil);
  Trans := TSQLTransaction.Create(nil);
  Counter := TAnimalCounter.Create;
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
      OpConn := Manager.Connections.Add as TOPConnection;
      OpConn.SingleConnection := Conn;
      OpConn.IsDefault := True;

      { Register TAnimal with RTTI persister }
      Registry.RegisterPersister(TAnimal, TRTTIPersister);

      { Register a METHOD factory for TAnimal - tracks creation count }
      OPFactory.RegisterFactory(TAnimal, @Counter.CreateAnimal);

      Manager.CreateSchema(Conn);

      { Insert some data }
      WriteLn('=== Inserting animals ===');
      for I := 1 to 3 do
      begin
        A := TAnimal.Create;
        try
          A.ID := I;
          A.Name := Format('Animal-%d', [I]);
          A.FState := opsNew;
          Manager.Save(A);
          WriteLn('  Saved: ID=', A.ID, ' Name="', A.Name, '"');
        finally
          A.Free;
        end;
      end;
      WriteLn;

      { Load list - factory will be called for each item }
      WriteLn('=== Loading animal list (factory creates each item) ===');
      Animals := TObjectList.Create(True);
      try
        Manager.Load(Animals, TAnimal);
        WriteLn;
        WriteLn('  Loaded ', Animals.Count, ' animals:');
        for I := 0 to Animals.Count - 1 do
        begin
          A := TAnimal(Animals[I]);
          WriteLn('    ID=', A.ID, ' Name="', A.Name, '" Tag="', A.Tag, '"');
        end;
      finally
        Animals.Free;
      end;
      WriteLn;
      WriteLn('  Factory was called ', Counter.Count, ' times');

    finally
      Manager.Free;
      M2MReg.Free;
      RelReg.Free;
      Registry.Free;
    end;
  finally
    Counter.Free;
    if Trans.Active then
      Trans.Rollback;
    Conn.Close;
    Trans.Free;
    Conn.Free;
  end;
end.
