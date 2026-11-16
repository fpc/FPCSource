unit utc.opf.lazycontracts;

{ Tests for automatic lazy loading (FPC 3.3.1+):
  - N:1: the foreign key column is read through the property mapped to it
  - lazy fields are set up for objects loaded in a list
  - bindings go with their entity; entity and manager can be freed in any order }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, DB, sqldb, fpcunit, testregistry,
  Opf.Types, Opf.Fields, Opf.Driver, Opf.Sql, Opf.Registry, Opf.Relations,
  Opf.M2M, Opf.Manager, Opf.Persister.RTTI, Opf.Persister.ExtRTTI, Opf.Criteria, Opf.Proxy, utc.opf.failconn;

type

  TLZParent = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TLZChild = class(TPersistent, IOPPersistenceState)
  private
    FID: Integer;
    FOwnerRef: Integer;
    FParentObj: TLZParent;
    FParent: TOPLazyObject;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetParent: TLZParent;
  public
    constructor Create;
    destructor Destroy; override;
    property Parent: TLZParent read GetParent;
  published
    property ID: Integer read FID write FID;
    // Stored in column ParentKey
    property OwnerRef: Integer read FOwnerRef write FOwnerRef;
  end;

  { TLZPlainChild: as TLZChild, but cannot be observed (not a TPersistent) }

  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TLZPlainChild = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FOwnerRef: Integer;
    FParentObj: TLZParent;
    FParent: TOPLazyObject;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
    function GetParent: TLZParent;
  public
    constructor Create;
    destructor Destroy; override;
    property Parent: TLZParent read GetParent;
  published
    property ID: Integer read FID write FID;
    property OwnerRef: Integer read FOwnerRef write FOwnerRef;
  end;

  { TTestLazyContracts }

  TTestLazyContracts = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FBuilder: TOPSQLBuilder;
    FManager: TOPManager;
    procedure Exec(const aSQL: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestManyToOneByProperty;
    procedure TestListLoadSetsUpLazyFields;
    procedure TestEntityFreedBeforeManager;
    procedure TestManagerFreedBeforeEntity;
    procedure TestManagerFreedThenLazyLoadRaises;
    procedure TestUnobservableManagerFreedThenLazyLoadRaises;
    procedure TestUnobservableEntityFreedBeforeManager;
    procedure TestUnobservableEntityFreeReleasesBinding;
    procedure TestObservableEntityFreeReleasesBinding;
    procedure TestLoadSQLSetsUpLazyFields;
  end;

implementation

{ TLZParent }

function TLZParent.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TLZParent.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TLZChild }

constructor TLZChild.Create;
begin
  inherited Create;
  FParentObj:=TLZParent.Create;
  FParent.Init(FParentObj);
end;

destructor TLZChild.Destroy;
begin
  FParentObj.Free;
  inherited Destroy;
end;

function TLZChild.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TLZChild.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

function TLZChild.GetParent: TLZParent;
begin
  Result:=TLZParent(FParent.Value);
end;

{ TLZPlainChild }

constructor TLZPlainChild.Create;
begin
  inherited Create;
  FParentObj:=TLZParent.Create;
  FParent.Init(FParentObj);
end;

destructor TLZPlainChild.Destroy;
begin
  FParentObj.Free;
  inherited Destroy;
end;

function TLZPlainChild.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TLZPlainChild.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

function TLZPlainChild.GetParent: TLZParent;
begin
  Result:=TLZParent(FParent.Value);
end;

{ TTestLazyContracts }

procedure TTestLazyContracts.Exec(const aSQL: String);
begin
  if FTrans.Active then
    FTrans.Commit;
  FConn.ExecuteDirect(aSQL);
  FTrans.Commit;
end;

procedure TTestLazyContracts.SetUp;
var
  C: TOPConnection;
  D: TPersisterDef;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  Exec('CREATE TABLE LZParent (ID INTEGER PRIMARY KEY, Name TEXT)');
  Exec('CREATE TABLE LZChild (ID INTEGER PRIMARY KEY, ParentKey INTEGER)');
  Exec('INSERT INTO LZParent (ID, Name) VALUES (7, ''parent seven'')');
  Exec('INSERT INTO LZChild (ID, ParentKey) VALUES (1, 7)');
  Exec('INSERT INTO LZChild (ID, ParentKey) VALUES (2, 7)');
  FRegistry:=TOPRegistry.Create(Nil);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  FRegistry.RegisterPersister(TLZParent, TRTTIPersister);
  // The extended RTTI persister creates list items with their own constructor
  D:=FRegistry.RegisterPersister(TLZChild, TExtRTTIPersister);
  D.FieldMap.FindByPropertyName('OwnerRef').FieldName:='ParentKey';
  FRelations.RegisterRelationship(TLZParent, TLZChild, 'ParentKey');
  Exec('CREATE TABLE LZPlainChild (ID INTEGER PRIMARY KEY, ParentKey INTEGER)');
  Exec('INSERT INTO LZPlainChild (ID, ParentKey) VALUES (1, 7)');
  D:=FRegistry.RegisterPersister(TLZPlainChild, TExtRTTIPersister);
  D.FieldMap.FindByPropertyName('OwnerRef').FieldName:='ParentKey';
  FRelations.RegisterRelationship(TLZParent, TLZPlainChild, 'ParentKey');
  FBuilder:=TOPSQLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dSQLite;
  FBuilder.Registry:=FRegistry;
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.RelationRegistry:=FRelations;
  FManager.M2MRegistry:=FM2M;
  FManager.SQLBuilder:=FBuilder;
  C:=FManager.Connections.Add as TOPConnection;
  C.SingleConnection:=FConn;
  C.IsDefault:=True;
end;

procedure TTestLazyContracts.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FBuilder);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestLazyContracts.TestManyToOneByProperty;
var
  C: TLZChild;
begin
  C:=TLZChild.Create;
  try
    C.ID:=1;
    FManager.Load(C);
    AssertEquals('Foreign key loaded', 7, C.OwnerRef);
    AssertEquals('Parent loaded on access', 'parent seven', C.Parent.Name);
  finally
    C.Free;
  end;
end;

procedure TTestLazyContracts.TestListLoadSetsUpLazyFields;
var
  L: TFPObjectList;
  Crit: TOPCriteria;
begin
  L:=TFPObjectList.Create(True);
  Crit:=TOPCriteria.Create;
  try
    Crit.OrderBy('ID');
    FManager.Load(L, TLZChild, Crit);
    AssertEquals(2, L.Count);
    AssertEquals('Lazy field of a list item works', 'parent seven', TLZChild(L[1]).Parent.Name);
  finally
    Crit.Free;
    L.Free;
  end;
end;

procedure TTestLazyContracts.TestEntityFreedBeforeManager;
var
  C: TLZChild;
begin
  C:=TLZChild.Create;
  C.ID:=1;
  FManager.Load(C);
  // The binding goes with the entity
  C.Free;
  FreeAndNil(FManager);
end;

procedure TTestLazyContracts.TestManagerFreedBeforeEntity;
var
  C: TLZChild;
begin
  C:=TLZChild.Create;
  try
    C.ID:=1;
    FManager.Load(C);
    // The binding stops observing the entity when the manager goes
    FreeAndNil(FManager);
  finally
    C.Free;
  end;
end;

procedure TTestLazyContracts.TestManagerFreedThenLazyLoadRaises;
var
  C: TLZChild;
  Raised: Boolean;
begin
  C:=TLZChild.Create;
  try
    C.ID:=1;
    FManager.Load(C);
    FreeAndNil(FManager);
    Raised:=False;
    try
      // Before the fix: the record called the freed binding
      AssertEquals('Not reached', '', C.Parent.Name);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Lazy load after the manager is gone raises', Raised);
  finally
    C.Free;
  end;
end;

procedure TTestLazyContracts.TestUnobservableManagerFreedThenLazyLoadRaises;
var
  C: TLZPlainChild;
  Raised: Boolean;
begin
  C:=TLZPlainChild.Create;
  try
    C.ID:=1;
    FManager.Load(C);
    AssertEquals('Foreign key loaded', 7, C.OwnerRef);
    FreeAndNil(FManager);
    Raised:=False;
    try
      // Before the fix: the manager freed the binding the record points at
      AssertEquals('Not reached', '', C.Parent.Name);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Lazy load after the manager is gone raises', Raised);
  finally
    C.Free;
  end;
end;

procedure TTestLazyContracts.TestUnobservableEntityFreedBeforeManager;
var
  C: TLZPlainChild;
begin
  C:=TLZPlainChild.Create;
  C.ID:=1;
  FManager.Load(C);
  AssertEquals('Lazy load works', 'parent seven', C.Parent.Name);
  C.Free;
  // The lazy record releases the binding with the entity
  FreeAndNil(FManager);
end;

procedure TTestLazyContracts.TestUnobservableEntityFreeReleasesBinding;
var
  C: TLZPlainChild;
  I: Integer;
begin
  // The entity cannot report its free: the lazy record releases the binding
  for I:=1 to 50 do
  begin
    C:=TLZPlainChild.Create;
    try
      C.ID:=1;
      FManager.Load(C);
      AssertEquals('One binding while the entity lives', 1, FManager.LazyBindingCount);
    finally
      C.Free;
    end;
  end;
  AssertEquals('Bindings released with their entities', 0, FManager.LazyBindingCount);
end;

procedure TTestLazyContracts.TestObservableEntityFreeReleasesBinding;
var
  C: TLZChild;
begin
  C:=TLZChild.Create;
  try
    C.ID:=1;
    FManager.Load(C);
    AssertTrue('Binding set up', FManager.LazyBindingCount>0);
  finally
    C.Free;
  end;
  AssertEquals('Binding released with its entity', 0, FManager.LazyBindingCount);
end;

procedure TTestLazyContracts.TestLoadSQLSetsUpLazyFields;
var
  C: TLZChild;
begin
  C:=TLZChild.Create;
  try
    // Before the fix LoadSQL(object) did not set up the lazy fields
    FManager.LoadSQL(C, 'SELECT ID, ParentKey FROM LZChild WHERE ID=1');
    AssertEquals('Parent loaded on access', 'parent seven', C.Parent.Name);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest(TTestLazyContracts);
end.
