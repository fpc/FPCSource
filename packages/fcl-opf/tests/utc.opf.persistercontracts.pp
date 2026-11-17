unit utc.opf.persistercontracts;

{ Tests for persister and change tracking contracts:
  - class properties: objects created through the factory, copying setters
    do not leak, a collection without factory raises, NULL clears a list
  - the interface persister runs the class's own constructor
  - exact factories do not create descendants
  - TOPFieldMap.Data is free for the user
  - the state table: many objects, freed observed objects }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, Data.Db, Data.Sqldb,
  Data.SqlDb.Sqlite3, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, Variants, DB, sqldb, sqlite3conn, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.FieldValue, Opf.Factory, Opf.Persister.RTTI,
  Opf.Persister.Intf, Opf.ChangeTracking, Opf.Snapshot;

type

  { TPCHolder: a TStrings property whose setter copies }

  TPCHolder = class(TPersistent)
  private
    FLines: TStrings;
    procedure SetLines(aValue: TStrings);
  public
    destructor Destroy; override;
  published
    property Lines: TStrings read FLines write SetLines;
  end;

  { TPCOwnHolder: a TStrings property that takes the object }

  TPCOwnHolder = class(TPersistent)
  private
    FLines: TStrings;
  public
    destructor Destroy; override;
  published
    property Lines: TStrings read FLines write FLines;
  end;

  TPCCollHolder = class(TPersistent)
  private
    FItems: TCollection;
  published
    property Items: TCollection read FItems write FItems;
  end;

  { TPCProvider: field definitions depend on the constructor }

  TPCProvider = class(TComponent, IOPFieldProvider)
  private
    FReady: Boolean;
  public
    constructor Create(aOwner: TComponent); override;
    procedure GetFieldDefs(aMap: TOPFieldMapList);
    function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  end;

  TPCBase = class(TComponent)
  public
    Created: Boolean;
  end;

  TPCDerived = class(TPCBase)
  public
    constructor Create(aOwner: TComponent); override;
  end;

  TPCItem = class(TPersistent)
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

  { TTestPersisterContracts }

  TTestPersisterContracts = class(TTestCase)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FQuery: TSQLQuery;
    FMap: TOPFieldMapList;
    FPersister: TRTTIPersister;
    FFactoryCalls: Integer;
    function FieldFor(const aSQL: String): TField;
    function BaseFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopyingSetterNoLeak;
    procedure TestOwningSetterKeepsObject;
    procedure TestExistingObjectFilled;
    procedure TestNullClearsStrings;
    procedure TestCollectionWithoutFactoryRaises;
    procedure TestPropertyFactoryContext;
    procedure TestInterfacePersisterRunsConstructor;
    procedure TestExactFactorySkipsDescendant;
    procedure TestExactFactoryCreatesOwnClass;
    procedure TestUserDataIgnored;
    procedure TestManyTrackedObjects;
    procedure TestFreedObservedObjectDropped;
  end;

implementation

var
  PropertyFactoryCalls: Integer;
  PropertyFactoryOwner: TObject;
  PropertyFactoryName: String;

function CollectionFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
begin
  Inc(PropertyFactoryCalls);
  PropertyFactoryOwner:=aContext.OwnerObject;
  PropertyFactoryName:=aContext.PropertyName;
  Result:=TCollection.Create(TCollectionItem);
end;

{ TPCHolder }

procedure TPCHolder.SetLines(aValue: TStrings);
begin
  if FLines=Nil then
    FLines:=TStringList.Create;
  FLines.Assign(aValue);
end;

destructor TPCHolder.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

{ TPCOwnHolder }

destructor TPCOwnHolder.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

{ TPCProvider }

constructor TPCProvider.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FReady:=True;
end;

procedure TPCProvider.GetFieldDefs(aMap: TOPFieldMapList);
begin
  if FReady then
    aMap.AddField('Name', 'Name');
end;

function TPCProvider.ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
begin
  Result:=False;
end;

function TPCProvider.PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
begin
  Result:=False;
end;

{ TPCDerived }

constructor TPCDerived.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Created:=True;
end;

{ TTestPersisterContracts }

procedure TTestPersisterContracts.SetUp;
begin
  inherited SetUp;
  FConn:=TSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  FQuery:=TSQLQuery.Create(Nil);
  FQuery.Database:=FConn;
  FQuery.Transaction:=FTrans;
  FMap:=TOPFieldMapList.Create(TOPFieldMap);
  FPersister:=TRTTIPersister.Create;
  PropertyFactoryCalls:=0;
end;

procedure TTestPersisterContracts.TearDown;
begin
  FreeAndNil(FPersister);
  FreeAndNil(FMap);
  FreeAndNil(FQuery);
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

function TTestPersisterContracts.FieldFor(const aSQL: String): TField;
begin
  FQuery.Close;
  FQuery.SQL.Text:=aSQL;
  FQuery.Open;
  Result:=FQuery.Fields[0];
end;

function TTestPersisterContracts.BaseFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
begin
  Inc(FFactoryCalls);
  Result:=TPCBase.Create(Nil);
end;

procedure TTestPersisterContracts.TestCopyingSetterNoLeak;
var
  H: TPCHolder;
begin
  FPersister.PopulateFieldMap(TPCHolder, FMap);
  H:=TPCHolder.Create;
  try
    FPersister.ApplyProperty(H, FMap.FindByPropertyName('Lines'), FieldFor('SELECT ''a'' AS Lines'));
    AssertNotNull('Setter created its list', H.Lines);
    AssertEquals('Value copied', 'a', Trim(H.Lines.Text));
  finally
    H.Free;
  end;
  // heaptrc reports the temporary list if it was not freed
end;

procedure TTestPersisterContracts.TestOwningSetterKeepsObject;
var
  H: TPCOwnHolder;
begin
  FPersister.PopulateFieldMap(TPCOwnHolder, FMap);
  H:=TPCOwnHolder.Create;
  try
    FPersister.ApplyProperty(H, FMap.FindByPropertyName('Lines'), FieldFor('SELECT ''b'' AS Lines'));
    AssertNotNull('Object taken', H.Lines);
    AssertEquals('b', Trim(H.Lines.Text));
  finally
    H.Free;
  end;
end;

procedure TTestPersisterContracts.TestExistingObjectFilled;
var
  H: TPCOwnHolder;
  L: TStrings;
begin
  FPersister.PopulateFieldMap(TPCOwnHolder, FMap);
  H:=TPCOwnHolder.Create;
  try
    L:=TStringList.Create;
    H.Lines:=L;
    FPersister.ApplyProperty(H, FMap.FindByPropertyName('Lines'), FieldFor('SELECT ''c'' AS Lines'));
    AssertSame('Same object', L, H.Lines);
    AssertEquals('c', Trim(H.Lines.Text));
  finally
    H.Free;
  end;
end;

procedure TTestPersisterContracts.TestNullClearsStrings;
var
  H: TPCOwnHolder;
begin
  FPersister.PopulateFieldMap(TPCOwnHolder, FMap);
  H:=TPCOwnHolder.Create;
  try
    H.Lines:=TStringList.Create;
    H.Lines.Text:='old';
    FPersister.ApplyProperty(H, FMap.FindByPropertyName('Lines'), FieldFor('SELECT CAST(NULL AS TEXT) AS Lines'));
    AssertEquals('Cleared', 0, H.Lines.Count);
  finally
    H.Free;
  end;
end;

procedure TTestPersisterContracts.TestCollectionWithoutFactoryRaises;
var
  H: TPCCollHolder;
  Raised: Boolean;
begin
  FPersister.PopulateFieldMap(TPCCollHolder, FMap);
  H:=TPCCollHolder.Create;
  try
    Raised:=False;
    try
      FPersister.ApplyProperty(H, FMap.FindByPropertyName('Items'), FieldFor('SELECT ''[]'' AS Items'));
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('A collection needs its item class', Raised);
    AssertNull('Nothing assigned', H.Items);
  finally
    H.Free;
  end;
end;

procedure TTestPersisterContracts.TestPropertyFactoryContext;
var
  H: TPCCollHolder;
begin
  OPFactory.RegisterFactory(TCollection, @CollectionFactory);
  try
    FPersister.PopulateFieldMap(TPCCollHolder, FMap);
    H:=TPCCollHolder.Create;
    try
      FPersister.ApplyProperty(H, FMap.FindByPropertyName('Items'), FieldFor('SELECT ''[]'' AS Items'));
      AssertEquals('Factory used', 1, PropertyFactoryCalls);
      AssertSame('Owner in the context', H, PropertyFactoryOwner);
      AssertEquals('Property in the context', 'Items', PropertyFactoryName);
      AssertNotNull('Assigned', H.Items);
    finally
      H.Items.Free;
      H.Free;
    end;
  finally
    OPFactory.UnregisterFactory(TCollection);
  end;
end;

procedure TTestPersisterContracts.TestInterfacePersisterRunsConstructor;
var
  P: TInterfacePersister;
begin
  P:=TInterfacePersister.Create;
  try
    P.PopulateFieldMap(TPCProvider, FMap);
    AssertEquals('Fields from a constructed instance', 1, FMap.Count);
  finally
    P.Free;
  end;
end;

procedure TTestPersisterContracts.TestExactFactorySkipsDescendant;
var
  O: TObject;
begin
  FFactoryCalls:=0;
  OPFactory.RegisterExactFactory(TPCBase, @BaseFactory);
  try
    O:=OPFactory.CreateInstance(TPCDerived, Nil);
    try
      AssertEquals('Factory not used', 0, FFactoryCalls);
      AssertTrue('Own class', O is TPCDerived);
      AssertTrue('Own constructor ran', TPCDerived(O).Created);
    finally
      O.Free;
    end;
  finally
    OPFactory.UnregisterFactory(TPCBase);
  end;
end;

procedure TTestPersisterContracts.TestExactFactoryCreatesOwnClass;
var
  O: TObject;
begin
  FFactoryCalls:=0;
  OPFactory.RegisterExactFactory(TPCBase, @BaseFactory);
  try
    O:=OPFactory.CreateInstance(TPCBase, Nil);
    O.Free;
    AssertEquals('Factory used for its own class', 1, FFactoryCalls);
    AssertTrue('Unregister by owner', OPFactory.HasFactory(TPCBase));
    OPFactory.UnregisterFactory(TPCBase, Pointer(1));
    AssertTrue('Other owner: kept', OPFactory.HasFactory(TPCBase));
    OPFactory.UnregisterFactory(TPCBase, Self);
    AssertFalse('Same owner: removed', OPFactory.HasFactory(TPCBase));
  finally
    OPFactory.UnregisterFactory(TPCBase);
  end;
end;

procedure TTestPersisterContracts.TestUserDataIgnored;
var
  F: TOPFieldMap;
  I: TPCItem;
  V: Variant;
begin
  F:=FMap.AddField('Name', 'Name');
  F.PropertyKind:=tkAString;
  // A manually added map without PropInfo, with data of the application
  F.Data:=Pointer(12345);
  I:=TPCItem.Create;
  try
    I.Name:='x';
    AssertTrue('Readable', OPFReadFieldValue(I, F, V));
    AssertEquals('x', String(V));
    AssertFalse('Not null', OPFIsFieldNull(I, F));
  finally
    I.Free;
  end;
end;

procedure TTestPersisterContracts.TestManyTrackedObjects;
const
  N = 20000;
var
  T: TOPSnapshotManager;
  Items: array of TPCItem;
  I, Changed: Integer;
  Names: TUTF8StringArray;
  Start: QWord;
begin
  FPersister.PopulateFieldMap(TPCItem, FMap);
  FMap.FindByPropertyName('Name').Options:=[foInSelect, foInInsert, foInUpdate];
  SetLength(Items, N);
  T:=TOPSnapshotManager.Create(Nil);
  try
    Start:=GetTickCount64;
    for I:=0 to N-1 do
    begin
      Items[I]:=TPCItem.Create;
      Items[I].Name:='n'+IntToStr(I);
      T.Capture(Items[I], FMap);
    end;
    Items[N div 2].Name:='changed';
    Changed:=0;
    for I:=0 to N-1 do
      if T.GetChanges(Items[I], FMap, Names) and (Length(Names)>0) then
        Inc(Changed);
    AssertEquals('One object changed', 1, Changed);
    // Linear lookups took minutes here; a hash lookup takes well under a second
    AssertTrue('Fast enough: '+IntToStr(GetTickCount64-Start)+' ms', GetTickCount64-Start<10000);
  finally
    T.Free;
    for I:=0 to N-1 do
      Items[I].Free;
  end;
end;

procedure TTestPersisterContracts.TestFreedObservedObjectDropped;
var
  T: TOPSnapshotManager;
  A, B: TPCItem;
  Names: TUTF8StringArray;
begin
  FPersister.PopulateFieldMap(TPCItem, FMap);
  FMap.FindByPropertyName('Name').Options:=[foInSelect, foInInsert, foInUpdate];
  T:=TOPSnapshotManager.Create(Nil);
  try
    A:=TPCItem.Create;
    A.Name:='a';
    T.Capture(A, FMap);
    // TPersistent reports being freed: the entry goes with it
    A.Free;
    B:=TPCItem.Create;
    try
      B.Name:='b';
      AssertFalse('New object (possibly at the same address) is not tracked',
        T.GetChanges(B, FMap, Names));
    finally
      B.Free;
    end;
  finally
    T.Free;
  end;
end;

initialization
  RegisterTest(TTestPersisterContracts);
end.
