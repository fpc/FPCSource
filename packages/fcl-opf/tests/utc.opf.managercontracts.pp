unit utc.opf.managercontracts;

{ Tests for manager contracts:
  - the connection resolver receives the class, connections per class
  - Save(object, state, persister) commits on its own
  - audit fields: an unchanged object is not updated, a changed one writes them
  - the audit user object is forgotten when it is freed
  - Load with KeyOnly reads only the key
  - LoadChildren checks the configured list property
  - the sqldb log hook with several managers }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, contnrs, DB, sqldb, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Driver, Opf.Sql, Opf.Registry, Opf.Relations,
  Opf.M2M, Opf.Manager, Opf.Persister.RTTI, Opf.Snapshot, utc.opf.failconn;

type

  TMCBase = class(TPersistent, IOPPersistenceState)
  private
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  end;

  TMCItem = class(TMCBase)
  private
    FID: Integer;
    FName: String;
    FUpdatedAt: TDateTime;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
  end;

  TMCOther = class(TMCBase)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TMCChild = class(TMCBase)
  private
    FID, FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
  end;

  TMCParent = class(TMCBase)
  private
    FID: Integer;
    FKids: TFPObjectList;
    FOther: TFPObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Kids: TFPObjectList read FKids;
    property Other: TFPObjectList read FOther;
  end;

  { TMCHolder: one child relation without configured list property, and a
    many-to-many list Others }

  TMCHolder = class(TMCBase)
  private
    FID: Integer;
    FItems: TFPObjectList;
    FOthers: TFPObjectList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ID: Integer read FID write FID;
    property Items: TFPObjectList read FItems;
    property Others: TFPObjectList read FOthers;
  end;

  { TMCResolver: records the classes asked for }

  TMCResolver = class(TObject, IOPConnectionResolver)
  public
    Classes: TStringList;
    Main, Second: TSQLConnection;
    constructor Create;
    destructor Destroy; override;
    function ResolveConnection(aClass: TClass): TSQLConnection;
  end;

  { TTestManagerContracts2 }

  TTestManagerContracts2 = class(TTestCase)
  private
    FConn, FConn2: TFailingSQLite3Connection;
    FTrans, FTrans2: TSQLTransaction;
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FBuilder: TOPSQLBuilder;
    FManager: TOPManager;
    FTracker: TOPSnapshotManager;
    function OpenConn(out aTrans: TSQLTransaction): TFailingSQLite3Connection;
    procedure Exec(aConn: TSQLConnection; const aSQL: String);
    function Value(aConn: TSQLConnection; const aSQL: String): Variant;
    function NewItem(aID: Integer; const aName: String): TMCItem;
    procedure CountAfterLoad(Sender: TObject; aObject: TObject);
    procedure SetUpHolder;
  protected
    FAfterLoadCount: Integer;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolverReceivesClass;
    procedure TestConnectionPerClass;
    procedure TestSaveWithPersisterCommits;
    procedure TestUnchangedAuditedObjectNotUpdated;
    procedure TestChangedAuditedObjectWritesAudit;
    procedure TestAuditObjectKeptAsKey;
    procedure TestAuditObjectWithoutKeyRaises;
    procedure TestKeyOnlyLoad;
    procedure TestKeyOnlyNotFoundRaises;
    procedure TestLoadChildrenWrongNameRaises;
    procedure TestLoadChildrenConfiguredName;
    procedure TestLogHookTwoManagers;
    procedure TestLogHookRestoresPrevious;
    // Review 2
    procedure TestLoadChildrenM2MListRaises;
    procedure TestLoadChildrenAnyListStillAccepted;
    procedure TestLoadChildrenConfigErrorDoesNotDoom;
    procedure TestAddAssociationDuplicateDoesNotDoom;
    procedure TestAddAssociationErrorKeepsOriginal;
    procedure TestJunctionRowsOnClassAConnection;
    procedure TestAfterLoadForListItems;
    procedure TestAfterLoadForLoadSQL;
    procedure TestChildrenCleanAndTracked;
  end;

implementation

var
  ForeignHookCalls: Integer;

type
  TForeignHook = class
    procedure Log(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
  end;

procedure TForeignHook.Log(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
begin
  Inc(ForeignHookCalls);
end;

{ TMCBase }

function TMCBase.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TMCBase.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TMCParent }

constructor TMCParent.Create;
begin
  inherited Create;
  FKids:=TFPObjectList.Create(True);
  FOther:=TFPObjectList.Create(True);
end;

destructor TMCParent.Destroy;
begin
  FKids.Free;
  FOther.Free;
  inherited Destroy;
end;

{ TMCHolder }

constructor TMCHolder.Create;
begin
  inherited Create;
  FItems:=TFPObjectList.Create(True);
  FOthers:=TFPObjectList.Create(True);
end;

destructor TMCHolder.Destroy;
begin
  FItems.Free;
  FOthers.Free;
  inherited Destroy;
end;

{ TMCResolver }

constructor TMCResolver.Create;
begin
  inherited Create;
  Classes:=TStringList.Create;
end;

destructor TMCResolver.Destroy;
begin
  Classes.Free;
  inherited Destroy;
end;

function TMCResolver.ResolveConnection(aClass: TClass): TSQLConnection;
begin
  if aClass=Nil then
    Classes.Add('nil')
  else
    Classes.Add(aClass.ClassName);
  if (aClass=TMCOther) and Assigned(Second) then
    Result:=Second
  else
    Result:=Main;
end;

{ TTestManagerContracts2 }

function TTestManagerContracts2.OpenConn(out aTrans: TSQLTransaction): TFailingSQLite3Connection;
begin
  Result:=TFailingSQLite3Connection.Create(Nil);
  Result.DatabaseName:=':memory:';
  aTrans:=TSQLTransaction.Create(Nil);
  aTrans.Database:=Result;
  Result.Transaction:=aTrans;
  Result.Open;
end;

procedure TTestManagerContracts2.Exec(aConn: TSQLConnection; const aSQL: String);
begin
  if aConn.Transaction.Active then
    aConn.Transaction.Commit;
  aConn.ExecuteDirect(aSQL);
  aConn.Transaction.Commit;
end;

function TTestManagerContracts2.Value(aConn: TSQLConnection; const aSQL: String): Variant;
var
  Q: TSQLQuery;
begin
  if aConn.Transaction.Active then
    aConn.Transaction.Commit;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConn;
    Q.Transaction:=aConn.Transaction;
    Q.SQL.Text:=aSQL;
    Q.Open;
    Result:=Q.Fields[0].Value;
    Q.Close;
  finally
    Q.Free;
  end;
  aConn.Transaction.Commit;
end;

function TTestManagerContracts2.NewItem(aID: Integer; const aName: String): TMCItem;
begin
  Result:=TMCItem.Create;
  Result.ID:=aID;
  Result.Name:=aName;
  Result.State:=opsNew;
end;

procedure TTestManagerContracts2.SetUp;
var
  C: TOPConnection;
  D: TPersisterDef;
begin
  inherited SetUp;
  FConn:=OpenConn(FTrans);
  Exec(FConn, 'CREATE TABLE MCItem (ID INTEGER PRIMARY KEY, Name TEXT, UpdatedAt TIMESTAMP)');
  Exec(FConn, 'CREATE TABLE MCOther (ID INTEGER PRIMARY KEY, Name TEXT)');
  Exec(FConn, 'CREATE TABLE MCChild (ID INTEGER PRIMARY KEY, ParentID INTEGER)');
  Exec(FConn, 'CREATE TABLE MCParent (ID INTEGER PRIMARY KEY)');
  FRegistry:=TOPRegistry.Create(Nil);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  D:=FRegistry.RegisterPersister(TMCItem, TRTTIPersister);
  D.FieldMap.FindByPropertyName('UpdatedAt').Options:=D.FieldMap.FindByPropertyName('UpdatedAt').Options+[foUpdatedAt];
  FRegistry.RegisterPersister(TMCOther, TRTTIPersister);
  FRegistry.RegisterPersister(TMCChild, TRTTIPersister);
  FRegistry.RegisterPersister(TMCParent, TRTTIPersister);
  FRelations.RegisterRelationship(TMCParent, TMCChild, 'ParentID', False, 'Kids');
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

procedure TTestManagerContracts2.TearDown;
begin
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FBuilder);
  FreeAndNil(FM2M);
  FreeAndNil(FRelations);
  FreeAndNil(FRegistry);
  if Assigned(FConn2) then
  begin
    if FTrans2.Active then
      FTrans2.Rollback;
    FConn2.Close;
    FreeAndNil(FTrans2);
    FreeAndNil(FConn2);
  end;
  if FTrans.Active then
    FTrans.Rollback;
  FConn.Close;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestManagerContracts2.TestResolverReceivesClass;
var
  R: TMCResolver;
  I: TMCItem;
begin
  R:=TMCResolver.Create;
  try
    R.Main:=FConn;
    FManager.ConnectionResolver:=R;
    I:=NewItem(1, 'a');
    try
      FManager.Save(I);
      I.ID:=1;
      FManager.Load(I);
      FManager.Delete(I);
    finally
      I.Free;
    end;
    FManager.ConnectionResolver:=Nil;
    AssertEquals('Save, Load, Delete: the class each time', 'TMCItem,TMCItem,TMCItem', R.Classes.CommaText);
  finally
    R.Free;
  end;
end;

procedure TTestManagerContracts2.TestConnectionPerClass;
var
  R: TMCResolver;
  O: TMCOther;
begin
  FConn2:=OpenConn(FTrans2);
  Exec(FConn2, 'CREATE TABLE MCOther (ID INTEGER PRIMARY KEY, Name TEXT)');
  R:=TMCResolver.Create;
  try
    R.Main:=FConn;
    R.Second:=FConn2;
    FManager.ConnectionResolver:=R;
    O:=TMCOther.Create;
    try
      O.ID:=1;
      O.Name:='second';
      O.State:=opsNew;
      FManager.Save(O);
    finally
      O.Free;
    end;
    FManager.ConnectionResolver:=Nil;
  finally
    R.Free;
  end;
  AssertEquals('Stored on the second connection', 1, Integer(Value(FConn2, 'SELECT COUNT(*) FROM MCOther')));
  AssertEquals('Not on the first', 0, Integer(Value(FConn, 'SELECT COUNT(*) FROM MCOther')));
end;

procedure TTestManagerContracts2.TestSaveWithPersisterCommits;
var
  I: TMCItem;
  P: TRTTIPersister;
begin
  I:=NewItem(5, 'p');
  P:=TRTTIPersister.Create;
  try
    FManager.Save(I, opsNew, P);
    AssertFalse('No transaction left open', FManager.InTransaction);
  finally
    P.Free;
    I.Free;
  end;
  AssertEquals('Committed', 1, Integer(Value(FConn, 'SELECT COUNT(*) FROM MCItem')));
end;

procedure TTestManagerContracts2.TestUnchangedAuditedObjectNotUpdated;
var
  I: TMCItem;
begin
  FTracker:=TOPSnapshotManager.Create(Nil);
  FTracker.Manager:=FManager;
  Exec(FConn, 'INSERT INTO MCItem (ID, Name, UpdatedAt) VALUES (1, ''a'', ''2001-01-01 00:00:00.000'')');
  I:=TMCItem.Create;
  try
    I.ID:=1;
    FManager.Load(I);
    I.State:=opsUpdate;
    FManager.Save(I);
    AssertEquals('Audit field not touched in the object', EncodeDate(2001,1,1), I.UpdatedAt, 1e-6);
  finally
    I.Free;
  end;
  AssertEquals('Nothing written', EncodeDate(2001,1,1), TDateTime(Value(FConn, 'SELECT UpdatedAt FROM MCItem')), 1e-6);
end;

procedure TTestManagerContracts2.TestChangedAuditedObjectWritesAudit;
var
  I: TMCItem;
begin
  FTracker:=TOPSnapshotManager.Create(Nil);
  FTracker.Manager:=FManager;
  Exec(FConn, 'INSERT INTO MCItem (ID, Name, UpdatedAt) VALUES (1, ''a'', ''2001-01-01 00:00:00.000'')');
  I:=TMCItem.Create;
  try
    I.ID:=1;
    FManager.Load(I);
    I.Name:='b';
    I.State:=opsUpdate;
    FManager.Save(I);
    AssertTrue('Audit field set', I.UpdatedAt>EncodeDate(2002,1,1));
  finally
    I.Free;
  end;
  AssertTrue('Audit field written', TDateTime(Value(FConn, 'SELECT UpdatedAt FROM MCItem'))>EncodeDate(2002,1,1));
end;

procedure TTestManagerContracts2.TestAuditObjectKeptAsKey;
var
  U: TMCOther;
begin
  U:=TMCOther.Create;
  U.ID:=7;
  FManager.AuditInfo.ObjectID:=U;
  U.Free;
  AssertTrue('Object user', FManager.AuditInfo.UserIDKind=ukObject);
  AssertEquals('Key kept', '7', FManager.AuditInfo.ObjectKey);
  AssertEquals('Class kept', 'TMCOther', FManager.AuditInfo.ObjectClassName);
  FManager.AuditInfo.ObjectID:=Nil;
  AssertTrue('No user any more', FManager.AuditInfo.UserIDKind=ukNone);
  AssertEquals('Key cleared', '', FManager.AuditInfo.ObjectKey);
end;

procedure TTestManagerContracts2.TestAuditObjectWithoutKeyRaises;
var
  U: TObject;
  Raised: Boolean;
begin
  FManager.AuditInfo.StringID:='keep';
  U:=TObject.Create;
  try
    Raised:=False;
    try
      FManager.AuditInfo.ObjectID:=U;
    except
      on E: EOPF do
        Raised:=True;
    end;
  finally
    U.Free;
  end;
  AssertTrue('Unregistered class raises', Raised);
  AssertTrue('User unchanged', FManager.AuditInfo.UserIDKind=ukString);
  AssertEquals('StringID unchanged', 'keep', FManager.AuditInfo.StringID);
end;

procedure TTestManagerContracts2.TestKeyOnlyLoad;
var
  I: TMCItem;
begin
  Exec(FConn, 'INSERT INTO MCItem (ID, Name, UpdatedAt) VALUES (1, ''full'', ''2001-01-01 00:00:00.000'')');
  I:=TMCItem.Create;
  try
    I.ID:=1;
    FManager.Load(I, True);
    AssertEquals('Other fields not read', '', I.Name);
    AssertTrue('Key state', I.State=opsKey);
  finally
    I.Free;
  end;
end;

procedure TTestManagerContracts2.TestKeyOnlyNotFoundRaises;
var
  I: TMCItem;
  Raised: Boolean;
begin
  I:=TMCItem.Create;
  try
    I.ID:=42;
    Raised:=False;
    try
      FManager.Load(I, True);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Existence check', Raised);
  finally
    I.Free;
  end;
end;

procedure TTestManagerContracts2.TestLoadChildrenWrongNameRaises;
var
  P: TMCParent;
  Raised: Boolean;
begin
  P:=TMCParent.Create;
  try
    P.ID:=1;
    Raised:=False;
    try
      FManager.LoadChildren(P, 'Other');
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Other is not the configured list', Raised);
    AssertEquals('Nothing loaded into it', 0, P.Other.Count);
  finally
    P.Free;
  end;
end;

procedure TTestManagerContracts2.TestLoadChildrenConfiguredName;
var
  P: TMCParent;
begin
  Exec(FConn, 'INSERT INTO MCChild (ID, ParentID) VALUES (1, 1)');
  P:=TMCParent.Create;
  try
    P.ID:=1;
    FManager.LoadChildren(P, 'Kids');
    AssertEquals(1, P.Kids.Count);
  finally
    P.Free;
  end;
end;

procedure TTestManagerContracts2.TestLogHookTwoManagers;
var
  M2: TOPManager;
  Saved: TDBLogNotifyEvent;
begin
  Saved:=GlobalDBLogHook;
  M2:=TOPManager.Create(Nil);
  try
    FManager.Options:=FManager.Options+[omoHookSQLDBLog];
    M2.Options:=M2.Options+[omoHookSQLDBLog];
    AssertTrue('Installed', Assigned(GlobalDBLogHook));
    FManager.Options:=FManager.Options-[omoHookSQLDBLog];
    AssertTrue('Still installed for the second manager', Assigned(GlobalDBLogHook));
    M2.Options:=M2.Options-[omoHookSQLDBLog];
    AssertTrue('Hook from before restored with the last manager',
      (TMethod(GlobalDBLogHook).Code=TMethod(Saved).Code) and (TMethod(GlobalDBLogHook).Data=TMethod(Saved).Data));
  finally
    M2.Free;
  end;
end;

procedure TTestManagerContracts2.TestLogHookRestoresPrevious;
var
  F: TForeignHook;
  Saved: TDBLogNotifyEvent;
begin
  Saved:=GlobalDBLogHook;
  F:=TForeignHook.Create;
  try
    GlobalDBLogHook:=@F.Log;
    FManager.Options:=FManager.Options+[omoHookSQLDBLog];
    FManager.Options:=FManager.Options-[omoHookSQLDBLog];
    AssertTrue('Previous hook restored', TMethod(GlobalDBLogHook).Data=Pointer(F));
  finally
    GlobalDBLogHook:=Saved;
    F.Free;
  end;
end;

procedure TTestManagerContracts2.CountAfterLoad(Sender: TObject; aObject: TObject);
begin
  Inc(FAfterLoadCount);
end;

procedure TTestManagerContracts2.SetUpHolder;
begin
  Exec(FConn, 'CREATE TABLE MCHolder (ID INTEGER PRIMARY KEY)');
  Exec(FConn, 'CREATE TABLE MCHolderOther (HolderID INTEGER, OtherID INTEGER CHECK (OtherID<>99), '+
              'PRIMARY KEY (HolderID, OtherID))');
  FRegistry.RegisterPersister(TMCHolder, TRTTIPersister);
  FRelations.RegisterRelationship(TMCHolder, TMCChild, 'ParentID');
  FM2M.RegisterManyToMany(TMCHolder, TMCOther, 'MCHolderOther', 'HolderID', 'OtherID', False, 'Others');
end;

procedure TTestManagerContracts2.TestLoadChildrenM2MListRaises;
var
  H: TMCHolder;
  Raised: Boolean;
begin
  SetUpHolder;
  Exec(FConn, 'INSERT INTO MCChild (ID, ParentID) VALUES (1, 1)');
  H:=TMCHolder.Create;
  try
    H.ID:=1;
    Raised:=False;
    try
      FManager.LoadChildren(H, 'Others');
    except
      on E: EOPF do
        Raised:=True;
    end;
    // Before the fix the children were loaded into the many-to-many list
    AssertTrue('Others holds associations', Raised);
    AssertEquals('Nothing loaded into it', 0, H.Others.Count);
  finally
    H.Free;
  end;
end;

procedure TTestManagerContracts2.TestLoadChildrenAnyListStillAccepted;
var
  H: TMCHolder;
begin
  SetUpHolder;
  Exec(FConn, 'INSERT INTO MCChild (ID, ParentID) VALUES (1, 1)');
  H:=TMCHolder.Create;
  try
    H.ID:=1;
    FManager.LoadChildren(H, 'Items');
    AssertEquals('Loaded as before', 1, H.Items.Count);
  finally
    H.Free;
  end;
end;

procedure TTestManagerContracts2.TestLoadChildrenConfigErrorDoesNotDoom;
var
  P: TMCParent;
  L: TStringList;
  I: TMCItem;
begin
  P:=TMCParent.Create;
  L:=TStringList.Create;
  I:=NewItem(1, 'a');
  try
    P.ID:=1;
    FManager.BeginTransaction;
    try
      // No list manipulator for TStringList
      FManager.LoadChildren(P, TMCChild, L);
      Fail('Must raise');
    except
      on E: EOPF do ;
    end;
    FManager.Save(I);
    // Before the fix: "Transaction was previously rolled back"
    FManager.Commit;
    AssertEquals('Saved', 1, Integer(Value(FConn, 'SELECT COUNT(*) FROM MCItem')));
  finally
    I.Free;
    L.Free;
    P.Free;
  end;
end;

procedure TTestManagerContracts2.TestAddAssociationDuplicateDoesNotDoom;
var
  H: TMCHolder;
  O: TMCOther;
  I: TMCItem;
  Msg: String;
begin
  SetUpHolder;
  H:=TMCHolder.Create;
  O:=TMCOther.Create;
  I:=NewItem(1, 'a');
  try
    H.ID:=1;
    O.ID:=2;
    FManager.BeginTransaction;
    FManager.AddAssociation(H, O);
    Msg:='';
    try
      FManager.AddAssociation(H, O);
    except
      on E: EOPF do
        Msg:=E.Message;
    end;
    AssertTrue('Duplicate reported: '+Msg, Pos('already exists', Msg)>0);
    FManager.Save(I);
    FManager.Commit;
    AssertEquals('Association committed', 1, Integer(Value(FConn, 'SELECT COUNT(*) FROM MCHolderOther')));
    AssertEquals('Item committed', 1, Integer(Value(FConn, 'SELECT COUNT(*) FROM MCItem')));
  finally
    I.Free;
    O.Free;
    H.Free;
  end;
end;

procedure TTestManagerContracts2.TestAddAssociationErrorKeepsOriginal;
var
  H: TMCHolder;
  O: TMCOther;
  Msg: String;
begin
  SetUpHolder;
  H:=TMCHolder.Create;
  O:=TMCOther.Create;
  try
    H.ID:=1;
    O.ID:=99;
    Msg:='';
    try
      FManager.AddAssociation(H, O);
    except
      on E: EOPF do
        Msg:=E.Message;
    end;
    // Before the fix every insert error was reported as a duplicate
    AssertTrue('Failed', Msg<>'');
    AssertTrue('Not a duplicate: '+Msg, Pos('already exists', Msg)=0);
    AssertTrue('Original error: '+Msg, Pos('CHECK', UpperCase(Msg))>0);
  finally
    O.Free;
    H.Free;
  end;
end;

procedure TTestManagerContracts2.TestJunctionRowsOnClassAConnection;
var
  R: TMCResolver;
  H: TMCHolder;
  O: TMCOther;
begin
  SetUpHolder;
  // TMCOther lives on the second connection; the junction table with
  // ClassA (TMCHolder) on the first. The join of a load needs TMCOther's
  // table on the first connection as well.
  FConn2:=OpenConn(FTrans2);
  Exec(FConn2, 'CREATE TABLE MCOther (ID INTEGER PRIMARY KEY, Name TEXT)');
  Exec(FConn2, 'CREATE TABLE MCHolderOther (HolderID INTEGER, OtherID INTEGER)');
  Exec(FConn2, 'INSERT INTO MCOther (ID, Name) VALUES (2, ''o'')');
  Exec(FConn, 'INSERT INTO MCOther (ID, Name) VALUES (2, ''o'')');
  R:=TMCResolver.Create;
  H:=TMCHolder.Create;
  O:=TMCOther.Create;
  try
    R.Main:=FConn;
    R.Second:=FConn2;
    FManager.ConnectionResolver:=R;
    H.ID:=1;
    O.ID:=2;
    O.State:=opsClean;
    FManager.AddAssociation(H, O);
    AssertEquals('Junction row on ClassA''s connection', 1,
      Integer(Value(FConn, 'SELECT COUNT(*) FROM MCHolderOther')));
    // Before the fix the load read the junction table of TMCOther's connection
    FManager.LoadAssociations(H, 'Others');
    AssertEquals('Association found', 1, H.Others.Count);
    // Before the fix the junction rows were deleted on TMCOther's connection
    FManager.Delete(O);
    AssertEquals('Junction row deleted', 0,
      Integer(Value(FConn, 'SELECT COUNT(*) FROM MCHolderOther')));
    AssertEquals('Other deleted', 0, Integer(Value(FConn2, 'SELECT COUNT(*) FROM MCOther')));
  finally
    FManager.ConnectionResolver:=Nil;
    O.Free;
    H.Free;
    R.Free;
  end;
end;

procedure TTestManagerContracts2.TestAfterLoadForListItems;
var
  L: TFPObjectList;
begin
  Exec(FConn, 'INSERT INTO MCItem (ID, Name) VALUES (1, ''a'')');
  Exec(FConn, 'INSERT INTO MCItem (ID, Name) VALUES (2, ''b'')');
  FManager.AfterLoad:=@CountAfterLoad;
  L:=TFPObjectList.Create(True);
  try
    FManager.Load(L, TMCItem, '');
    AssertEquals('Two loaded', 2, L.Count);
    // Before the fix AfterLoad was only called for single objects
    AssertEquals('AfterLoad per item', 2, FAfterLoadCount);
    AssertEquals('Clean', Ord(opsClean), Ord(TMCItem(L[0]).State));
  finally
    L.Free;
  end;
end;

procedure TTestManagerContracts2.TestAfterLoadForLoadSQL;
var
  I: TMCItem;
begin
  Exec(FConn, 'INSERT INTO MCItem (ID, Name) VALUES (1, ''a'')');
  FManager.AfterLoad:=@CountAfterLoad;
  I:=TMCItem.Create;
  try
    FManager.LoadSQL(I, 'SELECT ID, Name, UpdatedAt FROM MCItem WHERE ID=1');
    AssertEquals('Loaded', 'a', I.Name);
    AssertEquals('AfterLoad called', 1, FAfterLoadCount);
  finally
    I.Free;
  end;
end;

procedure TTestManagerContracts2.TestChildrenCleanAndTracked;
var
  P: TMCParent;
  N: TUTF8StringArray;
begin
  FTracker:=TOPSnapshotManager.Create(Nil);
  FTracker.Manager:=FManager;
  Exec(FConn, 'INSERT INTO MCChild (ID, ParentID) VALUES (1, 1)');
  P:=TMCParent.Create;
  try
    P.ID:=1;
    FManager.LoadChildren(P, 'Kids');
    AssertEquals('Loaded', 1, P.Kids.Count);
    AssertEquals('Clean', Ord(opsClean), Ord(TMCChild(P.Kids[0]).State));
    // Before the fix children got no reference state: every save wrote all fields
    AssertTrue('Tracked', FManager.GetDirtyFields(P.Kids[0], N));
  finally
    P.Free;
  end;
end;

initialization
  RegisterTest(TTestManagerContracts2);
end.
