unit utc.opf.undo;

{ In-memory object state after a rollback: the database rolls back, so the
  objects must get back what the manager wrote into them (generated key,
  version, audit fields, soft-delete flag, persistence state). }

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, contnrs, DB, sqldb,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Registry, Opf.Manager, Opf.Sql, Opf.Driver,
  Opf.Persister.RTTI, Opf.Snapshot, utc.opf.failconn;

type

  { TUndoItem: observable (TPersistent supports IFPObserved) }

  TUndoItem = class(TPersistent, IOPPersistenceState)
  private
    FCreatedAt: TDateTime;
    FID: Integer;
    FName: String;
    FVersion: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    constructor Create(const aName: String);
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Version: Integer read FVersion write FVersion;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

  { TUndoPlain: not observable }

  TUndoPlain = class(TObject, IOPPersistenceState)
  private
    FID: Integer;
    FName: String;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  { TUndoSoft: soft delete }

  TUndoSoft = class(TPersistent, IOPPersistenceState)
  private
    FDeleted: Boolean;
    FID: Integer;
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  published
    property ID: Integer read FID write FID;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  { TUndoResolver: TUndoSoft on the second connection, the rest on the first }

  TUndoResolver = class(TObject, IOPConnectionResolver)
  public
    Conn1, Conn2: TSQLConnection;
    function ResolveConnection(aClass: TClass): TSQLConnection;
  end;

  { TTestManagerUndo }

  TTestManagerUndo = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FBuilder: TOPSQLBuilder;
    FManager: TOPManager;
    FTracker: TOPSnapshotManager;
    procedure Exec(const aSQL: String);
    function Count(const aTable: String): Integer;
    procedure AttachTracker;
    function IsTracked(aObject: TObject): Boolean;
    function TrySave(aObject: TObject): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Failed operation
    procedure TestFailedInsertKeepsStateNew;
    procedure TestFailedInsertRetrySucceeds;
    procedure TestFailedUpdateRestoresVersion;
    procedure TestFailedUpdateRetryNoFalseConflict;
    procedure TestFailedSaveOfUnobservableRestored;
    // Explicit transactions
    procedure TestExplicitRollbackRestoresObservable;
    procedure TestExplicitRollbackThenResave;
    procedure TestNestedFailureRestoresBoth;
    procedure TestDoomedCommitRestores;
    procedure TestSaveTwiceRollbackRestoresOriginal;
    procedure TestCommitKeepsChanges;
    procedure TestUnobservableFreedBeforeRollbackIsSafe;
    procedure TestObservableFreedBeforeRollbackIsSafe;
    // Lists
    procedure TestSaveListPartialFailureRestoresEarlierItems;
    procedure TestSaveListRetryAfterFailure;
    procedure TestSaveListUnobservableItemsRestored;
    // Delete
    procedure TestDeleteRolledBackRestoresState;
    procedure TestSoftDeleteRolledBackRestoresFlag;
    // Commit failure
    procedure TestFailedOutermostCommitRestores;
    // Snapshots
    procedure TestRollbackDiscardsSnapshotTakenInTransaction;
    procedure TestRollbackKeepsSnapshotTakenBefore;
    procedure TestLoadNotFoundKeepsTracking;
    // Review 2
    procedure TestFailedCommitRestoresUnobservable;
    procedure TestFailedCommitRestoresUnobservableSaveState;
    procedure TestLoadMissDoesNotDoomTransaction;
    procedure TestTryLoadMissing;
    procedure TestReadOnlyRollbackKeepsSnapshots;
    procedure TestPartialCommitKeepsCommittedObjects;
    procedure TestManyObjectsSomeFreedThenRollback;
  end;

implementation

{ TUndoItem }

constructor TUndoItem.Create(const aName: String);
begin
  inherited Create;
  FName:=aName;
  FState:=opsNew;
end;

function TUndoItem.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TUndoItem.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TUndoPlain }

function TUndoPlain.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TUndoPlain.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TUndoSoft }

function TUndoSoft.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TUndoSoft.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TUndoResolver }

function TUndoResolver.ResolveConnection(aClass: TClass): TSQLConnection;
begin
  if aClass=TUndoSoft then
    Result:=Conn2
  else
    Result:=Conn1;
end;

{ TTestManagerUndo }

procedure TTestManagerUndo.Exec(const aSQL: String);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:=aSQL;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

function TTestManagerUndo.Count(const aTable: String): Integer;
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    if not FTrans.Active then
      FTrans.StartTransaction;
    Q.SQL.Text:='SELECT COUNT(*) FROM '+aTable;
    Q.Open;
    Result:=Q.Fields[0].AsInteger;
    Q.Close;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TTestManagerUndo.AttachTracker;
begin
  FTracker:=TOPSnapshotManager.Create(Nil);
  FTracker.Manager:=Nil;
  FTracker.Manager:=FManager;
end;

function TTestManagerUndo.IsTracked(aObject: TObject): Boolean;
var
  N: TUTF8StringArray;
begin
  Result:=FManager.GetDirtyFields(aObject, N);
end;

function TTestManagerUndo.TrySave(aObject: TObject): Boolean;
begin
  Result:=True;
  try
    FManager.Save(aObject);
  except
    on E: Exception do
      Result:=False;
  end;
end;

procedure TTestManagerUndo.SetUp;
var
  C: TOPConnection;
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  inherited SetUp;
  FConn:=TFailingSQLite3Connection.Create(Nil);
  FConn.DatabaseName:=':memory:';
  FTrans:=TSQLTransaction.Create(Nil);
  FTrans.Database:=FConn;
  FConn.Transaction:=FTrans;
  FConn.Open;
  // A CHECK constraint lets a test make a save fail on demand
  Exec('CREATE TABLE UndoItem (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT NOT NULL CHECK (Name<>''bad''), '+
       'Version INTEGER, CreatedAt REAL)');
  Exec('CREATE TABLE UndoPlain (ID INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT NOT NULL CHECK (Name<>''bad''))');
  Exec('CREATE TABLE UndoSoft (ID INTEGER PRIMARY KEY, Deleted BOOLEAN)');
  FRegistry:=TOPRegistry.Create(Nil);
  D:=FRegistry.RegisterPersister(TUndoItem, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options + [foAutoInc, foInKey] - [foInInsert];
  F:=D.FieldMap.FindByPropertyName('Version');
  F.Options:=F.Options + [foVersion];
  F:=D.FieldMap.FindByPropertyName('CreatedAt');
  F.Options:=F.Options + [foCreatedAt];
  D:=FRegistry.RegisterPersister(TUndoPlain, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('ID');
  F.Options:=F.Options + [foAutoInc, foInKey] - [foInInsert];
  D:=FRegistry.RegisterPersister(TUndoSoft, TRTTIPersister);
  F:=D.FieldMap.FindByPropertyName('Deleted');
  F.Options:=F.Options + [foSoftDelete];
  FBuilder:=TOPSQLBuilder.Create(Nil);
  FBuilder.SQLDialect:=dSQLite;
  FBuilder.Registry:=FRegistry;
  FManager:=TOPManager.Create(Nil);
  FManager.Registry:=FRegistry;
  FManager.SQLBuilder:=FBuilder;
  C:=FManager.Connections.Add as TOPConnection;
  C.SingleConnection:=FConn;
  C.IsDefault:=True;
end;

procedure TTestManagerUndo.TearDown;
begin
  while Assigned(FManager) and FManager.InTransaction do
    try
      FManager.Rollback;
    except
      on E: Exception do ;
    end;
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FBuilder);
  FreeAndNil(FRegistry);
  if Assigned(FConn) then
  begin
    FConn.FailCommit:=False;
    if FTrans.Active then
      FTrans.Rollback;
    FConn.Close;
  end;
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
  inherited TearDown;
end;

procedure TTestManagerUndo.TestFailedInsertKeepsStateNew;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('bad');
  try
    AssertFalse('Save fails', TrySave(O));
    AssertEquals('State still new', Ord(opsNew), Ord(O.State));
    AssertEquals('No key', 0, O.ID);
    AssertEquals('Initial version undone', 0, O.Version);
    AssertEquals('Audit timestamp undone', 0, O.CreatedAt, 0);
    AssertEquals('No row', 0, Count('UndoItem'));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedInsertRetrySucceeds;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('bad');
  try
    AssertFalse('First save fails', TrySave(O));
    O.Name:='good';
    AssertTrue('Retry succeeds', TrySave(O));
    AssertEquals('Key', 1, O.ID);
    AssertEquals('Version', 1, O.Version);
    AssertEquals('Clean', Ord(opsClean), Ord(O.State));
    AssertEquals('One row', 1, Count('UndoItem'));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedUpdateRestoresVersion;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('good');
  try
    AssertTrue('Insert', TrySave(O));
    AssertEquals('Version after insert', 1, O.Version);
    O.Name:='bad';
    O.State:=opsUpdate;
    AssertFalse('Update fails', TrySave(O));
    AssertEquals('Version not incremented', 1, O.Version);
    AssertEquals('State still update', Ord(opsUpdate), Ord(O.State));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedUpdateRetryNoFalseConflict;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('good');
  try
    AssertTrue('Insert', TrySave(O));
    O.Name:='bad';
    O.State:=opsUpdate;
    AssertFalse('Update fails', TrySave(O));
    O.Name:='better';
    // Before the fix the object held version 2 while the row still had 1,
    // and every retry raised a concurrency conflict.
    FManager.Save(O);
    AssertEquals('Version after the update', 2, O.Version);
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedSaveOfUnobservableRestored;
var
  O: TUndoPlain;
begin
  O:=TUndoPlain.Create;
  try
    O.Name:='bad';
    O.State:=opsNew;
    AssertFalse('Save fails', TrySave(O));
    AssertEquals('State still new', Ord(opsNew), Ord(O.State));
    AssertEquals('No key', 0, O.ID);
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestExplicitRollbackRestoresObservable;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('a');
  try
    FManager.BeginTransaction;
    FManager.Save(O);
    AssertEquals('Saved in the transaction', Ord(opsClean), Ord(O.State));
    AssertEquals('Key assigned', 1, O.ID);
    FManager.Rollback;
    AssertEquals('State restored', Ord(opsNew), Ord(O.State));
    AssertEquals('Key restored', 0, O.ID);
    AssertEquals('Version restored', 0, O.Version);
    AssertEquals('No row', 0, Count('UndoItem'));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestExplicitRollbackThenResave;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('a');
  try
    FManager.BeginTransaction;
    FManager.Save(O);
    FManager.Rollback;
    FManager.Save(O);
    AssertEquals('Inserted again', 1, Count('UndoItem'));
    AssertEquals('Clean', Ord(opsClean), Ord(O.State));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestNestedFailureRestoresBoth;
var
  A, B: TUndoItem;
begin
  A:=TUndoItem.Create('a');
  B:=TUndoItem.Create('bad');
  try
    FManager.BeginTransaction;
    FManager.Save(A);
    AssertFalse('B fails', TrySave(B));
    AssertEquals('B restored at once', Ord(opsNew), Ord(B.State));
    FManager.Rollback;
    AssertEquals('A restored', Ord(opsNew), Ord(A.State));
    AssertEquals('A key restored', 0, A.ID);
    AssertEquals('No rows', 0, Count('UndoItem'));
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestDoomedCommitRestores;
var
  A, B: TUndoItem;
  Raised: Boolean;
begin
  A:=TUndoItem.Create('a');
  B:=TUndoItem.Create('bad');
  try
    FManager.BeginTransaction;
    FManager.Save(A);
    AssertFalse('B fails', TrySave(B));
    Raised:=False;
    try
      FManager.Commit;
    except
      on EOPFTransaction do
        Raised:=True;
    end;
    AssertTrue('Doomed commit raises', Raised);
    AssertEquals('A restored', Ord(opsNew), Ord(A.State));
    AssertEquals('No rows', 0, Count('UndoItem'));
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestSaveTwiceRollbackRestoresOriginal;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('a');
  try
    FManager.BeginTransaction;
    FManager.Save(O);
    O.Name:='b';
    O.State:=opsUpdate;
    FManager.Save(O);
    AssertEquals('Version 2 in the transaction', 2, O.Version);
    FManager.Rollback;
    AssertEquals('Original version', 0, O.Version);
    AssertEquals('Original state', Ord(opsNew), Ord(O.State));
    AssertEquals('Original key', 0, O.ID);
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestCommitKeepsChanges;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('a');
  try
    FManager.BeginTransaction;
    FManager.Save(O);
    FManager.Commit;
    AssertEquals('Clean', Ord(opsClean), Ord(O.State));
    AssertEquals('Key kept', 1, O.ID);
    AssertEquals('Row', 1, Count('UndoItem'));
    // A later rollback does not touch it
    FManager.BeginTransaction;
    FManager.Rollback;
    AssertEquals('Still clean', Ord(opsClean), Ord(O.State));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestUnobservableFreedBeforeRollbackIsSafe;
var
  O: TUndoPlain;
begin
  FManager.BeginTransaction;
  O:=TUndoPlain.Create;
  try
    O.Name:='a';
    O.State:=opsNew;
    FManager.Save(O);
  finally
    O.Free;
  end;
  FManager.Rollback; // Must not touch the freed object
  AssertEquals('Rolled back', 0, Count('UndoPlain'));
end;

procedure TTestManagerUndo.TestObservableFreedBeforeRollbackIsSafe;
var
  O: TUndoItem;
begin
  FManager.BeginTransaction;
  O:=TUndoItem.Create('a');
  try
    FManager.Save(O);
  finally
    O.Free; // Notifies the undo log
  end;
  FManager.Rollback;
  AssertEquals('Rolled back', 0, Count('UndoItem'));
end;

procedure TTestManagerUndo.TestSaveListPartialFailureRestoresEarlierItems;
var
  L: TFPObjectList;
begin
  L:=TFPObjectList.Create(True);
  try
    L.Add(TUndoItem.Create('a'));
    L.Add(TUndoItem.Create('b'));
    L.Add(TUndoItem.Create('bad'));
    AssertFalse('List save fails', TrySave(L));
    AssertEquals('Item 1 new again', Ord(opsNew), Ord(TUndoItem(L[0]).State));
    AssertEquals('Item 1 no key', 0, TUndoItem(L[0]).ID);
    AssertEquals('Item 2 new again', Ord(opsNew), Ord(TUndoItem(L[1]).State));
    AssertEquals('Item 2 version', 0, TUndoItem(L[1]).Version);
    AssertEquals('No rows', 0, Count('UndoItem'));
  finally
    L.Free;
  end;
end;

procedure TTestManagerUndo.TestSaveListRetryAfterFailure;
var
  L: TFPObjectList;
begin
  L:=TFPObjectList.Create(True);
  try
    L.Add(TUndoItem.Create('a'));
    L.Add(TUndoItem.Create('b'));
    L.Add(TUndoItem.Create('bad'));
    AssertFalse('List save fails', TrySave(L));
    TUndoItem(L[2]).Name:='c';
    AssertTrue('Retry succeeds', TrySave(L));
    // Before the fix items 1 and 2 were left clean and silently skipped
    AssertEquals('All rows', 3, Count('UndoItem'));
    AssertEquals('Distinct keys', 6, TUndoItem(L[0]).ID+TUndoItem(L[1]).ID+TUndoItem(L[2]).ID);
  finally
    L.Free;
  end;
end;

procedure TTestManagerUndo.TestSaveListUnobservableItemsRestored;
var
  L: TFPObjectList;
  P: TUndoPlain;
  I: Integer;
begin
  L:=TFPObjectList.Create(True);
  try
    for I:=1 to 3 do
    begin
      P:=TUndoPlain.Create;
      P.State:=opsNew;
      if I=3 then
        P.Name:='bad'
      else
        P.Name:='p'+IntToStr(I);
      L.Add(P);
    end;
    AssertFalse('List save fails', TrySave(L));
    AssertEquals('Item 1 new again', Ord(opsNew), Ord(TUndoPlain(L[0]).State));
    AssertEquals('Item 2 no key', 0, TUndoPlain(L[1]).ID);
  finally
    L.Free;
  end;
end;

procedure TTestManagerUndo.TestDeleteRolledBackRestoresState;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('a');
  try
    FManager.Save(O);
    FManager.BeginTransaction;
    FManager.Delete(O);
    AssertEquals('Deleted in the transaction', Ord(opsDeleted), Ord(O.State));
    FManager.Rollback;
    AssertEquals('State restored', Ord(opsClean), Ord(O.State));
    AssertEquals('Row still there', 1, Count('UndoItem'));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestSoftDeleteRolledBackRestoresFlag;
var
  O: TUndoSoft;
begin
  Exec('INSERT INTO UndoSoft (ID, Deleted) VALUES (1, 0)');
  O:=TUndoSoft.Create;
  try
    O.ID:=1;
    O.State:=opsClean;
    FManager.BeginTransaction;
    FManager.Delete(O);
    AssertTrue('Flag set in the transaction', O.Deleted);
    FManager.Rollback;
    AssertFalse('Flag restored', O.Deleted);
    AssertEquals('State restored', Ord(opsClean), Ord(O.State));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedOutermostCommitRestores;
var
  O: TUndoItem;
begin
  O:=TUndoItem.Create('a');
  try
    FConn.FailCommit:=True;
    AssertFalse('Commit fails', TrySave(O));
    FConn.FailCommit:=False;
    AssertEquals('State restored', Ord(opsNew), Ord(O.State));
    AssertEquals('Key restored', 0, O.ID);
    AssertEquals('No row', 0, Count('UndoItem'));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestRollbackDiscardsSnapshotTakenInTransaction;
var
  O: TUndoItem;
begin
  AttachTracker;
  O:=TUndoItem.Create('a');
  try
    FManager.Save(O);
    AssertTrue('Tracked after save', IsTracked(O));
    FManager.BeginTransaction;
    O.Name:='b';
    O.State:=opsUpdate;
    FManager.Save(O);
    FManager.Rollback;
    // The snapshot taken in the transaction holds 'b', the database 'a'
    AssertFalse('Snapshot of the rolled back save discarded', IsTracked(O));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestRollbackKeepsSnapshotTakenBefore;
var
  A, B: TUndoItem;
begin
  AttachTracker;
  A:=TUndoItem.Create('a');
  B:=TUndoItem.Create('b');
  try
    FManager.Save(A);
    FManager.Save(B);
    FManager.BeginTransaction;
    B.Name:='c';
    B.State:=opsUpdate;
    FManager.Save(B);
    FManager.Rollback;
    AssertTrue('Untouched object keeps its snapshot', IsTracked(A));
    AssertFalse('Touched object loses its snapshot', IsTracked(B));
  finally
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestLoadNotFoundKeepsTracking;
var
  A, Missing: TUndoItem;
begin
  AttachTracker;
  A:=TUndoItem.Create('a');
  Missing:=TUndoItem.Create('');
  try
    FManager.Save(A);
    Missing.ID:=999;
    try
      FManager.Load(Missing);
      Fail('Load of a missing row must raise');
    except
      on EOPF do ;
    end;
    // Before the fix any failed operation purged every snapshot
    AssertTrue('Earlier object still tracked', IsTracked(A));
  finally
    Missing.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedCommitRestoresUnobservable;
var
  O: TUndoPlain;
begin
  O:=TUndoPlain.Create;
  try
    O.Name:='a';
    O.State:=opsNew;
    FConn.FailCommit:=True;
    // Before the fix the entry was dropped before the real COMMIT: the object
    // kept the generated key and state clean, and the next save skipped it
    AssertFalse('Commit fails', TrySave(O));
    FConn.FailCommit:=False;
    AssertEquals('State restored', Ord(opsNew), Ord(O.State));
    AssertEquals('Key restored', 0, O.ID);
    AssertTrue('Retry succeeds', TrySave(O));
    AssertEquals('Row inserted', 1, Count('UndoPlain'));
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestFailedCommitRestoresUnobservableSaveState;
var
  O: TUndoPlain;
  Failed: Boolean;
begin
  O:=TUndoPlain.Create;
  try
    O.Name:='a';
    O.State:=opsNew;
    FConn.FailCommit:=True;
    Failed:=False;
    try
      FManager.Save(O, opsNew);
    except
      on E: Exception do
        Failed:=True;
    end;
    FConn.FailCommit:=False;
    AssertTrue('Commit fails', Failed);
    AssertEquals('State restored', Ord(opsNew), Ord(O.State));
    AssertEquals('Key restored', 0, O.ID);
  finally
    O.Free;
  end;
end;

procedure TTestManagerUndo.TestLoadMissDoesNotDoomTransaction;
var
  A, B, Missing: TUndoItem;
begin
  A:=TUndoItem.Create('a');
  B:=TUndoItem.Create('b');
  Missing:=TUndoItem.Create('');
  try
    FManager.BeginTransaction;
    FManager.Save(A);
    Missing.ID:=999;
    try
      FManager.Load(Missing, True);
      Fail('Load of a missing row must raise');
    except
      on E: EOPF do ;
    end;
    FManager.Save(B);
    // Before the fix: "Transaction was previously rolled back"
    FManager.Commit;
    AssertEquals('Both rows committed', 2, Count('UndoItem'));
    AssertEquals('A clean', Ord(opsClean), Ord(A.State));
  finally
    Missing.Free;
    B.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestTryLoadMissing;
var
  A, Missing: TUndoPlain;
begin
  A:=TUndoPlain.Create;
  Missing:=TUndoPlain.Create;
  try
    A.Name:='a';
    A.State:=opsNew;
    Missing.Name:='x';
    Missing.State:=opsNew;
    FManager.Save(A);
    Missing.ID:=999;
    FManager.BeginTransaction;
    AssertFalse('Missing row', FManager.TryLoad(Missing));
    AssertEquals('Object unchanged', 'x', Missing.Name);
    AssertEquals('State unchanged', Ord(opsNew), Ord(Missing.State));
    Missing.ID:=A.ID;
    AssertTrue('Existing row', FManager.TryLoad(Missing));
    AssertEquals('Loaded', 'a', Missing.Name);
    FManager.Commit;
  finally
    Missing.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestReadOnlyRollbackKeepsSnapshots;
var
  A: TUndoItem;
  P: TUndoPlain;
begin
  AttachTracker;
  Exec('INSERT INTO UndoPlain (ID, Name) VALUES (1, ''p'')');
  A:=TUndoItem.Create('a');
  P:=TUndoPlain.Create;
  try
    FManager.Save(A);
    AssertTrue('Tracked after save', IsTracked(A));
    FManager.BeginTransaction;
    P.ID:=1;
    // An unobservable object gets a snapshot inside the transaction
    FManager.Load(P);
    FManager.Rollback;
    // Nothing was written: the database is as before, no snapshot is wrong.
    // Before the fix every snapshot was purged.
    AssertTrue('Earlier snapshot kept', IsTracked(A));
  finally
    P.Free;
    A.Free;
  end;
end;

procedure TTestManagerUndo.TestPartialCommitKeepsCommittedObjects;
var
  Conn2: TFailingSQLite3Connection;
  Trans2: TSQLTransaction;
  R: TUndoResolver;
  A: TUndoItem;
  B: TUndoSoft;
  Raised: Boolean;
  Q: TSQLQuery;
begin
  Conn2:=TFailingSQLite3Connection.Create(Nil);
  Trans2:=TSQLTransaction.Create(Nil);
  R:=TUndoResolver.Create;
  A:=TUndoItem.Create('a');
  B:=TUndoSoft.Create;
  try
    Conn2.DatabaseName:=':memory:';
    Trans2.Database:=Conn2;
    Conn2.Transaction:=Trans2;
    Conn2.Open;
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=Conn2;
      Q.Transaction:=Trans2;
      Trans2.StartTransaction;
      Q.SQL.Text:='CREATE TABLE UndoSoft (ID INTEGER PRIMARY KEY, Deleted BOOLEAN)';
      Q.ExecSQL;
      Trans2.Commit;
    finally
      Q.Free;
    end;
    R.Conn1:=FConn;
    R.Conn2:=Conn2;
    FManager.ConnectionResolver:=R;
    B.ID:=5;
    B.State:=opsNew;
    FManager.BeginTransaction;
    FManager.Save(A); // First connection: committed first
    FManager.Save(B); // Second connection: its commit fails
    Conn2.FailCommit:=True;
    Raised:=False;
    try
      FManager.Commit;
    except
      on EOPFPartialCommit do
        Raised:=True;
    end;
    Conn2.FailCommit:=False;
    AssertTrue('Partial commit', Raised);
    AssertEquals('A committed', 1, Count('UndoItem'));
    // Before the fix A was reset to new without key: a retry inserted it twice
    AssertEquals('A keeps state clean', Ord(opsClean), Ord(A.State));
    AssertEquals('A keeps its key', 1, A.ID);
    AssertEquals('B restored', Ord(opsNew), Ord(B.State));
  finally
    FManager.ConnectionResolver:=Nil;
    B.Free;
    A.Free;
    R.Free;
    if Trans2.Active then
      Trans2.Rollback;
    Conn2.Close;
    Trans2.Free;
    Conn2.Free;
  end;
end;

procedure TTestManagerUndo.TestManyObjectsSomeFreedThenRollback;
const
  N = 2000;
var
  L: array of TUndoItem;
  I: Integer;
begin
  L:=Nil;
  SetLength(L, N);
  for I:=0 to N-1 do
    L[I]:=TUndoItem.Create('n'+IntToStr(I));
  try
    FManager.BeginTransaction;
    for I:=0 to N-1 do
      FManager.Save(L[I]);
    // Save half of them again: two undo entries per object
    for I:=0 to N-1 do
      if I mod 4=1 then
      begin
        L[I].State:=opsUpdate;
        FManager.Save(L[I]);
      end;
    for I:=0 to N-1 do
      if Odd(I) then
        FreeAndNil(L[I]);
    FManager.Rollback;
    for I:=0 to N-1 do
      if Assigned(L[I]) then
      begin
        AssertEquals('Restored state '+IntToStr(I), Ord(opsNew), Ord(L[I].State));
        AssertEquals('Restored key '+IntToStr(I), 0, L[I].ID);
        AssertEquals('Restored version '+IntToStr(I), 0, L[I].Version);
      end;
    AssertEquals('No rows', 0, Count('UndoItem'));
  finally
    for I:=0 to N-1 do
      L[I].Free;
  end;
end;

initialization
  RegisterTest(TTestManagerUndo);
end.
