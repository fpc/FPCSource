unit utc.opf.deletecontracts;

{ Tests for Delete:
  - cascade over a relation of a class to itself (trees)
  - soft-delete cascade: soft parent only soft-deletes, hard parent hard-deletes
  - already soft-deleted rows keep their deletion value
  - version and rows-affected checks (DeleteChecks)
  - composite keys: soft delete binds all key columns, cascades refuse
  - database error details on EOPF
  - sequences: preset keys kept, out of range values raise }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, Data.Db, Data.Sqldb,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, Contnrs, DB, sqldb, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Driver, Opf.Sql, Opf.Registry, Opf.Relations,
  Opf.M2M, Opf.Manager, Opf.Persister.RTTI, utc.opf.failconn;

type

  { TDCBase: persistence state for all fixtures }

  TDCBase = class(TPersistent, IOPPersistenceState)
  private
    FState: TObjectPersistenceState;
    function GetPersistenceState: TObjectPersistenceState;
    procedure SetPersistenceState(aValue: TObjectPersistenceState);
  public
    property State: TObjectPersistenceState read FState write FState;
  end;

  // A tree
  TDCNode = class(TDCBase)
  private
    FID, FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
  end;

  // Soft parent with a soft child, a soft grandchild and a hard child
  TDCSoftParent = class(TDCBase)
  private
    FID: Integer;
    FDeletedAt: TDateTime;
  published
    property ID: Integer read FID write FID;
    property DeletedAt: TDateTime read FDeletedAt write FDeletedAt;
  end;

  TDCSoftChild = class(TDCBase)
  private
    FID, FParentID: Integer;
    FDeleted: Boolean;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TDCSoftGrand = class(TDCBase)
  private
    FID, FChildID: Integer;
    FDeletedAt: TDateTime;
  published
    property ID: Integer read FID write FID;
    property ChildID: Integer read FChildID write FChildID;
    property DeletedAt: TDateTime read FDeletedAt write FDeletedAt;
  end;

  TDCHardChild = class(TDCBase)
  private
    FID, FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
  end;

  // Hard parent with a child that has a soft-delete field
  TDCHardParent = class(TDCBase)
  private
    FID: Integer;
  published
    property ID: Integer read FID write FID;
  end;

  TDCSoftOfHard = class(TDCBase)
  private
    FID, FParentID: Integer;
    FDeleted: Boolean;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TDCVersioned = class(TDCBase)
  private
    FID, FVersion: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Version: Integer read FVersion write FVersion;
  end;

  TDCPlain = class(TDCBase)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  TDCComposite = class(TDCBase)
  private
    FA, FB: Integer;
    FDeleted: Boolean;
  published
    property DCCompositeID: Integer read FA write FA;
    property B: Integer read FB write FB;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TDCCompParent = class(TDCBase)
  private
    FA, FB: Integer;
  published
    property DCCompParentID: Integer read FA write FA;
    property B: Integer read FB write FB;
  end;

  TDCCompKid = class(TDCBase)
  private
    FID, FCompA: Integer;
  published
    property ID: Integer read FID write FID;
    property CompA: Integer read FCompA write FCompA;
  end;

  TDCSeq = class(TDCBase)
  private
    FID: Integer;
    FName: String;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
  end;

  // A tree with string keys
  TDCSNode = class(TDCBase)
  private
    FID, FParentID: String;
  published
    property ID: String read FID write FID;
    property ParentID: String read FParentID write FParentID;
  end;

  // Versioned parent with a child
  TDCVerParent = class(TDCBase)
  private
    FID, FVersion: Integer;
  published
    property ID: Integer read FID write FID;
    property Version: Integer read FVersion write FVersion;
  end;

  TDCVerKid = class(TDCBase)
  private
    FID, FParentID: Integer;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
  end;

  // Soft parent with a soft child cascaded by the database (DBCascade)
  TDCSoftDBParent = class(TDCBase)
  private
    FID: Integer;
    FDeleted: Boolean;
  published
    property ID: Integer read FID write FID;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TDCSoftDBKid = class(TDCBase)
  private
    FID, FParentID: Integer;
    FDeleted: Boolean;
  published
    property ID: Integer read FID write FID;
    property ParentID: Integer read FParentID write FParentID;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  { TDCSeqBuilder: a sequence on SQLite, returning SeqValue }

  TDCSeqBuilder = class(TOPSQLBuilder)
  public
    SeqValue: Int64;
    function GetSequenceNextValSQL(const aSequenceName: UTF8String): UTF8String; override;
  end;

  { TTestDeleteContracts }

  TTestDeleteContracts = class(TTestCase)
  private
    FConn: TFailingSQLite3Connection;
    FTrans: TSQLTransaction;
    FRegistry: TOPRegistry;
    FRelations: TOPRelationRegistry;
    FM2M: TOPManyToManyRegistry;
    FBuilder: TDCSeqBuilder;
    FManager: TOPManager;
    procedure Exec(const aSQL: String);
    function Count(const aTable: String; const aWhere: String = ''): Integer;
    function Value(const aSQL: String): Variant;
    function Reg(aClass: TClass; const aKey: String = 'ID'): TPersisterDef;
    function NewNode(aID, aParent: Integer): TDCNode;
    procedure DoDelete(aObj: TObject);
    function DeleteRaises(aObj: TObject; out aMsg: String): Boolean;
    function LoadSoftChildCount(const aCriteria: UTF8String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // Trees
    procedure TestTreeLeaf;
    procedure TestTreeThreeLevels;
    procedure TestTreeMiddleNode;
    procedure TestTreeSoftDelete;
    // Soft-delete cascade
    procedure TestSoftParentSoftDeletesSoftChildren;
    procedure TestSoftParentLeavesHardChildren;
    procedure TestSoftParentReachesSoftGrandchildren;
    procedure TestHardParentHardDeletesSoftChildren;
    procedure TestSoftDeleteKeepsEarlierTimestamp;
    procedure TestRawCriteriaLoadFiltersSoftDeleted;
    procedure TestRawCriteriaLoadUnfilteredWhenDisabled;
    // Delete checks
    procedure TestStaleVersionRaises;
    procedure TestStaleVersionKeepsRow;
    procedure TestCurrentVersionDeletes;
    procedure TestMissingRowRaises;
    procedure TestSecondSoftDeleteRaises;
    procedure TestNoChecksDeletesMissingSilently;
    procedure TestVersionCheckOnlyIsSilent;
    procedure TestVersionCheckOnlyStaleKeepsChildren;
    procedure TestVersionCheckOnlyCurrentCascades;
    procedure TestRowsCheckOnlyIgnoresVersion;
    // Review 2
    procedure TestTreeStringKeysCaseSensitive;
    procedure TestSoftDeleteFollowsDBCascadeRelation;
    procedure TestHardDeleteLeavesDBCascadeRelation;
    procedure TestNilPersisterClassClearError;
    // Composite keys
    procedure TestCompositeSoftDeleteBindsAllKeys;
    procedure TestCompositeWithRelationRaises;
    // Error details
    procedure TestErrorCodeFromDatabase;
    // Sequences
    procedure TestSequencePresetKeyKept;
    procedure TestSequenceAssignsKey;
    procedure TestSequenceOutOfRangeRaises;
  end;

implementation

{ TDCBase }

function TDCBase.GetPersistenceState: TObjectPersistenceState;
begin
  Result:=FState;
end;

procedure TDCBase.SetPersistenceState(aValue: TObjectPersistenceState);
begin
  FState:=aValue;
end;

{ TDCSeqBuilder }

function TDCSeqBuilder.GetSequenceNextValSQL(const aSequenceName: UTF8String): UTF8String;
begin
  Result:='SELECT '+IntToStr(SeqValue);
end;

{ TTestDeleteContracts }

procedure TTestDeleteContracts.Exec(const aSQL: String);
begin
  if FTrans.Active then
    FTrans.Commit;
  FConn.ExecuteDirect(aSQL);
  FTrans.Commit;
end;

function TTestDeleteContracts.Value(const aSQL: String): Variant;
var
  Q: TSQLQuery;
begin
  if FTrans.Active then
    FTrans.Commit;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=FConn;
    Q.Transaction:=FTrans;
    Q.SQL.Text:=aSQL;
    Q.Open;
    Result:=Q.Fields[0].Value;
    Q.Close;
  finally
    Q.Free;
  end;
  FTrans.Commit;
end;

function TTestDeleteContracts.Count(const aTable: String; const aWhere: String): Integer;
var
  S: String;
begin
  S:='SELECT COUNT(*) FROM '+aTable;
  if aWhere<>'' then
    S:=S+' WHERE '+aWhere;
  Result:=Value(S);
end;

function TTestDeleteContracts.Reg(aClass: TClass; const aKey: String): TPersisterDef;
var
  F: TOPFieldMap;
begin
  Result:=FRegistry.RegisterPersister(aClass, TRTTIPersister);
  F:=Result.FieldMap.FindByPropertyName(aKey);
  F.Options:=F.Options+[foInKey];
end;

function TTestDeleteContracts.NewNode(aID, aParent: Integer): TDCNode;
begin
  Exec(Format('INSERT INTO DCNode (ID, ParentID) VALUES (%d, %d)', [aID, aParent]));
  Result:=TDCNode.Create;
  Result.ID:=aID;
  Result.ParentID:=aParent;
  Result.State:=opsClean;
end;

procedure TTestDeleteContracts.DoDelete(aObj: TObject);
begin
  FManager.Delete(aObj);
end;

function TTestDeleteContracts.DeleteRaises(aObj: TObject; out aMsg: String): Boolean;
begin
  Result:=False;
  aMsg:='';
  try
    FManager.Delete(aObj);
  except
    on E: EOPF do
    begin
      Result:=True;
      aMsg:=E.Message;
    end;
  end;
end;

procedure TTestDeleteContracts.SetUp;
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
  Exec('CREATE TABLE DCNode (ID INTEGER PRIMARY KEY, ParentID INTEGER)');
  Exec('CREATE TABLE DCSoftParent (ID INTEGER PRIMARY KEY, DeletedAt TIMESTAMP)');
  Exec('CREATE TABLE DCSoftChild (ID INTEGER PRIMARY KEY, ParentID INTEGER, Deleted BOOLEAN)');
  Exec('CREATE TABLE DCSoftGrand (ID INTEGER PRIMARY KEY, ChildID INTEGER, DeletedAt TIMESTAMP)');
  Exec('CREATE TABLE DCHardChild (ID INTEGER PRIMARY KEY, ParentID INTEGER)');
  Exec('CREATE TABLE DCHardParent (ID INTEGER PRIMARY KEY)');
  Exec('CREATE TABLE DCSoftOfHard (ID INTEGER PRIMARY KEY, ParentID INTEGER, Deleted BOOLEAN)');
  Exec('CREATE TABLE DCVersioned (ID INTEGER PRIMARY KEY, Name TEXT, Version INTEGER)');
  Exec('CREATE TABLE DCPlain (ID INTEGER PRIMARY KEY, Name TEXT CHECK (Name<>''bad''))');
  Exec('CREATE TABLE DCComposite (DCCompositeID INTEGER, B INTEGER, Deleted BOOLEAN, PRIMARY KEY (DCCompositeID, B))');
  Exec('CREATE TABLE DCCompParent (DCCompParentID INTEGER, B INTEGER, PRIMARY KEY (DCCompParentID, B))');
  Exec('CREATE TABLE DCCompKid (ID INTEGER PRIMARY KEY, CompA INTEGER)');
  Exec('CREATE TABLE DCSeq (ID INTEGER PRIMARY KEY, Name TEXT)');
  Exec('CREATE TABLE DCSNode (ID TEXT PRIMARY KEY, ParentID TEXT)');
  Exec('CREATE TABLE DCVerParent (ID INTEGER PRIMARY KEY, Version INTEGER)');
  Exec('CREATE TABLE DCVerKid (ID INTEGER PRIMARY KEY, ParentID INTEGER)');
  Exec('CREATE TABLE DCSoftDBParent (ID INTEGER PRIMARY KEY, Deleted BOOLEAN)');
  Exec('CREATE TABLE DCSoftDBKid (ID INTEGER PRIMARY KEY, ParentID INTEGER, Deleted BOOLEAN)');
  FRegistry:=TOPRegistry.Create(Nil);
  FRelations:=TOPRelationRegistry.Create(Nil);
  FRelations.Registry:=FRegistry;
  FM2M:=TOPManyToManyRegistry.Create(Nil);
  FM2M.Registry:=FRegistry;
  Reg(TDCNode);
  FRelations.RegisterRelationship(TDCNode, TDCNode, 'ParentID');
  D:=Reg(TDCSoftParent);
  D.FieldMap.FindByPropertyName('DeletedAt').Options:=D.FieldMap.FindByPropertyName('DeletedAt').Options+[foSoftDelete];
  D:=Reg(TDCSoftChild);
  D.FieldMap.FindByPropertyName('Deleted').Options:=D.FieldMap.FindByPropertyName('Deleted').Options+[foSoftDelete];
  D:=Reg(TDCSoftGrand);
  D.FieldMap.FindByPropertyName('DeletedAt').Options:=D.FieldMap.FindByPropertyName('DeletedAt').Options+[foSoftDelete];
  Reg(TDCHardChild);
  FRelations.RegisterRelationship(TDCSoftParent, TDCSoftChild, 'ParentID');
  FRelations.RegisterRelationship(TDCSoftChild, TDCSoftGrand, 'ChildID');
  FRelations.RegisterRelationship(TDCSoftParent, TDCHardChild, 'ParentID');
  Reg(TDCHardParent);
  D:=Reg(TDCSoftOfHard);
  D.FieldMap.FindByPropertyName('Deleted').Options:=D.FieldMap.FindByPropertyName('Deleted').Options+[foSoftDelete];
  FRelations.RegisterRelationship(TDCHardParent, TDCSoftOfHard, 'ParentID');
  D:=Reg(TDCVersioned);
  D.FieldMap.FindByPropertyName('Version').Options:=D.FieldMap.FindByPropertyName('Version').Options+[foVersion];
  Reg(TDCPlain);
  D:=Reg(TDCComposite, 'DCCompositeID');
  D.FieldMap.FindByPropertyName('B').Options:=D.FieldMap.FindByPropertyName('B').Options+[foInKey];
  D.FieldMap.FindByPropertyName('Deleted').Options:=D.FieldMap.FindByPropertyName('Deleted').Options+[foSoftDelete];
  D:=Reg(TDCCompParent, 'DCCompParentID');
  D.FieldMap.FindByPropertyName('B').Options:=D.FieldMap.FindByPropertyName('B').Options+[foInKey];
  Reg(TDCCompKid);
  FRelations.RegisterRelationship(TDCCompParent, TDCCompKid, 'CompA');
  D:=Reg(TDCSeq);
  D.FieldMap.FindByPropertyName('ID').SequenceName:='dc_seq';
  Reg(TDCSNode);
  FRelations.RegisterRelationship(TDCSNode, TDCSNode, 'ParentID');
  D:=Reg(TDCVerParent);
  D.FieldMap.FindByPropertyName('Version').Options:=D.FieldMap.FindByPropertyName('Version').Options+[foVersion];
  Reg(TDCVerKid);
  FRelations.RegisterRelationship(TDCVerParent, TDCVerKid, 'ParentID');
  D:=Reg(TDCSoftDBParent);
  D.FieldMap.FindByPropertyName('Deleted').Options:=D.FieldMap.FindByPropertyName('Deleted').Options+[foSoftDelete];
  D:=Reg(TDCSoftDBKid);
  D.FieldMap.FindByPropertyName('Deleted').Options:=D.FieldMap.FindByPropertyName('Deleted').Options+[foSoftDelete];
  FRelations.RegisterRelationship(TDCSoftDBParent, TDCSoftDBKid, 'ParentID', True);
  FBuilder:=TDCSeqBuilder.Create(Nil);
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

procedure TTestDeleteContracts.TearDown;
begin
  while Assigned(FManager) and FManager.InTransaction do
    try
      FManager.Rollback;
    except
      on E: Exception do ;
    end;
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

{ Trees }

procedure TTestDeleteContracts.TestTreeLeaf;
var
  N: TDCNode;
begin
  NewNode(1, 0).Free;
  N:=NewNode(2, 1);
  try
    FManager.Delete(N);
  finally
    N.Free;
  end;
  AssertEquals('Only the leaf deleted', 1, Count('DCNode'));
end;

procedure TTestDeleteContracts.TestTreeThreeLevels;
var
  N: TDCNode;
begin
  N:=NewNode(1, 0);
  NewNode(2, 1).Free;
  NewNode(3, 1).Free;
  NewNode(4, 2).Free;
  NewNode(5, 4).Free;
  NewNode(6, 0).Free;
  try
    FManager.Delete(N);
  finally
    N.Free;
  end;
  AssertEquals('Only the other root remains', 1, Count('DCNode'));
  AssertEquals('Other root', 6, Integer(Value('SELECT ID FROM DCNode')));
end;

procedure TTestDeleteContracts.TestTreeMiddleNode;
var
  N: TDCNode;
begin
  NewNode(1, 0).Free;
  N:=NewNode(2, 1);
  NewNode(3, 2).Free;
  NewNode(4, 3).Free;
  NewNode(5, 1).Free;
  try
    FManager.Delete(N);
  finally
    N.Free;
  end;
  AssertEquals('Root and sibling remain', 2, Count('DCNode'));
  AssertEquals('Subtree gone', 0, Count('DCNode', 'ID IN (2,3,4)'));
end;

procedure TTestDeleteContracts.TestTreeSoftDelete;
var
  P: TDCSoftParent;
begin
  // A soft parent: its tree-less children are cascaded, see the soft tests.
  // A soft tree uses the same code path as a hard one: the per-row update.
  Exec('INSERT INTO DCSoftParent (ID) VALUES (1)');
  P:=TDCSoftParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
    AssertTrue('Parent soft-deleted', P.DeletedAt>0);
  finally
    P.Free;
  end;
  AssertEquals('Row kept', 1, Count('DCSoftParent'));
  AssertEquals('Marked', 1, Count('DCSoftParent', 'DeletedAt IS NOT NULL'));
end;

{ Soft-delete cascade }

procedure TTestDeleteContracts.TestSoftParentSoftDeletesSoftChildren;
var
  P: TDCSoftParent;
begin
  Exec('INSERT INTO DCSoftParent (ID) VALUES (1)');
  Exec('INSERT INTO DCSoftChild (ID, ParentID, Deleted) VALUES (10, 1, 0)');
  Exec('INSERT INTO DCSoftChild (ID, ParentID, Deleted) VALUES (11, 1, 0)');
  P:=TDCSoftParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Children kept', 2, Count('DCSoftChild'));
  AssertEquals('Children marked', 2, Count('DCSoftChild', 'Deleted=1'));
end;

procedure TTestDeleteContracts.TestSoftParentLeavesHardChildren;
var
  P: TDCSoftParent;
begin
  Exec('INSERT INTO DCSoftParent (ID) VALUES (1)');
  Exec('INSERT INTO DCHardChild (ID, ParentID) VALUES (20, 1)');
  P:=TDCSoftParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Child without soft-delete field left alone', 1, Count('DCHardChild'));
end;

procedure TTestDeleteContracts.TestSoftParentReachesSoftGrandchildren;
var
  P: TDCSoftParent;
begin
  Exec('INSERT INTO DCSoftParent (ID) VALUES (1)');
  Exec('INSERT INTO DCSoftChild (ID, ParentID, Deleted) VALUES (10, 1, 0)');
  Exec('INSERT INTO DCSoftGrand (ID, ChildID) VALUES (100, 10)');
  P:=TDCSoftParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Grandchild kept', 1, Count('DCSoftGrand'));
  AssertEquals('Grandchild marked', 1, Count('DCSoftGrand', 'DeletedAt IS NOT NULL'));
end;

procedure TTestDeleteContracts.TestHardParentHardDeletesSoftChildren;
var
  P: TDCHardParent;
begin
  Exec('INSERT INTO DCHardParent (ID) VALUES (1)');
  Exec('INSERT INTO DCSoftOfHard (ID, ParentID, Deleted) VALUES (30, 1, 0)');
  P:=TDCHardParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Parent gone', 0, Count('DCHardParent'));
  AssertEquals('No orphan left', 0, Count('DCSoftOfHard'));
end;

function TTestDeleteContracts.LoadSoftChildCount(const aCriteria: UTF8String): Integer;
var
  L: TObjectList;
begin
  Exec('INSERT INTO DCSoftChild (ID, ParentID, Deleted) VALUES (10, 1, 0)');
  Exec('INSERT INTO DCSoftChild (ID, ParentID, Deleted) VALUES (11, 1, 1)');
  L:=TObjectList.Create(True);
  try
    FManager.Load(L, TDCSoftChild, aCriteria);
    Result:=L.Count;
  finally
    L.Free;
  end;
end;

procedure TTestDeleteContracts.TestRawCriteriaLoadFiltersSoftDeleted;
begin
  AssertTrue('Filter on by default', omoRawCriteriaSoftDeleteFilter in FManager.Options);
  AssertEquals('Soft-deleted row excluded', 1, LoadSoftChildCount('ParentID=1'));
end;

procedure TTestDeleteContracts.TestRawCriteriaLoadUnfilteredWhenDisabled;
begin
  FManager.Options:=FManager.Options-[omoRawCriteriaSoftDeleteFilter];
  AssertEquals('Text used as-is', 2, LoadSoftChildCount('ParentID=1'));
end;

procedure TTestDeleteContracts.TestSoftDeleteKeepsEarlierTimestamp;
var
  P: TDCSoftParent;
begin
  Exec('INSERT INTO DCSoftParent (ID) VALUES (1)');
  Exec('INSERT INTO DCSoftChild (ID, ParentID, Deleted) VALUES (10, 1, 0)');
  Exec('INSERT INTO DCSoftGrand (ID, ChildID, DeletedAt) VALUES (100, 10, ''2001-01-01 00:00:00.000'')');
  P:=TDCSoftParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Earlier deletion kept', EncodeDate(2001, 1, 1),
    TDateTime(Value('SELECT DeletedAt FROM DCSoftGrand WHERE ID=100')), 1e-6);
end;

{ Delete checks }

procedure TTestDeleteContracts.TestStaleVersionRaises;
var
  V: TDCVersioned;
  Msg: String;
begin
  Exec('INSERT INTO DCVersioned (ID, Name, Version) VALUES (1, ''a'', 2)');
  V:=TDCVersioned.Create;
  try
    V.ID:=1;
    V.Version:=1;
    V.State:=opsClean;
    AssertTrue('Raises', DeleteRaises(V, Msg));
    AssertTrue('Conflict: '+Msg, Pos('concurrency', LowerCase(Msg))>0);
    AssertTrue('Object not marked deleted', V.State<>opsDeleted);
  finally
    V.Free;
  end;
end;

procedure TTestDeleteContracts.TestStaleVersionKeepsRow;
var
  V: TDCVersioned;
  Msg: String;
begin
  Exec('INSERT INTO DCVersioned (ID, Name, Version) VALUES (1, ''a'', 2)');
  V:=TDCVersioned.Create;
  try
    V.ID:=1;
    V.Version:=1;
    V.State:=opsClean;
    DeleteRaises(V, Msg);
  finally
    V.Free;
  end;
  AssertEquals('Row kept', 1, Count('DCVersioned'));
end;

procedure TTestDeleteContracts.TestCurrentVersionDeletes;
var
  V: TDCVersioned;
begin
  Exec('INSERT INTO DCVersioned (ID, Name, Version) VALUES (1, ''a'', 2)');
  V:=TDCVersioned.Create;
  try
    V.ID:=1;
    V.Version:=2;
    V.State:=opsClean;
    FManager.Delete(V);
    AssertTrue(V.State=opsDeleted);
  finally
    V.Free;
  end;
  AssertEquals(0, Count('DCVersioned'));
end;

procedure TTestDeleteContracts.TestMissingRowRaises;
var
  P: TDCPlain;
  Msg: String;
begin
  P:=TDCPlain.Create;
  try
    P.ID:=99;
    P.State:=opsClean;
    AssertTrue('Raises', DeleteRaises(P, Msg));
    AssertTrue('Not found: '+Msg, Pos('does not exist', Msg)>0);
  finally
    P.Free;
  end;
end;

procedure TTestDeleteContracts.TestSecondSoftDeleteRaises;
var
  P: TDCSoftParent;
  Msg: String;
begin
  Exec('INSERT INTO DCSoftParent (ID) VALUES (1)');
  P:=TDCSoftParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
    P.State:=opsClean;
    AssertTrue('Already deleted', DeleteRaises(P, Msg));
  finally
    P.Free;
  end;
end;

procedure TTestDeleteContracts.TestNoChecksDeletesMissingSilently;
var
  P: TDCPlain;
begin
  FManager.DeleteChecks:=[];
  P:=TDCPlain.Create;
  try
    P.ID:=99;
    P.State:=opsClean;
    FManager.Delete(P);
    AssertTrue('Old behaviour: no error', P.State=opsDeleted);
  finally
    P.Free;
  end;
end;

procedure TTestDeleteContracts.TestVersionCheckOnlyIsSilent;
var
  V: TDCVersioned;
begin
  FManager.DeleteChecks:=[dckVersion];
  Exec('INSERT INTO DCVersioned (ID, Name, Version) VALUES (1, ''a'', 2)');
  V:=TDCVersioned.Create;
  try
    V.ID:=1;
    V.Version:=1;
    V.State:=opsClean;
    FManager.Delete(V);
  finally
    V.Free;
  end;
  AssertEquals('Stale version: nothing deleted', 1, Count('DCVersioned'));
end;

procedure TTestDeleteContracts.TestRowsCheckOnlyIgnoresVersion;
var
  V: TDCVersioned;
begin
  FManager.DeleteChecks:=[dckRowsAffected];
  Exec('INSERT INTO DCVersioned (ID, Name, Version) VALUES (1, ''a'', 2)');
  V:=TDCVersioned.Create;
  try
    V.ID:=1;
    V.Version:=1;
    V.State:=opsClean;
    FManager.Delete(V);
  finally
    V.Free;
  end;
  AssertEquals('Version not checked: deleted', 0, Count('DCVersioned'));
end;

procedure TTestDeleteContracts.TestVersionCheckOnlyStaleKeepsChildren;
var
  P: TDCVerParent;
begin
  FManager.DeleteChecks:=[dckVersion];
  Exec('INSERT INTO DCVerParent (ID, Version) VALUES (1, 2)');
  Exec('INSERT INTO DCVerKid (ID, ParentID) VALUES (10, 1)');
  P:=TDCVerParent.Create;
  try
    P.ID:=1;
    P.Version:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Stale version: parent kept', 1, Count('DCVerParent'));
  // Before the fix the cascade ran first: the child was deleted, the parent kept
  AssertEquals('Stale version: child kept', 1, Count('DCVerKid'));
end;

procedure TTestDeleteContracts.TestVersionCheckOnlyCurrentCascades;
var
  P: TDCVerParent;
begin
  FManager.DeleteChecks:=[dckVersion];
  Exec('INSERT INTO DCVerParent (ID, Version) VALUES (1, 2)');
  Exec('INSERT INTO DCVerKid (ID, ParentID) VALUES (10, 1)');
  P:=TDCVerParent.Create;
  try
    P.ID:=1;
    P.Version:=2;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Parent deleted', 0, Count('DCVerParent'));
  AssertEquals('Child deleted', 0, Count('DCVerKid'));
end;

procedure TTestDeleteContracts.TestTreeStringKeysCaseSensitive;
var
  N: TDCSNode;
begin
  Exec('INSERT INTO DCSNode (ID, ParentID) VALUES (''root'', NULL)');
  Exec('INSERT INTO DCSNode (ID, ParentID) VALUES (''ab'', ''root'')');
  Exec('INSERT INTO DCSNode (ID, ParentID) VALUES (''AB'', ''root'')');
  Exec('INSERT INTO DCSNode (ID, ParentID) VALUES (''x'', ''AB'')');
  N:=TDCSNode.Create;
  try
    N.ID:='root';
    N.State:=opsClean;
    FManager.Delete(N);
  finally
    N.Free;
  end;
  // Before the fix 'AB' was taken for the visited 'ab': AB and x remained
  AssertEquals('Whole tree deleted', 0, Count('DCSNode'));
end;

procedure TTestDeleteContracts.TestSoftDeleteFollowsDBCascadeRelation;
var
  P: TDCSoftDBParent;
begin
  Exec('INSERT INTO DCSoftDBParent (ID, Deleted) VALUES (1, 0)');
  Exec('INSERT INTO DCSoftDBKid (ID, ParentID, Deleted) VALUES (10, 1, 0)');
  P:=TDCSoftDBParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Parent soft-deleted', 1, Count('DCSoftDBParent', 'Deleted=1'));
  // ON DELETE CASCADE does not fire on the UPDATE of a soft delete
  AssertEquals('Child soft-deleted too', 1, Count('DCSoftDBKid', 'Deleted=1'));
end;

procedure TTestDeleteContracts.TestHardDeleteLeavesDBCascadeRelation;
var
  P: TDCSoftDBParent;
  D: TPersisterDef;
  F: TOPFieldMap;
begin
  // A hard delete of the parent leaves DBCascade children to the database
  // (SQLite enforces no foreign keys here): make the parent hard by removing
  // its soft-delete option. The registry is created per test.
  D:=FRegistry.FindPersister(TDCSoftDBParent);
  F:=D.FieldMap.FindByPropertyName('Deleted');
  F.Options:=F.Options-[foSoftDelete];
  Exec('INSERT INTO DCSoftDBParent (ID, Deleted) VALUES (1, 0)');
  Exec('INSERT INTO DCSoftDBKid (ID, ParentID, Deleted) VALUES (10, 1, 0)');
  P:=TDCSoftDBParent.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    FManager.Delete(P);
  finally
    P.Free;
  end;
  AssertEquals('Parent deleted', 0, Count('DCSoftDBParent'));
  AssertEquals('Child left to the database', 1, Count('DCSoftDBKid'));
end;

procedure TTestDeleteContracts.TestNilPersisterClassClearError;
var
  P: TDCPlain;
  D: TPersisterDef;
  Saved: TOPPersisterClass;
  Msg: String;
begin
  Exec('INSERT INTO DCPlain (ID, Name) VALUES (1, ''a'')');
  D:=FRegistry.FindPersister(TDCPlain);
  Saved:=D.PersisterClass;
  P:=TDCPlain.Create;
  try
    P.ID:=1;
    P.State:=opsClean;
    D.PersisterClass:=Nil;
    try
      // Before the fix: access violation
      AssertTrue('Delete raises EOPF', DeleteRaises(P, Msg));
      AssertTrue('Clear message: '+Msg, Pos('no persister class', Msg)>0);
    finally
      D.PersisterClass:=Saved;
    end;
  finally
    P.Free;
  end;
end;

{ Composite keys }

procedure TTestDeleteContracts.TestCompositeSoftDeleteBindsAllKeys;
var
  C: TDCComposite;
begin
  Exec('INSERT INTO DCComposite (DCCompositeID, B, Deleted) VALUES (1, 1, 0)');
  Exec('INSERT INTO DCComposite (DCCompositeID, B, Deleted) VALUES (1, 2, 0)');
  C:=TDCComposite.Create;
  try
    C.DCCompositeID:=1;
    C.B:=2;
    C.State:=opsClean;
    FManager.Delete(C);
  finally
    C.Free;
  end;
  AssertEquals('Only (1,2) marked', 1, Count('DCComposite', 'Deleted=1'));
  AssertEquals('(1,2) is the one', 1, Count('DCComposite', 'Deleted=1 AND B=2'));
end;

procedure TTestDeleteContracts.TestCompositeWithRelationRaises;
var
  C: TDCCompParent;
  Msg: String;
begin
  Exec('INSERT INTO DCCompParent (DCCompParentID, B) VALUES (1, 1)');
  Exec('INSERT INTO DCCompKid (ID, CompA) VALUES (5, 1)');
  C:=TDCCompParent.Create;
  try
    C.DCCompParentID:=1;
    C.B:=1;
    C.State:=opsClean;
    AssertTrue('Raises', DeleteRaises(C, Msg));
  finally
    C.Free;
  end;
  AssertEquals('Parent kept', 1, Count('DCCompParent'));
  AssertEquals('Child kept', 1, Count('DCCompKid'));
end;

{ Error details }

procedure TTestDeleteContracts.TestErrorCodeFromDatabase;
var
  P: TDCPlain;
begin
  P:=TDCPlain.Create;
  try
    P.ID:=1;
    P.Name:='bad';
    P.State:=opsNew;
    try
      FManager.Save(P);
      Fail('CHECK constraint must fail');
    except
      on E: EOPF do
      begin
        AssertTrue('Database error code: '+IntToStr(E.ErrorCode), E.ErrorCode<>0);
        AssertEquals('Original class', 'ESQLDatabaseError', E.OriginalClassName);
      end;
    end;
  finally
    P.Free;
  end;
end;

{ Sequences }

procedure TTestDeleteContracts.TestSequencePresetKeyKept;
var
  S: TDCSeq;
begin
  FBuilder.SeqValue:=7;
  S:=TDCSeq.Create;
  try
    S.ID:=42;
    S.State:=opsNew;
    FManager.Save(S);
    AssertEquals('Preset key kept', 42, S.ID);
  finally
    S.Free;
  end;
  AssertEquals(1, Count('DCSeq', 'ID=42'));
end;

procedure TTestDeleteContracts.TestSequenceAssignsKey;
var
  S: TDCSeq;
begin
  FBuilder.SeqValue:=7;
  S:=TDCSeq.Create;
  try
    S.State:=opsNew;
    FManager.Save(S);
    AssertEquals('Key from sequence', 7, S.ID);
  finally
    S.Free;
  end;
end;

procedure TTestDeleteContracts.TestSequenceOutOfRangeRaises;
var
  S: TDCSeq;
  Raised: Boolean;
begin
  FBuilder.SeqValue:=5000000000;
  S:=TDCSeq.Create;
  try
    S.State:=opsNew;
    Raised:=False;
    try
      FManager.Save(S);
    except
      on E: EOPF do
        Raised:=True;
    end;
    AssertTrue('Not truncated silently', Raised);
  finally
    S.Free;
  end;
  AssertEquals('Nothing inserted', 0, Count('DCSeq'));
end;

initialization
  RegisterTest(TTestDeleteContracts);
end.
