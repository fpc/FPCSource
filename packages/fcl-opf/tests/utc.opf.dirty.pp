unit utc.opf.dirty;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, Variants, fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.SnapshotHooks, Opf.ChangeTracking, Opf.Snapshot, Opf.Registry, Opf.Manager;

type

  { TDirtyPerson — test class implementing IOPDirtyTracking for interface-based tests }

  TDirtyPerson = class(TPersistent, IOPDirtyTracking)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
    FSnapshot: TOPVariantArray;
    FHasSnapshot: Boolean;
    FMarkedDirty: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StoreSnapshot(const aValues: TOPVariantArray);
    function RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;
    procedure MarkDirty(const aPropertyName: UTF8String);
    function GetMarkedDirty: TUTF8StringArray;
    procedure ClearSnapshot;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TPlainPerson — test class without IOPDirtyTracking, tracked by TOPSnapshotManager }

  TPlainPerson = class(TPersistent)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

{$M+}
  { TUnobservedPerson — like TPlainPerson, but cannot be observed (no IFPObserved) }

  TUnobservedPerson = class(TObject)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;
{$M-}

  { TObservedPerson — test class implementing IOPDirtyTracking on a TPersistent, so a tracker can observe it }

  TObservedPerson = class(TPersistent, IOPDirtyTracking)
  private
    FID: Integer;
    FName: String;
    FAge: Integer;
    FSnapshot: TOPVariantArray;
    FHasSnapshot: Boolean;
    FMarkedDirty: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StoreSnapshot(const aValues: TOPVariantArray);
    function RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;
    procedure MarkDirty(const aPropertyName: UTF8String);
    function GetMarkedDirty: TUTF8StringArray;
    procedure ClearSnapshot;
  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TTestCaptureAndCompare — tests for OPFCapturePropertyValues and OPFComputeDirtyFields }

  TTestCaptureAndCompare = class(TTestCase)
  private
    FFieldMap: TOPFieldMapList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCaptureValues;
    procedure TestCompareNoDifference;
    procedure TestCompareWithDifference;
    procedure TestCompareWithMarkedDirty;
    procedure TestCompareStringChange;
  end;

  { TTestIOPDirtyTracking — tests for the interface-based approach }

  TTestIOPDirtyTracking = class(TTestCase)
  published
    procedure TestStoreAndRetrieve;
    procedure TestMarkDirty;
    procedure TestClearSnapshot;
    procedure TestNoSnapshotReturnsNil;
  end;

  { TTestSnapshotManager — tests for TOPSnapshotManager }

  TTestSnapshotManager = class(TTestCase)
  private
    FManager: TOPSnapshotManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFreshManagerDefaults;
    procedure TestDefaultModeIsImplicit;
    procedure TestImplicitTracksAllClasses;
    procedure TestImplicitExcludeSkipsClass;
    procedure TestExplicitOnlyTracksRegistered;
    procedure TestNoneDisablesAll;
  end;

  { TTestSnapshotChangeTracker — tests for the IOPChangeTracker side of TOPSnapshotManager }

  TTestSnapshotChangeTracker = class(TTestCase)
  private
    FManager: TOPSnapshotManager;
    FFieldMap: TOPFieldMapList;
    function CreatePerson: TPlainPerson;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCapturedUnchanged;
    procedure TestCapturedOnePropertyChanged;
    procedure TestNeverCaptured;
    procedure TestNoneModeMakesNoEntry;
    procedure TestClassNotTracked;
    procedure TestExplicitRegisteredIsTracked;
    procedure TestExcludedClass;
    procedure TestMaxEntriesEvictsOldest;
    procedure TestNilObject;
    procedure TestMarkedName;
    procedure TestCaptureClearsMarks;
    procedure TestMarkDirtyUntracked;
    procedure TestDiscard;
    procedure TestPurge;
    procedure TestPurgeAll;
    procedure TestReusedAddress;
    procedure TestFreedObservedObject;
    procedure TestFreedObservedObjectRunsPurgeAllSafely;
    procedure TestPartialFieldMap;
    procedure TestThroughInterface;
  end;

  { TTestSelfTracker — tests for TOPSelfTracker }

  TTestSelfTracker = class(TTestCase)
  private
    FTracker: TOPSelfTracker;
    FFieldMap: TOPFieldMapList;
    function CreateDirty: TDirtyPerson;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCapturedUnchanged;
    procedure TestCapturedOnePropertyChanged;
    procedure TestNeverCaptured;
    procedure TestObjectWithoutInterface;
    procedure TestMarkDirtyReachesObject;
    procedure TestMarkDirtyThroughInterface;
    procedure TestSnapshotClearedBehindTheTracker;
    procedure TestFreedObservedObjectIsNotReleased;
    procedure TestDiscard;
    procedure TestPurgeAll;
  end;

  { TTestSnapshotLifecycle — tests for FreeNotification / AttachChangeTracker / DetachChangeTracker }

  TTestSnapshotLifecycle = class(TTestCase)
  published
    procedure TestAttachSetsChangeTracker;
    procedure TestDetachNilsChangeTracker;
    procedure TestManagerFreedNilsSnapshotManagerRef;
    procedure TestSnapshotManagerFreedNilsManagerChangeTracker;
    procedure TestSelfTrackerAttachesToDefaultManager;
    procedure TestSnapshotManagerAttachesToDefaultManager;
  end;

  { TManagerStateTracker — minimal TOPObjectStateTracker descendant used to attach to a manager }

  TManagerStateTracker = class(TOPObjectStateTracker)
  protected
    function CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject; override;
    procedure CompareState(aState: TObject; aObject: TObject;
      aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames); override;
  end;

  { TTestManagerChangeTracker — tests for TOPManager.GetDirtyFields over IOPChangeTracker }

  TTestManagerChangeTracker = class(TTestCase)
  private
    FManager: TOPManager;
    FRegistry: TOPRegistry;
    FTracker: TOPSnapshotManager;
    FPerson: TPlainPerson;
    FDirty: TDirtyPerson;
    FPlainDef: TPersisterDef;
    FDirtyDef: TPersisterDef;
    procedure AttachTracker;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNoTrackerReturnsFalse;
    procedure TestNilObjectReturnsFalse;
    procedure TestNilListReturnsFalse;
    procedure TestNoPersisterReturnsFalse;
    procedure TestTrackedUnchangedReturnsTrueAndEmpty;
    procedure TestTrackedChangedReturnsChangedName;
    procedure TestNeverCapturedReturnsFalse;
    procedure TestStringsFormAppends;
    procedure TestStringsFormOnUntrackedLeavesList;
    procedure TestMarkedNameIsReported;
    procedure TestSelfTrackerAnswers;
    procedure TestNoTrackerIgnoresObjectSnapshot;
    procedure TestProviderWithoutInterfaceLeavesTrackerNil;
    procedure TestStateTrackerAttaches;
  end;

  { TTestTransactionPurge — tests that a reference state survives only a successful commit }

  TTestTransactionPurge = class(TTestCase)
  private
    FManager: TOPManager;
    FRegistry: TOPRegistry;
    FTracker: TOPSnapshotManager;
    FPerson: TPlainPerson;
    FOther: TPlainPerson;
    FDef: TPersisterDef;
    procedure AttachTracker;
    procedure CaptureReference;
    function IsTracked(aObject: TObject): Boolean;
    procedure DoCommit;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOutermostRollbackKeepsUntouched;
    procedure TestNestedRollbackKeepsReferenceState;
    procedure TestSuccessfulCommitKeepsReferenceState;
    procedure TestNestedCommitKeepsReferenceState;
    procedure TestFailingCommitKeepsUntouchedAndEndsTransaction;
    procedure TestFailedOperationKeepsOtherObjects;
    procedure TestNoTrackerAttached;
  end;

implementation

uses typinfo, Opf.Persister.RTTI;

{ Helper to create a standard 3-field map: ID (key), Name (update), Age (update) }

procedure CreateStandardFieldMap(aMap: TOPFieldMapList);
var
  F: TOPFieldMap;
begin
  F := aMap.Add as TOPFieldMap;
  F.PropertyName := 'ID';
  F.FieldName := 'id';
  F.PropertyKind := tkInteger;
  F.Options := [foInKey, foInSelect];

  F := aMap.Add as TOPFieldMap;
  F.PropertyName := 'Name';
  F.FieldName := 'name';
  F.PropertyKind := tkAString;
  F.Options := [foInSelect, foInInsert, foInUpdate];

  F := aMap.Add as TOPFieldMap;
  F.PropertyName := 'Age';
  F.FieldName := 'age';
  F.PropertyKind := tkInteger;
  F.Options := [foInSelect, foInInsert, foInUpdate];
end;

{ Set by TObservedPerson.ClearSnapshot, so a test can see whether the tracker
  released the state of an object that was being freed. }

var
  ObservedSnapshotCleared: Boolean;

{ TObservedPerson }

constructor TObservedPerson.Create;

begin
  inherited Create;
  FHasSnapshot:=False;
  FMarkedDirty:=TStringList.Create;
  FMarkedDirty.Sorted:=True;
  FMarkedDirty.Duplicates:=dupIgnore;
end;


destructor TObservedPerson.Destroy;

begin
  FreeAndNil(FMarkedDirty);
  inherited Destroy;
end;


procedure TObservedPerson.StoreSnapshot(const aValues: TOPVariantArray);

begin
  FSnapshot:=Copy(aValues);
  FHasSnapshot:=True;
  FMarkedDirty.Clear;
end;


function TObservedPerson.RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;

begin
  Result:=FHasSnapshot;
  if Result then
    aValues:=FSnapshot;
end;


procedure TObservedPerson.MarkDirty(const aPropertyName: UTF8String);

begin
  FMarkedDirty.Add(aPropertyName);
end;


function TObservedPerson.GetMarkedDirty: TUTF8StringArray;

var
  lIndex: Integer;

begin
  SetLength(Result,FMarkedDirty.Count);
  for lIndex:=0 to FMarkedDirty.Count-1 do
    Result[lIndex]:=FMarkedDirty[lIndex];
end;


{ The list may already be gone when this runs on a half-destroyed object, so a
  regression fails the assertion instead of crashing. }

procedure TObservedPerson.ClearSnapshot;

begin
  ObservedSnapshotCleared:=True;
  SetLength(FSnapshot,0);
  FHasSnapshot:=False;
  if Assigned(FMarkedDirty) then
    FMarkedDirty.Clear;
end;


{ TDirtyPerson }

constructor TDirtyPerson.Create;
begin
  inherited Create;
  FHasSnapshot := False;
  FMarkedDirty := TStringList.Create;
  FMarkedDirty.Sorted := True;
  FMarkedDirty.Duplicates := dupIgnore;
end;

destructor TDirtyPerson.Destroy;
begin
  FMarkedDirty.Free;
  inherited Destroy;
end;

procedure TDirtyPerson.StoreSnapshot(const aValues: TOPVariantArray);
begin
  FSnapshot := Copy(aValues);
  FHasSnapshot := True;
  FMarkedDirty.Clear;
end;

function TDirtyPerson.RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;
begin
  Result := FHasSnapshot;
  if Result then
    aValues := FSnapshot;
end;

procedure TDirtyPerson.MarkDirty(const aPropertyName: UTF8String);
begin
  FMarkedDirty.Add(aPropertyName);
end;

function TDirtyPerson.GetMarkedDirty: TUTF8StringArray;
var
  I: Integer;
begin
  SetLength(Result, FMarkedDirty.Count);
  for I := 0 to FMarkedDirty.Count - 1 do
    Result[I] := FMarkedDirty[I];
end;

procedure TDirtyPerson.ClearSnapshot;
begin
  SetLength(FSnapshot, 0);
  FHasSnapshot := False;
  FMarkedDirty.Clear;
end;

{ TTestCaptureAndCompare }

procedure TTestCaptureAndCompare.SetUp;
begin
  FFieldMap := TOPFieldMapList.Create(TOPFieldMap);
  CreateStandardFieldMap(FFieldMap);
end;

procedure TTestCaptureAndCompare.TearDown;
begin
  FFieldMap.Free;
end;

procedure TTestCaptureAndCompare.TestCaptureValues;
var
  P: TPlainPerson;
  Values: TOPVariantArray;
begin
  P := TPlainPerson.Create;
  try
    P.ID := 42;
    P.Name := 'Alice';
    P.Age := 30;
    Values := OPFCapturePropertyValues(P, FFieldMap);
    AssertEquals('Array length', 3, Length(Values));
    AssertEquals('ID value', 42, Integer(Values[0]));
    AssertEquals('Name value', 'Alice', String(Values[1]));
    AssertEquals('Age value', 30, Integer(Values[2]));
  finally
    P.Free;
  end;
end;

procedure TTestCaptureAndCompare.TestCompareNoDifference;
var
  Original, Current: TOPVariantArray;
  Dirty: TStringList;
begin
  SetLength(Original, 3);
  Original[0] := 1;
  Original[1] := 'Alice';
  Original[2] := 30;
  Current := Copy(Original);
  Dirty := OPFComputeDirtyFields(Original, Current, FFieldMap, nil);
  try
    AssertEquals('No dirty fields', 0, Dirty.Count);
  finally
    Dirty.Free;
  end;
end;

procedure TTestCaptureAndCompare.TestCompareWithDifference;
var
  Original, Current: TOPVariantArray;
  Dirty: TStringList;
begin
  SetLength(Original, 3);
  Original[0] := 1;
  Original[1] := 'Alice';
  Original[2] := 30;
  SetLength(Current, 3);
  Current[0] := 1;
  Current[1] := 'Alice';
  Current[2] := 31;
  Dirty := OPFComputeDirtyFields(Original, Current, FFieldMap, nil);
  try
    AssertEquals('One dirty field', 1, Dirty.Count);
    AssertEquals('Dirty field is Age', 'Age', Dirty[0]);
  finally
    Dirty.Free;
  end;
end;

procedure TTestCaptureAndCompare.TestCompareWithMarkedDirty;
var
  Original, Current: TOPVariantArray;
  Marked: TUTF8StringArray;
  Dirty: TStringList;
begin
  SetLength(Original, 3);
  Original[0] := 1;
  Original[1] := 'Alice';
  Original[2] := 30;
  Current := Copy(Original);
  SetLength(Marked, 1);
  Marked[0] := 'Name';
  Dirty := OPFComputeDirtyFields(Original, Current, FFieldMap, Marked);
  try
    AssertEquals('One dirty field from MarkDirty', 1, Dirty.Count);
    AssertEquals('Marked field is Name', 'Name', Dirty[0]);
  finally
    Dirty.Free;
  end;
end;

procedure TTestCaptureAndCompare.TestCompareStringChange;
var
  Original, Current: TOPVariantArray;
  Dirty: TStringList;
begin
  SetLength(Original, 3);
  Original[0] := 1;
  Original[1] := 'Alice';
  Original[2] := 30;
  SetLength(Current, 3);
  Current[0] := 1;
  Current[1] := 'Bob';
  Current[2] := 30;
  Dirty := OPFComputeDirtyFields(Original, Current, FFieldMap, nil);
  try
    AssertEquals('One dirty field', 1, Dirty.Count);
    AssertEquals('Dirty field is Name', 'Name', Dirty[0]);
  finally
    Dirty.Free;
  end;
end;

{ TTestIOPDirtyTracking }

procedure TTestIOPDirtyTracking.TestStoreAndRetrieve;
var
  P: TDirtyPerson;
  Values, Retrieved: TOPVariantArray;
begin
  P := TDirtyPerson.Create;
  try
    SetLength(Values, 3);
    Values[0] := 1;
    Values[1] := 'Alice';
    Values[2] := 30;
    P.StoreSnapshot(Values);
    AssertTrue('RetrieveSnapshot returns True', P.RetrieveSnapshot(Retrieved));
    AssertEquals('Retrieved length', 3, Length(Retrieved));
    AssertEquals('Retrieved Name', 'Alice', String(Retrieved[1]));
  finally
    P.Free;
  end;
end;

procedure TTestIOPDirtyTracking.TestMarkDirty;
var
  P: TDirtyPerson;
  Values: TOPVariantArray;
begin
  P := TDirtyPerson.Create;
  try
    SetLength(Values, 1);
    Values[0] := 1;
    P.StoreSnapshot(Values);
    P.MarkDirty('Name');
    AssertEquals('One marked dirty', 1, Length(P.GetMarkedDirty));
    AssertEquals('Marked name', 'Name', P.GetMarkedDirty[0]);
  finally
    P.Free;
  end;
end;

procedure TTestIOPDirtyTracking.TestClearSnapshot;
var
  P: TDirtyPerson;
  Values, Retrieved: TOPVariantArray;
begin
  P := TDirtyPerson.Create;
  try
    SetLength(Values, 1);
    Values[0] := 1;
    P.StoreSnapshot(Values);
    P.ClearSnapshot;
    AssertFalse('RetrieveSnapshot returns False after clear', P.RetrieveSnapshot(Retrieved));
  finally
    P.Free;
  end;
end;

procedure TTestIOPDirtyTracking.TestNoSnapshotReturnsNil;
var
  P: TDirtyPerson;
  Retrieved: TOPVariantArray;
begin
  P := TDirtyPerson.Create;
  try
    AssertFalse('No snapshot initially', P.RetrieveSnapshot(Retrieved));
  finally
    P.Free;
  end;
end;

{ TTestSnapshotManager }

procedure TTestSnapshotManager.SetUp;
begin
  FManager := TOPSnapshotManager.Create(nil);
  FManager.Manager := nil;
end;

procedure TTestSnapshotManager.TearDown;
begin
  FManager.Free;
end;

procedure TTestSnapshotManager.TestFreshManagerDefaults;
begin
  AssertEquals('Fresh manager has an unlimited table', 0, FManager.MaxEntries);
  AssertFalse('Fresh manager tracks observable objects only', FManager.TrackUnobserved);
end;

procedure TTestSnapshotManager.TestDefaultModeIsImplicit;
begin
  AssertTrue('Default mode is smImplicit', FManager.Mode = smImplicit);
end;

procedure TTestSnapshotManager.TestImplicitTracksAllClasses;
begin
  FManager.Mode := smImplicit;
  AssertTrue('TPlainPerson tracked', FManager.IsClassTracked(TPlainPerson));
  AssertTrue('TDirtyPerson tracked', FManager.IsClassTracked(TDirtyPerson));
end;

procedure TTestSnapshotManager.TestImplicitExcludeSkipsClass;
begin
  FManager.Mode := smImplicit;
  FManager.ExcludeClass(TPlainPerson);
  AssertFalse('TPlainPerson excluded', FManager.IsClassTracked(TPlainPerson));
  AssertTrue('TDirtyPerson still tracked', FManager.IsClassTracked(TDirtyPerson));
end;

procedure TTestSnapshotManager.TestExplicitOnlyTracksRegistered;
begin
  FManager.Mode := smExplicit;
  AssertFalse('TPlainPerson not tracked', FManager.IsClassTracked(TPlainPerson));
  FManager.RegisterClass(TPlainPerson);
  AssertTrue('TPlainPerson tracked after Register', FManager.IsClassTracked(TPlainPerson));
  AssertFalse('TDirtyPerson still not tracked', FManager.IsClassTracked(TDirtyPerson));
end;

procedure TTestSnapshotManager.TestNoneDisablesAll;
begin
  FManager.Mode := smNone;
  AssertFalse('TPlainPerson not tracked', FManager.IsClassTracked(TPlainPerson));
  FManager.RegisterClass(TPlainPerson);
  AssertFalse('Still not tracked after Register in smNone', FManager.IsClassTracked(TPlainPerson));
end;

{ TTestSnapshotChangeTracker }

function NamesContain(const aNames: TUTF8StringArray; const aName: UTF8String): Boolean;

var
  lIndex: Integer;

begin
  Result:=False;
  for lIndex:=0 to Length(aNames)-1 do
    if aNames[lIndex]=aName then
      Exit(True);
end;


procedure TTestSnapshotChangeTracker.SetUp;

begin
  FManager:=TOPSnapshotManager.Create(Nil);
  FManager.Manager:=Nil;
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateStandardFieldMap(FFieldMap);
end;


procedure TTestSnapshotChangeTracker.TearDown;

begin
  FreeAndNil(FManager);
  FreeAndNil(FFieldMap);
end;


function TTestSnapshotChangeTracker.CreatePerson: TPlainPerson;

begin
  Result:=TPlainPerson.Create;
  Result.ID:=1;
  Result.Name:='Alice';
  Result.Age:=30;
end;


procedure TTestSnapshotChangeTracker.TestCapturedUnchanged;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    AssertTrue('Captured object is tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an unchanged object',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestCapturedOnePropertyChanged;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Captured object is tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One changed name',1,Length(lNames));
    AssertEquals('Changed name is Age','Age',lNames[0]);
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestNeverCaptured;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    AssertFalse('Fresh object is not tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an untracked object',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestNoneModeMakesNoEntry;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  FManager.Mode:=smNone;
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    AssertFalse('smNone makes no entry',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names in smNone',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestExplicitRegisteredIsTracked;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  FManager.Mode:=smExplicit;
  FManager.RegisterClass(TPlainPerson);
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Registered class is tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One changed name',1,Length(lNames));
    AssertEquals('Changed name is Age','Age',lNames[0]);
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestMaxEntriesEvictsOldest;

var
  lFirst, lSecond, lThird: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  FManager.MaxEntries:=2;
  lFirst:=CreatePerson;
  lSecond:=CreatePerson;
  lThird:=CreatePerson;
  try
    FManager.Capture(lFirst,FFieldMap);
    FManager.Capture(lSecond,FFieldMap);
    FManager.Capture(lThird,FFieldMap);
    AssertFalse('Oldest entry evicted',FManager.GetChanges(lFirst,FFieldMap,lNames));
    AssertTrue('Second entry kept',FManager.GetChanges(lSecond,FFieldMap,lNames));
    AssertTrue('Third entry kept',FManager.GetChanges(lThird,FFieldMap,lNames));
  finally
    lThird.Free;
    lSecond.Free;
    lFirst.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestClassNotTracked;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  FManager.Mode:=smExplicit;
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    AssertFalse('Unregistered class makes no entry',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an untracked class',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestExcludedClass;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  FManager.Mode:=smImplicit;
  FManager.ExcludeClass(TPlainPerson);
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    AssertFalse('Excluded class makes no entry',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an excluded class',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestNilObject;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    FManager.Capture(Nil,FFieldMap);
    AssertTrue('Entry survives a Nil capture',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Reference state survives a Nil capture',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestMarkedName;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    FManager.MarkDirty(lPerson,'Name');
    AssertTrue('Captured object is tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Marked name is returned','Name',lNames[0]);
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestCaptureClearsMarks;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    FManager.MarkDirty(lPerson,'Name');
    FManager.Capture(lPerson,FFieldMap);
    AssertTrue('Captured object is tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Marks cleared by a new capture',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestMarkDirtyUntracked;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.MarkDirty(lPerson,'Name');
    AssertFalse('Marking does not create an entry',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestDiscard;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    FManager.Discard(lPerson);
    AssertFalse('Discarded object is untracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names after discard',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestPurge;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    FManager.Purge(lPerson);
    AssertFalse('Purged object is untracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names after purge',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestPurgeAll;

var
  lFirst, lSecond: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lFirst:=CreatePerson;
  lSecond:=CreatePerson;
  try
    FManager.Capture(lFirst,FFieldMap);
    FManager.Capture(lSecond,FFieldMap);
    FManager.PurgeAll;
    AssertFalse('First object untracked',FManager.GetChanges(lFirst,FFieldMap,lNames));
    AssertFalse('Second object untracked',FManager.GetChanges(lSecond,FFieldMap,lNames));
  finally
    lSecond.Free;
    lFirst.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestReusedAddress;

var
  lBlock: Pointer;
  lSize: PtrUInt;
  lFirst: TUnobservedPerson;
  lSecond: TDirtyPerson;
  lNames: TUTF8StringArray;

begin
  // Only an object that cannot be observed can be freed behind the tracker
  FManager.TrackUnobserved:=True;
  lSize:=TUnobservedPerson.InstanceSize;
  if TDirtyPerson.InstanceSize>lSize then
    lSize:=TDirtyPerson.InstanceSize;
  lBlock:=GetMem(lSize);
  try
    FillChar(lBlock^,lSize,0);
    lFirst:=TUnobservedPerson(TUnobservedPerson.InitInstance(lBlock));
    try
      lFirst.Name:='Alice';
      FManager.Capture(lFirst,FFieldMap);
      AssertTrue('First object is tracked',FManager.GetChanges(lFirst,FFieldMap,lNames));
    finally
      lFirst.CleanupInstance;
    end;
    FillChar(lBlock^,lSize,0);
    lSecond:=TDirtyPerson(TDirtyPerson.InitInstance(lBlock));
    try
      AssertFalse('Reused address is not tracked',FManager.GetChanges(lSecond,FFieldMap,lNames));
      AssertEquals('No names for a reused address',0,Length(lNames));
    finally
      lSecond.CleanupInstance;
    end;
  finally
    FreeMem(lBlock);
  end;
end;


{ The free notification is sent by hand so that the block stays available for a
  second object of the same class: the class of the entry then still matches, and
  only the observer can tell the tracker the first object is gone. }

procedure TTestSnapshotChangeTracker.TestFreedObservedObject;

var
  lBlock: Pointer;
  lSize: PtrUInt;
  lFirst, lSecond: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lSize:=TObservedPerson.InstanceSize;
  lBlock:=GetMem(lSize);
  try
    FillChar(lBlock^,lSize,0);
    lFirst:=TObservedPerson(TObservedPerson.InitInstance(lBlock));
    try
      lFirst.Name:='Alice';
      FManager.Capture(lFirst,FFieldMap);
      AssertTrue('Observed object is tracked',FManager.GetChanges(lFirst,FFieldMap,lNames));
      lFirst.FPONotifyObservers(lFirst,ooFree,Nil);
      AssertFalse('Entry dropped when the object was freed',
        FManager.GetChanges(lFirst,FFieldMap,lNames));
    finally
      FManager.Purge(lFirst);
      lFirst.FPODetachObserver(FManager);
      lFirst.CleanupInstance;
    end;
    FillChar(lBlock^,lSize,0);
    lSecond:=TObservedPerson(TObservedPerson.InitInstance(lBlock));
    try
      AssertFalse('Address of the freed object is not tracked',
        FManager.GetChanges(lSecond,FFieldMap,lNames));
      AssertEquals('No names for the address of a freed object',0,Length(lNames));
    finally
      lSecond.CleanupInstance;
    end;
  finally
    FreeMem(lBlock);
  end;
end;


{ TObservedPerson descends from TPersistent, so a plain Free runs the whole FCL
  notification path. PurgeAll afterwards detaches from every object it still has
  an entry for, which would reach into the freed one had the observer not dropped
  its entry. }

procedure TTestSnapshotChangeTracker.TestFreedObservedObjectRunsPurgeAllSafely;

var
  lFreed, lKept: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lFreed:=TObservedPerson.Create;
  lKept:=TObservedPerson.Create;
  try
    lFreed.Name:='Alice';
    lKept.Name:='Bob';
    FManager.Capture(lFreed,FFieldMap);
    FManager.Capture(lKept,FFieldMap);
    AssertTrue('First object is tracked',FManager.GetChanges(lFreed,FFieldMap,lNames));
    AssertTrue('Second object is tracked',FManager.GetChanges(lKept,FFieldMap,lNames));
    FreeAndNil(lFreed);
    FManager.PurgeAll;
    AssertFalse('Surviving object untracked after PurgeAll',
      FManager.GetChanges(lKept,FFieldMap,lNames));
  finally
    lKept.Free;
    lFreed.Free;
  end;
end;


{ The map built from the reported names holds the key field and the changed
  field only. }

procedure TTestSnapshotChangeTracker.TestPartialFieldMap;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;
  lMap: TOPFieldMapList;

begin
  lPerson:=CreatePerson;
  try
    FManager.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Captured object is tracked',FManager.GetChanges(lPerson,FFieldMap,lNames));
    lMap:=BuildChangedFieldMapList(FFieldMap,lNames);
    try
      AssertEquals('Key field and changed field only',2,lMap.Count);
      AssertEquals('Key field kept','id',lMap[0].FieldName);
      AssertEquals('Changed field kept','age',lMap[1].FieldName);
    finally
      lMap.Free;
    end;
  finally
    lPerson.Free;
  end;
end;


procedure TTestSnapshotChangeTracker.TestThroughInterface;

var
  lPerson: TPlainPerson;
  lTracker: IOPChangeTracker;
  lNames: TUTF8StringArray;

begin
  AssertTrue('Manager exposes IOPChangeTracker',
    FManager.GetInterface(SGUIDIOPChangeTracker,lTracker));
  lPerson:=CreatePerson;
  try
    lTracker.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Tracked through the interface',lTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One changed name',1,Length(lNames));
    AssertEquals('Changed name is Age','Age',lNames[0]);
    lTracker.MarkDirty(lPerson,'Name');
    AssertTrue('Still tracked',lTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Marked name added',2,Length(lNames));
    lTracker.Discard(lPerson);
    AssertFalse('Discarded through the interface',lTracker.GetChanges(lPerson,FFieldMap,lNames));
    lTracker.Capture(lPerson,FFieldMap);
    lTracker.PurgeAll;
    AssertFalse('Purged through the interface',lTracker.GetChanges(lPerson,FFieldMap,lNames));
  finally
    lPerson.Free;
  end;
end;


{ TTestSelfTracker }

procedure TTestSelfTracker.SetUp;

begin
  FTracker:=TOPSelfTracker.Create(Nil);
  // The constructor attaches to the global manager singleton, which this suite does not use.
  FTracker.Manager:=Nil;
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateStandardFieldMap(FFieldMap);
end;


procedure TTestSelfTracker.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FFieldMap);
end;


function TTestSelfTracker.CreateDirty: TDirtyPerson;

begin
  Result:=TDirtyPerson.Create;
  Result.ID:=1;
  Result.Name:='Alice';
  Result.Age:=30;
end;


procedure TTestSelfTracker.TestCapturedUnchanged;

var
  lPerson: TDirtyPerson;
  lNames: TUTF8StringArray;
  lValues: TOPVariantArray;

begin
  lPerson:=CreateDirty;
  try
    FTracker.Capture(lPerson,FFieldMap);
    AssertTrue('Captured object is tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an unchanged object',0,Length(lNames));
    AssertTrue('The object holds the snapshot',lPerson.RetrieveSnapshot(lValues));
  finally
    FTracker.Purge(lPerson);
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestCapturedOnePropertyChanged;

var
  lPerson: TDirtyPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreateDirty;
  try
    FTracker.Capture(lPerson,FFieldMap);
    lPerson.Age:=31;
    AssertTrue('Captured object is tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One changed name',1,Length(lNames));
    AssertEquals('Changed name is Age','Age',lNames[0]);
  finally
    FTracker.Purge(lPerson);
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestNeverCaptured;

var
  lPerson: TDirtyPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreateDirty;
  try
    AssertFalse('Fresh object is not tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an untracked object',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestObjectWithoutInterface;

var
  lPerson: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=TPlainPerson.Create;
  try
    lPerson.ID:=1;
    lPerson.Name:='Alice';
    lPerson.Age:=30;
    FTracker.Capture(lPerson,FFieldMap);
    AssertFalse('No entry without IOPDirtyTracking',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('No names for an untracked class',0,Length(lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestMarkDirtyReachesObject;

var
  lPerson: TDirtyPerson;
  lNames, lMarked: TUTF8StringArray;

begin
  lPerson:=CreateDirty;
  try
    FTracker.Capture(lPerson,FFieldMap);
    FTracker.MarkDirty(lPerson,'Name');
    lMarked:=lPerson.GetMarkedDirty;
    AssertEquals('One marked name on the object',1,Length(lMarked));
    AssertEquals('Marked name is Name','Name',lMarked[0]);
    AssertTrue('Still tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One reported name',1,Length(lNames));
    AssertEquals('Reported name is Name','Name',lNames[0]);
  finally
    FTracker.Purge(lPerson);
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestMarkDirtyThroughInterface;

var
  lPerson: TDirtyPerson;
  lChangeTracker: IOPChangeTracker;
  lNames, lMarked: TUTF8StringArray;

begin
  AssertTrue('Tracker exposes IOPChangeTracker',
    FTracker.GetInterface(SGUIDIOPChangeTracker,lChangeTracker));
  lPerson:=CreateDirty;
  try
    lChangeTracker.Capture(lPerson,FFieldMap);
    lChangeTracker.MarkDirty(lPerson,'Name');
    lMarked:=lPerson.GetMarkedDirty;
    AssertEquals('The override ran, not the base',1,Length(lMarked));
    AssertEquals('Marked name is Name','Name',lMarked[0]);
    AssertTrue('Tracked through the interface',
      lChangeTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('One reported name',1,Length(lNames));
    AssertEquals('Reported name is Name','Name',lNames[0]);
  finally
    lChangeTracker.Discard(lPerson);
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestSnapshotClearedBehindTheTracker;

var
  lPerson: TDirtyPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=CreateDirty;
  try
    FTracker.Capture(lPerson,FFieldMap);
    lPerson.ClearSnapshot;
    AssertTrue('Entry survives the cleared snapshot',
      FTracker.GetChanges(lPerson,FFieldMap,lNames));
    AssertEquals('Every updatable property reported',2,Length(lNames));
    AssertTrue('Name reported',NamesContain(lNames,'Name'));
    AssertTrue('Age reported',NamesContain(lNames,'Age'));
  finally
    FTracker.Purge(lPerson);
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestFreedObservedObjectIsNotReleased;

var
  lPerson: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=TObservedPerson.Create;
  lPerson.ID:=1;
  lPerson.Name:='Alice';
  lPerson.Age:=30;
  FTracker.Capture(lPerson,FFieldMap);
  AssertTrue('Captured object is tracked',FTracker.GetChanges(lPerson,FFieldMap,lNames));
  ObservedSnapshotCleared:=False;
  lPerson.Free;
  AssertFalse('ClearSnapshot not called on an object being freed',ObservedSnapshotCleared);
  AssertFalse('Entry dropped when the object was freed',
    FTracker.GetChanges(lPerson,FFieldMap,lNames));
end;


procedure TTestSelfTracker.TestDiscard;

var
  lPerson: TDirtyPerson;
  lNames: TUTF8StringArray;
  lValues: TOPVariantArray;

begin
  lPerson:=CreateDirty;
  try
    FTracker.Capture(lPerson,FFieldMap);
    FTracker.Discard(lPerson);
    AssertFalse('The object snapshot is cleared',lPerson.RetrieveSnapshot(lValues));
    AssertFalse('Not tracked after discard',FTracker.GetChanges(lPerson,FFieldMap,lNames));
  finally
    lPerson.Free;
  end;
end;


procedure TTestSelfTracker.TestPurgeAll;

var
  lFirst, lSecond: TDirtyPerson;
  lNames: TUTF8StringArray;
  lValues: TOPVariantArray;

begin
  lFirst:=CreateDirty;
  lSecond:=CreateDirty;
  try
    FTracker.Capture(lFirst,FFieldMap);
    FTracker.Capture(lSecond,FFieldMap);
    FTracker.PurgeAll;
    AssertFalse('First object snapshot cleared',lFirst.RetrieveSnapshot(lValues));
    AssertFalse('Second object snapshot cleared',lSecond.RetrieveSnapshot(lValues));
    AssertFalse('First object untracked',FTracker.GetChanges(lFirst,FFieldMap,lNames));
    AssertFalse('Second object untracked',FTracker.GetChanges(lSecond,FFieldMap,lNames));
  finally
    lFirst.Free;
    lSecond.Free;
  end;
end;


{ TTestSnapshotLifecycle }

procedure TTestSnapshotLifecycle.TestAttachSetsChangeTracker;
var
  Mgr: TOPManager;
  SM: TOPSnapshotManager;
begin
  Mgr := TOPManager.Create(nil);
  try
    SM := TOPSnapshotManager.Create(nil);
    SM.Manager := nil;
    try
      AssertTrue('ChangeTracker nil before attach', Mgr.ChangeTracker = nil);
      SM.Manager := Mgr;
      AssertTrue('ChangeTracker set after attach', Mgr.ChangeTracker <> nil);
    finally
      SM.Free;
    end;
  finally
    Mgr.Free;
  end;
end;

procedure TTestSnapshotLifecycle.TestDetachNilsChangeTracker;
var
  Mgr: TOPManager;
  SM: TOPSnapshotManager;
begin
  Mgr := TOPManager.Create(nil);
  try
    SM := TOPSnapshotManager.Create(nil);
    SM.Manager := nil;
    try
      SM.Manager := Mgr;
      AssertTrue('ChangeTracker set', Mgr.ChangeTracker <> nil);
      SM.Manager := nil;
      AssertTrue('ChangeTracker nil after detach', Mgr.ChangeTracker = nil);
    finally
      SM.Free;
    end;
  finally
    Mgr.Free;
  end;
end;

procedure TTestSnapshotLifecycle.TestManagerFreedNilsSnapshotManagerRef;
var
  Mgr: TOPManager;
  SM: TOPSnapshotManager;
begin
  SM := TOPSnapshotManager.Create(nil);
  SM.Manager := nil;
  try
    Mgr := TOPManager.Create(nil);
    SM.Manager := Mgr;
    AssertTrue('Manager set', SM.Manager = Mgr);
    Mgr.Free;
    AssertTrue('Manager nil after free', SM.Manager = nil);
  finally
    SM.Free;
  end;
end;

procedure TTestSnapshotLifecycle.TestSnapshotManagerFreedNilsManagerChangeTracker;
var
  Mgr: TOPManager;
  SM: TOPSnapshotManager;
begin
  Mgr := TOPManager.Create(nil);
  try
    SM := TOPSnapshotManager.Create(nil);
    SM.Manager := nil;
    SM.Manager := Mgr;
    AssertTrue('ChangeTracker set', Mgr.ChangeTracker <> nil);
    SM.Free;
    AssertTrue('ChangeTracker nil after SM freed', Mgr.ChangeTracker = nil);
  finally
    Mgr.Free;
  end;
end;

procedure TTestSnapshotLifecycle.TestSelfTrackerAttachesToDefaultManager;

var
  lTracker: TOPSelfTracker;
  lAttached: IOPChangeTracker;

begin
  TOPManager.ClearInstance;
  try
    lTracker:=TOPSelfTracker.Create(nil);
    try
      AssertTrue('Tracker exposes IOPChangeTracker',
        lTracker.GetInterface(SGUIDIOPChangeTracker,lAttached));
      AssertTrue('The constructor attached it to the default manager',
        OPManager.ChangeTracker=lAttached);
    finally
      lTracker.Free;
    end;
    AssertTrue('ChangeTracker nil after the tracker is freed',OPManager.ChangeTracker=nil);
  finally
    TOPManager.ClearInstance;
  end;
end;


{ The constructor attaches the tracker to the default manager, and freeing the
  tracker detaches it again. }

procedure TTestSnapshotLifecycle.TestSnapshotManagerAttachesToDefaultManager;

var
  lTracker: TOPSnapshotManager;
  lAttached: IOPChangeTracker;

begin
  TOPManager.ClearInstance;
  try
    lTracker:=TOPSnapshotManager.Create(nil);
    try
      AssertTrue('Tracker exposes IOPChangeTracker',
        lTracker.GetInterface(SGUIDIOPChangeTracker,lAttached));
      AssertTrue('The constructor attached it to the default manager',
        OPManager.ChangeTracker=lAttached);
    finally
      lTracker.Free;
    end;
    AssertTrue('ChangeTracker nil after the tracker is freed',OPManager.ChangeTracker=nil);
  finally
    TOPManager.ClearInstance;
  end;
end;


{ TManagerStateTracker }

function TManagerStateTracker.CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject;

begin
  Result:=TObject.Create;
end;


procedure TManagerStateTracker.CompareState(aState: TObject; aObject: TObject;
  aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames);

begin
  // Nothing ever differs for this tracker.
end;


{ TTestManagerChangeTracker }

procedure TTestManagerChangeTracker.SetUp;

begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(nil);
  FPlainDef:=FRegistry.RegisterPersister(TPlainPerson,nil);
  FPlainDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateStandardFieldMap(FPlainDef.FieldMap);
  FDirtyDef:=FRegistry.RegisterPersister(TDirtyPerson,nil);
  FDirtyDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateStandardFieldMap(FDirtyDef.FieldMap);
  FManager:=TOPManager.Create(nil);
  FManager.Registry:=FRegistry;
  FPerson:=TPlainPerson.Create;
  FPerson.ID:=1;
  FPerson.Name:='Alice';
  FPerson.Age:=30;
  FDirty:=TDirtyPerson.Create;
  FDirty.ID:=2;
  FDirty.Name:='Bob';
  FDirty.Age:=40;
end;


procedure TTestManagerChangeTracker.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FRegistry);
  FreeAndNil(FPerson);
  FreeAndNil(FDirty);
  inherited TearDown;
end;


procedure TTestManagerChangeTracker.AttachTracker;

begin
  FTracker:=TOPSnapshotManager.Create(nil);
  // The constructor attaches to the global manager singleton, so detach before attaching here.
  FTracker.Manager:=nil;
  FTracker.Manager:=FManager;
end;


procedure TTestManagerChangeTracker.TestNoTrackerReturnsFalse;

var
  lNames: TUTF8StringArray;

begin
  AssertTrue('No tracker attached',FManager.ChangeTracker=nil);
  AssertFalse('Untracked without tracker',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('No names returned',0,Length(lNames));
end;


procedure TTestManagerChangeTracker.TestNilObjectReturnsFalse;

var
  lNames: TUTF8StringArray;
  lList: TStringList;

begin
  AttachTracker;
  AssertFalse('Nil object untracked in array form',FManager.GetDirtyFields(TObject(nil),lNames));
  AssertEquals('No names returned',0,Length(lNames));
  lList:=TStringList.Create;
  try
    lList.Add('Existing');
    AssertFalse('Nil object untracked in list form',FManager.GetDirtyFields(TObject(nil),lList));
    AssertEquals('List untouched',1,lList.Count);
    AssertEquals('Existing entry kept','Existing',lList[0]);
  finally
    lList.Free;
  end;
end;


procedure TTestManagerChangeTracker.TestNilListReturnsFalse;

begin
  AttachTracker;
  FTracker.Capture(FPerson,FPlainDef.FieldMap);
  FPerson.Age:=31;
  AssertFalse('Nil list refused',FManager.GetDirtyFields(FPerson,TStrings(nil)));
end;


procedure TTestManagerChangeTracker.TestNoPersisterReturnsFalse;

var
  lEmpty: TOPRegistry;
  lNames: TUTF8StringArray;

begin
  AttachTracker;
  FTracker.Capture(FPerson,FPlainDef.FieldMap);
  FPerson.Age:=31;
  lEmpty:=TOPRegistry.Create(nil);
  try
    FManager.Registry:=lEmpty;
    AssertFalse('Untracked without persister',FManager.GetDirtyFields(FPerson,lNames));
    AssertEquals('No names returned',0,Length(lNames));
  finally
    FManager.Registry:=FRegistry;
    lEmpty.Free;
  end;
end;


procedure TTestManagerChangeTracker.TestTrackedUnchangedReturnsTrueAndEmpty;

var
  lNames: TUTF8StringArray;

begin
  AttachTracker;
  FTracker.Capture(FPerson,FPlainDef.FieldMap);
  AssertTrue('Tracked',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('Nothing changed',0,Length(lNames));
end;


procedure TTestManagerChangeTracker.TestTrackedChangedReturnsChangedName;

var
  lNames: TUTF8StringArray;

begin
  AttachTracker;
  FTracker.Capture(FPerson,FPlainDef.FieldMap);
  FPerson.Age:=31;
  AssertTrue('Tracked',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('One name changed',1,Length(lNames));
  AssertEquals('Age changed','Age',lNames[0]);
end;


procedure TTestManagerChangeTracker.TestNeverCapturedReturnsFalse;

var
  lNames: TUTF8StringArray;

begin
  AttachTracker;
  AssertFalse('Never captured',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('No names returned',0,Length(lNames));
end;


procedure TTestManagerChangeTracker.TestStringsFormAppends;

var
  lList: TStringList;

begin
  AttachTracker;
  FTracker.Capture(FPerson,FPlainDef.FieldMap);
  FPerson.Age:=31;
  lList:=TStringList.Create;
  try
    lList.Add('Existing');
    AssertTrue('Tracked',FManager.GetDirtyFields(FPerson,lList));
    AssertEquals('Existing entry kept plus one name',2,lList.Count);
    AssertEquals('Existing entry first','Existing',lList[0]);
    AssertEquals('Age appended','Age',lList[1]);
  finally
    lList.Free;
  end;
end;


procedure TTestManagerChangeTracker.TestStringsFormOnUntrackedLeavesList;

var
  lList: TStringList;

begin
  AttachTracker;
  lList:=TStringList.Create;
  try
    lList.Add('Existing');
    AssertFalse('Never captured',FManager.GetDirtyFields(FPerson,lList));
    AssertEquals('List untouched',1,lList.Count);
    AssertEquals('Existing entry kept','Existing',lList[0]);
  finally
    lList.Free;
  end;
end;


procedure TTestManagerChangeTracker.TestMarkedNameIsReported;

var
  lNames: TUTF8StringArray;

begin
  AttachTracker;
  FTracker.Capture(FPerson,FPlainDef.FieldMap);
  FTracker.MarkDirty(FPerson,'Name');
  AssertTrue('Tracked',FManager.GetDirtyFields(FPerson,lNames));
  AssertEquals('One marked name',1,Length(lNames));
  AssertEquals('Marked name reported','Name',lNames[0]);
end;


procedure TTestManagerChangeTracker.TestSelfTrackerAnswers;

var
  lTracker: TOPSelfTracker;
  lNames: TUTF8StringArray;

begin
  lTracker:=TOPSelfTracker.Create(nil);
  try
    // Replaces the global manager singleton the constructor attached to.
    lTracker.Manager:=FManager;
    lTracker.Capture(FDirty,FDirtyDef.FieldMap);
    FDirty.Age:=41;
    AssertTrue('Answered by the self tracker',FManager.GetDirtyFields(FDirty,lNames));
    AssertEquals('One name changed',1,Length(lNames));
    AssertEquals('Age changed','Age',lNames[0]);
    lTracker.Purge(FDirty);
  finally
    lTracker.Free;
  end;
end;


procedure TTestManagerChangeTracker.TestNoTrackerIgnoresObjectSnapshot;

var
  lNames: TUTF8StringArray;

begin
  AssertTrue('No tracker attached',FManager.ChangeTracker=nil);
  FDirty.StoreSnapshot(OPFCapturePropertyValues(FDirty,FDirtyDef.FieldMap));
  FDirty.Age:=41;
  AssertFalse('The object snapshot is not consulted',FManager.GetDirtyFields(FDirty,lNames));
  AssertEquals('No names returned',0,Length(lNames));
end;


procedure TTestManagerChangeTracker.TestProviderWithoutInterfaceLeavesTrackerNil;

var
  lProvider: TComponent;

begin
  lProvider:=TComponent.Create(nil);
  try
    FManager.AttachChangeTracker(lProvider);
    AssertTrue('ChangeTracker stays nil',FManager.ChangeTracker=nil);
  finally
    lProvider.Free;
  end;
  AssertTrue('ChangeTracker still nil after provider freed',FManager.ChangeTracker=nil);
end;


procedure TTestManagerChangeTracker.TestStateTrackerAttaches;

var
  lTracker: TManagerStateTracker;

begin
  lTracker:=TManagerStateTracker.Create(nil);
  try
    lTracker.Manager:=FManager;
    AssertTrue('ChangeTracker set by state tracker',FManager.ChangeTracker<>nil);
    lTracker.Manager:=nil;
    AssertTrue('ChangeTracker nil after detach',FManager.ChangeTracker=nil);
  finally
    lTracker.Free;
  end;
end;


{ Names the class of a caught exception, or reports that none arrived. }

function RaisedName(aRaised: TClass): String;

begin
  if aRaised=nil then
    Result:='nothing raised'
  else
    Result:=aRaised.ClassName;
end;


{ TTestTransactionPurge }

procedure TTestTransactionPurge.SetUp;

begin
  inherited SetUp;
  FRegistry:=TOPRegistry.Create(nil);
  FDef:=FRegistry.RegisterPersister(TPlainPerson,nil);
  FDef.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  CreateStandardFieldMap(FDef.FieldMap);
  // The persister class the manager instantiates for a save.
  FDef.PersisterClass:=TRTTIPersister;
  FManager:=TOPManager.Create(nil);
  FManager.Registry:=FRegistry;
  FPerson:=TPlainPerson.Create;
  FPerson.ID:=1;
  FPerson.Name:='Alice';
  FPerson.Age:=30;
  FOther:=TPlainPerson.Create;
  FOther.ID:=2;
  FOther.Name:='Bob';
  FOther.Age:=40;
end;


procedure TTestTransactionPurge.TearDown;

begin
  // Unwinds a transaction a failed assertion left open.
  while Assigned(FManager) and FManager.InTransaction do
    FManager.Rollback;
  FreeAndNil(FTracker);
  FreeAndNil(FManager);
  FreeAndNil(FRegistry);
  FreeAndNil(FPerson);
  FreeAndNil(FOther);
  inherited TearDown;
end;


procedure TTestTransactionPurge.AttachTracker;

begin
  FTracker:=TOPSnapshotManager.Create(nil);
  // The constructor attaches to the global manager singleton, so detach before attaching here.
  FTracker.Manager:=nil;
  FTracker.Manager:=FManager;
end;


procedure TTestTransactionPurge.CaptureReference;

begin
  FTracker.Capture(FPerson,FDef.FieldMap);
end;


function TTestTransactionPurge.IsTracked(aObject: TObject): Boolean;

var
  lNames: TUTF8StringArray;

begin
  Result:=FManager.GetDirtyFields(aObject,lNames);
end;


procedure TTestTransactionPurge.DoCommit;

begin
  FManager.Commit;
end;


{ A rollback only drops the snapshots that were taken or dropped inside the
  transaction; a snapshot from before still matches the database. }

procedure TTestTransactionPurge.TestOutermostRollbackKeepsUntouched;

begin
  AttachTracker;
  CaptureReference;
  AssertTrue('Tracked before the transaction',IsTracked(FPerson));
  FManager.BeginTransaction;
  FManager.Rollback;
  AssertFalse('Transaction ended',FManager.InTransaction);
  AssertTrue('Untouched object still tracked after an outermost rollback',IsTracked(FPerson));
end;


procedure TTestTransactionPurge.TestNestedRollbackKeepsReferenceState;

begin
  AttachTracker;
  CaptureReference;
  FManager.BeginTransaction;
  FManager.BeginTransaction;
  FManager.Rollback;
  AssertTrue('Still in a transaction',FManager.InTransaction);
  AssertTrue('Reference state survives a nested rollback',IsTracked(FPerson));
  FManager.Rollback;
  AssertFalse('Transaction ended',FManager.InTransaction);
  AssertTrue('Untouched object still tracked after the outermost rollback',IsTracked(FPerson));
end;


procedure TTestTransactionPurge.TestSuccessfulCommitKeepsReferenceState;

begin
  AttachTracker;
  CaptureReference;
  FManager.BeginTransaction;
  FManager.Commit;
  AssertFalse('Transaction ended',FManager.InTransaction);
  AssertTrue('Reference state survives a successful commit',IsTracked(FPerson));
end;


procedure TTestTransactionPurge.TestNestedCommitKeepsReferenceState;

begin
  AttachTracker;
  CaptureReference;
  FManager.BeginTransaction;
  FManager.BeginTransaction;
  FManager.Commit;
  AssertTrue('Still in a transaction after the nested commit',FManager.InTransaction);
  AssertTrue('Reference state survives a nested commit',IsTracked(FPerson));
  FManager.Commit;
  AssertFalse('Transaction ended',FManager.InTransaction);
  AssertTrue('Reference state survives the outer commit',IsTracked(FPerson));
end;


procedure TTestTransactionPurge.TestFailingCommitKeepsUntouchedAndEndsTransaction;

var
  lRaised: TClass;

begin
  AttachTracker;
  CaptureReference;
  FManager.BeginTransaction;
  FManager.BeginTransaction;
  FManager.Rollback;
  lRaised:=nil;
  try
    FManager.Commit;
  except
    on E: Exception do
      lRaised:=E.ClassType;
  end;
  AssertEquals('Exception reaching the caller','EOPFTransaction',RaisedName(lRaised));
  AssertFalse('Transaction ended',FManager.InTransaction);
  AssertTrue('Untouched object still tracked after a failing commit',IsTracked(FPerson));
  AssertException('Second commit finds no transaction',EOPF,@DoCommit);
end;


{ A failed operation does not cost the other objects their tracking:
  previously any rollback purged every snapshot. }

procedure TTestTransactionPurge.TestFailedOperationKeepsOtherObjects;

var
  lRaised: TClass;

begin
  AttachTracker;
  CaptureReference;
  FTracker.Capture(FOther,FDef.FieldMap);
  AssertTrue('Saved object tracked beforehand',IsTracked(FPerson));
  AssertTrue('Untouched object tracked beforehand',IsTracked(FOther));
  lRaised:=nil;
  try
    // Saves with no connection configured: the save fails inside its own transaction.
    FManager.Save(FPerson,opsUpdate);
  except
    on E: Exception do
      lRaised:=E.ClassType;
  end;
  AssertEquals('Exception reaching the caller','EOPF',RaisedName(lRaised));
  AssertFalse('Transaction ended',FManager.InTransaction);
  // The save failed before writing anything: its snapshot still matches the database
  AssertTrue('Object of the failed save still tracked',IsTracked(FPerson));
  AssertTrue('Untouched object still tracked',IsTracked(FOther));
end;


procedure TTestTransactionPurge.TestNoTrackerAttached;

var
  lRaised: TClass;

begin
  AssertTrue('No tracker attached',FManager.ChangeTracker=nil);
  FManager.BeginTransaction;
  FManager.Rollback;
  AssertFalse('Transaction ended after rollback',FManager.InTransaction);
  FManager.BeginTransaction;
  FManager.BeginTransaction;
  FManager.Rollback;
  lRaised:=nil;
  try
    FManager.Commit;
  except
    on E: Exception do
      lRaised:=E.ClassType;
  end;
  AssertEquals('Exception reaching the caller','EOPFTransaction',RaisedName(lRaised));
  AssertFalse('Transaction ended after the failing commit',FManager.InTransaction);
end;


initialization
  RegisterTest(TTestCaptureAndCompare);
  RegisterTest(TTestIOPDirtyTracking);
  RegisterTest(TTestSnapshotManager);
  RegisterTest(TTestSnapshotChangeTracker);
  RegisterTest(TTestSelfTracker);
  RegisterTest(TTestSnapshotLifecycle);
  RegisterTest(TTestManagerChangeTracker);
  RegisterTest(TTestTransactionPurge);

end.
