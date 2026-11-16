unit utc.opf.changetracking;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.TypInfo, FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils, typinfo, fpcunit, testregistry,
{$ENDIF}
  Opf.Fields, Opf.ChangeTracking, Opf.Logging;

type

  { TTestOPChangeNames - tests for the TOPChangeNames record }

  TTestOPChangeNames = class(TTestCase)
  published
    procedure FreshRecordIsEmpty;
    procedure AddAppendsAndCountFollows;
    procedure AddIgnoresDuplicate;
    procedure AddIgnoresCaseVariantDuplicate;
    procedure AddIfTrueAppends;
    procedure AddIfFalseDoesNotAppend;
    procedure ToArrayKeepsInsertionOrder;
    procedure ToArrayOnUntouchedRecordIsEmpty;
  end;

  { TTestBuildChangedFieldMapList - tests for BuildChangedFieldMapList }

  TTestBuildChangedFieldMapList = class(TTestCase)
  private
    FFieldMap: TOPFieldMapList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure KeyFieldAlwaysIncluded;
    procedure VersionFieldAlwaysIncluded;
    procedure NamedUpdateFieldIncluded;
    procedure UnnamedUpdateFieldExcluded;
    procedure NamedReadOnlyFieldExcluded;
    procedure NameMatchIsCaseInsensitive;
    procedure UnknownNameIsIgnored;
    procedure EmptyNamesGivesKeyAndVersionOnly;
    procedure ResultIsSeparateFromSource;
    procedure CopiedFieldCarriesSourceData;
  end;

  { TTrackedPerson - tracked test class }

  TTrackedPerson = class(TPersistent)
  private
    FName: String;
    FAge: Integer;
  published
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TTrackedCompany - second tracked test class, same properties, other class }

  TTrackedCompany = class(TPersistent)
  private
    FName: String;
    FAge: Integer;
  published
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

{$M+}

  { TObservedPerson - observable test class counting attach and detach itself }

  TObservedPerson = class(TObject, IFPObserved)
  private
    FName: String;
    FAge: Integer;
    FAttachCount: Integer;
    FDetachCount: Integer;
    FDetachCallCount: Integer;
    FRaiseOnDetach: Boolean;
    FObserver: TObject;
  public
    // Remember aObserver as the single observer of this object.
    procedure FPOAttachObserver(aObserver: TObject);
    // Forget aObserver.
    procedure FPODetachObserver(aObserver: TObject);
    // Send aOperation to the observer, if there is one.
    procedure FPONotifyObservers(aSender: TObject; aOperation: TFPObservedOperation;
      aData: Pointer);
    // Number of FPOAttachObserver calls received.
    property AttachCount: Integer read FAttachCount;
    // Number of FPODetachObserver calls that removed the observer held.
    property DetachCount: Integer read FDetachCount;
    // Number of FPODetachObserver calls received, whoever the observer was.
    property DetachCallCount: Integer read FDetachCallCount;
    // When True, FPODetachObserver raises after it has counted.
    property RaiseOnDetach: Boolean read FRaiseOnDetach write FRaiseOnDetach;
  published
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TObservedCompany - second observable test class, for the reused address }

  TObservedCompany = class(TObservedPerson)
  end;

  { TPlainPerson - test class that does not support IFPObserved }

  TPlainPerson = class(TObject)
  private
    FName: String;
    FAge: Integer;
  published
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  { TPlainCompany - second class without IFPObserved, for the reused address }

  TPlainCompany = class(TObject)
  private
    FName: String;
    FAge: Integer;
  published
    property Name: String read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

{$M-}

  { TTestState - reference state the test tracker keeps per object }

  TTestState = class(TObject)
  private
    FName: String;
    FAge: Integer;
    FCounter: PInteger;
  public
    // Create the state, counting its destruction into aCounter.
    constructor Create(aCounter: PInteger);
    // Count this state as destroyed.
    destructor Destroy; override;
    // Captured value of the Name property.
    property Name: String read FName write FName;
    // Captured value of the Age property.
    property Age: Integer read FAge write FAge;
  end;

  { TTestStateTracker - minimal TOPObjectStateTracker descendant over Name and Age }

  TTestStateTracker = class(TOPObjectStateTracker)
  private
    FLog: TStrings;
    FRefuseCreate: Boolean;
    FRaiseOnRelease: Boolean;
    FStateCounter: PInteger;
    FCreateStateFieldMap: TOPFieldMapList;
    FCompareStateFieldMap: TOPFieldMapList;
  protected
    function CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject; override;
    procedure CompareState(aState: TObject; aObject: TObject;
      aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames); override;
    procedure ReleaseState(aState: TObject; aObject: TObject); override;
  public
    // Receives the captured name of every released state, in call order. Not owned.
    property Log: TStrings read FLog write FLog;
    // When True, CreateState returns Nil and nothing can be tracked.
    property RefuseCreate: Boolean read FRefuseCreate write FRefuseCreate;
    // When True, ReleaseState raises after it has logged.
    property RaiseOnRelease: Boolean read FRaiseOnRelease write FRaiseOnRelease;
    // Counter every state created here counts its destruction into.
    property StateCounter: PInteger read FStateCounter write FStateCounter;
    // The field map last handed to CreateState.
    property CreateStateFieldMap: TOPFieldMapList read FCreateStateFieldMap;
    // The field map last handed to CompareState.
    property CompareStateFieldMap: TOPFieldMapList read FCompareStateFieldMap;
  end;

  { TTestOverrideTracker - descendant overriding all five IOPChangeTracker methods }

  TTestOverrideTracker = class(TTestStateTracker)
  private
    FCaptureRan: Boolean;
    FDiscardRan: Boolean;
    FGetChangesRan: Boolean;
    FMarkDirtyRan: Boolean;
    FPurgeAllRan: Boolean;
  public
    // Record that this override ran, then capture as the base does.
    procedure Capture(aObject: TObject; aFieldMap: TOPFieldMapList); override;
    // Record that this override ran, then discard as the base does.
    procedure Discard(aObject: TObject); override;
    // Record that this override ran, then answer as the base does.
    function GetChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
      out aNames: TUTF8StringArray): Boolean; override;
    // Record that this override ran, then mark the name as the base does.
    procedure MarkDirty(aObject: TObject; const aPropertyName: UTF8String); override;
    // Record that this override ran, then purge as the base does.
    procedure PurgeAll; override;
    // True once the override of Capture has run.
    property CaptureRan: Boolean read FCaptureRan;
    // True once the override of Discard has run.
    property DiscardRan: Boolean read FDiscardRan;
    // True once the override of GetChanges has run.
    property GetChangesRan: Boolean read FGetChangesRan;
    // True once the override of MarkDirty has run.
    property MarkDirtyRan: Boolean read FMarkDirtyRan;
    // True once the override of PurgeAll has run.
    property PurgeAllRan: Boolean read FPurgeAllRan;
  end;

  { TTestTrackerHost - component a tracker can attach itself to }

  TTestTrackerHost = class(TComponent, IOPChangeTrackerHost)
  private
    FAttached: TComponent;
    FAttachCount: Integer;
    FDetachCount: Integer;
  public
    // Remember aProvider as the attached tracker.
    procedure AttachChangeTracker(aProvider: TComponent);
    // Forget the attached tracker.
    procedure DetachChangeTracker;
    // The tracker last attached.
    property Attached: TComponent read FAttached;
    // Number of AttachChangeTracker calls received.
    property AttachCount: Integer read FAttachCount;
    // Number of DetachChangeTracker calls received.
    property DetachCount: Integer read FDetachCount;
  end;

  { TTestObjectStateTracker - tests for the TOPObjectStateTracker table }

  TTestObjectStateTracker = class(TTestCase)
  private
    FTracker: TTestStateTracker;
    FLog: TStringList;
    FPerson: TTrackedPerson;
    FDestroyed: Integer;
    function MakePerson(const aName: String; aAge: Integer): TTrackedPerson;
    function MakeObserved(const aName: String; aAge: Integer): TObservedPerson;
    function CountTracked(const aObjects: array of TObject): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FreshTrackerDefaults;
    procedure UntrackedObjectGivesFalse;
    procedure NilObjectGivesFalse;
    procedure TrackedUnchangedGivesEmptyNames;
    procedure TrackedChangedGivesChangedName;
    procedure MarkedNameIsReported;
    procedure MarkedAndChangedNameAppearsOnce;
    procedure MarkDirtyOnUntrackedDoesNothing;
    procedure ModeNoneCapturesNothing;
    procedure ExcludedClassIsNotCaptured;
    procedure ExplicitModeTracksOnlyRegisteredClass;
    procedure IsClassTrackedFollowsMode;
    procedure UntrackableObjectMakesNoEntry;
    procedure UntrackableObjectDropsExistingEntry;
    procedure ReusedAddressIsDroppedSilently;
    procedure MaxEntriesEvictsOldestAndReleases;
    procedure LoweringMaxEntriesEvictsDownToTheLimit;
    procedure RecaptureReplacesStateAndClearsMarks;
    procedure DiscardReleasesState;
    procedure PurgeReleasesState;
    procedure PurgeReachesTheDiscardOverride;
    procedure PurgeAllReleasesEveryState;
    procedure DestructorReleasesEveryState;
    procedure NilObjectIsIgnoredEverywhere;
    procedure FieldMapReachesBothHooks;
    procedure MarkDirtyThroughInterfaceReachesOverride;
    procedure EveryInterfaceMethodReachesTheOverride;
    procedure FreeingTrackedObjectDropsItsEntry;
    procedure ObservedFreeDropsEntryWithoutDetachOrRelease;
    procedure RepeatedCaptureAttachesOnce;
    procedure UnobservedObjectNotTrackedByDefault;
    procedure UnobservedObjectTrackedWithTrackUnobserved;
    procedure DiscardDetachesTheObserver;
    procedure PurgeDetachesTheObserver;
    procedure PurgeAllDetachesEveryObserver;
    procedure EvictionDetachesOnlyTheEvictedObject;
    procedure DestructorDetachesEveryObserver;
    procedure UnobservedEntryDroppedWhenSwitchedOff;
    procedure ChangeNotificationLeavesTheEntryAlone;
    procedure ReusedAddressOfObservedObjectDoesNotDetach;
    procedure RaisingReleaseStillDetaches;
    procedure RaisingDetachStillReleases;
    procedure RepeatedCaptureOfAPersistentAttachesOnce;
    procedure UntrackableObjectDropDetaches;
    procedure FreeingTrackerDetachesFromARealPersistent;
    procedure UnrelatedNotificationDropsNothing;
    // R2-L17: constant time removal; the table stays consistent when it grows
    procedure ManyEntriesStayConsistent;
  end;

  { TTestObjectStateTrackerManager - tests for the Manager property }

  TTestObjectStateTrackerManager = class(TTestCase)
  private
    FTracker: TTestStateTracker;
    FHost: TTestTrackerHost;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AssigningManagerAttaches;
    procedure AssigningTheSameManagerTwiceAttachesOnce;
    procedure ReplacingManagerDetachesTheOldOne;
    procedure ClearingManagerDetaches;
    procedure FreeingHostClearsManager;
    procedure FreeingTrackerDetachesFromHost;
    procedure FreeingLoggerClearsIt;
  end;

implementation

{ Fill the stack with a non-zero pattern, so a record declared by the caller
  cannot pass a zero test by accident. }

procedure DirtyTheStack;

var
  lBuffer: array[0..511] of Byte;
  lIndex: Integer;

begin
  for lIndex:=Low(lBuffer) to High(lBuffer) do
    lBuffer[lIndex]:=$7F;
  if lBuffer[Low(lBuffer)]<>$7F then
    Raise Exception.Create('stack pattern not written');
end;


{ Declared in a procedure of their own, so their stack frame lands in the
  region DirtyTheStack has just written. }

function FreshRecordCount: Integer;

var
  lNames: TOPChangeNames;

begin
  Result:=lNames.Count;
end;


function FreshRecordArrayLength: Integer;

var
  lNames: TOPChangeNames;

begin
  Result:=Length(lNames.ToArray);
end;


{ Place an instance of aClass in aBuffer, so two classes can be given the same
  address in turn. }

{$push}
{$warn 6058 off}
function InitInBuffer(aClass: TClass; aBuffer: Pointer): TObject;

begin
  Result:=aClass.InitInstance(aBuffer);
end;
{$pop}


function MakeNames(const aNames: array of UTF8String): TUTF8StringArray;

var
  lIndex: Integer;

begin
  Result:=Nil;
  SetLength(Result,Length(aNames));
  for lIndex:=0 to Length(aNames)-1 do
    Result[lIndex]:=aNames[lIndex];
end;


{ TTestOPChangeNames }

procedure TTestOPChangeNames.FreshRecordIsEmpty;

begin
  DirtyTheStack;
  AssertEquals('Fresh record has no names',0,FreshRecordCount);
end;


procedure TTestOPChangeNames.AddAppendsAndCountFollows;

var
  lNames: TOPChangeNames;

begin
  lNames.Add('Name');
  AssertEquals('One name',1,lNames.Count);
  lNames.Add('Age');
  AssertEquals('Two names',2,lNames.Count);
end;


procedure TTestOPChangeNames.AddIgnoresDuplicate;

var
  lNames: TOPChangeNames;

begin
  lNames.Add('Name');
  lNames.Add('Name');
  AssertEquals('Duplicate not appended',1,lNames.Count);
end;


procedure TTestOPChangeNames.AddIgnoresCaseVariantDuplicate;

var
  lNames: TOPChangeNames;

begin
  lNames.Add('Name');
  lNames.Add('name');
  AssertEquals('Case variant not appended',1,lNames.Count);
  AssertEquals('First spelling kept','Name',lNames.ToArray[0]);
end;


procedure TTestOPChangeNames.AddIfTrueAppends;

var
  lNames: TOPChangeNames;

begin
  lNames.AddIf(True,'Name');
  AssertEquals('Name appended',1,lNames.Count);
  AssertEquals('Appended name','Name',lNames.ToArray[0]);
end;


procedure TTestOPChangeNames.AddIfFalseDoesNotAppend;

var
  lNames: TOPChangeNames;

begin
  lNames.AddIf(False,'Name');
  AssertEquals('Nothing appended',0,lNames.Count);
end;


procedure TTestOPChangeNames.ToArrayKeepsInsertionOrder;

var
  lNames: TOPChangeNames;
  lResult: TUTF8StringArray;

begin
  lNames.Add('Age');
  lNames.Add('Name');
  lNames.Add('City');
  lResult:=lNames.ToArray;
  AssertEquals('Three names',3,Length(lResult));
  AssertEquals('First name','Age',lResult[0]);
  AssertEquals('Second name','Name',lResult[1]);
  AssertEquals('Third name','City',lResult[2]);
end;


procedure TTestOPChangeNames.ToArrayOnUntouchedRecordIsEmpty;

begin
  DirtyTheStack;
  AssertEquals('Empty array',0,FreshRecordArrayLength);
end;


{ TTestBuildChangedFieldMapList }

procedure TTestBuildChangedFieldMapList.SetUp;

var
  lField: TOPFieldMap;

begin
  FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
  lField:=FFieldMap.AddField('id','ID',[foInKey,foInSelect]);
  lField.PropertyKind:=tkInteger;
  lField:=FFieldMap.AddField('name','Name',[foInSelect,foInInsert,foInUpdate]);
  lField.PropertyKind:=tkAString;
  lField:=FFieldMap.AddField('age','Age',[foInSelect,foInInsert,foInUpdate]);
  lField.PropertyKind:=tkInteger;
  lField:=FFieldMap.AddField('version','Version',[]);
  lField.PropertyKind:=tkInteger;
  lField.Options:=[foInSelect,foInInsert,foInUpdate,foVersion];
  lField:=FFieldMap.AddField('created','Created',[foInSelect]);
  lField.PropertyKind:=tkFloat;
end;


procedure TTestBuildChangedFieldMapList.TearDown;

begin
  FreeAndNil(FFieldMap);
end;


procedure TTestBuildChangedFieldMapList.KeyFieldAlwaysIncluded;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames([]));
  try
    AssertNotNull('Key field present',lResult.FindByPropertyName('ID'));
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.VersionFieldAlwaysIncluded;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['Name']));
  try
    AssertNotNull('Version field present',lResult.FindByPropertyName('Version'));
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.NamedUpdateFieldIncluded;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['Name']));
  try
    AssertNotNull('Named field present',lResult.FindByPropertyName('Name'));
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.UnnamedUpdateFieldExcluded;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['Name']));
  try
    AssertNull('Unnamed field absent',lResult.FindByPropertyName('Age'));
    AssertEquals('Key, version and named field',3,lResult.Count);
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.NamedReadOnlyFieldExcluded;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['Created']));
  try
    AssertNull('Field without foInUpdate absent',lResult.FindByPropertyName('Created'));
    AssertEquals('Key and version only',2,lResult.Count);
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.NameMatchIsCaseInsensitive;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['name']));
  try
    AssertNotNull('Field matched regardless of case',lResult.FindByPropertyName('Name'));
    AssertEquals('Key, version and matched field',3,lResult.Count);
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.UnknownNameIsIgnored;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['NoSuchProperty']));
  try
    AssertEquals('Key and version only',2,lResult.Count);
    AssertEquals('First is ID','ID',lResult[0].PropertyName);
    AssertEquals('Second is Version','Version',lResult[1].PropertyName);
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.EmptyNamesGivesKeyAndVersionOnly;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames([]));
  try
    AssertEquals('Key and version only',2,lResult.Count);
    AssertEquals('First is ID','ID',lResult[0].PropertyName);
    AssertEquals('Second is Version','Version',lResult[1].PropertyName);
  finally
    lResult.Free;
  end;
end;


procedure TTestBuildChangedFieldMapList.ResultIsSeparateFromSource;

var
  lResult: TOPFieldMapList;

begin
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['Name']));
  try
    AssertTrue('Result is a new list',lResult<>FFieldMap);
    AssertTrue('Copied field is not the source field',
      lResult.FindByPropertyName('Name')<>FFieldMap.FindByPropertyName('Name'));
  finally
    lResult.Free;
  end;
  AssertEquals('Source field count unchanged',5,FFieldMap.Count);
  AssertEquals('Source still has Age','Age',FFieldMap[2].PropertyName);
end;


procedure TTestBuildChangedFieldMapList.CopiedFieldCarriesSourceData;

var
  lResult: TOPFieldMapList;
  lSource, lCopy: TOPFieldMap;

begin
  lSource:=FFieldMap.FindByPropertyName('Name');
  lResult:=BuildChangedFieldMapList(FFieldMap,MakeNames(['Name']));
  try
    lCopy:=lResult.FindByPropertyName('Name');
    AssertNotNull('Copied field present',lCopy);
    AssertEquals('Field name copied',lSource.FieldName,lCopy.FieldName);
    AssertEquals('Property name copied',lSource.PropertyName,lCopy.PropertyName);
    AssertTrue('Property kind copied',lCopy.PropertyKind=lSource.PropertyKind);
    AssertTrue('Options copied',lCopy.Options=lSource.Options);
  finally
    lResult.Free;
  end;
end;


{ TTestState }

constructor TTestState.Create(aCounter: PInteger);

begin
  inherited Create;
  FCounter:=aCounter;
end;


destructor TTestState.Destroy;

begin
  if Assigned(FCounter) then
    Inc(FCounter^);
  inherited Destroy;
end;


{ TObservedPerson }

procedure TObservedPerson.FPOAttachObserver(aObserver: TObject);

begin
  FObserver:=aObserver;
  Inc(FAttachCount);
end;


{ DetachCallCount counts every call, so a detach aimed at an observer this
  object never held is still visible; DetachCount counts only the removal of
  the observer actually held. }

procedure TObservedPerson.FPODetachObserver(aObserver: TObject);

begin
  Inc(FDetachCallCount);
  if FObserver=aObserver then
    begin
    FObserver:=Nil;
    Inc(FDetachCount);
    end;
  if FRaiseOnDetach then
    Raise Exception.Create('FPODetachObserver refused');
end;


procedure TObservedPerson.FPONotifyObservers(aSender: TObject;
  aOperation: TFPObservedOperation; aData: Pointer);

var
  lObserver: IFPObserver;

begin
  if not Assigned(FObserver) then
    Exit;
  if FObserver.GetInterface(SGUIDObserver,lObserver) then
    lObserver.FPOObservedChanged(aSender,aOperation,aData);
end;


{ TTestStateTracker }

function TTestStateTracker.CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject;

var
  lState: TTestState;

begin
  Result:=Nil;
  FCreateStateFieldMap:=aFieldMap;
  if FRefuseCreate then
    Exit;
  lState:=TTestState.Create(FStateCounter);
  lState.Name:=GetStrProp(aObject,'Name');
  lState.Age:=GetOrdProp(aObject,'Age');
  Result:=lState;
end;


procedure TTestStateTracker.CompareState(aState: TObject; aObject: TObject;
  aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames);

var
  lState: TTestState;

begin
  FCompareStateFieldMap:=aFieldMap;
  lState:=TTestState(aState);
  aNames.AddIf(lState.Name<>GetStrProp(aObject,'Name'),'Name');
  aNames.AddIf(lState.Age<>GetOrdProp(aObject,'Age'),'Age');
end;


{ Logs the captured name, which stays readable when the tracked object is gone. }

procedure TTestStateTracker.ReleaseState(aState: TObject; aObject: TObject);

begin
  inherited ReleaseState(aState,aObject);
  if Assigned(FLog) then
    FLog.Add(TTestState(aState).Name);
  if FRaiseOnRelease then
    Raise Exception.Create('ReleaseState refused');
end;


{ TTestOverrideTracker }

procedure TTestOverrideTracker.Capture(aObject: TObject; aFieldMap: TOPFieldMapList);

begin
  FCaptureRan:=True;
  inherited Capture(aObject,aFieldMap);
end;


procedure TTestOverrideTracker.Discard(aObject: TObject);

begin
  FDiscardRan:=True;
  inherited Discard(aObject);
end;


function TTestOverrideTracker.GetChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
  out aNames: TUTF8StringArray): Boolean;

begin
  FGetChangesRan:=True;
  Result:=inherited GetChanges(aObject,aFieldMap,aNames);
end;


procedure TTestOverrideTracker.MarkDirty(aObject: TObject; const aPropertyName: UTF8String);

begin
  FMarkDirtyRan:=True;
  inherited MarkDirty(aObject,aPropertyName);
end;


procedure TTestOverrideTracker.PurgeAll;

begin
  FPurgeAllRan:=True;
  inherited PurgeAll;
end;


{ TTestTrackerHost }

procedure TTestTrackerHost.AttachChangeTracker(aProvider: TComponent);

begin
  FAttached:=aProvider;
  Inc(FAttachCount);
end;


procedure TTestTrackerHost.DetachChangeTracker;

begin
  FAttached:=Nil;
  Inc(FDetachCount);
end;


{ TTestObjectStateTracker }

procedure TTestObjectStateTracker.SetUp;

begin
  FDestroyed:=0;
  FLog:=TStringList.Create;
  FTracker:=TTestStateTracker.Create(Nil);
  FTracker.Log:=FLog;
  FTracker.StateCounter:=@FDestroyed;
  FPerson:=MakePerson('Alice',30);
end;


procedure TTestObjectStateTracker.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FPerson);
  FreeAndNil(FLog);
end;


function TTestObjectStateTracker.MakePerson(const aName: String; aAge: Integer): TTrackedPerson;

begin
  Result:=TTrackedPerson.Create;
  Result.Name:=aName;
  Result.Age:=aAge;
end;


function TTestObjectStateTracker.MakeObserved(const aName: String; aAge: Integer): TObservedPerson;

begin
  Result:=TObservedPerson.Create;
  Result.Name:=aName;
  Result.Age:=aAge;
end;


function TTestObjectStateTracker.CountTracked(const aObjects: array of TObject): Integer;

var
  lIndex: Integer;
  lNames: TUTF8StringArray;

begin
  Result:=0;
  for lIndex:=0 to Length(aObjects)-1 do
    if FTracker.GetChanges(aObjects[lIndex],Nil,lNames) then
      Inc(Result);
end;


procedure TTestObjectStateTracker.FreshTrackerDefaults;

begin
  AssertTrue('Mode is tmImplicit',FTracker.Mode=tmImplicit);
  AssertEquals('MaxEntries is zero',0,FTracker.MaxEntries);
  AssertFalse('TrackUnobserved is off',FTracker.TrackUnobserved);
end;


procedure TTestObjectStateTracker.UntrackedObjectGivesFalse;

var
  lNames: TUTF8StringArray;

begin
  AssertFalse('Object never captured',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('No names',0,Length(lNames));
end;


procedure TTestObjectStateTracker.NilObjectGivesFalse;

var
  lNames: TUTF8StringArray;

begin
  AssertFalse('Nil object',FTracker.GetChanges(Nil,Nil,lNames));
  AssertEquals('No names',0,Length(lNames));
end;


procedure TTestObjectStateTracker.TrackedUnchangedGivesEmptyNames;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  AssertTrue('Object tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('Nothing changed',0,Length(lNames));
end;


procedure TTestObjectStateTracker.TrackedChangedGivesChangedName;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FPerson.Age:=31;
  AssertTrue('Object tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('One changed name',1,Length(lNames));
  AssertEquals('Changed name','Age',lNames[0]);
end;


procedure TTestObjectStateTracker.MarkedNameIsReported;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FTracker.MarkDirty(FPerson,'Age');
  AssertTrue('Object tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('One name',1,Length(lNames));
  AssertEquals('Marked name','Age',lNames[0]);
end;


procedure TTestObjectStateTracker.MarkedAndChangedNameAppearsOnce;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FPerson.Age:=31;
  FTracker.MarkDirty(FPerson,'Age');
  AssertTrue('Object tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('Name appears once',1,Length(lNames));
  AssertEquals('The name','Age',lNames[0]);
end;


procedure TTestObjectStateTracker.MarkDirtyOnUntrackedDoesNothing;

var
  lNames: TUTF8StringArray;

begin
  FTracker.MarkDirty(FPerson,'Age');
  AssertFalse('No entry created',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('No names',0,Length(lNames));
end;


procedure TTestObjectStateTracker.ModeNoneCapturesNothing;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Mode:=tmNone;
  FTracker.Capture(FPerson,Nil);
  AssertFalse('Nothing captured',FTracker.GetChanges(FPerson,Nil,lNames));
end;


procedure TTestObjectStateTracker.ExcludedClassIsNotCaptured;

var
  lCompany: TTrackedCompany;
  lNames: TUTF8StringArray;

begin
  FTracker.ExcludeClass(TTrackedPerson);
  FTracker.Capture(FPerson,Nil);
  AssertFalse('Excluded class not captured',FTracker.GetChanges(FPerson,Nil,lNames));
  lCompany:=TTrackedCompany.Create;
  try
    lCompany.Name:='ACME';
    FTracker.Capture(lCompany,Nil);
    AssertTrue('Other class still captured',FTracker.GetChanges(lCompany,Nil,lNames));
  finally
    FTracker.Purge(lCompany);
    lCompany.Free;
  end;
end;


procedure TTestObjectStateTracker.ExplicitModeTracksOnlyRegisteredClass;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Mode:=tmExplicit;
  FTracker.Capture(FPerson,Nil);
  AssertFalse('Unregistered class not captured',FTracker.GetChanges(FPerson,Nil,lNames));
  FTracker.RegisterClass(TTrackedPerson);
  FTracker.Capture(FPerson,Nil);
  AssertTrue('Registered class captured',FTracker.GetChanges(FPerson,Nil,lNames));
end;


procedure TTestObjectStateTracker.IsClassTrackedFollowsMode;

begin
  AssertTrue('Implicit mode tracks any class',FTracker.IsClassTracked(TTrackedPerson));
  FTracker.ExcludeClass(TTrackedPerson);
  AssertFalse('Excluded class not tracked',FTracker.IsClassTracked(TTrackedPerson));
  AssertTrue('Other class still tracked',FTracker.IsClassTracked(TTrackedCompany));
  FTracker.Mode:=tmExplicit;
  AssertFalse('Explicit mode needs registration',FTracker.IsClassTracked(TTrackedCompany));
  FTracker.RegisterClass(TTrackedCompany);
  AssertTrue('Registered class tracked',FTracker.IsClassTracked(TTrackedCompany));
  FTracker.Mode:=tmNone;
  AssertFalse('No class tracked',FTracker.IsClassTracked(TTrackedCompany));
end;


procedure TTestObjectStateTracker.UntrackableObjectMakesNoEntry;

var
  lNames: TUTF8StringArray;

begin
  FTracker.RefuseCreate:=True;
  FTracker.Capture(FPerson,Nil);
  AssertFalse('No entry made',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('Nothing released',0,FLog.Count);
end;


procedure TTestObjectStateTracker.UntrackableObjectDropsExistingEntry;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FTracker.RefuseCreate:=True;
  FTracker.Capture(FPerson,Nil);
  AssertFalse('Existing entry dropped',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('One state released',1,FLog.Count);
  AssertEquals('Released state','Alice',FLog[0]);
end;


{ Two classes are placed in the same manually allocated buffer in turn, so the
  reused address is certain rather than left to the heap. Only objects that
  cannot be observed are at risk, so they are tracked with TrackUnobserved. }

procedure TTestObjectStateTracker.ReusedAddressIsDroppedSilently;

var
  lBuffer: Pointer;
  lSize: SizeInt;
  lPerson: TPlainPerson;
  lCompany: TPlainCompany;
  lNames: TUTF8StringArray;

begin
  FTracker.TrackUnobserved:=True;
  lSize:=TPlainPerson.InstanceSize;
  if TPlainCompany.InstanceSize>lSize then
    lSize:=TPlainCompany.InstanceSize;
  GetMem(lBuffer,lSize);
  try
    lPerson:=TPlainPerson(InitInBuffer(TPlainPerson,lBuffer));
    lPerson.Name:='Alice';
    lPerson.Age:=30;
    FTracker.Capture(lPerson,Nil);
    AssertTrue('Person tracked',FTracker.GetChanges(lPerson,Nil,lNames));
    lPerson.CleanupInstance;
    lCompany:=TPlainCompany(InitInBuffer(TPlainCompany,lBuffer));
    lCompany.Name:='ACME';
    lCompany.Age:=1;
    AssertFalse('Reused address not tracked',FTracker.GetChanges(lCompany,Nil,lNames));
    AssertEquals('No names',0,Length(lNames));
    AssertEquals('ReleaseState not called',0,FLog.Count);
    AssertEquals('Dropped state destroyed',1,FDestroyed);
    AssertFalse('Entry really dropped',FTracker.GetChanges(lCompany,Nil,lNames));
    lCompany.CleanupInstance;
  finally
    FreeMem(lBuffer);
  end;
end;


procedure TTestObjectStateTracker.ManyEntriesStayConsistent;

const
  Count = 5000;

var
  lPeople: array of TTrackedPerson;
  lIndex, lTracked: Integer;
  lNames: TUTF8StringArray;

begin
  lPeople:=Nil;
  SetLength(lPeople,Count);
  try
    for lIndex:=0 to Count-1 do
      begin
      lPeople[lIndex]:=MakePerson('P'+IntToStr(lIndex),lIndex);
      FTracker.Capture(lPeople[lIndex],Nil);
      end;
    // Drop every third one, from the end, the start and the middle
    lIndex:=Count-1;
    while lIndex>=0 do
      begin
      FTracker.Discard(lPeople[lIndex]);
      Dec(lIndex,3);
      end;
    lTracked:=0;
    for lIndex:=0 to Count-1 do
      if FTracker.GetChanges(lPeople[lIndex],Nil,lNames) then
        begin
        Inc(lTracked);
        AssertTrue('Not a discarded one',((Count-1-lIndex) mod 3)<>0);
        end;
    AssertEquals('Tracked',Count-(Count+2) div 3,lTracked);
    // Eviction drops the oldest remaining entries first
    FLog.Clear;
    FTracker.MaxEntries:=1;
    FTracker.Capture(lPeople[Count-1],Nil);
    AssertEquals('Oldest evicted first','P0',FLog[0]);
    AssertTrue('Newest kept',FTracker.GetChanges(lPeople[Count-1],Nil,lNames));
    AssertFalse('Others evicted',FTracker.GetChanges(lPeople[Count-2],Nil,lNames));
  finally
    FTracker.PurgeAll;
    for lIndex:=0 to Count-1 do
      lPeople[lIndex].Free;
  end;
end;


procedure TTestObjectStateTracker.MaxEntriesEvictsOldestAndReleases;

var
  lSecond, lThird: TTrackedPerson;
  lNames: TUTF8StringArray;

begin
  FTracker.MaxEntries:=2;
  lSecond:=MakePerson('Bob',40);
  try
    lThird:=MakePerson('Carol',50);
    try
      FTracker.Capture(FPerson,Nil);
      FTracker.Capture(lSecond,Nil);
      FTracker.Capture(lThird,Nil);
      AssertFalse('Oldest evicted',FTracker.GetChanges(FPerson,Nil,lNames));
      AssertTrue('Second still tracked',FTracker.GetChanges(lSecond,Nil,lNames));
      AssertTrue('Third tracked',FTracker.GetChanges(lThird,Nil,lNames));
      AssertEquals('One state released',1,FLog.Count);
      AssertEquals('Released state','Alice',FLog[0]);
    finally
      FTracker.PurgeAll;
      lThird.Free;
    end;
  finally
    lSecond.Free;
  end;
end;


{ Lowering MaxEntries below the number of entries held must bring the table down
  to the limit, not drop a single entry per capture. }

procedure TTestObjectStateTracker.LoweringMaxEntriesEvictsDownToTheLimit;

var
  lPeople: array[0..4] of TTrackedPerson;
  lExtra: TTrackedPerson;
  lIndex: Integer;
  lNames: TUTF8StringArray;

begin
  lExtra:=Nil;
  for lIndex:=Low(lPeople) to High(lPeople) do
    lPeople[lIndex]:=Nil;
  try
    for lIndex:=Low(lPeople) to High(lPeople) do
      begin
      lPeople[lIndex]:=MakePerson('P'+IntToStr(lIndex),lIndex);
      FTracker.Capture(lPeople[lIndex],Nil);
      end;
    AssertEquals('Five entries held',5,CountTracked([lPeople[0],lPeople[1],lPeople[2],
      lPeople[3],lPeople[4]]));
    FTracker.MaxEntries:=2;
    lExtra:=MakePerson('Extra',60);
    FTracker.Capture(lExtra,Nil);
    AssertEquals('Only two entries survive',2,CountTracked([lPeople[0],lPeople[1],
      lPeople[2],lPeople[3],lPeople[4],lExtra]));
    AssertTrue('Newest kept',FTracker.GetChanges(lExtra,Nil,lNames));
    AssertTrue('Second newest kept',FTracker.GetChanges(lPeople[4],Nil,lNames));
    AssertEquals('Four states released',4,FLog.Count);
    AssertEquals('Oldest released first','P0',FLog[0]);
    AssertEquals('Then the next oldest','P1',FLog[1]);
    AssertEquals('Four states destroyed',4,FDestroyed);
  finally
    FTracker.PurgeAll;
    lExtra.Free;
    for lIndex:=Low(lPeople) to High(lPeople) do
      lPeople[lIndex].Free;
  end;
end;


procedure TTestObjectStateTracker.RecaptureReplacesStateAndClearsMarks;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FTracker.MarkDirty(FPerson,'Age');
  FPerson.Age:=31;
  FTracker.Capture(FPerson,Nil);
  AssertTrue('Object tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('New state, marks cleared',0,Length(lNames));
  AssertEquals('ReleaseState not called on replacement',0,FLog.Count);
  AssertEquals('Old state destroyed',1,FDestroyed);
end;


procedure TTestObjectStateTracker.DiscardReleasesState;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FTracker.Discard(FPerson);
  AssertFalse('No longer tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('One state released',1,FLog.Count);
  AssertEquals('Released state','Alice',FLog[0]);
end;


procedure TTestObjectStateTracker.PurgeReleasesState;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(FPerson,Nil);
  FTracker.Purge(FPerson);
  AssertFalse('No longer tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  AssertEquals('One state released',1,FLog.Count);
end;


{ Purge is not virtual, so it must reach a descendant through Discard. }

procedure TTestObjectStateTracker.PurgeReachesTheDiscardOverride;

var
  lTracker: TTestOverrideTracker;
  lNames: TUTF8StringArray;

begin
  lTracker:=TTestOverrideTracker.Create(Nil);
  try
    lTracker.Log:=FLog;
    lTracker.StateCounter:=@FDestroyed;
    lTracker.Capture(FPerson,Nil);
    lTracker.Purge(FPerson);
    AssertTrue('Discard override ran',lTracker.DiscardRan);
    AssertFalse('Entry gone',lTracker.GetChanges(FPerson,Nil,lNames));
    AssertEquals('One state released',1,FLog.Count);
  finally
    lTracker.Free;
  end;
end;


procedure TTestObjectStateTracker.PurgeAllReleasesEveryState;

var
  lSecond: TTrackedPerson;
  lNames: TUTF8StringArray;

begin
  lSecond:=MakePerson('Bob',40);
  try
    FTracker.Capture(FPerson,Nil);
    FTracker.Capture(lSecond,Nil);
    FTracker.PurgeAll;
    AssertFalse('First untracked',FTracker.GetChanges(FPerson,Nil,lNames));
    AssertFalse('Second untracked',FTracker.GetChanges(lSecond,Nil,lNames));
    AssertEquals('Two states released',2,FLog.Count);
    AssertEquals('Two states destroyed',2,FDestroyed);
  finally
    lSecond.Free;
  end;
end;


procedure TTestObjectStateTracker.DestructorReleasesEveryState;

var
  lTracker: TTestStateTracker;
  lSecond: TTrackedPerson;

begin
  lSecond:=MakePerson('Bob',40);
  try
    lTracker:=TTestStateTracker.Create(Nil);
    try
      lTracker.Log:=FLog;
      lTracker.StateCounter:=@FDestroyed;
      lTracker.Capture(FPerson,Nil);
      lTracker.Capture(lSecond,Nil);
    finally
      lTracker.Free;
    end;
    AssertEquals('Two states released',2,FLog.Count);
    AssertTrue('First state released',FLog.IndexOf('Alice')>=0);
    AssertTrue('Second state released',FLog.IndexOf('Bob')>=0);
    AssertEquals('Two states destroyed',2,FDestroyed);
  finally
    lSecond.Free;
  end;
end;


procedure TTestObjectStateTracker.NilObjectIsIgnoredEverywhere;

var
  lNames: TUTF8StringArray;

begin
  FTracker.Capture(Nil,Nil);
  FTracker.Discard(Nil);
  FTracker.Purge(Nil);
  FTracker.MarkDirty(Nil,'Name');
  AssertFalse('Nil is not tracked',FTracker.GetChanges(Nil,Nil,lNames));
  AssertEquals('No names',0,Length(lNames));
  AssertEquals('Nothing released',0,FLog.Count);
  FTracker.PurgeAll;
  AssertEquals('Table still empty',0,FLog.Count);
  AssertEquals('No state destroyed',0,FDestroyed);
end;


procedure TTestObjectStateTracker.FieldMapReachesBothHooks;

var
  lMap: TOPFieldMapList;
  lNames: TUTF8StringArray;

begin
  lMap:=TOPFieldMapList.Create(TOPFieldMap);
  try
    lMap.AddField('name','Name',[foInSelect,foInUpdate]);
    FTracker.Capture(FPerson,lMap);
    AssertSame('CreateState received the field map',lMap,FTracker.CreateStateFieldMap);
    AssertTrue('Object tracked',FTracker.GetChanges(FPerson,lMap,lNames));
    AssertSame('CompareState received the field map',lMap,FTracker.CompareStateFieldMap);
    AssertEquals('Field map unchanged',1,lMap.Count);
    AssertEquals('Field name unchanged','name',lMap[0].FieldName);
    AssertEquals('Property name unchanged','Name',lMap[0].PropertyName);
  finally
    lMap.Free;
  end;
end;


{ A call through IOPChangeTracker must reach the descendant's override, which
  only happens when the base declares the method virtual. }

procedure TTestObjectStateTracker.MarkDirtyThroughInterfaceReachesOverride;

var
  lTracker: TTestOverrideTracker;
  lTrackerIntf: IOPChangeTracker;
  lNames: TUTF8StringArray;

begin
  lTracker:=TTestOverrideTracker.Create(Nil);
  try
    lTracker.Log:=FLog;
    lTracker.Capture(FPerson,Nil);
    AssertTrue('Tracker offers the interface',
      lTracker.GetInterface(SGUIDIOPChangeTracker,lTrackerIntf));
    lTrackerIntf.MarkDirty(FPerson,'Age');
    AssertTrue('Descendant override ran',lTracker.MarkDirtyRan);
    AssertTrue('Object tracked',lTrackerIntf.GetChanges(FPerson,Nil,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Marked name','Age',lNames[0]);
  finally
    lTracker.Free;
  end;
end;


procedure TTestObjectStateTracker.EveryInterfaceMethodReachesTheOverride;

var
  lTracker: TTestOverrideTracker;
  lTrackerIntf: IOPChangeTracker;
  lNames: TUTF8StringArray;

begin
  lTracker:=TTestOverrideTracker.Create(Nil);
  try
    lTracker.Log:=FLog;
    lTracker.StateCounter:=@FDestroyed;
    AssertTrue('Tracker offers the interface',
      lTracker.GetInterface(SGUIDIOPChangeTracker,lTrackerIntf));
    lTrackerIntf.Capture(FPerson,Nil);
    AssertTrue('Capture override ran',lTracker.CaptureRan);
    AssertTrue('Object tracked',lTrackerIntf.GetChanges(FPerson,Nil,lNames));
    AssertTrue('GetChanges override ran',lTracker.GetChangesRan);
    lTrackerIntf.MarkDirty(FPerson,'Age');
    AssertTrue('MarkDirty override ran',lTracker.MarkDirtyRan);
    lTrackerIntf.Discard(FPerson);
    AssertTrue('Discard override ran',lTracker.DiscardRan);
    lTrackerIntf.Capture(FPerson,Nil);
    lTrackerIntf.PurgeAll;
    AssertTrue('PurgeAll override ran',lTracker.PurgeAllRan);
    AssertFalse('Table emptied',lTrackerIntf.GetChanges(FPerson,Nil,lNames));
  finally
    lTracker.Free;
  end;
end;


{ TTrackedPerson descends from TPersistent, so freeing it runs the FCL
  notification path end to end. }

procedure TTestObjectStateTracker.FreeingTrackedObjectDropsItsEntry;

var
  lSecond: TTrackedPerson;
  lNames: TUTF8StringArray;

begin
  lSecond:=MakePerson('Bob',40);
  FTracker.Capture(FPerson,Nil);
  FTracker.Capture(lSecond,Nil);
  FreeAndNil(lSecond);
  AssertEquals('State of the freed object destroyed',1,FDestroyed);
  AssertEquals('ReleaseState not called',0,FLog.Count);
  AssertTrue('Other object still tracked',FTracker.GetChanges(FPerson,Nil,lNames));
  FTracker.PurgeAll;
  AssertEquals('Only the surviving state released',1,FLog.Count);
  AssertEquals('Released state','Alice',FLog[0]);
end;


procedure TTestObjectStateTracker.ObservedFreeDropsEntryWithoutDetachOrRelease;

var
  lObserved: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    AssertEquals('Attached once',1,lObserved.AttachCount);
    lObserved.FPONotifyObservers(lObserved,ooFree,Nil);
    AssertFalse('Entry dropped',FTracker.GetChanges(lObserved,Nil,lNames));
    AssertEquals('Not detached',0,lObserved.DetachCallCount);
    AssertEquals('ReleaseState not called',0,FLog.Count);
    AssertEquals('State destroyed',1,FDestroyed);
  finally
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


procedure TTestObjectStateTracker.RepeatedCaptureAttachesOnce;

var
  lObserved: TObservedPerson;
  lIndex: Integer;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    for lIndex:=1 to 5 do
      FTracker.Capture(lObserved,Nil);
    AssertEquals('Attached once',1,lObserved.AttachCount);
    AssertTrue('Object tracked',FTracker.GetChanges(lObserved,Nil,lNames));
    AssertEquals('Four replaced states destroyed',4,FDestroyed);
    AssertEquals('No replacement released',0,FLog.Count);
    FTracker.PurgeAll;
    AssertEquals('One state released',1,FLog.Count);
    AssertEquals('Every state destroyed',5,FDestroyed);
    AssertEquals('Detached once',1,lObserved.DetachCount);
  finally
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


{ An object that cannot report being freed gets no reference state: it is
  then written with all its columns. }

procedure TTestObjectStateTracker.UnobservedObjectNotTrackedByDefault;

var
  lPlain: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPlain:=TPlainPerson.Create;
  try
    FTracker.Capture(lPlain,Nil);
    AssertFalse('Not tracked',FTracker.GetChanges(lPlain,Nil,lNames));
    AssertEquals('Nothing released',0,FLog.Count);
  finally
    lPlain.Free;
  end;
end;


procedure TTestObjectStateTracker.UnobservedObjectTrackedWithTrackUnobserved;

var
  lPlain: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  FTracker.TrackUnobserved:=True;
  lPlain:=TPlainPerson.Create;
  try
    lPlain.Name:='Alice';
    lPlain.Age:=30;
    FTracker.Capture(lPlain,Nil);
    AssertTrue('Object tracked',FTracker.GetChanges(lPlain,Nil,lNames));
    AssertEquals('Nothing changed',0,Length(lNames));
    lPlain.Age:=31;
    AssertTrue('Object still tracked',FTracker.GetChanges(lPlain,Nil,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Changed name','Age',lNames[0]);
    FTracker.Discard(lPlain);
    AssertFalse('No longer tracked',FTracker.GetChanges(lPlain,Nil,lNames));
    AssertEquals('One state released',1,FLog.Count);
    FTracker.Capture(lPlain,Nil);
    FTracker.Purge(lPlain);
    AssertFalse('Purged',FTracker.GetChanges(lPlain,Nil,lNames));
    AssertEquals('Two states released',2,FLog.Count);
  finally
    lPlain.Free;
  end;
end;


procedure TTestObjectStateTracker.DiscardDetachesTheObserver;

var
  lObserved: TObservedPerson;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    FTracker.Discard(lObserved);
    AssertEquals('Detached once',1,lObserved.DetachCount);
    AssertEquals('One state released',1,FLog.Count);
  finally
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


procedure TTestObjectStateTracker.PurgeDetachesTheObserver;

var
  lObserved: TObservedPerson;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    FTracker.Purge(lObserved);
    AssertEquals('Detached once',1,lObserved.DetachCount);
    AssertEquals('One state released',1,FLog.Count);
  finally
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


procedure TTestObjectStateTracker.PurgeAllDetachesEveryObserver;

var
  lFirst, lSecond: TObservedPerson;

begin
  lFirst:=MakeObserved('Alice',30);
  try
    lSecond:=MakeObserved('Bob',40);
    try
      FTracker.Capture(lFirst,Nil);
      FTracker.Capture(lSecond,Nil);
      FTracker.PurgeAll;
      AssertEquals('First detached',1,lFirst.DetachCount);
      AssertEquals('Second detached',1,lSecond.DetachCount);
      AssertEquals('Two states released',2,FLog.Count);
    finally
      FTracker.PurgeAll;
      lSecond.Free;
    end;
  finally
    lFirst.Free;
  end;
end;


procedure TTestObjectStateTracker.EvictionDetachesOnlyTheEvictedObject;

var
  lFirst, lSecond: TObservedPerson;

begin
  lFirst:=MakeObserved('Alice',30);
  try
    lSecond:=MakeObserved('Bob',40);
    try
      FTracker.MaxEntries:=1;
      FTracker.Capture(lFirst,Nil);
      FTracker.Capture(lSecond,Nil);
      AssertEquals('Evicted object detached',1,lFirst.DetachCount);
      AssertEquals('Kept object not detached',0,lSecond.DetachCallCount);
      AssertEquals('One state released',1,FLog.Count);
      AssertEquals('Released state','Alice',FLog[0]);
    finally
      FTracker.PurgeAll;
      lSecond.Free;
    end;
  finally
    lFirst.Free;
  end;
end;


procedure TTestObjectStateTracker.DestructorDetachesEveryObserver;

var
  lTracker: TTestStateTracker;
  lFirst, lSecond: TObservedPerson;

begin
  lFirst:=MakeObserved('Alice',30);
  try
    lSecond:=MakeObserved('Bob',40);
    try
      lTracker:=TTestStateTracker.Create(Nil);
      try
        lTracker.Log:=FLog;
        lTracker.StateCounter:=@FDestroyed;
        lTracker.Capture(lFirst,Nil);
        lTracker.Capture(lSecond,Nil);
      finally
        lTracker.Free;
      end;
      AssertEquals('First detached',1,lFirst.DetachCount);
      AssertEquals('Second detached',1,lSecond.DetachCount);
      AssertEquals('Two states released',2,FLog.Count);
    finally
      lSecond.Free;
    end;
  finally
    lFirst.Free;
  end;
end;


{ A state taken with TrackUnobserved is released by the next capture once it
  is switched off. }

procedure TTestObjectStateTracker.UnobservedEntryDroppedWhenSwitchedOff;

var
  lPlain: TPlainPerson;
  lNames: TUTF8StringArray;

begin
  lPlain:=TPlainPerson.Create;
  try
    FTracker.TrackUnobserved:=True;
    FTracker.Capture(lPlain,Nil);
    AssertTrue('Tracked',FTracker.GetChanges(lPlain,Nil,lNames));
    FTracker.TrackUnobserved:=False;
    FTracker.Capture(lPlain,Nil);
    AssertFalse('No longer tracked',FTracker.GetChanges(lPlain,Nil,lNames));
    AssertEquals('State released',1,FLog.Count);
  finally
    lPlain.Free;
  end;
end;


procedure TTestObjectStateTracker.ChangeNotificationLeavesTheEntryAlone;

var
  lObserved: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    lObserved.Age:=31;
    lObserved.FPONotifyObservers(lObserved,ooChange,Nil);
    AssertTrue('Still tracked',FTracker.GetChanges(lObserved,Nil,lNames));
    AssertEquals('One name',1,Length(lNames));
    AssertEquals('Changed name','Age',lNames[0]);
    AssertEquals('Nothing released',0,FLog.Count);
    AssertEquals('Not detached',0,lObserved.DetachCallCount);
  finally
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


{ The observed test classes keep a single observer pointer and never build an
  observer list, so an attach allocates nothing that CleanupInstance would have
  to free inside the manual buffer. }

procedure TTestObjectStateTracker.ReusedAddressOfObservedObjectDoesNotDetach;

var
  lBuffer: Pointer;
  lSize: SizeInt;
  lPerson: TObservedPerson;
  lCompany: TObservedCompany;
  lNames: TUTF8StringArray;

begin
  lSize:=TObservedPerson.InstanceSize;
  if TObservedCompany.InstanceSize>lSize then
    lSize:=TObservedCompany.InstanceSize;
  GetMem(lBuffer,lSize);
  try
    lPerson:=TObservedPerson(InitInBuffer(TObservedPerson,lBuffer));
    try
      lPerson.Name:='Alice';
      lPerson.Age:=30;
      FTracker.Capture(lPerson,Nil);
      AssertEquals('Attached once',1,lPerson.AttachCount);
    finally
      lPerson.CleanupInstance;
    end;
    lCompany:=TObservedCompany(InitInBuffer(TObservedCompany,lBuffer));
    try
      lCompany.Name:='ACME';
      lCompany.Age:=1;
      AssertFalse('Reused address not tracked',FTracker.GetChanges(lCompany,Nil,lNames));
      AssertEquals('Stranger not detached',0,lCompany.DetachCallCount);
      AssertEquals('ReleaseState not called',0,FLog.Count);
      AssertEquals('Dropped state destroyed',1,FDestroyed);
    finally
      lCompany.CleanupInstance;
    end;
  finally
    FTracker.PurgeAll;
    FreeMem(lBuffer);
  end;
end;


{ A ReleaseState that raises must not cost the detach, so no observer is left
  pointing at the tracker. }

procedure TTestObjectStateTracker.RaisingReleaseStillDetaches;

var
  lObserved: TObservedPerson;
  lRaised: Boolean;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    FTracker.RaiseOnRelease:=True;
    lRaised:=False;
    try
      FTracker.Discard(lObserved);
    except
      lRaised:=True;
    end;
    AssertTrue('ReleaseState raised',lRaised);
    AssertEquals('Detached once',1,lObserved.DetachCount);
    AssertEquals('State reached ReleaseState',1,FLog.Count);
    AssertEquals('State destroyed',1,FDestroyed);
    AssertFalse('Entry gone',FTracker.GetChanges(lObserved,Nil,lNames));
  finally
    FTracker.RaiseOnRelease:=False;
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


{ A FPODetachObserver that raises must not cost the release, so the state still
  reaches ReleaseState and the entry is gone. }

procedure TTestObjectStateTracker.RaisingDetachStillReleases;

var
  lObserved: TObservedPerson;
  lRaised: Boolean;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    lObserved.RaiseOnDetach:=True;
    lRaised:=False;
    try
      FTracker.Discard(lObserved);
    except
      lRaised:=True;
    end;
    AssertTrue('FPODetachObserver raised',lRaised);
    AssertEquals('Detached once',1,lObserved.DetachCount);
    AssertEquals('State reached ReleaseState',1,FLog.Count);
    AssertEquals('State destroyed',1,FDestroyed);
    AssertFalse('Entry gone',FTracker.GetChanges(lObserved,Nil,lNames));
  finally
    lObserved.RaiseOnDetach:=False;
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


{ TPersistent.FPOAttachObserver adds without checking for duplicates, so five
  captures of a real one must still leave a single observer: freeing the tracker
  first and the object afterwards would otherwise notify freed memory. }

procedure TTestObjectStateTracker.RepeatedCaptureOfAPersistentAttachesOnce;

var
  lTracker: TTestStateTracker;
  lPerson: TTrackedPerson;
  lIndex: Integer;

begin
  lPerson:=MakePerson('Bob',40);
  try
    lTracker:=TTestStateTracker.Create(Nil);
    try
      lTracker.Log:=FLog;
      lTracker.StateCounter:=@FDestroyed;
      for lIndex:=1 to 5 do
        lTracker.Capture(lPerson,Nil);
    finally
      lTracker.Free;
    end;
    AssertEquals('One state released',1,FLog.Count);
    AssertEquals('Every state destroyed',5,FDestroyed);
  finally
    lPerson.Free;
  end;
end;


{ CreateState returning Nil drops the entry that was there, which detaches like
  every other releasing path. }

procedure TTestObjectStateTracker.UntrackableObjectDropDetaches;

var
  lObserved: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    FTracker.Capture(lObserved,Nil);
    FTracker.RefuseCreate:=True;
    FTracker.Capture(lObserved,Nil);
    AssertFalse('Entry dropped',FTracker.GetChanges(lObserved,Nil,lNames));
    AssertEquals('Detached once',1,lObserved.DetachCount);
    AssertEquals('One state released',1,FLog.Count);
  finally
    FTracker.PurgeAll;
    lObserved.Free;
  end;
end;


{ Exercises TPersistent.FPODetachObserver itself: the tracker is freed first and
  freeing the object afterwards must not reach into it. }

procedure TTestObjectStateTracker.FreeingTrackerDetachesFromARealPersistent;

var
  lTracker: TTestStateTracker;
  lPerson: TTrackedPerson;
  lNames: TUTF8StringArray;

begin
  lPerson:=MakePerson('Bob',40);
  try
    lTracker:=TTestStateTracker.Create(Nil);
    try
      lTracker.Log:=FLog;
      lTracker.StateCounter:=@FDestroyed;
      lTracker.Capture(lPerson,Nil);
      AssertTrue('Object tracked',lTracker.GetChanges(lPerson,Nil,lNames));
    finally
      lTracker.Free;
    end;
    AssertEquals('One state released',1,FLog.Count);
    AssertEquals('One state destroyed',1,FDestroyed);
  finally
    lPerson.Free;
  end;
end;


{ A notification for an untracked sender, for Nil, or for an operation other
  than ooFree leaves the table as it is. }

procedure TTestObjectStateTracker.UnrelatedNotificationDropsNothing;

var
  lObserved, lStranger: TObservedPerson;
  lNames: TUTF8StringArray;

begin
  lObserved:=MakeObserved('Alice',30);
  try
    lStranger:=MakeObserved('Bob',40);
    try
      FTracker.Capture(lObserved,Nil);
      FTracker.FPOObservedChanged(lStranger,ooFree,Nil);
      FTracker.FPOObservedChanged(Nil,ooFree,Nil);
      FTracker.FPOObservedChanged(lObserved,ooAddItem,Nil);
      FTracker.FPOObservedChanged(lObserved,ooDeleteItem,Nil);
      AssertTrue('Entry untouched',FTracker.GetChanges(lObserved,Nil,lNames));
      AssertEquals('Nothing changed',0,Length(lNames));
      AssertEquals('Nothing released',0,FLog.Count);
      AssertEquals('No state destroyed',0,FDestroyed);
      AssertEquals('Tracked object not detached',0,lObserved.DetachCallCount);
      AssertEquals('Stranger not detached',0,lStranger.DetachCallCount);
    finally
      FTracker.PurgeAll;
      lStranger.Free;
    end;
  finally
    lObserved.Free;
  end;
end;


{ TTestObjectStateTrackerManager }

procedure TTestObjectStateTrackerManager.SetUp;

begin
  FTracker:=TTestStateTracker.Create(Nil);
  FHost:=TTestTrackerHost.Create(Nil);
end;


procedure TTestObjectStateTrackerManager.TearDown;

begin
  FreeAndNil(FTracker);
  FreeAndNil(FHost);
end;


procedure TTestObjectStateTrackerManager.AssigningManagerAttaches;

begin
  FTracker.Manager:=FHost;
  AssertSame('Host holds the tracker',FTracker,FHost.Attached);
  AssertEquals('Attached once',1,FHost.AttachCount);
  AssertSame('Manager holds the host',FHost,FTracker.Manager);
end;


procedure TTestObjectStateTrackerManager.AssigningTheSameManagerTwiceAttachesOnce;

begin
  FTracker.Manager:=FHost;
  FTracker.Manager:=FHost;
  AssertEquals('Attached once',1,FHost.AttachCount);
  AssertEquals('Never detached',0,FHost.DetachCount);
  AssertSame('Host still holds the tracker',FTracker,FHost.Attached);
end;


procedure TTestObjectStateTrackerManager.ReplacingManagerDetachesTheOldOne;

var
  lOther: TTestTrackerHost;

begin
  lOther:=TTestTrackerHost.Create(Nil);
  try
    FTracker.Manager:=FHost;
    FTracker.Manager:=lOther;
    AssertEquals('Old host detached',1,FHost.DetachCount);
    AssertNull('Old host has no tracker',FHost.Attached);
    AssertSame('New host holds the tracker',FTracker,lOther.Attached);
    AssertEquals('New host attached once',1,lOther.AttachCount);
    AssertEquals('New host not detached',0,lOther.DetachCount);
    FTracker.Manager:=Nil;
  finally
    lOther.Free;
  end;
end;


procedure TTestObjectStateTrackerManager.ClearingManagerDetaches;

begin
  FTracker.Manager:=FHost;
  FTracker.Manager:=Nil;
  AssertEquals('Host detached',1,FHost.DetachCount);
  AssertNull('Manager cleared',FTracker.Manager);
end;


procedure TTestObjectStateTrackerManager.FreeingHostClearsManager;

begin
  FTracker.Manager:=FHost;
  FreeAndNil(FHost);
  AssertNull('Manager cleared by notification',FTracker.Manager);
end;


procedure TTestObjectStateTrackerManager.FreeingTrackerDetachesFromHost;

begin
  FTracker.Manager:=FHost;
  FreeAndNil(FTracker);
  AssertEquals('Host detached',1,FHost.DetachCount);
  AssertNull('Host has no tracker',FHost.Attached);
end;


{ The Logger is cleared by TOPBaseComponent.Notification, which only runs when
  the tracker's own override calls inherited. }

procedure TTestObjectStateTrackerManager.FreeingLoggerClearsIt;

var
  lLogger: TOPLogger;
  lAddress: TComponent;

begin
  lAddress:=Nil;
  // The default logger exists first: it cannot take the freed logger's address
  TOPLogger.Instance;
  lLogger:=TOPLogger.Create(Nil);
  try
    lAddress:=lLogger;
    FTracker.Logger:=lLogger;
    AssertSame('Logger assigned',lLogger,FTracker.Logger);
  finally
    FreeAndNil(lLogger);
  end;
  AssertTrue('Logger cleared by notification',FTracker.Logger<>lAddress);
end;


initialization
  RegisterTest(TTestOPChangeNames);
  RegisterTest(TTestBuildChangedFieldMapList);
  RegisterTest(TTestObjectStateTracker);
  RegisterTest(TTestObjectStateTrackerManager);

end.
