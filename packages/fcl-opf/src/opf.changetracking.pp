{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Contracts and shared state table for change tracking of persistent objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.ChangeTracking;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs,
{$ELSE}
  Classes, SysUtils, contnrs,
{$ENDIF}
  Opf.Types, Opf.Fields;

const
  SGUIDIOPChangeTracker     = '{3E5C1A47-9D82-4F16-B0C7-6A5E2D31F84B}';
  SGUIDIOPChangeTrackerHost = '{8D1B4F60-7A35-4C92-BE48-13F5A9027C6D}';

type
  TOPTrackMode = (tmNone, tmImplicit, tmExplicit);

  // Contract for a mechanism that tracks changes to persistent objects.
  IOPChangeTracker = interface [SGUIDIOPChangeTracker]
    // Record the current state of aObject as the reference state.
    procedure Capture(aObject: TObject; aFieldMap: TOPFieldMapList);
    // Forget the reference state of aObject.
    procedure Discard(aObject: TObject);
    // False when aObject has no reference state; otherwise aNames holds the
    // names of the properties that differ from the reference state.
    function GetChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
      out aNames: TUTF8StringArray): Boolean;
    // Force aPropertyName into the next update of aObject.
    procedure MarkDirty(aObject: TObject; const aPropertyName: UTF8String);
    // Forget every reference state this tracker holds.
    procedure PurgeAll;
    // True when reference states are also kept for objects that do not
    // report being freed (IFPObserved): such an object may be freed while
    // its state is still held.
    function TracksUnobserved: Boolean;
  end;

  // The side that a change tracker attaches itself to.
  IOPChangeTrackerHost = interface [SGUIDIOPChangeTrackerHost]
    // Use aProvider as the change tracker from now on.
    procedure AttachChangeTracker(aProvider: TComponent);
    // Stop using the current change tracker.
    procedure DetachChangeTracker;
  end;

  { TOPChangeNames: collects property names during comparison. }

  TOPChangeNames = record
  private
    FNames: TUTF8StringArray;
    function GetCount: Integer;
  public
    // Append aName, unless it is already collected.
    procedure Add(const aName: UTF8String);
    // Append aName only when aChanged is True.
    procedure AddIf(aChanged: Boolean; const aName: UTF8String);
    // The collected names.
    function ToArray: TUTF8StringArray;
    // Number of names collected so far.
    property Count: Integer read GetCount;
  end;

  { TOPObjectStateTracker: table of reference states keyed by object,
    shared by all IOPChangeTracker implementations. }

  TOPObjectStateTracker = class(TOPBaseComponent, IOPChangeTracker, IFPObserver)
  private
    FTrackUnobserved: Boolean;
    FMode: TOPTrackMode;
    FMaxEntries: Integer;
    FManager: TComponent;
    // Entries by object address, for constant time lookup and removal
    FIndex: TObject;
    // Entries in capture order (a linked list): the oldest is evicted first
    FFirst: TObject;
    FLast: TObject;
    FCount: Integer;
    FRegisteredClasses: TFPList;
    FExcludedClasses: TFPList;
    function FindEntry(aObject: TObject): TObject;
    procedure DropEntry(aEntry: TObject; aRelease: Boolean; aDetach: Boolean);
    procedure PurgeEntries(aReleaseUnobserved: Boolean);
    procedure SetManager(AValue: TComponent);
    // Attach as observer of aObject; False when it cannot be observed.
    function AttachObserver(aObject: TObject): Boolean;
    // Stop observing aObject.
    procedure DetachObserver(aObject: TObject);
  protected
    // Produce the reference state for aObject, or Nil when it cannot be tracked.
    function CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject; virtual; abstract;
    // Add the name of every property of aObject that differs from aState.
    procedure CompareState(aState: TObject; aObject: TObject;
      aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames); virtual; abstract;
    // Called when an entry is dropped, except on a class mismatch and on ooFree.
    procedure ReleaseState(aState: TObject; aObject: TObject); virtual;
    // The exception class raised by every change tracker.
    class function GetExceptionClass: EOPFClass; override;
    // Clears Manager when the component it points to is freed.
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Create the tracker with an empty table, tracking every class.
    constructor Create(AOwner: TComponent); override;
    // Detach from the manager and release every entry.
    destructor Destroy; override;
    // Record the current state of aObject as the reference state.
    procedure Capture(aObject: TObject; aFieldMap: TOPFieldMapList); virtual;
    // Forget the reference state of aObject.
    procedure Discard(aObject: TObject); virtual;
    // False when aObject has no reference state; otherwise aNames holds the changed property names.
    function GetChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
      out aNames: TUTF8StringArray): Boolean; virtual;
    // Force aPropertyName into the next update of aObject.
    procedure MarkDirty(aObject: TObject; const aPropertyName: UTF8String); virtual;
    // Forget every reference state this tracker holds.
    procedure PurgeAll; virtual;
    // Returns TrackUnobserved
    function TracksUnobserved: Boolean;
    // Drops the entry of ASender when it is being freed.
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation;
      Data: Pointer); virtual;
    // Forget the reference state of aObject.
    procedure Purge(aObject: TObject);
    // Track aClass when Mode is tmExplicit.
    procedure RegisterClass(aClass: TClass);
    // Never track aClass when Mode is tmImplicit.
    procedure ExcludeClass(aClass: TClass);
    // True when objects of aClass are tracked under the current mode.
    function IsClassTracked(aClass: TClass): Boolean;
    { False (default): only objects that report being freed (IFPObserved, e.g.
      any TPersistent) get a reference state, which is dropped when the
      object is freed. Other objects have none, so they are saved with all
      their columns. True: other objects get a reference state as well; the
      application must then Purge them before freeing them, and a rollback
      that touched one of them purges all reference states. }
    property TrackUnobserved: Boolean read FTrackUnobserved write FTrackUnobserved;
    // Which classes are tracked.
    property Mode: TOPTrackMode read FMode write FMode;
    // Largest number of entries kept; 0 means unlimited, otherwise the oldest entry is evicted first.
    property MaxEntries: Integer read FMaxEntries write FMaxEntries;
    // The component this tracker attaches itself to. Must implement IOPChangeTrackerHost.
    property Manager: TComponent read FManager write SetManager;
  end;

{ Build a field map holding the key and version fields plus every foInUpdate
  field named in aNames. Caller owns the result. }
function BuildChangedFieldMapList(aFieldMap: TOPFieldMapList;
  const aNames: TUTF8StringArray): TOPFieldMapList;

implementation

type

  { TOPStateEntry: one tracked object, the reference state taken from it and
    the names marked for its next update. }

  TOPStateEntry = class(TObject)
  private
    FObj: Pointer;
    FTrackedClass: TClass;
    FState: TObject;
    FObserved: Boolean;
    FMarked: TOPChangeNames;
    // Capture order
    FPrev: TOPStateEntry;
    FNext: TOPStateEntry;
    // Next entry in the same TOPStateIndex bucket
    FHashNext: TOPStateEntry;
  public
    // Create the entry for aObject, taking ownership of aState.
    constructor Create(aObject: TObject; aState: TObject);
    // Free the reference state this entry owns.
    destructor Destroy; override;
    // Address of the tracked object.
    property Obj: Pointer read FObj;
    // Class the tracked object had when the entry was made.
    property TrackedClass: TClass read FTrackedClass write FTrackedClass;
    // The reference state, owned by this entry.
    property State: TObject read FState write FState;
    // True when the tracker attached itself as observer of the tracked object.
    property Observed: Boolean read FObserved write FObserved;
  end;

  { TOPStateIndex: the entries by object address, in a hash table chained
    through the entries themselves: adding, finding and removing an entry
    take constant time. }

  TOPStateIndex = class(TObject)
  private
    FBuckets: array of TOPStateEntry;
    FCount: Integer;
    function Slot(aObj: Pointer): Integer;
    procedure Grow;
  public
    // Create an empty index.
    constructor Create;
    // Add aEntry; its object must not be in the index yet.
    procedure Add(aEntry: TOPStateEntry);
    // Remove aEntry from the index.
    procedure Remove(aEntry: TOPStateEntry);
    // The entry of the object at address aObj, or Nil.
    function Find(aObj: Pointer): TOPStateEntry;
  end;

{ TOPChangeNames }

function TOPChangeNames.GetCount: Integer;

begin
  Result:=Length(FNames);
end;


procedure TOPChangeNames.Add(const aName: UTF8String);

var
  lIndex: Integer;

begin
  for lIndex:=0 to Length(FNames)-1 do
    if SameText(FNames[lIndex],aName) then
      Exit;
  SetLength(FNames,Length(FNames)+1);
  FNames[Length(FNames)-1]:=aName;
end;


procedure TOPChangeNames.AddIf(aChanged: Boolean; const aName: UTF8String);

begin
  if aChanged then
    Add(aName);
end;


function TOPChangeNames.ToArray: TUTF8StringArray;

begin
  Result:=FNames;
end;


function IndexOfName(const aNames: TUTF8StringArray; const aName: UTF8String): Integer;

begin
  Result:=Length(aNames)-1;
  while (Result>=0) and not SameText(aNames[Result],aName) do
    Dec(Result);
end;


function BuildChangedFieldMapList(aFieldMap: TOPFieldMapList;
  const aNames: TUTF8StringArray): TOPFieldMapList;

var
  lIndex: Integer;
  lField, lNewField: TOPFieldMap;

begin
  Result:=TOPFieldMapList.Create(TOPFieldMap);
  for lIndex:=0 to aFieldMap.Count-1 do
    begin
    lField:=aFieldMap[lIndex];
    if (foInKey in lField.Options) or
       (foVersion in lField.Options) or
       ((foInUpdate in lField.Options) and (IndexOfName(aNames,lField.PropertyName)>=0)) then
      begin
      lNewField:=Result.Add as TOPFieldMap;
      lNewField.Assign(lField);
      end;
    end;
end;


{ TOPStateIndex }

constructor TOPStateIndex.Create;

begin
  inherited Create;
  SetLength(FBuckets,64);
end;


function TOPStateIndex.Slot(aObj: Pointer): Integer;

var
  lHash: PtrUInt;

begin
  // Objects are aligned: the low bits carry no information
  lHash:=PtrUInt(aObj) shr 4;
  lHash:=lHash xor (lHash shr 11) xor (lHash shr 23);
  Result:=lHash and PtrUInt(Length(FBuckets)-1);
end;


procedure TOPStateIndex.Grow;

var
  lOld: array of TOPStateEntry;
  lEntry, lNext: TOPStateEntry;
  lIndex, lSlot: Integer;

begin
  lOld:=FBuckets;
  FBuckets:=Nil;
  SetLength(FBuckets,Length(lOld)*2);
  for lIndex:=0 to Length(lOld)-1 do
    begin
    lEntry:=lOld[lIndex];
    while Assigned(lEntry) do
      begin
      lNext:=lEntry.FHashNext;
      lSlot:=Slot(lEntry.Obj);
      lEntry.FHashNext:=FBuckets[lSlot];
      FBuckets[lSlot]:=lEntry;
      lEntry:=lNext;
      end;
    end;
end;


procedure TOPStateIndex.Add(aEntry: TOPStateEntry);

var
  lSlot: Integer;

begin
  if FCount>=Length(FBuckets) then
    Grow;
  lSlot:=Slot(aEntry.Obj);
  aEntry.FHashNext:=FBuckets[lSlot];
  FBuckets[lSlot]:=aEntry;
  Inc(FCount);
end;


procedure TOPStateIndex.Remove(aEntry: TOPStateEntry);

var
  lSlot: Integer;
  lPrev, lEntry: TOPStateEntry;

begin
  lSlot:=Slot(aEntry.Obj);
  lPrev:=Nil;
  lEntry:=FBuckets[lSlot];
  while Assigned(lEntry) and (lEntry<>aEntry) do
    begin
    lPrev:=lEntry;
    lEntry:=lEntry.FHashNext;
    end;
  if lEntry=Nil then
    Exit;
  if Assigned(lPrev) then
    lPrev.FHashNext:=lEntry.FHashNext
  else
    FBuckets[lSlot]:=lEntry.FHashNext;
  lEntry.FHashNext:=Nil;
  Dec(FCount);
end;


function TOPStateIndex.Find(aObj: Pointer): TOPStateEntry;

begin
  Result:=FBuckets[Slot(aObj)];
  while Assigned(Result) and (Result.Obj<>aObj) do
    Result:=Result.FHashNext;
end;


{ TOPStateEntry }

constructor TOPStateEntry.Create(aObject: TObject; aState: TObject);

begin
  inherited Create;
  FObj:=Pointer(aObject);
  FTrackedClass:=aObject.ClassType;
  FState:=aState;
end;


destructor TOPStateEntry.Destroy;

begin
  FreeAndNil(FState);
  inherited Destroy;
end;


{ TOPObjectStateTracker }

constructor TOPObjectStateTracker.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FTrackUnobserved:=False;
  FMode:=tmImplicit;
  FMaxEntries:=0;
  FIndex:=TOPStateIndex.Create;
  FRegisteredClasses:=TFPList.Create;
  FExcludedClasses:=TFPList.Create;
end;


{ PurgeAll runs here, so ReleaseState reaches a descendant after that
  descendant's own destructor has finished. }

destructor TOPObjectStateTracker.Destroy;

begin
  try
    Manager:=Nil;
    if Assigned(FIndex) then
      PurgeEntries(False);
  finally
    FreeAndNil(FIndex);
    FreeAndNil(FRegisteredClasses);
    FreeAndNil(FExcludedClasses);
    inherited Destroy;
  end;
end;


class function TOPObjectStateTracker.GetExceptionClass: EOPFClass;

begin
  Result:=EOPFChangeTracking;
end;


procedure TOPObjectStateTracker.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent,Operation);
  if (Operation=opRemove) and (AComponent=FManager) then
    FManager:=Nil;
end;


procedure TOPObjectStateTracker.SetManager(AValue: TComponent);

var
  lHost: IOPChangeTrackerHost;

begin
  if FManager=AValue then
    Exit;
  if Assigned(FManager) then
    begin
    if FManager.GetInterface(SGUIDIOPChangeTrackerHost,lHost) then
      lHost.DetachChangeTracker;
    FManager.RemoveFreeNotification(Self);
    end;
  FManager:=AValue;
  if Assigned(FManager) then
    begin
    FManager.FreeNotification(Self);
    if FManager.GetInterface(SGUIDIOPChangeTrackerHost,lHost) then
      lHost.AttachChangeTracker(Self);
    end;
end;


function TOPObjectStateTracker.AttachObserver(aObject: TObject): Boolean;

var
  lObserved: IFPObserved;

begin
  Result:=False;
  if not aObject.GetInterface(SGUIDObserved,lObserved) then
    Exit;
  lObserved.FPOAttachObserver(Self);
  Result:=True;
end;


procedure TOPObjectStateTracker.DetachObserver(aObject: TObject);

var
  lObserved: IFPObserved;

begin
  if aObject.GetInterface(SGUIDObserved,lObserved) then
    lObserved.FPODetachObserver(Self);
end;


{ Detaching happens before the release, and the two are independent: a release
  that raises still leaves no observer pointing at this tracker, and a detach
  that raises still lets the state reach ReleaseState. }

procedure TOPObjectStateTracker.DropEntry(aEntry: TObject; aRelease: Boolean; aDetach: Boolean);

var
  lEntry: TOPStateEntry absolute aEntry;

begin
  TOPStateIndex(FIndex).Remove(lEntry);
  if Assigned(lEntry.FPrev) then
    lEntry.FPrev.FNext:=lEntry.FNext
  else
    FFirst:=lEntry.FNext;
  if Assigned(lEntry.FNext) then
    lEntry.FNext.FPrev:=lEntry.FPrev
  else
    FLast:=lEntry.FPrev;
  Dec(FCount);
  try
    try
      if aDetach and lEntry.Observed then
        DetachObserver(TObject(lEntry.Obj));
    finally
      if aRelease then
        ReleaseState(lEntry.State,TObject(lEntry.Obj));
    end;
  finally
    lEntry.Free;
  end;
end;


{ Drops an entry whose stored class no longer matches, without releasing or
  detaching it, and reports the object as not tracked. The address now holds an
  object this tracker never attached to. }

function TOPObjectStateTracker.FindEntry(aObject: TObject): TObject;

begin
  Result:=Nil;
  if not Assigned(aObject) then
    Exit;
  Result:=TOPStateIndex(FIndex).Find(Pointer(aObject));
  if Result=Nil then
    Exit;
  // Same address, other class: a new object where a freed one was
  if TOPStateEntry(Result).TrackedClass<>aObject.ClassType then
    begin
    DropEntry(Result,False,False);
    Result:=Nil;
    end;
end;


procedure TOPObjectStateTracker.RegisterClass(aClass: TClass);

begin
  if FRegisteredClasses.IndexOf(aClass)<0 then
    FRegisteredClasses.Add(aClass);
end;


procedure TOPObjectStateTracker.ExcludeClass(aClass: TClass);

begin
  if FExcludedClasses.IndexOf(aClass)<0 then
    FExcludedClasses.Add(aClass);
end;


function TOPObjectStateTracker.IsClassTracked(aClass: TClass): Boolean;

begin
  case FMode of
    tmImplicit:
      Result:=FExcludedClasses.IndexOf(aClass)<0;
    tmExplicit:
      Result:=FRegisteredClasses.IndexOf(aClass)>=0;
  else
    Result:=False;
  end;
end;


{$push}
{$warn 5024 off}
procedure TOPObjectStateTracker.ReleaseState(aState: TObject; aObject: TObject);

begin
end;
{$pop}


procedure TOPObjectStateTracker.Capture(aObject: TObject; aFieldMap: TOPFieldMapList);

var
  lEntry: TOPStateEntry;
  lState: TObject;
  lObserved: IFPObserved;

begin
  if not Assigned(aObject) then
    Exit;
  if not IsClassTracked(aObject.ClassType) then
    Exit;
  // Without TrackUnobserved, an object that cannot report being freed gets no
  // reference state: it could be freed while the state is held
  if not FTrackUnobserved and not aObject.GetInterface(SGUIDObserved,lObserved) then
    begin
    lEntry:=TOPStateEntry(FindEntry(aObject));
    if Assigned(lEntry) then
      DropEntry(lEntry,True,True);
    Exit;
    end;
  lState:=CreateState(aObject,aFieldMap);
  lEntry:=TOPStateEntry(FindEntry(aObject));
  if not Assigned(lState) then
    begin
    if Assigned(lEntry) then
      DropEntry(lEntry,True,True);
    Exit;
    end;
  if Assigned(lEntry) then
    begin
    lEntry.State.Free;
    lEntry.State:=lState;
    lEntry.FMarked:=Default(TOPChangeNames);
    Exit;
    end;
  // An evicted object may have been freed: only an observed one is known to be alive
  while (FMaxEntries>0) and (FCount>=FMaxEntries) do
    DropEntry(FFirst,TOPStateEntry(FFirst).Observed,True);
  lEntry:=TOPStateEntry.Create(aObject,lState);
  TOPStateIndex(FIndex).Add(lEntry);
  lEntry.FPrev:=TOPStateEntry(FLast);
  if Assigned(FLast) then
    TOPStateEntry(FLast).FNext:=lEntry
  else
    FFirst:=lEntry;
  FLast:=lEntry;
  Inc(FCount);
  lEntry.Observed:=AttachObserver(aObject);
end;


procedure TOPObjectStateTracker.Discard(aObject: TObject);

var
  lEntry: TObject;

begin
  lEntry:=FindEntry(aObject);
  if Assigned(lEntry) then
    DropEntry(lEntry,True,True);
end;


function TOPObjectStateTracker.GetChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
  out aNames: TUTF8StringArray): Boolean;

var
  lEntry: TOPStateEntry;
  lNames: TOPChangeNames;
  lMarked: TUTF8StringArray;
  lIndex: Integer;

begin
  aNames:=Nil;
  lEntry:=TOPStateEntry(FindEntry(aObject));
  Result:=Assigned(lEntry);
  if not Result then
    Exit;
  lNames:=Default(TOPChangeNames);
  CompareState(lEntry.State,aObject,aFieldMap,lNames);
  lMarked:=lEntry.FMarked.ToArray;
  for lIndex:=0 to Length(lMarked)-1 do
    lNames.Add(lMarked[lIndex]);
  aNames:=lNames.ToArray;
end;


procedure TOPObjectStateTracker.MarkDirty(aObject: TObject; const aPropertyName: UTF8String);

var
  lEntry: TOPStateEntry;

begin
  lEntry:=TOPStateEntry(FindEntry(aObject));
  if Assigned(lEntry) then
    lEntry.FMarked.Add(aPropertyName);
end;


function TOPObjectStateTracker.TracksUnobserved: Boolean;

begin
  Result:=FTrackUnobserved;
end;


procedure TOPObjectStateTracker.PurgeAll;

begin
  PurgeEntries(True);
end;

{ aReleaseUnobserved: also release the state of objects that do not report
  being freed. False when they may be gone (tracker destroyed): releasing
  calls into the object for TOPSelfTracker. }

procedure TOPObjectStateTracker.PurgeEntries(aReleaseUnobserved: Boolean);

var
  lEntry: TOPStateEntry;

begin
  while Assigned(FLast) do
    begin
    lEntry:=TOPStateEntry(FLast);
    DropEntry(lEntry,aReleaseUnobserved or lEntry.Observed,True);
    end;
end;


{ On ooFree the object is already inside its own destructor, so the entry is
  dropped without detaching and without releasing its state. }

{$push}
{$warn 5024 off}
procedure TOPObjectStateTracker.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);

var
  lEntry: TObject;

begin
  if Operation<>ooFree then
    Exit;
  lEntry:=FindEntry(ASender);
  if Assigned(lEntry) then
    DropEntry(lEntry,False,False);
end;
{$pop}


procedure TOPObjectStateTracker.Purge(aObject: TObject);

begin
  Discard(aObject);
end;


end.
