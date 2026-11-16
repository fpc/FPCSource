{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Global snapshot manager and self tracker for dirty tracking

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Snapshot;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  Opf.Fields, Opf.SnapshotHooks, Opf.ChangeTracking;

type
  TOPSnapshotMode = TOPTrackMode;

const
  smNone = tmNone;
  smImplicit = tmImplicit;
  smExplicit = tmExplicit;

type

  { TOPVariantState - reference state of one object, held as a variant array
    indexed like the field map it was captured with. }

  TOPVariantState = class(TObject)
    Values: TOPVariantArray;
  end;

  { TOPSnapshotManager - change tracker keeping a variant array of the original
    property values of every tracked object.

    Mode controls which classes are tracked:
    - smNone: tracking disabled
    - smImplicit (default): all classes tracked except the excluded ones
    - smExplicit: only the registered classes tracked

    MaxEntries (default 0=unlimited): the oldest entry is dropped when the limit
    is reached.
    An object the tracker cannot observe must be purged before it is freed,
    otherwise its entry outlives it. }

  TOPSnapshotManager = class(TOPObjectStateTracker)
  protected
    // The values of the mapped properties of aObject at capture time.
    function CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject; override;
    // Add the name of every property of aObject that differs from the captured values.
    procedure CompareState(aState: TObject; aObject: TObject;
      aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames); override;
  public
    // Create the tracker and attach it to the default manager.
    constructor Create(AOwner: TComponent); override;
  end;

  { TOPSelfState - entry for an object that keeps its own snapshot.
    The CORBA reference carries no reference count, so it keeps nothing alive. }

  TOPSelfState = class(TObject)
    Tracking: IOPDirtyTracking;
  end;

  { TOPSelfTracker - change tracker for objects that keep their own snapshot
    through IOPDirtyTracking. An object not implementing that interface is not
    tracked.
    An object the tracker cannot observe must be purged before it is freed,
    otherwise its entry outlives it. }

  TOPSelfTracker = class(TOPObjectStateTracker)
  protected
    // Store the snapshot inside aObject; Nil when aObject has no IOPDirtyTracking.
    function CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject; override;
    // Add the name of every property of aObject that differs from its own snapshot.
    procedure CompareState(aState: TObject; aObject: TObject;
      aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames); override;
    // Clear the snapshot kept inside the object.
    procedure ReleaseState(aState: TObject; aObject: TObject); override;
  public
    // Create the tracker and attach it to the default manager.
    constructor Create(AOwner: TComponent); override;
    // Force aPropertyName into the next update of aObject, through the object itself.
    procedure MarkDirty(aObject: TObject; const aPropertyName: UTF8String); override;
  end;

implementation

uses Opf.Manager;

{ TOPSnapshotManager }

constructor TOPSnapshotManager.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  Manager:=OPManager;
end;


function TOPSnapshotManager.CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject;

var
  lValues: TOPVariantArray;

begin
  lValues:=OPFCapturePropertyValues(aObject,aFieldMap);
  Result:=TOPVariantState.Create;
  TOPVariantState(Result).Values:=lValues;
end;


{ No marked names are passed on: they live in the base entry, which merges them
  itself once this has returned. }

procedure TOPSnapshotManager.CompareState(aState: TObject; aObject: TObject;
  aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames);

var
  lDirty: TStringList;
  lIndex: Integer;

begin
  lDirty:=OPFComputeDirtyFields(TOPVariantState(aState).Values,
    OPFCapturePropertyValues(aObject,aFieldMap),aFieldMap,Nil);
  try
    for lIndex:=0 to lDirty.Count-1 do
      aNames.Add(lDirty[lIndex]);
  finally
    lDirty.Free;
  end;
end;


{ TOPSelfTracker }

constructor TOPSelfTracker.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  Manager:=OPManager;
end;


function TOPSelfTracker.CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject;

var
  lTracking: IOPDirtyTracking;
  lState: TOPSelfState;

begin
  Result:=Nil;
  if not aObject.GetInterface(SGUIDIOPDirtyTracking,lTracking) then
    Exit;
  lTracking.StoreSnapshot(OPFCapturePropertyValues(aObject,aFieldMap));
  lState:=TOPSelfState.Create;
  lState.Tracking:=lTracking;
  Result:=lState;
end;


{ An object that cleared its own snapshot has nothing left to compare against,
  so every updatable property counts as changed. }

procedure TOPSelfTracker.CompareState(aState: TObject; aObject: TObject;
  aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames);

var
  lTracking: IOPDirtyTracking;
  lOriginal: TOPVariantArray;
  lDirty: TStringList;
  lIndex: Integer;
  lField: TOPFieldMap;

begin
  lTracking:=TOPSelfState(aState).Tracking;
  if not lTracking.RetrieveSnapshot(lOriginal) then
    begin
    for lIndex:=0 to aFieldMap.Count-1 do
      begin
      lField:=aFieldMap[lIndex];
      if foInUpdate in lField.Options then
        aNames.Add(lField.PropertyName);
      end;
    Exit;
    end;
  lDirty:=OPFComputeDirtyFields(lOriginal,OPFCapturePropertyValues(aObject,aFieldMap),
    aFieldMap,lTracking.GetMarkedDirty);
  try
    for lIndex:=0 to lDirty.Count-1 do
      aNames.Add(lDirty[lIndex]);
  finally
    lDirty.Free;
  end;
end;


{$push}
{$warn 5024 off}
procedure TOPSelfTracker.ReleaseState(aState: TObject; aObject: TObject);

begin
  TOPSelfState(aState).Tracking.ClearSnapshot;
end;
{$pop}


{ The entry table is private to the base class, so the mark goes straight to the
  object. A mark on an untracked object is harmless: GetChanges still answers
  False for it. }

procedure TOPSelfTracker.MarkDirty(aObject: TObject; const aPropertyName: UTF8String);

var
  lTracking: IOPDirtyTracking;

begin
  if Assigned(aObject) and aObject.GetInterface(SGUIDIOPDirtyTracking,lTracking) then
    lTracking.MarkDirty(aPropertyName);
end;


end.
