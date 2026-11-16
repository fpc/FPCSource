{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Per-object dirty tracking interface and the variant capture and compare helpers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.SnapshotHooks;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  Opf.Fields;

const
  SGUIDIOPDirtyTracking = '{7F3A8B2E-C4D1-4E9F-B5A6-8D2E1F0C3B7A}';

type
  TOPVariantArray = array of Variant;

  { IOPDirtyTracking:

  Optional interface for per-class dirty tracking.
    Objects implementing this interface store their own snapshot of original property values, 
    enabling selective UPDATE generation.

    An attached TOPSelfTracker calls StoreSnapshot after Load and RetrieveSnapshot before Save;
    without one the object is not tracked.

    Users may call MarkDirty to force a property into the UPDATE.

    StoreSnapshot must clear the marked names:
    MarkDirty can also reach an object that the tracker does not contain,
    and a mark left behind forces its column into a later UPDATE. }

  IOPDirtyTracking = interface ['{7F3A8B2E-C4D1-4E9F-B5A6-8D2E1F0C3B7A}']
    { Stores the original property values and clears the marked names }
    procedure StoreSnapshot(const aValues: TOPVariantArray);
    { Returns the stored original values; false if there are none }
    function RetrieveSnapshot(out aValues: TOPVariantArray): Boolean;
    { Marks a property as changed so it is always saved }
    procedure MarkDirty(const aPropertyName: UTF8String);
    { The names of the properties marked as changed }
    function GetMarkedDirty: TUTF8StringArray;
    { Removes the stored original values and the marked names }
    procedure ClearSnapshot;
  end;

{ Capture all property values for fields with foInUpdate or foInKey into a variant array.
  Array indices correspond to aFieldMap field indices. Untracked fields get Unassigned.
  A null field (as the persister would write it) is captured as Null, so a change
  of only the null state is detected. Values are read with OPFReadFieldValue. }
function OPFCapturePropertyValues(aInstance: TObject; aFieldMap: TOPFieldMapList): TOPVariantArray;

{ Compare original and current variant arrays. Returns a new TStringList containing
  the PropertyName of each changed field. Merges in aMarkedDirty names.
  Caller owns the returned list. }
function OPFComputeDirtyFields(const aOriginal, aCurrent: TOPVariantArray;
  aFieldMap: TOPFieldMapList; const aMarkedDirty: TUTF8StringArray): TStringList;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo, System.Variants,
{$ELSE}
  typinfo, Variants,
{$ENDIF}
  Opf.FieldValue;

{ A field that cannot be read is captured as a variant error,
  which compares as changed: its column is always included in an UPDATE. }

function OPFCapturePropertyValues(aInstance: TObject; aFieldMap: TOPFieldMapList): TOPVariantArray;
var
  I: Integer;
  F: TOPFieldMap;
  V: Variant;
begin
  SetLength(Result, aFieldMap.Count);
  for I := 0 to aFieldMap.Count - 1 do
  begin
    F := aFieldMap[I];
    if ([foInUpdate, foInKey] * F.Options) <> [] then
    begin
      if OPFReadFieldValue(aInstance, F, V) then
        Result[I] := V
      else
        Result[I] := VarAsError(1);
    end
    else
      Result[I] := Unassigned;
  end;
end;

function OPFComputeDirtyFields(const aOriginal, aCurrent: TOPVariantArray;
  aFieldMap: TOPFieldMapList; const aMarkedDirty: TUTF8StringArray): TStringList;
var
  I: Integer;
  F: TOPFieldMap;
  IsDirty: Boolean;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  for I := 0 to aFieldMap.Count - 1 do
  begin
    F := aFieldMap[I];
    if not (foInUpdate in F.Options) then
      Continue;
    if (I < Length(aOriginal)) and (I < Length(aCurrent)) then
    begin
      // An unreadable field (captured as error) is always dirty
      if VarIsError(aOriginal[I]) or VarIsError(aCurrent[I]) then
        IsDirty := True
      else
        IsDirty := not OPFSameVariant(aOriginal[I], aCurrent[I]);
      if IsDirty then
        Result.Add(F.PropertyName);
    end;
  end;
  for I := 0 to Length(aMarkedDirty) - 1 do
    Result.Add(aMarkedDirty[I]);
end;

end.
