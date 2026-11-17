{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Lazy-loading proxy record TOPLazy<T> and IOPLazyInit interface.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Proxy;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Contnrs;
{$ELSE}
  contnrs;
{$ENDIF}

const
  SGUIDIOPLazyInit = '{A3F7B8C2-D4E1-49F0-B6A5-7E2D1C0F3A8B}';

type

  { TLazyLoadEvent:
    callback type for lazy loaders.
    aTarget receives the object or list to populate. }
  TLazyLoadEvent = procedure(aTarget: TObject) of object;

  { TOPLazy:
    generic lazy-loading proxy record.
    Wraps a value of type T and triggers a loader callback on first access.
    The record does not own FValue: Instead, the parent object manages its lifecycle.
    T is typically TObjectList (for 1:N / M:N) or a class (for N:1).
    FKeeper holds a reference to the object behind FOnLoad when that object
    must live as long as the record (the manager's automatic loaders): it is
    released when the entity is freed. }

  generic TOPLazy<T> = record
  private
    FValue: T;
    FLoaded: Boolean;
    FOnLoad: TLazyLoadEvent;
    FKeeper: IInterface;
    function GetValue: T;
  public
    { Init with value only - loader must be set separately via SetLoader. }
    procedure Init(aValue: T);
    { Init with value and loader in one call. }
    procedure Init(aValue: T; aOnLoad: TLazyLoadEvent);
    { Set or replace the loader callback. }
    procedure SetLoader(aOnLoad: TLazyLoadEvent);
    { Set the loader callback and the reference that keeps its object alive
      for as long as this record exists. }
    procedure SetLoader(aOnLoad: TLazyLoadEvent; const aKeeper: IInterface);
    { Reset loaded state - next Value access will re-trigger the loader. }
    procedure Invalidate;
    { Returns True if the loader has already fired (or no loader was set). }
    function IsLoaded: Boolean;
    { Access the wrapped value. Triggers the loader on first access. }
    property Value: T read GetValue;
  end;

  { IOPLazyInit:
    An optional CORBA interface for objects with lazy-load fields.
    After Load, the Manager calls InitLazyLoaders if the object supports it.
    aManager is TObject (not TOPManager) to avoid circular dependencies. }

  IOPLazyInit = interface ['{A3F7B8C2-D4E1-49F0-B6A5-7E2D1C0F3A8B}']
    { Sets up the loaders of the object's lazy-load fields, using the given manager. }
    procedure InitLazyLoaders(aManager: TObject);
  end;

{$IFNDEF VER3_2}
  { Pre-declared TOPLazy specializations with extended RTTI.
    The {$RTTI} directive ensures that FValue, FLoaded and FOnLoad fields
    are discoverable via TRttiRecordType.GetFields - required for
    automatic lazy-field setup by the Manager. }

  {$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TOPLazyObject = specialize TOPLazy<TObject>;

  {$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TOPLazyObjectList = specialize TOPLazy<TObjectList>;
{$ENDIF}

{$IFNDEF VER3_2}
{ IsLazyRecordField - detects whether an extended RTTI field is a
  TOPLazy<T> specialization by checking the mangled type name and
  inner field signature (FValue, FLoaded, FOnLoad). }
function IsLazyRecordField(aFieldRtti: TObject): Boolean;

{ GetLazyValueClass - returns the TClass of the FValue field inside
  a TOPLazy record, or nil if FValue is not a class type. }
function GetLazyValueClass(aFieldRtti: TObject): TClass;
{$ENDIF}

implementation

{$IFNDEF VER3_2}
uses
  {$IFDEF FPC_DOTTEDUNITS}System.Rtti{$ELSE}Rtti{$ENDIF};
{$ENDIF}

{ TOPLazy }

function TOPLazy.GetValue: T;
begin
  if (not FLoaded) then
  begin
    FLoaded := True;
    if Assigned(FOnLoad) then
    try
      FOnLoad(TObject(FValue));
    except
      // Not loaded: the next access tries again
      FLoaded := False;
      raise;
    end;
  end;
  Result := FValue;
end;

procedure TOPLazy.Init(aValue: T);
begin
  FValue := aValue;
  FLoaded := False;
  FOnLoad := nil;
  FKeeper := nil;
end;

procedure TOPLazy.Init(aValue: T; aOnLoad: TLazyLoadEvent);
begin
  FValue := aValue;
  FLoaded := False;
  FOnLoad := aOnLoad;
  FKeeper := nil;
end;

procedure TOPLazy.SetLoader(aOnLoad: TLazyLoadEvent);
begin
  FOnLoad := aOnLoad;
  FKeeper := nil;
end;

procedure TOPLazy.SetLoader(aOnLoad: TLazyLoadEvent; const aKeeper: IInterface);
begin
  FOnLoad := aOnLoad;
  FKeeper := aKeeper;
end;

procedure TOPLazy.Invalidate;
begin
  FLoaded := False;
end;

function TOPLazy.IsLoaded: Boolean;
begin
  Result := FLoaded;
end;

{$IFNDEF VER3_2}

function IsLazyRecordField(aFieldRtti: TObject): Boolean;
var
  RF: TRttiField;
  RecType: TRttiType;
  FVal, FLoadedF, FOnLoadF, FKeeperF: TRttiField;
begin
  Result := False;
  if not (aFieldRtti is TRttiField) then
    Exit;
  RF := TRttiField(aFieldRtti);
  if RF.FieldType.TypeKind <> tkRecord then
    Exit;
  { 
    Generic names are mangled as TOPLazy$1$crcXXXX for anonymous specializations.
    Named aliases (e.g., TOPLazyObjectList = specialize TOPLazy<TObjectList>)
    use the alias name. 
    Check for 'TOPLazy' prefix to match both cases,  then validate by inner field signature. 

    This is not an ideal detection mechanism, but for the moment it will have to do :(
    Maybe switching to a class is a better idea.
    }
  if Pos('TOPLazy', RF.FieldType.Name) <> 1 then
    Exit;
  RecType := RF.FieldType;
  FVal := RecType.GetField('FValue');
  FLoadedF := RecType.GetField('FLoaded');
  FOnLoadF := RecType.GetField('FOnLoad');
  FKeeperF := RecType.GetField('FKeeper');
  Result := (FVal <> nil) and (FLoadedF <> nil) and (FOnLoadF <> nil) and (FKeeperF <> nil)
    and (FLoadedF.FieldType.TypeKind = tkBool) and (FKeeperF.FieldType.TypeKind = tkInterface);
end;

function GetLazyValueClass(aFieldRtti: TObject): TClass;
var
  RF: TRttiField;
  RecType: TRttiType;
  FVal: TRttiField;
begin
  Result := nil;
  if not (aFieldRtti is TRttiField) then
    Exit;
  RF := TRttiField(aFieldRtti);
  RecType := RF.FieldType;
  FVal := RecType.GetField('FValue');
  if (FVal <> nil) and (FVal.FieldType.TypeKind = tkClass) then
    Result := FVal.FieldType.AsInstance.MetaclassType;
end;

{$ENDIF}

end.
