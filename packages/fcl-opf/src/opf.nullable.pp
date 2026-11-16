{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Support for Null values

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Nullable;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Nullable;
{$ELSE}
  Classes, SysUtils, nullable;
{$ENDIF}

const
  SGUIDIOPFNullable = '{D4E7A2B1-9C3F-4E8A-A5D6-7B1C0E2F3A84}';

type

  { IOPFNullable:
    Optional interface for explicit null tracking per property.
    Classes implementing this interface can mark individual properties as null
    without relying on stored specifiers. }

  IOPFNullable = interface ['{D4E7A2B1-9C3F-4E8A-A5D6-7B1C0E2F3A84}']
    { True when the named property is marked as null }
    function IsNull(const aPropertyName: UTF8String): Boolean;
    { Marks the named property as null }
    procedure SetNull(const aPropertyName: UTF8String);
    { Removes the null mark from the named property }
    procedure ClearNull(const aPropertyName: UTF8String);
  end;

  { TOPFNullableObject:
    framework-provided base class implementing IOPFNullable.
    Uses a sorted TStringList for null property tracking (O(log n) lookup).
    A TPersistent, so that it reports being freed (IFPObserved): change
    tracking only keeps reference states of such objects. }

  TOPFNullableObject = class(TPersistent, IOPFNullable)
  private
    FNullProps: TStringList;
  public
    { Creates the object with no properties marked as null }
    constructor Create;
    { Frees the object }
    destructor Destroy; override;
    { True when the named property is marked as null }
    function IsNull(const aPropertyName: UTF8String): Boolean;
    { Marks the named property as null }
    procedure SetNull(const aPropertyName: UTF8String);
    { Removes the null mark from the named property }
    procedure ClearNull(const aPropertyName: UTF8String);
  end;

  (*
    Pre-declared TNullable specializations with extended RTTI.
    The {$RTTI} directive ensures that FValue and FHasValue fields
    are discoverable via TRttiRecordType.GetFields. 
  *)

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TNullableInteger = specialize TNullable<Integer>;

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TNullableInt64 = specialize TNullable<Int64>;

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TNullableString = specialize TNullable<String>;

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TNullableDouble = specialize TNullable<Double>;

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TNullableBoolean = specialize TNullable<Boolean>;

  {$RTTI EXPLICIT FIELDS([vcPrivate,vcProtected,vcPublic,vcPublished])}
  TNullableDateTime = specialize TNullable<TDateTime>;

implementation

{ TOPFNullableObject }

constructor TOPFNullableObject.Create;
begin
  inherited Create;
  FNullProps := TStringList.Create;
  FNullProps.Sorted := True;
  FNullProps.Duplicates := dupIgnore;
end;

destructor TOPFNullableObject.Destroy;
begin
  FNullProps.Free;
  inherited Destroy;
end;

function TOPFNullableObject.IsNull(const aPropertyName: UTF8String): Boolean;
begin
  Result := FNullProps.IndexOf(aPropertyName) >= 0;
end;

procedure TOPFNullableObject.SetNull(const aPropertyName: UTF8String);
begin
  FNullProps.Add(aPropertyName);
end;

procedure TOPFNullableObject.ClearNull(const aPropertyName: UTF8String);
var
  Idx: Integer;
begin
  Idx := FNullProps.IndexOf(aPropertyName);
  if Idx >= 0 then
    FNullProps.Delete(Idx);
end;

end.
