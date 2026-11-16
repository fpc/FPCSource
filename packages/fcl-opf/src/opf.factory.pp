{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Class factory for customizable object creation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Factory;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

type
  TOPClassFactory = class;
  TOPClassFactoryClass = class of TOPClassFactory;

  { TOPFactoryReason - why the factory is being asked to create an instance }

  TOPFactoryReason = (
    frListItem,      // Element of a list being loaded
    frChild,         // Child object in a 1:N relationship
    frAssociation,   // Associated object in an N:M relationship
    frProperty       // Object for a class property that is nil (OwnerObject, PropertyName)
  );

  { TOPFactoryContext - context passed to factory callbacks describing
    where and why the instance is needed }

  TOPFactoryContext = class
  private
    FReason: TOPFactoryReason;
    FListObject: TObject;
    FOwnerObject: TObject;
    FPropertyName: UTF8String;
  public
    // Creates a context for the given reason
    constructor Create(aReason: TOPFactoryReason);
    // Why the instance is needed
    property Reason: TOPFactoryReason read FReason write FReason;
    // The list the new instance will be added to, if any
    property ListObject: TObject read FListObject write FListObject;
    // The object that will hold the new instance, if any
    property OwnerObject: TObject read FOwnerObject write FOwnerObject;
    // The property of the owner object that will hold the new instance, if any
    property PropertyName: UTF8String read FPropertyName write FPropertyName;
  end;

  { Factory callback types }

  TOPFactoryMethod = function(aClass: TClass; aContext: TOPFactoryContext): TObject of object;
  TOPFactoryFunc = function(aClass: TClass; aContext: TOPFactoryContext): TObject;

  { TOPFactoryEntry - internal storage for a registered factory }

  TOPFactoryEntry = record
    ForClass: TClass;
    Method: TOPFactoryMethod;
    Func: TOPFactoryFunc;
    IsMethod: Boolean;
    // Only for ForClass itself, not for its descendants
    ExactOnly: Boolean;
  end;
  TOPFactoryEntryArray = array of TOPFactoryEntry;

  { TOPClassFactory - singleton factory for customizable object creation.
    The entries are protected by a lock: registering and creating from
    several threads is safe. A factory callback is called outside the lock. }

  TOPClassFactory = class(TObject)
  private
    class var
      _Instance: TOPClassFactory;
      _InstanceClass: TOPClassFactoryClass;
    var
      FEntries: TOPFactoryEntryArray;
      FLock: TRTLCriticalSection;
    function FindEntry(aClass: TClass): Integer;
    function FindEntryForHierarchy(aClass: TClass): Integer;
    procedure SetEntry(aClass: TClass; aMethod: TOPFactoryMethod; aFunc: TOPFactoryFunc;
      aIsMethod, aExactOnly: Boolean);
  public
    // Creates an empty factory
    constructor Create; virtual;
    // Frees the factory
    destructor Destroy; override;
    // Sets the class used to create the shared factory instance
    class procedure SetInstanceClass(aClass: TOPClassFactoryClass);
    // Returns the shared factory instance, creating it when needed
    class function Instance: TOPClassFactory;
    // True if the singleton exists (Instance would create it)
    class function HasInstance: Boolean;
    // Frees the shared factory instance when the program ends
    class destructor Cleanup;
    // Registers a method that creates objects of aClass and its descendants
    procedure RegisterFactory(aClass: TClass; aFactory: TOPFactoryMethod); virtual; overload;
    // Registers a plain function that creates objects of aClass and its descendants
    procedure RegisterFactory(aClass: TClass; aFactory: TOPFactoryFunc); virtual; overload;
    // Registers aFactory for aClass and its descendants only when no factory
    // creates objects of aClass yet (for aClass itself or an ancestor).
    // Returns False, and changes nothing, when one exists: a factory
    // registered by the application is never replaced.
    function RegisterDefaultFactory(aClass: TClass; aFactory: TOPFactoryFunc): Boolean; virtual;
    // Removes the factory registered for aClass
    procedure UnregisterFactory(aClass: TClass); virtual; overload;
    // A factory for aClass only: descendants use their own constructor
    procedure RegisterExactFactory(aClass: TClass; aFactory: TOPFactoryMethod); virtual;
    // Remove the factory of aClass only if it is a method of aData
    procedure UnregisterFactory(aClass: TClass; aData: Pointer); virtual; overload;
    // Creates an object of aClass with its registered factory, or with its constructor if none
    function CreateInstance(aClass: TClass; aContext: TOPFactoryContext): TObject; virtual;
    // True if a factory is registered that creates objects of aClass
    function HasFactory(aClass: TClass): Boolean;
  end;

function OPFactory: TOPClassFactory;

implementation

uses Opf.Types;

{ TOPFactoryContext }

constructor TOPFactoryContext.Create(aReason: TOPFactoryReason);
begin
  inherited Create;
  FReason := aReason;
  FListObject := nil;
  FOwnerObject := nil;
  FPropertyName := '';
end;

{ TOPClassFactory }

constructor TOPClassFactory.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
end;

destructor TOPClassFactory.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

class procedure TOPClassFactory.SetInstanceClass(aClass: TOPClassFactoryClass);
begin
  if Assigned(_Instance) then
    raise Exception.Create('TOPClassFactory.SetInstanceClass: instance already created');
  _InstanceClass := aClass;
end;

class function TOPClassFactory.Instance: TOPClassFactory;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
      begin
        if _InstanceClass = nil then
          _InstanceClass := TOPClassFactory;
        _Instance := _InstanceClass.Create;
      end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class function TOPClassFactory.HasInstance: Boolean;
begin
  Result := _Instance <> nil;
end;

class destructor TOPClassFactory.Cleanup;
begin
  FreeAndNil(_Instance);
end;

function TOPClassFactory.FindEntry(aClass: TClass): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FEntries) - 1 do
    if FEntries[I].ForClass = aClass then
      Exit(I);
end;

function TOPClassFactory.FindEntryForHierarchy(aClass: TClass): Integer;
var
  C: TClass;
begin
  Result := -1;
  C := aClass;
  while C <> nil do
  begin
    Result := FindEntry(C);
    if Result >= 0 then
      // An exact-only factory does not create descendants
      if not FEntries[Result].ExactOnly or (C = aClass) then
        Exit;
    Result := -1;
    C := C.ClassParent;
  end;
end;

{ Called with the lock held }

procedure TOPClassFactory.SetEntry(aClass: TClass; aMethod: TOPFactoryMethod; aFunc: TOPFactoryFunc;
  aIsMethod, aExactOnly: Boolean);
var
  Idx: Integer;
begin
  Idx := FindEntry(aClass);
  if Idx < 0 then
  begin
    Idx := Length(FEntries);
    SetLength(FEntries, Idx + 1);
    FEntries[Idx].ForClass := aClass;
  end;
  FEntries[Idx].Method := aMethod;
  FEntries[Idx].Func := aFunc;
  FEntries[Idx].IsMethod := aIsMethod;
  FEntries[Idx].ExactOnly := aExactOnly;
end;

procedure TOPClassFactory.RegisterFactory(aClass: TClass; aFactory: TOPFactoryMethod);
begin
  EnterCriticalSection(FLock);
  try
    SetEntry(aClass, aFactory, nil, True, False);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TOPClassFactory.RegisterFactory(aClass: TClass; aFactory: TOPFactoryFunc);
begin
  EnterCriticalSection(FLock);
  try
    SetEntry(aClass, nil, aFactory, False, False);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPClassFactory.RegisterDefaultFactory(aClass: TClass; aFactory: TOPFactoryFunc): Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result := FindEntryForHierarchy(aClass) < 0;
    if Result then
      SetEntry(aClass, nil, aFactory, False, False);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TOPClassFactory.RegisterExactFactory(aClass: TClass; aFactory: TOPFactoryMethod);
begin
  EnterCriticalSection(FLock);
  try
    SetEntry(aClass, aFactory, nil, True, True);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TOPClassFactory.UnregisterFactory(aClass: TClass; aData: Pointer);
var
  Idx: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Idx := FindEntry(aClass);
    if (Idx >= 0) and FEntries[Idx].IsMethod and (TMethod(FEntries[Idx].Method).Data = aData) then
      UnregisterFactory(aClass);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TOPClassFactory.UnregisterFactory(aClass: TClass);
var
  Idx, Last: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Idx := FindEntry(aClass);
    if Idx >= 0 then
    begin
      Last := Length(FEntries) - 1;
      if Idx < Last then
        FEntries[Idx] := FEntries[Last];
      SetLength(FEntries, Last);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPClassFactory.CreateInstance(aClass: TClass; aContext: TOPFactoryContext): TObject;
var
  Idx: Integer;
  Entry: TOPFactoryEntry;
begin
  // A copy: the callback runs outside the lock, and may register factories
  EnterCriticalSection(FLock);
  try
    Idx := FindEntryForHierarchy(aClass);
    if Idx >= 0 then
      Entry := FEntries[Idx];
  finally
    LeaveCriticalSection(FLock);
  end;
  if Idx >= 0 then
  begin
    if Entry.IsMethod then
      Result := Entry.Method(aClass, aContext)
    else
      Result := Entry.Func(aClass, aContext);
  end
  else if aClass.InheritsFrom(TComponent) then
  begin
    if Assigned(aContext) and (aContext.OwnerObject is TComponent) then
      Result := TComponentClass(aClass).Create(TComponent(aContext.OwnerObject))
    else
      Result := TComponentClass(aClass).Create(nil);
  end
  else
    Result := aClass.Create;
end;

function TOPClassFactory.HasFactory(aClass: TClass): Boolean;
begin
  EnterCriticalSection(FLock);
  try
    Result := FindEntryForHierarchy(aClass) >= 0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function OPFactory: TOPClassFactory;
begin
  Result := TOPClassFactory.Instance;
end;

end.
