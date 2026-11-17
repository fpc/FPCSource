{
    This file is part of the Free Pascal component  library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    List management

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.ListReg;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, db,
{$ENDIF}
  Opf.Types;

Type

  TOPListManipulator = class;
  TOPListRegistry = class;
  TOPListRegistryClass = class of TOPListRegistry;

  { TOPListManipulator }

  TOPListManipulator = class
  public
    // Adds an object to the list.
    procedure AddItem(aList: TObject; aItem: TObject); virtual; abstract;
    // Removes all objects from the list.
    procedure ClearList(aList: TObject); virtual; abstract;
    // Returns the number of objects in the list.
    function ItemCount(aList: TObject): Integer; virtual; abstract;
    // Returns the object at the given position in the list.
    function GetItem(aList: TObject; aIndex: Integer): TObject; virtual; abstract;
  end;
  TOPListManipulatorClass = class of TOPListManipulator;

  { TOPListDef }

  TOPListDef = Class(TCollectionItem)
  private
    FListClass: TClass;
    FManipulator: TOPListManipulator;
  Public
    // Frees the list definition and its manipulator.
    Destructor Destroy; override;
    // The list class this definition is for.
    Property ListClass: TClass Read FListClass Write FListClass;
    // The object that adds, removes and reads items of lists of this class. Owned by the definition.
    Property Manipulator: TOPListManipulator Read FManipulator Write FManipulator;
  end;

  { TOPListDefs }

  TOPListDefs = class(TCollection)
  private
    function GetDef(aIndex: Integer): TOPListDef;
  Public
    // The list definitions, by position.
    Property Defs[aIndex: Integer]: TOPListDef Read GetDef; default;
  end;

  { TOPListRegistry }

  TOPListRegistry = Class(TOPBaseComponent)
  Private
    Class var
      _InstanceClass: TOPListRegistryClass;
      _Instance: TOPListRegistry;
  Private
    FDefs: TOPListDefs;
    FLock: TRTLCriticalSection;
    function GetListCount: Integer;
  Public
    // Frees the global list registry when the program ends.
    Class Destructor Cleanup;
    // Sets the class used to create the global registry. Must be called before it is created.
    Class Procedure SetInstanceClass(aClass: TOPListRegistryClass);
    // Returns the global list registry, creating it with the standard list classes if needed.
    Class Function Instance: TOPListRegistry;
    // Frees the global list registry. Not thread-safe: no other thread may
    // use the registry at that time.
    Class Procedure ClearInstance;
  Public
    // Creates an empty list registry.
    Constructor Create(aOwner: TComponent); override;
    // Frees the registry and all its list definitions.
    Destructor Destroy; override;
    // Returns the position of the list class or its nearest registered ancestor, or -1.
    Function IndexOfListClass(aListClass: TClass): Integer;
    // Index of aListClass itself, not of an ancestor
    Function IndexOfExactListClass(aListClass: TClass): Integer;
    // Finds the manipulator for a list class or its nearest registered ancestor, or returns Nil.
    Function FindManipulator(aListClass: TClass): TOPListManipulator;
    // Registers a list class with the manipulator to use for it. The registry takes ownership.
    // Registration and lookup are protected by a lock; registration is meant
    // for program start.
    Function RegisterListClass(aListClass: TClass; aManipulator: TOPListManipulator): TOPListDef;
    // Returns the class of errors raised by this registry.
    class function GetExceptionClass: EOPFClass; override;
    // The number of registered list classes.
    Property ListCount: Integer Read GetListCount;
  end;

  { TOPGObjectListManipulator:
    generic manipulator for TFPGObjectList<T> specializations.
    Not auto-registered, you must specialize and register per concrete list type.

    Example:

      type
        TMyObjectList = specialize TFPGObjectList<TMyObject>;
        TMyListManipulator = specialize TOPGObjectListManipulator<TMyObject>;

      OPListRegistry.RegisterListClass(TMyObjectList, TMyListManipulator.Create);
  }

  generic TOPGObjectListManipulator<T: class> = class(TOPListManipulator)
  public
    // Adds an object to the list.
    procedure AddItem(aList: TObject; aItem: TObject); override;
    // Removes all objects from the list.
    procedure ClearList(aList: TObject); override;
    // Returns the number of objects in the list.
    function ItemCount(aList: TObject): Integer; override;
    // Returns the object at the given position in the list.
    function GetItem(aList: TObject; aIndex: Integer): TObject; override;
  end;

Function OPListRegistry: TOPListRegistry;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Contnrs, System.FGL, System.Generics.Collections,
{$ELSE}
  contnrs, fgl, generics.collections,
{$ENDIF}
  Opf.Consts;

Type

  { TOPFPListManipulator }

  TOPFPListManipulator = class(TOPListManipulator)
  public
    procedure AddItem(aList: TObject; aItem: TObject); override;
    procedure ClearList(aList: TObject); override;
    function ItemCount(aList: TObject): Integer; override;
    function GetItem(aList: TObject; aIndex: Integer): TObject; override;
  end;

  { TOPObjectListManipulator }

  TOPObjectListManipulator = class(TOPListManipulator)
  public
    procedure AddItem(aList: TObject; aItem: TObject); override;
    procedure ClearList(aList: TObject); override;
    function ItemCount(aList: TObject): Integer; override;
    function GetItem(aList: TObject; aIndex: Integer): TObject; override;
  end;

  { TOPFPObjectListManipulator }

  TOPFPObjectListManipulator = class(TOPListManipulator)
  public
    procedure AddItem(aList: TObject; aItem: TObject); override;
    procedure ClearList(aList: TObject); override;
    function ItemCount(aList: TObject): Integer; override;
    function GetItem(aList: TObject; aIndex: Integer): TObject; override;
  end;

procedure RegisterDefaultManipulators(aRegistry: TOPListRegistry); forward;

{ Global accessor }

Function OPListRegistry: TOPListRegistry;
begin
  Result:=TOPListRegistry.Instance;
end;

{ TOPListDef }

destructor TOPListDef.Destroy;
begin
  FreeAndNil(FManipulator);
  inherited Destroy;
end;

{ TOPListDefs }

function TOPListDefs.GetDef(aIndex: Integer): TOPListDef;
begin
  Result:=TOPListDef(Items[aIndex]);
end;

{ TOPListRegistry }

function TOPListRegistry.GetListCount: Integer;
begin
  Result:=FDefs.Count;
end;

class destructor TOPListRegistry.Cleanup;
begin
  ClearInstance;
end;

class procedure TOPListRegistry.SetInstanceClass(aClass: TOPListRegistryClass);
begin
  if Assigned(_Instance) then
    _Instance.RaiseError('SetInstanceClass', SErrListRegCreated);
  _InstanceClass:=aClass;
end;

class function TOPListRegistry.Instance: TOPListRegistry;
var
  L: TOPListRegistry;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
        begin
        if _InstanceClass=Nil then
          _InstanceClass:=TOPListRegistry;
        // Published only when complete: another thread may read it unlocked
        L:=_InstanceClass.Create(Nil);
        RegisterDefaultManipulators(L);
        _Instance:=L;
        end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPListRegistry.ClearInstance;
begin
  EnterCriticalSection(OPFSingletonLock);
  try
    FreeAndNil(_Instance);
  finally
    LeaveCriticalSection(OPFSingletonLock);
  end;
end;

constructor TOPListRegistry.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FLock);
  FDefs:=TOPListDefs.Create(TOPListDef);
end;

destructor TOPListRegistry.Destroy;
begin
  FreeAndNil(FDefs);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

function TOPListRegistry.IndexOfExactListClass(aListClass: TClass): Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := ListCount - 1;
    while (Result >= 0) and (FDefs[Result].ListClass <> aListClass) do
      Dec(Result);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPListRegistry.IndexOfListClass(aListClass: TClass): Integer;
var
  C: TClass;
begin
  EnterCriticalSection(FLock);
  try
    { The class itself first, then the ancestor chain for inherited list types }
    C := aListClass;
    while C <> nil do
    begin
      Result := ListCount - 1;
      while (Result >= 0) and (FDefs[Result].ListClass <> C) do
        Dec(Result);
      if Result >= 0 then
        Exit;
      C := C.ClassParent;
    end;
    Result := -1;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPListRegistry.FindManipulator(aListClass: TClass): TOPListManipulator;
Var
  Idx: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Idx:=IndexOfListClass(aListClass);
    if Idx=-1 then
      Result:=nil
    else
      Result:=FDefs[Idx].Manipulator;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPListRegistry.RegisterListClass(aListClass: TClass; aManipulator: TOPListManipulator): TOPListDef;
begin
  if aListClass=Nil then
  begin
    aManipulator.Free;
    RaiseError('RegisterListClass', SErrNeedListClass);
  end;
  if aManipulator=Nil then
    RaiseError('RegisterListClass', SErrNeedManipulator);
  EnterCriticalSection(FLock);
  try
    // Exact: a subclass of a registered list class gets its own manipulator
    if IndexOfExactListClass(aListClass)<>-1 then
    begin
      aManipulator.Free;
      RaiseError('RegisterListClass', SErrDuplicateListClass, [aListClass.ClassName]);
    end;
    Result:=FDefs.Add as TOPListDef;
    Result.ListClass:=aListClass;
    Result.Manipulator:=aManipulator;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class function TOPListRegistry.GetExceptionClass: EOPFClass;
begin
  Result := EOPFListReg;
end;

{ TOPFPListManipulator }

procedure TOPFPListManipulator.AddItem(aList: TObject; aItem: TObject);
begin
  TFPList(aList).Add(Pointer(aItem));
end;

procedure TOPFPListManipulator.ClearList(aList: TObject);
begin
  TFPList(aList).Clear;
end;

function TOPFPListManipulator.ItemCount(aList: TObject): Integer;
begin
  Result:=TFPList(aList).Count;
end;

function TOPFPListManipulator.GetItem(aList: TObject; aIndex: Integer): TObject;
begin
  Result:=TObject(TFPList(aList).Items[aIndex]);
end;

{ TOPObjectListManipulator }

procedure TOPObjectListManipulator.AddItem(aList: TObject; aItem: TObject);
begin
  TObjectList(aList).Add(aItem);
end;

procedure TOPObjectListManipulator.ClearList(aList: TObject);
begin
  TObjectList(aList).Clear;
end;

function TOPObjectListManipulator.ItemCount(aList: TObject): Integer;
begin
  Result:=TObjectList(aList).Count;
end;

function TOPObjectListManipulator.GetItem(aList: TObject; aIndex: Integer): TObject;
begin
  Result:=TObjectList(aList).Items[aIndex];
end;

{ TOPFPObjectListManipulator }

procedure TOPFPObjectListManipulator.AddItem(aList: TObject; aItem: TObject);
begin
  TFPObjectList(aList).Add(aItem);
end;

procedure TOPFPObjectListManipulator.ClearList(aList: TObject);
begin
  TFPObjectList(aList).Clear;
end;

function TOPFPObjectListManipulator.ItemCount(aList: TObject): Integer;
begin
  Result:=TFPObjectList(aList).Count;
end;

function TOPFPObjectListManipulator.GetItem(aList: TObject; aIndex: Integer): TObject;
begin
  Result:=TFPObjectList(aList).Items[aIndex];
end;

{ TOPGObjectListManipulator }

procedure TOPGObjectListManipulator.AddItem(aList: TObject; aItem: TObject);
begin
  specialize TFPGObjectList<T>(aList).Add(T(aItem));
end;

procedure TOPGObjectListManipulator.ClearList(aList: TObject);
begin
  specialize TFPGObjectList<T>(aList).Clear;
end;

function TOPGObjectListManipulator.ItemCount(aList: TObject): Integer;
begin
  Result:=specialize TFPGObjectList<T>(aList).Count;
end;

function TOPGObjectListManipulator.GetItem(aList: TObject; aIndex: Integer): TObject;
begin
  Result:=specialize TFPGObjectList<T>(aList).Items[aIndex];
end;

{ RegisterDefaultManipulators }

procedure RegisterDefaultManipulators(aRegistry: TOPListRegistry);
begin
  aRegistry.RegisterListClass(TFPList, TOPFPListManipulator.Create);
  aRegistry.RegisterListClass(TObjectList, TOPObjectListManipulator.Create);
  aRegistry.RegisterListClass(TFPObjectList, TOPFPObjectListManipulator.Create);
end;

end.
