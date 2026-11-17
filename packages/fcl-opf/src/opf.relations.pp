{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Manage relations between objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Relations;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, db,
{$ENDIF}
  Opf.Types, Opf.Registry, Opf.Fields;

Type

  TOPRelationRegistry = Class;
  TOPRelationRegistryClass = class of TOPRelationRegistry;
  TOPRelationDef = Class;
  TOPRelationDefArray = array of TOPRelationDef;

  { TOPRelationDef }

  TOPRelationDef = class(TCollectionItem)
  private
    FParentClass: TClass;
    FChildClass: TClass;
    FForeignKeyFieldName: UTF8String;
    FConstraintName: UTF8String;
    FDBCascade: Boolean;
    FListPropertyName: UTF8String;
  public
    { Copies the settings of another relation definition. }
    procedure Assign(aSource: TPersistent); override;
    { The class on the "one" side of the relation. }
    property ParentClass: TClass read FParentClass write FParentClass;
    { The class on the "many" side of the relation. }
    property ChildClass: TClass read FChildClass write FChildClass;
    { The field of the child class that holds the key of its parent. }
    property ForeignKeyFieldName: UTF8String read FForeignKeyFieldName write FForeignKeyFieldName;
    { The name of the foreign key constraint in the database. }
    property ConstraintName: UTF8String read FConstraintName write FConstraintName;
    { True if the database deletes the child records itself when the parent record is deleted. }
    property DBCascade: Boolean read FDBCascade write FDBCascade;
    { The name of the list property in the parent class that holds the child objects. }
    property ListPropertyName: UTF8String read FListPropertyName write FListPropertyName;
  end;

  { TOPRelationDefs }

  TOPRelationDefs = class(TCollection)
  private
    function GetRelation(aIndex: Integer): TOPRelationDef;
  Public
    { The relation definitions, by position. }
    property Relations[aIndex: Integer]: TOPRelationDef read GetRelation; default;
  end;

  { TOPRelationRegistry }

  TOPRelationRegistry = Class(TOPBaseComponent)
  Private
    Class var
      _Instance: TOPRelationRegistry;
      _InstanceClass: TOPRelationRegistryClass;
  Private
    FDefs: TOPRelationDefs;
    FRegistry: TOPRegistry;
    function GetRelation(aIndex: Integer): TOPRelationDef;
    function GetRelationCount: Integer;
    function GetRegistry: TOPRegistry;
    procedure SetRegistry(AValue: TOPRegistry);
  Protected
    Function CreateDefs: TOPRelationDefs; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    { Frees the global relation registry when the program ends. }
    Class Destructor Cleanup;
    { Sets the class used to create the global registry. Must be called before it is created. }
    Class Procedure SetInstanceClass(aClass: TOPRelationRegistryClass);
    { Returns the global relation registry, creating it if needed. }
    Class Function Instance: TOPRelationRegistry;
    { Frees the global relation registry. }
    Class Procedure ClearInstance;
    { Removes the relations of the global relation registry when it uses the
      global class registry (Registry not set). Called when the global class
      registry is freed. }
    Class Procedure ClearGlobalRelations;
  Public
    { Creates an empty relation registry. }
    Constructor Create(aOwner: TComponent); override;
    { Frees the registry and all its relations. }
    Destructor Destroy; override;
    { Registers a parent-child relation between two stored classes. }
    Function RegisterRelationship(aParentClass, aChildClass: TClass; const aForeignKeyField: UTF8String; aDBCascade: Boolean = False; const aListPropertyName: UTF8String = ''): TOPRelationDef;
    { Returns all relations in which the given class is the parent. }
    Function FindChildRelations(aParentClass: TClass): TOPRelationDefArray;
    { Returns the first relation in which the given class is the child, or Nil. }
    Function FindParentRelation(aChildClass: TClass): TOPRelationDef;
    { Returns the position of the relation between a parent and a child class, or -1. }
    Function IndexOfRelation(aParentClass, aChildClass: TClass): Integer;
    { Removes all relations. }
    Procedure Clear;
    { Returns the class of errors raised by this registry. }
    class function GetExceptionClass: EOPFClass; override;
    { The relation definitions, by position. }
    Property Relations[aIndex: Integer]: TOPRelationDef Read GetRelation; default;
    { The number of registered relations. }
    Property RelationCount: Integer Read GetRelationCount;
  Published
    { The registry of stored classes to check against. Uses the global one if not set. }
    Property Registry: TOPRegistry Read GetRegistry Write SetRegistry;
  end;

Function OPRelationRegistry: TOPRelationRegistry;

implementation

uses Opf.Consts;

Function OPRelationRegistry: TOPRelationRegistry;

begin
  Result:=TOPRelationRegistry.Instance;
end;

{ TOPRelationDef }

procedure TOPRelationDef.Assign(aSource: TPersistent);

var
  D: TOPRelationDef absolute aSource;

begin
  if (aSource is TOPRelationDef) then
    begin
    FParentClass:=D.ParentClass;
    FChildClass:=D.ChildClass;
    FForeignKeyFieldName:=D.ForeignKeyFieldName;
    FConstraintName:=D.ConstraintName;
    FDBCascade:=D.DBCascade;
    FListPropertyName:=D.ListPropertyName;
    end
  else
    inherited Assign(aSource);
end;

{ TOPRelationDefs }

function TOPRelationDefs.GetRelation(aIndex: Integer): TOPRelationDef;

begin
  Result:=TOPRelationDef(Items[aIndex]);
end;

{ TOPRelationRegistry }

function TOPRelationRegistry.GetRelation(aIndex: Integer): TOPRelationDef;

begin
  Result:=FDefs[aIndex];
end;

function TOPRelationRegistry.GetRelationCount: Integer;

begin
  Result:=FDefs.Count;
end;

function TOPRelationRegistry.CreateDefs: TOPRelationDefs;

begin
  Result:=TOPRelationDefs.Create(TOPRelationDef);
end;

class procedure TOPRelationRegistry.SetInstanceClass(aClass: TOPRelationRegistryClass);

begin
  if Assigned(_Instance) then
    _Instance.RaiseError('SetInstanceClass', SErrRelationRegistryCreated);
  _InstanceClass:=aClass;
end;

class function TOPRelationRegistry.Instance: TOPRelationRegistry;

begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
        begin
        if _InstanceClass=Nil then
          _InstanceClass:=TOPRelationRegistry;
        _Instance:=_InstanceClass.Create(Nil);
        end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPRelationRegistry.ClearInstance;

begin
  FreeAndNil(_Instance);
end;

class procedure TOPRelationRegistry.ClearGlobalRelations;

begin
  if Assigned(_Instance) and (_Instance.FRegistry=Nil) then
    _Instance.Clear;
end;

procedure TOPRelationRegistry.Clear;

begin
  FDefs.Clear;
end;

class destructor TOPRelationRegistry.Cleanup;

begin
  ClearInstance;
end;

function TOPRelationRegistry.GetRegistry: TOPRegistry;

begin
  Result := FRegistry;
  if Result = nil then
    Result := TOPRegistry.Instance;
end;

procedure TOPRelationRegistry.SetRegistry(AValue: TOPRegistry);

begin
  if FRegistry = AValue then Exit;
  if Assigned(FRegistry) then
    FRegistry.RemoveFreeNotification(Self);
  FRegistry := AValue;
  if Assigned(FRegistry) then
    FRegistry.FreeNotification(Self);
end;

procedure TOPRelationRegistry.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FRegistry) then
    FRegistry := nil;
end;

constructor TOPRelationRegistry.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FDefs:=CreateDefs;
end;

destructor TOPRelationRegistry.Destroy;

begin
  FreeAndNil(FDefs);
  inherited Destroy;
end;

class function TOPRelationRegistry.GetExceptionClass: EOPFClass;

begin
  Result := EOPFRelation;
end;

function TOPRelationRegistry.IndexOfRelation(aParentClass, aChildClass: TClass): Integer;

begin
  Result:=FDefs.Count-1;
  While (Result>=0) and not ((FDefs[Result].ParentClass=aParentClass) and (FDefs[Result].ChildClass=aChildClass)) do
    Dec(Result);
end;

function TOPRelationRegistry.FindChildRelations(aParentClass: TClass): TOPRelationDefArray;

var
  I, Cnt: Integer;

begin
  SetLength(Result, FDefs.Count);
  Cnt:=0;
  for I:=0 to FDefs.Count-1 do
    if FDefs[I].ParentClass=aParentClass then
      begin
      Result[Cnt]:=FDefs[I];
      Inc(Cnt);
      end;
  SetLength(Result, Cnt);
end;

function TOPRelationRegistry.FindParentRelation(aChildClass: TClass): TOPRelationDef;

var
  I: Integer;

begin
  Result:=nil;
  for I:=0 to FDefs.Count-1 do
    if FDefs[I].ChildClass=aChildClass then
      Exit(FDefs[I]);
end;

function TOPRelationRegistry.RegisterRelationship(aParentClass, aChildClass: TClass;
  const aForeignKeyField: UTF8String; aDBCascade: Boolean = False; const aListPropertyName: UTF8String = ''): TOPRelationDef;

var
  ParentDef, ChildDef: TPersisterDef;

begin
  ParentDef:=Registry.FindPersister(aParentClass);
  if ParentDef=nil then
    RaiseError('RegisterRelationship', SErrRelationParentNotRegistered, [aParentClass.ClassName]);
  ChildDef:=Registry.FindPersister(aChildClass);
  if ChildDef=nil then
    RaiseError('RegisterRelationship', SErrRelationChildNotRegistered, [aChildClass.ClassName]);
  if IndexOfRelation(aParentClass, aChildClass)<>-1 then
    RaiseError('RegisterRelationship', SErrDuplicateRelationship, [aParentClass.ClassName, aChildClass.ClassName]);
  if ChildDef.FieldMap.FindByFieldName(aForeignKeyField)=nil then
    RaiseError('RegisterRelationship', SErrRelationForeignKeyNotFound, [aForeignKeyField, aChildClass.ClassName]);
  Result:=FDefs.Add as TOPRelationDef;
  Result.ParentClass:=aParentClass;
  Result.ChildClass:=aChildClass;
  Result.ForeignKeyFieldName:=aForeignKeyField;
  Result.DBCascade:=aDBCascade;
  Result.ListPropertyName:=aListPropertyName;
end;

end.
