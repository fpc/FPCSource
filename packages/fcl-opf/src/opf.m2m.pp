{
    This file is part of the Free Pascal component  library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    M2M management: many-to-many associations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.M2M;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  Opf.Types, Opf.Registry;

Type

  TOPManyToManyRegistry = Class;
  TOPManyToManyRegistryClass = class of TOPManyToManyRegistry;
  TOPManyToManyDef = Class;
  TOPManyToManyDefArray = array of TOPManyToManyDef;

  { TOPManyToManyDef }

  TOPManyToManyDef = class(TCollectionItem)
  private
    FClassA: TClass;
    FClassB: TClass;
    FJunctionTable: UTF8String;
    FForeignKeyA: UTF8String;
    FForeignKeyB: UTF8String;
    FDBCascade: Boolean;
    FListPropertyNameA: UTF8String;
    FListPropertyNameB: UTF8String;
  public
    { Copies the settings of another many-to-many definition. }
    procedure Assign(aSource: TPersistent); override;
    { The first of the two linked classes. }
    property ClassA: TClass read FClassA write FClassA;
    { The second of the two linked classes. }
    property ClassB: TClass read FClassB write FClassB;
    { The database table that holds the links between the two classes. }
    property JunctionTable: UTF8String read FJunctionTable write FJunctionTable;
    { The column in the link table that refers to an object of ClassA. }
    property ForeignKeyA: UTF8String read FForeignKeyA write FForeignKeyA;
    { The column in the link table that refers to an object of ClassB. }
    property ForeignKeyB: UTF8String read FForeignKeyB write FForeignKeyB;
    { True if the database deletes the links itself when a linked record is deleted. }
    property DBCascade: Boolean read FDBCascade write FDBCascade;
    { The name of the list property in ClassA that holds the linked ClassB objects. }
    property ListPropertyNameA: UTF8String read FListPropertyNameA write FListPropertyNameA;
    { The name of the list property in ClassB that holds the linked ClassA objects. }
    property ListPropertyNameB: UTF8String read FListPropertyNameB write FListPropertyNameB;
  end;

  { TOPManyToManyDefs }

  TOPManyToManyDefs = class(TCollection)
  private
    function GetDef(aIndex: Integer): TOPManyToManyDef;
  Public
    { The many-to-many definitions, by position. }
    property Defs[aIndex: Integer]: TOPManyToManyDef read GetDef; default;
  end;

  { TOPManyToManyRegistry }

  TOPManyToManyRegistry = Class(TOPBaseComponent)
  Private
    Class var
      _Instance: TOPManyToManyRegistry;
      _InstanceClass: TOPManyToManyRegistryClass;
  Private
    FDefs: TOPManyToManyDefs;
    FRegistry: TOPRegistry;
    function GetDef(aIndex: Integer): TOPManyToManyDef;
    function GetDefCount: Integer;
    function GetRegistry: TOPRegistry;
    procedure SetRegistry(AValue: TOPRegistry);
  Protected
    Function CreateDefs: TOPManyToManyDefs; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    { Frees the global many-to-many registry when the program ends. }
    Class Destructor Cleanup;
    { Sets the class used to create the global registry. Must be called before it is created. }
    Class Procedure SetInstanceClass(aClass: TOPManyToManyRegistryClass);
    { Returns the global many-to-many registry, creating it if needed. }
    Class Function Instance: TOPManyToManyRegistry;
    { Frees the global many-to-many registry. }
    Class Procedure ClearInstance;
  Public
    { Creates an empty many-to-many registry. }
    Constructor Create(aOwner: TComponent); override;
    { Frees the registry and all its definitions. }
    Destructor Destroy; override;
    { Registers a many-to-many link between two stored classes through a link table. }
    Function RegisterManyToMany(aClassA, aClassB: TClass; const aJunctionTable, aForeignKeyA, aForeignKeyB: UTF8String; aDBCascade: Boolean = False; const aListPropertyNameA: UTF8String = ''; const aListPropertyNameB: UTF8String = ''): TOPManyToManyDef;
    { Returns the position of the link between two classes (in either order), or -1. }
    Function IndexOfDef(aClassA, aClassB: TClass): Integer;
    { Finds the link between two classes (in either order), or returns Nil. }
    Function FindDef(aClassA, aClassB: TClass): TOPManyToManyDef;
    { Returns all many-to-many links that involve the given class. }
    Function FindDefsForClass(aClass: TClass): TOPManyToManyDefArray;
    { Returns the class of errors raised by this registry. }
    class function GetExceptionClass: EOPFClass; override;
    { The many-to-many definitions, by position. }
    Property Defs[aIndex: Integer]: TOPManyToManyDef Read GetDef; default;
    { The number of registered many-to-many links. }
    Property DefCount: Integer Read GetDefCount;
  Published
    { The registry of stored classes to check against. Uses the global one if not set. }
    Property Registry: TOPRegistry Read GetRegistry Write SetRegistry;
  end;

Function OPM2MRegistry: TOPManyToManyRegistry;

implementation

uses Opf.Consts, Opf.Logging;

Function OPM2MRegistry: TOPManyToManyRegistry;

begin
  Result:=TOPManyToManyRegistry.Instance;
end;

{ TOPManyToManyDef }

procedure TOPManyToManyDef.Assign(aSource: TPersistent);

var
  D: TOPManyToManyDef absolute aSource;

begin
  if (aSource is TOPManyToManyDef) then
    begin
    FClassA:=D.ClassA;
    FClassB:=D.ClassB;
    FJunctionTable:=D.JunctionTable;
    FForeignKeyA:=D.ForeignKeyA;
    FForeignKeyB:=D.ForeignKeyB;
    FDBCascade:=D.DBCascade;
    FListPropertyNameA:=D.ListPropertyNameA;
    FListPropertyNameB:=D.ListPropertyNameB;
    end
  else
    inherited Assign(aSource);
end;

{ TOPManyToManyDefs }

function TOPManyToManyDefs.GetDef(aIndex: Integer): TOPManyToManyDef;
begin
  Result:=TOPManyToManyDef(Items[aIndex]);
end;

{ TOPManyToManyRegistry }

function TOPManyToManyRegistry.GetDef(aIndex: Integer): TOPManyToManyDef;
begin
  Result:=FDefs[aIndex];
end;

function TOPManyToManyRegistry.GetDefCount: Integer;
begin
  Result:=FDefs.Count;
end;

function TOPManyToManyRegistry.CreateDefs: TOPManyToManyDefs;
begin
  Result:=TOPManyToManyDefs.Create(TOPManyToManyDef);
end;

class procedure TOPManyToManyRegistry.SetInstanceClass(aClass: TOPManyToManyRegistryClass);
begin
  if Assigned(_Instance) then
    _Instance.RaiseError('SetInstanceClass', SErrM2MRegistryCreated);
  _InstanceClass:=aClass;
end;

class function TOPManyToManyRegistry.Instance: TOPManyToManyRegistry;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
        begin
        if _InstanceClass=Nil then
          _InstanceClass:=TOPManyToManyRegistry;
        _Instance:=_InstanceClass.Create(Nil);
        end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPManyToManyRegistry.ClearInstance;
begin
  FreeAndNil(_Instance);
end;

class destructor TOPManyToManyRegistry.Cleanup;
begin
  ClearInstance;
end;

function TOPManyToManyRegistry.GetRegistry: TOPRegistry;
begin
  Result := FRegistry;
  if Result = nil then
    Result := TOPRegistry.Instance;
end;

procedure TOPManyToManyRegistry.SetRegistry(AValue: TOPRegistry);
begin
  if FRegistry = AValue then Exit;
  if Assigned(FRegistry) then
    FRegistry.RemoveFreeNotification(Self);
  FRegistry := AValue;
  if Assigned(FRegistry) then
    FRegistry.FreeNotification(Self);
end;

procedure TOPManyToManyRegistry.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FRegistry) then
    FRegistry := nil;
end;

constructor TOPManyToManyRegistry.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FDefs:=CreateDefs;
end;

destructor TOPManyToManyRegistry.Destroy;
begin
  FreeAndNil(FDefs);
  inherited Destroy;
end;

class function TOPManyToManyRegistry.GetExceptionClass: EOPFClass;
begin
  Result := EOPFManyToMany;
end;

function TOPManyToManyRegistry.IndexOfDef(aClassA, aClassB: TClass): Integer;
begin
  Result:=FDefs.Count-1;
  While (Result>=0) and not (((FDefs[Result].ClassA=aClassA) and (FDefs[Result].ClassB=aClassB))
    or ((FDefs[Result].ClassA=aClassB) and (FDefs[Result].ClassB=aClassA))) do
    Dec(Result);
end;

function TOPManyToManyRegistry.FindDef(aClassA, aClassB: TClass): TOPManyToManyDef;
var
  Idx: Integer;
begin
  Idx:=IndexOfDef(aClassA, aClassB);
  if Idx>=0 then
    Result:=FDefs[Idx]
  else
    Result:=nil;
end;

function TOPManyToManyRegistry.FindDefsForClass(aClass: TClass): TOPManyToManyDefArray;
var
  I, N: Integer;
begin
  SetLength(Result, FDefs.Count);
  N:=0;
  for I:=0 to FDefs.Count-1 do
    if (FDefs[I].ClassA=aClass) or (FDefs[I].ClassB=aClass) then
      begin
      Result[N]:=FDefs[I];
      Inc(N);
      end;
  SetLength(Result, N);
end;

function TOPManyToManyRegistry.RegisterManyToMany(aClassA, aClassB: TClass;
  const aJunctionTable, aForeignKeyA, aForeignKeyB: UTF8String;
  aDBCascade: Boolean = False;
  const aListPropertyNameA: UTF8String = '';
  const aListPropertyNameB: UTF8String = ''): TOPManyToManyDef;

begin
  if aClassA=Nil then
    RaiseError('RegisterManyToMany', SErrM2MNilClassA);
  if aClassB=Nil then
    RaiseError('RegisterManyToMany', SErrM2MNilClassB);
  if aClassA=aClassB then
    RaiseError('RegisterManyToMany', SErrM2MSelfRelation, [aClassA.ClassName]);
  if aJunctionTable='' then
    RaiseError('RegisterManyToMany', SErrM2MEmptyJunctionTable);
  if aForeignKeyA='' then
    RaiseError('RegisterManyToMany', SErrM2MEmptyForeignKeyA);
  if aForeignKeyB='' then
    RaiseError('RegisterManyToMany', SErrM2MEmptyForeignKeyB);
  if Registry.FindPersister(aClassA)=nil then
    RaiseError('RegisterManyToMany', SErrM2MClassANotRegistered, [aClassA.ClassName]);
  if Registry.FindPersister(aClassB)=nil then
    RaiseError('RegisterManyToMany', SErrM2MClassBNotRegistered, [aClassB.ClassName]);
  if IndexOfDef(aClassA, aClassB)<>-1 then
    RaiseError('RegisterManyToMany', SErrDuplicateM2M, [aClassA.ClassName, aClassB.ClassName]);
  Result:=FDefs.Add as TOPManyToManyDef;
  Result.ClassA:=aClassA;
  Result.ClassB:=aClassB;
  Result.JunctionTable:=aJunctionTable;
  Result.ForeignKeyA:=aForeignKeyA;
  Result.ForeignKeyB:=aForeignKeyB;
  Result.DBCascade:=aDBCascade;
  Result.ListPropertyNameA:=aListPropertyNameA;
  Result.ListPropertyNameB:=aListPropertyNameB;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkInfo) then
    TOPLogger(GetLogger).LogInfo(Self, 'Registered M2M: %s <-> %s via %s', [aClassA.ClassName, aClassB.ClassName, aJunctionTable]);
  {$ENDIF}
end;

end.
