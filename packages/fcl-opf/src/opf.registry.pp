{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    The object registry: use this to register all your classes that must be persisted

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Registry;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, db,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.Factory;

Const
  SGUIDIOPPrimaryKey = '{937F69B6-B4B0-4A95-AFA6-005BD80573FD}';

Type

  TOPPersister = Class;
  TPersisterDef = Class;
  TOPRegistry = Class;
  TOPRegistryClass = class of TOPRegistry;

  { TOPPersister }

  TOPPersister = class
  Public
    // Default is class name minus initial "T".
    Class Function GetTableName(aClass : TClass) : UTF8String; virtual;
    // Applies the persister options of a class definition. Does nothing by default.
    Procedure ApplyOptions(aOptions : TStrings); virtual;
    // Fills aMap with the fields to store for aClass.
    Procedure PopulateFieldMap(aClass : TClass; aMap :  TOPFieldMapList); virtual; abstract;
    // Copies the value of a database field into a property of the object.
    Function ApplyProperty(Instance : TObject; aFieldMap : TOPFieldMap; aField : TField) : Boolean; virtual; abstract;
    // Copies the value of a property of the object into a query parameter.
    Function PersistProperty(Instance : TObject; aFieldMap : TOPFieldMap; aParam : TParam) : Boolean; virtual; abstract;
    // Called after registration is complete. Override to perform post-registration
    // setup such as factory registration.
    Procedure AfterRegister(aDef: TPersisterDef); virtual;
  end;
  TOPPersisterClass = Class of TOPPersister;

  { IOPRegistrationListener }

  // Receives the class definitions a registry creates with a persister class.
  // An exception raised by either method undoes the registration.
  IOPRegistrationListener = interface ['{6C2E7D0B-5A1F-4E86-9B3D-2F7A4C8E1D59}']
    // Called when the field map is filled, before the primary key is resolved.
    procedure ClassRegistering(aDef: TPersisterDef);
    // Called when the definition and its attribute relations are complete.
    procedure ClassRegistered(aDef: TPersisterDef);
  end;

  { TPersisterDef }

  TPersisterDef = Class(TCollectionItem)
  private
    FFieldMap: TOPFieldMapList;
    FCacheSQL: Boolean;
    FHasIOPPrimaryKey: Boolean;
    FOptions: TStringList;
    FInstanceClass: TClass;
    FPersisterClass: TOPPersisterClass;
    FDeleteSQL: UTF8string;
    FInsertSQL: UTF8string;
    FInstanceSQL: UTF8string;
    FListSQL: UTF8string;
    FTableName: UTF8String;
    FUpdateSQL: UTF8string;
    FPKConstraintName: UTF8String;
    FIndexes: TOPIndexDefArray;
    FConstructorCode: CodePointer;
    FAutoCreated: Boolean;
    function GetOptions: TStrings;
    function GetTableName: UTF8String;
    procedure SetOptions(AValue: TStrings);
  Protected
    procedure SetFieldMap(AValue: TOPFieldMapList);
  Public
    // Creates an empty class definition.
    Constructor Create(aCollection : TCollection); override;
    // Frees the class definition and its field list.
    Destructor Destroy; override;
    // Copies the settings of another class definition.
    Procedure Assign(aSource : TPersistent); override;
    // Returns True if the options list has been created.
    Function HasOptions : Boolean;
    // Returns True if any field is marked as part of the primary key.
    Function HasKeyFields: Boolean;
    // Determines the primary key fields, raising an error if none can be found.
    Procedure ResolvePrimaryKey; virtual;
    // Raises an error for aOperation with the given message.
    procedure RaiseError(const aOperation, aReason: string); virtual; overload;
    // Raises an error for aOperation with a message built from a format and arguments.
    procedure RaiseError(const aOperation, aFmt: string; const aArgs: array of const); virtual; overload;
    // The first 2 are mandatory.
    Property PersisterClass : TOPPersisterClass Read FPersisterClass Write FPersisterClass;
    // The class whose objects are stored.
    Property InstanceClass : TClass Read FInstanceClass Write FInstanceClass;
    // Default is class name minus initial "T"
    Property TableName : UTF8String Read GetTableName Write FTableName;
    // The following are optional.
    // Extra settings for the class, as name=value lines
    Property Options : TStrings Read GetOptions Write SetOptions;
    // If True, the SQLs will be built once when needed, but kept for subsequent executions.
    Property CacheSQL : Boolean Read FCacheSQL Write FCacheSQL;
    // If empty, the following are built on an as-need basis and optionally kept.
    // The SQL statement used to insert an object.
    Property InsertSQL : UTF8string Read FInsertSQL Write FInsertSQL;
    // The SQL statement used to update an object.
    Property UpdateSQL : UTF8string Read FUpdateSQL Write FUpdateSQL;
    // The SQL statement used to delete an object.
    Property DeleteSQL : UTF8string Read FDeleteSQL Write FDeleteSQL;
    // The SQL statement used to load a single object.
    Property InstanceSQL : UTF8string Read FInstanceSQL Write FInstanceSQL;
    // The SQL statement used to load a list of objects.
    Property ListSQL : UTF8string Read FListSQL Write FListSQL;
    // The list of fields that are stored, and how they map to the database table.
    Property FieldMap : TOPFieldMapList Read FFieldMap Write SetFieldMap;
    // True if the class provides its own primary key through the IOPPrimaryKey interface.
    Property HasIOPPrimaryKey : Boolean Read FHasIOPPrimaryKey;
    // The name of the primary key constraint in the database table.
    Property PKConstraintName : UTF8String Read FPKConstraintName Write FPKConstraintName;
    // The indexes declared for the database table.
    Property Indexes : TOPIndexDefArray Read FIndexes Write FIndexes;
    // Creates an object of the class with its own constructor. TExtRTTIPersister
    // registers a factory of its own that does not depend on the definition;
    // this method can be registered by the application.
    function RttiFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
    // The constructor used to create new objects of the class, if one was found.
    Property ConstructorCode : CodePointer Read FConstructorCode Write FConstructorCode;
    // True if a later RegisterPersister for the same class replaces this definition instead of raising.
    Property AutoCreated : Boolean Read FAutoCreated Write FAutoCreated;
  end;

  { TPersisterDefs }

  TPersisterDefs = class(TCollection)
  private
    function GetDef(aIndex : Integer): TPersisterDef;
  Public
    // The class definitions, by position.
    Property Defs [aIndex : Integer] : TPersisterDef Read GetDef; default;
  end;

  { TOPRegistry }

  TOPRegistry = Class (TOPBaseComponent)
  Private
    Class var
       _InstanceClass : TOPRegistryClass;
       _Instance : TOPRegistry;
  Private
    FDefs : TPersisterDefs;
    FRelationRegistry : TComponent;
    FOwnsRelationRegistry : Boolean;
    FLock : TRTLCriticalSection;
    FRegistrationListener : IOPRegistrationListener;
    function GetPersister(aIndex : Integer): TPersisterDef;
    function GetPersisterCount: Integer;
    function GetRelationRegistry: TComponent;
    procedure SetRelationRegistry(aValue: TComponent);
    function FindDefByClassName(const aName: UTF8String): TPersisterDef;
    Function DoRegisterPersister(aInstanceClass : TClass; aPersisterClass : TOPPersisterClass) : TPersisterDef;
  Protected
    Function CreateDefs : TPersisterDefs; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    // Frees the global registry when the program ends.
    Class Destructor cleanup;
    // Sets the class used to create the global registry. Must be called before it is created.
    Class Procedure SetInstanceClass(aClass: TOPRegistryClass);
    // Returns the global registry, creating it if needed.
    Class Function Instance : TOPRegistry;
    // Frees the global registry. The relations of its classes are removed from
    // the global relation registry. Not thread-safe: no other thread may use
    // the registry at that time.
    Class Procedure ClearInstance;
  Public
    // Creates an empty registry.
    Constructor Create(aOwner : TComponent); override;
    // Frees the registry and all its class definitions.
    Destructor Destroy; override;
    // Returns the position of the definition for a class, or -1 if it is not registered.
    Function IndexOfPersister(aInstanceClass : TClass) : Integer;
    // Finds the definition for a class, or returns Nil if it is not registered.
    Function FindPersister(aInstanceClass : TClass) : TPersisterDef;
    // Registers a class to be stored, and returns its new definition.
    // Registration and lookup are protected by a lock, but registration is
    // meant for program start: a definition must stay alive while other
    // threads use it.
    Function RegisterPersister(aInstanceClass : TClass; aPersisterClass : TOPPersisterClass) : TPersisterDef;
    // Returns the class of errors raised by the registry.
    class function GetExceptionClass: EOPFClass; override;
    // The class definitions, by position.
    Property Persisters[aIndex : Integer] : TPersisterDef Read GetPersister; default;
    // The number of registered classes.
    Property PersisterCount : Integer Read GetPersisterCount;
    // TOPRelationRegistry for relations declared with OPRelation attributes.
    // The global registry uses the global relation registry; another registry
    // creates one of its own unless one is assigned.
    Property RelationRegistry : TComponent Read GetRelationRegistry Write SetRelationRegistry;
    // Notified of every class registered with a persister class; Nil for none.
    Property RegistrationListener : IOPRegistrationListener Read FRegistrationListener Write FRegistrationListener;
  end;

Function OPRegistry : TOPRegistry;

implementation

uses Opf.Consts, Opf.Logging, Opf.Relations
  {$IFNDEF VER3_2}
  , Opf.Persister.ExtRTTI
  {$ENDIF}
  ;

Function OPRegistry : TOPRegistry;

begin
  Result:=TOPRegistry.Instance;
end;

{ TOPPersister }

class function TOPPersister.GetTableName(aClass: TClass): UTF8String;
begin
  Result:=aClass.ClassName;
  if (Upcase(Result[1])='T') then
    Result:=Copy(Result,2,Length(Result)-1);
end;

procedure TOPPersister.ApplyOptions(aOptions: TStrings);
begin
  // Do nothing
end;

procedure TOPPersister.AfterRegister(aDef: TPersisterDef);
begin
  // Do nothing - override in subclasses for post-registration setup
end;

{ TPersisterDefs }

function TPersisterDefs.GetDef(aIndex : Integer): TPersisterDef;
begin
  Result:=TPersisterDef(Items[aIndex])
end;


{ TOPRegistry }

function TOPRegistry.GetPersister(aIndex : Integer): TPersisterDef;
begin
  Result:=FDefs[aIndex]
end;

function TOPRegistry.GetPersisterCount: Integer;
begin
  Result:=FDefs.Count;
end;

function TOPRegistry.CreateDefs: TPersisterDefs;
begin
  Result:=TPersisterDefs.Create(TPersisterDef);
end;

class procedure TOPRegistry.SetInstanceClass(aClass: TOPRegistryClass);
begin
  if Assigned(_Instance) then
     _Instance.RaiseError('SetInstanceClass', SErrRegistryCreated);
  _InstanceClass:=aClass;
end;

class function TOPRegistry.Instance: TOPRegistry;
begin
  if _Instance=Nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Instance=Nil then
         begin
         if _InstanceClass=Nil then
            _InstanceClass:=TOPRegistry;
         _Instance:=_InstanceClass.Create(Nil);
         end;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result:=_Instance;
end;

class procedure TOPRegistry.ClearInstance;
begin
  EnterCriticalSection(OPFSingletonLock);
  try
    if Assigned(_Instance) then
      // Relations between the classes of the global registry: they would
      // refer to classes that are no longer registered
      TOPRelationRegistry.ClearGlobalRelations;
    FreeAndNil(_Instance);
  finally
    LeaveCriticalSection(OPFSingletonLock);
  end;
end;

class destructor TOPRegistry.cleanup;
begin
  ClearInstance;
end;

constructor TOPRegistry.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitCriticalSection(FLock);
  FDefs:=CreateDefs;
end;

destructor TOPRegistry.Destroy;
begin
  if FOwnsRelationRegistry then
    FreeAndNil(FRelationRegistry)
  else
    RelationRegistry:=Nil;
  FreeAndNil(FDefs);
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

function TOPRegistry.IndexOfPersister(aInstanceClass: TClass): Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result:=PersisterCount-1;
    While (Result>=0) and (Persisters[Result].InstanceClass<>aInstanceClass) do
      Dec(Result);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPRegistry.FindPersister(aInstanceClass: TClass): TPersisterDef;

Var
  Idx : Integer;

begin
  Idx:=IndexOfPersister(aInstanceClass);
  if Idx=-1 then
    Result:=nil
  else
    Result:=Persisters[Idx];
end;

class function TOPRegistry.GetExceptionClass: EOPFClass;
begin
  Result := EOPFRegistry;
end;

function TOPRegistry.GetRelationRegistry: TComponent;
var
  R: TOPRelationRegistry;
begin
  if FRelationRegistry=Nil then
  begin
    if Self=_Instance then
      Result:=TOPRelationRegistry.Instance
    else
    begin
      // Relations between the classes of this registry
      R:=TOPRelationRegistry.Create(Nil);
      R.Registry:=Self;
      FRelationRegistry:=R;
      FOwnsRelationRegistry:=True;
      Result:=R;
    end;
  end
  else
    Result:=FRelationRegistry;
end;

procedure TOPRegistry.SetRelationRegistry(aValue: TComponent);
begin
  if FRelationRegistry=aValue then
    Exit;
  if Assigned(FRelationRegistry) then
  begin
    if FOwnsRelationRegistry then
      FRelationRegistry.Free
    else
      FRelationRegistry.RemoveFreeNotification(Self);
  end;
  FOwnsRelationRegistry:=False;
  FRelationRegistry:=aValue;
  if Assigned(FRelationRegistry) then
    FRelationRegistry.FreeNotification(Self);
end;

procedure TOPRegistry.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FRelationRegistry) then
  begin
    FRelationRegistry:=Nil;
    FOwnsRelationRegistry:=False;
  end;
end;

{ The registered class named aName: a class name, or UnitName.ClassName to
  tell apart classes with the same name. Raises when the name is ambiguous. }

function TOPRegistry.FindDefByClassName(const aName: UTF8String): TPersisterDef;
var
  J: Integer;
  Qualified: Boolean;
  N: UTF8String;
begin
  Result:=Nil;
  Qualified:=Pos('.', aName)>0;
  for J:=0 to FDefs.Count-1 do
  begin
    if Qualified then
      N:=FDefs[J].InstanceClass.UnitName+'.'+FDefs[J].InstanceClass.ClassName
    else
      N:=FDefs[J].InstanceClass.ClassName;
    if CompareText(N, aName)<>0 then
      Continue;
    if Result<>Nil then
      RaiseError('RegisterPersister', SErrAmbiguousParentClass, [aName]);
    Result:=FDefs[J];
  end;
end;

function TOPRegistry.RegisterPersister(aInstanceClass: TClass; aPersisterClass: TOPPersisterClass): TPersisterDef;
begin
  EnterCriticalSection(FLock);
  try
    Result:=DoRegisterPersister(aInstanceClass, aPersisterClass);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TOPRegistry.DoRegisterPersister(aInstanceClass: TClass; aPersisterClass: TOPPersisterClass): TPersisterDef;
var
  Persister: TOPPersister;
  lOldDef: TPersisterDef;
  {$IFNDEF VER3_2}
  DeferredRels: TOPDeferredRelationArray;
  HasAttrPK: Boolean;
  I, lRelIdx: Integer;
  DR: TOPDeferredRelation;
  ParentDef: TPersisterDef;
  RelDef: TOPRelationDef;
  Added: TFPList;
  {$ENDIF}
begin
  if aInstanceClass=Nil then
    RaiseError('RegisterClass', SErrNeedClass);
  lOldDef:=FindPersister(aInstanceClass);
  if (lOldDef<>Nil) and not lOldDef.AutoCreated then
    RaiseError('RegisterClass', SErrDuplicatePersister, [aInstanceClass.ClassName]);
  {$IFNDEF VER3_2}
  SetLength(DeferredRels, 0);
  HasAttrPK:=False;
  Added:=TFPList.Create;
  try
  {$ENDIF}
  Result:=FDefs.Add as TPersisterDef;
  try
    Result.InstanceClass:=aInstanceClass;
    Result.PersisterClass:=aPersisterClass;
    // Always a field map: without persister class the caller fills it
    Result.FieldMap:=TOPFieldMapList.Create(TOPFieldMap);
    if aPersisterClass<>Nil then
      begin
      Result.FieldMap.DefaultFieldOptions:=[foInSelect, foInInsert, foInUpdate];
      Persister:=aPersisterClass.Create;
      try
        Persister.PopulateFieldMap(aInstanceClass, Result.FieldMap);
        {$IFNDEF VER3_2}
        // Transfer attribute metadata if TAttributePersister was used
        if Persister is TAttributePersister then
        begin
          Result.Indexes:=TAttributePersister(Persister).Indexes;
          Result.PKConstraintName:=TAttributePersister(Persister).PKConstraintName;
          DeferredRels:=TAttributePersister(Persister).DeferredRelations;
          HasAttrPK:=TAttributePersister(Persister).HasAttributePK;
        end;
        {$ENDIF}
        Persister.AfterRegister(Result);
      finally
        Persister.Free;
      end;
      if Assigned(FRegistrationListener) then
        FRegistrationListener.ClassRegistering(Result);
      if Result.FieldMap.Count>0 then
      begin
        {$IFNDEF VER3_2}
        // If TAttributePersister had OPPrimaryKey attribute, skip convention detection
        if not HasAttrPK then
        {$ENDIF}
          Result.ResolvePrimaryKey;
      end;
      end;
    {$IFNDEF VER3_2}
    // Relations from OPRelation attributes: part of the registration, so a
    // failure leaves nothing behind and the class can be registered again
    for I:=0 to High(DeferredRels) do
    begin
      DR:=DeferredRels[I];
      ParentDef:=FindDefByClassName(DR.ParentClassName);
      if ParentDef = nil then
        RaiseError('RegisterPersister', SErrParentNotRegistered,
          [DR.ParentClassName, aInstanceClass.ClassName]);
      lRelIdx:=TOPRelationRegistry(RelationRegistry).IndexOfRelation(ParentDef.InstanceClass, aInstanceClass);
      if (lOldDef<>Nil) and (lRelIdx<>-1) then
      begin
        // Registered for the auto-created definition
        RelDef:=TOPRelationRegistry(RelationRegistry).Relations[lRelIdx];
        RelDef.ForeignKeyFieldName:=DR.FKFieldName;
        RelDef.DBCascade:=DR.DBCascade;
      end
      else
      begin
        RelDef:=TOPRelationRegistry(RelationRegistry).RegisterRelationship(
          ParentDef.InstanceClass, aInstanceClass, DR.FKFieldName, DR.DBCascade);
        Added.Add(RelDef);
      end;
      // The foreign key name of the attribute names the constraint
      RelDef.ConstraintName:=DR.FKName;
    end;
    {$ENDIF}
    if (aPersisterClass<>Nil) and Assigned(FRegistrationListener) then
      FRegistrationListener.ClassRegistered(Result);
  except
    {$IFNDEF VER3_2}
    for I:=Added.Count-1 downto 0 do
      TObject(Added[I]).Free;
    {$ENDIF}
    // A factory the persister registered stays: it does not depend on the
    // definition and never replaced one of the application
    FDefs.Delete(FDefs.Count-1);
    raise;
  end;
  {$IFNDEF VER3_2}
  finally
    Added.Free;
  end;
  {$ENDIF}
  if lOldDef<>Nil then
    lOldDef.Free;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkInfo) then
    TOPLogger(GetLogger).LogInfo(Self, 'Registered persister for class "%s" with %d fields', [aInstanceClass.ClassName, Result.FieldMap.Count]);
  {$ENDIF}
end;


{ TPersisterDef }

type
  TConstructorFunc = function(aClass: TClass; aFlag: Integer): TObject;

function TPersisterDef.RttiFactory(aClass: TClass; aContext: TOPFactoryContext): TObject;
var
  Code: CodePointer;
begin
  Code := FConstructorCode;
  {$IFNDEF VER3_2}
  // A descendant runs its own constructor when it has one
  if (aClass <> FInstanceClass) and (OPFFindDefaultConstructor(aClass) <> nil) then
    Code := OPFFindDefaultConstructor(aClass);
  {$ENDIF}
  if Assigned(Code) then
    Result := TConstructorFunc(Code)(aClass, 1)
  else
    Result := aClass.Create;
end;

procedure TPersisterDef.RaiseError(const aOperation, aReason: string);
begin
  OPFRaiseError(EOPFRegistry, ClassName, aOperation, aReason);
end;

procedure TPersisterDef.RaiseError(const aOperation, aFmt: string; const aArgs: array of const);
begin
  OPFRaiseError(EOPFRegistry, ClassName, aOperation, aFmt, aArgs);
end;

function TPersisterDef.HasKeyFields: Boolean;
var
  I: Integer;
begin
  Result:=False;
  if FFieldMap=nil then
    Exit;
  for I:=0 to FFieldMap.Count-1 do
    if foInKey in FFieldMap[I].Options then
      Exit(True);
end;

procedure TPersisterDef.ResolvePrimaryKey;
var
  ClsName, ConventionName: UTF8String;
  F: TOPFieldMap;
begin
  if FFieldMap=nil then
    RaiseError('ResolvePrimaryKey', SErrNoPrimaryKey, ['(no field map)', '']);
  FHasIOPPrimaryKey:=False;
  // 1. Check explicit keys
  if HasKeyFields then
    Exit;
  // 2. Check IOPPrimaryKey interface (CORBA - use GetInterfaceEntryByStr)
  if Assigned(FInstanceClass) and
     (FInstanceClass.GetInterfaceEntryByStr(SGUIDIOPPrimaryKey) <> nil) then
  begin
    FHasIOPPrimaryKey:=True;
    Exit;
  end;
  // 3. Convention detection: try 'ID' first, then '<ClassName>ID'
  F:=FFieldMap.FindByPropertyName('ID');
  if F<>nil then
  begin
    F.Options:=F.Options + [foInKey];
    Exit;
  end;
  // Derive convention name: class name minus leading 'T'
  if Assigned(FInstanceClass) then
  begin
    ClsName:=FInstanceClass.ClassName;
    if (Length(ClsName)>1) and (UpCase(ClsName[1])='T') then
      ConventionName:=Copy(ClsName, 2, Length(ClsName)-1) + 'ID'
    else
      ConventionName:=ClsName + 'ID';
    F:=FFieldMap.FindByPropertyName(ConventionName);
    if F<>nil then
    begin
      F.Options:=F.Options + [foInKey];
      Exit;
    end;
  end;
  // 4. Validate: no PK found
  if Assigned(FInstanceClass) then
  begin
    ClsName:=FInstanceClass.ClassName;
    if (Length(ClsName)>1) and (UpCase(ClsName[1])='T') then
      ClsName:=Copy(ClsName, 2, Length(ClsName)-1);
    RaiseError('ResolvePrimaryKey', SErrNoPrimaryKey, [FInstanceClass.ClassName, ClsName]);
  end
  else
    RaiseError('ResolvePrimaryKey', SErrNoPrimaryKey, ['(unknown)', '']);
end;

function TPersisterDef.GetOptions: TStrings;
begin
  if FOptions=Nil then
    FOptions:=TStringList.Create;
  Result:=FOptions;
end;

function TPersisterDef.GetTableName: UTF8String;
begin
  Result:=FTableName;
  If (Result='') then
    begin
    if PersisterClass<>Nil then
      Result:=PersisterClass.GetTableName(InstanceClass);
    if (Result='') then
      Result:=TOPPersister.GetTableName(InstanceClass);
    end;
end;

procedure TPersisterDef.SetOptions(AValue: TStrings);
begin
  Options.Assign(aValue);
end;

procedure TPersisterDef.SetFieldMap(AValue: TOPFieldMapList);
begin
  if FFieldMap=AValue then Exit;
  FreeAndNil(FFieldMap);
  FFieldMap:=AValue;
end;

constructor TPersisterDef.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FOptions:=TStringlist.Create;
end;

destructor TPersisterDef.Destroy;
begin
  // When the application registered RttiFactory of this definition, the
  // factory holds a method of it: remove it, or it would call into freed
  // memory (only when it still is ours)
  if Assigned(FConstructorCode) and TOPClassFactory.HasInstance then
    OPFactory.UnregisterFactory(FInstanceClass, Self);
  FreeAndNil(FFieldMap);
  FreeAndNil(Foptions);
  inherited Destroy;
end;

procedure TPersisterDef.Assign(aSource: TPersistent);

var
  D: TPersisterDef absolute aSource;

begin
  if (aSource is TPersisterDef) then
    begin
    FCacheSQL:=D.CacheSQL;
    if D.HasOptions then
      Options:=D.Options
    else
      FreeAndNil(Foptions);
    FInstanceClass:=D.InstanceClass;
    FPersisterClass:=D.PersisterClass;
    FDeleteSQL:=D.DeleteSQL;
    FInsertSQL:=D.InsertSQL;
    FInstanceSQL:=D.InstanceSQL;
    FListSQL:=D.ListSQL;
    FUpdateSQL:=D.UpdateSQL;
    FTableName:=D.FTableName;
    FHasIOPPrimaryKey:=D.FHasIOPPrimaryKey;
    FAutoCreated:=D.FAutoCreated;
    if Assigned(D.FFieldMap) then
      begin
      if FFieldMap=nil then
        FFieldMap:=TOPFieldMapList.Create(TOPFieldMap);
      FFieldMap.DefaultFieldOptions:=D.FFieldMap.DefaultFieldOptions;
      FFieldMap.Assign(D.FFieldMap);
      end
    else
      FreeAndNil(FFieldMap);
    end
  else
    inherited Assign(aSource);
end;

function TPersisterDef.HasOptions: Boolean;
begin
  Result:=Assigned(Foptions);
end;

end.

