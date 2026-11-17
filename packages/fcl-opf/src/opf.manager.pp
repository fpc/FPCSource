{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    The OPF manager: use this to load/save objects from/to database

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Manager;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Variants, Data.Db, Data.Sqldb,
{$ELSE}
  Classes, SysUtils, Variants, db, sqldb,
{$ENDIF}
  Opf.Types, Opf.Connections, Opf.Registry, Opf.Fields, Opf.Sql, Opf.Relations, Opf.M2M,
  Opf.Transaction, Opf.DDL, Opf.Migration, Opf.Schema, Opf.Criteria,
  Opf.ChangeTracking, Opf.Proxy, Opf.ListReg;

Type

  // import some definitions
  TOPPersister = Opf.Registry.TOPPersister;
  TOPPersisterClass = Opf.Registry.TOPPersisterClass;
  TOPRegistry = Opf.Registry.TOPRegistry;
  TOPSQLBuilder = Opf.Sql.TOPSQLBuilder;

  TMigrateOption = Opf.Migration.TMigrateOption;
  TMigrateOptions = Opf.Migration.TMigrateOptions;
  TOPMigrationStep = Opf.Migration.TOPMigrationStep;
  TOPMigrationStepDef = Opf.Migration.TOPMigrationStepDef;
  TOPMigrationStepArray = Opf.Migration.TOPMigrationStepArray;
  TOPMigrationResult = Opf.Migration.TOPMigrationResult;
  TGenerateSQLOption = Opf.Migration.TGenerateSQLOption;
  TGenerateSQLOptions = Opf.Migration.TGenerateSQLOptions;

  TOPManager = Class;
  TOPManagerClass = Class of TOPManager;

  TOPUserIDKind = (ukNone, ukNumerical, ukString, ukObject);
  TOPTimestampKind = (tkLocal, tkUTC);
  TOPVariantArray = array of Variant;

  { Checks made by Delete:
    dckVersion: a versioned object is only deleted when the version in the
      database is still the one it was loaded with. Otherwise nothing is
      deleted, no cascade either: with dckRowsAffected Delete raises
      SErrConcurrencyConflict, without it the delete does nothing silently.
    dckRowsAffected: deleting a row that is not there (any more) raises:
      SErrConcurrencyConflict for a versioned object, SErrDeleteNotFound
      otherwise. Without it, such a delete succeeds silently. }
  TOPDeleteCheck = (dckVersion, dckRowsAffected);
  TOPDeleteChecks = set of TOPDeleteCheck;

  { Manager options:
    omoRawCriteriaSoftDeleteFilter: Load(aList, aClass, 'criteria text') adds
      the soft-delete filter to the text. Without it the text is used as-is,
      so it can select soft-deleted rows.
    omoHookSQLDBLog: the log messages of the database connections are sent
      to the manager's log.
    omoResetConnectionOnRollbackError: after a failed rollback, close and
      reopen the connection so that no transaction stays open on the server
      (see TTransactionHandler). Closing also closes the datasets open on
      that connection. }
  TOPFManagerOption = (omoRawCriteriaSoftDeleteFilter, omoHookSQLDBLog, omoResetConnectionOnRollbackError);
  TOPFManagerOptions = set of TOPFManagerOption;

Const
  DefaultManagerOptions = [omoRawCriteriaSoftDeleteFilter, omoResetConnectionOnRollbackError];

Type

  { TOPAuditInfo - consolidates all audit configuration }

  TOPAuditInfo = class(TPersistent)
  Private
    FNumericalID: Int64;
    FStringID: String;
    FObjectKey: String;
    FObjectClassName: String;
    FUserIDKind: TOPUserIDKind;
    FTimestampKind: TOPTimestampKind;
    // TOPManager whose registry finds the key of ObjectID; Nil: the global registry
    FManager: TObject;
    procedure SetNumericalID(AValue: Int64);
    procedure SetStringID(AValue: String);
    procedure SetObjectID(AValue: TObject);
  Public
    // Copies the audit settings from another TOPAuditInfo
    Procedure Assign(Source: TPersistent); override;
    { The object identifying the user. Its primary key is read when it is
      assigned (IOPPrimaryKey, else the registered persister's key field) and
      only the key is kept, so the object may be freed afterwards; a key it
      gets later is not seen. Raises EOPF when the key cannot be found.
      Nil clears it. }
    Property ObjectID: TObject Write SetObjectID;
    // The primary key of the object assigned to ObjectID, as text
    Property ObjectKey: String Read FObjectKey;
    // The class name of the object assigned to ObjectID
    Property ObjectClassName: String Read FObjectClassName;
  Published
    // Whether audit timestamps are stored in local time or in UTC
    Property TimestampKind: TOPTimestampKind Read FTimestampKind Write FTimestampKind default tkLocal;
    // Which kind of user identification is stored in the audit fields
    Property UserIDKind: TOPUserIDKind Read FUserIDKind Write FUserIDKind default ukNone;
    // The number identifying the user
    Property NumericalID: Int64 Read FNumericalID Write SetNumericalID default 0;
    // The text identifying the user
    Property StringID: String Read FStringID Write SetStringID;
  end;

  { IOPConnectionResolver
    Optional interface for custom connection routing.

    Implement this to support per-class or per-operation connection selection
    instead of the default single-connection model. }
  IOPConnectionResolver = Interface ['{F2A8C5D1-7E3B-4F9A-B6D2-1A4E8C3F5B70}']
    // Returns the database connection to use for objects of aClass
    Function ResolveConnection(aClass: TClass): TSQLConnection;
  end;

  TObjectPersistenceState = (opsClean,opsKey,opsNew,opsUpdate,opsDelete,opsDeleted);

  { Event types for entity lifecycle hooks }
  TOPSaveEvent = procedure(Sender: TObject; aObject: TObject; aState: TObjectPersistenceState) of object;
  TOPObjectEvent = procedure(Sender: TObject; aObject: TObject) of object;

  IOPPersistenceState = Interface ['{84DE4D96-0D52-0DC1-3129-1C576A55493A}']
    // Returns the persistence state of the object
    Function GetPersistenceState :  TObjectPersistenceState;
    // Sets the persistence state of the object
    Procedure SetPersistenceState(aValue :  TObjectPersistenceState);
    // Whether the object is new, changed, to be deleted, unchanged, ...
    Property PersistenceState :  TObjectPersistenceState Read GetPersistenceState Write SetPersistenceState;
  end;

  IOPPrimaryKey = Interface  ['{937F69B6-B4B0-4A95-AFA6-005BD80573FD}']
    // Returns the primary key values of AInstance as text
    Function GetPrimaryKey(AInstance : TObject) : TStringArray;
    // Sets the primary key values of AInstance from text
    Procedure SetPrimaryKey(AInstance : TObject; aValue : TStringArray);
    // True when the object already has a primary key value
    Function HasPrimaryKeyValue : Boolean;
  end;

  { TOPConnection }

  TOPConnection = Class(TSQLDBConnectionDef)
  private
    FDefault: Boolean;
    procedure SetDefault(AValue: Boolean);
  Published
    // True when this is the connection used by default
    Property IsDefault : Boolean Read FDefault Write SetDefault;
  end;

  { TOPConnectionList }

  TOPConnectionList = Class(TSQLDBConnectionList)
  private
    function GetConnection(aIndex : Integer): TOPConnection; inline;
    procedure SetConnection(aIndex : Integer; AValue: TOPConnection); inline;
  Public
    // The connection definitions in the list, by position
    Property Connections [aIndex : Integer] : TOPConnection Read GetConnection Write SetConnection; default;
  end;


    { TOPManager }

  TOPManager = Class (TOPBaseComponent, IOPChangeTrackerHost)
  Private
    Class var
      _Instance : TOPManager;
      _InstanceClass : TOPManagerClass;
  Private
    FConnections: TOPConnectionList;
    FConnectionResolver: IOPConnectionResolver;
    FRegistry: TOPRegistry;
    FSQLBuilder : TOPSQLBuilder;
    FM2MRegistry: TOPManyToManyRegistry;
    FRelationRegistry: TOPRelationRegistry;
    FSchemaMigrator: TOPSchemaMigrator;
    FOwnsSchemaMigrator: Boolean;
    {$IFNDEF VER3_2}
    FLazyBindings: TObject; // TOPPointerMap, set of TOPLazyBinding
    {$ENDIF}
    FTransactionHandler: TTransactionHandler;
    FActiveTransactionPolicy: TOPActiveTransactionPolicy;
    FDeleteChecks: TOPDeleteChecks;
    FOptions: TOPFManagerOptions;
    FOnPartialCommit: TOPPartialCommitEvent;
    // TOPUndoLog: in-memory object state to restore when the transaction rolls back
    FUndoLog: TObject;
    // Exception already logged by RollbackAfterError while unwinding nested operations
    FLoggedError: TObject;
    FBeforeSave: TOPSaveEvent;
    FAfterSave: TOPSaveEvent;
    FAfterLoad: TOPObjectEvent;
    FBeforeDelete: TOPObjectEvent;
    FAfterDelete: TOPObjectEvent;
    FAuditInfo: TOPAuditInfo;
    FChangeTracker: IOPChangeTracker;
    FChangeTrackerOwner: TComponent;
    // A persister of aDef's persister class; raises a clear error when there is none
    function CreatePersister(aDef: TPersisterDef; const aOperation: string): TOPPersister;
    procedure RecordUndo(aObject: TObject; aFieldMap: TOPFieldMapList; aConn: TSQLConnection);
    procedure TouchObject(aObject: TObject);
    procedure FinishUndo(aRestore: Boolean); overload;
    // aCommitted: objects written through these connections are not restored
    procedure FinishUndo(aRestore: Boolean; const aCommitted: array of TSQLConnection); overload;
    procedure BeginUndoOperation;
    procedure EndUndoOperation(aFailed: Boolean);
    procedure DoSave(aObject: TObject; aState: TObjectPersistenceState; aPersister: TOPPersister);
    procedure CaptureObjectSnapshot(aObject: TObject; aFieldMap: TOPFieldMapList);
    procedure DiscardObjectSnapshot(aObject: TObject);
    // Drops every reference state the tracker holds.
    procedure PurgeAllSnapshots;
    // Rollback from an except block: logs the failure of aOperation once
    // and never replaces the exception being handled.
    procedure RollbackAfterError(const aOperation: string);
    // From an except block: log the failure of aOperation once
    procedure LogOperationFailure(const aOperation: string);
    // Raise an error after the operation's transaction level ended, logged
    // like a failure that rolls back
    procedure RaiseOperationError(const aOperation, aFmt: string; const aArgs: array of const);
    // False when aObject is not tracked; otherwise aNames holds the changed properties.
    function GetObjectChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
      out aNames: TUTF8StringArray): Boolean;
    procedure SetAuditInfo(AValue: TOPAuditInfo);
    function GetDefaultConnection: TSQLConnection;
    // Connection for objects of aClass: the resolver decides, otherwise the
    // default connection. Delete cascades use the deleted object's connection.
    // Junction rows of a many-to-many definition are always read and written
    // through the connection of its ClassA (JunctionConnection), whichever
    // side is loaded, linked or deleted. Loading associations joins the
    // junction table with the other class's table, so both classes of a
    // many-to-many definition must be stored in that database.
    function ConnectionFor(aClass: TClass): TSQLConnection;
    function JunctionConnection(aDef: TOPManyToManyDef): TSQLConnection;
    procedure RegisterConnectionForTransaction(aConn: TSQLConnection);
    procedure SetOptions(AValue: TOPFManagerOptions);
    procedure HookSQLDBLog;
    procedure UnhookSQLDBLog;
    procedure DoSQLDBLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
    function GetMigrationTableName: UTF8String;
    procedure SetMigrationTableName(const AValue: UTF8String);
    function GetSchemaMigrator: TOPSchemaMigrator;
    procedure SetSchemaMigrator(AValue: TOPSchemaMigrator);
    procedure EnsureMigratorRegistries;
    function GetM2MRegistry: TOPManyToManyRegistry;
    function GetRegistry: TOPRegistry;
    function GetRelationRegistry: TOPRelationRegistry;
    function GetSQLBuilder: TOPSQLBuilder;
    procedure SetM2MRegistry(AValue: TOPManyToManyRegistry);
    procedure SetRelationRegistry(AValue: TOPRelationRegistry);
    procedure DeleteCascadeChildren(aParentClass: TClass;
      const aFKWhereExpr: UTF8String; aParamValue: Variant;
      IsSubQuery: Boolean; aConn: TSQLConnection; aVisited: TFPList;
      aSoft: Boolean; aVisitedKeys: TStrings);
    procedure SetSoftDeleteParam(aParam: TParam; aField: TOPFieldMap; aTimestamp: TDateTime);
    procedure ExecCascadeSQL(const aSQL: UTF8String; aParamValue: Variant;
      aSoftDeleteField: TOPFieldMap; aConn: TSQLConnection; aChildClass, aParentClass: TClass);
    function NeedsKeyCascade(aClass: TClass; aSoft: Boolean): Boolean;
    function RowWithVersionExists(aDef: TPersisterDef; aObject: TObject;
      aPersister: TOPPersister; aConn: TSQLConnection; aVersionField: TOPFieldMap): Boolean;
    function SelectCascadeKeys(const aSQL: UTF8String; aParamValue: Variant;
      aConn: TSQLConnection): TOPVariantArray;
    function ResolveChildListPropertyName(aParentClass: TClass; aRel: TOPRelationDef): UTF8String;
    procedure CheckLoadChildren(aParent: TObject; aRel: TOPRelationDef; aList: TObject;
      out aChildDef, aParentDef: TPersisterDef; out aPKField: TOPFieldMap;
      out aManipulator: TOPListManipulator);
    procedure DoLoadChildren(aParent: TObject; aRel: TOPRelationDef; aList: TObject);
    function ResolveM2MListPropertyName(aDef: TOPManyToManyDef; aAssociatedClass: TClass): UTF8String;
    function IsM2MListProperty(aClass: TClass; const aPropertyName: UTF8String): Boolean;
    function JunctionKeyValue(aObject: TObject; const aOperation: string): Variant;
    procedure DoLoadAssociations(aParent: TObject; aDef: TOPManyToManyDef; aAssociatedClass: TClass; aList: TObject);
    procedure DoAddAssociation(aObjectA, aObjectB: TObject; aDef: TOPManyToManyDef);
    procedure DoRemoveAssociation(aObjectA, aObjectB: TObject; aDef: TOPManyToManyDef);
    procedure DeleteM2MJunctionRows(aParentClass: TClass; aPKValue: Variant);
    procedure InitLazyFields(aObject: TObject);
    // Every loaded object: state opsClean, AfterLoad, lazy loaders, change tracking snapshot
    procedure FinishLoadedObject(aObject: TObject; aFieldMap: TOPFieldMapList);
    procedure FinishLoadedList(aList: TObject; aManipulator: TOPListManipulator; aFieldMap: TOPFieldMapList);
    {$IFNDEF VER3_2}
    // Keep a TOPLazyBinding; it is freed with its entity when that reports being freed
    procedure AddLazyBinding(aBinding: TObject);
    {$ENDIF}
    class function ArgsToParams(const aArgs: array of const): TParams;
    procedure SetConnections(AValue: TOPConnectionList);
    procedure SetRegistry(AValue: TOPRegistry);
    procedure SetSQLBuilder(AValue: TOPSQLBuilder);
  Protected
    Function CreateConnections : TOPConnectionList; virtual;
    // Patterns that are not allowed in raw SQL criteria. Override to adapt per dialect.
    Function GetUnsafeCriteriaPatterns : TUTF8StringArray; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Entity lifecycle hooks - override in descendants, default calls published event
    Procedure DoBeforeSave(aObject: TObject; aState: TObjectPersistenceState); virtual;
    Procedure DoAfterSave(aObject: TObject; aState: TObjectPersistenceState); virtual;
    Procedure DoAfterLoad(aObject: TObject); virtual;
    Procedure DoBeforeDelete(aObject: TObject); virtual;
    Procedure DoAfterDelete(aObject: TObject); virtual;
    // List save support
    Procedure DoSaveList(aList: TObject; aManipulator: TOPListManipulator); virtual;
    // Audit field support
    Function GetTimestamp: TDateTime; virtual;
    Procedure ApplyAuditFields(aObject: TObject; aState: TObjectPersistenceState; aFieldMap: TOPFieldMapList); virtual;
    // Soft delete support
    Function GetSoftDeleteFilter(aFieldMap: TOPFieldMapList; const aTablePrefix: UTF8String = ''): UTF8String; virtual;
    // Join the non-empty conditions with AND, each one parenthesized
    Function CombineWhere(const aParts: array of UTF8String): UTF8String;
    // SQL generation for the load operations
    Function BuildKeySelectSQL(aDef: TPersisterDef): UTF8String; virtual; overload;
    // SELECT of the columns of aSelectMap for the row with the key of aDef
    Function BuildKeySelectSQL(aDef: TPersisterDef; aSelectMap: TOPFieldMapList): UTF8String; virtual; overload;
    Function BuildCriteriaSelectSQL(aDef: TPersisterDef; aCriteria: TOPCriteria): UTF8String; virtual;
    // Children of a parent; the parent key is parameter :opf_parentkey
    Function BuildChildrenSQL(aChildDef: TPersisterDef; const aForeignKeyField: UTF8String): UTF8String; virtual;
    // Associated objects through a junction table; the parent key is parameter :opf_parentkey
    Function BuildAssociationsSQL(aAssocDef: TPersisterDef; const aJunctionTable, aFKParent, aFKAssociated: UTF8String): UTF8String; virtual;
    // Params with a single parameter opf_parentkey holding the key value of aParent
    Function CreateParentKeyParams(aParent: TObject; aParentDef: TPersisterDef; aPKField: TOPFieldMap): TParams;
  Public
    // Creates the manager with an empty connection list and its own schema migrator
    Constructor Create(aOwner : TComponent); override;
    // Frees the manager and everything it owns
    Destructor Destroy; override; overload;
    // Frees the global manager instance when the program ends
    Class Destructor Cleanup;
    // Sets the class used to create the global manager instance
    Class Procedure SetInstanceClass (aClass : TOPManagerClass);
    // Returns the global manager instance, creating it when needed
    Class Function Instance : TOPManager;
    // Frees the global manager instance
    Class Procedure ClearInstance;
    // Returns the exception class raised by the manager
    class function GetExceptionClass: EOPFClass; override;
    // Explicit transaction boundaries
    // Starts a transaction; calls can be nested
    Procedure BeginTransaction; virtual;
    // Ends the current transaction and stores its changes in the database
    Procedure Commit; virtual;
    // Ends the current transaction and discards its changes
    Procedure Rollback; virtual;
    // True when a transaction started with BeginTransaction is active
    Function InTransaction: Boolean; virtual;
    // Load/Save/Delete
    // Loads the object with the key it holds from the database; KeyOnly reads only the key.
    // Raises SErrLoadNotFound when there is no such row. That error does not
    // doom a transaction started with BeginTransaction.
    Procedure Load(aObject : TObject; KeyOnly : Boolean = False; aPersister : TOPPersister = Nil); virtual;
    // As Load, but returns False instead of raising when there is no such row
    // (the object is then left unchanged). With KeyOnly an existence check.
    Function TryLoad(aObject : TObject; KeyOnly : Boolean = False; aPersister : TOPPersister = Nil) : Boolean; virtual;
    // Loads all objects of aClass matching a WHERE condition given as text into aList.
    // Soft-deleted rows are excluded as with the TOPCriteria overload, unless
    // omoRawCriteriaSoftDeleteFilter is not in Options: then a non-empty aCriteria is used
    // as-is and soft-deleted rows are loaded too.
    Procedure Load(aList: TObject; aClass: TClass; const aCriteria: UTF8String = ''); virtual;
    // Loads all objects of aClass matching aCriteria into aList
    Procedure Load(aList: TObject; aClass: TClass; aCriteria: TOPCriteria); virtual;
    // Number of automatic lazy loaders set up by this manager whose entity
    // still exists (diagnostics; always 0 without extended RTTI)
    Function LazyBindingCount: Integer;
    // Saves an object, or every object of a list, according to its persistence state
    Procedure Save(aObject : TObject); virtual;
    // Saves an object as if it had persistence state aState
    Procedure Save(aObject : TObject; aState : TObjectPersistenceState); virtual;
    // Saves an object as if it had persistence state aState, using aPersister
    Procedure Save(aObject : TObject; aState : TObjectPersistenceState; aPersister : TOPPersister); virtual;
    // Deletes the object from the database
    Procedure Delete(aObject : TObject); virtual;
    // LoadSQL - custom SQL with ORM result mapping
    // Loads an object from the result of an SQL statement with aParams
    Procedure LoadSQL(aObject: TObject; const aSQL: UTF8String; aParams: TParams); virtual;
    // Loads an object from the result of an SQL statement, with parameter values in aArgs
    Procedure LoadSQL(aObject: TObject; const aSQL: UTF8String; const aArgs: array of const); virtual;
    // Loads an object from the result of an SQL statement without parameters
    Procedure LoadSQL(aObject: TObject; const aSQL: UTF8String); virtual;
    // Loads objects of aClass into aList from the result of an SQL statement with aParams
    Procedure LoadSQL(aList: TObject; aClass: TClass; const aSQL: UTF8String; aParams: TParams); virtual;
    // Loads objects of aClass into aList from an SQL statement, with parameter values in aArgs
    Procedure LoadSQL(aList: TObject; aClass: TClass; const aSQL: UTF8String; const aArgs: array of const); virtual;
    // Loads objects of aClass into aList from the result of an SQL statement without parameters
    Procedure LoadSQL(aList: TObject; aClass: TClass; const aSQL: UTF8String); virtual;
    // LoadChildren
    // Loads the children of aParent into its list property aPropertyName.
    // With one child relation that has no ListPropertyName, any list property
    // is accepted, except the list property of a many-to-many definition.
    Procedure LoadChildren(aParent: TObject; const aPropertyName: UTF8String); virtual; overload;
    // Loads the children of class aChildClass into the list property aPropertyName of aParent
    Procedure LoadChildren(aParent: TObject; aChildClass: TClass; const aPropertyName: UTF8String); virtual; overload;
    // Loads the children of class aChildClass of aParent into aList
    Procedure LoadChildren(aParent: TObject; aChildClass: TClass; aList: TObject); virtual; overload;
    // Loads the children of aParent into aList; aParent must have exactly one child relation
    Procedure LoadChildren(aParent: TObject; aList: TObject); virtual; overload;
    // Loads the children of every child relation of aParent into their list properties
    Procedure LoadAllChildren(aParent: TObject); virtual;
    // LoadAssociations - many-to-many
    // Loads the many-to-many associated objects of aParent into its list property aPropertyName
    Procedure LoadAssociations(aParent: TObject; const aPropertyName: UTF8String); virtual;
    // Loads the associated objects of class aAssociatedClass into list property aPropertyName
    Procedure LoadAssociations(aParent: TObject; aAssociatedClass: TClass; const aPropertyName: UTF8String); virtual;
    // Loads the associated objects of class aAssociatedClass of aParent into aList
    Procedure LoadAssociations(aParent: TObject; aAssociatedClass: TClass; aList: TObject); virtual;
    // AddAssociation / RemoveAssociation - many-to-many junction management
    // Stores a many-to-many link between two objects in the database. Raises
    // SErrM2MDuplicateAssociation when the link exists already; that error
    // does not doom a transaction started with BeginTransaction.
    Procedure AddAssociation(aObjectA, aObjectB: TObject); virtual;
    // Removes the many-to-many link between two objects from the database
    Procedure RemoveAssociation(aObjectA, aObjectB: TObject); virtual;
    // Create complete database schema from registered metadata
    Procedure CreateSchema(aConnection: TSQLConnection; aCreateOptions: TSQLCreateOptions = [];
      aSchemaOptions: TSchemaCreateOptions = [scoForeignKeys, scoIndexes, scoJunctionTables]); virtual;
    // Register a user-defined migration step for a specific version transition
    Procedure RegisterMigration(aFromVersion, aToVersion: Integer;
      aStep: TOPMigrationStep; const aDescription: UTF8String = ''); virtual;
    // Migrate existing database schema to match current registered ORM model
    Function MigrateSchema(aConnection: TSQLConnection; aTargetVersion: Integer;
      aOptions: TMigrateOptions = []): TOPMigrationResult; virtual;
    // Generate migration SQL script without executing. Use for external migration tools.
    Function GenerateMigrationSQL(aConnection: TSQLConnection; aTargetVersion: Integer;
      aOptions: TMigrateOptions = [];
      aGenerateOptions: TGenerateSQLOptions = [gsoIncludeVersionTracking]): TUTF8StringArray; virtual;
    // Use aProvider as the change tracker from now on.
    Procedure AttachChangeTracker(aProvider: TComponent);
    // Stop using the current change tracker.
    Procedure DetachChangeTracker;
    // Appends the changed property names to aFields. False when aObject is not tracked.
    Function GetDirtyFields(aObject: TObject; aFields: TStrings): Boolean; overload;
    // Returns the changed property names in aFields. False when aObject is not tracked.
    Function GetDirtyFields(aObject: TObject; out aFields: TUTF8StringArray): Boolean; overload;
    // The change tracker currently attached, Nil when none is attached.
    Property ChangeTracker: IOPChangeTracker Read FChangeTracker;
    // Chooses the database connection per class; Nil uses the default connection
    Property ConnectionResolver : IOPConnectionResolver Read FConnectionResolver Write FConnectionResolver;
    // The many-to-many definitions used; Nil uses the global one
    Property M2MRegistry: TOPManyToManyRegistry Read GetM2MRegistry Write SetM2MRegistry;
    // The parent-child relation definitions used; Nil uses the global one
    Property RelationRegistry: TOPRelationRegistry Read GetRelationRegistry Write SetRelationRegistry;
  Published
    // The database connections known to the manager
    Property Connections : TOPConnectionList Read FConnections Write SetConnections;
    // The class-to-table mapping definitions used; Nil uses the global one
    Property Registry : TOPRegistry Read GetRegistry Write SetRegistry;
    // Builds the text of the SQL statements; Nil uses the global one
    Property SQLBuilder : TOPSQLBuilder Read GetSQLBuilder Write SetSQLBuilder;
    // Manager behaviour switches, see TOPFManagerOption
    Property Options: TOPFManagerOptions Read FOptions Write SetOptions default DefaultManagerOptions;
    // Called before an object is saved
    Property BeforeSave: TOPSaveEvent Read FBeforeSave Write FBeforeSave;
    // Called after an object is saved
    Property AfterSave: TOPSaveEvent Read FAfterSave Write FAfterSave;
    // Called after an object is loaded
    Property AfterLoad: TOPObjectEvent Read FAfterLoad Write FAfterLoad;
    // Called before an object is deleted
    Property BeforeDelete: TOPObjectEvent Read FBeforeDelete Write FBeforeDelete;
    // Called after an object is deleted
    Property AfterDelete: TOPObjectEvent Read FAfterDelete Write FAfterDelete;
    // How the created/changed by and at fields are filled in
    Property AuditInfo: TOPAuditInfo Read FAuditInfo Write SetAuditInfo;
    // Name of the database table that records the schema version
    Property MigrationTableName: UTF8String Read GetMigrationTableName Write SetMigrationTableName;
    // Updates the database structure; Nil restores the manager's own one
    Property SchemaMigrator: TOPSchemaMigrator Read GetSchemaMigrator Write SetSchemaMigrator;
    // What to do when a connection's own transaction is already active when the manager needs it.
    Property ActiveTransactionPolicy: TOPActiveTransactionPolicy Read FActiveTransactionPolicy Write FActiveTransactionPolicy default atpRaise;
    // Checks made by Delete; [] deletes without checking version or row count
    Property DeleteChecks: TOPDeleteChecks Read FDeleteChecks Write FDeleteChecks default [dckVersion, dckRowsAffected];
    // Called when a commit fails after other connections were committed,
    // before EOPFPartialCommit is raised (there is no two-phase commit)
    Property OnPartialCommit: TOPPartialCommitEvent Read FOnPartialCommit Write FOnPartialCommit;
  end;

// Returns the global manager instance
Function OPManager : TOPManager;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.TypInfo, System.DateUtils,
{$ELSE}
  typinfo, DateUtils,
{$ENDIF}
  Opf.Consts, Opf.Executor, Opf.Logging, Opf.Driver, Opf.FieldValue
{$IFNDEF VER3_2}
  , {$IFDEF FPC_DOTTEDUNITS}System.Rtti{$ELSE}Rtti{$ENDIF}
{$ENDIF}
  ;

resourcestring
  SErrNoPersisterClassForClass = 'The persister definition of class "%s" has no persister class';
  SErrLazyManagerFreed = 'Cannot lazy load for an object of class "%s": the manager that loaded it was freed';
  SErrM2MAddFailed = 'Cannot add the association between %s (PK=%s) and %s (PK=%s) in junction table "%s": %s';
  SErrM2MListIsAssociation = 'List property "%s" of class "%s" holds many-to-many associations, not children';

type
  { TOPVersionValue: the value of a version property, read from the object. }

  TOPVersionValue = record
    Kind: TTypeKind;
    IntValue: Int64;
    StrValue: UTF8String;
  end;

  { TOPPointerMap: hash map from pointer to pointer (open addressing).
    Used as a set (values Nil) where TFPList.IndexOf/Remove would make the
    bookkeeping of large loads quadratic. }

  TOPPointerMap = class
  private
    FKeys: array of Pointer;
    FValues: array of Pointer;
    FCount: Integer;
    function HomeSlot(aKey: Pointer): Integer;
    function FindSlot(aKey: Pointer): Integer;
    procedure Grow;
  public
    function Contains(aKey: Pointer): Boolean;
    function Find(aKey: Pointer; out aValue: Pointer): Boolean;
    procedure Put(aKey, aValue: Pointer);
    function Remove(aKey: Pointer): Boolean;
    procedure Clear;
    // Iteration: slots 0..Capacity-1, empty slots have key Nil
    function Capacity: Integer;
    function KeyAt(aSlot: Integer): Pointer;
    property Count: Integer read FCount;
  end;

  { One field value saved before the manager changed it }

  TOPUndoField = record
    Field: TOPFieldMap;
    Value: Variant;
    WasNull: Boolean;
  end;

  { State of an object before a save or delete in a transaction.
    Obj is Nil once the object was freed (the entry is then skipped). }

  TOPUndoEntry = class
    Obj: TObject;
    Conn: TSQLConnection;          // Connection the object was written through
    PrevSame: TOPUndoEntry;        // Previous entry of the same object
    HasState: Boolean;
    OldState: TObjectPersistenceState;
    Fields: array of TOPUndoField;
  end;

  { TOPUndoLog:
    The database rolls back, but objects keep what the manager wrote into them:
    a new version, a generated key, audit values, the persistence state.
    The log records these before each save or delete inside a transaction.

    - When an operation (Save, Delete, Save of a list) fails, the objects it
      changed are restored at once: they are certainly still alive, and the
      transaction is doomed, so the changes will not reach the database.
    - When an operation succeeds, the caller may free its objects. Entries are
      only kept for objects that report being freed (IFPObserved, e.g. any
      TPersistent); they are restored when the outermost transaction rolls
      back and discarded when it commits.

    The log also remembers which objects had their change tracking snapshot
    taken or discarded in the transaction, so only those snapshots are dropped
    on rollback. The tracker normally keeps snapshots of observable objects
    only. When it also keeps them for others (TrackUnobserved), and such an
    object was touched and the transaction wrote anything through the
    manager, all snapshots are purged instead (a transaction without writes
    leaves the database as it was, so its snapshots stay valid).

    After a partial commit (several connections, EOPFPartialCommit) only the
    objects written through a connection that did not commit are restored. }

  TOPUndoLog = class(TObject, IFPObserver)
  private
    FEntries: TFPList;
    FLastEntry: TOPPointerMap;  // Observed object -> its newest entry
    FTouched: TOPPointerMap;    // Set of touched (observed) objects
    FObserved: TOPPointerMap;   // Set of objects this log observes
    FDepth: Integer;        // Nesting of operations
    FMark: Integer;         // First entry of the outermost running operation
    FLostTouched: Boolean;  // An unobservable object had its snapshot changed
    FHasWrites: Boolean;    // A save or delete was recorded
    function Observe(aObject: TObject): Boolean;
    procedure RestoreEntry(aEntry: TOPUndoEntry);
    procedure DropUnobserved;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    procedure BeginOperation;
    procedure EndOperation(aFailed: Boolean);
    procedure RecordObject(aObject: TObject; aFieldMap: TOPFieldMapList; aConn: TSQLConnection);
    // aTracksUnobserved: the change tracker keeps states of unobservable objects
    procedure Touch(aObject: TObject; aTracksUnobserved: Boolean);
    // Restore all entries, except those written through a connection in
    // aCommitted, discard the snapshots of touched objects, then clear
    procedure Restore(aTracker: IOPChangeTracker; const aCommitted: array of TSQLConnection);
    procedure Clear;
  end;

{ TOPPointerMap }

function TOPPointerMap.HomeSlot(aKey: Pointer): Integer;
begin
  {$push}{$R-}{$Q-}
  Result:=Integer(((PtrUInt(aKey) shr 3)*PtrUInt(2654435761)) and PtrUInt(Length(FKeys)-1));
  {$pop}
end;

{ Slot holding aKey, or the empty slot where it would go; -1 when there are no slots }

function TOPPointerMap.FindSlot(aKey: Pointer): Integer;
var
  Mask: Integer;
begin
  if Length(FKeys)=0 then
    Exit(-1);
  Mask:=Length(FKeys)-1;
  Result:=HomeSlot(aKey);
  while (FKeys[Result]<>Nil) and (FKeys[Result]<>aKey) do
    Result:=(Result+1) and Mask;
end;

procedure TOPPointerMap.Grow;
var
  OldKeys, OldValues: array of Pointer;
  I, Len, S: Integer;
begin
  OldKeys:=FKeys;
  OldValues:=FValues;
  Len:=Length(OldKeys)*2;
  if Len<16 then
    Len:=16;
  FKeys:=Nil;
  FValues:=Nil;
  SetLength(FKeys, Len);
  SetLength(FValues, Len);
  for I:=0 to Length(OldKeys)-1 do
    if OldKeys[I]<>Nil then
    begin
      S:=FindSlot(OldKeys[I]);
      FKeys[S]:=OldKeys[I];
      FValues[S]:=OldValues[I];
    end;
end;

function TOPPointerMap.Contains(aKey: Pointer): Boolean;
var
  S: Integer;
begin
  S:=FindSlot(aKey);
  Result:=(S>=0) and (FKeys[S]<>Nil);
end;

function TOPPointerMap.Find(aKey: Pointer; out aValue: Pointer): Boolean;
var
  S: Integer;
begin
  aValue:=Nil;
  S:=FindSlot(aKey);
  Result:=(S>=0) and (FKeys[S]<>Nil);
  if Result then
    aValue:=FValues[S];
end;

procedure TOPPointerMap.Put(aKey, aValue: Pointer);
var
  S: Integer;
begin
  if aKey=Nil then
    Exit;
  // Keep the table at most half full
  if (FCount+1)*2>Length(FKeys) then
    Grow;
  S:=FindSlot(aKey);
  if FKeys[S]=Nil then
  begin
    FKeys[S]:=aKey;
    Inc(FCount);
  end;
  FValues[S]:=aValue;
end;

{ Linear probing: the entries after the removed one are shifted back, so no
  lookup chain is broken. }

function TOPPointerMap.Remove(aKey: Pointer): Boolean;
var
  I, J, K, Mask: Integer;
begin
  I:=FindSlot(aKey);
  Result:=(I>=0) and (FKeys[I]<>Nil);
  if not Result then
    Exit;
  Mask:=Length(FKeys)-1;
  FKeys[I]:=Nil;
  FValues[I]:=Nil;
  Dec(FCount);
  J:=I;
  repeat
    J:=(J+1) and Mask;
    if FKeys[J]=Nil then
      Break;
    K:=HomeSlot(FKeys[J]);
    // The entry at J may move to I when I lies cyclically in [K, J)
    if ((I<=J) and ((K<=I) or (K>J))) or ((I>J) and (K<=I) and (K>J)) then
    begin
      FKeys[I]:=FKeys[J];
      FValues[I]:=FValues[J];
      FKeys[J]:=Nil;
      FValues[J]:=Nil;
      I:=J;
    end;
  until False;
end;

procedure TOPPointerMap.Clear;
begin
  FKeys:=Nil;
  FValues:=Nil;
  FCount:=0;
end;

function TOPPointerMap.Capacity: Integer;
begin
  Result:=Length(FKeys);
end;

function TOPPointerMap.KeyAt(aSlot: Integer): Pointer;
begin
  Result:=FKeys[aSlot];
end;

{ Fields whose value the manager may change while saving or deleting }

function IsManagerWrittenField(aField: TOPFieldMap): Boolean;
begin
  Result:=(([foInKey, foAutoInc, foUUID, foVersion, foSoftDelete,
             foCreatedAt, foUpdatedAt, foCreatedBy, foUpdatedBy] * aField.Options) <> [])
          or (aField.SequenceName<>'')
          or Assigned(aField.OnGenerateValue);
end;

constructor TOPUndoLog.Create;
begin
  inherited Create;
  FEntries:=TFPList.Create;
  FLastEntry:=TOPPointerMap.Create;
  FTouched:=TOPPointerMap.Create;
  FObserved:=TOPPointerMap.Create;
end;

destructor TOPUndoLog.Destroy;
begin
  Clear;
  FreeAndNil(FObserved);
  FreeAndNil(FTouched);
  FreeAndNil(FLastEntry);
  FreeAndNil(FEntries);
  inherited Destroy;
end;

function TOPUndoLog.Observe(aObject: TObject): Boolean;
var
  Observed: IFPObserved;
begin
  Result:=FObserved.Contains(aObject);
  if Result then
    Exit;
  Result:=aObject.GetInterface(SGUIDObserved, Observed);
  if Result then
  begin
    Observed.FPOAttachObserver(Self);
    FObserved.Put(aObject, Nil);
  end;
end;

{ The entries of a freed object stay in the list, marked dead (Obj=Nil):
  removing them from the middle of the list would cost a scan per object. }

procedure TOPUndoLog.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  P: Pointer;
  Entry: TOPUndoEntry;
begin
  if Operation<>ooFree then
    Exit;
  if FLastEntry.Find(ASender, P) then
  begin
    Entry:=TOPUndoEntry(P);
    while Assigned(Entry) do
    begin
      Entry.Obj:=Nil;
      Entry:=Entry.PrevSame;
    end;
    FLastEntry.Remove(ASender);
  end;
  FTouched.Remove(ASender);
  FObserved.Remove(ASender);
end;

procedure TOPUndoLog.RecordObject(aObject: TObject; aFieldMap: TOPFieldMapList; aConn: TSQLConnection);
var
  Entry: TOPUndoEntry;
  IPS: IOPPersistenceState;
  I, Len: Integer;
  F: TOPFieldMap;
  P: Pointer;
begin
  FHasWrites:=True;
  Entry:=TOPUndoEntry.Create;
  Entry.Obj:=aObject;
  Entry.Conn:=aConn;
  Entry.HasState:=Supports(aObject, IOPPersistenceState, IPS);
  if Entry.HasState then
    Entry.OldState:=IPS.GetPersistenceState;
  Len:=0;
  SetLength(Entry.Fields, aFieldMap.Count);
  for I:=0 to aFieldMap.Count-1 do
  begin
    F:=aFieldMap[I];
    // Only published properties of simple types can be read and written back
    if (F.PropInfo=Nil) or (F.PropertyKind=tkClass) or not IsManagerWrittenField(F) then
      Continue;
    try
      Entry.Fields[Len].Value:=GetPropValue(aObject, F.PropInfo, False);
    except
      Continue;
    end;
    Entry.Fields[Len].Field:=F;
    Entry.Fields[Len].WasNull:=OPFIsFieldNull(aObject, F);
    Inc(Len);
  end;
  SetLength(Entry.Fields, Len);
  if Observe(aObject) then
  begin
    if FLastEntry.Find(aObject, P) then
      Entry.PrevSame:=TOPUndoEntry(P);
    FLastEntry.Put(aObject, Entry);
  end
  else if FDepth=0 then
  begin
    // Outside an operation an unobservable object cannot be kept safely
    Entry.Free;
    Exit;
  end;
  FEntries.Add(Entry);
end;

procedure TOPUndoLog.Touch(aObject: TObject; aTracksUnobserved: Boolean);
begin
  if FTouched.Contains(aObject) then
    Exit;
  // An object that cannot be observed may be freed before the rollback. It
  // only matters when the tracker keeps a state for it.
  if Observe(aObject) then
    FTouched.Put(aObject, Nil)
  else if aTracksUnobserved then
    FLostTouched:=True;
end;

procedure TOPUndoLog.RestoreEntry(aEntry: TOPUndoEntry);
var
  I: Integer;
  IPS: IOPPersistenceState;
  F: TOPFieldMap;
begin
  for I:=0 to Length(aEntry.Fields)-1 do
  begin
    F:=aEntry.Fields[I].Field;
    try
      SetPropValue(aEntry.Obj, F.PropInfo, aEntry.Fields[I].Value);
      // After the value: a setter may change the null state
      if foNullable in F.Options then
        OPFSetPropNull(aEntry.Obj, F.PropertyName, F.PropInfo, aEntry.Fields[I].WasNull);
    except
      on E: Exception do
        OPLogger.LogWarning(Self, 'Rollback: cannot restore %s.%s: %s',
          [aEntry.Obj.ClassName, F.PropertyName, E.Message]);
    end;
  end;
  if aEntry.HasState and Supports(aEntry.Obj, IOPPersistenceState, IPS) then
    IPS.SetPersistenceState(aEntry.OldState);
end;

procedure TOPUndoLog.BeginOperation;
begin
  if FDepth=0 then
    FMark:=FEntries.Count;
  Inc(FDepth);
end;

procedure TOPUndoLog.EndOperation(aFailed: Boolean);
var
  I: Integer;
  Entry: TOPUndoEntry;
  P: Pointer;
begin
  if FDepth>0 then
    Dec(FDepth);
  if FDepth>0 then
    Exit;
  if FMark>FEntries.Count then
    FMark:=FEntries.Count;
  if aFailed then
  begin
    // Newest first: each removed entry is the newest of its object
    for I:=FEntries.Count-1 downto FMark do
    begin
      Entry:=TOPUndoEntry(FEntries[I]);
      if Assigned(Entry.Obj) then
      begin
        RestoreEntry(Entry);
        if FLastEntry.Find(Entry.Obj, P) and (P=Pointer(Entry)) then
          if Assigned(Entry.PrevSame) then
            FLastEntry.Put(Entry.Obj, Entry.PrevSame)
          else
            FLastEntry.Remove(Entry.Obj);
      end;
      Entry.Free;
    end;
    FEntries.Count:=FMark;
  end;
  // The caller may free the objects from now on
  DropUnobserved;
end;

{ Entries before FMark were checked when their operation ended, so only the
  entries of the operation that just ended are checked. Unobservable objects
  are never in FTouched (Touch flags them at once). }

procedure TOPUndoLog.DropUnobserved;
var
  I, J: Integer;
  Entry: TOPUndoEntry;
begin
  J:=FMark;
  for I:=FMark to FEntries.Count-1 do
  begin
    Entry:=TOPUndoEntry(FEntries[I]);
    if Assigned(Entry.Obj) and FObserved.Contains(Entry.Obj) then
    begin
      FEntries[J]:=Entry;
      Inc(J);
    end
    else
      Entry.Free;
  end;
  FEntries.Count:=J;
end;

procedure TOPUndoLog.Restore(aTracker: IOPChangeTracker; const aCommitted: array of TSQLConnection);

  function IsCommitted(aConn: TSQLConnection): Boolean;
  var
    C: TSQLConnection;
  begin
    Result:=False;
    if aConn<>Nil then
      for C in aCommitted do
        if C=aConn then
          Exit(True);
  end;

var
  I: Integer;
  Entry: TOPUndoEntry;
begin
  // Reverse order: the oldest entry of an object is applied last
  for I:=FEntries.Count-1 downto 0 do
  begin
    Entry:=TOPUndoEntry(FEntries[I]);
    if Assigned(Entry.Obj) and not IsCommitted(Entry.Conn) then
      RestoreEntry(Entry);
  end;
  if Assigned(aTracker) then
    try
      if FLostTouched and FHasWrites then
        aTracker.PurgeAll
      else
        for I:=0 to FTouched.Capacity-1 do
          if FTouched.KeyAt(I)<>Nil then
            aTracker.Discard(TObject(FTouched.KeyAt(I)));
    except
      on E: Exception do
        OPLogger.LogWarning(Self, 'Rollback: cannot discard snapshots: %s', [E.Message]);
    end;
  Clear;
end;

procedure TOPUndoLog.Clear;
var
  I: Integer;
  Observed: IFPObserved;
begin
  for I:=0 to FObserved.Capacity-1 do
    if (FObserved.KeyAt(I)<>Nil)
       and TObject(FObserved.KeyAt(I)).GetInterface(SGUIDObserved, Observed) then
      Observed.FPODetachObserver(Self);
  FObserved.Clear;
  for I:=0 to FEntries.Count-1 do
    TOPUndoEntry(FEntries[I]).Free;
  FEntries.Clear;
  FLastEntry.Clear;
  FTouched.Clear;
  FLostTouched:=False;
  FHasWrites:=False;
  FMark:=0;
end;

{$IFNDEF VER3_2}
type
  TOPLazyBindKind = (lbkChildren, lbkAssociations, lbkSingle);

  { TOPLazyBinding - helper object that captures context for auto-setup lazy loading.
    Its DoLoad method is assigned as the TLazyLoadEvent callback on the record.
    The record also holds an interface reference to it (TOPLazy.FKeeper), so
    it is freed when the entity is freed, whatever the entity's class. The
    manager only keeps weak references: when the manager is freed, its
    bindings stay with their records, and raise when their entity is lazy
    loaded (FManager=Nil). }

  TOPLazyBinding = class(TInterfacedObject)
  public
    FManager: TObject;
    FParent: TObject;
    FChildClass: TClass;
    FForeignKeyField: UTF8String;
    FKind: TOPLazyBindKind;
    destructor Destroy; override;
    procedure DoLoad(aTarget: TObject);
  end;

destructor TOPLazyBinding.Destroy;
begin
  if Assigned(FManager) then
    TOPPointerMap(TOPManager(FManager).FLazyBindings).Remove(Self);
  inherited Destroy;
end;

procedure TOPLazyBinding.DoLoad(aTarget: TObject);
var
  FKProp: UTF8String;
  Mgr: TOPManager;
  FKValue: Variant;
  Def: Opf.Registry.TPersisterDef;
  PKField: TOPFieldMap;
  Persister: TOPPersister;
  PKParam: TParam;
begin
  if FManager=Nil then
    raise EOPF.CreateFmt(SErrLazyManagerFreed, [FParent.ClassName]);
  Mgr := TOPManager(FManager);
  case FKind of
    lbkChildren:
      Mgr.LoadChildren(FParent, FChildClass, aTarget);
    lbkAssociations:
      Mgr.LoadAssociations(FParent, FChildClass, aTarget);
    lbkSingle:
    begin
      { Transfer FK value from parent to target's PK, then Load }
      if FForeignKeyField = '' then
        Exit;
      { The relation names the column: read the property mapped to it }
      FKProp := FForeignKeyField;
      Def := Mgr.Registry.FindPersister(FParent.ClassType);
      if Def <> nil then
      begin
        PKField := Def.FieldMap.FindByFieldName(FForeignKeyField);
        if PKField <> nil then
          FKProp := PKField.PropertyName;
      end;
      FKValue := GetPropValue(FParent, FKProp);
      if VarIsNull(FKValue) or VarIsEmpty(FKValue) then
        Exit;
      { Skip if FK is zero (default/unset for integer keys) }
      if VarIsOrdinal(FKValue) and (FKValue = 0) then
        Exit;
      { Set PK on target }
      Def := Mgr.Registry.FindPersister(aTarget.ClassType);
      if Def = nil then
        Exit;
      PKField := Def.FieldMap.FindPrimaryKeyField;
      if PKField = nil then
        Exit;
      SetPropValue(aTarget, PKField.PropertyName, FKValue);
      Mgr.Load(aTarget);
    end;
  end;
end;
{$ENDIF}

{ Dirty tracking private methods }

procedure TOPManager.CaptureObjectSnapshot(aObject: TObject; aFieldMap: TOPFieldMapList);
begin
  if Assigned(FChangeTracker) then
  begin
    TouchObject(aObject);
    FChangeTracker.Capture(aObject, aFieldMap);
  end;
end;


function TOPManager.GetObjectChanges(aObject: TObject; aFieldMap: TOPFieldMapList;
  out aNames: TUTF8StringArray): Boolean;

begin
  aNames:=Nil;
  Result:=Assigned(FChangeTracker) and FChangeTracker.GetChanges(aObject,aFieldMap,aNames);
end;

procedure TOPManager.DiscardObjectSnapshot(aObject: TObject);
begin
  if Assigned(FChangeTracker) then
  begin
    TouchObject(aObject);
    FChangeTracker.Discard(aObject);
  end;
end;


procedure TOPManager.PurgeAllSnapshots;

begin
  if Assigned(FChangeTracker) then
    FChangeTracker.PurgeAll;
end;


Function OPManager : TOPManager;

begin
  Result:=TOPManager.Instance
end;


{ TOPConnectionList }

function TOPConnectionList.GetConnection(aIndex : Integer): TOPConnection;
begin
  Result:=TOPConnection(Definitions[aIndex]);
end;

procedure TOPConnectionList.SetConnection(aIndex : Integer; AValue: TOPConnection);
begin
  Definitions[aIndex]:=aValue;
end;

{ TOPAuditInfo }

procedure TOPAuditInfo.SetNumericalID(AValue: Int64);
begin
  if FNumericalID = AValue then
    Exit;
  FNumericalID := AValue;
  if AValue <> 0 then
    FUserIDKind := ukNumerical
  else if FUserIDKind = ukNumerical then
    FUserIDKind := ukNone;
end;

procedure TOPAuditInfo.SetStringID(AValue: String);
begin
  if FStringID = AValue then
    Exit;
  FStringID := AValue;
  if AValue <> '' then
    FUserIDKind := ukString
  else if FUserIDKind = ukString then
    FUserIDKind := ukNone;
end;

{ The primary key of aObject as text: IOPPrimaryKey, else the key field of
  its persister in aRegistry. Raises EOPF when there is none. }

function AuditObjectKey(aObject: TObject; aRegistry: TOPRegistry): String;
var
  IPK: IOPPrimaryKey;
  PKArr: TStringArray;
  Def: Opf.Registry.TPersisterDef;
  PKField: TOPFieldMap;
begin
  Result := '';
  if Supports(aObject, IOPPrimaryKey, IPK) then
  begin
    PKArr := IPK.GetPrimaryKey(aObject);
    if Length(PKArr) = 0 then
      raise EOPF.CreateFmt(SErrAuditUserNoPrimaryKey, [aObject.ClassName]);
    Exit(PKArr[0]);
  end;
  Def := aRegistry.FindPersister(aObject.ClassType);
  if Def = Nil then
    raise EOPF.CreateFmt(SErrAuditUserNoPrimaryKey, [aObject.ClassName]);
  PKField := Def.FieldMap.FindPrimaryKeyField;
  if PKField = Nil then
    raise EOPF.CreateFmt(SErrAuditUserNoPrimaryKey, [aObject.ClassName]);
  case PKField.PropertyKind of
    tkSString, tkAString, tkWString, tkUString:
      Result := GetStrProp(aObject, PKField.PropertyName);
    tkInteger:
      Result := IntToStr(GetOrdProp(aObject, PKField.PropertyName));
    tkInt64:
      Result := IntToStr(GetInt64Prop(aObject, PKField.PropertyName));
  else
    raise EOPF.CreateFmt(SErrAuditUserNoPrimaryKey, [aObject.ClassName]);
  end;
end;

procedure TOPAuditInfo.SetObjectID(AValue: TObject);
var
  Reg: TOPRegistry;
begin
  if AValue = Nil then
  begin
    FObjectKey := '';
    FObjectClassName := '';
    if FUserIDKind = ukObject then
      FUserIDKind := ukNone;
    Exit;
  end;
  if Assigned(FManager) then
    Reg := TOPManager(FManager).Registry
  else
    Reg := TOPRegistry.Instance;
  // Raises before anything is changed
  FObjectKey := AuditObjectKey(AValue, Reg);
  FObjectClassName := AValue.ClassName;
  FUserIDKind := ukObject;
end;

procedure TOPAuditInfo.Assign(Source: TPersistent);
var
  Src: TOPAuditInfo;
begin
  if Source is TOPAuditInfo then
  begin
    Src := TOPAuditInfo(Source);
    FTimestampKind := Src.FTimestampKind;
    FUserIDKind := Src.FUserIDKind;
    FNumericalID := Src.FNumericalID;
    FStringID := Src.FStringID;
    FObjectKey := Src.FObjectKey;
    FObjectClassName := Src.FObjectClassName;
  end
  else
    inherited Assign(Source);
end;

{ TOPConnection }

procedure TOPConnection.SetDefault(AValue: Boolean);

Var
  i : integer;

begin
  if FDefault=AValue then Exit;
  if AValue then
    if Collection is TOPConnectionList then
      with TOPConnectionList(Collection) do
        for i:=0 to Count-1 do
          if Connections[i]<>Self then
            Connections[i].IsDefault:=False;
  FDefault:=AValue;
end;

{ TOPManager }

function TOPManager.GetDefaultConnection: TSQLConnection;
begin
  Result:=ConnectionFor(Nil);
end;

function TOPManager.ConnectionFor(aClass: TClass): TSQLConnection;
var
  I: Integer;
  C: TOPConnection;
begin
  if Assigned(FConnectionResolver) then
  begin
    Result:=FConnectionResolver.ResolveConnection(aClass);
    if Result<>Nil then
      Exit;
  end;
  for I:=0 to FConnections.Count-1 do
  begin
    C:=FConnections[I];
    if C.IsDefault then
    begin
      Result:=C.SingleConnection;
      if Result=Nil then
        RaiseError('GetDefaultConnection', SErrDefaultConnectionNil);
      Exit;
    end;
  end;
  RaiseError('GetDefaultConnection', SErrNoDefaultConnection);
  Result:=Nil;
end;

function TOPManager.JunctionConnection(aDef: TOPManyToManyDef): TSQLConnection;
begin
  Result:=ConnectionFor(aDef.ClassA);
end;

class function TOPManager.GetExceptionClass: EOPFClass;
begin
  Result := EOPF;
end;

procedure TOPManager.RegisterConnectionForTransaction(aConn: TSQLConnection);
begin
  if Assigned(FTransactionHandler) then
    FTransactionHandler.EnsureConnection(aConn);
end;

{ The sqldb log hook is global: one dispatcher is installed for all managers
  that want it, so managers can switch logging on and off in any order. The
  hook that was installed before the first manager is restored after the last. }

type
  TOPDBLogDispatcher = class
    procedure Log(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
  end;

var
  DBLogManagers: TFPList = Nil;
  DBLogDispatcher: TOPDBLogDispatcher = Nil;
  DBLogPrevious: TDBLogNotifyEvent = Nil;

procedure TOPDBLogDispatcher.Log(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
var
  I: Integer;
begin
  // Does not use Self: also safe when called after the unit was finalized
  if DBLogManagers=Nil then
    Exit;
  for I:=0 to DBLogManagers.Count-1 do
    TOPManager(DBLogManagers[I]).DoSQLDBLog(Sender, EventType, Msg);
end;

function DBLogHookIsOurs: Boolean;
var
  Ours: TDBLogNotifyEvent;
begin
  Result:=Assigned(DBLogDispatcher);
  if not Result then
    Exit;
  Ours:=@DBLogDispatcher.Log;
  Result:=(TMethod(GlobalDBLogHook).Code=TMethod(Ours).Code)
          and (TMethod(GlobalDBLogHook).Data=TMethod(Ours).Data);
end;

procedure TOPManager.SetOptions(AValue: TOPFManagerOptions);
var
  Old: TOPFManagerOptions;
begin
  if FOptions=AValue then Exit;
  Old:=FOptions;
  FOptions:=AValue;
  if (omoHookSQLDBLog in AValue) and not (omoHookSQLDBLog in Old) then
    HookSQLDBLog
  else if (omoHookSQLDBLog in Old) and not (omoHookSQLDBLog in AValue) then
    UnhookSQLDBLog;
  if Assigned(FTransactionHandler) then
    FTransactionHandler.ResetConnectionOnRollbackError:=omoResetConnectionOnRollbackError in FOptions;
end;

procedure TOPManager.HookSQLDBLog;
begin
  if DBLogManagers=Nil then
    DBLogManagers:=TFPList.Create;
  if DBLogDispatcher=Nil then
    DBLogDispatcher:=TOPDBLogDispatcher.Create;
  if DBLogManagers.Count=0 then
  begin
    DBLogPrevious:=GlobalDBLogHook;
    GlobalDBLogHook:=@DBLogDispatcher.Log;
  end;
  DBLogManagers.Add(Self);
end;

procedure TOPManager.UnhookSQLDBLog;
begin
  // Nil after finalization: the class destructor frees the global instance later
  if DBLogManagers=Nil then
    Exit;
  DBLogManagers.Remove(Self);
  // Someone else may have replaced the hook since: then leave theirs
  if (DBLogManagers.Count=0) and DBLogHookIsOurs then
  begin
    GlobalDBLogHook:=DBLogPrevious;
    DBLogPrevious:=Nil;
  end;
end;

procedure TOPManager.DoSQLDBLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
var
  CN: String;
begin
  if Assigned(Sender) then
  begin
    CN:=Sender.Name;
    if CN='' then
      CN:=Sender.ClassName;
  end
  else
    CN:='(nil)';
  TOPLogger(GetLogger).Log(Self, lkSQL, '[SQLDB] (%s) (%s): %s', [CN, GetEnumName(TypeInfo(TDBEventType), Ord(EventType)), Msg]);
end;

procedure TOPManager.DoBeforeSave(aObject: TObject; aState: TObjectPersistenceState);
begin
  if Assigned(FBeforeSave) then
    FBeforeSave(Self, aObject, aState);
end;

procedure TOPManager.DoAfterSave(aObject: TObject; aState: TObjectPersistenceState);
begin
  if Assigned(FAfterSave) then
    FAfterSave(Self, aObject, aState);
end;

procedure TOPManager.DoAfterLoad(aObject: TObject);
begin
  if Assigned(FAfterLoad) then
    FAfterLoad(Self, aObject);
end;

function TOPManager.LazyBindingCount: Integer;
begin
  {$IFNDEF VER3_2}
  Result:=TOPPointerMap(FLazyBindings).Count;
  {$ELSE}
  Result:=0;
  {$ENDIF}
end;

{$IFNDEF VER3_2}

procedure TOPManager.AddLazyBinding(aBinding: TObject);
begin
  TOPPointerMap(FLazyBindings).Put(aBinding, Nil);
end;
{$ENDIF}

procedure TOPManager.InitLazyFields(aObject: TObject);
{$IFNDEF VER3_2}

  procedure SetupAutoLazy(aObject: TObject);

    { Read a class field's TObject value via RTTI, avoiding property getters.
      Looks for field named 'F' + aPropertyName on the class. }
    function ReadBackingField(aRttiType: TRttiType; aObj: TObject;
      const aPropertyName: UTF8String): TObject;
    var
      BF: TRttiField;
    begin
      Result := nil;
      BF := aRttiType.GetField('F' + aPropertyName);
      if (BF <> nil) and (BF.FieldType.TypeKind = tkClass) then
        Result := TObject(PPointer(Pointer(aObj) + BF.Offset)^);
    end;

  var
    Ctx: TRttiContext;
    RttiClassType: TRttiType;
    ClassFields: specialize TArray<TRttiField>;
    RF: TRttiField;
    RecPtr: Pointer;
    FOnLoadRF: TRttiField;
    FKeeperRF: TRttiField;
    FValueObj: TObject;
    Relations: TOPRelationDefArray;
    M2MDefs: TOPManyToManyDefArray;
    ParentRel: TOPRelationDef;
    I: Integer;
    PropName: UTF8String;
    ListObj: TObject;
    Binding: TOPLazyBinding;
    M: TMethod;
    Matched: Boolean;
  begin
    Ctx := TRttiContext.Create(False);
    try
      RttiClassType := Ctx.GetType(aObject.ClassType);
      if RttiClassType = nil then
        Exit;
      ClassFields := RttiClassType.GetFields;
      for RF in ClassFields do
      begin
        if not IsLazyRecordField(RF) then
          Continue;
        { Get pointer to the record field inside the object instance }
        RecPtr := Pointer(aObject) + RF.Offset;
        { Find the FOnLoad field in the record type }
        FOnLoadRF := RF.FieldType.GetField('FOnLoad');
        FKeeperRF := RF.FieldType.GetField('FKeeper');
        if (FOnLoadRF = nil) or (FKeeperRF = nil) then
          Continue;
        { Skip if FOnLoad is already set (avoid binding accumulation on repeated Load) }
        if PMethod(RecPtr + FOnLoadRF.Offset)^.Code <> nil then
          Continue;
        { Read FValue from the record using RTTI offset }
        FValueObj := TObject(PPointer(RecPtr + RF.FieldType.GetField('FValue').Offset)^);
        if FValueObj = nil then
          Continue;
        Matched := False;
        { Match against 1:N relation registry by comparing FValue instance
          to the backing field (F<PropertyName>) - avoids calling property
          getters which would trigger premature lazy loading. }
        Relations := RelationRegistry.FindChildRelations(aObject.ClassType);
        for I := 0 to High(Relations) do
        begin
          PropName := ResolveChildListPropertyName(aObject.ClassType, Relations[I]);
          ListObj := ReadBackingField(RttiClassType, aObject, PropName);
          if ListObj <> FValueObj then
            Continue;
          { Match found - this lazy field wraps this relation's list }
          Binding := TOPLazyBinding.Create;
          Binding.FManager := Self;
          Binding.FParent := aObject;
          Binding.FChildClass := Relations[I].ChildClass;
          Binding.FKind := lbkChildren;
          M.Code := @TOPLazyBinding.DoLoad;
          M.Data := Binding;
          PMethod(RecPtr + FOnLoadRF.Offset)^ := M;
          // The record keeps the binding alive, and frees it with the entity
          PInterface(RecPtr + FKeeperRF.Offset)^ := IInterface(Binding);
          AddLazyBinding(Binding);
          Matched := True;
          Break;
        end;
        if Matched then
          Continue;
        { Match against M:N registry }
        M2MDefs := M2MRegistry.FindDefsForClass(aObject.ClassType);
        for I := 0 to High(M2MDefs) do
        begin
          { Resolve associated class (the OTHER side) and list property name }
          if aObject.ClassType = M2MDefs[I].ClassA then
            PropName := ResolveM2MListPropertyName(M2MDefs[I], M2MDefs[I].ClassB)
          else
            PropName := ResolveM2MListPropertyName(M2MDefs[I], M2MDefs[I].ClassA);
          ListObj := ReadBackingField(RttiClassType, aObject, PropName);
          if ListObj <> FValueObj then
            Continue;
          Binding := TOPLazyBinding.Create;
          Binding.FManager := Self;
          Binding.FParent := aObject;
          if aObject.ClassType = M2MDefs[I].ClassA then
            Binding.FChildClass := M2MDefs[I].ClassB
          else
            Binding.FChildClass := M2MDefs[I].ClassA;
          Binding.FKind := lbkAssociations;
          M.Code := @TOPLazyBinding.DoLoad;
          M.Data := Binding;
          PMethod(RecPtr + FOnLoadRF.Offset)^ := M;
          // The record keeps the binding alive, and frees it with the entity
          PInterface(RecPtr + FKeeperRF.Offset)^ := IInterface(Binding);
          AddLazyBinding(Binding);
          Matched := True;
          Break;
        end;
        if Matched then
          Continue;
        { Match against N:1 reverse relation - FValue is a single entity object.
          Look for a relation where the current class is the child. }
        ParentRel := RelationRegistry.FindParentRelation(aObject.ClassType);
        if (ParentRel <> nil) and (FValueObj.ClassType = ParentRel.ParentClass) then
        begin
          Binding := TOPLazyBinding.Create;
          Binding.FManager := Self;
          Binding.FParent := aObject;
          Binding.FChildClass := ParentRel.ParentClass;
          Binding.FForeignKeyField := ParentRel.ForeignKeyFieldName;
          Binding.FKind := lbkSingle;
          M.Code := @TOPLazyBinding.DoLoad;
          M.Data := Binding;
          PMethod(RecPtr + FOnLoadRF.Offset)^ := M;
          // The record keeps the binding alive, and frees it with the entity
          PInterface(RecPtr + FKeeperRF.Offset)^ := IInterface(Binding);
          AddLazyBinding(Binding);
        end;
      end;
    finally
      Ctx.Free;
    end;
  end;

{$ENDIF}
var
  LI: IOPLazyInit;
begin
  { Path 1: IOPLazyInit interface - works on all FPC versions }
  if aObject.GetInterface(SGUIDIOPLazyInit, LI) then
    LI.InitLazyLoaders(Self);
{$IFNDEF VER3_2}
  { Path 2: Extended RTTI auto-setup - FPC 3.3.1+ only.
    Only runs if IOPLazyInit was NOT implemented (avoid double setup). }
  if not aObject.GetInterface(SGUIDIOPLazyInit, LI) then
    SetupAutoLazy(aObject);
{$ENDIF}
end;

procedure TOPManager.DoBeforeDelete(aObject: TObject);
begin
  if Assigned(FBeforeDelete) then
    FBeforeDelete(Self, aObject);
end;

procedure TOPManager.DoAfterDelete(aObject: TObject);
begin
  if Assigned(FAfterDelete) then
    FAfterDelete(Self, aObject);
end;

procedure TOPManager.SetAuditInfo(AValue: TOPAuditInfo);
begin
  if AValue <> FAuditInfo then
    FAuditInfo.Assign(AValue);
end;

function TOPManager.GetTimestamp: TDateTime;
begin
  if FAuditInfo.TimestampKind = tkUTC then
    Result := LocalTimeToUniversal(Now)
  else
    Result := Now;
end;

procedure TOPManager.ApplyAuditFields(aObject: TObject; aState: TObjectPersistenceState; aFieldMap: TOPFieldMapList);

  function ResolveUserValueAsString: String;
  begin
    case FAuditInfo.UserIDKind of
      ukNumerical:
        Result := IntToStr(FAuditInfo.NumericalID);
      ukString:
        Result := FAuditInfo.StringID;
      ukObject:
        Result := FAuditInfo.ObjectKey;
    else
      Result := '';
    end;
  end;

  function ResolveUserValueAsInt64: Int64;
  begin
    case FAuditInfo.UserIDKind of
      ukNumerical:
        Result := FAuditInfo.NumericalID;
      ukString:
        try
          Result := StrToInt64(FAuditInfo.StringID);
        except
          on E: Exception do
            RaiseError('ApplyAuditFields', SErrAuditUserConversion, ['', E.Message]);
        end;
      ukObject:
        try
          Result := StrToInt64(ResolveUserValueAsString);
        except
          on E: Exception do
            RaiseError('ApplyAuditFields', SErrAuditUserConversion, ['', E.Message]);
        end;
    else
      Result := 0;
    end;
  end;

  procedure ApplyUserField(aField: TOPFieldMap; const aOptionName: String);
  begin
    if FAuditInfo.UserIDKind = ukNone then
      RaiseError('ApplyAuditFields', SErrAuditUserNotSet, [aField.PropertyName, aOptionName]);
    case aField.PropertyKind of
      tkSString, tkAString, tkWString, tkUString:
        SetStrProp(aObject, aField.PropertyName, ResolveUserValueAsString);
      tkInteger:
        SetOrdProp(aObject, aField.PropertyName, ResolveUserValueAsInt64);
      tkInt64:
        SetInt64Prop(aObject, aField.PropertyName, ResolveUserValueAsInt64);
    else
      RaiseError('ApplyAuditFields', SErrAuditUserConversion, [aField.PropertyName, 'unsupported property type']);
    end;
  end;

var
  I: Integer;
  Field: TOPFieldMap;
  IsInsert: Boolean;
  TS: TDateTime;
  HasTimestamp: Boolean;
begin
  IsInsert := (aState = opsNew);
  HasTimestamp := False;
  TS := 0;
  for I := 0 to aFieldMap.Count - 1 do
  begin
    Field := aFieldMap[I];
    // foCreatedAt - INSERT only
    if (foCreatedAt in Field.Options) and IsInsert then
    begin
      if not HasTimestamp then
      begin
        TS := GetTimestamp;
        HasTimestamp := True;
      end;
      SetFloatProp(aObject, Field.PropertyName, TS);
    end;
    // foUpdatedAt - UPDATE always; INSERT only if foRequired
    if foUpdatedAt in Field.Options then
    begin
      if (not IsInsert) or (foRequired in Field.Options) then
      begin
        if not HasTimestamp then
        begin
          TS := GetTimestamp;
          HasTimestamp := True;
        end;
        SetFloatProp(aObject, Field.PropertyName, TS);
      end;
    end;
    // foCreatedBy - INSERT only
    if (foCreatedBy in Field.Options) and IsInsert then
      ApplyUserField(Field, 'foCreatedBy');
    // foUpdatedBy - UPDATE always; INSERT only if foRequired
    if foUpdatedBy in Field.Options then
    begin
      if (not IsInsert) or (foRequired in Field.Options) then
        ApplyUserField(Field, 'foUpdatedBy');
    end;
  end;
end;

function TOPManager.GetSoftDeleteFilter(aFieldMap: TOPFieldMapList; const aTablePrefix: UTF8String): UTF8String;
var
  SDF: TOPFieldMap;
  ColName: UTF8String;
  UseBoolKeyword: Boolean;
begin
  Result := '';
  SDF := aFieldMap.FindSoftDeleteField;
  if SDF = Nil then
    Exit;
  if aTablePrefix <> '' then
    ColName := SQLBuilder.QualifiedIdentifier(aTablePrefix, SDF.FieldName)
  else
    ColName := SQLBuilder.Identifier(SDF.FieldName);
  case SDF.PropertyKind of
    tkBool:
      begin
        UseBoolKeyword := (SQLBuilder.Driver.GetBooleanKeyword = kwBoolean);
        if foRequired in SDF.Options then
        begin
          // NOT NULL Boolean: FALSE = not deleted
          if UseBoolKeyword then
            Result := ColName + ' = FALSE'
          else
            Result := ColName + ' = 0';
        end
        else
        begin
          // Nullable Boolean: NULL or FALSE = not deleted
          if UseBoolKeyword then
            Result := '(' + ColName + ' IS NULL OR ' + ColName + ' = FALSE)'
          else
            Result := '(' + ColName + ' IS NULL OR ' + ColName + ' = 0)';
        end;
      end;
    tkFloat:
      // DateTime: NULL = not deleted
      Result := ColName + ' IS NULL';
  end;
end;

function TOPManager.CombineWhere(const aParts: array of UTF8String): UTF8String;
var
  S: UTF8String;
begin
  Result:='';
  for S in aParts do
    if S<>'' then
    begin
      if Result<>'' then
        Result:=Result+' AND ';
      Result:=Result+'('+S+')';
    end;
end;

function TOPManager.BuildKeySelectSQL(aDef: TPersisterDef): UTF8String;
begin
  Result:=BuildKeySelectSQL(aDef, aDef.FieldMap);
end;

function TOPManager.BuildKeySelectSQL(aDef: TPersisterDef; aSelectMap: TOPFieldMapList): UTF8String;
var
  KeyWhere: UTF8String;
begin
  KeyWhere:=SQLBuilder.GetWhereKey(aDef.TableName, aDef.FieldMap.AsArray);
  if KeyWhere='' then
    RaiseError('Load', SErrLoadNoKeyFields, [aDef.InstanceClass.ClassName]);
  Result:=SQLBuilder.GetSelectSQL(aDef.TableName, aSelectMap.AsArray)+
          ' WHERE '+CombineWhere([KeyWhere, GetSoftDeleteFilter(aDef.FieldMap)]);
end;

function TOPManager.BuildCriteriaSelectSQL(aDef: TPersisterDef; aCriteria: TOPCriteria): UTF8String;
var
  WhereSQL, OrderSQL, SDFilter, LimitSQL: UTF8String;
  PKField: TOPFieldMap;
begin
  Result:=SQLBuilder.GetSelectSQL(aDef.TableName, aDef.FieldMap.AsArray);
  WhereSQL:='';
  if aCriteria.HasWhereClause then
    WhereSQL:=aCriteria.ToWhereSQL(aDef.FieldMap, @SQLBuilder.FormatFieldIdentifier,
      @SQLBuilder.Driver.EscapeLikeText);
  SDFilter:='';
  if not aCriteria.HasIncludeDeleted then
    SDFilter:=GetSoftDeleteFilter(aDef.FieldMap);
  WhereSQL:=CombineWhere([WhereSQL, SDFilter]);
  if WhereSQL <> '' then
    Result:=Result + ' WHERE ' + WhereSQL;
  OrderSQL:=aCriteria.ToOrderBySQL(aDef.FieldMap, @SQLBuilder.FormatFieldIdentifier);
  if OrderSQL <> '' then
    Result:=Result + ' ' + OrderSQL;
  if (aCriteria.GetLimitValue > 0) or (aCriteria.GetOffsetValue > 0) then
  begin
    LimitSQL:=SQLBuilder.Driver.GetLimitOffsetSQL(aCriteria.GetLimitValue, aCriteria.GetOffsetValue);
    if LimitSQL <> '' then
    begin
      // SQL Server: OFFSET/FETCH is only valid after an ORDER BY
      if (OrderSQL='') and SQLBuilder.Driver.PaginationRequiresOrderBy then
      begin
        PKField:=aDef.FieldMap.FindPrimaryKeyField;
        if Assigned(PKField) then
          Result:=Result + ' ORDER BY ' + SQLBuilder.Identifier(PKField.FieldName)
        else
          Result:=Result + ' ORDER BY (SELECT NULL)';
      end;
      Result:=Result + ' ' + LimitSQL;
    end;
  end;
end;

function TOPManager.BuildChildrenSQL(aChildDef: TPersisterDef; const aForeignKeyField: UTF8String): UTF8String;
begin
  Result:=SQLBuilder.GetSelectSQL(aChildDef.TableName, aChildDef.FieldMap.AsArray)+
          ' WHERE '+CombineWhere([SQLBuilder.Identifier(aForeignKeyField)+' = :opf_parentkey',
                                  GetSoftDeleteFilter(aChildDef.FieldMap)]);
end;

function TOPManager.BuildAssociationsSQL(aAssocDef: TPersisterDef;
  const aJunctionTable, aFKParent, aFKAssociated: UTF8String): UTF8String;
var
  PKField: TOPFieldMap;
  FieldList: UTF8String;
begin
  PKField:=aAssocDef.FieldMap.FindPrimaryKeyField;
  if PKField = Nil then
    RaiseError('LoadAssociations', SErrLoadNoKeyFields, [aAssocDef.InstanceClass.ClassName]);
  FieldList:=SQLBuilder.GetSelectSQL(aAssocDef.TableName, aAssocDef.FieldMap.AsArray, [soFieldListOnly, soPrependTable]);
  Result:=Format('SELECT %s FROM %s INNER JOIN %s ON %s = %s WHERE %s',
    [FieldList,
     SQLBuilder.Identifier(aAssocDef.TableName, itTable),
     SQLBuilder.Identifier(aJunctionTable, itTable),
     SQLBuilder.QualifiedIdentifier(aJunctionTable, aFKAssociated),
     SQLBuilder.QualifiedIdentifier(aAssocDef.TableName, PKField.FieldName),
     CombineWhere([SQLBuilder.QualifiedIdentifier(aJunctionTable, aFKParent)+' = :opf_parentkey',
                   GetSoftDeleteFilter(aAssocDef.FieldMap, aAssocDef.TableName)])]);
end;

function TOPManager.CreateParentKeyParams(aParent: TObject; aParentDef: TPersisterDef;
  aPKField: TOPFieldMap): TParams;
var
  Persister: TOPPersister;
  P: TParam;
begin
  Result:=TParams.Create(TParam);
  try
    P:=TParam(Result.Add);
    Persister:=CreatePersister(aParentDef, 'Load');
    try
      // The persister sets the value with the field's data type, so the key
      // is compared with its native type (integer, string, GUID, ...).
      Persister.PersistProperty(aParent, aPKField, P);
    finally
      Persister.Free;
    end;
    P.Name:='opf_parentkey';
  except
    Result.Free;
    raise;
  end;
end;

procedure TOPManager.BeginTransaction;
begin
  // A new operation starts: no exception is being unwound through the manager
  FLoggedError:=Nil;
  if not Assigned(FTransactionHandler) then
  begin
    FTransactionHandler:=TTransactionHandler.Create;
    FTransactionHandler.ActiveTransactionPolicy:=FActiveTransactionPolicy;
    FTransactionHandler.ResetConnectionOnRollbackError:=omoResetConnectionOnRollbackError in FOptions;
    FTransactionHandler.OnPartialCommit:=FOnPartialCommit;
    FTransactionHandler.Sender:=Self;
  end;
  FTransactionHandler.BeginTransaction;
end;

function TOPManager.CreatePersister(aDef: TPersisterDef; const aOperation: string): TOPPersister;
begin
  if aDef.PersisterClass=Nil then
    RaiseError(aOperation, SErrNoPersisterClassForClass, [aDef.InstanceClass.ClassName]);
  Result:=aDef.PersisterClass.Create;
end;

procedure TOPManager.RecordUndo(aObject: TObject; aFieldMap: TOPFieldMapList; aConn: TSQLConnection);
begin
  // Outside a transaction nothing is rolled back, so there is nothing to undo
  if not Assigned(FTransactionHandler) or (aObject=Nil) or (aFieldMap=Nil) then
    Exit;
  if FUndoLog=Nil then
    FUndoLog:=TOPUndoLog.Create;
  TOPUndoLog(FUndoLog).RecordObject(aObject, aFieldMap, aConn);
end;

procedure TOPManager.TouchObject(aObject: TObject);
begin
  if not Assigned(FTransactionHandler) or (aObject=Nil) then
    Exit;
  if FUndoLog=Nil then
    FUndoLog:=TOPUndoLog.Create;
  TOPUndoLog(FUndoLog).Touch(aObject, Assigned(FChangeTracker) and FChangeTracker.TracksUnobserved);
end;

{ End of the outermost transaction: on rollback, objects get back the state
  they had before the transaction, and the change tracking snapshots taken or
  dropped in it are discarded (the next save writes all fields). }

procedure TOPManager.FinishUndo(aRestore: Boolean);
begin
  FinishUndo(aRestore, []);
end;

procedure TOPManager.FinishUndo(aRestore: Boolean; const aCommitted: array of TSQLConnection);
begin
  if FUndoLog=Nil then
    Exit;
  if aRestore then
    TOPUndoLog(FUndoLog).Restore(FChangeTracker, aCommitted)
  else
    TOPUndoLog(FUndoLog).Clear;
end;

procedure TOPManager.Commit;
var
  Committed: TOPConnectionArray;
begin
  if not Assigned(FTransactionHandler) then
    RaiseError('Commit', SErrNoActiveTransaction);
  try
    if FTransactionHandler.Commit then
    begin
      FreeAndNil(FTransactionHandler);
      FinishUndo(False);
    end;
  except
    // Failed or doomed outermost commit: the database was rolled back, except
    // on the connections a partial commit did commit: their objects keep
    // what was written to the database.
    Committed:=Nil;
    if (ExceptObject is EOPFPartialCommit) and Assigned(FTransactionHandler) then
      Committed:=FTransactionHandler.CommittedConnections;
    FreeAndNil(FTransactionHandler);
    FinishUndo(True, Committed);
    raise;
  end;
end;

procedure TOPManager.Rollback;
var
  IsOutermost: Boolean;
begin
  if not Assigned(FTransactionHandler) then
    RaiseError('Rollback', SErrNoActiveTransaction);
  try
    IsOutermost:=FTransactionHandler.Rollback;
  except
    // The handler only raises on the outermost rollback, after releasing all transactions.
    FreeAndNil(FTransactionHandler);
    FinishUndo(True);
    raise;
  end;
  if IsOutermost then
  begin
    FreeAndNil(FTransactionHandler);
    // Covers any outermost rollback, including the implicit transaction of a failed operation.
    FinishUndo(True);
  end;
end;

{ Called from an except block of an operation that started its own transaction.
  The failure is logged once: nested operations share the exception object, and
  FLoggedError is reset by BeginTransaction when the next operation starts.
  If a failed outermost Commit already released the transaction there is nothing
  left to roll back. A rollback failure is logged and appended to the message of
  the exception being handled instead of replacing it, so the original error
  propagates when the caller re-raises. }

procedure TOPManager.LogOperationFailure(const aOperation: string);
{$IFNDEF OPF_SKIP_LOGGING}
var
  Original: TObject;
{$ENDIF}
begin
  {$IFNDEF OPF_SKIP_LOGGING}
  Original:=ExceptObject;
  if (Original<>FLoggedError) and (Original is Exception) then
  begin
    FLoggedError:=Original;
    if TOPLogger(GetLogger).IsKindActive(lkError) then
      TOPLogger(GetLogger).LogError(Self, '%s failed: %s: %s',
        [aOperation, Original.ClassName, Exception(Original).Message]);
  end;
  {$ENDIF}
end;

procedure TOPManager.RaiseOperationError(const aOperation, aFmt: string; const aArgs: array of const);
begin
  try
    RaiseError(aOperation, aFmt, aArgs);
  except
    LogOperationFailure(aOperation);
    raise;
  end;
end;

procedure TOPManager.RollbackAfterError(const aOperation: string);
var
  Original: TObject;
begin
  Original:=ExceptObject;
  LogOperationFailure(aOperation);
  if not Assigned(FTransactionHandler) then
    Exit;
  try
    Rollback;
  except
    on E: Exception do
    begin
      {$IFNDEF OPF_SKIP_LOGGING}
      TOPLogger(GetLogger).LogWarning(Self, '%s: rollback after error failed: %s',
        [aOperation, E.Message]);
      {$ENDIF}
      if Original is Exception then
        Exception(Original).Message:=Exception(Original).Message+LineEnding+E.Message;
    end;
  end;
end;

function TOPManager.InTransaction: Boolean;
begin
  Result:=Assigned(FTransactionHandler);
end;

procedure TOPManager.DoSaveList(aList: TObject; aManipulator: TOPListManipulator);
var
  I: Integer;
  Item: TObject;
  IPS: IOPPersistenceState;
begin
  // One operation for the whole list: when an item fails, the items saved
  // before it get back their state (new, no generated key) as well.
  BeginUndoOperation;
  BeginTransaction;
  try
    for I := 0 to aManipulator.ItemCount(aList) - 1 do
    begin
      Item := aManipulator.GetItem(aList, I);
      if Supports(Item, IOPPersistenceState, IPS) then
      begin
        case IPS.GetPersistenceState of
          opsNew, opsUpdate:
            Save(Item, IPS.GetPersistenceState);
          opsDelete:
            Delete(Item);
        end;
      end;
    end;
    Commit;
  except
    EndUndoOperation(True);
    RollbackAfterError('Save');
    raise;
  end;
  EndUndoOperation(False);
end;

procedure TOPManager.Save(aObject: TObject);

Var
  IPS : IOPPersistenceState;
  Manipulator: TOPListManipulator;
begin
  if aObject=Nil then
    RaiseError('Save', SErrExecutorNilObject);
  { Check if aObject is a registered list - if so, save all items }
  Manipulator:=OPListRegistry.FindManipulator(aObject.ClassType);
  if Assigned(Manipulator) then
  begin
    {$IFNDEF OPF_SKIP_LOGGING}
    if TOPLogger(GetLogger).IsKindActive(lkTrace) then
      TOPLogger(GetLogger).LogTrace(Self, 'Save: begin (list %s, %d items)', [aObject.ClassName, Manipulator.ItemCount(aObject)]);
    {$ENDIF}
    DoSaveList(aObject, Manipulator);
    {$IFNDEF OPF_SKIP_LOGGING}
    if TOPLogger(GetLogger).IsKindActive(lkTrace) then
      TOPLogger(GetLogger).LogTrace(Self, 'Save: end (list)');
    {$ENDIF}
    Exit;
  end;
  { Single object save }
  if not Supports(aObject, IOPPersistenceState, IPS) then
    RaiseError('Save', SErrNoStateSupport, [aObject.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Save: begin (%s)', [aObject.ClassName]);
  {$ENDIF}
  // The undo operation spans the real COMMIT: when it fails, the object
  // gets back its state, also when it cannot be observed
  BeginUndoOperation;
  BeginTransaction;
  try
    Save(aObject,IPS.GetPersistenceState);
    Commit;
  except
    EndUndoOperation(True);
    RollbackAfterError('Save');
    raise;
  end;
  EndUndoOperation(False);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Save: end');
  {$ENDIF}
end;

procedure TOPManager.Save(aObject: TObject; aState: TObjectPersistenceState);
var
  Def: Opf.Registry.TPersisterDef;
  Persister: TOPPersister;
begin
  Def:=Registry.FindPersister(aObject.ClassType);
  if Def=Nil then
    RaiseError('Save', SErrNoPersisterForClass, [aObject.ClassName]);
  Persister:=CreatePersister(Def, 'Save');
  try
    // See Save(aObject): the undo operation spans the real COMMIT
    BeginUndoOperation;
    BeginTransaction;
    try
      Save(aObject, aState, Persister);
      Commit;
    except
      EndUndoOperation(True);
      RollbackAfterError('Save');
      raise;
    end;
    EndUndoOperation(False);
  finally
    Persister.Free;
  end;
end;

{ A new field map with the key fields of aFieldMap only. Caller frees. }

function KeyFieldMap(aFieldMap: TOPFieldMapList): TOPFieldMapList;
var
  I: Integer;
begin
  Result:=TOPFieldMapList.Create(TOPFieldMapClass(aFieldMap.ItemClass));
  try
    for I:=0 to aFieldMap.Count-1 do
      if foInKey in aFieldMap[I].Options then
        Result.Add.Assign(aFieldMap[I]);
  except
    Result.Free;
    raise;
  end;
end;

{ Property names of the audit fields an update sets }

function UpdateAuditFieldNames(aFieldMap: TOPFieldMapList): TUTF8StringArray;
var
  I: Integer;
begin
  Result:=Nil;
  for I:=0 to aFieldMap.Count-1 do
    if ([foUpdatedAt, foUpdatedBy]*aFieldMap[I].Options)<>[] then
      Result:=Concat(Result, [aFieldMap[I].PropertyName]);
end;

function HasUpdatableField(aFieldMap: TOPFieldMapList): Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I:=0 to aFieldMap.Count-1 do
    if foInUpdate in aFieldMap[I].Options then
      Exit(True);
end;

function HasAutoIncField(aFieldMap: TOPFieldMapList): Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I:=0 to aFieldMap.Count-1 do
    if foAutoInc in aFieldMap[I].Options then
      Exit(True);
end;

function HasGeneratedField(aFieldMap: TOPFieldMapList): Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I:=0 to aFieldMap.Count-1 do
    if (foUUID in aFieldMap[I].Options) or (aFieldMap[I].SequenceName <> '') or Assigned(aFieldMap[I].OnGenerateValue) then
      Exit(True);
end;

procedure TOPManager.BeginUndoOperation;
begin
  if FUndoLog=Nil then
    FUndoLog:=TOPUndoLog.Create;
  TOPUndoLog(FUndoLog).BeginOperation;
end;

procedure TOPManager.EndUndoOperation(aFailed: Boolean);
begin
  if Assigned(FUndoLog) then
    TOPUndoLog(FUndoLog).EndOperation(aFailed);
end;

procedure TOPManager.Save(aObject: TObject; aState: TObjectPersistenceState; aPersister: TOPPersister);
begin
  BeginUndoOperation;
  // A transaction of its own (nested in the caller's when there is one):
  // otherwise the change would never be committed
  BeginTransaction;
  try
    DoSave(aObject, aState, aPersister);
    Commit;
  except
    // The object gets back what the manager wrote into it
    EndUndoOperation(True);
    RollbackAfterError('Save');
    raise;
  end;
  EndUndoOperation(False);
end;

{ Version value of an object, before it is changed }

function ReadVersion(aObj: TObject; aVersionField: TOPFieldMap): TOPVersionValue;
begin
  Result.Kind := aVersionField.PropertyKind;
  case Result.Kind of
    tkInteger:
      Result.IntValue := GetOrdProp(aObj, aVersionField.PropertyName);
    tkInt64:
      Result.IntValue := GetInt64Prop(aObj, aVersionField.PropertyName);
    tkSString, tkAString:
      Result.StrValue := GetStrProp(aObj, aVersionField.PropertyName);
    tkUString, tkWString:
      Result.StrValue := UTF8Encode(GetUnicodeStrProp(aObj, aVersionField.PropertyName));
  end;
end;

procedure SetVersionParam(aParam: TParam; const aVersion: TOPVersionValue);
begin
  case aVersion.Kind of
    tkInteger:
      aParam.AsInteger := aVersion.IntValue;
    tkInt64:
      aParam.AsLargeInt := aVersion.IntValue;
    tkSString, tkAString, tkUString, tkWString:
      aParam.AsString := aVersion.StrValue;
  end;
end;

procedure TOPManager.DoSave(aObject: TObject; aState: TObjectPersistenceState; aPersister: TOPPersister);

  procedure ApplyInitialVersion(aObj: TObject; aVersionField: TOPFieldMap);
  var
    GUID: TGUID;
    UUIDStr: String;
  begin
    case aVersionField.PropertyKind of
      tkInteger:
        if GetOrdProp(aObj, aVersionField.PropertyName) = 0 then
          SetOrdProp(aObj, aVersionField.PropertyName, 1);
      tkInt64:
        if GetInt64Prop(aObj, aVersionField.PropertyName) = 0 then
          SetInt64Prop(aObj, aVersionField.PropertyName, 1);
      tkSString, tkAString:
        if GetStrProp(aObj, aVersionField.PropertyName) = '' then
        begin
          CreateGUID(GUID);
          UUIDStr := LowerCase(GUIDToString(GUID));
          UUIDStr := Copy(UUIDStr, 2, Length(UUIDStr) - 2);
          SetStrProp(aObj, aVersionField.PropertyName, UUIDStr);
        end;
      tkUString, tkWString:
        if GetUnicodeStrProp(aObj, aVersionField.PropertyName) = '' then
        begin
          CreateGUID(GUID);
          UUIDStr := LowerCase(GUIDToString(GUID));
          UUIDStr := Copy(UUIDStr, 2, Length(UUIDStr) - 2);
          SetUnicodeStrProp(aObj, aVersionField.PropertyName, UnicodeString(UUIDStr));
        end;
    end;
  end;

  procedure IncrementVersion(aObj: TObject; aVersionField: TOPFieldMap);
  var
    GUID: TGUID;
    UUIDStr: String;
  begin
    case aVersionField.PropertyKind of
      tkInteger:
        SetOrdProp(aObj, aVersionField.PropertyName,
          GetOrdProp(aObj, aVersionField.PropertyName) + 1);
      tkInt64:
        SetInt64Prop(aObj, aVersionField.PropertyName,
          GetInt64Prop(aObj, aVersionField.PropertyName) + 1);
      tkSString, tkAString, tkUString, tkWString:
        begin
          CreateGUID(GUID);
          UUIDStr := LowerCase(GUIDToString(GUID));
          UUIDStr := Copy(UUIDStr, 2, Length(UUIDStr) - 2);
          if aVersionField.PropertyKind in [tkUString, tkWString] then
            SetUnicodeStrProp(aObj, aVersionField.PropertyName, UnicodeString(UUIDStr))
          else
            SetStrProp(aObj, aVersionField.PropertyName, UUIDStr);
        end;
    end;
  end;

  procedure ExecuteVersionedUpdate(aObj: TObject; aPers: TOPPersister;
    aFieldMap: TOPFieldMapList; const aSQL: UTF8String;
    aConn: TSQLConnection; const aOldVersion: TOPVersionValue);
  var
    Query: TSQLQuery;
    I: Integer;
    Field: TOPFieldMap;
    Param: TParam;
  begin
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := aConn;
      Query.Transaction := aConn.Transaction;
      Query.SQL.Text := aSQL;
      for I := 0 to aFieldMap.Count - 1 do
      begin
        Field := aFieldMap[I];
        Param := Query.Params.FindParam(SQLBuilder.FieldParamName(Field));
        if Param <> nil then
          aPers.PersistProperty(aObj, Field, Param);
      end;
      Param := Query.Params.FindParam('opf_oldversion');
      if Param <> nil then
        SetVersionParam(Param, aOldVersion);
      try
        Query.ExecSQL;
      except
        on E: EDatabaseError do
          RaiseDBError('Save', SErrExecutorExecFailed, [E.Message], E);
      end;
      if Query.RowsAffected = 0 then
        RaiseError('Save', SErrConcurrencyConflict, [aObj.ClassName]);
    finally
      Query.Free;
    end;
  end;

var
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  SQL: UTF8String;
  IPS: IOPPersistenceState;
  lTracked: Boolean;
  lNames: TUTF8StringArray;
  lMap: TOPFieldMapList;
  SkippedUpdate: Boolean;
  VersionField: TOPFieldMap;
  OldVersion: TOPVersionValue;
begin
  Def:=Registry.FindPersister(aObject.ClassType);
  if Def=Nil then
    RaiseError('Save', SErrNoPersisterForClass, [aObject.ClassName]);
  Conn:=ConnectionFor(aObject.ClassType);
  // Before anything is written into the object (audit fields, version, keys)
  RecordUndo(aObject, Def.FieldMap, Conn);
  RegisterConnectionForTransaction(Conn);
  DoBeforeSave(aObject, aState);
  // Updates apply the audit fields after the change detection: they would
  // otherwise make every audited object look changed
  if aState<>opsUpdate then
    ApplyAuditFields(aObject, aState, Def.FieldMap);
  VersionField := Def.FieldMap.FindVersionField;
  SkippedUpdate := False;
  case aState of
    opsNew:
      begin
        if Assigned(VersionField) then
          ApplyInitialVersion(aObject, VersionField);
        SQL:=SQLBuilder.GetInsertSQL(Def.TableName, Def.FieldMap.AsArray);
        // Pass our builder: it generated SQL, so it decides the dialect specifics
        if HasAutoIncField(Def.FieldMap) then
          OPExecutor.ExecuteSQLAutoInc(aObject, aPersister, Def.FieldMap, SQL, Conn, SQLBuilder)
        else if HasGeneratedField(Def.FieldMap) then
          OPExecutor.ExecuteSQLWithGeneration(aObject, aPersister, Def.FieldMap, SQL, Conn, SQLBuilder)
        else
          OPExecutor.ExecuteSQL(aObject, aPersister, Def.FieldMap, SQL, Conn, SQLBuilder);
      end;
    opsUpdate:
      begin
        lTracked := GetObjectChanges(aObject, Def.FieldMap, lNames);
        if lTracked and (Length(lNames) = 0) then
          SkippedUpdate := True
        else
        begin
          ApplyAuditFields(aObject, aState, Def.FieldMap);
          // The audit fields just set are written too
          if lTracked then
            lNames := Concat(lNames, UpdateAuditFieldNames(Def.FieldMap));
          if lTracked then
            lMap := BuildChangedFieldMapList(Def.FieldMap, lNames)
          else
            lMap := Nil;
          try
            if lMap = Nil then
              lMap := Def.FieldMap;
            // Only fields that are not written by UPDATE changed (e.g. read-only
            // fields): there is nothing to write, the object is saved as is.
            if HasUpdatableField(lMap) then
            begin
              SQL:=SQLBuilder.GetUpdateSQL(Def.TableName, lMap.AsArray);
              if Assigned(VersionField) then
              begin
                OldVersion := ReadVersion(aObject, VersionField);
                IncrementVersion(aObject, VersionField);
                SQL := SQL + ' AND (' + SQLBuilder.Identifier(VersionField.FieldName) + '=:opf_oldversion)';
                ExecuteVersionedUpdate(aObject, aPersister, lMap, SQL, Conn,
                  OldVersion);
              end
              else
                OPExecutor.ExecuteSQL(aObject, aPersister, lMap, SQL, Conn, SQLBuilder);
            end;
          finally
            if lMap <> Def.FieldMap then
              lMap.Free;
          end;
        end;
      end;
  else
    RaiseError('Save', SErrUnsupportedPersistenceState, [Ord(aState)]);
  end;
  if not SkippedUpdate then
  begin
    DoAfterSave(aObject, aState);
    if Supports(aObject, IOPPersistenceState, IPS) then
      IPS.SetPersistenceState(opsClean);
    CaptureObjectSnapshot(aObject, Def.FieldMap);
  end;
end;

procedure TOPManager.SetRegistry(AValue: TOPRegistry);
begin
  if FRegistry=AValue then Exit;
  if Assigned(FRegistry) then
    FRegistry.RemoveFreeNotification(self);
  FRegistry:=AValue;
  if Assigned(FRegistry) then
    FRegistry.FreeNotification(self);
end;

procedure TOPManager.SetSQLBuilder(AValue: TOPSQLBuilder);
begin
  if FSQLBuilder=AValue then Exit;
  if Assigned(FSQLBuilder) then
    FSQLBuilder.RemoveFreeNotification(self);
  FSQLBuilder:=AValue;
  if Assigned(FSQLBuilder) then
    FSQLBuilder.FreeNotification(self);
end;

function TOPManager.CreateConnections: TOPConnectionList;
begin
  Result:=TOPConnectionList.Create(TOPConnection);
end;

function TOPManager.GetUnsafeCriteriaPatterns: TUTF8StringArray;
begin
  Result:=TUTF8StringArray.Create(';','--','/*');
end;

procedure TOPManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if aComponent=FRegistry then
      FRegistry:=Nil
    else if aComponent=FSQLBuilder then
      FSQLBuilder:=Nil
    else if aComponent=FM2MRegistry then
      FM2MRegistry:=Nil
    else if aComponent=FRelationRegistry then
      FRelationRegistry:=Nil
    else if aComponent=FSchemaMigrator then
    begin
      FSchemaMigrator:=TOPSchemaMigrator.Create(Self);
      FOwnsSchemaMigrator:=True;
    end
    else if aComponent=FChangeTrackerOwner then
    begin
      FChangeTracker:=Nil;
      FChangeTrackerOwner:=Nil;
    end;
  end;
end;

procedure TOPManager.AttachChangeTracker(aProvider: TComponent);
var
  lTracker: IOPChangeTracker;
begin
  if FChangeTrackerOwner = aProvider then
    Exit;
  if Assigned(FChangeTrackerOwner) then
    FChangeTrackerOwner.RemoveFreeNotification(Self);
  FChangeTrackerOwner := aProvider;
  if Assigned(aProvider) then
  begin
    aProvider.FreeNotification(Self);
    if aProvider.GetInterface(SGUIDIOPChangeTracker, lTracker) then
      FChangeTracker := lTracker
    else
      FChangeTracker := nil;
  end
  else
    FChangeTracker := nil;
end;

procedure TOPManager.DetachChangeTracker;
begin
  if Assigned(FChangeTrackerOwner) then
    FChangeTrackerOwner.RemoveFreeNotification(Self);
  FChangeTracker := nil;
  FChangeTrackerOwner := nil;
end;

function TOPManager.GetDirtyFields(aObject: TObject; aFields: TStrings): Boolean;

var
  lNames: TUTF8StringArray;
  lIndex: Integer;

begin
  if not Assigned(aFields) then
    Exit(False);
  Result:=GetDirtyFields(aObject,lNames);
  if not Result then
    Exit;
  for lIndex:=0 to Length(lNames)-1 do
    aFields.Add(lNames[lIndex]);
end;


function TOPManager.GetDirtyFields(aObject: TObject; out aFields: TUTF8StringArray): Boolean;

var
  lDef: Opf.Registry.TPersisterDef;

begin
  aFields:=Nil;
  if not Assigned(aObject) then
    Exit(False);
  lDef:=Registry.FindPersister(aObject.ClassType);
  Result:=Assigned(lDef) and Assigned(lDef.FieldMap)
          and GetObjectChanges(aObject,lDef.FieldMap,aFields);
end;

constructor TOPManager.Create(aOwner : TComponent);
begin
  inherited;
  FConnections:=CreateConnections;
  FAuditInfo:=TOPAuditInfo.Create;
  FAuditInfo.FManager:=Self;
  FSchemaMigrator:=TOPSchemaMigrator.Create(Self);
  FOwnsSchemaMigrator:=True;
  FDeleteChecks:=[dckVersion, dckRowsAffected];
  FOptions:=DefaultManagerOptions;
  {$IFNDEF VER3_2}
  FLazyBindings:=TOPPointerMap.Create;
  {$ENDIF}
end;

destructor TOPManager.Destroy;
{$IFNDEF VER3_2}
var
  I: Integer;
  B: TOPLazyBinding;
  Bindings: TOPPointerMap;
{$ENDIF}
begin
  {$IFNDEF VER3_2}
  if Assigned(FLazyBindings) then
  begin
    // The lazy records own their bindings: only detach them from this manager
    Bindings:=TOPPointerMap(FLazyBindings);
    for I := 0 to Bindings.Capacity - 1 do
    begin
      B:=TOPLazyBinding(Bindings.KeyAt(I));
      if B<>Nil then
        B.FManager:=Nil;
    end;
    FreeAndNil(FLazyBindings);
  end;
  {$ENDIF}
  if omoHookSQLDBLog in FOptions then
    UnhookSQLDBLog;
  if FOwnsSchemaMigrator then
    FreeAndNil(FSchemaMigrator);
  FreeAndNil(FTransactionHandler);
  // Not restored: the objects may already be gone
  FreeAndNil(FUndoLog);
  FreeAndNil(FConnections);
  FreeAndNil(FAuditInfo);
  inherited Destroy;
end;

class destructor TOPManager.Cleanup;
begin
  ClearInstance;
end;

class procedure TOPManager.SetInstanceClass(aClass: TOPManagerClass);
begin
  if Assigned(_Instance) then
     _Instance.RaiseError('SetInstanceClass', SErrManagerCreated);
  _InstanceClass:=aClass;
end;

procedure TOPManager.SetConnections(AValue: TOPConnectionList);
begin
  if FConnections=AValue then Exit;
  FConnections.Assign(AValue);
end;

function TOPManager.GetM2MRegistry: TOPManyToManyRegistry;
begin
  Result := FM2MRegistry;
  if Result = nil then
    Result := TOPManyToManyRegistry.Instance;
end;

function TOPManager.GetRegistry: TOPRegistry;
begin
  Result := FRegistry;
  if Result = nil then
    Result := TOPRegistry.Instance;
end;

function TOPManager.GetRelationRegistry: TOPRelationRegistry;
begin
  Result := FRelationRegistry;
  if Result = nil then
    Result := TOPRelationRegistry.Instance;
end;

function TOPManager.GetSQLBuilder: TOPSQLBuilder;
begin
  Result := FSQLBuilder;
  if Result = nil then
    Result := TOPSQLBuilder.Instance;
end;

procedure TOPManager.SetM2MRegistry(AValue: TOPManyToManyRegistry);
begin
  if FM2MRegistry = AValue then Exit;
  if Assigned(FM2MRegistry) then
    FM2MRegistry.RemoveFreeNotification(Self);
  FM2MRegistry := AValue;
  if Assigned(FM2MRegistry) then
    FM2MRegistry.FreeNotification(Self);
end;

procedure TOPManager.SetRelationRegistry(AValue: TOPRelationRegistry);
begin
  if FRelationRegistry = AValue then Exit;
  if Assigned(FRelationRegistry) then
    FRelationRegistry.RemoveFreeNotification(Self);
  FRelationRegistry := AValue;
  if Assigned(FRelationRegistry) then
    FRelationRegistry.FreeNotification(Self);
end;

class function TOPManager.Instance: TOPManager;
begin
  if (_Instance=Nil) then
    begin
    if _InstanceClass=Nil then
      _InstanceClass:=TOPManager;
    _Instance:=_InstanceClass.Create(Nil);
    end;
  Result:=_Instance;
end;

class procedure TOPManager.ClearInstance;
begin
  FreeAndNil(_Instance);
end;

{ Value that marks a row as soft-deleted }

procedure TOPManager.SetSoftDeleteParam(aParam: TParam; aField: TOPFieldMap; aTimestamp: TDateTime);
begin
  case aField.PropertyKind of
    tkBool:
      aParam.AsBoolean:=True;
    tkFloat:
      OPFSetFloatParam(aParam, aField, aTimestamp);
  end;
end;

procedure TOPManager.ExecCascadeSQL(const aSQL: UTF8String; aParamValue: Variant;
  aSoftDeleteField: TOPFieldMap; aConn: TSQLConnection; aChildClass, aParentClass: TClass);
var
  Q: TSQLQuery;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConn;
    Q.Transaction:=aConn.Transaction;
    Q.SQL.Text:=aSQL;
    Q.Params.ParamByName('opf_pk').Value:=aParamValue;
    if Assigned(aSoftDeleteField) then
      SetSoftDeleteParam(Q.Params.ParamByName('opf_sd'), aSoftDeleteField, GetTimestamp);
    try
      Q.ExecSQL;
    except
      on E: EDatabaseError do
        RaiseDBError('Delete', SErrCascadeDeleteFailed,
          [aChildClass.ClassName, aParentClass.ClassName, E.Message], E);
    end;
  finally
    Q.Free;
  end;
end;

{ Key values of the rows returned by aSQL (one column, parameter opf_pk) }

function TOPManager.SelectCascadeKeys(const aSQL: UTF8String; aParamValue: Variant;
  aConn: TSQLConnection): TOPVariantArray;
var
  Q: TSQLQuery;
  N: Integer;
begin
  Result:=Nil;
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConn;
    Q.Transaction:=aConn.Transaction;
    Q.SQL.Text:=aSQL;
    Q.Params.ParamByName('opf_pk').Value:=aParamValue;
    Q.Open;
    N:=0;
    while not Q.EOF do
    begin
      SetLength(Result, N+1);
      Result[N]:=Q.Fields[0].Value;
      Inc(N);
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

{ Delete (or soft delete, aSoft) the children of the parent rows given by
  aFKWhereExpr, depth-first.
  - aFKWhereExpr is ':opf_pk' (IsSubQuery=False) or a subquery returning parent keys.
  - Soft delete only reaches children that have a soft-delete field; other
    children (and everything below them) are left alone, so the parent can be
    restored. Rows that are already soft-deleted keep their deletion value.
  - Hard delete removes all children, also those with a soft-delete field.
    Relations with DBCascade are left to the database; a soft delete follows
    them too (a database cascade does not fire on an UPDATE).
  - A relation of a class to itself (a tree) is followed row by row: the depth
    of the data is not known in advance. aVisitedKeys (case-sensitive) stops
    cycles in the data.
  - aVisited holds the classes on the current path: a cycle between different
    classes raises. }

procedure TOPManager.DeleteCascadeChildren(aParentClass: TClass;
  const aFKWhereExpr: UTF8String; aParamValue: Variant;
  IsSubQuery: Boolean; aConn: TSQLConnection; aVisited: TFPList;
  aSoft: Boolean; aVisitedKeys: TStrings);
var
  Relations: TOPRelationDefArray;
  I, K: Integer;
  Rel: TOPRelationDef;
  ChildDef: TPersisterDef;
  ChildPKField, ChildSDF: TOPFieldMap;
  ChildPKSubQuery, SQL, Where, NotDeleted: UTF8String;
  ChildTable, FKField, PKCol, KeyName: UTF8String;
  Keys: TOPVariantArray;
  Added: Boolean;
begin
  Added:=aVisited.IndexOf(aParentClass)<0;
  if not Added and not ((aVisited.Count>0) and (aVisited[aVisited.Count-1]=Pointer(aParentClass))) then
    RaiseError('Delete', SErrCascadeCycleDetected, [aParentClass.ClassName]);
  if Added then
    aVisited.Add(aParentClass);
  try
  Relations:=RelationRegistry.FindChildRelations(aParentClass);
  for I:=0 to High(Relations) do
  begin
    Rel:=Relations[I];
    // ON DELETE CASCADE does not fire on the UPDATE of a soft delete, so a
    // soft delete also follows relations the database cascades
    if Rel.DBCascade and not aSoft then
      Continue;
    ChildDef:=Registry.FindPersister(Rel.ChildClass);
    if ChildDef=Nil then
      RaiseError('Delete', SErrNoPersisterForClass, [Rel.ChildClass.ClassName]);
    ChildSDF:=ChildDef.FieldMap.FindSoftDeleteField;
    // A soft delete leaves children that cannot be soft-deleted alone
    if aSoft and not Assigned(ChildSDF) then
      Continue;
    if not aSoft then
      ChildSDF:=Nil;
    ChildTable:=SQLBuilder.Identifier(ChildDef.TableName, itTable);
    FKField:=SQLBuilder.Identifier(Rel.ForeignKeyFieldName);
    if IsSubQuery then
      Where:=Format('%s IN (%s)', [FKField, aFKWhereExpr])
    else
      Where:=Format('%s = %s', [FKField, aFKWhereExpr]);
    NotDeleted:='';
    if Assigned(ChildSDF) then
      NotDeleted:=GetSoftDeleteFilter(ChildDef.FieldMap);
    ChildPKField:=ChildDef.FieldMap.FindPrimaryKeyField;
    if not Assigned(ChildPKField) then
    begin
      if Length(RelationRegistry.FindChildRelations(Rel.ChildClass)) > 0 then
        RaiseError('Delete', SErrCascadeChildNoPK, [Rel.ChildClass.ClassName]);
    end
    else
    begin
      PKCol:=SQLBuilder.Identifier(ChildPKField.FieldName);
      if Rel.ChildClass=aParentClass then
      begin
        // A tree: follow the rows one by one, deepest first, and delete each by key
        Keys:=SelectCascadeKeys(Format('SELECT %s FROM %s WHERE %s', [PKCol, ChildTable, Where]),
          aParamValue, aConn);
        for K:=0 to High(Keys) do
        begin
          KeyName:=Rel.ChildClass.ClassName+'#'+VarToStr(Keys[K]);
          if aVisitedKeys.IndexOf(KeyName)>=0 then
            Continue;
          aVisitedKeys.Add(KeyName);
          DeleteCascadeChildren(Rel.ChildClass, ':opf_pk', Keys[K], False, aConn, aVisited, aSoft, aVisitedKeys);
          if Assigned(ChildSDF) then
            SQL:=Format('UPDATE %s SET %s = :opf_sd WHERE %s',
              [ChildTable, SQLBuilder.Identifier(ChildSDF.FieldName),
               CombineWhere([PKCol+' = :opf_pk', NotDeleted])])
          else
            SQL:=Format('DELETE FROM %s WHERE %s = :opf_pk', [ChildTable, PKCol]);
          ExecCascadeSQL(SQL, Keys[K], ChildSDF, aConn, Rel.ChildClass, aParentClass);
        end;
        Continue;
      end;
      // Children of these children first
      ChildPKSubQuery:=Format('SELECT %s FROM %s WHERE %s', [PKCol, ChildTable, Where]);
      DeleteCascadeChildren(Rel.ChildClass, ChildPKSubQuery, aParamValue, True, aConn, aVisited, aSoft, aVisitedKeys);
    end;
    // Delete or soft-delete the children at this level
    if Assigned(ChildSDF) then
      SQL:=Format('UPDATE %s SET %s = :opf_sd WHERE %s',
        [ChildTable, SQLBuilder.Identifier(ChildSDF.FieldName), CombineWhere([Where, NotDeleted])])
    else
      SQL:=Format('DELETE FROM %s WHERE %s', [ChildTable, Where]);
    ExecCascadeSQL(SQL, aParamValue, ChildSDF, aConn, Rel.ChildClass, aParentClass);
  end;
  finally
    if Added then
      aVisited.Delete(aVisited.IndexOf(aParentClass));
  end;
end;

{ True if deleting an object of aClass needs to cascade with its key value:
  child relations or many-to-many definitions that the database does not
  handle itself. A soft delete follows every child relation. }

function TOPManager.NeedsKeyCascade(aClass: TClass; aSoft: Boolean): Boolean;
var
  Rels: TOPRelationDefArray;
  Defs: TOPManyToManyDefArray;
  I: Integer;
begin
  Result:=False;
  Rels:=RelationRegistry.FindChildRelations(aClass);
  for I:=0 to High(Rels) do
    if aSoft or not Rels[I].DBCascade then
      Exit(True);
  Defs:=M2MRegistry.FindDefsForClass(aClass);
  for I:=0 to High(Defs) do
    if not Defs[I].DBCascade then
      Exit(True);
end;

{ True if the row of aObject is there, not soft-deleted, with the version
  aObject holds }

function TOPManager.RowWithVersionExists(aDef: TPersisterDef; aObject: TObject;
  aPersister: TOPPersister; aConn: TSQLConnection; aVersionField: TOPFieldMap): Boolean;
var
  Q: TSQLQuery;
  I: Integer;
  Field: TOPFieldMap;
  Param: TParam;
begin
  Q:=TSQLQuery.Create(Nil);
  try
    Q.Database:=aConn;
    Q.Transaction:=aConn.Transaction;
    Q.SQL.Text:='SELECT 1 FROM '+SQLBuilder.Identifier(aDef.TableName, itTable)+' WHERE '+
      CombineWhere([SQLBuilder.GetWhereKey(aDef.TableName, aDef.FieldMap.AsArray),
                    SQLBuilder.Identifier(aVersionField.FieldName)+'=:opf_oldversion',
                    GetSoftDeleteFilter(aDef.FieldMap)]);
    SetVersionParam(Q.Params.ParamByName('opf_oldversion'), ReadVersion(aObject, aVersionField));
    for I:=0 to aDef.FieldMap.Count-1 do
    begin
      Field:=aDef.FieldMap[I];
      if not (foInKey in Field.Options) then
        Continue;
      Param:=Q.Params.FindParam(SQLBuilder.FieldParamName(Field));
      if Param<>Nil then
        aPersister.PersistProperty(aObject, Field, Param);
    end;
    try
      Q.Open;
    except
      on E: EDatabaseError do
        RaiseDBError('Delete', SErrExecutorExecFailed, [E.Message], E);
    end;
    Result:=not Q.EOF;
    Q.Close;
  finally
    Q.Free;
  end;
end;

procedure TOPManager.Delete(aObject: TObject);
var
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  SQL, NotDeleted: UTF8String;
  IPS: IOPPersistenceState;
  Persister: TOPPersister;
  PKField, SoftDeleteField, VersionField, Field: TOPFieldMap;
  PKParam, Param: TParam;
  PKValue: Variant;
  Visited: TFPList;
  VisitedKeys: TStringList;
  IsSoftDelete, CheckVersion, StaleVersion: Boolean;
  Q: TSQLQuery;
  SDTimestamp: TDateTime;
  I, KeyCount: Integer;
begin
  if aObject=Nil then
    RaiseError('Delete', SErrExecutorNilObject);
  if not Supports(aObject, IOPPersistenceState, IPS) then
    RaiseError('Delete', SErrNoStateSupport, [aObject.ClassName]);
  Def:=Registry.FindPersister(aObject.ClassType);
  if Def=Nil then
    RaiseError('Delete', SErrNoPersisterForClass, [aObject.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Delete: begin (%s)', [aObject.ClassName]);
  {$ENDIF}
  // Cascades pass one key value: refuse before anything is changed
  KeyCount:=0;
  for I:=0 to Def.FieldMap.Count-1 do
    if foInKey in Def.FieldMap[I].Options then
      Inc(KeyCount);
  SoftDeleteField:=Def.FieldMap.FindSoftDeleteField;
  IsSoftDelete:=Assigned(SoftDeleteField);
  if (KeyCount>1) and NeedsKeyCascade(aObject.ClassType, IsSoftDelete) then
    RaiseError('Delete', SErrCascadeCompositeKey, [aObject.ClassName]);
  Conn:=ConnectionFor(aObject.ClassType);
  VersionField:=Def.FieldMap.FindVersionField;
  CheckVersion:=(dckVersion in FDeleteChecks) and Assigned(VersionField);
  if IsSoftDelete then
  begin
    SQL:=SQLBuilder.GetSoftDeleteSQL(Def.TableName, Def.FieldMap.AsArray, SoftDeleteField);
    // An already deleted row keeps its deletion value
    NotDeleted:=GetSoftDeleteFilter(Def.FieldMap);
    if NotDeleted<>'' then
      SQL:=SQL+' AND ('+NotDeleted+')';
  end
  else
    SQL:=SQLBuilder.GetDeleteSQL(Def.TableName, Def.FieldMap.AsArray);
  if CheckVersion then
    SQL:=SQL+' AND ('+SQLBuilder.Identifier(VersionField.FieldName)+'=:opf_oldversion)';
  Persister:=CreatePersister(Def, 'Delete');
  try
    BeginUndoOperation;
    BeginTransaction;
    try
      RecordUndo(aObject, Def.FieldMap, Conn);
      RegisterConnectionForTransaction(Conn);
      DoBeforeDelete(aObject);
      // Extract parent PK value
      PKField:=Def.FieldMap.FindPrimaryKeyField;
      // dckVersion without dckRowsAffected: a stale delete does nothing and
      // raises nothing. Check the version before cascading, or the children
      // would be deleted while the parent stays.
      StaleVersion:=CheckVersion and not (dckRowsAffected in FDeleteChecks)
        and Assigned(PKField) and (KeyCount=1)
        and NeedsKeyCascade(aObject.ClassType, IsSoftDelete)
        and not RowWithVersionExists(Def, aObject, Persister, Conn, VersionField);
      if Assigned(PKField) and (KeyCount=1) and not StaleVersion then
      begin
        PKParam:=TParam.Create(nil);
        try
          Persister.PersistProperty(aObject, PKField, PKParam);
          PKValue:=PKParam.Value;
        finally
          PKParam.Free;
        end;
        // Cascade delete children depth-first
        Visited:=TFPList.Create;
        VisitedKeys:=TStringList.Create;
        try
          // String keys 'ab' and 'AB' are different rows
          VisitedKeys.CaseSensitive:=True;
          VisitedKeys.Sorted:=True;
          DeleteCascadeChildren(aObject.ClassType, ':opf_pk', PKValue, False, Conn, Visited,
            IsSoftDelete, VisitedKeys);
        finally
          VisitedKeys.Free;
          Visited.Free;
        end;
        // Delete M2M junction rows - skip for soft delete (preserve for restorability)
        if not IsSoftDelete then
          DeleteM2MJunctionRows(aObject.ClassType, PKValue);
      end;
      Q:=TSQLQuery.Create(Nil);
      try
        Q.Database:=Conn;
        Q.Transaction:=Conn.Transaction;
        Q.SQL.Text:=SQL;
        // All key fields: a composite key binds every column
        for I:=0 to Def.FieldMap.Count-1 do
        begin
          Field:=Def.FieldMap[I];
          if not (foInKey in Field.Options) then
            Continue;
          Param:=Q.Params.FindParam(SQLBuilder.FieldParamName(Field));
          if Param<>Nil then
            Persister.PersistProperty(aObject, Field, Param);
        end;
        if CheckVersion then
          SetVersionParam(Q.Params.ParamByName('opf_oldversion'), ReadVersion(aObject, VersionField));
        SDTimestamp:=GetTimestamp;
        if IsSoftDelete then
          SetSoftDeleteParam(Q.Params.ParamByName('opf_sd'), SoftDeleteField, SDTimestamp);
        try
          Q.ExecSQL;
        except
          on E: EDatabaseError do
            RaiseDBError('Delete', SErrExecutorExecFailed, [E.Message], E);
        end;
        if (dckRowsAffected in FDeleteChecks) and (Q.RowsAffected=0) then
        begin
          if Assigned(VersionField) and CheckVersion then
            RaiseError('Delete', SErrConcurrencyConflict, [aObject.ClassName])
          else
            RaiseError('Delete', SErrDeleteNotFound, [aObject.ClassName]);
        end;
      finally
        Q.Free;
      end;
      // Reflect the soft-delete value in the object
      if IsSoftDelete then
        case SoftDeleteField.PropertyKind of
          tkBool:
            SetOrdProp(aObject, SoftDeleteField.PropertyName, Ord(True));
          tkFloat:
            SetFloatProp(aObject, SoftDeleteField.PropertyName, SDTimestamp);
        end;
      DoAfterDelete(aObject);
      DiscardObjectSnapshot(aObject);
      Commit;
      IPS.SetPersistenceState(opsDeleted);
    except
      EndUndoOperation(True);
      RollbackAfterError('Delete');
      raise;
    end;
    EndUndoOperation(False);
  finally
    Persister.Free;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Delete: end');
  {$ENDIF}
end;

{ Every object read from the database: state clean, AfterLoad, lazy loaders,
  reference state for change tracking }

procedure TOPManager.FinishLoadedObject(aObject: TObject; aFieldMap: TOPFieldMapList);
var
  IPS: IOPPersistenceState;
begin
  if Supports(aObject, IOPPersistenceState, IPS) then
    IPS.SetPersistenceState(opsClean);
  DoAfterLoad(aObject);
  InitLazyFields(aObject);
  CaptureObjectSnapshot(aObject, aFieldMap);
end;

procedure TOPManager.FinishLoadedList(aList: TObject; aManipulator: TOPListManipulator;
  aFieldMap: TOPFieldMapList);
var
  I: Integer;
begin
  for I:=0 to aManipulator.ItemCount(aList)-1 do
    FinishLoadedObject(aManipulator.GetItem(aList, I), aFieldMap);
end;

procedure TOPManager.Load(aObject: TObject; KeyOnly: Boolean; aPersister: TOPPersister);
begin
  // Raised after the load's own transaction level was committed: a missing
  // row does not doom the caller's transaction
  if not TryLoad(aObject, KeyOnly, aPersister) then
    RaiseOperationError('Load', SErrLoadNotFound, [aObject.ClassName]);
end;

function TOPManager.TryLoad(aObject: TObject; KeyOnly: Boolean; aPersister: TOPPersister): Boolean;
var
  Map: TOPFieldMapList;
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  SQL: UTF8String;
  IPS: IOPPersistenceState;
  Persister: TOPPersister;
  OwnsPersister: Boolean;
  RowFound: Boolean;
begin
  Result:=False;
  if aObject=Nil then
    RaiseError('Load', SErrExecutorNilObject);
  if not Supports(aObject, IOPPersistenceState, IPS) then
    RaiseError('Load', SErrNoStateSupport, [aObject.ClassName]);
  Def:=Registry.FindPersister(aObject.ClassType);
  if Def=Nil then
    RaiseError('Load', SErrNoPersisterForClass, [aObject.ClassName]);
  Conn:=ConnectionFor(aObject.ClassType);
  // KeyOnly: only the key columns are read (an existence check); the object
  // gets state opsKey and no reference state
  Map:=Def.FieldMap;
  if KeyOnly then
    Map:=KeyFieldMap(Def.FieldMap);
  try
  SQL:=BuildKeySelectSQL(Def, Map);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Load: begin (%s)', [aObject.ClassName]);
  {$ENDIF}
  OwnsPersister:=aPersister=Nil;
  if OwnsPersister then
    Persister:=CreatePersister(Def, 'Load')
  else
    Persister:=aPersister;
  try
    BeginTransaction;
    try
      RegisterConnectionForTransaction(Conn);
      RowFound:=OPExecutor.QuerySQL(aObject, Persister, Map, SQL, Conn, SQLBuilder);
      if RowFound then
      begin
        if KeyOnly then
          IPS.SetPersistenceState(opsKey)
        else
          FinishLoadedObject(aObject, Def.FieldMap);
      end;
      Commit;
      Result:=RowFound;
    except
      RollbackAfterError('Load');
      raise;
    end;
  finally
    if OwnsPersister then
      Persister.Free;
  end;
  finally
    if Map<>Def.FieldMap then
      Map.Free;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Load: end');
  {$ENDIF}
end;

procedure TOPManager.Load(aList: TObject; aClass: TClass; const aCriteria: UTF8String);
var
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  SQL: UTF8String;
  P: UTF8String;
  Manipulator: TOPListManipulator;
  Persister: TOPPersister;
begin
  if aList=Nil then
    RaiseError('Load', SErrLoadNilList);
  if aClass=Nil then
    RaiseError('Load', SErrLoadNilClass);
  if aCriteria<>'' then
    for P in GetUnsafeCriteriaPatterns do
      if Pos(P,aCriteria)>0 then
        RaiseError('Load', SErrUnsafeCriteria, [P]);
  Def:=Registry.FindPersister(aClass);
  if Def=Nil then
    RaiseError('Load', SErrNoPersisterForClass, [aClass.ClassName]);
  Manipulator:=OPListRegistry.FindManipulator(aList.ClassType);
  if Manipulator=Nil then
    RaiseError('Load', SErrLoadNoManipulator, [aList.ClassName]);
  Conn:=ConnectionFor(aClass);
  SQL:=SQLBuilder.GetSelectSQL(Def.TableName, Def.FieldMap.AsArray);
  if (aCriteria='') or (omoRawCriteriaSoftDeleteFilter in FOptions) then
    P:=GetSoftDeleteFilter(Def.FieldMap)
  else
    P:='';
  if (aCriteria<>'') and (P<>'') then
    SQL:=SQL + ' WHERE (' + aCriteria + ') AND ' + P
  else if aCriteria<>'' then
    SQL:=SQL + ' WHERE (' + aCriteria + ')'
  else if P<>'' then
    SQL:=SQL + ' WHERE ' + P;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Load: begin (list %s)', [aClass.ClassName]);
  {$ENDIF}
  Persister:=CreatePersister(Def, 'Load');
  try
    BeginTransaction;
    try
      RegisterConnectionForTransaction(Conn);
      OPExecutor.QuerySQLList(aList, Manipulator, aClass, Persister, Def.FieldMap, SQL, Conn, SQLBuilder);
      FinishLoadedList(aList, Manipulator, Def.FieldMap);
      Commit;
    except
      RollbackAfterError('Load');
      raise;
    end;
  finally
    Persister.Free;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Load: end');
  {$ENDIF}
end;

procedure TOPManager.Load(aList: TObject; aClass: TClass; aCriteria: TOPCriteria);
var
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  SQL: UTF8String;
  Manipulator: TOPListManipulator;
  Persister: TOPPersister;
begin
  if aList=Nil then
    RaiseError('Load', SErrLoadNilList);
  if aClass=Nil then
    RaiseError('Load', SErrLoadNilClass);
  Def:=Registry.FindPersister(aClass);
  if Def=Nil then
    RaiseError('Load', SErrNoPersisterForClass, [aClass.ClassName]);
  Manipulator:=OPListRegistry.FindManipulator(aList.ClassType);
  if Manipulator=Nil then
    RaiseError('Load', SErrLoadNoManipulator, [aList.ClassName]);
  if aCriteria=Nil then
    RaiseError('Load', SErrLoadNilCriteria);
  // Limit(0): no rows, and not every dialect accepts a zero row limit
  if aCriteria.GetLimitValue=0 then
    Exit;
  Conn:=ConnectionFor(aClass);
  SQL:=BuildCriteriaSelectSQL(Def, aCriteria);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Load: begin (list criteria %s)', [aClass.ClassName]);
  {$ENDIF}
  Persister:=CreatePersister(Def, 'Load');
  try
    BeginTransaction;
    try
      RegisterConnectionForTransaction(Conn);
      OPExecutor.QueryCustomSQLList(aList, Manipulator, aClass, Persister, Def.FieldMap, SQL, Conn, aCriteria.Params, SQLBuilder);
      FinishLoadedList(aList, Manipulator, Def.FieldMap);
      Commit;
    except
      RollbackAfterError('Load');
      raise;
    end;
  finally
    Persister.Free;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'Load: end');
  {$ENDIF}
end;

class function TOPManager.ArgsToParams(const aArgs: array of const): TParams;
var
  I: Integer;
  ParamName: String;
begin
  if Length(aArgs) = 0 then
    Exit(nil);
  if (Length(aArgs) mod 2) <> 0 then
    raise EOPF.CreateFmt('TOPManager.ArgsToParams: %s', [SErrLoadSQLOddParamCount]);
  Result:=TParams.Create;
  try
    I:=0;
    while I <= High(aArgs) do
    begin
      // Even index (0,2,4,...): param name (must be string type)
      case aArgs[I].VType of
        vtAnsiString: ParamName:=AnsiString(aArgs[I].VAnsiString);
        vtString:     ParamName:=aArgs[I].VString^;
        vtChar:       ParamName:=aArgs[I].VChar;
        vtWideChar:   ParamName:=UTF8Encode(UnicodeString(aArgs[I].VWideChar));
        vtWideString: ParamName:=UTF8Encode(WideString(aArgs[I].VWideString));
        vtUnicodeString: ParamName:=UTF8Encode(UnicodeString(aArgs[I].VUnicodeString));
      else
        raise EOPF.CreateFmt('TOPManager.ArgsToParams: %s', [SErrLoadSQLParamNameNotString]);
      end;
      // Odd index (1,3,5,...): param value
      OPFAssignVarRec(aArgs[I+1], Result.CreateParam(ftUnknown, ParamName, ptInput));
      Inc(I, 2);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TOPManager.LoadSQL(aObject: TObject; const aSQL: UTF8String; aParams: TParams);
var
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  IPS: IOPPersistenceState;
  Persister: TOPPersister;
  RowFound: Boolean;
begin
  if aObject=Nil then
    RaiseError('LoadSQL', SErrExecutorNilObject);
  if not Supports(aObject, IOPPersistenceState, IPS) then
    RaiseError('LoadSQL', SErrNoStateSupport, [aObject.ClassName]);
  Def:=Registry.FindPersister(aObject.ClassType);
  if Def=Nil then
    RaiseError('LoadSQL', SErrNoPersisterForClass, [aObject.ClassName]);
  Conn:=ConnectionFor(aObject.ClassType);
  if aSQL='' then
    RaiseError('LoadSQL', SErrLoadSQLEmptySQL);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadSQL: begin (%s)', [aObject.ClassName]);
  {$ENDIF}
  RowFound:=False;
  Persister:=CreatePersister(Def, 'LoadSQL');
  try
    BeginTransaction;
    try
      RegisterConnectionForTransaction(Conn);
      RowFound:=OPExecutor.QueryCustomSQL(aObject, Persister, Def.FieldMap, aSQL, Conn, aParams, SQLBuilder);
      if RowFound then
        FinishLoadedObject(aObject, Def.FieldMap);
      Commit;
    except
      RollbackAfterError('LoadSQL');
      raise;
    end;
  finally
    Persister.Free;
  end;
  // After the commit of the load's own level: does not doom the caller's transaction
  if not RowFound then
    RaiseOperationError('LoadSQL', SErrLoadNotFound, [aObject.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadSQL: end');
  {$ENDIF}
end;

procedure TOPManager.LoadSQL(aObject: TObject; const aSQL: UTF8String; const aArgs: array of const);
var
  P: TParams;
begin
  P:=ArgsToParams(aArgs);
  try
    LoadSQL(aObject, aSQL, P);
  finally
    P.Free;
  end;
end;

procedure TOPManager.LoadSQL(aObject: TObject; const aSQL: UTF8String);
begin
  LoadSQL(aObject, aSQL, TParams(nil));
end;

procedure TOPManager.LoadSQL(aList: TObject; aClass: TClass; const aSQL: UTF8String; aParams: TParams);
var
  Def: Opf.Registry.TPersisterDef;
  Conn: TSQLConnection;
  Manipulator: TOPListManipulator;
  Persister: TOPPersister;
begin
  if aList=Nil then
    RaiseError('LoadSQL', SErrLoadNilList);
  if aClass=Nil then
    RaiseError('LoadSQL', SErrLoadNilClass);
  Def:=Registry.FindPersister(aClass);
  if Def=Nil then
    RaiseError('LoadSQL', SErrNoPersisterForClass, [aClass.ClassName]);
  Manipulator:=OPListRegistry.FindManipulator(aList.ClassType);
  if Manipulator=Nil then
    RaiseError('LoadSQL', SErrLoadNoManipulator, [aList.ClassName]);
  Conn:=ConnectionFor(aClass);
  if aSQL='' then
    RaiseError('LoadSQL', SErrLoadSQLEmptySQL);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadSQL: begin (list %s)', [aClass.ClassName]);
  {$ENDIF}
  Persister:=CreatePersister(Def, 'LoadSQL');
  try
    BeginTransaction;
    try
      RegisterConnectionForTransaction(Conn);
      OPExecutor.QueryCustomSQLList(aList, Manipulator, aClass, Persister, Def.FieldMap, aSQL, Conn, aParams, SQLBuilder);
      FinishLoadedList(aList, Manipulator, Def.FieldMap);
      Commit;
    except
      RollbackAfterError('LoadSQL');
      raise;
    end;
  finally
    Persister.Free;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadSQL: end');
  {$ENDIF}
end;

procedure TOPManager.LoadSQL(aList: TObject; aClass: TClass; const aSQL: UTF8String; const aArgs: array of const);
var
  P: TParams;
begin
  P:=ArgsToParams(aArgs);
  try
    LoadSQL(aList, aClass, aSQL, P);
  finally
    P.Free;
  end;
end;

procedure TOPManager.LoadSQL(aList: TObject; aClass: TClass; const aSQL: UTF8String);
begin
  LoadSQL(aList, aClass, aSQL, TParams(nil));
end;

function TOPManager.ResolveChildListPropertyName(aParentClass: TClass; aRel: TOPRelationDef): UTF8String;
var
  ChildName: UTF8String;
begin
  if aRel.ListPropertyName <> '' then
    Result:=aRel.ListPropertyName
  else
  begin
    ChildName:=aRel.ChildClass.ClassName;
    if (Length(ChildName) > 1) and (ChildName[1] = 'T') then
      ChildName:=Copy(ChildName, 2, Length(ChildName) - 1);
    Result:=ChildName + 'List';
  end;
end;

{ Definitions needed to load the children of aRel into aList. Raises for a
  configuration error, before anything is read. }

procedure TOPManager.CheckLoadChildren(aParent: TObject; aRel: TOPRelationDef; aList: TObject;
  out aChildDef, aParentDef: TPersisterDef; out aPKField: TOPFieldMap;
  out aManipulator: TOPListManipulator);
begin
  aChildDef:=Registry.FindPersister(aRel.ChildClass);
  if aChildDef=Nil then
    RaiseError('LoadChildren', SErrNoPersisterForClass, [aRel.ChildClass.ClassName]);
  aParentDef:=Registry.FindPersister(aParent.ClassType);
  if aParentDef=Nil then
    RaiseError('LoadChildren', SErrNoPersisterForClass, [aParent.ClassName]);
  aPKField:=aParentDef.FieldMap.FindPrimaryKeyField;
  if aPKField=Nil then
    RaiseError('LoadChildren', SErrLoadNoKeyFields, [aParent.ClassName]);
  aManipulator:=OPListRegistry.FindManipulator(aList.ClassType);
  if aManipulator=Nil then
    RaiseError('LoadChildren', SErrLoadNoManipulator, [aList.ClassName]);
end;

{ Configuration errors are raised before the transaction level is started, and
  only database errors doom the caller's transaction. }

procedure TOPManager.DoLoadChildren(aParent: TObject; aRel: TOPRelationDef; aList: TObject);
var
  Conn: TSQLConnection;
  ChildDef: Opf.Registry.TPersisterDef;
  ParentDef: Opf.Registry.TPersisterDef;
  Persister: TOPPersister;
  PKField: TOPFieldMap;
  Params: TParams;
  SQL: UTF8String;
  Manipulator: TOPListManipulator;
begin
  CheckLoadChildren(aParent, aRel, aList, ChildDef, ParentDef, PKField, Manipulator);
  Manipulator.ClearList(aList);
  Params:=CreateParentKeyParams(aParent, ParentDef, PKField);
  try
    // A parent without key value (not saved yet) has no children
    if Params[0].IsNull then
      Exit;
    SQL:=BuildChildrenSQL(ChildDef, aRel.ForeignKeyFieldName);
    Conn:=ConnectionFor(aRel.ChildClass);
    Persister:=CreatePersister(ChildDef, 'LoadChildren');
    try
      BeginTransaction;
      try
        RegisterConnectionForTransaction(Conn);
        OPExecutor.QueryCustomSQLList(aList, Manipulator, aRel.ChildClass, Persister, ChildDef.FieldMap,
          SQL, Conn, Params, SQLBuilder);
        FinishLoadedList(aList, Manipulator, ChildDef.FieldMap);
        Commit;
      except
        RollbackAfterError('LoadChildren');
        raise;
      end;
    finally
      Persister.Free;
    end;
  finally
    Params.Free;
  end;
end;

function TOPManager.ResolveM2MListPropertyName(aDef: TOPManyToManyDef; aAssociatedClass: TClass): UTF8String;
var
  AssocName: UTF8String;
begin
  if (aAssociatedClass = aDef.ClassB) and (aDef.ListPropertyNameA <> '') then
    Result:=aDef.ListPropertyNameA
  else if (aAssociatedClass = aDef.ClassA) and (aDef.ListPropertyNameB <> '') then
    Result:=aDef.ListPropertyNameB
  else
  begin
    AssocName:=aAssociatedClass.ClassName;
    if (Length(AssocName) > 1) and (AssocName[1] = 'T') then
      AssocName:=Copy(AssocName, 2, Length(AssocName) - 1);
    Result:=AssocName + 'List';
  end;
end;

{ True if aPropertyName is the list property of a many-to-many definition of aClass }

function TOPManager.IsM2MListProperty(aClass: TClass; const aPropertyName: UTF8String): Boolean;
var
  Defs: TOPManyToManyDefArray;
  I: Integer;
  Other: TClass;
begin
  Result:=False;
  Defs:=M2MRegistry.FindDefsForClass(aClass);
  for I:=0 to High(Defs) do
  begin
    if aClass = Defs[I].ClassA then
      Other:=Defs[I].ClassB
    else
      Other:=Defs[I].ClassA;
    if CompareText(ResolveM2MListPropertyName(Defs[I], Other), aPropertyName) = 0 then
      Exit(True);
  end;
end;

procedure TOPManager.DoLoadAssociations(aParent: TObject; aDef: TOPManyToManyDef; aAssociatedClass: TClass; aList: TObject);
var
  Conn: TSQLConnection;
  AssocDef: Opf.Registry.TPersisterDef;
  ParentDef: Opf.Registry.TPersisterDef;
  Persister: TOPPersister;
  PKField: TOPFieldMap;
  Params: TParams;
  FKParent, FKAssociated: UTF8String;
  SQL: UTF8String;
  Manipulator: TOPListManipulator;
begin
  if aParent = nil then
    RaiseError('LoadAssociations', SErrSerializeNilObject);
  // Determine direction
  if aParent.ClassType = aDef.ClassA then
  begin
    FKParent:=aDef.ForeignKeyA;
    FKAssociated:=aDef.ForeignKeyB;
  end
  else
  begin
    FKParent:=aDef.ForeignKeyB;
    FKAssociated:=aDef.ForeignKeyA;
  end;
  // Resolve associated class persister
  AssocDef:=Registry.FindPersister(aAssociatedClass);
  if AssocDef = Nil then
    RaiseError('LoadAssociations', SErrNoPersisterForClass, [aAssociatedClass.ClassName]);
  // Resolve parent class persister
  ParentDef:=Registry.FindPersister(aParent.ClassType);
  if ParentDef = Nil then
    RaiseError('LoadAssociations', SErrNoPersisterForClass, [aParent.ClassName]);
  PKField:=ParentDef.FieldMap.FindPrimaryKeyField;
  if PKField = Nil then
    RaiseError('LoadAssociations', SErrLoadNoKeyFields, [aParent.ClassName]);
  if AssocDef.FieldMap.FindPrimaryKeyField = Nil then
    RaiseError('LoadAssociations', SErrLoadNoKeyFields, [aAssociatedClass.ClassName]);
  // Resolve list manipulator, clear list, query
  Manipulator:=OPListRegistry.FindManipulator(aList.ClassType);
  if Manipulator = Nil then
    RaiseError('LoadAssociations', SErrLoadNoManipulator, [aList.ClassName]);
  Manipulator.ClearList(aList);
  Params:=CreateParentKeyParams(aParent, ParentDef, PKField);
  try
    // A parent without key value (not saved yet) has no associations
    if Params[0].IsNull then
      Exit;
    SQL:=BuildAssociationsSQL(AssocDef, aDef.JunctionTable, FKParent, FKAssociated);
    Conn:=JunctionConnection(aDef);
    Persister:=CreatePersister(AssocDef, 'LoadAssociations');
    try
      // Configuration errors were raised above: only database errors doom
      // the caller's transaction
      BeginTransaction;
      try
        RegisterConnectionForTransaction(Conn);
        OPExecutor.QueryCustomSQLList(aList, Manipulator, aAssociatedClass, Persister, AssocDef.FieldMap,
          SQL, Conn, Params, SQLBuilder);
        FinishLoadedList(aList, Manipulator, AssocDef.FieldMap);
        Commit;
      except
        RollbackAfterError('LoadAssociations');
        raise;
      end;
    finally
      Persister.Free;
    end;
  finally
    Params.Free;
  end;
end;

procedure TOPManager.LoadAssociations(aParent: TObject; const aPropertyName: UTF8String);
var
  Defs: TOPManyToManyDefArray;
  Def, MatchedDef: TOPManyToManyDef;
  I, MatchCount: Integer;
  PropName: UTF8String;
  PropInf: PPropInfo;
  ListObj: TObject;
  AssociatedClass: TClass;
begin
  Defs:=M2MRegistry.FindDefsForClass(aParent.ClassType);
  MatchedDef:=nil;
  MatchCount:=0;
  for I:=0 to High(Defs) do
  begin
    Def:=Defs[I];
    // Determine associated class (the "other side")
    if aParent.ClassType = Def.ClassA then
      AssociatedClass:=Def.ClassB
    else
      AssociatedClass:=Def.ClassA;
    PropName:=ResolveM2MListPropertyName(Def, AssociatedClass);
    if CompareText(PropName, aPropertyName) = 0 then
    begin
      MatchedDef:=Def;
      Inc(MatchCount);
    end;
  end;
  if MatchCount = 0 then
    RaiseError('LoadAssociations', SErrM2MPropertyNotFound, [aPropertyName, aParent.ClassName]);
  if MatchCount > 1 then
    RaiseError('LoadAssociations', SErrM2MAmbiguousProperty, [aPropertyName, aParent.ClassName]);
  PropInf:=GetPropInfo(aParent, aPropertyName);
  if PropInf = nil then
    RaiseError('LoadAssociations', SErrM2MPropertyNotFound, [aPropertyName, aParent.ClassName]);
  ListObj:=GetObjectProp(aParent, PropInf);
  if ListObj = nil then
    RaiseError('LoadAssociations', SErrM2MPropertyNotFound, [aPropertyName, aParent.ClassName]);
  // Determine associated class from matched def
  if aParent.ClassType = MatchedDef.ClassA then
    AssociatedClass:=MatchedDef.ClassB
  else
    AssociatedClass:=MatchedDef.ClassA;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAssociations: begin (%s, %s)', [aParent.ClassName, aPropertyName]);
  {$ENDIF}
  DoLoadAssociations(aParent, MatchedDef, AssociatedClass, ListObj);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAssociations: end');
  {$ENDIF}
end;

procedure TOPManager.LoadAssociations(aParent: TObject; aAssociatedClass: TClass; const aPropertyName: UTF8String);
var
  Def: TOPManyToManyDef;
  PropInf: PPropInfo;
  ListObj: TObject;
begin
  Def:=M2MRegistry.FindDef(aParent.ClassType, aAssociatedClass);
  if Def = nil then
    RaiseError('LoadAssociations', SErrM2MRelationNotFound, [aParent.ClassName, aAssociatedClass.ClassName]);
  PropInf:=GetPropInfo(aParent, aPropertyName);
  if PropInf = nil then
    RaiseError('LoadAssociations', SErrM2MPropertyNotFound, [aPropertyName, aParent.ClassName]);
  ListObj:=GetObjectProp(aParent, PropInf);
  if ListObj = nil then
    RaiseError('LoadAssociations', SErrM2MPropertyNotFound, [aPropertyName, aParent.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAssociations: begin (%s, %s)', [aParent.ClassName, aAssociatedClass.ClassName]);
  {$ENDIF}
  DoLoadAssociations(aParent, Def, aAssociatedClass, ListObj);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAssociations: end');
  {$ENDIF}
end;

procedure TOPManager.LoadAssociations(aParent: TObject; aAssociatedClass: TClass; aList: TObject);
var
  Def: TOPManyToManyDef;
begin
  Def:=M2MRegistry.FindDef(aParent.ClassType, aAssociatedClass);
  if Def = nil then
    RaiseError('LoadAssociations', SErrM2MRelationNotFound, [aParent.ClassName, aAssociatedClass.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAssociations: begin (%s, %s)', [aParent.ClassName, aAssociatedClass.ClassName]);
  {$ENDIF}
  DoLoadAssociations(aParent, Def, aAssociatedClass, aList);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAssociations: end');
  {$ENDIF}
end;

{ Key value of aObject, for a junction row }

function TOPManager.JunctionKeyValue(aObject: TObject; const aOperation: string): Variant;
var
  Def: Opf.Registry.TPersisterDef;
  PKField: TOPFieldMap;
  Persister: TOPPersister;
  PKParam: TParam;
begin
  Def:=Registry.FindPersister(aObject.ClassType);
  if Def = Nil then
    RaiseError(aOperation, SErrNoPersisterForClass, [aObject.ClassName]);
  PKField:=Def.FieldMap.FindPrimaryKeyField;
  if PKField = Nil then
    RaiseError(aOperation, SErrLoadNoKeyFields, [aObject.ClassName]);
  Persister:=CreatePersister(Def, aOperation);
  try
    PKParam:=TParam.Create(nil);
    try
      Persister.PersistProperty(aObject, PKField, PKParam);
      Result:=PKParam.Value;
    finally
      PKParam.Free;
    end;
  finally
    Persister.Free;
  end;
end;

{ Configuration errors and a duplicate are raised outside the transaction
  level: they do not doom the caller's transaction. A duplicate is detected
  with a SELECT first, so the INSERT does not fail (on PostgreSQL a failed
  statement aborts the whole transaction). }

procedure TOPManager.DoAddAssociation(aObjectA, aObjectB: TObject; aDef: TOPManyToManyDef);
var
  PKValueA, PKValueB: Variant;
  FKFieldA, FKFieldB: UTF8String;
  JTable, FKA, FKB: UTF8String;
  Conn: TSQLConnection;
  Query: TSQLQuery;
  Exists: Boolean;
begin
  if aObjectA = nil then
    RaiseError('AddAssociation', SErrSerializeNilObject);
  if aObjectB = nil then
    RaiseError('AddAssociation', SErrSerializeNilObject);
  // Determine direction
  if aObjectA.ClassType = aDef.ClassA then
  begin
    FKFieldA:=aDef.ForeignKeyA;
    FKFieldB:=aDef.ForeignKeyB;
  end
  else
  begin
    FKFieldA:=aDef.ForeignKeyB;
    FKFieldB:=aDef.ForeignKeyA;
  end;
  PKValueA:=JunctionKeyValue(aObjectA, 'AddAssociation');
  PKValueB:=JunctionKeyValue(aObjectB, 'AddAssociation');
  JTable:=SQLBuilder.Identifier(aDef.JunctionTable, itTable);
  FKA:=SQLBuilder.Identifier(FKFieldA);
  FKB:=SQLBuilder.Identifier(FKFieldB);
  Conn:=JunctionConnection(aDef);
  Exists:=False;
  BeginTransaction;
  try
    RegisterConnectionForTransaction(Conn);
    Query:=TSQLQuery.Create(Nil);
    try
      Query.Database:=Conn;
      Query.Transaction:=Conn.Transaction;
      Query.SQL.Text:=Format('SELECT 1 FROM %s WHERE %s = :opf_fka AND %s = :opf_fkb', [JTable, FKA, FKB]);
      Query.Params.ParamByName('opf_fka').Value:=PKValueA;
      Query.Params.ParamByName('opf_fkb').Value:=PKValueB;
      try
        Query.Open;
        Exists:=not Query.EOF;
        Query.Close;
      except
        on E: EDatabaseError do
          RaiseDBError('AddAssociation', SErrExecutorExecFailed, [E.Message], E);
      end;
      if not Exists then
      begin
        Query.SQL.Text:=Format('INSERT INTO %s (%s, %s) VALUES (:opf_fka, :opf_fkb)', [JTable, FKA, FKB]);
        Query.Params.ParamByName('opf_fka').Value:=PKValueA;
        Query.Params.ParamByName('opf_fkb').Value:=PKValueB;
        try
          Query.ExecSQL;
        except
          // The original error: a foreign key violation is not a duplicate
          on E: EDatabaseError do
            RaiseDBError('AddAssociation', SErrM2MAddFailed,
              [aObjectA.ClassName, VarToStr(PKValueA), aObjectB.ClassName, VarToStr(PKValueB),
               aDef.JunctionTable, E.Message], E);
        end;
      end;
    finally
      Query.Free;
    end;
    Commit;
  except
    RollbackAfterError('AddAssociation');
    raise;
  end;
  if Exists then
    RaiseOperationError('AddAssociation', SErrM2MDuplicateAssociation,
      [aObjectA.ClassName, VarToStr(PKValueA), aObjectB.ClassName, VarToStr(PKValueB), aDef.JunctionTable]);
end;

procedure TOPManager.DoRemoveAssociation(aObjectA, aObjectB: TObject; aDef: TOPManyToManyDef);
var
  PKValueA, PKValueB: Variant;
  FKFieldA, FKFieldB: UTF8String;
  SQL: UTF8String;
  Conn: TSQLConnection;
  Query: TSQLQuery;
begin
  if aObjectA = nil then
    RaiseError('RemoveAssociation', SErrSerializeNilObject);
  if aObjectB = nil then
    RaiseError('RemoveAssociation', SErrSerializeNilObject);
  // Determine direction
  if aObjectA.ClassType = aDef.ClassA then
  begin
    FKFieldA:=aDef.ForeignKeyA;
    FKFieldB:=aDef.ForeignKeyB;
  end
  else
  begin
    FKFieldA:=aDef.ForeignKeyB;
    FKFieldB:=aDef.ForeignKeyA;
  end;
  PKValueA:=JunctionKeyValue(aObjectA, 'RemoveAssociation');
  PKValueB:=JunctionKeyValue(aObjectB, 'RemoveAssociation');
  // Build DELETE SQL
  SQL:=Format('DELETE FROM %s WHERE %s = :opf_fka AND %s = :opf_fkb',
    [SQLBuilder.Identifier(aDef.JunctionTable, itTable),
     SQLBuilder.Identifier(FKFieldA), SQLBuilder.Identifier(FKFieldB)]);
  Conn:=JunctionConnection(aDef);
  BeginTransaction;
  try
    RegisterConnectionForTransaction(Conn);
    Query:=TSQLQuery.Create(Nil);
    try
      Query.Database:=Conn;
      Query.Transaction:=Conn.Transaction;
      Query.SQL.Text:=SQL;
      Query.Params.ParamByName('opf_fka').Value:=PKValueA;
      Query.Params.ParamByName('opf_fkb').Value:=PKValueB;
      Query.ExecSQL;
    finally
      Query.Free;
    end;
    Commit;
  except
    RollbackAfterError('RemoveAssociation');
    raise;
  end;
end;

procedure TOPManager.DeleteM2MJunctionRows(aParentClass: TClass; aPKValue: Variant);
var
  Defs: TOPManyToManyDefArray;
  I: Integer;
  Def: TOPManyToManyDef;
  FK, SQL: UTF8String;
  Q: TSQLQuery;
  Conn: TSQLConnection;
begin
  Defs:=M2MRegistry.FindDefsForClass(aParentClass);
  for I:=0 to High(Defs) do
  begin
    Def:=Defs[I];
    if Def.DBCascade then
      Continue;
    if aParentClass = Def.ClassA then
      FK:=Def.ForeignKeyA
    else
      FK:=Def.ForeignKeyB;
    SQL:=Format('DELETE FROM %s WHERE %s = :opf_pk',
      [SQLBuilder.Identifier(Def.JunctionTable, itTable), SQLBuilder.Identifier(FK)]);
    Conn:=JunctionConnection(Def);
    RegisterConnectionForTransaction(Conn);
    Q:=TSQLQuery.Create(Nil);
    try
      Q.Database:=Conn;
      Q.Transaction:=Conn.Transaction;
      Q.SQL.Text:=SQL;
      Q.Params.ParamByName('opf_pk').Value:=aPKValue;
      Q.ExecSQL;
    finally
      Q.Free;
    end;
  end;
end;

procedure TOPManager.AddAssociation(aObjectA, aObjectB: TObject);
var
  Def: TOPManyToManyDef;
begin
  if aObjectA = nil then
    RaiseError('AddAssociation', SErrSerializeNilObject);
  if aObjectB = nil then
    RaiseError('AddAssociation', SErrSerializeNilObject);
  Def:=M2MRegistry.FindDef(aObjectA.ClassType, aObjectB.ClassType);
  if Def = nil then
    RaiseError('AddAssociation', SErrM2MRelationNotFound, [aObjectA.ClassName, aObjectB.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'AddAssociation: begin (%s, %s)', [aObjectA.ClassName, aObjectB.ClassName]);
  {$ENDIF}
  DoAddAssociation(aObjectA, aObjectB, Def);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'AddAssociation: end');
  {$ENDIF}
end;

procedure TOPManager.RemoveAssociation(aObjectA, aObjectB: TObject);
var
  Def: TOPManyToManyDef;
begin
  if aObjectA = nil then
    RaiseError('RemoveAssociation', SErrSerializeNilObject);
  if aObjectB = nil then
    RaiseError('RemoveAssociation', SErrSerializeNilObject);
  Def:=M2MRegistry.FindDef(aObjectA.ClassType, aObjectB.ClassType);
  if Def = nil then
    RaiseError('RemoveAssociation', SErrM2MRelationNotFound, [aObjectA.ClassName, aObjectB.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'RemoveAssociation: begin (%s, %s)', [aObjectA.ClassName, aObjectB.ClassName]);
  {$ENDIF}
  DoRemoveAssociation(aObjectA, aObjectB, Def);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'RemoveAssociation: end');
  {$ENDIF}
end;

procedure TOPManager.LoadChildren(aParent: TObject; const aPropertyName: UTF8String);
var
  Relations: TOPRelationDefArray;
  Rel: TOPRelationDef;
  I: Integer;
  PropName: UTF8String;
  ListObj: TObject;
  PropInf: PPropInfo;
begin
  Relations:=RelationRegistry.FindChildRelations(aParent.ClassType);
  if Length(Relations) = 0 then
    RaiseError('LoadChildren', SErrLoadChildrenNoRelations, [aParent.ClassName]);
  Rel:=nil;
  // A single relation without a configured list property accepts any list
  // property (as before), except the list of a many-to-many definition;
  // a configured one must be the one asked for
  if (Length(Relations) = 1) and (Relations[0].ListPropertyName = '') then
  begin
    if (CompareText(ResolveChildListPropertyName(aParent.ClassType, Relations[0]), aPropertyName) <> 0)
       and IsM2MListProperty(aParent.ClassType, aPropertyName) then
      RaiseError('LoadChildren', SErrM2MListIsAssociation, [aPropertyName, aParent.ClassName]);
    Rel:=Relations[0];
  end
  else
  begin
    for I:=0 to High(Relations) do
    begin
      PropName:=ResolveChildListPropertyName(aParent.ClassType, Relations[I]);
      if CompareText(PropName, aPropertyName) = 0 then
      begin
        Rel:=Relations[I];
        Break;
      end;
    end;
  end;
  if Rel=nil then
    RaiseError('LoadChildren', SErrLoadChildrenNoMatchingProperty, [aParent.ClassName, aPropertyName]);
  PropInf:=GetPropInfo(aParent, aPropertyName);
  if PropInf=nil then
    RaiseError('LoadChildren', SErrLoadChildrenPropertyNotFound, [aPropertyName, aParent.ClassName]);
  ListObj:=GetObjectProp(aParent, PropInf);
  if ListObj=nil then
    RaiseError('LoadChildren', SErrLoadChildrenPropertyNotFound, [aPropertyName, aParent.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: begin (%s, %s)', [aParent.ClassName, aPropertyName]);
  {$ENDIF}
  DoLoadChildren(aParent, Rel, ListObj);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: end');
  {$ENDIF}
end;

procedure TOPManager.LoadChildren(aParent: TObject; aChildClass: TClass; const aPropertyName: UTF8String);
var
  Relations: TOPRelationDefArray;
  Rel: TOPRelationDef;
  I: Integer;
  PropInf: PPropInfo;
  ListObj: TObject;
begin
  Relations:=RelationRegistry.FindChildRelations(aParent.ClassType);
  Rel:=nil;
  for I:=0 to High(Relations) do
    if Relations[I].ChildClass=aChildClass then
    begin
      Rel:=Relations[I];
      Break;
    end;
  if Rel=nil then
    RaiseError('LoadChildren', SErrLoadChildrenRelationNotFound, [aParent.ClassName, aChildClass.ClassName]);
  PropInf:=GetPropInfo(aParent, aPropertyName);
  if PropInf=nil then
    RaiseError('LoadChildren', SErrLoadChildrenPropertyNotFound, [aPropertyName, aParent.ClassName]);
  ListObj:=GetObjectProp(aParent, PropInf);
  if ListObj=nil then
    RaiseError('LoadChildren', SErrLoadChildrenPropertyNotFound, [aPropertyName, aParent.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: begin (%s, %s)', [aParent.ClassName, aChildClass.ClassName]);
  {$ENDIF}
  DoLoadChildren(aParent, Rel, ListObj);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: end');
  {$ENDIF}
end;

procedure TOPManager.LoadChildren(aParent: TObject; aChildClass: TClass; aList: TObject);
var
  Relations: TOPRelationDefArray;
  Rel: TOPRelationDef;
  I: Integer;
begin
  Relations:=RelationRegistry.FindChildRelations(aParent.ClassType);
  Rel:=nil;
  for I:=0 to High(Relations) do
    if Relations[I].ChildClass=aChildClass then
    begin
      Rel:=Relations[I];
      Break;
    end;
  if Rel=nil then
    RaiseError('LoadChildren', SErrLoadChildrenRelationNotFound, [aParent.ClassName, aChildClass.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: begin (%s, %s)', [aParent.ClassName, aChildClass.ClassName]);
  {$ENDIF}
  DoLoadChildren(aParent, Rel, aList);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: end');
  {$ENDIF}
end;

procedure TOPManager.LoadChildren(aParent: TObject; aList: TObject);
var
  Relations: TOPRelationDefArray;
begin
  Relations:=RelationRegistry.FindChildRelations(aParent.ClassType);
  if Length(Relations) = 0 then
    RaiseError('LoadChildren', SErrLoadChildrenNoRelations, [aParent.ClassName]);
  if Length(Relations) > 1 then
    RaiseError('LoadChildren', SErrLoadChildrenAmbiguous, [aParent.ClassName]);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: begin (%s)', [aParent.ClassName]);
  {$ENDIF}
  DoLoadChildren(aParent, Relations[0], aList);
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadChildren: end');
  {$ENDIF}
end;

procedure TOPManager.LoadAllChildren(aParent: TObject);
var
  Relations: TOPRelationDefArray;
  Lists: array of TObject;
  I: Integer;
  PropInf: PPropInfo;
  ChildDef, ParentDef: TPersisterDef;
  PKField: TOPFieldMap;
  Manipulator: TOPListManipulator;
begin
  Relations:=RelationRegistry.FindChildRelations(aParent.ClassType);
  if Length(Relations) = 0 then
    Exit;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAllChildren: begin (%s)', [aParent.ClassName]);
  {$ENDIF}
  // Configuration errors first: they must not doom the caller's transaction
  Lists:=Nil;
  SetLength(Lists, Length(Relations));
  for I:=0 to High(Relations) do
  begin
    Lists[I]:=Nil;
    PropInf:=GetPropInfo(aParent, ResolveChildListPropertyName(aParent.ClassType, Relations[I]));
    if PropInf=nil then
      Continue;
    Lists[I]:=GetObjectProp(aParent, PropInf);
    if Lists[I]<>Nil then
      CheckLoadChildren(aParent, Relations[I], Lists[I], ChildDef, ParentDef, PKField, Manipulator);
  end;
  // One transaction level for all relations: a consistent view
  BeginTransaction;
  try
    for I:=0 to High(Relations) do
      if Lists[I]<>Nil then
        DoLoadChildren(aParent, Relations[I], Lists[I]);
    Commit;
  except
    RollbackAfterError('LoadAllChildren');
    raise;
  end;
  {$IFNDEF OPF_SKIP_LOGGING}
  if TOPLogger(GetLogger).IsKindActive(lkTrace) then
    TOPLogger(GetLogger).LogTrace(Self, 'LoadAllChildren: end');
  {$ENDIF}
end;

function TOPManager.GetMigrationTableName: UTF8String;
begin
  Result := FSchemaMigrator.MigrationTableName;
end;

procedure TOPManager.SetMigrationTableName(const AValue: UTF8String);
begin
  FSchemaMigrator.MigrationTableName := AValue;
end;

function TOPManager.GetSchemaMigrator: TOPSchemaMigrator;
begin
  Result := FSchemaMigrator;
end;

procedure TOPManager.SetSchemaMigrator(AValue: TOPSchemaMigrator);
begin
  if FSchemaMigrator = AValue then
    Exit;
  if AValue = nil then
  begin
    if not FOwnsSchemaMigrator then
    begin
      FSchemaMigrator.RemoveFreeNotification(Self);
      FSchemaMigrator := TOPSchemaMigrator.Create(Self);
      FOwnsSchemaMigrator := True;
    end;
  end
  else
  begin
    if FOwnsSchemaMigrator then
      FreeAndNil(FSchemaMigrator);
    FSchemaMigrator := AValue;
    FOwnsSchemaMigrator := False;
    FSchemaMigrator.FreeNotification(Self);
  end;
end;

procedure TOPManager.EnsureMigratorRegistries;
begin
  if FSchemaMigrator.Registry = nil then
    FSchemaMigrator.Registry := Registry;
  if FSchemaMigrator.RelationRegistry = nil then
    FSchemaMigrator.RelationRegistry := RelationRegistry;
  if FSchemaMigrator.M2MRegistry = nil then
    FSchemaMigrator.M2MRegistry := M2MRegistry;
end;

procedure TOPManager.RegisterMigration(aFromVersion, aToVersion: Integer;
  aStep: TOPMigrationStep; const aDescription: UTF8String);
begin
  FSchemaMigrator.RegisterMigration(aFromVersion, aToVersion, aStep, aDescription);
end;

procedure TOPManager.CreateSchema(aConnection: TSQLConnection;
  aCreateOptions: TSQLCreateOptions; aSchemaOptions: TSchemaCreateOptions);
begin
  EnsureMigratorRegistries;
  FSchemaMigrator.CreateSchema(aConnection, aCreateOptions, aSchemaOptions);
end;

function TOPManager.MigrateSchema(aConnection: TSQLConnection;
  aTargetVersion: Integer; aOptions: TMigrateOptions): TOPMigrationResult;
begin
  EnsureMigratorRegistries;
  Result := FSchemaMigrator.MigrateSchema(aConnection, aTargetVersion, aOptions);
end;

function TOPManager.GenerateMigrationSQL(aConnection: TSQLConnection;
  aTargetVersion: Integer; aOptions: TMigrateOptions;
  aGenerateOptions: TGenerateSQLOptions): TUTF8StringArray;
begin
  EnsureMigratorRegistries;
  Result := FSchemaMigrator.GenerateMigrationSQL(aConnection, aTargetVersion, aOptions, aGenerateOptions);
end;

finalization
  // Connections that log later (e.g. freed by another unit's finalization)
  // must not call the dispatcher freed here
  if DBLogHookIsOurs then
    GlobalDBLogHook:=DBLogPrevious;
  DBLogPrevious:=Nil;
  FreeAndNil(DBLogManagers);
  FreeAndNil(DBLogDispatcher);
end.

