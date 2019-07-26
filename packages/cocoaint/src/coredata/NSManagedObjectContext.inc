{ Parsed from CoreData.framework NSManagedObjectContext.h }


{$ifdef TYPES}
type
  NSManagedObjectContextPtr = ^NSManagedObjectContext;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSManagedObjectContextWillSaveNotification: NSString { available in 10_5, 3_0 }; cvar; external;
  NSManagedObjectContextDidSaveNotification: NSString { available in 10_4, 3_0 }; cvar; external;
  NSManagedObjectContextObjectsDidChangeNotification: NSString { available in 10_4, 3_0 }; cvar; external;
  NSInsertedObjectsKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSUpdatedObjectsKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSDeletedObjectsKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSRefreshedObjectsKey: NSString { available in 10_5, 3_0 }; cvar; external;
  NSInvalidatedObjectsKey: NSString { available in 10_5, 3_0 }; cvar; external;
  NSInvalidatedAllObjectsKey: NSString { available in 10_5, 3_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSManagedObjectContextConcurrencyType = NSUInteger;
  NSManagedObjectContextConcurrencyTypePtr = ^NSManagedObjectContextConcurrencyType;

const
  NSConfinementConcurrencyType = $00;
  NSPrivateQueueConcurrencyType = $01;
  NSMainQueueConcurrencyType = $02;
{$endif}

{$ifdef CLASSES}

type
  NSManagedObjectContext = objcclass external (NSObject, NSCodingProtocol, NSLockingProtocol)
  private
    _queueOwner: id;
    _dispatchQueue: pointer;
    _reserved1: pointer;
    _spinLock: cint32;
    _parentObjectStore: id;
    _managedObjectContextFlags: bitpacked record
      case byte of
        0: (_anonBitField__managedObjectContextFlags0: cuint);
        1: (
          _registeredForCallback: 0..1;
          _propagatesDeletesAtEndOfEvent: 0..1;
          _exhaustiveValidation: 0..1;
          _processingChanges: 0..1;
          _useCommittedSnapshot: 0..1;
          _registeredUndoTransactionID: 0..1;
          _retainsAllRegisteredObjects: 0..1;
          _savingInProgress: 0..1;
          _wasDisposed: 0..1;
          _unprocessedChangesPending: 0..1;
          _isDirty: 0..1;
          _ignoreUndoCheckpoints: 0..1;
          _propagatingDeletes: 0..1;
          _isNSEditorEditing: 0..1;
          _isMainThreadBlessed: 0..1;
          _isImportContext: 0..1;
          _preflightSaveInProgress: 0..1;
          _disableDiscardEditing: 0..1;
          _isParentStoreContext: 0..1;
          _postSaveNotifications: 0..1;
          _isMerging: 0..1;
          _concurrencyType: 0..1;
          _reservedFlags: 0..((1 shl 10)-1);
        );
      end;
    _unprocessedChanges: NSMutableSet;
    _unprocessedDeletes: NSMutableSet;
    _unprocessedInserts: NSMutableSet;
    _insertedObjects: NSMutableSet;
    _deletedObjects: NSMutableSet;
    _changedObjects: NSMutableSet;
    _lockedObjects: NSMutableSet;
    _refreshedObjects: NSMutableSet;
    _infoByGID: id;
    _cachedObsInfoByEntity: idPtr;
    _undoTransactionID: cshort;
    _lock: id;
    _lockCount: clong;
    _objectStoreLockCount: clong;
    _fetchTimestamp: NSTimeInterval;
    _reserved2: id;
    _referenceQueue: id;
    _reserved3: id;
    _reserved4: id;
    _cd_rc: cint32;
    _ignoreChangeNotification: cint32;
    _reserved6: id;
    _contextLabel: NSString;
    _additionalPrivateIvars: idPtr;
  public
    function init: instancetype; message 'init';
    function initWithConcurrencyType (ct: NSManagedObjectContextConcurrencyType): instancetype; message 'initWithConcurrencyType:'; { available in 10_7,  5_0 }
    procedure performBlock (block: OpaqueCBlock); message 'performBlock:'; { available in 10_7,  5_0 }
    procedure performBlockAndWait (block: OpaqueCBlock); message 'performBlockAndWait:'; { available in 10_7,  5_0 }
    procedure setPersistentStoreCoordinator(newValue: NSPersistentStoreCoordinator); message 'setPersistentStoreCoordinator:';
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; message 'persistentStoreCoordinator';
    procedure setParentContext(newValue: NSManagedObjectContext); message 'setParentContext:';
    function parentContext: NSManagedObjectContext; message 'parentContext';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setUndoManager(newValue: NSUndoManager); message 'setUndoManager:';
    function undoManager: NSUndoManager; message 'undoManager';
    function hasChanges: ObjCBOOL; message 'hasChanges';
    function userInfo: NSMutableDictionary; message 'userInfo';
    function concurrencyType: NSManagedObjectContextConcurrencyType; message 'concurrencyType';
    function objectRegisteredForID (objectID: NSManagedObjectID): NSManagedObject; message 'objectRegisteredForID:';
    function objectWithID (objectID: NSManagedObjectID): NSManagedObject; message 'objectWithID:';
    function existingObjectWithID_error (objectID: NSManagedObjectID; error: NSErrorPtr): NSManagedObject; message 'existingObjectWithID:error:'; { available in 10_6, 3_0 }
    function executeFetchRequest_error (request: NSFetchRequest; error: NSErrorPtr): NSArray; message 'executeFetchRequest:error:';
    function countForFetchRequest_error (request: NSFetchRequest; error: NSErrorPtr): NSUInteger; message 'countForFetchRequest:error:'; { available in 10_5, 3_0 }
    function executeRequest_error (request: NSPersistentStoreRequest; error: NSErrorPtr): NSPersistentStoreResult; message 'executeRequest:error:'; { available in 10_10, 8_0 }
    procedure insertObject (object_: NSManagedObject); message 'insertObject:';
    procedure deleteObject (object_: NSManagedObject); message 'deleteObject:';
    procedure refreshObject_mergeChanges (object_: NSManagedObject; flag: ObjCBOOL); message 'refreshObject:mergeChanges:';
    procedure detectConflictsForObject (object_: NSManagedObject); message 'detectConflictsForObject:';
    procedure observeValueForKeyPath_ofObject_change_context (keyPath: NSString; object_: id; change: NSDictionary; context: pointer); message 'observeValueForKeyPath:ofObject:change:context:';
    procedure processPendingChanges; message 'processPendingChanges';
    procedure assignObject_toPersistentStore (object_: id; store: NSPersistentStore); message 'assignObject:toPersistentStore:';
    function insertedObjects: NSSet; message 'insertedObjects';
    function updatedObjects: NSSet; message 'updatedObjects';
    function deletedObjects: NSSet; message 'deletedObjects';
    function registeredObjects: NSSet; message 'registeredObjects';
    procedure undo; message 'undo';
    procedure redo; message 'redo';
    procedure reset; message 'reset';
    procedure rollback; message 'rollback';
    function save (error: NSErrorPtr): ObjCBOOL; message 'save:';
    procedure lock; message 'lock'; deprecated 'in 10_4, 10_10, 3_0, 8_0, "Use a queue style context and -performBlockAndWait: instead"';
    procedure unlock; message 'unlock'; deprecated 'in 10_4, 10_10, 3_0, 8_0, "Use a queue style context and -performBlockAndWait: instead"';
    function tryLock: ObjCBOOL; message 'tryLock'; deprecated 'in 10_4, 10_10, 3_0, 8_0, "Use a queue style context and -performBlock: instead"';
    procedure setPropagatesDeletesAtEndOfEvent(newValue: ObjCBOOL); message 'setPropagatesDeletesAtEndOfEvent:';
    function propagatesDeletesAtEndOfEvent: ObjCBOOL; message 'propagatesDeletesAtEndOfEvent';
    procedure setRetainsRegisteredObjects(newValue: ObjCBOOL); message 'setRetainsRegisteredObjects:';
    function retainsRegisteredObjects: ObjCBOOL; message 'retainsRegisteredObjects';
    procedure setStalenessInterval(newValue: NSTimeInterval); message 'setStalenessInterval:';
    function stalenessInterval: NSTimeInterval; message 'stalenessInterval';
    procedure setMergePolicy(newValue: id); message 'setMergePolicy:';
    function mergePolicy: id; message 'mergePolicy';
    function obtainPermanentIDsForObjects_error (objects: NSArray; error: NSErrorPtr): ObjCBOOL; message 'obtainPermanentIDsForObjects:error:'; { available in 10_5, 3_0 }
    procedure mergeChangesFromContextDidSaveNotification (notification: NSNotification); message 'mergeChangesFromContextDidSaveNotification:'; { available in 10_5, 3_0 }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

