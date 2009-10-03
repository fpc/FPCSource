{ Parsed from Coredata.framework NSManagedObjectContext.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSMANAGEDOBJECTCONTEXT_PAS_T}
{$define NSMANAGEDOBJECTCONTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMANAGEDOBJECTCONTEXT_PAS_R}
{$define NSMANAGEDOBJECTCONTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMANAGEDOBJECTCONTEXT_PAS_F}
{$define NSMANAGEDOBJECTCONTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMANAGEDOBJECTCONTEXT_PAS_S}
{$define NSMANAGEDOBJECTCONTEXT_PAS_S}

{ External string constants }
var
  NSManagedObjectContextDidSaveNotification: NSString; external name '_NSManagedObjectContextDidSaveNotification';
  NSManagedObjectContextObjectsDidChangeNotification: NSString; external name '_NSManagedObjectContextObjectsDidChangeNotification';
  NSInsertedObjectsKey: NSString; external name '_NSInsertedObjectsKey';
  NSUpdatedObjectsKey: NSString; external name '_NSUpdatedObjectsKey';
  NSDeletedObjectsKey: NSString; external name '_NSDeletedObjectsKey';
  NSRefreshedObjectsKey: NSString; external name '_NSRefreshedObjectsKey';
  NSInvalidatedObjectsKey: NSString; external name '_NSInvalidatedObjectsKey';
  NSInvalidatedAllObjectsKey: NSString; external name '_NSInvalidatedAllObjectsKey';

{ External symbols }
var
  NSErrorMergePolicy: id; external name '_NSErrorMergePolicy';
  NSMergeByPropertyStoreTrumpMergePolicy: id; external name '_NSMergeByPropertyStoreTrumpMergePolicy';
  NSMergeByPropertyObjectTrumpMergePolicy: id; external name '_NSMergeByPropertyObjectTrumpMergePolicy';
  NSOverwriteMergePolicy: id; external name '_NSOverwriteMergePolicy';
  NSRollbackMergePolicy: id; external name '_NSRollbackMergePolicy';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSManagedObjectContext = objcclass;
  NSManagedObjectContextPointer = ^NSManagedObjectContext;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMANAGEDOBJECTCONTEXT_PAS_C}
{$define NSMANAGEDOBJECTCONTEXT_PAS_C}

{ NSManagedObjectContext }
  NSManagedObjectContext = objcclass(NSObject, NSCodingProtocol, NSLockingProtocol)
  private
    __spinLock: longint;
    __objectStore: NSPersistentStoreCoordinator;
    __undoManager: NSUndoManager;
    __children: NSMutableArray;
    __flags: bitpacked record
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
        _reservedFlags: 0..((1 shl 19)-1);
      end;
    __unprocessedChanges: NSMutableSet;
    __unprocessedDeletes: NSMutableSet;
    __unprocessedInserts: NSMutableSet;
    __insertedObjects: NSMutableSet;
    __deletedObjects: NSMutableSet;
    __changedObjects: NSMutableSet;
    __lockedObjects: NSMutableSet;
    __refreshedObjects: NSMutableSet;
    __infoByGID: id;
    __cachedObsInfoByEntity: id;
    __undoTransactionID: cshort;
    __lock: id;
    __lockCount: clong;
    __objectStoreLockCount: clong;
    __fetchTimestamp: NSTimeInterval;
    __delegate: id;
    __referenceQueue: id;
    __ignoreChangeNotification: clong;
    __mergePolicy: id;
    __cd_rc: longint;
    __managedObjectContextEditorFlags: bitpacked record
        _isEditing: 0..1;
        _reservedManagedObjectContext: 0..((1 shl 31)-1);
      end;
    __editors: id;
    __debuggingRecords: id;
    __reserved1: Pointer;
    __reserved2: Pointer;
    
  public
    class function alloc: NSManagedObjectContext; message 'alloc';

    procedure setPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); message 'setPersistentStoreCoordinator:';
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; message 'persistentStoreCoordinator';
    procedure setUndoManager(undoManager_: NSUndoManager); message 'setUndoManager:';
    function undoManager: NSUndoManager; message 'undoManager';
    function hasChanges: Boolean; message 'hasChanges';
    function objectRegisteredForID(objectID: NSManagedObjectID): NSManagedObject; message 'objectRegisteredForID:';
    function objectWithID(objectID: NSManagedObjectID): NSManagedObject; message 'objectWithID:';
    function executeFetchRequest_error(request: NSFetchRequest; error: NSErrorPointer): NSArray; message 'executeFetchRequest:error:';
    function countForFetchRequest_error(request: NSFetchRequest; error: NSErrorPointer): NSUInteger; message 'countForFetchRequest:error:';
    procedure insertObject(object_: NSManagedObject); message 'insertObject:';
    procedure deleteObject(object_: NSManagedObject); message 'deleteObject:';
    procedure refreshObject_mergeChanges(object_: NSManagedObject; flag: Boolean); message 'refreshObject:mergeChanges:';
    procedure detectConflictsForObject(object_: NSManagedObject); message 'detectConflictsForObject:';
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString; object_: id; change: NSDictionary; context: Pointer); message 'observeValueForKeyPath:ofObject:change:context:';
    procedure processPendingChanges; message 'processPendingChanges';
    procedure assignObject_toPersistentStore(object_: id; store: NSPersistentStore); message 'assignObject:toPersistentStore:';
    function insertedObjects: NSSet; message 'insertedObjects';
    function updatedObjects: NSSet; message 'updatedObjects';
    function deletedObjects: NSSet; message 'deletedObjects';
    function registeredObjects: NSSet; message 'registeredObjects';
    procedure undo; message 'undo';
    procedure redo; message 'redo';
    procedure reset; message 'reset';
    procedure rollback; message 'rollback';
    function save(error: NSErrorPointer): Boolean; message 'save:';
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
    function tryLock: Boolean; message 'tryLock';
    function propagatesDeletesAtEndOfEvent: Boolean; message 'propagatesDeletesAtEndOfEvent';
    procedure setPropagatesDeletesAtEndOfEvent(flag: Boolean); message 'setPropagatesDeletesAtEndOfEvent:';
    function retainsRegisteredObjects: Boolean; message 'retainsRegisteredObjects';
    procedure setRetainsRegisteredObjects(flag: Boolean); message 'setRetainsRegisteredObjects:';
    function stalenessInterval: NSTimeInterval; message 'stalenessInterval';
    procedure setStalenessInterval(expiration: NSTimeInterval); message 'setStalenessInterval:';
    procedure setMergePolicy(mergePolicy_: id); message 'setMergePolicy:';
    function mergePolicy: id; message 'mergePolicy';
    function obtainPermanentIDsForObjects_error(objects: NSArray; error: NSErrorPointer): Boolean; message 'obtainPermanentIDsForObjects:error:';
    procedure mergeChangesFromContextDidSaveNotification(notification: NSNotification); message 'mergeChangesFromContextDidSaveNotification:';
  end; external;

{$endif}
{$endif}
