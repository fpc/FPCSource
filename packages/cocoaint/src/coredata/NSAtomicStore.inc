{ Parsed from Coredata.framework NSAtomicStore.h }

{$ifdef TYPES}
{$ifndef NSATOMICSTORE_PAS_T}
{$define NSATOMICSTORE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSATOMICSTORE_PAS_R}
{$define NSATOMICSTORE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSATOMICSTORE_PAS_F}
{$define NSATOMICSTORE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSATOMICSTORE_PAS_S}
{$define NSATOMICSTORE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAtomicStore = objcclass;
  NSAtomicStorePointer = ^NSAtomicStore;
  NSAtomicStorePtr = NSAtomicStorePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATOMICSTORE_PAS_C}
{$define NSATOMICSTORE_PAS_C}

{ NSAtomicStore }
  NSAtomicStore = objcclass external (NSPersistentStore)
  private
    _nodeCache: NSMutableDictionary;
    _entityCache: NSMutableDictionary;
    _storeMetadata: NSMutableDictionary;
    _nextReference: NSInteger;
    _reserved4: Pointer;
    _reserved5: Pointer;
    
  public
    function initWithPersistentStoreCoordinator_configurationName_URL_options(coordinator: NSPersistentStoreCoordinator; configurationName_: NSString; url_: NSURL; options_: NSDictionary): id; message 'initWithPersistentStoreCoordinator:configurationName:URL:options:';
    function load_(error: NSErrorPointer): Boolean; message 'load:';
    function save(error: NSErrorPointer): Boolean; message 'save:';
    function newCacheNodeForManagedObject(managedObject: NSManagedObject): NSAtomicStoreCacheNode; message 'newCacheNodeForManagedObject:';
    procedure updateCacheNode_fromManagedObject(node: NSAtomicStoreCacheNode; managedObject: NSManagedObject); message 'updateCacheNode:fromManagedObject:';
    function cacheNodes: NSSet; message 'cacheNodes';
    procedure addCacheNodes(cacheNodes_: NSSet); message 'addCacheNodes:';
    procedure willRemoveCacheNodes(cacheNodes_: NSSet); message 'willRemoveCacheNodes:';
    function cacheNodeForObjectID(objectID: NSManagedObjectID): NSAtomicStoreCacheNode; message 'cacheNodeForObjectID:';
    function objectIDForEntity_referenceObject(entity: NSEntityDescription; data: id): NSManagedObjectID; message 'objectIDForEntity:referenceObject:';
    function newReferenceObjectForManagedObject(managedObject: NSManagedObject): id; message 'newReferenceObjectForManagedObject:';
    function referenceObjectForObjectID(objectID: NSManagedObjectID): id; message 'referenceObjectForObjectID:';
  end;

{$endif}
{$endif}
