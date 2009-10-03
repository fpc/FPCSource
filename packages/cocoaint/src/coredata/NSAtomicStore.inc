{ Parsed from Coredata.framework NSAtomicStore.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSATOMICSTORE_PAS_C}
{$define NSATOMICSTORE_PAS_C}

{ NSAtomicStore }
  NSAtomicStore = objcclass(NSPersistentStore)
  private
    __nodeCache: NSMutableDictionary;
    __entityCache: NSMutableDictionary;
    __storeMetadata: NSMutableDictionary;
    __nextReference: NSInteger;
    __reserved4: Pointer;
    __reserved5: Pointer;
    
  public
    class function alloc: NSAtomicStore; message 'alloc';

    function initWithPersistentStoreCoordinator_configurationName_URL_options(coordinator: NSPersistentStoreCoordinator; configurationName_: NSString; URL_: NSURL; options_: NSDictionary): id; message 'initWithPersistentStoreCoordinator:configurationName:URL:options:';
	
	{ GK.: Need to differentiate from load class method }
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
  end; external;

{$endif}
{$endif}
