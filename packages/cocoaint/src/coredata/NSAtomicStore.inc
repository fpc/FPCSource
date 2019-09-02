{ Parsed from CoreData.framework NSAtomicStore.h }


{$ifdef TYPES}
type
  NSAtomicStorePtr = ^NSAtomicStore;
{$endif}

{$ifdef CLASSES}

type
  NSAtomicStore = objcclass external (NSPersistentStore)
  private
    _nodeCache: NSMutableDictionary;
    _entityCache: NSMutableDictionary;
    _storeMetadata: NSMutableDictionary;
    _nextReference: NSInteger;
    _reserved4: pointer;
    _reserved5: pointer;
  public
    function initWithPersistentStoreCoordinator_configurationName_URL_options (coordinator: NSPersistentStoreCoordinator; configurationName_: NSString; url_: NSURL; options_: NSDictionary): instancetype; message 'initWithPersistentStoreCoordinator:configurationName:URL:options:';
    function load_ (error: NSErrorPtr): ObjCBOOL; message 'load:';
    function save (error: NSErrorPtr): ObjCBOOL; message 'save:';
    function newCacheNodeForManagedObject (managedObject: NSManagedObject): NSAtomicStoreCacheNode; message 'newCacheNodeForManagedObject:';
    procedure updateCacheNode_fromManagedObject (node: NSAtomicStoreCacheNode; managedObject: NSManagedObject); message 'updateCacheNode:fromManagedObject:';
    function cacheNodes: NSSet; message 'cacheNodes';
    procedure addCacheNodes (cacheNodes_: NSSet); message 'addCacheNodes:';
    procedure willRemoveCacheNodes (cacheNodes_: NSSet); message 'willRemoveCacheNodes:';
    function cacheNodeForObjectID (objectID: NSManagedObjectID): NSAtomicStoreCacheNode; message 'cacheNodeForObjectID:';
    function objectIDForEntity_referenceObject (entity: NSEntityDescription; data: id): NSManagedObjectID; message 'objectIDForEntity:referenceObject:';
    function newReferenceObjectForManagedObject (managedObject: NSManagedObject): id; message 'newReferenceObjectForManagedObject:';
    function referenceObjectForObjectID (objectID: NSManagedObjectID): id; message 'referenceObjectForObjectID:';
  end;
{$endif}

