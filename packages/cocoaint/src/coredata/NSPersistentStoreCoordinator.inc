{ Parsed from Coredata.framework NSPersistentStoreCoordinator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSPERSISTENTSTORECOORDINATOR_PAS_T}
{$define NSPERSISTENTSTORECOORDINATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPERSISTENTSTORECOORDINATOR_PAS_R}
{$define NSPERSISTENTSTORECOORDINATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPERSISTENTSTORECOORDINATOR_PAS_F}
{$define NSPERSISTENTSTORECOORDINATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPERSISTENTSTORECOORDINATOR_PAS_S}
{$define NSPERSISTENTSTORECOORDINATOR_PAS_S}

{ External string constants }
var
  NSSQLiteStoreType: NSString; external name '_NSSQLiteStoreType';
  NSXMLStoreType: NSString; external name '_NSXMLStoreType';
  NSBinaryStoreType: NSString; external name '_NSBinaryStoreType';
  NSInMemoryStoreType: NSString; external name '_NSInMemoryStoreType';
  NSStoreTypeKey: NSString; external name '_NSStoreTypeKey';
  NSStoreUUIDKey: NSString; external name '_NSStoreUUIDKey';
  NSPersistentStoreCoordinatorStoresDidChangeNotification: NSString; external name '_NSPersistentStoreCoordinatorStoresDidChangeNotification';
  NSAddedPersistentStoresKey: NSString; external name '_NSAddedPersistentStoresKey';
  NSRemovedPersistentStoresKey: NSString; external name '_NSRemovedPersistentStoresKey';
  NSUUIDChangedPersistentStoresKey: NSString; external name '_NSUUIDChangedPersistentStoresKey';
  NSReadOnlyPersistentStoreOption: NSString; external name '_NSReadOnlyPersistentStoreOption';
  NSValidateXMLStoreOption: NSString; external name '_NSValidateXMLStoreOption';
  NSPersistentStoreTimeoutOption: NSString; external name '_NSPersistentStoreTimeoutOption';
  NSSQLitePragmasOption: NSString; external name '_NSSQLitePragmasOption';
  NSIgnorePersistentStoreVersioningOption: NSString; external name '_NSIgnorePersistentStoreVersioningOption';
  NSMigratePersistentStoresAutomaticallyOption: NSString; external name '_NSMigratePersistentStoresAutomaticallyOption';
  NSStoreModelVersionHashesKey: NSString; external name '_NSStoreModelVersionHashesKey';
  NSStoreModelVersionIdentifiersKey: NSString; external name '_NSStoreModelVersionIdentifiersKey';
  NSPersistentStoreOSCompatibility: NSString; external name '_NSPersistentStoreOSCompatibility';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPersistentStoreCoordinator = objcclass;
  NSPersistentStoreCoordinatorPointer = ^NSPersistentStoreCoordinator;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPERSISTENTSTORECOORDINATOR_PAS_C}
{$define NSPERSISTENTSTORECOORDINATOR_PAS_C}

{ NSPersistentStoreCoordinator }
  NSPersistentStoreCoordinator = objcclass(NSObject, NSLockingProtocol)
  private
    __reserved: Pointer;
    __reserved2: Pointer;
    __reserved3: Pointer;
    __reserved4: Pointer;
    __managedObjectModel: NSManagedObjectModel;
    __coreLock: id;
    __persistentStores: NSMutableArray;
    
  public
    class function alloc: NSPersistentStoreCoordinator; message 'alloc';

    class function registeredStoreTypes: NSDictionary; message 'registeredStoreTypes';
    class procedure registerStoreClass_forStoreType(storeClass: Pobjc_class; storeType: NSString); message 'registerStoreClass:forStoreType:';
    class function metadataForPersistentStoreOfType_URL_error(storeType: NSString; url: NSURL; error: NSErrorPointer): NSDictionary; message 'metadataForPersistentStoreOfType:URL:error:';
    class function setMetadata_forPersistentStoreOfType_URL_error(metadata: NSDictionary; storeType: NSString; url: NSURL; error: NSErrorPointer): Boolean; message 'setMetadata:forPersistentStoreOfType:URL:error:';
    procedure setMetadata_forPersistentStore(metadata: NSDictionary; store: NSPersistentStore); message 'setMetadata:forPersistentStore:';
    function metadataForPersistentStore(store: NSPersistentStore): NSDictionary; message 'metadataForPersistentStore:';
    function initWithManagedObjectModel(model: NSManagedObjectModel): id; message 'initWithManagedObjectModel:';
    function managedObjectModel: NSManagedObjectModel; message 'managedObjectModel';
    function persistentStores: NSArray; message 'persistentStores';
    function persistentStoreForURL(URL: NSURL): NSPersistentStore; message 'persistentStoreForURL:';
    function URLForPersistentStore(store: NSPersistentStore): NSURL; message 'URLForPersistentStore:';
    function setURL_forPersistentStore(url: NSURL; store: NSPersistentStore): Boolean; message 'setURL:forPersistentStore:';
    function addPersistentStoreWithType_configuration_URL_options_error(storeType: NSString; configuration: NSString; storeURL: NSURL; options: NSDictionary; error: NSErrorPointer): NSPersistentStore; message 'addPersistentStoreWithType:configuration:URL:options:error:';
    function removePersistentStore_error(store: NSPersistentStore; error: NSErrorPointer): Boolean; message 'removePersistentStore:error:';
    function migratePersistentStore_toURL_options_withType_error(store: NSPersistentStore; URL: NSURL; options: NSDictionary; storeType: NSString; error: NSErrorPointer): NSPersistentStore; message 'migratePersistentStore:toURL:options:withType:error:';
    function managedObjectIDForURIRepresentation(url: NSURL): NSManagedObjectID; message 'managedObjectIDForURIRepresentation:';
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
    function tryLock: Boolean; message 'tryLock';
    class function metadataForPersistentStoreWithURL_error(url: NSURL; error: NSErrorPointer): NSDictionary; message 'metadataForPersistentStoreWithURL:error:';
  end; external;

{$endif}
{$endif}
