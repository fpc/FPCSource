{ Parsed from Coredata.framework NSPersistentStoreCoordinator.h }

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
  NSSQLiteStoreType: NSString; cvar; external;
  NSXMLStoreType: NSString; cvar; external;
  NSBinaryStoreType: NSString; cvar; external;
  NSInMemoryStoreType: NSString; cvar; external;
  NSStoreTypeKey: NSString; cvar; external;
  NSStoreUUIDKey: NSString; cvar; external;
  NSPersistentStoreCoordinatorStoresDidChangeNotification: NSString; cvar; external;
  NSPersistentStoreCoordinatorWillRemoveStoreNotification: NSString; cvar; external;
  NSAddedPersistentStoresKey: NSString; cvar; external;
  NSRemovedPersistentStoresKey: NSString; cvar; external;
  NSUUIDChangedPersistentStoresKey: NSString; cvar; external;
  NSReadOnlyPersistentStoreOption: NSString; cvar; external;
  NSValidateXMLStoreOption: NSString; cvar; external;
  NSPersistentStoreTimeoutOption: NSString; cvar; external;
  NSSQLitePragmasOption: NSString; cvar; external;
  NSSQLiteAnalyzeOption: NSString; cvar; external;
  NSSQLiteManualVacuumOption: NSString; cvar; external;
  NSIgnorePersistentStoreVersioningOption: NSString; cvar; external;
  NSMigratePersistentStoresAutomaticallyOption: NSString; cvar; external;
  NSInferMappingModelAutomaticallyOption: NSString; cvar; external;
  NSStoreModelVersionHashesKey: NSString; cvar; external;
  NSStoreModelVersionIdentifiersKey: NSString; cvar; external;
  NSPersistentStoreOSCompatibility: NSString; cvar; external;
  NSXMLExternalRecordType: NSString; cvar; external;
  NSBinaryExternalRecordType: NSString; cvar; external;
  NSExternalRecordsFileFormatOption: NSString; cvar; external;
  NSExternalRecordsDirectoryOption: NSString; cvar; external;
  NSExternalRecordExtensionOption: NSString; cvar; external;
  NSEntityNameInPathKey: NSString; cvar; external;
  NSStoreUUIDInPathKey: NSString; cvar; external;
  NSStorePathKey: NSString; cvar; external;
  NSModelPathKey: NSString; cvar; external;
  NSObjectURIKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPersistentStoreCoordinator = objcclass;
  NSPersistentStoreCoordinatorPointer = ^NSPersistentStoreCoordinator;
  NSPersistentStoreCoordinatorPtr = NSPersistentStoreCoordinatorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPERSISTENTSTORECOORDINATOR_PAS_C}
{$define NSPERSISTENTSTORECOORDINATOR_PAS_C}

{ NSPersistentStoreCoordinator }
  NSPersistentStoreCoordinator = objcclass external (NSObject, NSLockingProtocol)
  private
    _reserved: Pointer;
    _reserved2: Pointer;
    _reserved3: Pointer;
    _externalRecordsHelper: id;
    _managedObjectModel: NSManagedObjectModel;
    _coreLock: id;
    _persistentStores: NSMutableArray;
    
  public
    class function registeredStoreTypes: NSDictionary; message 'registeredStoreTypes';
    class procedure registerStoreClass_forStoreType(storeClass: Pobjc_class; storeType: NSString); message 'registerStoreClass:forStoreType:';
    class function metadataForPersistentStoreOfType_URL_error(storeType: NSString; url: NSURL; error: NSErrorPointer): NSDictionary; message 'metadataForPersistentStoreOfType:URL:error:';
    class function setMetadata_forPersistentStoreOfType_URL_error(metadata: NSDictionary; storeType: NSString; url: NSURL; error: NSErrorPointer): Boolean; message 'setMetadata:forPersistentStoreOfType:URL:error:';
    class function elementsDerivedFromExternalRecordURL(fileURL: NSURL): NSDictionary; message 'elementsDerivedFromExternalRecordURL:';
    function importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error(storeIdentifier: NSString; externalRecordsURL: NSURL; destinationURL: NSURL; options: NSDictionary; storeType: NSString; error: NSErrorPointer): NSPersistentStore; message 'importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:';
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
    class function metadataForPersistentStoreWithURL_error(url: NSURL; error: NSErrorPointer): NSDictionary; message 'metadataForPersistentStoreWithURL:error:'; deprecated 'in Mac OS X 10.5 and later';

    { Adopted Protocols }
  end;

{$endif}
{$endif}
