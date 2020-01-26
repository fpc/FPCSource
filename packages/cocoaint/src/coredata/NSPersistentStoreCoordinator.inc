{ Parsed from CoreData.framework NSPersistentStoreCoordinator.h }


{$ifdef TYPES}
type
  NSPersistentStoreCoordinatorPtr = ^NSPersistentStoreCoordinator;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSQLiteStoreType: NSString { available in 10_4, 3_0 }; cvar; external;
  NSXMLStoreType: NSString { available in 10_4, NA }; cvar; external;
  NSBinaryStoreType: NSString { available in 10_4, 3_0 }; cvar; external;
  NSInMemoryStoreType: NSString { available in 10_4, 3_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSPersistentStoreUbiquitousTransitionType = NSUInteger;
  NSPersistentStoreUbiquitousTransitionTypePtr = ^NSPersistentStoreUbiquitousTransitionType;

const
  NSPersistentStoreUbiquitousTransitionTypeAccountAdded = 1;
  NSPersistentStoreUbiquitousTransitionTypeAccountRemoved = 2;
  NSPersistentStoreUbiquitousTransitionTypeContentRemoved = 3;
  NSPersistentStoreUbiquitousTransitionTypeInitialImportCompleted = 4;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSStoreTypeKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSStoreUUIDKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSPersistentStoreCoordinatorStoresWillChangeNotification: NSString { available in 10_9, 7_0 }; cvar; external;
  NSPersistentStoreCoordinatorStoresDidChangeNotification: NSString { available in 10_4, 3_0 }; cvar; external;
  NSPersistentStoreCoordinatorWillRemoveStoreNotification: NSString { available in 10_5, 3_0 }; cvar; external;
  NSAddedPersistentStoresKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSRemovedPersistentStoresKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSUUIDChangedPersistentStoresKey: NSString { available in 10_4, 3_0 }; cvar; external;
  NSReadOnlyPersistentStoreOption: NSString { available in 10_4, 3_0 }; cvar; external;
  NSValidateXMLStoreOption: NSString { available in 10_4, NA }; cvar; external;
  NSPersistentStoreTimeoutOption: NSString { available in 10_5, 3_0 }; cvar; external;
  NSSQLitePragmasOption: NSString { available in 10_5, 3_0 }; cvar; external;
  NSSQLiteAnalyzeOption: NSString { available in 10_5, 3_0 }; cvar; external;
  NSSQLiteManualVacuumOption: NSString { available in 10_6, 3_0 }; cvar; external;
  NSIgnorePersistentStoreVersioningOption: NSString { available in 10_5, 3_0 }; cvar; external;
  NSMigratePersistentStoresAutomaticallyOption: NSString { available in 10_5, 3_0 }; cvar; external;
  NSInferMappingModelAutomaticallyOption: NSString { available in 10_6, 3_0 }; cvar; external;
  NSStoreModelVersionHashesKey: NSString { available in 10_5, 3_0 }; cvar; external;
  NSStoreModelVersionIdentifiersKey: NSString { available in 10_5, 3_0 }; cvar; external;
  NSPersistentStoreOSCompatibility: NSString { available in 10_5, 3_0 }; cvar; external;
  NSXMLExternalRecordType: NSString { available in 10_6, NA }; cvar; external;
  NSBinaryExternalRecordType: NSString { available in 10_6, NA }; cvar; external;
  NSExternalRecordsFileFormatOption: NSString { available in 10_6, NA }; cvar; external;
  NSExternalRecordsDirectoryOption: NSString { available in 10_6, NA }; cvar; external;
  NSExternalRecordExtensionOption: NSString { available in 10_6, NA }; cvar; external;
  NSEntityNameInPathKey: NSString { available in 10_6, NA }; cvar; external;
  NSStoreUUIDInPathKey: NSString { available in 10_6, NA }; cvar; external;
  NSStorePathKey: NSString { available in 10_6, NA }; cvar; external;
  NSModelPathKey: NSString { available in 10_6, NA }; cvar; external;
  NSObjectURIKey: NSString { available in 10_6, NA }; cvar; external;
  NSPersistentStoreUbiquitousContentNameKey: NSString { available in 10_7, 5_0 }; cvar; external;
  NSPersistentStoreUbiquitousContentURLKey: NSString { available in 10_7, 5_0 }; cvar; external;
  NSPersistentStoreDidImportUbiquitousContentChangesNotification: NSString { available in 10_7, 5_0 }; cvar; external;
  NSPersistentStoreUbiquitousTransitionTypeKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSPersistentStoreUbiquitousPeerTokenOption: NSString { available in 10_9, 7_0 }; cvar; external;
  NSPersistentStoreRemoveUbiquitousMetadataOption: NSString { available in 10_9, 7_0 }; cvar; external;
  NSPersistentStoreUbiquitousContainerIdentifierKey: NSString { available in 10_9, 7_0 }; cvar; external;
  NSPersistentStoreRebuildFromUbiquitousContentOption: NSString { available in 10_9, 7_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSPersistentStoreCoordinator = objcclass external (NSObject, NSLockingProtocol)
  private
    _queueOwner: id;
    _dispatchQueue: pointer;
    _persistentStoreCoordinatorFlags: bitpacked record
      case byte of
        0: (_anonBitField__persistentStoreCoordinatorFlags0: cuint);
        1: (
          _isRegistered: 0..1;
          _reservedFlags: 0..((1 shl 31)-1);
        );
      end;
    {$ifdef __LP64__}
    _reserved32: cuint32;
    {$endif}
    _miniLock: clong;
    _additionalPrivateIvars: idPtr;
    _managedObjectModel: NSManagedObjectModel;
    _persistentStores: NSArray;
  public
    function initWithManagedObjectModel (model: NSManagedObjectModel): instancetype; message 'initWithManagedObjectModel:'; { NS_DESIGNATED_INITIALIZER }
    function managedObjectModel: NSManagedObjectModel; message 'managedObjectModel';
    function persistentStores: NSArray; message 'persistentStores';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    function persistentStoreForURL (URL: NSURL): NSPersistentStore; message 'persistentStoreForURL:';
    function URLForPersistentStore (store: NSPersistentStore): NSURL; message 'URLForPersistentStore:';
    function setURL_forPersistentStore (url: NSURL; store: NSPersistentStore): ObjCBOOL; message 'setURL:forPersistentStore:'; { available in 10_5, 3_0 }
    function addPersistentStoreWithType_configuration_URL_options_error (storeType: NSString; configuration: NSString; storeURL: NSURL; options: NSDictionary; error: NSErrorPtr): NSPersistentStore; message 'addPersistentStoreWithType:configuration:URL:options:error:';
    function removePersistentStore_error (store: NSPersistentStore; error: NSErrorPtr): ObjCBOOL; message 'removePersistentStore:error:';
    procedure setMetadata_forPersistentStore (metadata: NSDictionary; store: NSPersistentStore); message 'setMetadata:forPersistentStore:';
    function metadataForPersistentStore (store: NSPersistentStore): NSDictionary; message 'metadataForPersistentStore:';
    function managedObjectIDForURIRepresentation (url: NSURL): NSManagedObjectID; message 'managedObjectIDForURIRepresentation:';
    function executeRequest_withContext_error (request: NSPersistentStoreRequest; context: NSManagedObjectContext; error: NSErrorPtr): id; message 'executeRequest:withContext:error:'; { available in 10_7,  5_0 }
    class function registeredStoreTypes: NSDictionary; message 'registeredStoreTypes'; { available in 10_5, 3_0 }
    class procedure registerStoreClass_forStoreType (storeClass: pobjc_class; storeType: NSString); message 'registerStoreClass:forStoreType:'; { available in 10_5, 3_0 }
    class function metadataForPersistentStoreOfType_URL_error (storeType: NSString; url: NSURL; error: NSErrorPtr): NSDictionary; message 'metadataForPersistentStoreOfType:URL:error:'; { available in 10_5, 3_0 }
    class function setMetadata_forPersistentStoreOfType_URL_error (metadata: NSDictionary; storeType: NSString; url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'setMetadata:forPersistentStoreOfType:URL:error:'; { available in 10_5, 3_0 }
    class function elementsDerivedFromExternalRecordURL (fileURL: NSURL): NSDictionary; message 'elementsDerivedFromExternalRecordURL:'; { available in 10_6, NA }
    class function removeUbiquitousContentAndPersistentStoreAtURL_options_error (storeURL: NSURL; options: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'removeUbiquitousContentAndPersistentStoreAtURL:options:error:'; { available in 10_9, 7_0 }
    function importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error (storeIdentifier: NSString; externalRecordsURL: NSURL; destinationURL: NSURL; options: NSDictionary; storeType: NSString; error: NSErrorPtr): NSPersistentStore; message 'importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:'; { available in 10_6, NA }
    function migratePersistentStore_toURL_options_withType_error (store: NSPersistentStore; URL: NSURL; options: NSDictionary; storeType: NSString; error: NSErrorPtr): NSPersistentStore; message 'migratePersistentStore:toURL:options:withType:error:';
    procedure performBlock (block: OpaqueCBlock); message 'performBlock:'; { available in 10_10, 8_0 }
    procedure performBlockAndWait (block: OpaqueCBlock); message 'performBlockAndWait:'; { available in 10_10, 8_0 }
    class function metadataForPersistentStoreWithURL_error (url: NSURL; error: NSErrorPtr): NSDictionary; message 'metadataForPersistentStoreWithURL:error:'; deprecated 'in 10_4,10_5';
    procedure lock; message 'lock'; deprecated 'in 10_4, 10_10, 3_0, 8_0, "Use -performBlockAndWait: instead"';
    procedure unlock; message 'unlock'; deprecated 'in 10_4, 10_10, 3_0, 8_0, "Use -performBlockAndWait: instead"';
    function tryLock: ObjCBOOL; message 'tryLock'; deprecated 'in 10_4, 10_10, 3_0, 8_0, "Use -performBlock: instead"';
  end;
{$endif}

