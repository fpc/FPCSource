{ Parsed from AppKit.framework NSPersistentDocument.h }


{$ifdef TYPES}
type
  NSPersistentDocumentPtr = ^NSPersistentDocument;
{$endif}

{$ifdef CLASSES}

type
  NSPersistentDocument = objcclass external (NSDocument)
  private
    _managedObjectModel: NSManagedObjectModel;
    _managedObjectContext: NSManagedObjectContext;
    _store: id;
    _pDocFlags: culong;
    _relatedRequestURLs: id;
    _reserved3: pointer;
    _reserved4: pointer;
  public
    procedure setManagedObjectContext(newValue: NSManagedObjectContext); message 'setManagedObjectContext:';
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    function managedObjectModel: id; message 'managedObjectModel';
    function configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error (url: NSURL; fileType_: NSString; configuration: NSString; storeOptions: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:'; { available in 10_5 }
    function persistentStoreTypeForFileType (fileType_: NSString): NSString; message 'persistentStoreTypeForFileType:';
    function writeToURL_ofType_forSaveOperation_originalContentsURL_error (absoluteURL: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; absoluteOriginalContentsURL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'writeToURL:ofType:forSaveOperation:originalContentsURL:error:';
    function readFromURL_ofType_error (absoluteURL: NSURL; typeName: NSString; error: NSErrorPtr): ObjCBOOL; message 'readFromURL:ofType:error:';
    function revertToContentsOfURL_ofType_error (inAbsoluteURL: NSURL; inTypeName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'revertToContentsOfURL:ofType:error:';
  end;


type
  NSPersistentDocument_NSDeprecated = objccategory external name 'NSDeprecated' (NSPersistentDocument)
    function configurePersistentStoreCoordinatorForURL_ofType_error (url: NSURL; fileType_: NSString; error: NSErrorPtr): ObjCBOOL; message 'configurePersistentStoreCoordinatorForURL:ofType:error:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

