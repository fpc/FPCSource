{ Parsed from Appkit.framework NSPersistentDocument.h }

{$ifdef TYPES}
{$ifndef NSPERSISTENTDOCUMENT_PAS_T}
{$define NSPERSISTENTDOCUMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPERSISTENTDOCUMENT_PAS_R}
{$define NSPERSISTENTDOCUMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPERSISTENTDOCUMENT_PAS_F}
{$define NSPERSISTENTDOCUMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPERSISTENTDOCUMENT_PAS_S}
{$define NSPERSISTENTDOCUMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPersistentDocument = objcclass;
  NSPersistentDocumentPointer = ^NSPersistentDocument;
  NSPersistentDocumentPtr = NSPersistentDocumentPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPERSISTENTDOCUMENT_PAS_C}
{$define NSPERSISTENTDOCUMENT_PAS_C}

{ NSPersistentDocument }
  NSPersistentDocument = objcclass external (NSDocument)
  private
    _managedObjectModel: NSManagedObjectModel;
    _managedObjectContext: NSManagedObjectContext;
    _store: id;
    _reserved: Pointer;
    _reserved2: Pointer;
    _reserved3: Pointer;
    _reserved4: Pointer;
    
  public
    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    procedure setManagedObjectContext(managedObjectContext_: NSManagedObjectContext); message 'setManagedObjectContext:';
    function managedObjectModel: id; message 'managedObjectModel';
    function configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error(url: NSURL; fileType_: NSString; configuration: NSString; storeOptions: NSDictionary; error: NSErrorPointer): Boolean; message 'configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:';
    function persistentStoreTypeForFileType(fileType_: NSString): NSString; message 'persistentStoreTypeForFileType:';
    function writeToURL_ofType_forSaveOperation_originalContentsURL_error(absoluteURL: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; absoluteOriginalContentsURL: NSURL; error: NSErrorPointer): Boolean; message 'writeToURL:ofType:forSaveOperation:originalContentsURL:error:';
    function readFromURL_ofType_error(absoluteURL: NSURL; typeName: NSString; error: NSErrorPointer): Boolean; message 'readFromURL:ofType:error:';
    function revertToContentsOfURL_ofType_error(inAbsoluteURL: NSURL; inTypeName: NSString; outError: NSErrorPointer): Boolean; message 'revertToContentsOfURL:ofType:error:';
  end;

{ NSDeprecated_NSPersistentDocumentCategory }
  NSDeprecated_NSPersistentDocumentCategory = objccategory external name 'NSDeprecated' (NSPersistentDocument)
    function configurePersistentStoreCoordinatorForURL_ofType_error(url: NSURL; fileType_: NSString; error: NSErrorPointer): Boolean; message 'configurePersistentStoreCoordinatorForURL:ofType:error:'; deprecated 'in Mac OS X 10.5 and later';
  end;

{$endif}
{$endif}
