{ Parsed from Appkit.framework NSPersistentDocument.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPERSISTENTDOCUMENT_PAS_H}
{$define NSPERSISTENTDOCUMENT_PAS_H}
type
  NSPersistentDocumentPointer = Pointer;

{$endif}
{$endif}

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSPERSISTENTDOCUMENT_PAS_C}
{$define NSPERSISTENTDOCUMENT_PAS_C}

{ NSPersistentDocument }
  NSPersistentDocument = objcclass(NSDocument)
  private
    __managedObjectModel: NSManagedObjectModel;
    __managedObjectContext: NSManagedObjectContext;
    __store: id;
    __reserved: Pointer;
    __reserved2: Pointer;
    __reserved3: Pointer;
    __reserved4: Pointer;
    
  public
    class function alloc: NSPersistentDocument; message 'alloc';

    function managedObjectContext: NSManagedObjectContext; message 'managedObjectContext';
    procedure setManagedObjectContext(managedObjectContext_: NSManagedObjectContextPointer); message 'setManagedObjectContext:';
    function managedObjectModel: id; message 'managedObjectModel';
    function configurePersistentStoreCoordinatorForURL_ofType_modelConfiguration_storeOptions_error(url: NSURL; fileType_: NSString; configuration: NSString; storeOptions: NSDictionary; error: NSErrorPointer): Boolean; message 'configurePersistentStoreCoordinatorForURL:ofType:modelConfiguration:storeOptions:error:';
    function persistentStoreTypeForFileType(fileType_: NSString): NSString; message 'persistentStoreTypeForFileType:';
    function writeToURL_ofType_forSaveOperation_originalContentsURL_error(absoluteURL: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; absoluteOriginalContentsURL: NSURL; error: NSErrorPointer): Boolean; message 'writeToURL:ofType:forSaveOperation:originalContentsURL:error:';
    function readFromURL_ofType_error(absoluteURL: NSURL; typeName: NSString; error: NSErrorPointer): Boolean; message 'readFromURL:ofType:error:';
    function revertToContentsOfURL_ofType_error(inAbsoluteURL: NSURL; inTypeName: NSString; outError: NSErrorPointer): Boolean; message 'revertToContentsOfURL:ofType:error:';

    { Category: NSPersistentDocumentDeprecated }
    function configurePersistentStoreCoordinatorForURL_ofType_error(url: NSURL; fileType_: NSString; error: NSErrorPointer): Boolean; message 'configurePersistentStoreCoordinatorForURL:ofType:error:';
  end; external;

{$endif}
{$endif}
