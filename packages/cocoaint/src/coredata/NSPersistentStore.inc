{ Parsed from Coredata.framework NSPersistentStore.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSPERSISTENTSTORE_PAS_T}
{$define NSPERSISTENTSTORE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPERSISTENTSTORE_PAS_R}
{$define NSPERSISTENTSTORE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPERSISTENTSTORE_PAS_F}
{$define NSPERSISTENTSTORE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPERSISTENTSTORE_PAS_S}
{$define NSPERSISTENTSTORE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPersistentStore = objcclass;
  NSPersistentStorePointer = ^NSPersistentStore;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPERSISTENTSTORE_PAS_C}
{$define NSPERSISTENTSTORE_PAS_C}

{ NSPersistentStore }
  NSPersistentStore = objcclass(NSObject)
  private
    __coordinator: NSPersistentStoreCoordinator; {garbage collector: __weak }
    __configurationName: NSString;
    __url: NSURL;
    __options: NSDictionary;
    __oidFactories: NSMutableDictionary;
    __defaultFaultHandler: id;
    __flags: bitpacked record
        isReadOnly: 0..1;
        cleanOnRemove: 0..1;
        isMDDirty: 0..1;
        _RESERVED: 0..((1 shl 29)-1);
      end;
    __temporaryIDClass: Pointer;
    __reserved2: Pointer;
    __reserved3: Pointer;
    
  public
    class function alloc: NSPersistentStore; message 'alloc';

    class function metadataForPersistentStoreWithURL_error(URL_: NSURL; error: NSErrorPointer): NSDictionary; message 'metadataForPersistentStoreWithURL:error:';
    class function setMetadata_forPersistentStoreWithURL_error(metadata_: NSDictionary; URL_: NSURL; error: NSErrorPointer): Boolean; message 'setMetadata:forPersistentStoreWithURL:error:';
    function initWithPersistentStoreCoordinator_configurationName_URL_options(root: NSPersistentStoreCoordinator; name: NSString; URL_: NSURL; options_: NSDictionary): id; message 'initWithPersistentStoreCoordinator:configurationName:URL:options:';
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; message 'persistentStoreCoordinator';
    function configurationName: NSString; message 'configurationName';
    function options: NSDictionary; message 'options';
    function URL: NSURL; message 'URL';
    procedure setURL(URL_: NSURL); message 'setURL:';
    function identifier: NSString; message 'identifier';
    procedure setIdentifier(identifier_: NSString); message 'setIdentifier:';
    function type_: NSString; message 'type';
    function isReadOnly: Boolean; message 'isReadOnly';
    procedure setReadOnly(flag: Boolean); message 'setReadOnly:';
    function metadata: NSDictionary; message 'metadata';
    procedure setMetadata(storeMetadata: NSDictionary); message 'setMetadata:';
    procedure didAddToPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); message 'didAddToPersistentStoreCoordinator:';
    procedure willRemoveFromPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); message 'willRemoveFromPersistentStoreCoordinator:';
  end; external;

{$endif}
{$endif}
