{ Parsed from CoreData.framework NSPersistentStore.h }


{$ifdef TYPES}
type
  NSPersistentStorePtr = ^NSPersistentStore;
{$endif}

{$ifdef CLASSES}

type
  NSPersistentStore = objcclass external (NSObject)
  private
    _coordinator: NSPersistentStoreCoordinator;
    _configurationName: NSString;
    _url: NSURL;
    _options: NSDictionary;
    _oidFactories: idPtr;
    _defaultFaultHandler: id;
    _objectStoreFlags: bitpacked record
      case byte of
        0: (_anonBitField__objectStoreFlags0: cuint);
        1: (
          isReadOnly: 0..1;
          cleanOnRemove: 0..1;
          isMDDirty: 0..1;
          _RESERVED: 0..((1 shl 29)-1);
        );
      end;
    _temporaryIDClass: pointer;
    _externalRecordsMonitor: id;
    _reserved3: pointer;
  public
    class function metadataForPersistentStoreWithURL_error (url: NSURL; error: NSErrorPtr): NSDictionary; message 'metadataForPersistentStoreWithURL:error:';
    class function setMetadata_forPersistentStoreWithURL_error (metadata: NSDictionary; url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'setMetadata:forPersistentStoreWithURL:error:';
    class function migrationManagerClass: pobjc_class; message 'migrationManagerClass'; { available in 10_6, 3_0 }
    function initWithPersistentStoreCoordinator_configurationName_URL_options (root: NSPersistentStoreCoordinator; name: NSString; url: NSURL; options: NSDictionary): instancetype; message 'initWithPersistentStoreCoordinator:configurationName:URL:options:'; { NS_DESIGNATED_INITIALIZER }
    function loadMetadata (error: NSErrorPtr): ObjCBOOL; message 'loadMetadata:';
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; message 'persistentStoreCoordinator';
    function configurationName: NSString; message 'configurationName';
    function options: NSDictionary; message 'options';
    procedure setURL(newValue: NSURL); message 'setURL:';
    function URL: NSURL; message 'URL';
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    function type_: NSString; message 'type';
    procedure setReadOnly(newValue: ObjCBOOL); message 'setReadOnly:';
    function isReadOnly: ObjCBOOL; message 'isReadOnly';
    procedure setMetadata(newValue: NSDictionary); message 'setMetadata:';
    function metadata: NSDictionary; message 'metadata';
    procedure didAddToPersistentStoreCoordinator (coordinator: NSPersistentStoreCoordinator); message 'didAddToPersistentStoreCoordinator:';
    procedure willRemoveFromPersistentStoreCoordinator (coordinator: NSPersistentStoreCoordinator); message 'willRemoveFromPersistentStoreCoordinator:';
  end;
{$endif}

