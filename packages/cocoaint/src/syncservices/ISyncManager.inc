{ Parsed from SyncServices.framework ISyncManager.h }


{$ifdef TYPES}
type
  ISyncManagerPtr = ^ISyncManager;
{$endif}

{$ifdef CLASSES}

type
  ISyncManager = objcclass external (NSObject)
  public
    class function sharedManager: ISyncManager; message 'sharedManager'; deprecated 'in 10_4, 10_7';
    function isEnabled: ObjCBOOL; message 'isEnabled'; deprecated 'in 10_4, 10_7';
    function syncDisabledReason: NSError; message 'syncDisabledReason'; deprecated 'in 10_4, 10_7';
    function clientWithIdentifier (clientId: NSString): ISyncClient; message 'clientWithIdentifier:'; deprecated 'in 10_4, 10_7';
    function registerClientWithIdentifier_descriptionFilePath (clientId: NSString; descriptionFilePath: NSString): ISyncClient; message 'registerClientWithIdentifier:descriptionFilePath:'; deprecated 'in 10_4, 10_7';
    procedure unregisterClient (client: ISyncClient); message 'unregisterClient:'; deprecated 'in 10_4, 10_7';
    function registerSchemaWithBundlePath (bundlePath: NSString): ObjCBOOL; message 'registerSchemaWithBundlePath:'; deprecated 'in 10_4, 10_7';
    procedure unregisterSchemaWithName (schemaName: NSString); message 'unregisterSchemaWithName:'; deprecated 'in 10_4, 10_7';
    procedure clientWithIdentifier_needsSyncing (clientId: NSString; flag: ObjCBOOL); message 'clientWithIdentifier:needsSyncing:'; deprecated 'in 10_6, 10_7';
    function snapshotOfRecordsInTruthWithEntityNames_usingIdentifiersForClient (entityNames: NSArray; client: ISyncClient): ISyncRecordSnapshot; message 'snapshotOfRecordsInTruthWithEntityNames:usingIdentifiersForClient:'; deprecated 'in 10_4, 10_7';
    procedure addRequestMode (mode: NSString); message 'addRequestMode:'; deprecated 'in 10_4, 10_7';
    procedure removeRequestMode (mode: NSString); message 'removeRequestMode:'; deprecated 'in 10_4, 10_7';
    function requestModes: NSArray; message 'requestModes'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ISyncAvailabilityChangedNotification: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncServerUnavailableException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
{$endif}

