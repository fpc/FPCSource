{ Parsed from SyncServices.framework ISyncSessionDriver.h }


{$ifdef TYPES}
type
  ISyncSessionDriverPtr = ^ISyncSessionDriver;
  ISyncSessionDriverDataSourceProtocolPtr = ^ISyncSessionDriverDataSourceProtocol;
{$endif}

{$ifdef TYPES}
const
  ISyncSessionDriverModeFast = 1;
  ISyncSessionDriverModeSlow = 2;
  ISyncSessionDriverModeRefresh = 3;
type
  ISyncSessionDriverMode = clong;
const
  ISyncSessionDriverChangeRefused = 0;
  ISyncSessionDriverChangeAccepted = 1;
  ISyncSessionDriverChangeIgnored = 2;
  ISyncSessionDriverChangeError = 3;
type
  ISyncSessionDriverChangeResult = clong;
{$endif}

{$ifdef CLASSES}

type
  ISyncSessionDriver = objcclass external (NSObject)
  private
  public
    class function sessionDriverWithDataSource (dataSource: ISyncSessionDriverDataSourceProtocol): ISyncSessionDriver; message 'sessionDriverWithDataSource:'; deprecated 'in 10_4, 10_7';
    function sync: ObjCBOOL; message 'sync'; deprecated 'in 10_4, 10_7';
    function startAsynchronousSync (outError: NSErrorPtr): ObjCBOOL; message 'startAsynchronousSync:'; deprecated 'in 10_4, 10_7';
    function lastError: NSError; message 'lastError'; deprecated 'in 10_4, 10_7';
    function dataSource: ISyncSessionDriverDataSourceProtocol; message 'dataSource'; deprecated 'in 10_4, 10_7';
    procedure setDelegate (delegate: id); message 'setDelegate:'; deprecated 'in 10_4, 10_7';
    function delegate: id; message 'delegate'; deprecated 'in 10_4, 10_7';
    procedure setHandlesSyncAlerts (yesOrNo: ObjCBOOL); message 'setHandlesSyncAlerts:'; deprecated 'in 10_4, 10_7';
    function handlesSyncAlerts: ObjCBOOL; message 'handlesSyncAlerts'; deprecated 'in 10_4, 10_7';
    function client: ISyncClient; message 'client'; deprecated 'in 10_4, 10_7';
    function session: ISyncSession; message 'session'; deprecated 'in 10_4, 10_7';
    procedure finishSyncing; message 'finishSyncing'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  ISyncSessionDriverDataSourceProtocol = objcprotocol external name 'ISyncSessionDriverDataSource'
    function clientIdentifier: NSString; message 'clientIdentifier'; deprecated 'in 10_4, 10_7';
    function clientDescriptionURL: NSURL; message 'clientDescriptionURL'; deprecated 'in 10_4, 10_7';
    function schemaBundleURLs: NSArray; message 'schemaBundleURLs'; deprecated 'in 10_4, 10_7';
    function preferredSyncModeForEntityName (entity: NSString): ISyncSessionDriverMode; message 'preferredSyncModeForEntityName:'; deprecated 'in 10_4, 10_7';
    function recordsForEntityName_moreComing_error (entity: NSString; moreComing: pObjCBOOL; outError: NSErrorPtr): NSDictionary; message 'recordsForEntityName:moreComing:error:'; deprecated 'in 10_4, 10_7';
    function applyChange_forEntityName_remappedRecordIdentifier_formattedRecord_error (change: ISyncChange; entityName: NSString; outRecordIdentifier: NSStringPtr; outRecord: NSDictionaryPtr; outError: NSErrorPtr): ISyncSessionDriverChangeResult; message 'applyChange:forEntityName:remappedRecordIdentifier:formattedRecord:error:'; deprecated 'in 10_4, 10_7';
    function deleteAllRecordsForEntityName_error (entityName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'deleteAllRecordsForEntityName:error:'; deprecated 'in 10_4, 10_7';
  optional
    function entityNamesToSync: NSArray; message 'entityNamesToSync'; deprecated 'in 10_4, 10_7';
    function entityNamesToPull: NSArray; message 'entityNamesToPull'; deprecated 'in 10_4, 10_7';
    function sessionBeginTimeout: NSTimeInterval; message 'sessionBeginTimeout'; deprecated 'in 10_4, 10_7';
    function sessionPullChangesTimeout: NSTimeInterval; message 'sessionPullChangesTimeout'; deprecated 'in 10_4, 10_7';
    function lastAnchorForEntityName (entityName: NSString): NSString; message 'lastAnchorForEntityName:'; deprecated 'in 10_4, 10_7';
    function nextAnchorForEntityName (entityName: NSString): NSString; message 'nextAnchorForEntityName:'; deprecated 'in 10_4, 10_7';
    function changedRecordsForEntityName_moreComing_error (entity: NSString; moreComing: pObjCBOOL; outError: NSErrorPtr): NSDictionary; message 'changedRecordsForEntityName:moreComing:error:'; deprecated 'in 10_4, 10_7';
    function changesForEntityName_moreComing_error (entity: NSString; moreComing: pObjCBOOL; outError: NSErrorPtr): NSArray; message 'changesForEntityName:moreComing:error:'; deprecated 'in 10_4, 10_7';
    function identifiersForRecordsToDeleteForEntityName_moreComing_error (entityName: NSString; moreComing: pObjCBOOL; outError: NSErrorPtr): NSArray; message 'identifiersForRecordsToDeleteForEntityName:moreComing:error:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

{$ifdef CLASSES}

type
  ISyncSessionDriverDelegate = objccategory external (NSObject)
    function sessionDriver_didRegisterClientAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:didRegisterClientAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_willNegotiateAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:willNegotiateAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_didNegotiateAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:didNegotiateAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_willPushAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:willPushAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_didPushAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:didPushAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_willPullAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:willPullAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_didPullAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:didPullAndReturnError:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_willFinishSessionAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:willFinishSessionAndReturnError:'; deprecated 'in 10_4, 10_7';
    procedure sessionDriverDidFinishSession (sender: ISyncSessionDriver); message 'sessionDriverDidFinishSession:'; deprecated 'in 10_4, 10_7';
    procedure sessionDriverWillCancelSession (sender: ISyncSessionDriver); message 'sessionDriverWillCancelSession:'; deprecated 'in 10_4, 10_7';
    procedure sessionDriverDidCancelSession (sender: ISyncSessionDriver); message 'sessionDriverDidCancelSession:'; deprecated 'in 10_4, 10_7';
    function sessionDriver_didReceiveSyncAlertAndReturnError (sender: ISyncSessionDriver; outError: NSErrorPtr): ObjCBOOL; message 'sessionDriver:didReceiveSyncAlertAndReturnError:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

