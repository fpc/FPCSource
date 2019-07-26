{ Parsed from SyncServices.framework ISyncSession.h }


{$ifdef TYPES}
type
  ISyncSessionPtr = ^ISyncSession;
{$endif}

{$ifdef CLASSES}

type
  ISyncSession = objcclass external (NSObject)
  public
    class function beginSessionWithClient_entityNames_beforeDate (client: ISyncClient; entityNames: NSArray; date: NSDate): ISyncSession; message 'beginSessionWithClient:entityNames:beforeDate:'; deprecated 'in 10_4, 10_7';
    class procedure beginSessionInBackgroundWithClient_entityNames_target_selector (client: ISyncClient; entityNames: NSArray; target: id; selector: SEL); message 'beginSessionInBackgroundWithClient:entityNames:target:selector:'; deprecated 'in 10_4, 10_7';
    class procedure cancelPreviousBeginSessionWithClient (client: ISyncClient); message 'cancelPreviousBeginSessionWithClient:'; deprecated 'in 10_4, 10_7';
    class function beginSessionWithClient_entityNames_beforeDate_lastAnchors (client: ISyncClient; entityNames: NSArray; date: NSDate; anchors: NSDictionary): ISyncSession; message 'beginSessionWithClient:entityNames:beforeDate:lastAnchors:'; deprecated 'in 10_5, 10_7';
    class procedure beginSessionInBackgroundWithClient_entityNames_target_selector_lastAnchors (client: ISyncClient; entityNames: NSArray; target: id; selector: SEL; anchors: NSDictionary); message 'beginSessionInBackgroundWithClient:entityNames:target:selector:lastAnchors:'; deprecated 'in 10_5, 10_7';
    procedure clientDidResetEntityNames (entityNames: NSArray); message 'clientDidResetEntityNames:'; deprecated 'in 10_4, 10_7';
    procedure clientWantsToPushAllRecordsForEntityNames (entityNames: NSArray); message 'clientWantsToPushAllRecordsForEntityNames:'; deprecated 'in 10_4, 10_7';
    function shouldPushChangesForEntityName (entityName: NSString): ObjCBOOL; message 'shouldPushChangesForEntityName:'; deprecated 'in 10_4, 10_7';
    function shouldPushAllRecordsForEntityName (entityName: NSString): ObjCBOOL; message 'shouldPushAllRecordsForEntityName:'; deprecated 'in 10_4, 10_7';
    function shouldPullChangesForEntityName (entityName: NSString): ObjCBOOL; message 'shouldPullChangesForEntityName:'; deprecated 'in 10_4, 10_7';
    function shouldReplaceAllRecordsOnClientForEntityName (entityName: NSString): ObjCBOOL; message 'shouldReplaceAllRecordsOnClientForEntityName:'; deprecated 'in 10_4, 10_7';
    procedure pushChange (change: ISyncChange); message 'pushChange:'; deprecated 'in 10_4, 10_7';
    procedure pushChangesFromRecord_withIdentifier (record_: NSDictionary; recordId: NSString); message 'pushChangesFromRecord:withIdentifier:'; deprecated 'in 10_4, 10_7';
    procedure deleteRecordWithIdentifier (recordId: NSString); message 'deleteRecordWithIdentifier:'; deprecated 'in 10_4, 10_7';
    procedure clientLostRecordWithIdentifier_shouldReplaceOnNextSync (recordId: NSString; flag: ObjCBOOL); message 'clientLostRecordWithIdentifier:shouldReplaceOnNextSync:'; deprecated 'in 10_4, 10_7';
    procedure clientFinishedPushingChangesWithNextAnchors (anchors: NSDictionary); message 'clientFinishedPushingChangesWithNextAnchors:'; deprecated 'in 10_5, 10_7';
    function prepareToPullChangesForEntityNames_beforeDate (entityNames: NSArray; date: NSDate): ObjCBOOL; message 'prepareToPullChangesForEntityNames:beforeDate:'; deprecated 'in 10_4, 10_7';
    procedure prepareToPullChangesInBackgroundForEntityNames_target_selector (entityNames: NSArray; target: id; selector: SEL); message 'prepareToPullChangesInBackgroundForEntityNames:target:selector:'; deprecated 'in 10_4, 10_7';
    function changeEnumeratorForEntityNames (entityNames: NSArray): NSEnumerator; message 'changeEnumeratorForEntityNames:'; deprecated 'in 10_4, 10_7';
    procedure clientAcceptedChangesForRecordWithIdentifier_formattedRecord_newRecordIdentifier (recordId: NSString; formattedRecord: NSDictionary; recordId3: NSString); message 'clientAcceptedChangesForRecordWithIdentifier:formattedRecord:newRecordIdentifier:'; deprecated 'in 10_4, 10_7';
    procedure clientRefusedChangesForRecordWithIdentifier (recordId: NSString); message 'clientRefusedChangesForRecordWithIdentifier:'; deprecated 'in 10_4, 10_7';
    procedure clientCommittedAcceptedChanges; message 'clientCommittedAcceptedChanges'; deprecated 'in 10_4, 10_7';
    procedure clientCommittedAcceptedChangesWithNextAnchors (anchors: NSDictionary); message 'clientCommittedAcceptedChangesWithNextAnchors:'; deprecated 'in 10_5, 10_7';
    procedure clientChangedRecordIdentifiers (oldToNew: NSDictionary); message 'clientChangedRecordIdentifiers:'; deprecated 'in 10_4, 10_7';
    function isCancelled: ObjCBOOL; message 'isCancelled'; deprecated 'in 10_4, 10_7';
    procedure cancelSyncing; message 'cancelSyncing'; deprecated 'in 10_4, 10_7';
    procedure finishSyncing; message 'finishSyncing'; deprecated 'in 10_4, 10_7';
    function clientInfoForRecordWithIdentifier (recordId: NSString): id; message 'clientInfoForRecordWithIdentifier:'; deprecated 'in 10_4, 10_7';
    procedure setClientInfo_forRecordWithIdentifier (clientInfo: NSCodingProtocol; recordId: NSString); message 'setClientInfo:forRecordWithIdentifier:'; deprecated 'in 10_4, 10_7';
    function snapshotOfRecordsInTruth: ISyncRecordSnapshot; message 'snapshotOfRecordsInTruth'; deprecated 'in 10_4, 10_7';
    procedure ping; message 'ping'; deprecated 'in 10_5, 10_7';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ISyncSessionCancelledException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncSessionUnavailableException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncInvalidRecordException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncInvalidRecordIdentifiersKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncInvalidRecordReasonsKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncInvalidRecordsKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncInvalidEntityException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncUnsupportedEntityException: NSString deprecated 'in 10_4, 10_7'; cvar; external;
  ISyncRecordEntityNameKey: NSString deprecated 'in 10_4, 10_7'; cvar; external;
{$endif}

