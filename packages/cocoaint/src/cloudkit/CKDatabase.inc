{ Parsed from CloudKit.framework CKDatabase.h }


{$ifdef TYPES}
type
  CKDatabasePtr = ^CKDatabase;
{$endif}

{$ifdef CLASSES}

type
  CKDatabase = objcclass external (NSObject)
  public
    function init: instancetype; message 'init';
    procedure addOperation (operation: CKDatabaseOperation); message 'addOperation:';
  end;


type
  ConvenienceMethods = objccategory external (CKDatabase)
    procedure fetchRecordWithID_completionHandler (recordID: CKRecordID; completionHandler: OpaqueCBlock); message 'fetchRecordWithID:completionHandler:';
    procedure saveRecord_completionHandler (record_: CKRecord; completionHandler: OpaqueCBlock); message 'saveRecord:completionHandler:';
    procedure deleteRecordWithID_completionHandler (recordID: CKRecordID; completionHandler: OpaqueCBlock); message 'deleteRecordWithID:completionHandler:';
    procedure performQuery_inZoneWithID_completionHandler (query: CKQuery; zoneID: CKRecordZoneID; completionHandler: OpaqueCBlock); message 'performQuery:inZoneWithID:completionHandler:';
    procedure fetchAllRecordZonesWithCompletionHandler (completionHandler: OpaqueCBlock); message 'fetchAllRecordZonesWithCompletionHandler:';
    procedure fetchRecordZoneWithID_completionHandler (zoneID: CKRecordZoneID; completionHandler: OpaqueCBlock); message 'fetchRecordZoneWithID:completionHandler:';
    procedure saveRecordZone_completionHandler (zone: CKRecordZone; completionHandler: OpaqueCBlock); message 'saveRecordZone:completionHandler:';
    procedure deleteRecordZoneWithID_completionHandler (zoneID: CKRecordZoneID; completionHandler: OpaqueCBlock); message 'deleteRecordZoneWithID:completionHandler:';
    procedure fetchSubscriptionWithID_completionHandler (subscriptionID: NSString; completionHandler: OpaqueCBlock); message 'fetchSubscriptionWithID:completionHandler:';
    procedure fetchAllSubscriptionsWithCompletionHandler (completionHandler: OpaqueCBlock); message 'fetchAllSubscriptionsWithCompletionHandler:';
    procedure saveSubscription_completionHandler (subscription: CKSubscription; completionHandler: OpaqueCBlock); message 'saveSubscription:completionHandler:';
    procedure deleteSubscriptionWithID_completionHandler (subscriptionID: NSString; completionHandler: OpaqueCBlock); message 'deleteSubscriptionWithID:completionHandler:';
  end;
{$endif}

