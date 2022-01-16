{ Parsed from CloudKit.framework CKFetchSubscriptionsOperation.h }


{$ifdef TYPES}
type
  CKFetchSubscriptionsOperationPtr = ^CKFetchSubscriptionsOperation;
{$endif}

{$ifdef CLASSES}

type
  CKFetchSubscriptionsOperation = objcclass external (CKDatabaseOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    class function fetchAllSubscriptionsOperation: instancetype; message 'fetchAllSubscriptionsOperation';
    function initWithSubscriptionIDs (subscriptionIDs: NSArray): instancetype; message 'initWithSubscriptionIDs:';
    procedure setSubscriptionIDs(newValue: NSArray); message 'setSubscriptionIDs:';
    function subscriptionIDs: NSArray; message 'subscriptionIDs';
    procedure setFetchSubscriptionCompletionBlock(newValue: OpaqueCBlock); message 'setFetchSubscriptionCompletionBlock:';
    function fetchSubscriptionCompletionBlock: OpaqueCBlock; message 'fetchSubscriptionCompletionBlock';
  end;
{$endif}

