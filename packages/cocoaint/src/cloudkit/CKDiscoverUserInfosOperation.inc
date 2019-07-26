{ Parsed from CloudKit.framework CKDiscoverUserInfosOperation.h }


{$ifdef TYPES}
type
  CKDiscoverUserInfosOperationPtr = ^CKDiscoverUserInfosOperation;
{$endif}

{$ifdef CLASSES}

type
  CKDiscoverUserInfosOperation = objcclass external (CKOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithEmailAddresses_userRecordIDs (emailAddresses: NSArray; userRecordIDs: NSArray): instancetype; message 'initWithEmailAddresses:userRecordIDs:';
    procedure setEmailAddresses(newValue: NSArray); message 'setEmailAddresses:';
    function emailAddresses: NSArray; message 'emailAddresses';
    procedure setUserRecordIDs(newValue: NSArray); message 'setUserRecordIDs:';
    function userRecordIDs: NSArray; message 'userRecordIDs';
    procedure setDiscoverUserInfosCompletionBlock(newValue: OpaqueCBlock); message 'setDiscoverUserInfosCompletionBlock:';
    function discoverUserInfosCompletionBlock: OpaqueCBlock; message 'discoverUserInfosCompletionBlock';
  end;
{$endif}

