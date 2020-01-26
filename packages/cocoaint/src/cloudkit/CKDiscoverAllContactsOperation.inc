{ Parsed from CloudKit.framework CKDiscoverAllContactsOperation.h }


{$ifdef TYPES}
type
  CKDiscoverAllContactsOperationPtr = ^CKDiscoverAllContactsOperation;
{$endif}

{$ifdef CLASSES}

type
  CKDiscoverAllContactsOperation = objcclass external (CKOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure setDiscoverAllContactsCompletionBlock(newValue: OpaqueCBlock); message 'setDiscoverAllContactsCompletionBlock:';
    function discoverAllContactsCompletionBlock: OpaqueCBlock; message 'discoverAllContactsCompletionBlock';
  end;
{$endif}

