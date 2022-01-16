{ Parsed from CloudKit.framework CKModifyBadgeOperation.h }


{$ifdef TYPES}
type
  CKModifyBadgeOperationPtr = ^CKModifyBadgeOperation;
{$endif}

{$ifdef CLASSES}

type
  CKModifyBadgeOperation = objcclass external (CKOperation)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithBadgeValue (badgeValue: NSUInteger): instancetype; message 'initWithBadgeValue:';
    procedure setBadgeValue(newValue: NSUInteger); message 'setBadgeValue:';
    function badgeValue: NSUInteger; message 'badgeValue';
    procedure setModifyBadgeCompletionBlock(newValue: OpaqueCBlock); message 'setModifyBadgeCompletionBlock:';
    function modifyBadgeCompletionBlock: OpaqueCBlock; message 'modifyBadgeCompletionBlock';
  end;
{$endif}

