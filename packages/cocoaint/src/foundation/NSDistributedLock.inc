{ Parsed from Foundation.framework NSDistributedLock.h }


{$ifdef TYPES}
type
  NSDistributedLockPtr = ^NSDistributedLock;
{$endif}

{$ifdef CLASSES}

type
  NSDistributedLock = objcclass external (NSObject)
  private
    _priv: pointer;
  public
    class function lockWithPath (path: NSString): NSDistributedLock; message 'lockWithPath:';
    function init: instancetype; message 'init';
    function initWithPath (path: NSString): instancetype; message 'initWithPath:'; { NS_DESIGNATED_INITIALIZER }
    function tryLock: ObjCBOOL; message 'tryLock';
    procedure unlock; message 'unlock';
    procedure breakLock; message 'breakLock';
    function lockDate: NSDate; message 'lockDate';
  end;
{$endif}

