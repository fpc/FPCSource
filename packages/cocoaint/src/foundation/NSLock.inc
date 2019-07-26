{ Parsed from Foundation.framework NSLock.h }


{$ifdef TYPES}
type
  NSLockPtr = ^NSLock;
  NSConditionLockPtr = ^NSConditionLock;
  NSRecursiveLockPtr = ^NSRecursiveLock;
  NSConditionPtr = ^NSCondition;
  NSLockingProtocolPtr = ^NSLockingProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSLockingProtocol = objcprotocol external name 'NSLocking'
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSLock = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: pointer;
  public
    function tryLock: ObjCBOOL; message 'tryLock';
    function lockBeforeDate (limit: NSDate): ObjCBOOL; message 'lockBeforeDate:';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted protocols }
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;

type
  NSConditionLock = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: pointer;
  public
    function initWithCondition (condition: NSInteger): instancetype; message 'initWithCondition:'; { NS_DESIGNATED_INITIALIZER }
    function condition: NSInteger; message 'condition';
    procedure lockWhenCondition (condition_: NSInteger); message 'lockWhenCondition:';
    function tryLock: ObjCBOOL; message 'tryLock';
    function tryLockWhenCondition (condition_: NSInteger): ObjCBOOL; message 'tryLockWhenCondition:';
    procedure unlockWithCondition (condition_: NSInteger); message 'unlockWithCondition:';
    function lockBeforeDate (limit: NSDate): ObjCBOOL; message 'lockBeforeDate:';
    function lockWhenCondition_beforeDate (condition_: NSInteger; limit: NSDate): ObjCBOOL; message 'lockWhenCondition:beforeDate:';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted protocols }
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;

type
  NSRecursiveLock = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: pointer;
  public
    function tryLock: ObjCBOOL; message 'tryLock';
    function lockBeforeDate (limit: NSDate): ObjCBOOL; message 'lockBeforeDate:';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted protocols }
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;

type
  NSCondition = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: pointer;
  public
    procedure wait; message 'wait';
    function waitUntilDate (limit: NSDate): ObjCBOOL; message 'waitUntilDate:';
    procedure signal; message 'signal';
    procedure broadcast; message 'broadcast';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted protocols }
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;
{$endif}

