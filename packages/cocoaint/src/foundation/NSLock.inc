{ Parsed from Foundation.framework NSLock.h }

{$ifdef TYPES}
{$ifndef NSLOCK_PAS_T}
{$define NSLOCK_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSLOCK_PAS_R}
{$define NSLOCK_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSLOCK_PAS_F}
{$define NSLOCK_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSLOCK_PAS_S}
{$define NSLOCK_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLockingProtocol = objcprotocol;
  NSLock = objcclass;
  NSLockPointer = ^NSLock;
  NSLockPtr = NSLockPointer;
  NSConditionLock = objcclass;
  NSConditionLockPointer = ^NSConditionLock;
  NSConditionLockPtr = NSConditionLockPointer;
  NSRecursiveLock = objcclass;
  NSRecursiveLockPointer = ^NSRecursiveLock;
  NSRecursiveLockPtr = NSRecursiveLockPointer;
  NSCondition = objcclass;
  NSConditionPointer = ^NSCondition;
  NSConditionPtr = NSConditionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLOCK_PAS_C}
{$define NSLOCK_PAS_C}

{ NSLock }
  NSLock = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: Pointer;
    
  public
    function tryLock: Boolean; message 'tryLock';
    function lockBeforeDate(limit: NSDate): Boolean; message 'lockBeforeDate:';
    procedure setName(n: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted Protocols }
    procedure lock;
    procedure unlock;
  end;

{ NSConditionLock }
  NSConditionLock = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: Pointer;
    
  public
    function initWithCondition(condition_: NSInteger): id; message 'initWithCondition:';
    function condition: NSInteger; message 'condition';
    procedure lockWhenCondition(condition_: NSInteger); message 'lockWhenCondition:';
    function tryLock: Boolean; message 'tryLock';
    function tryLockWhenCondition(condition_: NSInteger): Boolean; message 'tryLockWhenCondition:';
    procedure unlockWithCondition(condition_: NSInteger); message 'unlockWithCondition:';
    function lockBeforeDate(limit: NSDate): Boolean; message 'lockBeforeDate:';
    function lockWhenCondition_beforeDate(condition_: NSInteger; limit: NSDate): Boolean; message 'lockWhenCondition:beforeDate:';
    procedure setName(n: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted Protocols }
    procedure lock;
    procedure unlock;
  end;

{ NSRecursiveLock }
  NSRecursiveLock = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: Pointer;
    
  public
    function tryLock: Boolean; message 'tryLock';
    function lockBeforeDate(limit: NSDate): Boolean; message 'lockBeforeDate:';
    procedure setName(n: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted Protocols }
    procedure lock;
    procedure unlock;
  end;

{ NSCondition }
  NSCondition = objcclass external (NSObject, NSLockingProtocol)
  private
    _priv: Pointer;
    
  public
    procedure wait; message 'wait';
    function waitUntilDate(limit: NSDate): Boolean; message 'waitUntilDate:';
    procedure signal; message 'signal';
    procedure broadcast; message 'broadcast';
    procedure setName(n: NSString); message 'setName:';
    function name: NSString; message 'name';

    { Adopted Protocols }
    procedure lock;
    procedure unlock;
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSLOCK_PAS_P}
{$define NSLOCK_PAS_P}
  
{ NSLocking Protocol }
  NSLockingProtocol = objcprotocol external name 'NSLocking'
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end;
{$endif}
{$endif}
