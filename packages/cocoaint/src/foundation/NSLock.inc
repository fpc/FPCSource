{ Parsed from Foundation.framework NSLock.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSLOCK_PAS_H}
{$define NSLOCK_PAS_H}
type
  NSLockPointer = Pointer;
  NSConditionLockPointer = Pointer;
  NSRecursiveLockPointer = Pointer;
  NSConditionPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSLOCK_PAS_T}
{$define NSLOCK_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSLockingProtocol = objcprotocol;
  NSLock = objcclass;
  NSConditionLock = objcclass;
  NSRecursiveLock = objcclass;
  NSCondition = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSLOCK_PAS_C}
{$define NSLOCK_PAS_C}

{ NSLock }
  NSLock = objcclass(NSObject, NSLockingProtocol)
  private
    __priv: Pointer;
    
  public
    class function alloc: NSLock; message 'alloc';

    function tryLock: Boolean; message 'tryLock';
    function lockBeforeDate(limit: NSDate): Boolean; message 'lockBeforeDate:';
    procedure setName(n: NSString); message 'setName:';
  end; external;

{ NSConditionLock }
  NSConditionLock = objcclass(NSObject, NSLockingProtocol)
  private
    __priv: Pointer;
    
  public
    class function alloc: NSConditionLock; message 'alloc';

    function initWithCondition(condition_: clong): id; message 'initWithCondition:';
    function condition: clong; message 'condition';
    procedure lockWhenCondition(condition_: clong); message 'lockWhenCondition:';
    function tryLock: Boolean; message 'tryLock';
    function tryLockWhenCondition(condition_: clong): Boolean; message 'tryLockWhenCondition:';
    procedure unlockWithCondition(condition_: clong); message 'unlockWithCondition:';
    function lockBeforeDate(limit: NSDate): Boolean; message 'lockBeforeDate:';
    function lockWhenCondition_beforeDate(condition_: clong; limit: NSDate): Boolean; message 'lockWhenCondition:beforeDate:';
    procedure setName(n: NSString); message 'setName:';
  end; external;

{ NSRecursiveLock }
  NSRecursiveLock = objcclass(NSObject, NSLockingProtocol)
  private
    __priv: Pointer;
    
  public
    class function alloc: NSRecursiveLock; message 'alloc';

    function tryLock: Boolean; message 'tryLock';
    function lockBeforeDate(limit: NSDate): Boolean; message 'lockBeforeDate:';
    procedure setName(n: NSString); message 'setName:';
  end; external;

{ NSCondition }
  NSCondition = objcclass(NSObject, NSLockingProtocol)
  private
    __priv: Pointer;
    
  public
    class function alloc: NSCondition; message 'alloc';

    procedure wait; message 'wait';
    function waitUntilDate(limit: NSDate): Boolean; message 'waitUntilDate:';
    procedure signal; message 'signal';
    procedure broadcast; message 'broadcast';
    procedure setName(n: NSString); message 'setName:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSLOCK_PAS_P}
{$define NSLOCK_PAS_P}
  
{ NSLocking Protocol }
  NSLockingProtocol = objcprotocol
    procedure lock; message 'lock';
    procedure unlock; message 'unlock';
  end; external name 'NSLocking';
{$endif}
{$endif}
