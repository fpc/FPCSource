{ Parsed from Foundation.framework NSDistributedLock.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSDISTRIBUTEDLOCK_PAS_T}
{$define NSDISTRIBUTEDLOCK_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDISTRIBUTEDLOCK_PAS_R}
{$define NSDISTRIBUTEDLOCK_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDISTRIBUTEDLOCK_PAS_F}
{$define NSDISTRIBUTEDLOCK_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDISTRIBUTEDLOCK_PAS_S}
{$define NSDISTRIBUTEDLOCK_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDistributedLock = objcclass;
  NSDistributedLockPointer = ^NSDistributedLock;
  NSDistributedLockPtr = NSDistributedLockPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDISTRIBUTEDLOCK_PAS_C}
{$define NSDISTRIBUTEDLOCK_PAS_C}

{ NSDistributedLock }
  NSDistributedLock = objcclass(NSObject)
  private
    _priv: Pointer;
    
  public
    class function alloc: NSDistributedLock; message 'alloc';

    class function lockWithPath(path: NSString): NSDistributedLock; message 'lockWithPath:';
    function initWithPath(path: NSString): id; message 'initWithPath:';
    function tryLock: Boolean; message 'tryLock';
    procedure unlock; message 'unlock';
    procedure breakLock; message 'breakLock';
    function lockDate: NSDate; message 'lockDate';
  end; external;

{$endif}
{$endif}
