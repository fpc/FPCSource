{ Parsed from Foundation.framework NSDistributedLock.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSDISTRIBUTEDLOCK_PAS_C}
{$define NSDISTRIBUTEDLOCK_PAS_C}

{ NSDistributedLock }
  NSDistributedLock = objcclass(NSObject)
  private
    __priv: Pointer;
    
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
