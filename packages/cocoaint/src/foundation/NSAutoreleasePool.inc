{ Parsed from Foundation.framework NSAutoreleasePool.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSAUTORELEASEPOOL_PAS_T}
{$define NSAUTORELEASEPOOL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAUTORELEASEPOOL_PAS_R}
{$define NSAUTORELEASEPOOL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAUTORELEASEPOOL_PAS_F}
{$define NSAUTORELEASEPOOL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAUTORELEASEPOOL_PAS_S}
{$define NSAUTORELEASEPOOL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSAutoreleasePool = objcclass;
  NSAutoreleasePoolPointer = ^NSAutoreleasePool;
  NSAutoreleasePoolPtr = NSAutoreleasePoolPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSAUTORELEASEPOOL_PAS_C}
{$define NSAUTORELEASEPOOL_PAS_C}

{ NSAutoreleasePool }
  NSAutoreleasePool = objcclass(NSObject)
  private
    _token: Pointer;
    _reserved3: Pointer;
    _reserved2: Pointer;
    _reserved: Pointer;
    
  public
    class function alloc: NSAutoreleasePool; message 'alloc';

    class procedure addObject(anObject: id); message 'addObject:';
    procedure drain; message 'drain';
  end; external;

{$endif}
{$endif}
