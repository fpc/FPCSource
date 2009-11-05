{ Parsed from Foundation.framework NSAutoreleasePool.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSAUTORELEASEPOOL_PAS_C}
{$define NSAUTORELEASEPOOL_PAS_C}

{ NSAutoreleasePool }
  NSAutoreleasePool = objcclass(NSObject)
  private
    __token: Pointer;
    __reserved3: Pointer;
    __reserved2: Pointer;
    __reserved: Pointer;
    
  public
    class function alloc: NSAutoreleasePool; message 'alloc';

    class procedure addObject(anObject: id); message 'addObject:';
    procedure drain; message 'drain';
  end; external;

{$endif}
{$endif}
