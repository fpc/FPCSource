{ Parsed from Foundation.framework NSProxy.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPROXY_PAS_T}
{$define NSPROXY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROXY_PAS_R}
{$define NSPROXY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROXY_PAS_F}
{$define NSPROXY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROXY_PAS_S}
{$define NSPROXY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSProxy = objcclass;
  NSProxyPointer = ^NSProxy;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROXY_PAS_C}
{$define NSPROXY_PAS_C}

{ NSProxy }
  NSProxy = objcclass(NSObject, NSObjectProtocol)
  private
    _isa: Pobjc_class;
    
  public
    class function alloc: NSProxy; message 'alloc';

    class function alloc: id; message 'alloc';
    class function allocWithZone(zone_: NSZonePointer): id; message 'allocWithZone:';
    class function class_: Pobjc_class; message 'class';
    procedure forwardInvocation(invocation: NSInvocation); message 'forwardInvocation:';
    function methodSignatureForSelector(sel: SEL): NSMethodSignature; message 'methodSignatureForSelector:';
    procedure dealloc; message 'dealloc';
    procedure finalize; message 'finalize';
    function description: NSString; message 'description';
    class function respondsToSelector(aSelector: SEL): Boolean; message 'respondsToSelector:';
  end; external;

{$endif}
{$endif}
