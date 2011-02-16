{ Parsed from Foundation.framework NSProxy.h }

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
  NSProxyPtr = NSProxyPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROXY_PAS_C}
{$define NSPROXY_PAS_C}

{ NSProxy }
  NSProxy = objcclass external (NSObjectProtocol)
  private
    isa: Pobjc_class;
    
  public
    class function alloc: id; message 'alloc';
    class function allocWithZone(zone_: NSZonePtr): id; message 'allocWithZone:';
    class function classClass: Pobjc_class; message 'class';
    procedure forwardInvocation(invocation: NSInvocation); message 'forwardInvocation:';
    function methodSignatureForSelector(sel_: SEL): NSMethodSignature; message 'methodSignatureForSelector:';
    procedure dealloc; message 'dealloc';
    procedure finalize; message 'finalize';
    function description: NSString; message 'description';
    class function classRespondsToSelector(aSelector: SEL): Boolean; message 'respondsToSelector:';

    { Adopted Protocols }
    function isEqual(object_: id): Boolean;
    function hash: NSUInteger;
    function superclass: Pobjc_class;
    function _class: Pobjc_class;
    function self: id;
    function zone: NSZonePtr;
    function performSelector(aSelector: SEL): id;
    function performSelector_withObject(aSelector: SEL; object_: id): id;
    function performSelector_withObject_withObject(aSelector: SEL; object_: id; object_1: id): id;
    function isProxy: Boolean;
    function isKindOfClass(aClass: Pobjc_class): Boolean;
    function isMemberOfClass(aClass: Pobjc_class): Boolean;
    function conformsToProtocol(aProtocol: objc_protocol): Boolean;
    function respondsToSelector(aSelector: SEL): Boolean;
    function retain: id;
    procedure release;
    function autorelease: id;
    function retainCount: NSUInteger;
  end;

{$endif}
{$endif}
