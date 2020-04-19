{ Parsed from Foundation.framework NSProxy.h }


{$ifdef TYPES}
type
  NSProxyPtr = ^NSProxy;
{$endif}

{$ifdef CLASSES}

type
  NSProxy = objcclass external (NSObjectProtocol)
  private
    isa: pobjc_class;
  public
    class function alloc: id; message 'alloc';
    class function allocWithZone (zone: NSZonePtr): id; message 'allocWithZone:';
    class function classClass: pobjc_class; message 'class';
    procedure forwardInvocation (invocation: NSInvocation); message 'forwardInvocation:';
    function methodSignatureForSelector (sel_: SEL): NSMethodSignature; message 'methodSignatureForSelector:';
    procedure dealloc; message 'dealloc';
    procedure finalize; message 'finalize';
    function description: NSString; message 'description';
    function debugDescription: NSString; message 'debugDescription';
    class function classRespondsToSelector (aSelector: SEL): ObjCBOOL; message 'respondsToSelector:';
    function allowsWeakReference: ObjCBOOL; message 'allowsWeakReference';
    function retainWeakReference: ObjCBOOL; message 'retainWeakReference';

    function isEqual(obj: id): ObjCBOOL; message 'isEqual:';
    function hash: NSUInteger; message 'hash';

    function superclass: pobjc_class; message 'superclass';
    function _class: pobjc_class; message 'class';
    function self: id;  message 'self';
    function zone: PNSZone;  message 'zone';

    function performSelector(aSelector: SEL): id; message 'performSelector:';
    function performSelector_withObject(aSelector: SEL; obj: id): id; message 'performSelector:withObject:';
    function performSelector_withObject_withObject(aSelector: SEL; obj1, obj2: id): id; message 'performSelector:withObject:withObject:';

    function isProxy: ObjCBOOL; message 'isProxy';

    function isKindOfClass(aClass: pobjc_class): ObjCBOOL; message 'isKindOfClass:';
    function isMemberOfClass(aClass: pobjc_class): ObjCBOOL; message 'isMemberOfClass:';
    function conformsToProtocol(aProtocol: Protocol): ObjCBOOL; message 'conformsToProtocol:';

    function respondsToSelector(aSelector: SEL): ObjCBOOL; message 'respondsToSelector:';

    function retain: id; message 'retain';
    procedure release;  message 'release'; { oneway }
    function autorelease: id; message 'autorelease';
    function retainCount: NSUInteger; message 'retainCount';

  end;
{$endif}

