{ Parsed from Foundation.framework NSProxy.h }


{$ifdef TYPES}
type
  NSProxyPtr = ^NSProxy;
{$endif}

{$ifdef CLASSES}

type
  NSProxy = objcclass external (NSObject, NSObjectProtocol)
  private
    isa: pobjc_class;
  public
    class function alloc: id; message 'alloc';
    class function allocWithZone (zone: NSZonePtr): id; message 'allocWithZone:';
    class function class_: pobjc_class; message 'class';
    procedure forwardInvocation (invocation: NSInvocation); message 'forwardInvocation:';
    function methodSignatureForSelector (sel_: SEL): NSMethodSignature; message 'methodSignatureForSelector:';
    procedure dealloc; message 'dealloc';
    procedure finalize; message 'finalize';
    function description: NSString; message 'description';
    function debugDescription: NSString; message 'debugDescription';
    class function classRespondsToSelector (aSelector: SEL): ObjCBOOL; message 'respondsToSelector:';
    function allowsWeakReference: ObjCBOOL; message 'allowsWeakReference';
    function retainWeakReference: ObjCBOOL; message 'retainWeakReference';
  end;
{$endif}

