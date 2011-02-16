{ Parsed from Foundation.framework NSObject.h }

{$ifdef TYPES}
{$ifndef NSOBJECT_PAS_T}
{$define NSOBJECT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSOBJECT_PAS_R}
{$define NSOBJECT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSOBJECT_PAS_F}
{$define NSOBJECT_PAS_F}

{ Functions }
function NSAllocateObject(aClass: Pobjc_class; extraBytes: NSUInteger; zone: NSZonePtr): id; cdecl; external;
procedure NSDeallocateObject(object_: id); cdecl; external;
function NSCopyObject(object_: id; extraBytes: NSUInteger; zone: NSZonePtr): id; cdecl; external;
function NSShouldRetainWithZone(anObject: id; requestedZone: NSZonePtr): Boolean; cdecl; external;
procedure NSIncrementExtraRefCount(object_: id); cdecl; external;
function NSDecrementExtraRefCountWasZero(object_: id): Boolean; cdecl; external;
function NSExtraRefCount(object_: id): NSUInteger; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSOBJECT_PAS_S}
{$define NSOBJECT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSObjectProtocol = objcprotocol;
  NSCopyingProtocol = objcprotocol;
  NSMutableCopyingProtocol = objcprotocol;
  NSCodingProtocol = objcprotocol;
  NSDiscardableContentProtocol = objcprotocol;
  NSObject = objcclass;
  NSObjectPointer = ^NSObject;
  NSObjectPtr = NSObjectPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOBJECT_PAS_C}
{$define NSOBJECT_PAS_C}

{ NSObject }
  NSObject = objcclass external (NSObjectProtocol)
  private
    isa: Pobjc_class;
    
  public
    class procedure load; message 'load';
    class procedure initialize; message 'initialize';
    function init: id; message 'init';
    class function new_: id; message 'new';
    class function allocWithZone(zone_: NSZonePtr): id; message 'allocWithZone:';
    class function alloc: id; message 'alloc';
    procedure dealloc; message 'dealloc';
    procedure finalize; message 'finalize';
    function copy: id; message 'copy';
    function mutableCopy: id; message 'mutableCopy';
    class function copyWithZone(zone_: NSZonePtr): id; message 'copyWithZone:';
    class function mutableCopyWithZone(zone_: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function superclass: Pobjc_class; message 'superclass';
    class function classClass: Pobjc_class; message 'class';
    class function instancesRespondToSelector(aSelector: SEL): Boolean; message 'instancesRespondToSelector:';
    class function classConformsToProtocol(protocol: objc_protocol): Boolean; message 'conformsToProtocol:';
    function methodForSelector(aSelector: SEL): IMP; message 'methodForSelector:';
    class function instanceMethodForSelector(aSelector: SEL): IMP; message 'instanceMethodForSelector:';
    procedure doesNotRecognizeSelector(aSelector: SEL); message 'doesNotRecognizeSelector:';
    function forwardingTargetForSelector(aSelector: SEL): id; message 'forwardingTargetForSelector:';
    procedure forwardInvocation(anInvocation: NSInvocation); message 'forwardInvocation:';
    function methodSignatureForSelector(aSelector: SEL): NSMethodSignature; message 'methodSignatureForSelector:';
    class function instanceMethodSignatureForSelector(aSelector: SEL): NSMethodSignature; message 'instanceMethodSignatureForSelector:';
    class function description: NSString; message 'description';
    class function isSubclassOfClass(aClass: Pobjc_class): Boolean; message 'isSubclassOfClass:';
    class function resolveClassMethod(sel_: SEL): Boolean; message 'resolveClassMethod:';
    class function resolveInstanceMethod(sel_: SEL): Boolean; message 'resolveInstanceMethod:';

    { Adopted Protocols }
    function isEqual(object_: id): Boolean;
    function hash: NSUInteger;
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

{ NSCoderMethodsCategory }
  NSCoderMethodsCategory = objccategory external (NSObject)
    class function version: NSInteger; message 'version';
    class procedure setVersion(aVersion: NSInteger); message 'setVersion:';
    function classForCoder: Pobjc_class; message 'classForCoder';
    function replacementObjectForCoder(aCoder: NSCoder): id; message 'replacementObjectForCoder:';
    function awakeAfterUsingCoder(aDecoder: NSCoder): id; message 'awakeAfterUsingCoder:';
  end;

{ NSDeprecatedMethodsCategory }
  NSDeprecatedMethodsCategory = objccategory external (NSObject)
    class procedure poseAsClass(aClass: Pobjc_class); message 'poseAsClass:';
  end;

{ NSDiscardableContentProxyCategory }
  NSDiscardableContentProxyCategory = objccategory external (NSObject)
    function autoContentAccessingProxy: id; message 'autoContentAccessingProxy';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSOBJECT_PAS_P}
{$define NSOBJECT_PAS_P}
  
{ NSObject Protocol }
  NSObjectProtocol = objcprotocol external name 'NSObject'
    function isEqual(object_: id): Boolean; message 'isEqual:';
    function hash: NSUInteger; message 'hash';
    function superclass: Pobjc_class; message 'superclass';
    function _class: Pobjc_class; message 'class';
    function self: id; message 'self';
    function zone: NSZonePtr; message 'zone';
    function performSelector(aSelector: SEL): id; message 'performSelector:';
    function performSelector_withObject(aSelector: SEL; object_: id): id; message 'performSelector:withObject:';
    function performSelector_withObject_withObject(aSelector: SEL; object_: id; object_1: id): id; message 'performSelector:withObject:withObject:';
    function isProxy: Boolean; message 'isProxy';
    function isKindOfClass(aClass: Pobjc_class): Boolean; message 'isKindOfClass:';
    function isMemberOfClass(aClass: Pobjc_class): Boolean; message 'isMemberOfClass:';
    function conformsToProtocol(aProtocol: objc_protocol): Boolean; message 'conformsToProtocol:';
    function respondsToSelector(aSelector: SEL): Boolean; message 'respondsToSelector:';
    function retain: id; message 'retain';
    procedure release; message 'release';
    function autorelease: id; message 'autorelease';
    function retainCount: NSUInteger; message 'retainCount';
    function description: NSString; message 'description';
  end;
  
{ NSCopying Protocol }
  NSCopyingProtocol = objcprotocol external name 'NSCopying'
    function copyWithZone(zone_: NSZonePtr): id; message 'copyWithZone:';
  end;
  
{ NSMutableCopying Protocol }
  NSMutableCopyingProtocol = objcprotocol external name 'NSMutableCopying'
    function mutableCopyWithZone(zone_: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;
  
{ NSCoding Protocol }
  NSCodingProtocol = objcprotocol external name 'NSCoding'
    procedure encodeWithCoder(aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder(aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
  
{ NSDiscardableContent Protocol }
  NSDiscardableContentProtocol = objcprotocol external name 'NSDiscardableContent'
  required
    function beginContentAccess: Boolean; message 'beginContentAccess';
    procedure endContentAccess; message 'endContentAccess';
    procedure discardContentIfPossible; message 'discardContentIfPossible';
    function isContentDiscarded: Boolean; message 'isContentDiscarded';
  end;
{$endif}
{$endif}
