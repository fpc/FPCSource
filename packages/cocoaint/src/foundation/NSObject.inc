{ Parsed from Foundation.framework NSObject.h }


{ Types from NSDeprecatedMethods }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSCopyingProtocolPtr = ^NSCopyingProtocol;
  NSMutableCopyingProtocolPtr = ^NSMutableCopyingProtocol;
  NSCodingProtocolPtr = ^NSCodingProtocol;
  NSSecureCodingProtocolPtr = ^NSSecureCodingProtocol;
  NSDiscardableContentProtocolPtr = ^NSDiscardableContentProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSCopyingProtocol = objcprotocol external name 'NSCopying'
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;

type
  NSMutableCopyingProtocol = objcprotocol external name 'NSMutableCopying'
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  NSCodingProtocol = objcprotocol external name 'NSCoding'
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;

type
  NSSecureCodingProtocol = objcprotocol external name 'NSSecureCoding' (NSCodingProtocol)
  required
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSCoderMethods = objccategory external (NSObject)
    class function version: NSInteger; message 'version';
    class procedure setVersion (aVersion: NSInteger); message 'setVersion:';
    function classForCoder: pobjc_class; message 'classForCoder';
    function replacementObjectForCoder (aCoder: NSCoder): id; message 'replacementObjectForCoder:';
    function awakeAfterUsingCoder (aDecoder: NSCoder): id; message 'awakeAfterUsingCoder:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  NSDeprecatedMethods = objccategory external (NSObject)
  end;
{$endif}

{$endif}
{$ifdef PROTOCOLS}

type
  NSDiscardableContentProtocol = objcprotocol external name 'NSDiscardableContent'
  required
    function beginContentAccess: ObjCBOOL; message 'beginContentAccess';
    procedure endContentAccess; message 'endContentAccess';
    procedure discardContentIfPossible; message 'discardContentIfPossible';
    function isContentDiscarded: ObjCBOOL; message 'isContentDiscarded';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSDiscardableContentProxy = objccategory external (NSObject)
    function autoContentAccessingProxy: id; message 'autoContentAccessingProxy';
  end;
{$endif}

{$ifdef FUNCTIONS}
function NSAllocateObject(aClass: pobjc_class; extraBytes: NSUInteger; zone: NSZonePtr): id; cdecl; external;
procedure NSDeallocateObject(object_: id); cdecl; external;
function NSCopyObject(object_: id; extraBytes: NSUInteger; zone: NSZonePtr): id; cdecl; external; deprecated 'in 10_0, 10_8, 2_0, 6_0';
function NSShouldRetainWithZone(anObject: id; requestedZone: NSZonePtr): ObjCBOOL; cdecl; external;
procedure NSIncrementExtraRefCount(object_: id); cdecl; external;
function NSDecrementExtraRefCountWasZero(object_: id): ObjCBOOL; cdecl; external;
function NSExtraRefCount(object_: id): NSUInteger; cdecl; external;
{$endif}

