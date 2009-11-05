{ Parsed from Foundation.framework NSCoder.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSCODER_PAS_T}
{$define NSCODER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCODER_PAS_R}
{$define NSCODER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCODER_PAS_F}
{$define NSCODER_PAS_F}

{ Functions }
function NXReadNSObjectFromCoder(decoderPointer: NSCoder): NSObject; cdecl; external name 'NXReadNSObjectFromCoder';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCODER_PAS_S}
{$define NSCODER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCoder = objcclass;
  NSCoderPointer = ^NSCoder;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCODER_PAS_C}
{$define NSCODER_PAS_C}

{ NSCoder }
  NSCoder = objcclass(NSObject)
    
  public
    class function alloc: NSCoder; message 'alloc';

    procedure encodeValueOfObjCType_at(type_: PChar; addr: Pointer); message 'encodeValueOfObjCType:at:';
    procedure encodeDataObject(data: NSData); message 'encodeDataObject:';
    procedure decodeValueOfObjCType_at(type_: PChar; data: Pointer); message 'decodeValueOfObjCType:at:';
    function decodeDataObject: NSData; message 'decodeDataObject';
    function versionForClassName(className_: NSString): NSInteger; message 'versionForClassName:';

    { Category: NSExtendedCoder }
    procedure encodeObject(object_: id); message 'encodeObject:';
    procedure encodePropertyList(aPropertyList: id); message 'encodePropertyList:';
    procedure encodeRootObject(rootObject: id); message 'encodeRootObject:';
    procedure encodeBycopyObject(anObject: id); message 'encodeBycopyObject:';
    procedure encodeByrefObject(anObject: id); message 'encodeByrefObject:';
    procedure encodeConditionalObject(object_: id); message 'encodeConditionalObject:';
    procedure encodeValuesOfObjCTypes(types: PChar); varargs; message 'encodeValuesOfObjCTypes:';
    procedure encodeArrayOfObjCType_count_at(type_: PChar; count: NSUInteger; array_: Pointer); message 'encodeArrayOfObjCType:count:at:';
    procedure encodeBytes_length(byteaddr: Pointer; length: NSUInteger); message 'encodeBytes:length:';
    function decodeObject: id; message 'decodeObject';
    function decodePropertyList: id; message 'decodePropertyList';
    procedure decodeValuesOfObjCTypes(types: PChar); varargs; message 'decodeValuesOfObjCTypes:';
    procedure decodeArrayOfObjCType_count_at(itemType: PChar; count: NSUInteger; array_: Pointer); message 'decodeArrayOfObjCType:count:at:';
    function decodeBytesWithReturnedLength(lengthp: NSUIntegerPointer): Pointer; message 'decodeBytesWithReturnedLength:';
    procedure setObjectZone(zone_: NSZonePointer); message 'setObjectZone:';
    function objectZone: NSZone; message 'objectZone';
    function systemVersion: cuint; message 'systemVersion';
    function allowsKeyedCoding: Boolean; message 'allowsKeyedCoding';
    procedure encodeObject_forKey(objv: id; key: NSString); message 'encodeObject:forKey:';
    procedure encodeConditionalObject_forKey(objv: id; key: NSString); message 'encodeConditionalObject:forKey:';
    procedure encodeBool_forKey(boolv: Boolean; key: NSString); message 'encodeBool:forKey:';
    procedure encodeInt_forKey(intv: cint; key: NSString); message 'encodeInt:forKey:';
    procedure encodeInt32_forKey(intv: longint; key: NSString); message 'encodeInt32:forKey:';
    procedure encodeInt64_forKey(intv: clonglong; key: NSString); message 'encodeInt64:forKey:';
    procedure encodeFloat_forKey(realv: single; key: NSString); message 'encodeFloat:forKey:';
    procedure encodeDouble_forKey(realv: double; key: NSString); message 'encodeDouble:forKey:';
    procedure encodeBytes_length_forKey(bytesp: pbyte; lenv: NSUInteger; key: NSString); message 'encodeBytes:length:forKey:';
    function containsValueForKey(key: NSString): Boolean; message 'containsValueForKey:';
    function decodeObjectForKey(key: NSString): id; message 'decodeObjectForKey:';
    function decodeBoolForKey(key: NSString): Boolean; message 'decodeBoolForKey:';
    function decodeIntForKey(key: NSString): cint; message 'decodeIntForKey:';
    function decodeInt32ForKey(key: NSString): longint; message 'decodeInt32ForKey:';
    function decodeInt64ForKey(key: NSString): clonglong; message 'decodeInt64ForKey:';
    function decodeFloatForKey(key: NSString): single; message 'decodeFloatForKey:';
    function decodeDoubleForKey(key: NSString): double; message 'decodeDoubleForKey:';
    function decodeBytesForKey_returnedLength(key: NSString; lengthp: NSUIntegerPointer): byte; message 'decodeBytesForKey:returnedLength:';
    procedure encodeInteger_forKey(intv: NSInteger; key: NSString); message 'encodeInteger:forKey:';
    function decodeIntegerForKey(key: NSString): NSInteger; message 'decodeIntegerForKey:';

    { Category: NSTypedstreamCompatibility }
    procedure encodeNXObject(object_: id); message 'encodeNXObject:';

    { Category: NSGeometryKeyedCoding }
    procedure encodePoint_forKey(point: NSPoint; key: NSString); message 'encodePoint:forKey:';
    procedure encodeSize_forKey(size: NSSize; key: NSString); message 'encodeSize:forKey:';
    procedure encodeRect_forKey(rect: NSRect; key: NSString); message 'encodeRect:forKey:';
    function decodePointForKey(key: NSString): NSPoint; message 'decodePointForKey:';
    function decodeSizeForKey(key: NSString): NSSize; message 'decodeSizeForKey:';
    function decodeRectForKey(key: NSString): NSRect; message 'decodeRectForKey:';

    { Category: NSAppKitColorExtensions }
    function decodeNXColor: NSColor; message 'decodeNXColor';
  end; external;

{$endif}
{$endif}
