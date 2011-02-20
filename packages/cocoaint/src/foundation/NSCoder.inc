{ Parsed from Foundation.framework NSCoder.h }

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
function NXReadNSObjectFromCoder(decoder: NSCoder): NSObject; cdecl; external; deprecated 'in Mac OS X 10.5 and later';

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
  NSCoderPtr = NSCoderPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCODER_PAS_C}
{$define NSCODER_PAS_C}

{ NSCoder }
  NSCoder = objcclass external (NSObject)
    
  public
    procedure encodeValueOfObjCType_at(type_: PChar; addr: Pointer); message 'encodeValueOfObjCType:at:';
    procedure encodeDataObject(data: NSData); message 'encodeDataObject:';
    procedure decodeValueOfObjCType_at(type_: PChar; data: Pointer); message 'decodeValueOfObjCType:at:';
    function decodeDataObject: NSData; message 'decodeDataObject';
    function versionForClassName(className: NSString): NSInteger; message 'versionForClassName:';
  end;

{ NSExtendedCoderCategory }
  NSExtendedCoderCategory = objccategory external (NSCoder)
    procedure encodeObject(object_: id); message 'encodeObject:';
    procedure encodeRootObject(rootObject: id); message 'encodeRootObject:';
    procedure encodeBycopyObject(anObject: id); message 'encodeBycopyObject:';
    procedure encodeByrefObject(anObject: id); message 'encodeByrefObject:';
    procedure encodeConditionalObject(object_: id); message 'encodeConditionalObject:';
    procedure encodeValuesOfObjCTypes(firstKey: id); varargs; message 'encodeValuesOfObjCTypes:';
    procedure encodeArrayOfObjCType_count_at(type_: PChar; count: NSUInteger; array_: Pointer); message 'encodeArrayOfObjCType:count:at:';
    procedure encodeBytes_length(byteaddr: Pointer; length: NSUInteger); message 'encodeBytes:length:';
    function decodeObject: id; message 'decodeObject';
    procedure decodeValuesOfObjCTypes(firstKey: id); varargs; message 'decodeValuesOfObjCTypes:';
    procedure decodeArrayOfObjCType_count_at(itemType: PChar; count: NSUInteger; array_: Pointer); message 'decodeArrayOfObjCType:count:at:';
    function decodeBytesWithReturnedLength(lengthp: NSUIntegerPtr): Pointer; message 'decodeBytesWithReturnedLength:';
    procedure encodePropertyList(aPropertyList: id); message 'encodePropertyList:';
    function decodePropertyList: id; message 'decodePropertyList';
    procedure setObjectZone(zone_: NSZonePtr); message 'setObjectZone:';
    function objectZone: NSZonePtr; message 'objectZone';
    function systemVersion: cuint; message 'systemVersion';
    function allowsKeyedCoding: Boolean; message 'allowsKeyedCoding';
    procedure encodeObject_forKey(objv: id; key: NSString); message 'encodeObject:forKey:';
    procedure encodeConditionalObject_forKey(objv: id; key: NSString); message 'encodeConditionalObject:forKey:';
    procedure encodeBool_forKey(boolv: Boolean; key: NSString); message 'encodeBool:forKey:';
    procedure encodeInt_forKey(intv: cint; key: NSString); message 'encodeInt:forKey:';
    procedure encodeInt32_forKey(intv: cint32; key: NSString); message 'encodeInt32:forKey:';
    procedure encodeInt64_forKey(intv: cint64; key: NSString); message 'encodeInt64:forKey:';
    procedure encodeFloat_forKey(realv: single; key: NSString); message 'encodeFloat:forKey:';
    procedure encodeDouble_forKey(realv: double; key: NSString); message 'encodeDouble:forKey:';
    procedure encodeBytes_length_forKey(bytesp: pbyte; lenv: NSUInteger; key: NSString); message 'encodeBytes:length:forKey:';
    function containsValueForKey(key: NSString): Boolean; message 'containsValueForKey:';
    function decodeObjectForKey(key: NSString): id; message 'decodeObjectForKey:';
    function decodeBoolForKey(key: NSString): Boolean; message 'decodeBoolForKey:';
    function decodeIntForKey(key: NSString): cint; message 'decodeIntForKey:';
    function decodeInt32ForKey(key: NSString): cint32; message 'decodeInt32ForKey:';
    function decodeInt64ForKey(key: NSString): cint64; message 'decodeInt64ForKey:';
    function decodeFloatForKey(key: NSString): single; message 'decodeFloatForKey:';
    function decodeDoubleForKey(key: NSString): double; message 'decodeDoubleForKey:';
    function decodeBytesForKey_returnedLength(key: NSString; lengthp: NSUIntegerPtr): pbyte; message 'decodeBytesForKey:returnedLength:';
    procedure encodeInteger_forKey(intv: NSInteger; key: NSString); message 'encodeInteger:forKey:';
    function decodeIntegerForKey(key: NSString): NSInteger; message 'decodeIntegerForKey:';
  end;

{ NSTypedstreamCompatibilityCategory }
  NSTypedstreamCompatibilityCategory = objccategory external (NSCoder)
    procedure encodeNXObject(object_: id); message 'encodeNXObject:'; deprecated 'in Mac OS X 10.5 and later';
    function decodeNXObject: id; message 'decodeNXObject'; deprecated 'in Mac OS X 10.5 and later';
  end;

{$endif}
{$endif}
