{ Parsed from Foundation.framework NSKeyedArchiver.h }


{$ifdef TYPES}
type
  NSKeyedArchiverPtr = ^NSKeyedArchiver;
  NSKeyedUnarchiverPtr = ^NSKeyedUnarchiver;
  NSKeyedArchiverDelegateProtocolPtr = ^NSKeyedArchiverDelegateProtocol;
  NSKeyedUnarchiverDelegateProtocolPtr = ^NSKeyedUnarchiverDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSInvalidArchiveOperationException: NSString; cvar; external;
  NSInvalidUnarchiveOperationException: NSString; cvar; external;
  NSKeyedArchiveRootObjectKey: NSString { available in 10_9, 7_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSKeyedArchiver = objcclass external (NSCoder)
  private
    _stream: pointer;
    _flags: NSUInteger;
    _delegate: id;
    _containers: id;
    _objects: id;
    _objRefMap: id;
    _replacementMap: id;
    _classNameMap: id;
    _conditionals: id;
    _classes: id;
    _genericKey: NSUInteger;
    _cache: pointer;
    _cacheSize: NSUInteger;
    _estimatedCount: NSUInteger;
    _reserved2: pointer;
    _visited: id;
    _reserved0: pointer;
  public
    class function archivedDataWithRootObject (rootObject: id): NSData; message 'archivedDataWithRootObject:';
    class function archiveRootObject_toFile (rootObject: id; path: NSString): ObjCBOOL; message 'archiveRootObject:toFile:';
    function initForWritingWithMutableData (data: NSMutableData): instancetype; message 'initForWritingWithMutableData:';
    procedure setDelegate(newValue: NSKeyedArchiverDelegateProtocol); message 'setDelegate:';
    function delegate: NSKeyedArchiverDelegateProtocol; message 'delegate';
    procedure setOutputFormat(newValue: NSPropertyListFormat); message 'setOutputFormat:';
    function outputFormat: NSPropertyListFormat; message 'outputFormat';
    procedure finishEncoding; message 'finishEncoding';
    class procedure classSetClassName_forClass (codedName: NSString; cls: pobjc_class); message 'setClassName:forClass:';
    procedure setClassName_forClass (codedName: NSString; cls: pobjc_class); message 'setClassName:forClass:';
    class function classClassNameForClass (cls: pobjc_class): NSString; message 'classNameForClass:';
    function classNameForClass (cls: pobjc_class): NSString; message 'classNameForClass:';
    procedure encodeObject_forKey (objv: id; key: NSString); message 'encodeObject:forKey:';
    procedure encodeConditionalObject_forKey (objv: id; key: NSString); message 'encodeConditionalObject:forKey:';
    procedure encodeBool_forKey (boolv: ObjCBOOL; key: NSString); message 'encodeBool:forKey:';
    procedure encodeInt_forKey (intv: cint; key: NSString); message 'encodeInt:forKey:';
    procedure encodeInt32_forKey (intv: cint32; key: NSString); message 'encodeInt32:forKey:';
    procedure encodeInt64_forKey (intv: cint64; key: NSString); message 'encodeInt64:forKey:';
    procedure encodeFloat_forKey (realv: single; key: NSString); message 'encodeFloat:forKey:';
    procedure encodeDouble_forKey (realv: double; key: NSString); message 'encodeDouble:forKey:';
    procedure encodeBytes_length_forKey (bytesp: pbyte; lenv: NSUInteger; key: NSString); message 'encodeBytes:length:forKey:';
    procedure setRequiresSecureCoding (b: ObjCBOOL); message 'setRequiresSecureCoding:'; { available in 10_8, 6_0 }
  end;

type
  NSKeyedUnarchiver = objcclass external (NSCoder)
  private
    _delegate: id;
    _flags: cuint32;
    _objRefMap: id;
    _replacementMap: id;
    _nameClassMap: id;
    _tmpRefObjMap: id;
    _refObjMap: id;
    _genericKey: cint32;
    _data: id;
    _offsetData: pointer;
    _containers: id;
    _objects: id;
    _bytes: pbyte;
    _len: cuint64;
    _helper: id;
    _reserved0: pointer;
  public
    class function unarchiveObjectWithData (data: NSData): id; message 'unarchiveObjectWithData:';
    class function unarchiveObjectWithFile (path: NSString): id; message 'unarchiveObjectWithFile:';
    function initForReadingWithData (data: NSData): instancetype; message 'initForReadingWithData:';
    procedure setDelegate(newValue: NSKeyedUnarchiverDelegateProtocol); message 'setDelegate:';
    function delegate: NSKeyedUnarchiverDelegateProtocol; message 'delegate';
    procedure finishDecoding; message 'finishDecoding';
    class procedure classSetClass_forClassName (cls: pobjc_class; codedName: NSString); message 'setClass:forClassName:';
    procedure setClass_forClassName (cls: pobjc_class; codedName: NSString); message 'setClass:forClassName:';
    class function classClassForClassName (codedName: NSString): pobjc_class; message 'classForClassName:';
    function classForClassName (codedName: NSString): pobjc_class; message 'classForClassName:';
    function containsValueForKey (key: NSString): ObjCBOOL; message 'containsValueForKey:';
    function decodeObjectForKey (key: NSString): id; message 'decodeObjectForKey:';
    function decodeBoolForKey (key: NSString): ObjCBOOL; message 'decodeBoolForKey:';
    function decodeIntForKey (key: NSString): cint; message 'decodeIntForKey:';
    function decodeInt32ForKey (key: NSString): cint32; message 'decodeInt32ForKey:';
    function decodeInt64ForKey (key: NSString): cint64; message 'decodeInt64ForKey:';
    function decodeFloatForKey (key: NSString): single; message 'decodeFloatForKey:';
    function decodeDoubleForKey (key: NSString): double; message 'decodeDoubleForKey:';
    function decodeBytesForKey_returnedLength (key: NSString; lengthp: NSUIntegerPtr): pbyte; message 'decodeBytesForKey:returnedLength:';
    procedure setRequiresSecureCoding (b: ObjCBOOL); message 'setRequiresSecureCoding:'; { available in 10_8, 6_0 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSKeyedArchiverDelegateProtocol = objcprotocol external name 'NSKeyedArchiverDelegate' (NSObjectProtocol)
  optional
    function archiver_willEncodeObject (archiver: NSKeyedArchiver; object_: id): id; message 'archiver:willEncodeObject:';
    procedure archiver_didEncodeObject (archiver: NSKeyedArchiver; object_: id); message 'archiver:didEncodeObject:';
    procedure archiver_willReplaceObject_withObject (archiver: NSKeyedArchiver; object_: id; newObject: id); message 'archiver:willReplaceObject:withObject:';
    procedure archiverWillFinish (archiver: NSKeyedArchiver); message 'archiverWillFinish:';
    procedure archiverDidFinish (archiver: NSKeyedArchiver); message 'archiverDidFinish:';
  end;

type
  NSKeyedUnarchiverDelegateProtocol = objcprotocol external name 'NSKeyedUnarchiverDelegate' (NSObjectProtocol)
  optional
    function unarchiver_cannotDecodeObjectOfClassName_originalClasses (unarchiver: NSKeyedUnarchiver; name: NSString; classNames: NSArray): pobjc_class; message 'unarchiver:cannotDecodeObjectOfClassName:originalClasses:';
    function unarchiver_didDecodeObject (unarchiver: NSKeyedUnarchiver; object_: id): id; message 'unarchiver:didDecodeObject:';
    procedure unarchiver_willReplaceObject_withObject (unarchiver: NSKeyedUnarchiver; object_: id; newObject: id); message 'unarchiver:willReplaceObject:withObject:';
    procedure unarchiverWillFinish (unarchiver: NSKeyedUnarchiver); message 'unarchiverWillFinish:';
    procedure unarchiverDidFinish (unarchiver: NSKeyedUnarchiver); message 'unarchiverDidFinish:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSKeyedArchiverObjectSubstitution = objccategory external (NSObject)
    function classForKeyedArchiver: pobjc_class; message 'classForKeyedArchiver';
    function replacementObjectForKeyedArchiver (archiver: NSKeyedArchiver): id; message 'replacementObjectForKeyedArchiver:';
    class function classFallbacksForKeyedArchiver: NSArray; message 'classFallbacksForKeyedArchiver';
  end;

type
  NSKeyedUnarchiverObjectSubstitution = objccategory external (NSObject)
    class function classForKeyedUnarchiver: pobjc_class; message 'classForKeyedUnarchiver';
  end;
{$endif}

