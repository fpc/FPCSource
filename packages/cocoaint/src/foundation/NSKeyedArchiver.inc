{ Parsed from Foundation.framework NSKeyedArchiver.h }

{$ifdef TYPES}
{$ifndef NSKEYEDARCHIVER_PAS_T}
{$define NSKEYEDARCHIVER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSKEYEDARCHIVER_PAS_R}
{$define NSKEYEDARCHIVER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSKEYEDARCHIVER_PAS_F}
{$define NSKEYEDARCHIVER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSKEYEDARCHIVER_PAS_S}
{$define NSKEYEDARCHIVER_PAS_S}

{ External string constants }
var
  NSInvalidArchiveOperationException: NSString; cvar; external;
  NSInvalidUnarchiveOperationException: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSKeyedArchiverDelegateProtocol = objcprotocol;
  NSKeyedUnarchiverDelegateProtocol = objcprotocol;
  NSKeyedArchiver = objcclass;
  NSKeyedArchiverPointer = ^NSKeyedArchiver;
  NSKeyedArchiverPtr = NSKeyedArchiverPointer;
  NSKeyedUnarchiver = objcclass;
  NSKeyedUnarchiverPointer = ^NSKeyedUnarchiver;
  NSKeyedUnarchiverPtr = NSKeyedUnarchiverPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSKEYEDARCHIVER_PAS_C}
{$define NSKEYEDARCHIVER_PAS_C}

{ NSKeyedArchiver }
  NSKeyedArchiver = objcclass external (NSCoder)
  private
    _stream: Pointer;
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
    _cache: Pointer;
    _cacheSize: NSUInteger;
    _estimatedCount: NSUInteger;
    _reserved2: Pointer;
    _visited: id;
    _reserved0: Pointer; {garbage collector: __strong }
    
  public
    class function archivedDataWithRootObject(rootObject: id): NSData; message 'archivedDataWithRootObject:';
    class function archiveRootObject_toFile(rootObject: id; path: NSString): Boolean; message 'archiveRootObject:toFile:';
    function initForWritingWithMutableData(data: NSMutableData): id; message 'initForWritingWithMutableData:';
    procedure setDelegate(delegate_: NSKeyedArchiverDelegateProtocol); message 'setDelegate:';
    function delegate: NSKeyedArchiverDelegateProtocol; message 'delegate';
    procedure setOutputFormat(format: NSPropertyListFormat); message 'setOutputFormat:';
    function outputFormat: NSPropertyListFormat; message 'outputFormat';
    procedure finishEncoding; message 'finishEncoding';
    class procedure setClassName_forClass(codedName: NSString; cls: Pobjc_class); message 'setClassName:forClass:';
    class function classNameForClass(cls: Pobjc_class): NSString; message 'classNameForClass:';
    procedure encodeObject_forKey(objv: id; key: NSString); message 'encodeObject:forKey:';
    procedure encodeConditionalObject_forKey(objv: id; key: NSString); message 'encodeConditionalObject:forKey:';
    procedure encodeBool_forKey(boolv: Boolean; key: NSString); message 'encodeBool:forKey:';
    procedure encodeInt_forKey(intv: cint; key: NSString); message 'encodeInt:forKey:';
    procedure encodeInt32_forKey(intv: cint32; key: NSString); message 'encodeInt32:forKey:';
    procedure encodeInt64_forKey(intv: cint64; key: NSString); message 'encodeInt64:forKey:';
    procedure encodeFloat_forKey(realv: single; key: NSString); message 'encodeFloat:forKey:';
    procedure encodeDouble_forKey(realv: double; key: NSString); message 'encodeDouble:forKey:';
    procedure encodeBytes_length_forKey(bytesp: pbyte; lenv: NSUInteger; key: NSString); message 'encodeBytes:length:forKey:';
  end;

{ NSKeyedUnarchiver }
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
    _offsetData: Pointer;
    _containers: id;
    _objects: id;
    _bytes: pbyte;
    _len: cuint64;
    _white: id;
    _reserved0: Pointer; {garbage collector: __strong }
    
  public
    class function unarchiveObjectWithData(data: NSData): id; message 'unarchiveObjectWithData:';
    class function unarchiveObjectWithFile(path: NSString): id; message 'unarchiveObjectWithFile:';
    function initForReadingWithData(data: NSData): id; message 'initForReadingWithData:';
    procedure setDelegate(delegate_: NSKeyedUnarchiverDelegateProtocol); message 'setDelegate:';
    function delegate: NSKeyedUnarchiverDelegateProtocol; message 'delegate';
    procedure finishDecoding; message 'finishDecoding';
    class procedure setClass_forClassName(cls: Pobjc_class; codedName: NSString); message 'setClass:forClassName:';
    class function classForClassName(codedName: NSString): Pobjc_class; message 'classForClassName:';
    function containsValueForKey(key: NSString): Boolean; message 'containsValueForKey:';
    function decodeObjectForKey(key: NSString): id; message 'decodeObjectForKey:';
    function decodeBoolForKey(key: NSString): Boolean; message 'decodeBoolForKey:';
    function decodeIntForKey(key: NSString): cint; message 'decodeIntForKey:';
    function decodeInt32ForKey(key: NSString): cint32; message 'decodeInt32ForKey:';
    function decodeInt64ForKey(key: NSString): cint64; message 'decodeInt64ForKey:';
    function decodeFloatForKey(key: NSString): single; message 'decodeFloatForKey:';
    function decodeDoubleForKey(key: NSString): double; message 'decodeDoubleForKey:';
    function decodeBytesForKey_returnedLength(key: NSString; lengthp: NSUIntegerPtr): pbyte; message 'decodeBytesForKey:returnedLength:';
  end;

{ NSKeyedArchiverObjectSubstitutionCategory }
  NSKeyedArchiverObjectSubstitutionCategory = objccategory external (NSObject)
    function classForKeyedArchiver: Pobjc_class; message 'classForKeyedArchiver';
    function replacementObjectForKeyedArchiver(archiver: NSKeyedArchiver): id; message 'replacementObjectForKeyedArchiver:';
    class function classFallbacksForKeyedArchiver: NSArray; message 'classFallbacksForKeyedArchiver';
  end;

{ NSKeyedUnarchiverObjectSubstitutionCategory }
  NSKeyedUnarchiverObjectSubstitutionCategory = objccategory external (NSObject)
    class function classForKeyedUnarchiver: Pobjc_class; message 'classForKeyedUnarchiver';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSKEYEDARCHIVER_PAS_P}
{$define NSKEYEDARCHIVER_PAS_P}
  
{ NSKeyedArchiverDelegate Protocol }
  NSKeyedArchiverDelegateProtocol = objcprotocol external name 'NSKeyedArchiverDelegate'
  optional
    function archiver_willEncodeObject(archiver: NSKeyedArchiver; object_: id): id; message 'archiver:willEncodeObject:';
    procedure archiver_didEncodeObject(archiver: NSKeyedArchiver; object_: id); message 'archiver:didEncodeObject:';
    procedure archiver_willReplaceObject_withObject(archiver: NSKeyedArchiver; object_: id; newObject: id); message 'archiver:willReplaceObject:withObject:';
    procedure archiverWillFinish(archiver: NSKeyedArchiver); message 'archiverWillFinish:';
    procedure archiverDidFinish(archiver: NSKeyedArchiver); message 'archiverDidFinish:';
  end;
  
{ NSKeyedUnarchiverDelegate Protocol }
  NSKeyedUnarchiverDelegateProtocol = objcprotocol external name 'NSKeyedUnarchiverDelegate'
  optional
    function unarchiver_cannotDecodeObjectOfClassName_originalClasses(unarchiver: NSKeyedUnarchiver; name: NSString; classNames: NSArray): Pobjc_class; message 'unarchiver:cannotDecodeObjectOfClassName:originalClasses:';
    function unarchiver_didDecodeObject(unarchiver: NSKeyedUnarchiver; object_: id): id; message 'unarchiver:didDecodeObject:';
    procedure unarchiver_willReplaceObject_withObject(unarchiver: NSKeyedUnarchiver; object_: id; newObject: id); message 'unarchiver:willReplaceObject:withObject:';
    procedure unarchiverWillFinish(unarchiver: NSKeyedUnarchiver); message 'unarchiverWillFinish:';
    procedure unarchiverDidFinish(unarchiver: NSKeyedUnarchiver); message 'unarchiverDidFinish:';
  end;
{$endif}
{$endif}
