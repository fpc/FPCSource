{ Parsed from Foundation.framework NSKeyedArchiver.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:51 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSKEYEDARCHIVER_PAS_H}
{$define NSKEYEDARCHIVER_PAS_H}
type
  NSKeyedArchiverPointer = Pointer;
  NSKeyedUnarchiverPointer = Pointer;

{$endif}
{$endif}

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
  NSInvalidArchiveOperationException: NSString; external name '_NSInvalidArchiveOperationException';
  NSInvalidUnarchiveOperationException: NSString; external name '_NSInvalidUnarchiveOperationException';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSKeyedArchiver = objcclass;
  NSKeyedUnarchiver = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSKEYEDARCHIVER_PAS_C}
{$define NSKEYEDARCHIVER_PAS_C}

{ NSKeyedArchiver }
  NSKeyedArchiver = objcclass(NSCoder)
  private
    __stream: Pointer;
    __flags: NSUInteger;
    __delegate: id;
    __containers: id;
    __objects: id;
    __objRefMap: id;
    __replacementMap: id;
    __classNameMap: id;
    __conditionals: id;
    __classes: id;
    __genericKey: NSUInteger;
    __cache: Pointer;
    __cacheSize: NSUInteger;
    __reserved3: Pointer;
    __reserved2: Pointer;
    __reserved1: Pointer;
    __reserved0: Pointer; {garbage collector: __strong }
    
  public
    class function alloc: NSKeyedArchiver; message 'alloc';

    class function archivedDataWithRootObject(rootObject: id): NSData; message 'archivedDataWithRootObject:';
    class function archiveRootObject_toFile(rootObject: id; path: NSString): Boolean; message 'archiveRootObject:toFile:';
    function initForWritingWithMutableData(data: NSMutableData): id; message 'initForWritingWithMutableData:';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setOutputFormat(format: NSPropertyListFormat); message 'setOutputFormat:';
    function outputFormat: NSPropertyListFormat; message 'outputFormat';
    procedure finishEncoding; message 'finishEncoding';
    class procedure setClassName_forClass(codedName: NSString; cls: Pobjc_class); message 'setClassName:forClass:';
    class function classNameForClass(cls: Pobjc_class): NSString; message 'classNameForClass:';
    procedure encodeObject_forKey(objv: id; key: NSString); message 'encodeObject:forKey:';
    procedure encodeConditionalObject_forKey(objv: id; key: NSString); message 'encodeConditionalObject:forKey:';
    procedure encodeBool_forKey(boolv: Boolean; key: NSString); message 'encodeBool:forKey:';
    procedure encodeInt_forKey(intv: cint; key: NSString); message 'encodeInt:forKey:';
    procedure encodeInt32_forKey(intv: longint; key: NSString); message 'encodeInt32:forKey:';
    procedure encodeInt64_forKey(intv: clonglong; key: NSString); message 'encodeInt64:forKey:';
    procedure encodeFloat_forKey(realv: single; key: NSString); message 'encodeFloat:forKey:';
    procedure encodeDouble_forKey(realv: double; key: NSString); message 'encodeDouble:forKey:';
    procedure encodeBytes_length_forKey(bytesp: pbyte; lenv: NSUInteger; key: NSString); message 'encodeBytes:length:forKey:';
  end; external;

{ NSKeyedUnarchiver }
  NSKeyedUnarchiver = objcclass(NSCoder)
  private
    __delegate: id;
    __flags: cardinal;
    __objRefMap: id;
    __replacementMap: id;
    __nameClassMap: id;
    __tmpRefObjMap: id;
    __refObjMap: id;
    __genericKey: longint;
    __data: id;
    __offsetData: Pointer;
    __containers: id;
    __objects: id;
    __bytes: byte;
    __len: clonglong;
    __white: id;
    __reserved0: Pointer; {garbage collector: __strong }
    
  public
    class function alloc: NSKeyedUnarchiver; message 'alloc';

    class function unarchiveObjectWithData(data: NSData): id; message 'unarchiveObjectWithData:';
    class function unarchiveObjectWithFile(path: NSString): id; message 'unarchiveObjectWithFile:';
    function initForReadingWithData(data: NSData): id; message 'initForReadingWithData:';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure finishDecoding; message 'finishDecoding';
    class procedure setClass_forClassName(cls: Pobjc_class; codedName: NSString); message 'setClass:forClassName:';
    class function classForClassName(codedName: NSString): Pobjc_class; message 'classForClassName:';
    function containsValueForKey(key: NSString): Boolean; message 'containsValueForKey:';
    function decodeObjectForKey(key: NSString): id; message 'decodeObjectForKey:';
    function decodeBoolForKey(key: NSString): Boolean; message 'decodeBoolForKey:';
    function decodeIntForKey(key: NSString): cint; message 'decodeIntForKey:';
    function decodeInt32ForKey(key: NSString): longint; message 'decodeInt32ForKey:';
    function decodeInt64ForKey(key: NSString): clonglong; message 'decodeInt64ForKey:';
    function decodeFloatForKey(key: NSString): single; message 'decodeFloatForKey:';
    function decodeDoubleForKey(key: NSString): double; message 'decodeDoubleForKey:';
    function decodeBytesForKey_returnedLength(key: NSString; lengthp: NSUIntegerPointer): byte; message 'decodeBytesForKey:returnedLength:';
  end; external;

{$endif}
{$endif}
