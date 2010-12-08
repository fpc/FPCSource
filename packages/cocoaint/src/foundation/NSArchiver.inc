{ Parsed from Foundation.framework NSArchiver.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSARCHIVER_PAS_T}
{$define NSARCHIVER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSARCHIVER_PAS_R}
{$define NSARCHIVER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSARCHIVER_PAS_F}
{$define NSARCHIVER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSARCHIVER_PAS_S}
{$define NSARCHIVER_PAS_S}

{ External string constants }
var
  NSInconsistentArchiveException: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSArchiver = objcclass;
  NSArchiverPointer = ^NSArchiver;
  NSArchiverPtr = NSArchiverPointer;
  NSUnarchiver = objcclass;
  NSUnarchiverPointer = ^NSUnarchiver;
  NSUnarchiverPtr = NSUnarchiverPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSARCHIVER_PAS_C}
{$define NSARCHIVER_PAS_C}

{ NSArchiver }
  NSArchiver = objcclass(NSCoder)
  private
    mdata: Pointer; {garbage collector: __strong }
    pointerTable: Pointer;
    stringTable: Pointer;
    ids: Pointer;
    map: Pointer;
    replacementTable: Pointer;
    reserved: Pointer;
    
  public
    class function alloc: NSArchiver; message 'alloc';

    function initForWritingWithMutableData(mdata_: NSMutableData): id; message 'initForWritingWithMutableData:';
    function archiverData: NSMutableData; message 'archiverData';
    procedure encodeRootObject(rootObject: id); message 'encodeRootObject:';
    procedure encodeConditionalObject(object_: id); message 'encodeConditionalObject:';
    class function archivedDataWithRootObject(rootObject: id): NSData; message 'archivedDataWithRootObject:';
    class function archiveRootObject_toFile(rootObject: id; path: NSString): Boolean; message 'archiveRootObject:toFile:';
    procedure encodeClassName_intoClassName(trueName: NSString; inArchiveName: NSString); message 'encodeClassName:intoClassName:';
    function classNameEncodedForTrueClassName(trueName: NSString): NSString; message 'classNameEncodedForTrueClassName:';
    procedure replaceObject_withObject(object_: id; newObject: id); message 'replaceObject:withObject:';
  end; external;

{ NSUnarchiver }
  NSUnarchiver = objcclass(NSCoder)
  private
    datax: Pointer;
    cursor: NSUInteger;
    _objectZone: NSZonePtr;
    _systemVersion: NSUInteger;
    streamerVersion: char;
    swap: char;
    unused1: char;
    unused2: char;
    pointerTable: Pointer;
    stringTable: Pointer;
    classVersions: id;
    lastLabel: NSInteger;
    map: Pointer;
    allUnarchivedObjects: Pointer;
    reserved: id;
    
  public
    class function alloc: NSUnarchiver; message 'alloc';

    function initForReadingWithData(data: NSData): id; message 'initForReadingWithData:';
    procedure setObjectZone(zone_: NSZonePtr); message 'setObjectZone:';
    function objectZone: NSZonePtr; message 'objectZone';
    function isAtEnd: Boolean; message 'isAtEnd';
    function systemVersion: cuint; message 'systemVersion';
    class function unarchiveObjectWithData(data: NSData): id; message 'unarchiveObjectWithData:';
    class function unarchiveObjectWithFile(path: NSString): id; message 'unarchiveObjectWithFile:';
    class procedure decodeClassName_asClassName(inArchiveName: NSString; trueName: NSString); message 'decodeClassName:asClassName:';
    class function classNameDecodedForArchiveClassName(inArchiveName: NSString): NSString; message 'classNameDecodedForArchiveClassName:';
    procedure replaceObject_withObject(object_: id; newObject: id); message 'replaceObject:withObject:';
  end; external;

{ NSArchiverCallbackCategory }
  NSArchiverCallbackCategory = objccategory(NSObject)
    function classForArchiver: Pobjc_class; message 'classForArchiver';
    function replacementObjectForArchiver(archiver: NSArchiver): id; message 'replacementObjectForArchiver:';
  end; external;

{$endif}
{$endif}
