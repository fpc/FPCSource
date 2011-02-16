{ Parsed from Foundation.framework NSArchiver.h }

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
  NSArchiver = objcclass external (NSCoder)
  private
    mdata: Pointer; {garbage collector: __strong }
    pointerTable: Pointer;
    stringTable: Pointer;
    ids: Pointer;
    map: Pointer;
    replacementTable: Pointer;
    reserved: Pointer;
    
  public
    function initForWritingWithMutableData(mdata_: NSMutableData): id; message 'initForWritingWithMutableData:';
    function archiverData: NSMutableData; message 'archiverData';
    procedure encodeRootObject(rootObject: id); message 'encodeRootObject:';
    procedure encodeConditionalObject(object_: id); message 'encodeConditionalObject:';
    class function archivedDataWithRootObject(rootObject: id): NSData; message 'archivedDataWithRootObject:';
    class function archiveRootObject_toFile(rootObject: id; path: NSString): Boolean; message 'archiveRootObject:toFile:';
    procedure encodeClassName_intoClassName(trueName: NSString; inArchiveName: NSString); message 'encodeClassName:intoClassName:';
    function classNameEncodedForTrueClassName(trueName: NSString): NSString; message 'classNameEncodedForTrueClassName:';
    procedure replaceObject_withObject(object_: id; newObject: id); message 'replaceObject:withObject:';
  end;

{ NSUnarchiver }
  NSUnarchiver = objcclass external (NSCoder)
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
  end;

{ NSArchiverCallbackCategory }
  NSArchiverCallbackCategory = objccategory external (NSObject)
    function classForArchiver: Pobjc_class; message 'classForArchiver';
    function replacementObjectForArchiver(archiver: NSArchiver): id; message 'replacementObjectForArchiver:';
  end;

{$endif}
{$endif}
