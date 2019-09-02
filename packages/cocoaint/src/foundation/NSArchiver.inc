{ Parsed from Foundation.framework NSArchiver.h }


{$ifdef TYPES}
type
  NSArchiverPtr = ^NSArchiver;
  NSUnarchiverPtr = ^NSUnarchiver;
{$endif}

{$ifdef CLASSES}

type
  NSArchiver = objcclass external (NSCoder)
  private
    mdata: pointer;
    pointerTable: pointer;
    stringTable: pointer;
    ids: pointer;
    map: pointer;
    replacementTable: pointer;
    reserved: pointer;
  public
    function initForWritingWithMutableData (mdata_: NSMutableData): instancetype; message 'initForWritingWithMutableData:'; { NS_DESIGNATED_INITIALIZER }
    function archiverData: NSMutableData; message 'archiverData';
    procedure encodeRootObject (rootObject: id); message 'encodeRootObject:';
    procedure encodeConditionalObject (object_: id); message 'encodeConditionalObject:';
    class function archivedDataWithRootObject (rootObject: id): NSData; message 'archivedDataWithRootObject:';
    class function archiveRootObject_toFile (rootObject: id; path: NSString): ObjCBOOL; message 'archiveRootObject:toFile:';
    procedure encodeClassName_intoClassName (trueName: NSString; inArchiveName: NSString); message 'encodeClassName:intoClassName:';
    function classNameEncodedForTrueClassName (trueName: NSString): NSString; message 'classNameEncodedForTrueClassName:';
    procedure replaceObject_withObject (object_: id; newObject: id); message 'replaceObject:withObject:';
  end;

type
  NSUnarchiver = objcclass external (NSCoder)
  private
    datax: pointer;
    cursor: NSUInteger;
    objectZone_: NSZonePtr;
    systemVersion: NSUInteger;
    streamerVersion: char;
    swap: char;
    unused1: char;
    unused2: char;
    pointerTable: pointer;
    stringTable: pointer;
    classVersions: id;
    lastLabel: NSInteger;
    map: pointer;
    allUnarchivedObjects: pointer;
    reserved: id;
  public
    function initForReadingWithData (data: NSData): instancetype; message 'initForReadingWithData:'; { NS_DESIGNATED_INITIALIZER }
    procedure setObjectZone (zone: NSZonePtr); message 'setObjectZone:';
    function objectZone: NSZonePtr; message 'objectZone';
    function isAtEnd: ObjCBOOL; message 'isAtEnd';
    function systemVersion_: cuint; message 'systemVersion';
    class function unarchiveObjectWithData (data: NSData): id; message 'unarchiveObjectWithData:';
    class function unarchiveObjectWithFile (path: NSString): id; message 'unarchiveObjectWithFile:';
    class procedure classDecodeClassName_asClassName (inArchiveName: NSString; trueName: NSString); message 'decodeClassName:asClassName:';
    procedure decodeClassName_asClassName (inArchiveName: NSString; trueName: NSString); message 'decodeClassName:asClassName:';
    class function classClassNameDecodedForArchiveClassName (inArchiveName: NSString): NSString; message 'classNameDecodedForArchiveClassName:';
    function classNameDecodedForArchiveClassName (inArchiveName: NSString): NSString; message 'classNameDecodedForArchiveClassName:';
    procedure replaceObject_withObject (object_: id; newObject: id); message 'replaceObject:withObject:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSInconsistentArchiveException: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSArchiverCallback = objccategory external (NSObject)
    function classForArchiver: pobjc_class; message 'classForArchiver';
    function replacementObjectForArchiver (archiver: NSArchiver): id; message 'replacementObjectForArchiver:';
  end;
{$endif}

