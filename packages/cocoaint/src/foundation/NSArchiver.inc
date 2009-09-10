{ Parsed from Foundation.framework NSArchiver.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSARCHIVER_PAS_H}
{$define NSARCHIVER_PAS_H}
type
  NSArchiverPointer = Pointer;
  NSUnarchiverPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSARCHIVER_PAS_T}
{$define NSARCHIVER_PAS_T}

{ CFString constants }
var
  NSInconsistentArchiveException: CFStringRef; external name '_NSInconsistentArchiveException';

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
{$ifndef NSARCHIVER_PAS_T}
{$define NSARCHIVER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSArchiver = objcclass;
  NSUnarchiver = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSARCHIVER_PAS_C}
{$define NSARCHIVER_PAS_C}

{ NSArchiver }
  NSArchiver = objcclass(NSCoder)
  private
    _mdata: Pointer; {garbage collector: __strong }
    _pointerTable: Pointer;
    _stringTable: Pointer;
    _ids: Pointer;
    _map: Pointer;
    _replacementTable: Pointer;
    _reserved: Pointer;
    
  public
    class function alloc: NSArchiver; message 'alloc';

    function initForWritingWithMutableData(mdata: NSMutableData): id; message 'initForWritingWithMutableData:';
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
    _datax: Pointer;
    _cursor: culong;
    _objectZone: NSZone;
    _systemVersion: culong;
    _streamerVersion: char;
    _swap: char;
    _unused1: char;
    _unused2: char;
    _pointerTable: Pointer;
    _stringTable: Pointer;
    _classVersions: id;
    _lastLabel: clong;
    _map: Pointer;
    _allUnarchivedObjects: Pointer;
    _reserved: id;
    
  public
    class function alloc: NSUnarchiver; message 'alloc';

    function initForReadingWithData(data: NSData): id; message 'initForReadingWithData:';
    procedure setObjectZone(var zone_: NSZone); message 'setObjectZone:';
    function objectZone: NSZone; message 'objectZone';
    function isAtEnd: Boolean; message 'isAtEnd';
    function systemVersion: cuint; message 'systemVersion';
    class function unarchiveObjectWithData(data: NSData): id; message 'unarchiveObjectWithData:';
    class function unarchiveObjectWithFile(path: NSString): id; message 'unarchiveObjectWithFile:';
    class procedure decodeClassName_asClassName(inArchiveName: NSString; trueName: NSString); message 'decodeClassName:asClassName:';
    class function classNameDecodedForArchiveClassName(inArchiveName: NSString): NSString; message 'classNameDecodedForArchiveClassName:';
    procedure replaceObject_withObject(object_: id; newObject: id); message 'replaceObject:withObject:';
  end; external;

{$endif}
{$endif}
