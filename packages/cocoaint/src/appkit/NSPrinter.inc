{ Parsed from AppKit.framework NSPrinter.h }


{$ifdef TYPES}
type
  NSPrinterPtr = ^NSPrinter;
{$endif}

{$ifdef TYPES}
type
  NSPrinterTableStatus = NSUInteger;
  NSPrinterTableStatusPtr = ^NSPrinterTableStatus;

const
  NSPrinterTableOK = 0;
  NSPrinterTableNotFound = 1;
  NSPrinterTableError = 2;
{$endif}

{$ifdef CLASSES}

type
  NSPrinter = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _printerName: NSString;
    _printer: pointer;
    _cachedDeviceDescription: NSDictionary;
    _ppdCreationNum: cint;
    _ppdNodes: pointer;
    _ppdPriv: pointer;
    {$if defined(__LP64__)}
    _reserved:array[0..2] of id;
    {$else}
    _compatibilityPadding:array[0..19] of char;
    {$endif}
  public
    class function printerNames: NSArray; message 'printerNames';
    class function printerTypes: NSArray; message 'printerTypes';
    class function printerWithName (name: NSString): NSPrinter; message 'printerWithName:';
    class function printerWithType (type_: NSString): NSPrinter; message 'printerWithType:';
    function name: NSString; message 'name';
    function type_: NSString; message 'type';
    function languageLevel: NSInteger; message 'languageLevel';
    function pageSizeForPaper (paperName: NSString): NSSize; message 'pageSizeForPaper:';
    function deviceDescription: NSDictionary; message 'deviceDescription';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSPrinter_NSDeprecated = objccategory external name 'NSDeprecated' (NSPrinter)
    function statusForTable (tableName: NSString): NSPrinterTableStatus; message 'statusForTable:'; deprecated 'in 10_0, 10_9';
    function isKey_inTable (key: NSString; table: NSString): ObjCBOOL; message 'isKey:inTable:'; deprecated 'in 10_0, 10_9';
    function ObjCBOOLForKey_inTable (key: NSString; table: NSString): ObjCBOOL; message 'ObjCBOOLForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function floatForKey_inTable (key: NSString; table: NSString): single; message 'floatForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function intForKey_inTable (key: NSString; table: NSString): cint; message 'intForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function rectForKey_inTable (key: NSString; table: NSString): NSRect; message 'rectForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function sizeForKey_inTable (key: NSString; table: NSString): NSSize; message 'sizeForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function stringForKey_inTable (key: NSString; table: NSString): NSString; message 'stringForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function stringListForKey_inTable (key: NSString; table: NSString): NSArray; message 'stringListForKey:inTable:'; deprecated 'in 10_0, 10_9';
    function imageRectForPaper (paperName: NSString): NSRect; message 'imageRectForPaper:'; deprecated 'in 10_0, 10_2';
    function acceptsBinary: ObjCBOOL; message 'acceptsBinary'; deprecated 'in 10_0, 10_2';
    function isColor: ObjCBOOL; message 'isColor'; deprecated 'in 10_0, 10_2';
    function isFontAvailable (faceName: NSString): ObjCBOOL; message 'isFontAvailable:'; deprecated 'in 10_0, 10_2';
    function isOutputStackInReverseOrder: ObjCBOOL; message 'isOutputStackInReverseOrder'; deprecated 'in 10_0, 10_2';
    class function printerWithName_domain_includeUnavailable (name_: NSString; domain: NSString; flag: ObjCBOOL): NSPrinter; message 'printerWithName:domain:includeUnavailable:'; deprecated 'in 10_0, 10_2';
    function domain: NSString; message 'domain'; deprecated 'in 10_0, 10_2';
    function host: NSString; message 'host'; deprecated 'in 10_0, 10_2';
    function note: NSString; message 'note'; deprecated 'in 10_0, 10_2';
  end;
{$endif}

