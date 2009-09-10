{ Parsed from Appkit.framework NSPrinter.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPRINTER_PAS_H}
{$define NSPRINTER_PAS_H}
type
  NSPrinterPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSPRINTER_PAS_T}
{$define NSPRINTER_PAS_T}

{ Constants }

const
  NSPrinterTableOK = 0;
  NSPrinterTableNotFound = 1;
  NSPrinterTableError = 2;

{ Types }
type
  NSPrinterTableStatus = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPRINTER_PAS_R}
{$define NSPRINTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPRINTER_PAS_F}
{$define NSPRINTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPRINTER_PAS_T}
{$define NSPRINTER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPrinter = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPRINTER_PAS_C}
{$define NSPRINTER_PAS_C}

{ NSPrinter }
  NSPrinter = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __printerName: NSString;
    __printer: Pointer;
    __cachedDeviceDescription: NSDictionary;
    __ppdCreationNum: cint;
    __ppdNodes: Pointer;
    __ppdPriv: Pointer;
    {$ifdef cpu64}
    __reserved: id;
    {$else}
    __compatibilityPadding: char;
    {$endif}
    
  public
    class function alloc: NSPrinter; message 'alloc';

    class function printerNames: NSArray; message 'printerNames';
    class function printerTypes: NSArray; message 'printerTypes';
    class function printerWithName(name_: NSString): NSPrinter; message 'printerWithName:';
    class function printerWithType(type__: NSString): NSPrinter; message 'printerWithType:';
    function name: NSString; message 'name';
    function type_: NSString; message 'type';
    function languageLevel: clong; message 'languageLevel';
    function pageSizeForPaper(paperName: NSString): NSSize; message 'pageSizeForPaper:';
    function statusForTable(tableName: NSString): NSPrinterTableStatus; message 'statusForTable:';
    function isKey_inTable(key: NSString; table: NSString): Boolean; message 'isKey:inTable:';
    function booleanForKey_inTable(key: NSString; table: NSString): Boolean; message 'booleanForKey:inTable:';
    function floatForKey_inTable(key: NSString; table: NSString): single; message 'floatForKey:inTable:';
    function intForKey_inTable(key: NSString; table: NSString): cint; message 'intForKey:inTable:';
    function rectForKey_inTable(key: NSString; table: NSString): NSRect; message 'rectForKey:inTable:';
    function sizeForKey_inTable(key: NSString; table: NSString): NSSize; message 'sizeForKey:inTable:';
    function stringForKey_inTable(key: NSString; table: NSString): NSString; message 'stringForKey:inTable:';
    function stringListForKey_inTable(key: NSString; table: NSString): NSArray; message 'stringListForKey:inTable:';
    function deviceDescription: NSDictionary; message 'deviceDescription';

    { Category: NSDeprecated }
    function imageRectForPaper(paperName: NSString): NSRect; message 'imageRectForPaper:';
    function acceptsBinary: Boolean; message 'acceptsBinary';
    function isColor: Boolean; message 'isColor';
    function isFontAvailable(faceName: NSString): Boolean; message 'isFontAvailable:';
    function isOutputStackInReverseOrder: Boolean; message 'isOutputStackInReverseOrder';
    class function printerWithName_domain_includeUnavailable(name_: NSString; domain_: NSString; flag: Boolean): NSPrinter; message 'printerWithName:domain:includeUnavailable:';
    function domain: NSString; message 'domain';
    function host: NSString; message 'host';
    function note: NSString; message 'note';
  end; external;

{$endif}
{$endif}
