{ Parsed from Foundation.framework NSScanner.h }

{$ifdef TYPES}
{$ifndef NSSCANNER_PAS_T}
{$define NSSCANNER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCANNER_PAS_R}
{$define NSSCANNER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCANNER_PAS_F}
{$define NSSCANNER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCANNER_PAS_S}
{$define NSSCANNER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScanner = objcclass;
  NSScannerPointer = ^NSScanner;
  NSScannerPtr = NSScannerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCANNER_PAS_C}
{$define NSSCANNER_PAS_C}

{ NSScanner }
  NSScanner = objcclass external (NSObject, NSCopyingProtocol)
    
  public
    function string_: NSString; message 'string';
    function scanLocation: NSUInteger; message 'scanLocation';
    procedure setScanLocation(pos: NSUInteger); message 'setScanLocation:';
    procedure setCharactersToBeSkipped(set_: NSCharacterSet); message 'setCharactersToBeSkipped:';
    procedure setCaseSensitive(flag: Boolean); message 'setCaseSensitive:';
    procedure setLocale(locale_: id); message 'setLocale:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ NSExtendedScannerCategory }
  NSExtendedScannerCategory = objccategory external (NSScanner)
    function charactersToBeSkipped: NSCharacterSet; message 'charactersToBeSkipped';
    function caseSensitive: Boolean; message 'caseSensitive';
    function locale: id; message 'locale';
    function scanInt(value: pcint): Boolean; message 'scanInt:';
    function scanInteger(value: NSIntegerPtr): Boolean; message 'scanInteger:';
    function scanHexLongLong(result_: pculonglong): Boolean; message 'scanHexLongLong:';
    function scanHexFloat(result_: psingle): Boolean; message 'scanHexFloat:';
    function scanHexDouble(result_: pdouble): Boolean; message 'scanHexDouble:';
    function scanHexInt(value: pcuint): Boolean; message 'scanHexInt:';
    function scanLongLong(value: pclonglong): Boolean; message 'scanLongLong:';
    function scanFloat(value: psingle): Boolean; message 'scanFloat:';
    function scanDouble(value: pdouble): Boolean; message 'scanDouble:';
    function scanString_intoString(string__: NSString; value: NSStringPointer): Boolean; message 'scanString:intoString:';
    function scanCharactersFromSet_intoString(set_: NSCharacterSet; value: NSStringPointer): Boolean; message 'scanCharactersFromSet:intoString:';
    function scanUpToString_intoString(string__: NSString; value: NSStringPointer): Boolean; message 'scanUpToString:intoString:';
    function scanUpToCharactersFromSet_intoString(set_: NSCharacterSet; value: NSStringPointer): Boolean; message 'scanUpToCharactersFromSet:intoString:';
    function isAtEnd: Boolean; message 'isAtEnd';
    function initWithString(string__: NSString): id; message 'initWithString:';
    class function scannerWithString(string__: NSString): id; message 'scannerWithString:';
    class function localizedScannerWithString(string__: NSString): id; message 'localizedScannerWithString:';
  end;

{$endif}
{$endif}
