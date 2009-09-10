{ Parsed from Foundation.framework NSScanner.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCANNER_PAS_H}
{$define NSSCANNER_PAS_H}
type
  NSScannerPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSSCANNER_PAS_T}
{$define NSSCANNER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScanner = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCANNER_PAS_C}
{$define NSSCANNER_PAS_C}

{ NSScanner }
  NSScanner = objcclass(NSObject, NSCopyingProtocol)
    
  public
    class function alloc: NSScanner; message 'alloc';

    function string_: NSString; message 'string';
    function scanLocation: culong; message 'scanLocation';
    procedure setScanLocation(pos: culong); message 'setScanLocation:';
    procedure setCharactersToBeSkipped(set_: NSCharacterSet); message 'setCharactersToBeSkipped:';
    procedure setCaseSensitive(flag: Boolean); message 'setCaseSensitive:';
    procedure setLocale(locale_: id); message 'setLocale:';

    { Category: NSExtendedScanner }
    function charactersToBeSkipped: NSCharacterSet; message 'charactersToBeSkipped';
    function caseSensitive: Boolean; message 'caseSensitive';
    function locale: id; message 'locale';
    function scanInt(var value: cint): Boolean; message 'scanInt:';
    function scanInteger(var value: clong): Boolean; message 'scanInteger:';
    function scanHexLongLong(var result_: culonglong): Boolean; message 'scanHexLongLong:';
    function scanHexFloat(var result_: single): Boolean; message 'scanHexFloat:';
    function scanHexDouble(var result_: double): Boolean; message 'scanHexDouble:';
    function scanHexInt(value: Pointer): Boolean; message 'scanHexInt:';
    function scanLongLong(var value: clonglong): Boolean; message 'scanLongLong:';
    function scanFloat(var value: single): Boolean; message 'scanFloat:';
    function scanDouble(var value: double): Boolean; message 'scanDouble:';
    function scanString_intoString(string__: NSString; var value: NSString): Boolean; message 'scanString:intoString:';
    function scanCharactersFromSet_intoString(set_: NSCharacterSet; var value: NSString): Boolean; message 'scanCharactersFromSet:intoString:';
    function scanUpToString_intoString(string__: NSString; var value: NSString): Boolean; message 'scanUpToString:intoString:';
    function scanUpToCharactersFromSet_intoString(set_: NSCharacterSet; var value: NSString): Boolean; message 'scanUpToCharactersFromSet:intoString:';
    function isAtEnd: Boolean; message 'isAtEnd';
    function initWithString(string__: NSString): id; message 'initWithString:';
    class function scannerWithString(string__: NSString): id; message 'scannerWithString:';
    class function localizedScannerWithString(string__: NSString): id; message 'localizedScannerWithString:';

    { Category: NSDecimalNumberScanning }
    function scanDecimal(var dcm: NSDecimal): Boolean; message 'scanDecimal:';
  end; external;

{$endif}
{$endif}
