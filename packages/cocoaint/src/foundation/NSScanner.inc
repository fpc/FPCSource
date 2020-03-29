{ Parsed from Foundation.framework NSScanner.h }


{$ifdef TYPES}
type
  NSScannerPtr = ^NSScanner;
{$endif}

{$ifdef CLASSES}

type
  NSScanner = objcclass external (NSObject, NSCopyingProtocol)
  public
    function string_: NSString; message 'string';
    procedure setScanLocation(newValue: NSUInteger); message 'setScanLocation:';
    function scanLocation: NSUInteger; message 'scanLocation';
    procedure setCharactersToBeSkipped(newValue: NSCharacterSet); message 'setCharactersToBeSkipped:';
    function charactersToBeSkipped: NSCharacterSet; message 'charactersToBeSkipped';
    procedure setCaseSensitive(newValue: ObjCBOOL); message 'setCaseSensitive:';
    function caseSensitive: ObjCBOOL; message 'caseSensitive';
    procedure setLocale(newValue: id); message 'setLocale:';
    function locale: id; message 'locale';
    function initWithString (string_: NSString): instancetype; message 'initWithString:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  NSExtendedScanner = objccategory external (NSScanner)
    function scanInt (result_: pcint): ObjCBOOL; message 'scanInt:';
    function scanInteger (result_: NSIntegerPtr): ObjCBOOL; message 'scanInteger:'; { available in 10_5, 2_0 }
    function scanLongLong (result_: pclonglong): ObjCBOOL; message 'scanLongLong:';
    function scanUnsignedLongLong (result_: pculonglong): ObjCBOOL; message 'scanUnsignedLongLong:'; { available in 10_9, 7_0 }
    function scanFloat (result_: psingle): ObjCBOOL; message 'scanFloat:';
    function scanDouble (result_: pdouble): ObjCBOOL; message 'scanDouble:';
    function scanHexInt (result_: pcuint): ObjCBOOL; message 'scanHexInt:';
    function scanHexLongLong (result_: pculonglong): ObjCBOOL; message 'scanHexLongLong:'; { available in 10_5, 2_0 }
    function scanHexFloat (result_: psingle): ObjCBOOL; message 'scanHexFloat:'; { available in 10_5, 2_0 }
    function scanHexDouble (result_: pdouble): ObjCBOOL; message 'scanHexDouble:'; { available in 10_5, 2_0 }
    function scanString_intoString (string_: NSString; result_: NSStringPtr): ObjCBOOL; message 'scanString:intoString:';
    function scanCharactersFromSet_intoString (set_: NSCharacterSet; result_: NSStringPtr): ObjCBOOL; message 'scanCharactersFromSet:intoString:';
    function scanUpToString_intoString (string_: NSString; result_: NSStringPtr): ObjCBOOL; message 'scanUpToString:intoString:';
    function scanUpToCharactersFromSet_intoString (set_: NSCharacterSet; result_: NSStringPtr): ObjCBOOL; message 'scanUpToCharactersFromSet:intoString:';
    function isAtEnd: ObjCBOOL; message 'isAtEnd';
    class function scannerWithString (string_: NSString): instancetype; message 'scannerWithString:';
    class function localizedScannerWithString (string_: NSString): id; message 'localizedScannerWithString:';
  end;
{$endif}

