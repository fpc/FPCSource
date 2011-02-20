{ Parsed from Foundation.framework NSCharacterSet.h }

{$ifdef TYPES}
{$ifndef NSCHARACTERSET_PAS_T}
{$define NSCHARACTERSET_PAS_T}

{ Constants }

const
  NSOpenStepUnicodeReservedBase = $F400;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCHARACTERSET_PAS_R}
{$define NSCHARACTERSET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCHARACTERSET_PAS_F}
{$define NSCHARACTERSET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCHARACTERSET_PAS_S}
{$define NSCHARACTERSET_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCharacterSet = objcclass;
  NSCharacterSetPointer = ^NSCharacterSet;
  NSCharacterSetPtr = NSCharacterSetPointer;
  NSMutableCharacterSet = objcclass;
  NSMutableCharacterSetPointer = ^NSMutableCharacterSet;
  NSMutableCharacterSetPtr = NSMutableCharacterSetPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCHARACTERSET_PAS_C}
{$define NSCHARACTERSET_PAS_C}

{ NSCharacterSet }
  NSCharacterSet = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    class function controlCharacterSet: id; message 'controlCharacterSet';
    class function whitespaceCharacterSet: id; message 'whitespaceCharacterSet';
    class function whitespaceAndNewlineCharacterSet: id; message 'whitespaceAndNewlineCharacterSet';
    class function decimalDigitCharacterSet: id; message 'decimalDigitCharacterSet';
    class function letterCharacterSet: id; message 'letterCharacterSet';
    class function lowercaseLetterCharacterSet: id; message 'lowercaseLetterCharacterSet';
    class function uppercaseLetterCharacterSet: id; message 'uppercaseLetterCharacterSet';
    class function nonBaseCharacterSet: id; message 'nonBaseCharacterSet';
    class function alphanumericCharacterSet: id; message 'alphanumericCharacterSet';
    class function decomposableCharacterSet: id; message 'decomposableCharacterSet';
    class function illegalCharacterSet: id; message 'illegalCharacterSet';
    class function punctuationCharacterSet: id; message 'punctuationCharacterSet';
    class function capitalizedLetterCharacterSet: id; message 'capitalizedLetterCharacterSet';
    class function symbolCharacterSet: id; message 'symbolCharacterSet';
    class function newlineCharacterSet: id; message 'newlineCharacterSet';
    class function characterSetWithRange(aRange: NSRange): id; message 'characterSetWithRange:';
    class function characterSetWithCharactersInString(aString: NSString): id; message 'characterSetWithCharactersInString:';
    class function characterSetWithBitmapRepresentation(data: NSData): id; message 'characterSetWithBitmapRepresentation:';
    class function characterSetWithContentsOfFile(fName: NSString): id; message 'characterSetWithContentsOfFile:';
    function characterIsMember(aCharacter: unichar): Boolean; message 'characterIsMember:';
    function bitmapRepresentation: NSData; message 'bitmapRepresentation';
    function invertedSet: NSCharacterSet; message 'invertedSet';
    function longCharacterIsMember(theLongChar: UTF32Char): Boolean; message 'longCharacterIsMember:';
    function isSupersetOfSet(theOtherSet: NSCharacterSet): Boolean; message 'isSupersetOfSet:';
    function hasMemberInPlane(thePlane: cuint8): Boolean; message 'hasMemberInPlane:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSMutableCharacterSet }
  NSMutableCharacterSet = objcclass external (NSCharacterSet, NSCopyingProtocol, NSMutableCopyingProtocol)
    
  public
    procedure addCharactersInRange(aRange: NSRange); message 'addCharactersInRange:';
    procedure removeCharactersInRange(aRange: NSRange); message 'removeCharactersInRange:';
    procedure addCharactersInString(aString: NSString); message 'addCharactersInString:';
    procedure removeCharactersInString(aString: NSString); message 'removeCharactersInString:';
    procedure formUnionWithCharacterSet(otherSet: NSCharacterSet); message 'formUnionWithCharacterSet:';
    procedure formIntersectionWithCharacterSet(otherSet: NSCharacterSet); message 'formIntersectionWithCharacterSet:';
    procedure invert; message 'invert';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
