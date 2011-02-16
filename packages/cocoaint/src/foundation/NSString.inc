{ Parsed from Foundation.framework NSString.h }

{$ifdef TYPES}
{$ifndef NSSTRING_PAS_T}
{$define NSSTRING_PAS_T}

{ Types }
type
  unichar = cushort;
  unicharPtr = ^unichar;
  NSStringCompareOptions = NSUInteger;
  NSStringCompareOptionsPtr = ^NSStringCompareOptions;
  NSStringEncoding = NSUInteger;
  NSStringEncodingPtr = ^NSStringEncoding;
  NSStringEncodingConversionOptions = NSUInteger;
  NSStringEncodingConversionOptionsPtr = ^NSStringEncodingConversionOptions;

{ Constants }

const
  NSCaseInsensitiveSearch = 1;
  NSLiteralSearch = 2;
  NSBackwardsSearch = 4;
  NSAnchoredSearch = 8;
  NSNumericSearch = 64;
  NSDiacriticInsensitiveSearch = 128;
  NSWidthInsensitiveSearch = 256;
  NSForcedOrderingSearch = 512;

const
  NSASCIIStringEncoding = 1;
  NSNEXTSTEPStringEncoding = 2;
  NSJapaneseEUCStringEncoding = 3;
  NSUTF8StringEncoding = 4;
  NSISOLatin1StringEncoding = 5;
  NSSymbolStringEncoding = 6;
  NSNonLossyASCIIStringEncoding = 7;
  NSShiftJISStringEncoding = 8;
  NSISOLatin2StringEncoding = 9;
  NSUnicodeStringEncoding = 10;
  NSWindowsCP1251StringEncoding = 11;
  NSWindowsCP1252StringEncoding = 12;
  NSWindowsCP1253StringEncoding = 13;
  NSWindowsCP1254StringEncoding = 14;
  NSWindowsCP1250StringEncoding = 15;
  NSISO2022JPStringEncoding = 21;
  NSMacOSRomanStringEncoding = 30;
  NSUTF16StringEncoding = NSUnicodeStringEncoding;
  NSUTF16BigEndianStringEncoding = $90000100;
  NSUTF16LittleEndianStringEncoding = $94000100;
  NSUTF32StringEncoding = $8c000100;
  NSUTF32BigEndianStringEncoding = $98000100;
  NSUTF32LittleEndianStringEncoding = $9c000100;

const
  NSStringEncodingConversionAllowLossy = 1;
  NSStringEncodingConversionExternalRepresentation = 2;

const
  NSProprietaryStringEncoding = 65536;

{ Defines }
const
  NS_UNICHAR_IS_EIGHT_BIT = 0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSTRING_PAS_R}
{$define NSSTRING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSTRING_PAS_F}
{$define NSSTRING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSTRING_PAS_S}
{$define NSSTRING_PAS_S}

{ External string constants }
var
  NSParseErrorException: NSString; cvar; external;
  NSCharacterConversionException: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSString = objcclass;
  NSStringPointer = ^NSString;
  NSStringPtr = NSStringPointer;
  NSMutableString = objcclass;
  NSMutableStringPointer = ^NSMutableString;
  NSMutableStringPtr = NSMutableStringPointer;
  NSSimpleCString = objcclass;
  NSSimpleCStringPointer = ^NSSimpleCString;
  NSSimpleCStringPtr = NSSimpleCStringPointer;
  NSConstantString = objcclass;
  NSConstantStringPointer = ^NSConstantString;
  NSConstantStringPtr = NSConstantStringPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTRING_PAS_C}
{$define NSSTRING_PAS_C}

{ NSString }
  NSString = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    function length: NSUInteger; message 'length';
    function characterAtIndex(index: NSUInteger): unichar; message 'characterAtIndex:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSMutableString }
  NSMutableString = objcclass external (NSString)
    
  public
    procedure replaceCharactersInRange_withString(range: NSRange; aString: NSString); message 'replaceCharactersInRange:withString:';
  end;

{ NSSimpleCString }
  NSSimpleCString = objcclass external (NSString)
  private
    bytes: PChar;
    numBytes: cint;
    {$ifdef cpu64}
    _unused: cint;
    {$endif}
    
  public
  end;

{ NSConstantString }
  NSConstantString = objcclass external (NSSimpleCString)
    
  public
  end;

{ NSStringExtensionMethodsCategory }
  NSStringExtensionMethodsCategory = objccategory external (NSString)
    procedure getCharacters_range(buffer: PChar; aRange: NSRange); message 'getCharacters:range:';
    function substringFromIndex(from: NSUInteger): NSString; message 'substringFromIndex:';
    function substringToIndex(to_: NSUInteger): NSString; message 'substringToIndex:';
    function substringWithRange(range: NSRange): NSString; message 'substringWithRange:';
    function compare(string__: NSString): NSComparisonResult; message 'compare:';
    function compare_options(string__: NSString; mask: NSStringCompareOptions): NSComparisonResult; message 'compare:options:';
    function compare_options_range(string__: NSString; mask: NSStringCompareOptions; compareRange: NSRange): NSComparisonResult; message 'compare:options:range:';
    function compare_options_range_locale(string__: NSString; mask: NSStringCompareOptions; compareRange: NSRange; locale: id): NSComparisonResult; message 'compare:options:range:locale:';
    function caseInsensitiveCompare(string__: NSString): NSComparisonResult; message 'caseInsensitiveCompare:';
    function localizedCompare(string__: NSString): NSComparisonResult; message 'localizedCompare:';
    function localizedCaseInsensitiveCompare(string__: NSString): NSComparisonResult; message 'localizedCaseInsensitiveCompare:';
    function localizedStandardCompare(string__: NSString): NSComparisonResult; message 'localizedStandardCompare:';
    function isEqualToString(aString: NSString): Boolean; message 'isEqualToString:';
    function hasPrefix(aString: NSString): Boolean; message 'hasPrefix:';
    function hasSuffix(aString: NSString): Boolean; message 'hasSuffix:';
    function rangeOfString(aString: NSString): NSRange; message 'rangeOfString:';
    function rangeOfString_options(aString: NSString; mask: NSStringCompareOptions): NSRange; message 'rangeOfString:options:';
    function rangeOfString_options_range(aString: NSString; mask: NSStringCompareOptions; searchRange: NSRange): NSRange; message 'rangeOfString:options:range:';
    function rangeOfString_options_range_locale(aString: NSString; mask: NSStringCompareOptions; searchRange: NSRange; locale: NSLocale): NSRange; message 'rangeOfString:options:range:locale:';
    function rangeOfCharacterFromSet(aSet: NSCharacterSet): NSRange; message 'rangeOfCharacterFromSet:';
    function rangeOfCharacterFromSet_options(aSet: NSCharacterSet; mask: NSStringCompareOptions): NSRange; message 'rangeOfCharacterFromSet:options:';
    function rangeOfCharacterFromSet_options_range(aSet: NSCharacterSet; mask: NSStringCompareOptions; searchRange: NSRange): NSRange; message 'rangeOfCharacterFromSet:options:range:';
    function rangeOfComposedCharacterSequenceAtIndex(index: NSUInteger): NSRange; message 'rangeOfComposedCharacterSequenceAtIndex:';
    function rangeOfComposedCharacterSequencesForRange(range: NSRange): NSRange; message 'rangeOfComposedCharacterSequencesForRange:';
    function stringByAppendingString(aString: NSString): NSString; message 'stringByAppendingString:';
    function stringByAppendingFormat(firstKey: id): NSString; varargs; message 'stringByAppendingFormat:';
    function doubleValue: double; message 'doubleValue';
    function floatValue: single; message 'floatValue';
    function intValue: cint; message 'intValue';
    function integerValue: NSInteger; message 'integerValue';
    function longLongValue: clonglong; message 'longLongValue';
    function boolValue: Boolean; message 'boolValue';
    function componentsSeparatedByString(separator: NSString): NSArray; message 'componentsSeparatedByString:';
    function componentsSeparatedByCharactersInSet(separator: NSCharacterSet): NSArray; message 'componentsSeparatedByCharactersInSet:';
    function commonPrefixWithString_options(aString: NSString; mask: NSStringCompareOptions): NSString; message 'commonPrefixWithString:options:';
    function uppercaseString: NSString; message 'uppercaseString';
    function lowercaseString: NSString; message 'lowercaseString';
    function capitalizedString: NSString; message 'capitalizedString';
    function stringByTrimmingCharactersInSet(set_: NSCharacterSet): NSString; message 'stringByTrimmingCharactersInSet:';
    function stringByPaddingToLength_withString_startingAtIndex(newLength: NSUInteger; padString: NSString; padIndex: NSUInteger): NSString; message 'stringByPaddingToLength:withString:startingAtIndex:';
    procedure getLineStart_end_contentsEnd_forRange(startPtr: NSUIntegerPtr; lineEndPtr: NSUIntegerPtr; contentsEndPtr: NSUIntegerPtr; range: NSRange); message 'getLineStart:end:contentsEnd:forRange:';
    function lineRangeForRange(range: NSRange): NSRange; message 'lineRangeForRange:';
    procedure getParagraphStart_end_contentsEnd_forRange(startPtr: NSUIntegerPtr; parEndPtr: NSUIntegerPtr; contentsEndPtr: NSUIntegerPtr; range: NSRange); message 'getParagraphStart:end:contentsEnd:forRange:';
    function paragraphRangeForRange(range: NSRange): NSRange; message 'paragraphRangeForRange:';
    function description: NSString; message 'description';
    function hash: NSUInteger; message 'hash';
    function fastestEncoding: NSStringEncoding; message 'fastestEncoding';
    function smallestEncoding: NSStringEncoding; message 'smallestEncoding';
    function dataUsingEncoding_allowLossyConversion(encoding: NSStringEncoding; lossy: Boolean): NSData; message 'dataUsingEncoding:allowLossyConversion:';
    function dataUsingEncoding(encoding: NSStringEncoding): NSData; message 'dataUsingEncoding:';
    function canBeConvertedToEncoding(encoding: NSStringEncoding): Boolean; message 'canBeConvertedToEncoding:';
    function cStringUsingEncoding(encoding: NSStringEncoding): PChar; message 'cStringUsingEncoding:';
    function getCString_maxLength_encoding(buffer: PChar; maxBufferCount: NSUInteger; encoding: NSStringEncoding): Boolean; message 'getCString:maxLength:encoding:';
    function getBytes_maxLength_usedLength_encoding_options_range_remainingRange(buffer: Pointer; maxBufferCount: NSUInteger; usedBufferCount: NSUIntegerPtr; encoding: NSStringEncoding; options: NSStringEncodingConversionOptions; range: NSRange; leftover: NSRangePointer): Boolean; message 'getBytes:maxLength:usedLength:encoding:options:range:remainingRange:';
    function maximumLengthOfBytesUsingEncoding(enc: NSStringEncoding): NSUInteger; message 'maximumLengthOfBytesUsingEncoding:';
    function lengthOfBytesUsingEncoding(enc: NSStringEncoding): NSUInteger; message 'lengthOfBytesUsingEncoding:';
    function decomposedStringWithCanonicalMapping: NSString; message 'decomposedStringWithCanonicalMapping';
    function precomposedStringWithCanonicalMapping: NSString; message 'precomposedStringWithCanonicalMapping';
    function decomposedStringWithCompatibilityMapping: NSString; message 'decomposedStringWithCompatibilityMapping';
    function precomposedStringWithCompatibilityMapping: NSString; message 'precomposedStringWithCompatibilityMapping';
    function stringByFoldingWithOptions_locale(options: NSStringCompareOptions; locale: NSLocale): NSString; message 'stringByFoldingWithOptions:locale:';
    function stringByReplacingOccurrencesOfString_withString_options_range(target: NSString; replacement: NSString; options: NSStringCompareOptions; searchRange: NSRange): NSString; message 'stringByReplacingOccurrencesOfString:withString:options:range:';
    function stringByReplacingOccurrencesOfString_withString(target: NSString; replacement: NSString): NSString; message 'stringByReplacingOccurrencesOfString:withString:';
    function stringByReplacingCharactersInRange_withString(range: NSRange; replacement: NSString): NSString; message 'stringByReplacingCharactersInRange:withString:';
    function UTF8String: PChar; message 'UTF8String';
    class function defaultCStringEncoding: NSStringEncoding; message 'defaultCStringEncoding';
    class function availableStringEncodings: NSStringEncodingPtr; message 'availableStringEncodings';
    class function localizedNameOfStringEncoding(encoding: NSStringEncoding): NSString; message 'localizedNameOfStringEncoding:';
    function init: id; message 'init';
    function initWithCharactersNoCopy_length_freeWhenDone(characters: PChar; length_: NSUInteger; freeBuffer: Boolean): id; message 'initWithCharactersNoCopy:length:freeWhenDone:';
    function initWithCharacters_length(characters: PChar; length_: NSUInteger): id; message 'initWithCharacters:length:';
    function initWithUTF8String(nullTerminatedCString: PChar): id; message 'initWithUTF8String:';
    function initWithString(aString: NSString): id; message 'initWithString:';
    function initWithFormat(firstKey: id): id; varargs; message 'initWithFormat:';
    function initWithFormat_arguments(format: NSString; argList: va_list): id; message 'initWithFormat:arguments:';
    function initWithFormat_locale(format: NSString; firstKey: id): id; varargs; message 'initWithFormat:locale:';
    function initWithFormat_locale_arguments(format: NSString; locale: id; argList: va_list): id; message 'initWithFormat:locale:arguments:';
    function initWithData_encoding(data: NSData; encoding: NSStringEncoding): id; message 'initWithData:encoding:';
    function initWithBytes_length_encoding(bytes: Pointer; len: NSUInteger; encoding: NSStringEncoding): id; message 'initWithBytes:length:encoding:';
    function initWithBytesNoCopy_length_encoding_freeWhenDone(bytes: Pointer; len: NSUInteger; encoding: NSStringEncoding; freeBuffer: Boolean): id; message 'initWithBytesNoCopy:length:encoding:freeWhenDone:';
    class function string_: id; message 'string';
    class function stringWithString(string__: NSString): id; message 'stringWithString:';
    class function stringWithCharacters_length(characters: PChar; length_: NSUInteger): id; message 'stringWithCharacters:length:';
    class function stringWithUTF8String(nullTerminatedCString: PChar): id; message 'stringWithUTF8String:';
    class function stringWithFormat(firstKey: id): id; varargs; message 'stringWithFormat:';
    class function localizedStringWithFormat(firstKey: id): id; varargs; message 'localizedStringWithFormat:';
    function initWithCString_encoding(nullTerminatedCString: PChar; encoding: NSStringEncoding): id; message 'initWithCString:encoding:';
    class function stringWithCString_encoding(cString_: PChar; enc: NSStringEncoding): id; message 'stringWithCString:encoding:';
    function initWithContentsOfURL_encoding_error(url: NSURL; enc: NSStringEncoding; error: NSErrorPointer): id; message 'initWithContentsOfURL:encoding:error:';
    function initWithContentsOfFile_encoding_error(path: NSString; enc: NSStringEncoding; error: NSErrorPointer): id; message 'initWithContentsOfFile:encoding:error:';
    class function stringWithContentsOfURL_encoding_error(url: NSURL; enc: NSStringEncoding; error: NSErrorPointer): id; message 'stringWithContentsOfURL:encoding:error:';
    class function stringWithContentsOfFile_encoding_error(path: NSString; enc: NSStringEncoding; error: NSErrorPointer): id; message 'stringWithContentsOfFile:encoding:error:';
    function initWithContentsOfURL_usedEncoding_error(url: NSURL; enc: NSStringEncodingPtr; error: NSErrorPointer): id; message 'initWithContentsOfURL:usedEncoding:error:';
    function initWithContentsOfFile_usedEncoding_error(path: NSString; enc: NSStringEncodingPtr; error: NSErrorPointer): id; message 'initWithContentsOfFile:usedEncoding:error:';
    class function stringWithContentsOfURL_usedEncoding_error(url: NSURL; enc: NSStringEncodingPtr; error: NSErrorPointer): id; message 'stringWithContentsOfURL:usedEncoding:error:';
    class function stringWithContentsOfFile_usedEncoding_error(path: NSString; enc: NSStringEncodingPtr; error: NSErrorPointer): id; message 'stringWithContentsOfFile:usedEncoding:error:';
    function writeToURL_atomically_encoding_error(url: NSURL; useAuxiliaryFile: Boolean; enc: NSStringEncoding; error: NSErrorPointer): Boolean; message 'writeToURL:atomically:encoding:error:';
    function writeToFile_atomically_encoding_error(path: NSString; useAuxiliaryFile: Boolean; enc: NSStringEncoding; error: NSErrorPointer): Boolean; message 'writeToFile:atomically:encoding:error:';
  end;

{ NSMutableStringExtensionMethodsCategory }
  NSMutableStringExtensionMethodsCategory = objccategory external (NSMutableString)
    procedure insertString_atIndex(aString: NSString; loc: NSUInteger); message 'insertString:atIndex:';
    procedure deleteCharactersInRange(range: NSRange); message 'deleteCharactersInRange:';
    procedure appendString(aString: NSString); message 'appendString:';
    procedure appendFormat(firstKey: id); varargs; message 'appendFormat:';
    procedure setString(aString: NSString); message 'setString:';
    function initWithCapacity(capacity: NSUInteger): id; message 'initWithCapacity:';
    class function stringWithCapacity(capacity: NSUInteger): id; message 'stringWithCapacity:';
    function replaceOccurrencesOfString_withString_options_range(target: NSString; replacement: NSString; options: NSStringCompareOptions; searchRange: NSRange): NSUInteger; message 'replaceOccurrencesOfString:withString:options:range:';
  end;

{ NSExtendedStringPropertyListParsingCategory }
  NSExtendedStringPropertyListParsingCategory = objccategory external (NSString)
    function propertyList: id; message 'propertyList';
    function propertyListFromStringsFileFormat: NSDictionary; message 'propertyListFromStringsFileFormat';
  end;

{ NSStringDeprecatedCategory }
  NSStringDeprecatedCategory = objccategory external (NSString)
    function cString: PChar; message 'cString'; deprecated 'in Mac OS X 10.4 and later';
    function lossyCString: PChar; message 'lossyCString'; deprecated 'in Mac OS X 10.4 and later';
    function cStringLength: NSUInteger; message 'cStringLength'; deprecated 'in Mac OS X 10.4 and later';
    procedure getCString(bytes: PChar); message 'getCString:'; deprecated 'in Mac OS X 10.4 and later';
    procedure getCString_maxLength(bytes: PChar; maxLength: NSUInteger); message 'getCString:maxLength:'; deprecated 'in Mac OS X 10.4 and later';
    procedure getCString_maxLength_range_remainingRange(bytes: PChar; maxLength: NSUInteger; aRange: NSRange; leftoverRange: NSRangePointer); message 'getCString:maxLength:range:remainingRange:'; deprecated 'in Mac OS X 10.4 and later';
    function writeToFile_atomically(path: NSString; useAuxiliaryFile: Boolean): Boolean; message 'writeToFile:atomically:'; deprecated 'in Mac OS X 10.4 and later';
    function writeToURL_atomically(url: NSURL; atomically: Boolean): Boolean; message 'writeToURL:atomically:'; deprecated 'in Mac OS X 10.4 and later';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:'; deprecated 'in Mac OS X 10.4 and later';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:'; deprecated 'in Mac OS X 10.4 and later';
    class function stringWithContentsOfFile(path: NSString): id; message 'stringWithContentsOfFile:'; deprecated 'in Mac OS X 10.4 and later';
    class function stringWithContentsOfURL(url: NSURL): id; message 'stringWithContentsOfURL:'; deprecated 'in Mac OS X 10.4 and later';
    function initWithCStringNoCopy_length_freeWhenDone(bytes: PChar; length_: NSUInteger; freeBuffer: Boolean): id; message 'initWithCStringNoCopy:length:freeWhenDone:'; deprecated 'in Mac OS X 10.4 and later';
    function initWithCString_length(bytes: PChar; length_: NSUInteger): id; message 'initWithCString:length:'; deprecated 'in Mac OS X 10.4 and later';
    function initWithCString(bytes: PChar): id; message 'initWithCString:'; deprecated 'in Mac OS X 10.4 and later';
    class function stringWithCString_length(bytes: PChar; length_: NSUInteger): id; message 'stringWithCString:length:'; deprecated 'in Mac OS X 10.4 and later';
    class function stringWithCString(bytes: PChar): id; message 'stringWithCString:'; deprecated 'in Mac OS X 10.4 and later';
    procedure getCharacters(buffer: PChar); message 'getCharacters:';
  end;

{$endif}
{$endif}
