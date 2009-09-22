{ Parsed from Foundation.framework NSString.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSTRING_PAS_H}
{$define NSSTRING_PAS_H}
type
  NSStringPointer = Pointer;
  NSMutableStringPointer = Pointer;
  NSSimpleCStringPointer = Pointer;
  NSConstantStringPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSTRING_PAS_T}
{$define NSSTRING_PAS_T}

{ Types }
type
  unichar = cushort;
  NSStringCompareOptions = NSUInteger;
  NSStringEncoding = NSUInteger;
  NSStringEncodingConversionOptions = NSUInteger;

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
{$ifndef NSSTRING_PAS_T}
{$define NSSTRING_PAS_T}

{ External string constants }
var
  NSParseErrorException: NSString; external name '_NSParseErrorException';
  NSCharacterConversionException: NSString; external name '_NSCharacterConversionException';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSString = objcclass;
  NSMutableString = objcclass;
  NSSimpleCString = objcclass;
  NSConstantString = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTRING_PAS_C}
{$define NSSTRING_PAS_C}

{ NSString }
  NSString = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
    
  public
    class function alloc: NSString; message 'alloc';

    function length: NSUInteger; message 'length';
    function characterAtIndex(index: NSUInteger): unichar; message 'characterAtIndex:';

    { Category: NSStringExtensionMethods }
    procedure getCharacters(buffer: PChar); message 'getCharacters:';
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
    function stringByAppendingFormat(format: NSString): NSString; varargs; message 'stringByAppendingFormat:';
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
    procedure getLineStart_end_contentsEnd_forRange(startPtr: NSUIntegerPointer; lineEndPtr: NSUIntegerPointer; contentsEndPtr: NSUIntegerPointer; range: NSRange); message 'getLineStart:end:contentsEnd:forRange:';
    function lineRangeForRange(range: NSRange): NSRange; message 'lineRangeForRange:';
    procedure getParagraphStart_end_contentsEnd_forRange(startPtr: NSUIntegerPointer; parEndPtr: NSUIntegerPointer; contentsEndPtr: NSUIntegerPointer; range: NSRange); message 'getParagraphStart:end:contentsEnd:forRange:';
    function paragraphRangeForRange(range: NSRange): NSRange; message 'paragraphRangeForRange:';
    function description: NSString; message 'description';
    function hash: NSUInteger; message 'hash';
    function fastestEncoding: NSStringEncoding; message 'fastestEncoding';
    function smallestEncoding: NSStringEncoding; message 'smallestEncoding';
    function dataUsingEncoding_allowLossyConversion(encoding: NSStringEncoding; lossy: Boolean): NSData; message 'dataUsingEncoding:allowLossyConversion:';
    function dataUsingEncoding(encoding: NSStringEncoding): NSData; message 'dataUsingEncoding:';
    function canBeConvertedToEncoding(encoding: NSStringEncoding): Boolean; message 'canBeConvertedToEncoding:';
    function cStringUsingEncoding(encoding: NSStringEncoding): PChar; message 'cStringUsingEncoding:';
    function getCString_maxLength_encoding(buffer: Pointer; maxBufferCount: NSUInteger; encoding: NSStringEncoding): Boolean; message 'getCString:maxLength:encoding:';
    function getBytes_maxLength_usedLength_encoding_options_range_remainingRange(buffer: Pointer; maxBufferCount: NSUInteger; usedBufferCount: NSUIntegerPointer; encoding: NSStringEncoding; options: NSStringEncodingConversionOptions; range: NSRange; leftover: NSRangePointer): Boolean; message 'getBytes:maxLength:usedLength:encoding:options:range:remainingRange:';
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
    class function availableStringEncodings: NSStringEncoding; message 'availableStringEncodings';
    class function localizedNameOfStringEncoding(encoding: NSStringEncoding): NSString; message 'localizedNameOfStringEncoding:';
    function init: id; message 'init';
    function initWithCharactersNoCopy_length_freeWhenDone(characters: PChar; length_: NSUInteger; freeBuffer: Boolean): id; message 'initWithCharactersNoCopy:length:freeWhenDone:';
    function initWithCharacters_length(characters: PChar; length_: NSUInteger): id; message 'initWithCharacters:length:';
    function initWithUTF8String(nullTerminatedCString: PChar): id; message 'initWithUTF8String:';
    function initWithString(aString: NSString): id; message 'initWithString:';
    function initWithFormat(format: NSString): id; varargs; message 'initWithFormat:';
    function initWithFormat_arguments(format: NSString; argList: va_list): id; message 'initWithFormat:arguments:';
    function initWithFormat_locale(format: NSString; locale: id): id; varargs; message 'initWithFormat:locale:';
    function initWithFormat_locale_arguments(format: NSString; locale: id; argList: va_list): id; message 'initWithFormat:locale:arguments:';
    function initWithData_encoding(data: NSData; encoding: NSStringEncoding): id; message 'initWithData:encoding:';
    function initWithBytes_length_encoding(bytes: Pointer; len: NSUInteger; encoding: NSStringEncoding): id; message 'initWithBytes:length:encoding:';
    function initWithBytesNoCopy_length_encoding_freeWhenDone(bytes: Pointer; len: NSUInteger; encoding: NSStringEncoding; freeBuffer: Boolean): id; message 'initWithBytesNoCopy:length:encoding:freeWhenDone:';
    class function string_: id; message 'string';
    class function stringWithString(string__: NSString): id; message 'stringWithString:';
    class function stringWithCharacters_length(characters: PChar; length_: NSUInteger): id; message 'stringWithCharacters:length:';
    class function stringWithUTF8String(nullTerminatedCString: PChar): id; message 'stringWithUTF8String:';
    class function stringWithFormat(format: NSString): id; varargs; message 'stringWithFormat:';
    class function localizedStringWithFormat(format: NSString): id; varargs; message 'localizedStringWithFormat:';
    function initWithCString_encoding(nullTerminatedCString: PChar; encoding: NSStringEncoding): id; message 'initWithCString:encoding:';
    class function stringWithCString_encoding(cString: PChar; enc: NSStringEncoding): id; message 'stringWithCString:encoding:';
    function initWithContentsOfURL_encoding_error(url: NSURL; enc: NSStringEncoding; error: NSErrorPointer): id; message 'initWithContentsOfURL:encoding:error:';
    function initWithContentsOfFile_encoding_error(path: NSString; enc: NSStringEncoding; error: NSErrorPointer): id; message 'initWithContentsOfFile:encoding:error:';
    class function stringWithContentsOfURL_encoding_error(url: NSURL; enc: NSStringEncoding; error: NSErrorPointer): id; message 'stringWithContentsOfURL:encoding:error:';
    class function stringWithContentsOfFile_encoding_error(path: NSString; enc: NSStringEncoding; error: NSErrorPointer): id; message 'stringWithContentsOfFile:encoding:error:';
    function initWithContentsOfURL_usedEncoding_error(url: NSURL; enc: NSStringEncodingPointer; error: NSErrorPointer): id; message 'initWithContentsOfURL:usedEncoding:error:';
    function initWithContentsOfFile_usedEncoding_error(path: NSString; enc: NSStringEncodingPointer; error: NSErrorPointer): id; message 'initWithContentsOfFile:usedEncoding:error:';
    class function stringWithContentsOfURL_usedEncoding_error(url: NSURL; enc: NSStringEncodingPointer; error: NSErrorPointer): id; message 'stringWithContentsOfURL:usedEncoding:error:';
    class function stringWithContentsOfFile_usedEncoding_error(path: NSString; enc: NSStringEncodingPointer; error: NSErrorPointer): id; message 'stringWithContentsOfFile:usedEncoding:error:';
    function writeToURL_atomically_encoding_error(url: NSURL; useAuxiliaryFile: Boolean; enc: NSStringEncoding; error: NSErrorPointer): Boolean; message 'writeToURL:atomically:encoding:error:';
    function writeToFile_atomically_encoding_error(path: NSString; useAuxiliaryFile: Boolean; enc: NSStringEncoding; error: NSErrorPointer): Boolean; message 'writeToFile:atomically:encoding:error:';

    { Category: NSExtendedStringPropertyListParsing }
    function propertyList: id; message 'propertyList';
    function propertyListFromStringsFileFormat: NSDictionary; message 'propertyListFromStringsFileFormat';

    { Category: NSStringDeprecated }
    procedure getCString(bytes: Pointer); message 'getCString:';
    procedure getCString_maxLength(bytes: Pointer; maxLength: NSUInteger); message 'getCString:maxLength:';
    procedure getCString_maxLength_range_remainingRange(bytes: Pointer; maxLength: NSUInteger; aRange: NSRange; leftoverRange: NSRangePointer); message 'getCString:maxLength:range:remainingRange:';
    function writeToFile_atomically(path: NSString; useAuxiliaryFile: Boolean): Boolean; message 'writeToFile:atomically:';
    function writeToURL_atomically(url: NSURL; atomically: Boolean): Boolean; message 'writeToURL:atomically:';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    class function stringWithContentsOfFile(path: NSString): id; message 'stringWithContentsOfFile:';
    class function stringWithContentsOfURL(url: NSURL): id; message 'stringWithContentsOfURL:';
    function initWithCStringNoCopy_length_freeWhenDone(bytes: Pointer; length_: NSUInteger; freeBuffer: Boolean): id; message 'initWithCStringNoCopy:length:freeWhenDone:';
    function initWithCString_length(bytes: PChar; length_: NSUInteger): id; message 'initWithCString:length:';
    function initWithCString(bytes: PChar): id; message 'initWithCString:';
    class function stringWithCString_length(bytes: PChar; length_: NSUInteger): id; message 'stringWithCString:length:';
    class function stringWithCString(bytes: PChar): id; message 'stringWithCString:';

    { Category: NSStringPathExtensions }
    class function pathWithComponents(components: NSArray): NSString; message 'pathWithComponents:';
    function pathComponents: NSArray; message 'pathComponents';
    function isAbsolutePath: Boolean; message 'isAbsolutePath';
    function lastPathComponent: NSString; message 'lastPathComponent';
    function stringByDeletingLastPathComponent: NSString; message 'stringByDeletingLastPathComponent';
    function stringByAppendingPathComponent(str: NSString): NSString; message 'stringByAppendingPathComponent:';
    function pathExtension: NSString; message 'pathExtension';
    function stringByDeletingPathExtension: NSString; message 'stringByDeletingPathExtension';
    function stringByAppendingPathExtension(str: NSString): NSString; message 'stringByAppendingPathExtension:';
    function stringByAbbreviatingWithTildeInPath: NSString; message 'stringByAbbreviatingWithTildeInPath';
    function stringByExpandingTildeInPath: NSString; message 'stringByExpandingTildeInPath';
    function stringByStandardizingPath: NSString; message 'stringByStandardizingPath';
    function stringByResolvingSymlinksInPath: NSString; message 'stringByResolvingSymlinksInPath';
    function stringsByAppendingPaths(paths: NSArray): NSArray; message 'stringsByAppendingPaths:';
    function completePathIntoString_caseSensitive_matchesIntoArray_filterTypes(outputName: NSStringPointer; flag: Boolean; outputArray: NSArrayPointer; filterTypes: NSArray): NSUInteger; message 'completePathIntoString:caseSensitive:matchesIntoArray:filterTypes:';
    function fileSystemRepresentation: PChar; message 'fileSystemRepresentation';
    function getFileSystemRepresentation_maxLength(cname: Pointer; max: NSUInteger): Boolean; message 'getFileSystemRepresentation:maxLength:';

    { Category: NSURLUtilities }
    function stringByAddingPercentEscapesUsingEncoding(enc: NSStringEncoding): NSString; message 'stringByAddingPercentEscapesUsingEncoding:';
    function stringByReplacingPercentEscapesUsingEncoding(enc: NSStringEncoding): NSString; message 'stringByReplacingPercentEscapesUsingEncoding:';

    { Category: NSStringDrawing }
    function sizeWithAttributes(attrs: NSDictionary): NSSize; message 'sizeWithAttributes:';
    procedure drawAtPoint_withAttributes(point: NSPoint; attrs: NSDictionary); message 'drawAtPoint:withAttributes:';
    procedure drawInRect_withAttributes(rect: NSRect; attrs: NSDictionary); message 'drawInRect:withAttributes:';

    { Category: NSExtendedStringDrawing }
    procedure drawWithRect_options_attributes(rect: NSRect; options: NSStringDrawingOptions; attributes: NSDictionary); message 'drawWithRect:options:attributes:';
    function boundingRectWithSize_options_attributes(size: NSSize; options: NSStringDrawingOptions; attributes: NSDictionary): NSRect; message 'boundingRectWithSize:options:attributes:';
  end; external;

{ NSMutableString }
  NSMutableString = objcclass(NSString)
    
  public
    class function alloc: NSMutableString; message 'alloc';

    procedure replaceCharactersInRange_withString(range: NSRange; aString: NSString); message 'replaceCharactersInRange:withString:';

    { Category: NSMutableStringExtensionMethods }
    procedure insertString_atIndex(aString: NSString; loc: NSUInteger); message 'insertString:atIndex:';
    procedure deleteCharactersInRange(range: NSRange); message 'deleteCharactersInRange:';
    procedure appendString(aString: NSString); message 'appendString:';
    procedure appendFormat(format: NSString); message 'appendFormat:';
    procedure setString(aString: NSString); message 'setString:';
    function initWithCapacity(capacity: NSUInteger): id; message 'initWithCapacity:';
    class function stringWithCapacity(capacity: NSUInteger): id; message 'stringWithCapacity:';
    function replaceOccurrencesOfString_withString_options_range(target: NSString; replacement: NSString; options: NSStringCompareOptions; searchRange: NSRange): NSUInteger; message 'replaceOccurrencesOfString:withString:options:range:';
  end; external;

{ NSSimpleCString }
  NSSimpleCString = objcclass(NSString)
  private
    _bytes: char;
    _numBytes: cint;
    {$ifdef cpu64}
    __unused: cint;
    {$endif}
    
  public
    class function alloc: NSSimpleCString; message 'alloc';
  end; external;

{ NSConstantString }
  NSConstantString = objcclass(NSSimpleCString)
    
  public
    class function alloc: NSConstantString; message 'alloc';
  end; external;

{$endif}
{$endif}
