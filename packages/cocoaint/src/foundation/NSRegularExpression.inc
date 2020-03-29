{ Parsed from Foundation.framework NSRegularExpression.h }


{$ifdef TYPES}
type
  NSRegularExpressionPtr = ^NSRegularExpression;
  NSDataDetectorPtr = ^NSDataDetector;
{$endif}

{$ifdef TYPES}
type
  NSRegularExpressionOptions = NSUInteger;
  NSRegularExpressionOptionsPtr = ^NSRegularExpressionOptions;

const
  NSRegularExpressionCaseInsensitive = 1 shl 0;
  NSRegularExpressionAllowCommentsAndWhitespace = 1 shl 1;
  NSRegularExpressionIgnoreMetacharacters = 1 shl 2;
  NSRegularExpressionDotMatchesLineSeparators = 1 shl 3;
  NSRegularExpressionAnchorsMatchLines = 1 shl 4;
  NSRegularExpressionUseUnixLineSeparators = 1 shl 5;
  NSRegularExpressionUseUnicodeWordBoundaries = 1 shl 6;
{$endif}

{$ifdef CLASSES}

type
  NSRegularExpression = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _pattern: NSString;
    _options: NSUInteger;
    _internal: pointer;
    _reserved1: id;
    _checkout: cint32;
    _reserved2: cint32;
  public
    class function regularExpressionWithPattern_options_error (pattern: NSString; options: NSRegularExpressionOptions; error: NSErrorPtr): NSRegularExpression; message 'regularExpressionWithPattern:options:error:';
    function initWithPattern_options_error (pattern: NSString; options: NSRegularExpressionOptions; error: NSErrorPtr): instancetype; message 'initWithPattern:options:error:'; { NS_DESIGNATED_INITIALIZER }
    function pattern: NSString; message 'pattern';
    function options: NSRegularExpressionOptions; message 'options';
    function numberOfCaptureGroups: NSUInteger; message 'numberOfCaptureGroups';
    class function escapedPatternForString (string_: NSString): NSString; message 'escapedPatternForString:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSMatchingOptions = NSUInteger;
  NSMatchingOptionsPtr = ^NSMatchingOptions;

const
  NSMatchingReportProgress = 1 shl 0;
  NSMatchingReportCompletion = 1 shl 1;
  NSMatchingAnchored = 1 shl 2;
  NSMatchingWithTransparentBounds = 1 shl 3;
  NSMatchingWithoutAnchoringBounds = 1 shl 4;

type
  NSMatchingFlags = NSUInteger;
  NSMatchingFlagsPtr = ^NSMatchingFlags;

const
  NSMatchingProgress = 1 shl 0;
  NSMatchingCompleted = 1 shl 1;
  NSMatchingHitEnd = 1 shl 2;
  NSMatchingRequiredEnd = 1 shl 3;
  NSMatchingInternalError = 1 shl 4;
{$endif}

{$ifdef CLASSES}

type
  NSMatching = objccategory external (NSRegularExpression)
    procedure enumerateMatchesInString_options_range_usingBlock (string_: NSString; options_: NSMatchingOptions; range: NSRange; block: OpaqueCBlock); message 'enumerateMatchesInString:options:range:usingBlock:';
    function matchesInString_options_range (string_: NSString; options_: NSMatchingOptions; range: NSRange): NSArray; message 'matchesInString:options:range:';
    function numberOfMatchesInString_options_range (string_: NSString; options_: NSMatchingOptions; range: NSRange): NSUInteger; message 'numberOfMatchesInString:options:range:';
    function firstMatchInString_options_range (string_: NSString; options_: NSMatchingOptions; range: NSRange): NSTextCheckingResult; message 'firstMatchInString:options:range:';
    function rangeOfFirstMatchInString_options_range (string_: NSString; options_: NSMatchingOptions; range: NSRange): NSRange; message 'rangeOfFirstMatchInString:options:range:';
  end;

type
  NSReplacement = objccategory external (NSRegularExpression)
    function stringByReplacingMatchesInString_options_range_withTemplate (string_: NSString; options_: NSMatchingOptions; range: NSRange; templ: NSString): NSString; message 'stringByReplacingMatchesInString:options:range:withTemplate:';
    function replaceMatchesInString_options_range_withTemplate (string_: NSMutableString; options_: NSMatchingOptions; range: NSRange; templ: NSString): NSUInteger; message 'replaceMatchesInString:options:range:withTemplate:';
    function replacementStringForResult_inString_offset_template (result_: NSTextCheckingResult; string_: NSString; offset: NSInteger; templ: NSString): NSString; message 'replacementStringForResult:inString:offset:template:';
    class function escapedTemplateForString (string_: NSString): NSString; message 'escapedTemplateForString:';
  end;


type
  NSDataDetector = objcclass external (NSRegularExpression)
  private
    _types: NSTextCheckingTypes;
  public
    class function dataDetectorWithTypes_error (checkingTypes: NSTextCheckingTypes; error: NSErrorPtr): NSDataDetector; message 'dataDetectorWithTypes:error:';
    function initWithTypes_error (checkingTypes: NSTextCheckingTypes; error: NSErrorPtr): instancetype; message 'initWithTypes:error:'; { NS_DESIGNATED_INITIALIZER }
    function checkingTypes: NSTextCheckingTypes; message 'checkingTypes';
  end;
{$endif}

