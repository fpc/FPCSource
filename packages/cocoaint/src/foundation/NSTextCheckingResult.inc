{ Parsed from Foundation.framework NSTextCheckingResult.h }

{$ifdef TYPES}
{$ifndef NSTEXTCHECKINGRESULT_PAS_T}
{$define NSTEXTCHECKINGRESULT_PAS_T}

{ Constants }

const
  NSTextCheckingTypeOrthography = 1 shl 0;
  NSTextCheckingTypeSpelling = 1 shl 1;
  NSTextCheckingTypeGrammar = 1 shl 2;
  NSTextCheckingTypeDate = 1 shl 3;
  NSTextCheckingTypeAddress = 1 shl 4;
  NSTextCheckingTypeLink = 1 shl 5;
  NSTextCheckingTypeQuote = 1 shl 6;
  NSTextCheckingTypeDash = 1 shl 7;
  NSTextCheckingTypeReplacement = 1 shl 8;
  NSTextCheckingTypeCorrection = 1 shl 9;

const
  NSTextCheckingAllSystemTypes = $ffffffff;
  NSTextCheckingAllCustomTypes = $ffffffff;

{ Types }
type
  NSTextCheckingType = cuint64;
  NSTextCheckingTypePtr = ^NSTextCheckingType;
  NSTextCheckingTypes = cuint64;
  NSTextCheckingTypesPtr = ^NSTextCheckingTypes;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTCHECKINGRESULT_PAS_R}
{$define NSTEXTCHECKINGRESULT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTCHECKINGRESULT_PAS_F}
{$define NSTEXTCHECKINGRESULT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTCHECKINGRESULT_PAS_S}
{$define NSTEXTCHECKINGRESULT_PAS_S}

{ External string constants }
var
  NSTextCheckingNameKey: NSString; cvar; external;
  NSTextCheckingJobTitleKey: NSString; cvar; external;
  NSTextCheckingOrganizationKey: NSString; cvar; external;
  NSTextCheckingStreetKey: NSString; cvar; external;
  NSTextCheckingCityKey: NSString; cvar; external;
  NSTextCheckingStateKey: NSString; cvar; external;
  NSTextCheckingZIPKey: NSString; cvar; external;
  NSTextCheckingCountryKey: NSString; cvar; external;
  NSTextCheckingPhoneKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextCheckingResult = objcclass;
  NSTextCheckingResultPointer = ^NSTextCheckingResult;
  NSTextCheckingResultPtr = NSTextCheckingResultPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTCHECKINGRESULT_PAS_C}
{$define NSTEXTCHECKINGRESULT_PAS_C}

{ NSTextCheckingResult }
  NSTextCheckingResult = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
    
  public
    function resultType: NSTextCheckingType; message 'resultType';
    function range: NSRange; message 'range';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSTextCheckingResultOptionalCategory }
  NSTextCheckingResultOptionalCategory = objccategory external (NSTextCheckingResult)
    function orthography: NSOrthography; message 'orthography';
    function grammarDetails: NSArray; message 'grammarDetails';
    function date: NSDate; message 'date';
    function timeZone: NSTimeZone; message 'timeZone';
    function duration: NSTimeInterval; message 'duration';
    function addressComponents: NSDictionary; message 'addressComponents';
    function uRL: NSURL; message 'uRL';
    function replacementString: NSString; message 'replacementString';
  end;

{ NSTextCheckingResultCreationCategory }
  NSTextCheckingResultCreationCategory = objccategory external (NSTextCheckingResult)
    class function orthographyCheckingResultWithRange_orthography(range_: NSRange; orthography_: NSOrthography): NSTextCheckingResult; message 'orthographyCheckingResultWithRange:orthography:';
    class function spellCheckingResultWithRange(range_: NSRange): NSTextCheckingResult; message 'spellCheckingResultWithRange:';
    class function grammarCheckingResultWithRange_details(range_: NSRange; details: NSArray): NSTextCheckingResult; message 'grammarCheckingResultWithRange:details:';
    class function dateCheckingResultWithRange_date(range_: NSRange; date_: NSDate): NSTextCheckingResult; message 'dateCheckingResultWithRange:date:';
    class function dateCheckingResultWithRange_date_timeZone_duration(range_: NSRange; date_: NSDate; timeZone_: NSTimeZone; duration_: NSTimeInterval): NSTextCheckingResult; message 'dateCheckingResultWithRange:date:timeZone:duration:';
    class function addressCheckingResultWithRange_components(range_: NSRange; components: NSDictionary): NSTextCheckingResult; message 'addressCheckingResultWithRange:components:';
    class function linkCheckingResultWithRange_URL(range_: NSRange; url_: NSURL): NSTextCheckingResult; message 'linkCheckingResultWithRange:URL:';
    class function quoteCheckingResultWithRange_replacementString(range_: NSRange; replacementString_: NSString): NSTextCheckingResult; message 'quoteCheckingResultWithRange:replacementString:';
    class function dashCheckingResultWithRange_replacementString(range_: NSRange; replacementString_: NSString): NSTextCheckingResult; message 'dashCheckingResultWithRange:replacementString:';
    class function replacementCheckingResultWithRange_replacementString(range_: NSRange; replacementString_: NSString): NSTextCheckingResult; message 'replacementCheckingResultWithRange:replacementString:';
    class function correctionCheckingResultWithRange_replacementString(range_: NSRange; replacementString_: NSString): NSTextCheckingResult; message 'correctionCheckingResultWithRange:replacementString:';
  end;

{$endif}
{$endif}
