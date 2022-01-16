{ Parsed from Foundation.framework NSTextCheckingResult.h }


{$ifdef TYPES}
type
  NSTextCheckingResultPtr = ^NSTextCheckingResult;
{$endif}

{$ifdef TYPES}
type
  NSTextCheckingType = cuint64;
  NSTextCheckingTypePtr = ^NSTextCheckingType;

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
  NSTextCheckingTypeRegularExpression = 1 shl 10 { available in 10_7, 4_0 };
  NSTextCheckingTypePhoneNumber = 1 shl 11 { available in 10_7, 4_0 };
  NSTextCheckingTypeTransitInformation = 1 shl 12 { available in 10_7, 4_0 };
const
  NSTextCheckingAllSystemTypes = $ffffffff;
  NSTextCheckingAllCustomTypes = $ffffffff shl 32;
  NSTextCheckingAllTypes = NSTextCheckingAllSystemTypes + NSTextCheckingAllCustomTypes;

type
  NSTextCheckingTypes = cuint64;
  NSTextCheckingTypesPtr = ^NSTextCheckingTypes;
{$endif}

{$ifdef CLASSES}

type
  NSTextCheckingResult = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    function resultType: NSTextCheckingType; message 'resultType';
    function range: NSRange; message 'range';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSTextCheckingResultOptional = objccategory external (NSTextCheckingResult)
    function orthography: NSOrthography; message 'orthography';
    function grammarDetails: NSArray; message 'grammarDetails';
    function date: NSDate; message 'date';
    function timeZone: NSTimeZone; message 'timeZone';
    function duration: NSTimeInterval; message 'duration';
    function components: NSDictionary; message 'components';
    function URL: NSURL; message 'URL';
    function replacementString: NSString; message 'replacementString';
    function alternativeStrings: NSArray; message 'alternativeStrings';
    function regularExpression: NSRegularExpression; message 'regularExpression';
    function phoneNumber: NSString; message 'phoneNumber';
    function addressComponents: NSDictionary; message 'addressComponents';
    function numberOfRanges: NSUInteger; message 'numberOfRanges';
    function rangeAtIndex (idx: NSUInteger): NSRange; message 'rangeAtIndex:'; { available in 10_7, 4_0 }
    function resultByAdjustingRangesWithOffset (offset: NSInteger): NSTextCheckingResult; message 'resultByAdjustingRangesWithOffset:'; { available in 10_7, 5_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTextCheckingNameKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingJobTitleKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingOrganizationKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingStreetKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingCityKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingStateKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingZIPKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingCountryKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingPhoneKey: NSString { available in 10_6, 4_0 }; cvar; external;
  NSTextCheckingAirlineKey: NSString { available in 10_7, 4_0 }; cvar; external;
  NSTextCheckingFlightKey: NSString { available in 10_7, 4_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSTextCheckingResultCreation = objccategory external (NSTextCheckingResult)
    class function orthographyCheckingResultWithRange_orthography (range_: NSRange; orthography: NSOrthography): NSTextCheckingResult; message 'orthographyCheckingResultWithRange:orthography:';
    class function spellCheckingResultWithRange (range_: NSRange): NSTextCheckingResult; message 'spellCheckingResultWithRange:';
    class function grammarCheckingResultWithRange_details (range_: NSRange; details: NSArray): NSTextCheckingResult; message 'grammarCheckingResultWithRange:details:';
    class function dateCheckingResultWithRange_date (range_: NSRange; date: NSDate): NSTextCheckingResult; message 'dateCheckingResultWithRange:date:';
    class function dateCheckingResultWithRange_date_timeZone_duration (range_: NSRange; date: NSDate; timeZone: NSTimeZone; duration: NSTimeInterval): NSTextCheckingResult; message 'dateCheckingResultWithRange:date:timeZone:duration:';
    class function addressCheckingResultWithRange_components (range_: NSRange; components: NSDictionary): NSTextCheckingResult; message 'addressCheckingResultWithRange:components:';
    class function linkCheckingResultWithRange_URL (range_: NSRange; url: NSURL): NSTextCheckingResult; message 'linkCheckingResultWithRange:URL:';
    class function quoteCheckingResultWithRange_replacementString (range_: NSRange; replacementString: NSString): NSTextCheckingResult; message 'quoteCheckingResultWithRange:replacementString:';
    class function dashCheckingResultWithRange_replacementString (range_: NSRange; replacementString: NSString): NSTextCheckingResult; message 'dashCheckingResultWithRange:replacementString:';
    class function replacementCheckingResultWithRange_replacementString (range_: NSRange; replacementString: NSString): NSTextCheckingResult; message 'replacementCheckingResultWithRange:replacementString:';
    class function correctionCheckingResultWithRange_replacementString (range_: NSRange; replacementString: NSString): NSTextCheckingResult; message 'correctionCheckingResultWithRange:replacementString:';
    class function correctionCheckingResultWithRange_replacementString_alternativeStrings (range_: NSRange; replacementString: NSString; alternativeStrings: NSArray): NSTextCheckingResult; message 'correctionCheckingResultWithRange:replacementString:alternativeStrings:'; { available in 10_9, 7_0 }
    class function regularExpressionCheckingResultWithRanges_count_regularExpression (ranges: NSRangePointer; count: NSUInteger; regularExpression: NSRegularExpression): NSTextCheckingResult; message 'regularExpressionCheckingResultWithRanges:count:regularExpression:'; { available in 10_7, 4_0 }
    class function phoneNumberCheckingResultWithRange_phoneNumber (range_: NSRange; phoneNumber: NSString): NSTextCheckingResult; message 'phoneNumberCheckingResultWithRange:phoneNumber:'; { available in 10_7, 4_0 }
    class function transitInformationCheckingResultWithRange_components (range_: NSRange; components: NSDictionary): NSTextCheckingResult; message 'transitInformationCheckingResultWithRange:components:'; { available in 10_7, 4_0 }
  end;
{$endif}

