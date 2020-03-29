{ Parsed from Foundation.framework NSNumberFormatter.h }


{ Types from NSNumberFormatter }
{$ifdef TYPES}

type
  NSNumberFormatterStyle = NSUInteger;
type
  NSNumberFormatterStylePtr = ^NSNumberFormatterStyle;

const
  NSNumberFormatterNoStyle = kCFNumberFormatterNoStyle;
  NSNumberFormatterDecimalStyle = kCFNumberFormatterDecimalStyle;
  NSNumberFormatterCurrencyStyle = kCFNumberFormatterCurrencyStyle;
  NSNumberFormatterPercentStyle = kCFNumberFormatterPercentStyle;
  NSNumberFormatterScientificStyle = kCFNumberFormatterScientificStyle;
  NSNumberFormatterSpellOutStyle = kCFNumberFormatterSpellOutStyle;

type
  NSNumberFormatterPadPosition = NSUInteger;
type
  NSNumberFormatterPadPositionPtr = ^NSNumberFormatterPadPosition;

const
  NSNumberFormatterPadBeforePrefix = kCFNumberFormatterPadBeforePrefix;
  NSNumberFormatterPadAfterPrefix = kCFNumberFormatterPadAfterPrefix;
  NSNumberFormatterPadBeforeSuffix = kCFNumberFormatterPadBeforeSuffix;
  NSNumberFormatterPadAfterSuffix = kCFNumberFormatterPadAfterSuffix;

type
  NSNumberFormatterRoundingMode = NSUInteger;
type
  NSNumberFormatterRoundingModePtr = ^NSNumberFormatterRoundingMode;

const
  NSNumberFormatterRoundCeiling = kCFNumberFormatterRoundCeiling;
  NSNumberFormatterRoundFloor = kCFNumberFormatterRoundFloor;
  NSNumberFormatterRoundDown = kCFNumberFormatterRoundDown;
  NSNumberFormatterRoundUp = kCFNumberFormatterRoundUp;
  NSNumberFormatterRoundHalfEven = kCFNumberFormatterRoundHalfEven;
  NSNumberFormatterRoundHalfDown = kCFNumberFormatterRoundHalfDown;
  NSNumberFormatterRoundHalfUp = kCFNumberFormatterRoundHalfUp;
{$endif}


{$ifdef TYPES}
type
  NSNumberFormatterPtr = ^NSNumberFormatter;
{$endif}

{$ifdef TYPES}
type
  NSNumberFormatterBehavior = NSUInteger;
  NSNumberFormatterBehaviorPtr = ^NSNumberFormatterBehavior;

const
  NSNumberFormatterBehaviorDefault = 0;
  {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
  NSNumberFormatterBehavior10_0 = 1000;
  {$endif}
  NSNumberFormatterBehavior10_4 = 1040;
{$endif}

{$ifdef CLASSES}

type
  NSNumberFormatter = objcclass external (NSFormatter)
  private
    _attributes: NSMutableDictionary;
    _formatter: CFNumberFormatterRef;
    _counter: NSUInteger;
    _behavior: NSNumberFormatterBehavior;
    _lock: NSRecursiveLock;
    _stateBitMask: culong;
    _reserved:array[0..8] of pointer;
  public
    procedure setFormattingContext(newValue: NSFormattingContext); message 'setFormattingContext:';
    function formattingContext: NSFormattingContext; message 'formattingContext';
    function getObjectValue_forString_range_error (obj: idPtr; string_: NSString; rangep: NSRangePtr; error: NSErrorPtr): ObjCBOOL; message 'getObjectValue:forString:range:error:';
    function stringFromNumber (number: NSNumber): NSString; message 'stringFromNumber:';
    function numberFromString (string_: NSString): NSNumber; message 'numberFromString:';
    class function localizedStringFromNumber_numberStyle (num: NSNumber; nstyle: NSNumberFormatterStyle): NSString; message 'localizedStringFromNumber:numberStyle:'; { available in 10_6, 4_0 }
    class function defaultFormatterBehavior: NSNumberFormatterBehavior; message 'defaultFormatterBehavior';
    class procedure setDefaultFormatterBehavior (behavior: NSNumberFormatterBehavior); message 'setDefaultFormatterBehavior:';
    procedure setNumberStyle(newValue: NSNumberFormatterStyle); message 'setNumberStyle:';
    function numberStyle: NSNumberFormatterStyle; message 'numberStyle';
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setGeneratesDecimalNumbers(newValue: ObjCBOOL); message 'setGeneratesDecimalNumbers:';
    function generatesDecimalNumbers: ObjCBOOL; message 'generatesDecimalNumbers';
    procedure setFormatterBehavior(newValue: NSNumberFormatterBehavior); message 'setFormatterBehavior:';
    function formatterBehavior: NSNumberFormatterBehavior; message 'formatterBehavior';
    procedure setNegativeFormat(newValue: NSString); message 'setNegativeFormat:';
    function negativeFormat: NSString; message 'negativeFormat';
    procedure setTextAttributesForNegativeValues(newValue: NSDictionary); message 'setTextAttributesForNegativeValues:';
    function textAttributesForNegativeValues: NSDictionary; message 'textAttributesForNegativeValues';
    procedure setPositiveFormat(newValue: NSString); message 'setPositiveFormat:';
    function positiveFormat: NSString; message 'positiveFormat';
    procedure setTextAttributesForPositiveValues(newValue: NSDictionary); message 'setTextAttributesForPositiveValues:';
    function textAttributesForPositiveValues: NSDictionary; message 'textAttributesForPositiveValues';
    procedure setAllowsFloats(newValue: ObjCBOOL); message 'setAllowsFloats:';
    function allowsFloats: ObjCBOOL; message 'allowsFloats';
    procedure setDecimalSeparator(newValue: NSString); message 'setDecimalSeparator:';
    function decimalSeparator: NSString; message 'decimalSeparator';
    procedure setAlwaysShowsDecimalSeparator(newValue: ObjCBOOL); message 'setAlwaysShowsDecimalSeparator:';
    function alwaysShowsDecimalSeparator: ObjCBOOL; message 'alwaysShowsDecimalSeparator';
    procedure setCurrencyDecimalSeparator(newValue: NSString); message 'setCurrencyDecimalSeparator:';
    function currencyDecimalSeparator: NSString; message 'currencyDecimalSeparator';
    procedure setUsesGroupingSeparator(newValue: ObjCBOOL); message 'setUsesGroupingSeparator:';
    function usesGroupingSeparator: ObjCBOOL; message 'usesGroupingSeparator';
    procedure setGroupingSeparator(newValue: NSString); message 'setGroupingSeparator:';
    function groupingSeparator: NSString; message 'groupingSeparator';
    procedure setZeroSymbol(newValue: NSString); message 'setZeroSymbol:';
    function zeroSymbol: NSString; message 'zeroSymbol';
    procedure setTextAttributesForZero(newValue: NSDictionary); message 'setTextAttributesForZero:';
    function textAttributesForZero: NSDictionary; message 'textAttributesForZero';
    procedure setNilSymbol(newValue: NSString); message 'setNilSymbol:';
    function nilSymbol: NSString; message 'nilSymbol';
    procedure setTextAttributesForNil(newValue: NSDictionary); message 'setTextAttributesForNil:';
    function textAttributesForNil: NSDictionary; message 'textAttributesForNil';
    procedure setNotANumberSymbol(newValue: NSString); message 'setNotANumberSymbol:';
    function notANumberSymbol: NSString; message 'notANumberSymbol';
    procedure setTextAttributesForNotANumber(newValue: NSDictionary); message 'setTextAttributesForNotANumber:';
    function textAttributesForNotANumber: NSDictionary; message 'textAttributesForNotANumber';
    procedure setPositiveInfinitySymbol(newValue: NSString); message 'setPositiveInfinitySymbol:';
    function positiveInfinitySymbol: NSString; message 'positiveInfinitySymbol';
    procedure setTextAttributesForPositiveInfinity(newValue: NSDictionary); message 'setTextAttributesForPositiveInfinity:';
    function textAttributesForPositiveInfinity: NSDictionary; message 'textAttributesForPositiveInfinity';
    procedure setNegativeInfinitySymbol(newValue: NSString); message 'setNegativeInfinitySymbol:';
    function negativeInfinitySymbol: NSString; message 'negativeInfinitySymbol';
    procedure setTextAttributesForNegativeInfinity(newValue: NSDictionary); message 'setTextAttributesForNegativeInfinity:';
    function textAttributesForNegativeInfinity: NSDictionary; message 'textAttributesForNegativeInfinity';
    procedure setPositivePrefix(newValue: NSString); message 'setPositivePrefix:';
    function positivePrefix: NSString; message 'positivePrefix';
    procedure setPositiveSuffix(newValue: NSString); message 'setPositiveSuffix:';
    function positiveSuffix: NSString; message 'positiveSuffix';
    procedure setNegativePrefix(newValue: NSString); message 'setNegativePrefix:';
    function negativePrefix: NSString; message 'negativePrefix';
    procedure setNegativeSuffix(newValue: NSString); message 'setNegativeSuffix:';
    function negativeSuffix: NSString; message 'negativeSuffix';
    procedure setCurrencyCode(newValue: NSString); message 'setCurrencyCode:';
    function currencyCode: NSString; message 'currencyCode';
    procedure setCurrencySymbol(newValue: NSString); message 'setCurrencySymbol:';
    function currencySymbol: NSString; message 'currencySymbol';
    procedure setInternationalCurrencySymbol(newValue: NSString); message 'setInternationalCurrencySymbol:';
    function internationalCurrencySymbol: NSString; message 'internationalCurrencySymbol';
    procedure setPercentSymbol(newValue: NSString); message 'setPercentSymbol:';
    function percentSymbol: NSString; message 'percentSymbol';
    procedure setPerMillSymbol(newValue: NSString); message 'setPerMillSymbol:';
    function perMillSymbol: NSString; message 'perMillSymbol';
    procedure setMinusSign(newValue: NSString); message 'setMinusSign:';
    function minusSign: NSString; message 'minusSign';
    procedure setPlusSign(newValue: NSString); message 'setPlusSign:';
    function plusSign: NSString; message 'plusSign';
    procedure setExponentSymbol(newValue: NSString); message 'setExponentSymbol:';
    function exponentSymbol: NSString; message 'exponentSymbol';
    procedure setGroupingSize(newValue: NSUInteger); message 'setGroupingSize:';
    function groupingSize: NSUInteger; message 'groupingSize';
    procedure setSecondaryGroupingSize(newValue: NSUInteger); message 'setSecondaryGroupingSize:';
    function secondaryGroupingSize: NSUInteger; message 'secondaryGroupingSize';
    procedure setMultiplier(newValue: NSNumber); message 'setMultiplier:';
    function multiplier: NSNumber; message 'multiplier';
    procedure setFormatWidth(newValue: NSUInteger); message 'setFormatWidth:';
    function formatWidth: NSUInteger; message 'formatWidth';
    procedure setPaddingCharacter(newValue: NSString); message 'setPaddingCharacter:';
    function paddingCharacter: NSString; message 'paddingCharacter';
    procedure setPaddingPosition(newValue: NSNumberFormatterPadPosition); message 'setPaddingPosition:';
    function paddingPosition: NSNumberFormatterPadPosition; message 'paddingPosition';
    procedure setRoundingMode(newValue: NSNumberFormatterRoundingMode); message 'setRoundingMode:';
    function roundingMode: NSNumberFormatterRoundingMode; message 'roundingMode';
    procedure setRoundingIncrement(newValue: NSNumber); message 'setRoundingIncrement:';
    function roundingIncrement: NSNumber; message 'roundingIncrement';
    procedure setMinimumIntegerDigits(newValue: NSUInteger); message 'setMinimumIntegerDigits:';
    function minimumIntegerDigits: NSUInteger; message 'minimumIntegerDigits';
    procedure setMaximumIntegerDigits(newValue: NSUInteger); message 'setMaximumIntegerDigits:';
    function maximumIntegerDigits: NSUInteger; message 'maximumIntegerDigits';
    procedure setMinimumFractionDigits(newValue: NSUInteger); message 'setMinimumFractionDigits:';
    function minimumFractionDigits: NSUInteger; message 'minimumFractionDigits';
    procedure setMaximumFractionDigits(newValue: NSUInteger); message 'setMaximumFractionDigits:';
    function maximumFractionDigits: NSUInteger; message 'maximumFractionDigits';
    procedure setMinimum(newValue: NSNumber); message 'setMinimum:';
    function minimum: NSNumber; message 'minimum';
    procedure setMaximum(newValue: NSNumber); message 'setMaximum:';
    function maximum: NSNumber; message 'maximum';
    procedure setCurrencyGroupingSeparator(newValue: NSString); message 'setCurrencyGroupingSeparator:';
    function currencyGroupingSeparator: NSString; message 'currencyGroupingSeparator';
    procedure setLenient(newValue: ObjCBOOL); message 'setLenient:';
    function isLenient: ObjCBOOL; message 'isLenient';
    procedure setUsesSignificantDigits(newValue: ObjCBOOL); message 'setUsesSignificantDigits:';
    function usesSignificantDigits: ObjCBOOL; message 'usesSignificantDigits';
    procedure setMinimumSignificantDigits(newValue: NSUInteger); message 'setMinimumSignificantDigits:';
    function minimumSignificantDigits: NSUInteger; message 'minimumSignificantDigits';
    procedure setMaximumSignificantDigits(newValue: NSUInteger); message 'setMaximumSignificantDigits:';
    function maximumSignificantDigits: NSUInteger; message 'maximumSignificantDigits';
    procedure setPartialStringValidationEnabled(newValue: ObjCBOOL); message 'setPartialStringValidationEnabled:';
    function isPartialStringValidationEnabled: ObjCBOOL; message 'isPartialStringValidationEnabled';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  NSNumberFormatterCompatibility = objccategory external (NSNumberFormatter)
    procedure setHasThousandSeparators(newValue: ObjCBOOL); message 'setHasThousandSeparators:';
    function hasThousandSeparators: ObjCBOOL; message 'hasThousandSeparators';
    procedure setThousandSeparator(newValue: NSString); message 'setThousandSeparator:';
    function thousandSeparator: NSString; message 'thousandSeparator';
    procedure setLocalizesFormat(newValue: ObjCBOOL); message 'setLocalizesFormat:';
    function localizesFormat: ObjCBOOL; message 'localizesFormat';
    procedure setFormat(newValue: NSString); message 'setFormat:';
    function format: NSString; message 'format';
    procedure setAttributedStringForZero(newValue: NSAttributedString); message 'setAttributedStringForZero:';
    function attributedStringForZero: NSAttributedString; message 'attributedStringForZero';
    procedure setAttributedStringForNil(newValue: NSAttributedString); message 'setAttributedStringForNil:';
    function attributedStringForNil: NSAttributedString; message 'attributedStringForNil';
    procedure setAttributedStringForNotANumber(newValue: NSAttributedString); message 'setAttributedStringForNotANumber:';
    function attributedStringForNotANumber: NSAttributedString; message 'attributedStringForNotANumber';
    procedure setRoundingBehavior(newValue: NSDecimalNumberHandler); message 'setRoundingBehavior:';
    function roundingBehavior: NSDecimalNumberHandler; message 'roundingBehavior';
  end;
{$endif}

{$endif}
