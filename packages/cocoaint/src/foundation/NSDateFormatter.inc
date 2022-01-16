{ Parsed from Foundation.framework NSDateFormatter.h }


{ Types from NSDateFormatter }
{$ifdef TYPES}

type
  NSDateFormatterStyle = NSUInteger;
type
  NSDateFormatterStylePtr = ^NSDateFormatterStyle;

const
  NSDateFormatterNoStyle = kCFDateFormatterNoStyle;
  NSDateFormatterShortStyle = kCFDateFormatterShortStyle;
  NSDateFormatterMediumStyle = kCFDateFormatterMediumStyle;
  NSDateFormatterLongStyle = kCFDateFormatterLongStyle;
  NSDateFormatterFullStyle = kCFDateFormatterFullStyle;

type
  NSDateFormatterBehavior = NSUInteger;
type
  NSDateFormatterBehaviorPtr = ^NSDateFormatterBehavior;

const
  NSDateFormatterBehaviorDefault = 0;
  {$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
  NSDateFormatterBehavior10_0 = 1000;
  {$endif}
  NSDateFormatterBehavior10_4 = 1040;
{$endif}


{$ifdef TYPES}
type
  NSDateFormatterPtr = ^NSDateFormatter;
{$endif}

{$ifdef CLASSES}

type
  NSDateFormatter = objcclass external (NSFormatter)
  private
    _attributes: NSMutableDictionary;
    _formatter: CFDateFormatterRef;
    _counter: NSUInteger;
  public
    procedure setFormattingContext(newValue: NSFormattingContext); message 'setFormattingContext:';
    function formattingContext: NSFormattingContext; message 'formattingContext';
    function getObjectValue_forString_range_error (obj: idPtr; string_: NSString; rangep: NSRangePtr; error: NSErrorPtr): ObjCBOOL; message 'getObjectValue:forString:range:error:';
    function stringFromDate (date: NSDate): NSString; message 'stringFromDate:';
    function dateFromString (string_: NSString): NSDate; message 'dateFromString:';
    class function localizedStringFromDate_dateStyle_timeStyle (date: NSDate; dstyle: NSDateFormatterStyle; tstyle: NSDateFormatterStyle): NSString; message 'localizedStringFromDate:dateStyle:timeStyle:'; { available in 10_6, 4_0 }
    class function dateFormatFromTemplate_options_locale (tmplate: NSString; opts: NSUInteger; locale: NSLocale): NSString; message 'dateFormatFromTemplate:options:locale:'; { available in 10_6, 4_0 }
    class function defaultFormatterBehavior: NSDateFormatterBehavior; message 'defaultFormatterBehavior';
    class procedure setDefaultFormatterBehavior (behavior: NSDateFormatterBehavior); message 'setDefaultFormatterBehavior:';
    procedure setLocalizedDateFormatFromTemplate (dateFormatTemplate: NSString); message 'setLocalizedDateFormatFromTemplate:'; { available in 10_10, 8_0 }
    procedure setDateFormat(newValue: NSString); message 'setDateFormat:';
    function dateFormat: NSString; message 'dateFormat';
    procedure setDateStyle(newValue: NSDateFormatterStyle); message 'setDateStyle:';
    function dateStyle: NSDateFormatterStyle; message 'dateStyle';
    procedure setTimeStyle(newValue: NSDateFormatterStyle); message 'setTimeStyle:';
    function timeStyle: NSDateFormatterStyle; message 'timeStyle';
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setGeneratesCalendarDates(newValue: ObjCBOOL); message 'setGeneratesCalendarDates:';
    function generatesCalendarDates: ObjCBOOL; message 'generatesCalendarDates';
    procedure setFormatterBehavior(newValue: NSDateFormatterBehavior); message 'setFormatterBehavior:';
    function formatterBehavior: NSDateFormatterBehavior; message 'formatterBehavior';
    procedure setTimeZone(newValue: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setCalendar(newValue: NSCalendar); message 'setCalendar:';
    function calendar: NSCalendar; message 'calendar';
    procedure setLenient(newValue: ObjCBOOL); message 'setLenient:';
    function isLenient: ObjCBOOL; message 'isLenient';
    procedure setTwoDigitStartDate(newValue: NSDate); message 'setTwoDigitStartDate:';
    function twoDigitStartDate: NSDate; message 'twoDigitStartDate';
    procedure setDefaultDate(newValue: NSDate); message 'setDefaultDate:';
    function defaultDate: NSDate; message 'defaultDate';
    procedure setEraSymbols(newValue: NSArray); message 'setEraSymbols:';
    function eraSymbols: NSArray; message 'eraSymbols';
    procedure setMonthSymbols(newValue: NSArray); message 'setMonthSymbols:';
    function monthSymbols: NSArray; message 'monthSymbols';
    procedure setShortMonthSymbols(newValue: NSArray); message 'setShortMonthSymbols:';
    function shortMonthSymbols: NSArray; message 'shortMonthSymbols';
    procedure setWeekdaySymbols(newValue: NSArray); message 'setWeekdaySymbols:';
    function weekdaySymbols: NSArray; message 'weekdaySymbols';
    procedure setShortWeekdaySymbols(newValue: NSArray); message 'setShortWeekdaySymbols:';
    function shortWeekdaySymbols: NSArray; message 'shortWeekdaySymbols';
    procedure setAMSymbol(newValue: NSString); message 'setAMSymbol:';
    function AMSymbol: NSString; message 'AMSymbol';
    procedure setPMSymbol(newValue: NSString); message 'setPMSymbol:';
    function PMSymbol: NSString; message 'PMSymbol';
    procedure setLongEraSymbols(newValue: NSArray); message 'setLongEraSymbols:';
    function longEraSymbols: NSArray; message 'longEraSymbols';
    procedure setVeryShortMonthSymbols(newValue: NSArray); message 'setVeryShortMonthSymbols:';
    function veryShortMonthSymbols: NSArray; message 'veryShortMonthSymbols';
    procedure setStandaloneMonthSymbols(newValue: NSArray); message 'setStandaloneMonthSymbols:';
    function standaloneMonthSymbols: NSArray; message 'standaloneMonthSymbols';
    procedure setShortStandaloneMonthSymbols(newValue: NSArray); message 'setShortStandaloneMonthSymbols:';
    function shortStandaloneMonthSymbols: NSArray; message 'shortStandaloneMonthSymbols';
    procedure setVeryShortStandaloneMonthSymbols(newValue: NSArray); message 'setVeryShortStandaloneMonthSymbols:';
    function veryShortStandaloneMonthSymbols: NSArray; message 'veryShortStandaloneMonthSymbols';
    procedure setVeryShortWeekdaySymbols(newValue: NSArray); message 'setVeryShortWeekdaySymbols:';
    function veryShortWeekdaySymbols: NSArray; message 'veryShortWeekdaySymbols';
    procedure setStandaloneWeekdaySymbols(newValue: NSArray); message 'setStandaloneWeekdaySymbols:';
    function standaloneWeekdaySymbols: NSArray; message 'standaloneWeekdaySymbols';
    procedure setShortStandaloneWeekdaySymbols(newValue: NSArray); message 'setShortStandaloneWeekdaySymbols:';
    function shortStandaloneWeekdaySymbols: NSArray; message 'shortStandaloneWeekdaySymbols';
    procedure setVeryShortStandaloneWeekdaySymbols(newValue: NSArray); message 'setVeryShortStandaloneWeekdaySymbols:';
    function veryShortStandaloneWeekdaySymbols: NSArray; message 'veryShortStandaloneWeekdaySymbols';
    procedure setQuarterSymbols(newValue: NSArray); message 'setQuarterSymbols:';
    function quarterSymbols: NSArray; message 'quarterSymbols';
    procedure setShortQuarterSymbols(newValue: NSArray); message 'setShortQuarterSymbols:';
    function shortQuarterSymbols: NSArray; message 'shortQuarterSymbols';
    procedure setStandaloneQuarterSymbols(newValue: NSArray); message 'setStandaloneQuarterSymbols:';
    function standaloneQuarterSymbols: NSArray; message 'standaloneQuarterSymbols';
    procedure setShortStandaloneQuarterSymbols(newValue: NSArray); message 'setShortStandaloneQuarterSymbols:';
    function shortStandaloneQuarterSymbols: NSArray; message 'shortStandaloneQuarterSymbols';
    procedure setGregorianStartDate(newValue: NSDate); message 'setGregorianStartDate:';
    function gregorianStartDate: NSDate; message 'gregorianStartDate';
    procedure setDoesRelativeDateFormatting(newValue: ObjCBOOL); message 'setDoesRelativeDateFormatting:';
    function doesRelativeDateFormatting: ObjCBOOL; message 'doesRelativeDateFormatting';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  NSDateFormatterCompatibility = objccategory external (NSDateFormatter)
    function initWithDateFormat_allowNaturalLanguage (format: NSString; flag: ObjCBOOL): id; message 'initWithDateFormat:allowNaturalLanguage:'; deprecated 'in 10_4, 10_9, "Use -setDateFormat: as of OS X 10.9 to set the date format of a 10.0-style date formatter; but expect 10.0-style date formatting to be deprecated in the future as well"';
    function allowsNaturalLanguage: ObjCBOOL; message 'allowsNaturalLanguage'; deprecated 'in 10_4, 10_9, "There is no replacement"';
  end;
{$endif}

{$endif}
