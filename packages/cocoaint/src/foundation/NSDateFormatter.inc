{ Parsed from Foundation.framework NSDateFormatter.h }

{$ifdef TYPES}
{$ifndef NSDATEFORMATTER_PAS_T}
{$define NSDATEFORMATTER_PAS_T}

{ Constants }

const
  NSDateFormatterNoStyle = kCFDateFormatterNoStyle;
  NSDateFormatterShortStyle = kCFDateFormatterShortStyle;
  NSDateFormatterMediumStyle = kCFDateFormatterMediumStyle;
  NSDateFormatterLongStyle = kCFDateFormatterLongStyle;
  NSDateFormatterFullStyle = kCFDateFormatterFullStyle;

const
  NSDateFormatterBehaviorDefault = 0;
  NSDateFormatterBehavior10_0 = 1000;
  NSDateFormatterBehavior10_4 = 1040;

{ Types }
type
  NSDateFormatterStyle = NSUInteger;
  NSDateFormatterStylePtr = ^NSDateFormatterStyle;
  NSDateFormatterBehavior = NSUInteger;
  NSDateFormatterBehaviorPtr = ^NSDateFormatterBehavior;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDATEFORMATTER_PAS_R}
{$define NSDATEFORMATTER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDATEFORMATTER_PAS_F}
{$define NSDATEFORMATTER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDATEFORMATTER_PAS_S}
{$define NSDATEFORMATTER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDateFormatter = objcclass;
  NSDateFormatterPointer = ^NSDateFormatter;
  NSDateFormatterPtr = NSDateFormatterPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATEFORMATTER_PAS_C}
{$define NSDATEFORMATTER_PAS_C}

{ NSDateFormatter }
  NSDateFormatter = objcclass external (NSFormatter)
  private
    _attributes: NSMutableDictionary;
    _formatter: CFDateFormatterRef; {garbage collector: __strong }
    _counter: NSUInteger;
    
  public
    function getObjectValue_forString_range_error(obj: idPtr; string_: NSString; rangep: NSRangePtr; error: NSErrorPointer): Boolean; message 'getObjectValue:forString:range:error:';
    function stringFromDate(date: NSDate): NSString; message 'stringFromDate:';
    function dateFromString(string_: NSString): NSDate; message 'dateFromString:';
    class function localizedStringFromDate_dateStyle_timeStyle(date: NSDate; dstyle: NSDateFormatterStyle; tstyle: NSDateFormatterStyle): NSString; message 'localizedStringFromDate:dateStyle:timeStyle:';
    class function dateFormatFromTemplate_options_locale(tmplate: NSString; opts: NSUInteger; locale_: NSLocale): NSString; message 'dateFormatFromTemplate:options:locale:';
    function dateFormat: NSString; message 'dateFormat';
    function dateStyle: NSDateFormatterStyle; message 'dateStyle';
    procedure setDateStyle(style: NSDateFormatterStyle); message 'setDateStyle:';
    function timeStyle: NSDateFormatterStyle; message 'timeStyle';
    procedure setTimeStyle(style: NSDateFormatterStyle); message 'setTimeStyle:';
    function locale: NSLocale; message 'locale';
    procedure setLocale(locale_: NSLocale); message 'setLocale:';
    function generatesCalendarDates: Boolean; message 'generatesCalendarDates';
    procedure setGeneratesCalendarDates(b: Boolean); message 'setGeneratesCalendarDates:';
    function formatterBehavior: NSDateFormatterBehavior; message 'formatterBehavior';
    procedure setFormatterBehavior(behavior: NSDateFormatterBehavior); message 'setFormatterBehavior:';
    class function defaultFormatterBehavior: NSDateFormatterBehavior; message 'defaultFormatterBehavior';
    class procedure setDefaultFormatterBehavior(behavior: NSDateFormatterBehavior); message 'setDefaultFormatterBehavior:';
    procedure setDateFormat(string_: NSString); message 'setDateFormat:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setTimeZone(tz: NSTimeZone); message 'setTimeZone:';
    function calendar: NSCalendar; message 'calendar';
    procedure setCalendar(calendar_: NSCalendar); message 'setCalendar:';
    function isLenient: Boolean; message 'isLenient';
    procedure setLenient(b: Boolean); message 'setLenient:';
    function twoDigitStartDate: NSDate; message 'twoDigitStartDate';
    procedure setTwoDigitStartDate(date: NSDate); message 'setTwoDigitStartDate:';
    function defaultDate: NSDate; message 'defaultDate';
    procedure setDefaultDate(date: NSDate); message 'setDefaultDate:';
    function eraSymbols: NSArray; message 'eraSymbols';
    procedure setEraSymbols(array_: NSArray); message 'setEraSymbols:';
    function monthSymbols: NSArray; message 'monthSymbols';
    procedure setMonthSymbols(array_: NSArray); message 'setMonthSymbols:';
    function shortMonthSymbols: NSArray; message 'shortMonthSymbols';
    procedure setShortMonthSymbols(array_: NSArray); message 'setShortMonthSymbols:';
    function weekdaySymbols: NSArray; message 'weekdaySymbols';
    procedure setWeekdaySymbols(array_: NSArray); message 'setWeekdaySymbols:';
    function shortWeekdaySymbols: NSArray; message 'shortWeekdaySymbols';
    procedure setShortWeekdaySymbols(array_: NSArray); message 'setShortWeekdaySymbols:';
    function AMSymbol: NSString; message 'AMSymbol';
    procedure setAMSymbol(string_: NSString); message 'setAMSymbol:';
    function PMSymbol: NSString; message 'PMSymbol';
    procedure setPMSymbol(string_: NSString); message 'setPMSymbol:';
    function longEraSymbols: NSArray; message 'longEraSymbols';
    procedure setLongEraSymbols(array_: NSArray); message 'setLongEraSymbols:';
    function veryShortMonthSymbols: NSArray; message 'veryShortMonthSymbols';
    procedure setVeryShortMonthSymbols(array_: NSArray); message 'setVeryShortMonthSymbols:';
    function standaloneMonthSymbols: NSArray; message 'standaloneMonthSymbols';
    procedure setStandaloneMonthSymbols(array_: NSArray); message 'setStandaloneMonthSymbols:';
    function shortStandaloneMonthSymbols: NSArray; message 'shortStandaloneMonthSymbols';
    procedure setShortStandaloneMonthSymbols(array_: NSArray); message 'setShortStandaloneMonthSymbols:';
    function veryShortStandaloneMonthSymbols: NSArray; message 'veryShortStandaloneMonthSymbols';
    procedure setVeryShortStandaloneMonthSymbols(array_: NSArray); message 'setVeryShortStandaloneMonthSymbols:';
    function veryShortWeekdaySymbols: NSArray; message 'veryShortWeekdaySymbols';
    procedure setVeryShortWeekdaySymbols(array_: NSArray); message 'setVeryShortWeekdaySymbols:';
    function standaloneWeekdaySymbols: NSArray; message 'standaloneWeekdaySymbols';
    procedure setStandaloneWeekdaySymbols(array_: NSArray); message 'setStandaloneWeekdaySymbols:';
    function shortStandaloneWeekdaySymbols: NSArray; message 'shortStandaloneWeekdaySymbols';
    procedure setShortStandaloneWeekdaySymbols(array_: NSArray); message 'setShortStandaloneWeekdaySymbols:';
    function veryShortStandaloneWeekdaySymbols: NSArray; message 'veryShortStandaloneWeekdaySymbols';
    procedure setVeryShortStandaloneWeekdaySymbols(array_: NSArray); message 'setVeryShortStandaloneWeekdaySymbols:';
    function quarterSymbols: NSArray; message 'quarterSymbols';
    procedure setQuarterSymbols(array_: NSArray); message 'setQuarterSymbols:';
    function shortQuarterSymbols: NSArray; message 'shortQuarterSymbols';
    procedure setShortQuarterSymbols(array_: NSArray); message 'setShortQuarterSymbols:';
    function standaloneQuarterSymbols: NSArray; message 'standaloneQuarterSymbols';
    procedure setStandaloneQuarterSymbols(array_: NSArray); message 'setStandaloneQuarterSymbols:';
    function shortStandaloneQuarterSymbols: NSArray; message 'shortStandaloneQuarterSymbols';
    procedure setShortStandaloneQuarterSymbols(array_: NSArray); message 'setShortStandaloneQuarterSymbols:';
    function gregorianStartDate: NSDate; message 'gregorianStartDate';
    procedure setGregorianStartDate(date: NSDate); message 'setGregorianStartDate:';
    function doesRelativeDateFormatting: Boolean; message 'doesRelativeDateFormatting';
    procedure setDoesRelativeDateFormatting(b: Boolean); message 'setDoesRelativeDateFormatting:';
  end;

{ NSDateFormatterCompatibilityCategory }
  NSDateFormatterCompatibilityCategory = objccategory external (NSDateFormatter)
    function initWithDateFormat_allowNaturalLanguage(format: NSString; flag: Boolean): id; message 'initWithDateFormat:allowNaturalLanguage:';
    function allowsNaturalLanguage: Boolean; message 'allowsNaturalLanguage';
  end;

{$endif}
{$endif}
