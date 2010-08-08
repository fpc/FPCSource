{ Parsed from Foundation.framework NSDateFormatter.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
  NSDateFormatterBehavior = NSUInteger;

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

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATEFORMATTER_PAS_C}
{$define NSDATEFORMATTER_PAS_C}

{ NSDateFormatter }
  NSDateFormatter = objcclass(NSFormatter)
  private
    __attributes: NSMutableDictionary;
    __formatter: CFDateFormatterRef; {garbage collector: __strong }
    __counter: NSUInteger;
    
  public
    class function alloc: NSDateFormatter; message 'alloc';

    function init: id; message 'init';
    function getObjectValue_forString_range_error(obj: id; string_: NSString; rangep: NSRangePointer; error: NSErrorPointer): Boolean; message 'getObjectValue:forString:range:error:';
    function stringFromDate(date: NSDate): NSString; message 'stringFromDate:';
    function dateFromString(string_: NSString): NSDate; message 'dateFromString:';
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
    procedure setLongEraSymbols(array_: NSArray); message 'setLongEraSymbols:';
    procedure setVeryShortMonthSymbols(array_: NSArray); message 'setVeryShortMonthSymbols:';
    procedure setStandaloneMonthSymbols(array_: NSArray); message 'setStandaloneMonthSymbols:';
    procedure setShortStandaloneMonthSymbols(array_: NSArray); message 'setShortStandaloneMonthSymbols:';
    procedure setVeryShortStandaloneMonthSymbols(array_: NSArray); message 'setVeryShortStandaloneMonthSymbols:';
    procedure setVeryShortWeekdaySymbols(array_: NSArray); message 'setVeryShortWeekdaySymbols:';
    procedure setStandaloneWeekdaySymbols(array_: NSArray); message 'setStandaloneWeekdaySymbols:';
    procedure setShortStandaloneWeekdaySymbols(array_: NSArray); message 'setShortStandaloneWeekdaySymbols:';
    procedure setVeryShortStandaloneWeekdaySymbols(array_: NSArray); message 'setVeryShortStandaloneWeekdaySymbols:';
    procedure setQuarterSymbols(array_: NSArray); message 'setQuarterSymbols:';
    procedure setShortQuarterSymbols(array_: NSArray); message 'setShortQuarterSymbols:';
    procedure setStandaloneQuarterSymbols(array_: NSArray); message 'setStandaloneQuarterSymbols:';
    procedure setShortStandaloneQuarterSymbols(array_: NSArray); message 'setShortStandaloneQuarterSymbols:';
    procedure setGregorianStartDate(date: NSDate); message 'setGregorianStartDate:';

    { Category: NSDateFormatterCompatibility }
    function initWithDateFormat_allowNaturalLanguage(format: NSString; flag: Boolean): id; message 'initWithDateFormat:allowNaturalLanguage:';
    function allowsNaturalLanguage: Boolean; message 'allowsNaturalLanguage';
  end; external;

{$endif}
{$endif}
