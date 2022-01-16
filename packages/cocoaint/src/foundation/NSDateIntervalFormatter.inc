{ Parsed from Foundation.framework NSDateIntervalFormatter.h }


{$ifdef TYPES}
type
  NSDateIntervalFormatterPtr = ^NSDateIntervalFormatter;
{$endif}

{$ifdef TYPES}
type
  NSDateIntervalFormatterStyle = NSUInteger;
  NSDateIntervalFormatterStylePtr = ^NSDateIntervalFormatterStyle;

const
  NSDateIntervalFormatterNoStyle = 0;
  NSDateIntervalFormatterShortStyle = 1;
  NSDateIntervalFormatterMediumStyle = 2;
  NSDateIntervalFormatterLongStyle = 3;
  NSDateIntervalFormatterFullStyle = 4;
{$endif}

{$ifdef CLASSES}

type
  NSDateIntervalFormatter = objcclass external (NSFormatter)
  private
    _locale: NSLocale;
    _calendar: NSCalendar;
    _timeZone: NSTimeZone;
    _dateTemplate: NSString;
    _dateTemplateFromStyles: NSString;
    _formatter: pointer;
    _dateStyle: NSDateIntervalFormatterStyle;
    _timeStyle: NSDateIntervalFormatterStyle;
    _modified: ObjCBOOL;
    _useTemplate: ObjCBOOL;
    _lock: dispatch_semaphore_t;
    _reserved:array[0..3] of pointer;
  public
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setCalendar(newValue: NSCalendar); message 'setCalendar:';
    function calendar: NSCalendar; message 'calendar';
    procedure setTimeZone(newValue: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setDateTemplate(newValue: NSString); message 'setDateTemplate:';
    function dateTemplate: NSString; message 'dateTemplate';
    procedure setDateStyle(newValue: NSDateIntervalFormatterStyle); message 'setDateStyle:';
    function dateStyle: NSDateIntervalFormatterStyle; message 'dateStyle';
    procedure setTimeStyle(newValue: NSDateIntervalFormatterStyle); message 'setTimeStyle:';
    function timeStyle: NSDateIntervalFormatterStyle; message 'timeStyle';
    function stringFromDate_toDate (fromDate: NSDate; toDate: NSDate): NSString; message 'stringFromDate:toDate:';
  end;
{$endif}

