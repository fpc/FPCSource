{ Parsed from Appkit.framework NSDatePickerCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:08 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSDATEPICKERCELL_PAS_T}
{$define NSDATEPICKERCELL_PAS_T}

{ Constants }

const
  NSTextFieldAndStepperDatePickerStyle = 0;
  NSClockAndCalendarDatePickerStyle = 1;
  NSTextFieldDatePickerStyle = 2;

const
  NSSingleDateMode = 0;
  NSRangeDateMode = 1;

const
  NSHourMinuteDatePickerElementFlag = $000c;
  NSHourMinuteSecondDatePickerElementFlag = $000e;
  NSTimeZoneDatePickerElementFlag = $0010;
  NSYearMonthDatePickerElementFlag = $00c0;
  NSYearMonthDayDatePickerElementFlag = $00e0;
  NSEraDatePickerElementFlag = $0100;

{ Types }
type
  NSDatePickerStyle = NSUInteger;
  NSDatePickerMode = NSUInteger;
  NSDatePickerElementFlags = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDATEPICKERCELL_PAS_R}
{$define NSDATEPICKERCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDATEPICKERCELL_PAS_F}
{$define NSDATEPICKERCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDATEPICKERCELL_PAS_S}
{$define NSDATEPICKERCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDatePickerCell = objcclass;
  NSDatePickerCellPointer = ^NSDatePickerCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATEPICKERCELL_PAS_C}
{$define NSDATEPICKERCELL_PAS_C}

{ NSDatePickerCell }
  NSDatePickerCell = objcclass(NSActionCell)
  private
    __timeInterval: NSTimeInterval;
    __minDate: NSDate;
    __maxDate: NSDate;
    __dcFlags: bitpacked record
        elements: 0..((1 shl 16)-1);
        controlStyle: 0..((1 shl 3)-1);
        controlMode: 0..((1 shl 2)-1);
        trackingHand: 0..((1 shl 2)-1);
        reserved2: 0..((1 shl 4)-1);
        drawsBackground: 0..1;
        digitsEntered: 0..((1 shl 2)-1);
        forcesLeadingZeroes: 0..1;
        wrapsDateComponentArithmetic: 0..1;
      end;
    __delegate: id;
    __calendar: NSCalendar;
    __locale: NSLocale;
    __timeZone: NSTimeZone;
    __backgroundColor: NSColor;
    __textColor: NSColor;
    __indexOfSelectedSubfield: cint;
    __reserved0: cint;
    __reserved1: id;
    __reserved2: id;
    __reserved3: id;
    __reserved4: id;
    
  public
    class function alloc: NSDatePickerCell; message 'alloc';

    function datePickerStyle: NSDatePickerStyle; message 'datePickerStyle';
    procedure setDatePickerStyle(newStyle: NSDatePickerStyle); message 'setDatePickerStyle:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function textColor: NSColor; message 'textColor';
    procedure setTextColor(color: NSColor); message 'setTextColor:';
    function datePickerMode: NSDatePickerMode; message 'datePickerMode';
    procedure setDatePickerMode(newMode: NSDatePickerMode); message 'setDatePickerMode:';
    function datePickerElements: NSDatePickerElementFlags; message 'datePickerElements';
    procedure setDatePickerElements(elementFlags: NSDatePickerElementFlags); message 'setDatePickerElements:';
    function calendar: NSCalendar; message 'calendar';
    procedure setCalendar(newCalendar: NSCalendar); message 'setCalendar:';
    function locale: NSLocale; message 'locale';
    procedure setLocale(newLocale: NSLocale); message 'setLocale:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setTimeZone(newTimeZone: NSTimeZone); message 'setTimeZone:';
    function dateValue: NSDate; message 'dateValue';
    procedure setDateValue(newStartDate: NSDate); message 'setDateValue:';
    function timeInterval: NSTimeInterval; message 'timeInterval';
    procedure setTimeInterval(newTimeInterval: NSTimeInterval); message 'setTimeInterval:';
    function minDate: NSDate; message 'minDate';
    procedure setMinDate(date: NSDate); message 'setMinDate:';
    function maxDate: NSDate; message 'maxDate';
    procedure setMaxDate(date: NSDate); message 'setMaxDate:';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
  end; external;

{$endif}
{$endif}
