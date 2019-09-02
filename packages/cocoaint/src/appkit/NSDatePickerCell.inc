{ Parsed from AppKit.framework NSDatePickerCell.h }


{$ifdef TYPES}
type
  NSDatePickerCellPtr = ^NSDatePickerCell;
  NSDatePickerCellDelegateProtocolPtr = ^NSDatePickerCellDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSDatePickerStyle = NSUInteger;
  NSDatePickerStylePtr = ^NSDatePickerStyle;

const
  NSTextFieldAndStepperDatePickerStyle = 0;
  NSClockAndCalendarDatePickerStyle = 1;
  NSTextFieldDatePickerStyle = 2;

type
  NSDatePickerMode = NSUInteger;
  NSDatePickerModePtr = ^NSDatePickerMode;

const
  NSSingleDateMode = 0;
  NSRangeDateMode = 1;

type
  NSDatePickerElementFlags = NSUInteger;
  NSDatePickerElementFlagsPtr = ^NSDatePickerElementFlags;

const
  NSHourMinuteDatePickerElementFlag = $000c;
  NSHourMinuteSecondDatePickerElementFlag = $000e;
  NSTimeZoneDatePickerElementFlag = $0010;
  NSYearMonthDatePickerElementFlag = $00c0;
  NSYearMonthDayDatePickerElementFlag = $00e0;
  NSEraDatePickerElementFlag = $0100;
{$endif}

{$ifdef CLASSES}

type
  NSDatePickerCell = objcclass external (NSActionCell)
  private
    _timeInterval: NSTimeInterval;
    _minDate: NSDate;
    _maxDate: NSDate;
    _dcFlags: bitpacked record
      case byte of
        0: (_anonBitField__dcFlags0: cuint);
        1: (
          elements: 0..((1 shl 16)-1);
          controlStyle: 0..((1 shl 3)-1);
          controlMode: 0..((1 shl 2)-1);
          trackingHand: 0..((1 shl 2)-1);
          reserved2: 0..((1 shl 4)-1);
          drawsBackground: 0..1;
          digitsEntered: 0..((1 shl 2)-1);
          forcesLeadingZeroes: 0..1;
          wrapsDateComponentArithmetic: 0..1;
        );
      end;
    _delegate: id;
    _calendar: NSCalendar;
    _locale: NSLocale;
    _timeZone: NSTimeZone;
    _backgroundColor: NSColor;
    _textColor: NSColor;
    _indexOfSelectedSubfield: cint;
    _reserved0: cint;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
    _reserved4: id;
  public
    procedure setDatePickerStyle(newValue: NSDatePickerStyle); message 'setDatePickerStyle:';
    function datePickerStyle: NSDatePickerStyle; message 'datePickerStyle';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setTextColor(newValue: NSColor); message 'setTextColor:';
    function textColor: NSColor; message 'textColor';
    procedure setDatePickerMode(newValue: NSDatePickerMode); message 'setDatePickerMode:';
    function datePickerMode: NSDatePickerMode; message 'datePickerMode';
    procedure setDatePickerElements(newValue: NSDatePickerElementFlags); message 'setDatePickerElements:';
    function datePickerElements: NSDatePickerElementFlags; message 'datePickerElements';
    procedure setCalendar(newValue: NSCalendar); message 'setCalendar:';
    function calendar: NSCalendar; message 'calendar';
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setTimeZone(newValue: NSTimeZone); message 'setTimeZone:';
    function timeZone: NSTimeZone; message 'timeZone';
    procedure setDateValue(newValue: NSDate); message 'setDateValue:';
    function dateValue: NSDate; message 'dateValue';
    procedure setTimeInterval(newValue: NSTimeInterval); message 'setTimeInterval:';
    function timeInterval: NSTimeInterval; message 'timeInterval';
    procedure setMinDate(newValue: NSDate); message 'setMinDate:';
    function minDate: NSDate; message 'minDate';
    procedure setMaxDate(newValue: NSDate); message 'setMaxDate:';
    function maxDate: NSDate; message 'maxDate';
    procedure setDelegate(newValue: NSDatePickerCellDelegateProtocol); message 'setDelegate:';
    function delegate: NSDatePickerCellDelegateProtocol; message 'delegate';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSDatePickerCellDelegateProtocol = objcprotocol external name 'NSDatePickerCellDelegate' (NSObjectProtocol)
  optional
    procedure datePickerCell_validateProposedDateValue_timeInterval (aDatePickerCell: NSDatePickerCell; proposedDateValue: NSDatePtr; proposedTimeInterval: NSTimeIntervalPtr); message 'datePickerCell:validateProposedDateValue:timeInterval:';
  end;
{$endif}

