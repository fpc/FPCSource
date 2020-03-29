{ Parsed from AppKit.framework NSDatePicker.h }


{$ifdef TYPES}
type
  NSDatePickerPtr = ^NSDatePicker;
{$endif}

{$ifdef CLASSES}

type
  NSDatePicker = objcclass external (NSControl)
  private
  public
    procedure setDatePickerStyle(newValue: NSDatePickerStyle); message 'setDatePickerStyle:';
    function datePickerStyle: NSDatePickerStyle; message 'datePickerStyle';
    procedure setBezeled(newValue: ObjCBOOL); message 'setBezeled:';
    function isBezeled: ObjCBOOL; message 'isBezeled';
    procedure setBordered(newValue: ObjCBOOL); message 'setBordered:';
    function isBordered: ObjCBOOL; message 'isBordered';
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

