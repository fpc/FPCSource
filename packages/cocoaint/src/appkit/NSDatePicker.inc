{ Parsed from Appkit.framework NSDatePicker.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDATEPICKER_PAS_H}
{$define NSDATEPICKER_PAS_H}
type
  NSDatePickerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDATEPICKER_PAS_T}
{$define NSDATEPICKER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDATEPICKER_PAS_R}
{$define NSDATEPICKER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDATEPICKER_PAS_F}
{$define NSDATEPICKER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDATEPICKER_PAS_T}
{$define NSDATEPICKER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDatePicker = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATEPICKER_PAS_C}
{$define NSDATEPICKER_PAS_C}

{ NSDatePicker }
  NSDatePicker = objcclass(NSControl)
    
  public
    class function alloc: NSDatePicker; message 'alloc';

    function datePickerStyle: NSDatePickerStyle; message 'datePickerStyle';
    procedure setDatePickerStyle(newStyle: NSDatePickerStyle); message 'setDatePickerStyle:';
    function isBezeled: Boolean; message 'isBezeled';
    procedure setBezeled(flag: Boolean); message 'setBezeled:';
    function isBordered: Boolean; message 'isBordered';
    procedure setBordered(flag: Boolean); message 'setBordered:';
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
