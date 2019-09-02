{ Parsed from EventKit.framework EKRecurrenceDayOfWeek.h }


{$ifdef TYPES}
type
  EKRecurrenceDayOfWeekPtr = ^EKRecurrenceDayOfWeek;
{$endif}

{$ifdef CLASSES}

type
  EKRecurrenceDayOfWeek = objcclass external (NSObject, NSCopyingProtocol)
  private
  public
    class function dayOfWeek (dayOfTheWeek: NSInteger): EKRecurrenceDayOfWeek; message 'dayOfWeek:';
    class function dayOfWeek_weekNumber (dayOfTheWeek: NSInteger; weekNumber: NSInteger): EKRecurrenceDayOfWeek; message 'dayOfWeek:weekNumber:';
    function initWithDayOfTheWeek_weekNumber (dayOfTheWeek: NSInteger; weekNumber: NSInteger): id; message 'initWithDayOfTheWeek:weekNumber:';
    function dayOfTheWeek: NSInteger; message 'dayOfTheWeek';
    function weekNumber: NSInteger; message 'weekNumber';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

