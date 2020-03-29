{ Parsed from EventKit.framework EKRecurrenceEnd.h }


{$ifdef TYPES}
type
  EKRecurrenceEndPtr = ^EKRecurrenceEnd;
{$endif}

{$ifdef CLASSES}

type
  EKRecurrenceEnd = objcclass external (NSObject, NSCopyingProtocol)
  private
  public
    class function recurrenceEndWithEndDate (endDate: NSDate): id; message 'recurrenceEndWithEndDate:';
    class function recurrenceEndWithOccurrenceCount (occurrenceCount: NSUInteger): id; message 'recurrenceEndWithOccurrenceCount:';
    function endDate: NSDate; message 'endDate';
    function occurrenceCount: NSUInteger; message 'occurrenceCount';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

