{ Parsed from EventKit.framework EKAlarm.h }


{$ifdef TYPES}
type
  EKAlarmPtr = ^EKAlarm;
{$endif}

{$ifdef CLASSES}

type
  EKAlarm = objcclass external (EKObject, NSCopyingProtocol)
  private
  public
    class function alarmWithAbsoluteDate (date: NSDate): EKAlarm; message 'alarmWithAbsoluteDate:';
    class function alarmWithRelativeOffset (offset: NSTimeInterval): EKAlarm; message 'alarmWithRelativeOffset:';
    procedure setRelativeOffset(newValue: NSTimeInterval); message 'setRelativeOffset:';
    function relativeOffset: NSTimeInterval; message 'relativeOffset';
    procedure setAbsoluteDate(newValue: NSDate); message 'setAbsoluteDate:';
    function absoluteDate: NSDate; message 'absoluteDate';
    procedure setStructuredLocation(newValue: EKStructuredLocation); message 'setStructuredLocation:';
    function structuredLocation: EKStructuredLocation; message 'structuredLocation';
    procedure setProximity(newValue: EKAlarmProximity); message 'setProximity:';
    function proximity: EKAlarmProximity; message 'proximity';
    function type_: EKAlarmType; message 'type';
    procedure setEmailAddress(newValue: NSString); message 'setEmailAddress:';
    function emailAddress: NSString; message 'emailAddress';
    procedure setSoundName(newValue: NSString); message 'setSoundName:';
    function soundName: NSString; message 'soundName';
    procedure setUrl(newValue: NSURL); message 'setUrl:';
    function url: NSURL; message 'url';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

