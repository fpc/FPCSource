{ Parsed from CalendarStore.framework CalAttendee.h }


{$ifdef TYPES}
type
  CalAttendeePtr = ^CalAttendee;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CalAttendeeStatusNeedsAction: NSString; cvar; external;
  CalAttendeeStatusAccepted: NSString; cvar; external;
  CalAttendeeStatusDeclined: NSString; cvar; external;
  CalAttendeeStatusTentative: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CalAttendee = objcclass external (NSObject, NSCopyingProtocol)
  private
    _objectID: id;
    _reserved: pointer;
    _address: NSURL;
    _commonName: NSString;
    _status: NSString;
  public
    function address: NSURL; message 'address';
    function commonName: NSString; message 'commonName';
    function status: NSString; message 'status';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

