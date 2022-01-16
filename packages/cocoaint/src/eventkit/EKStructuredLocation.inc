{ Parsed from EventKit.framework EKStructuredLocation.h }


{$ifdef TYPES}
type
  EKStructuredLocationPtr = ^EKStructuredLocation;
{$endif}

{$ifdef CLASSES}

type
  EKStructuredLocation = objcclass external (EKObject, NSCopyingProtocol)
  private
  public
    class function locationWithTitle (title: NSString): EKStructuredLocation; message 'locationWithTitle:';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setGeoLocation(newValue: CLLocation); message 'setGeoLocation:';
    function geoLocation: CLLocation; message 'geoLocation';
    procedure setRadius(newValue: double); message 'setRadius:';
    function radius: double; message 'radius';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

