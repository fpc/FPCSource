{ Parsed from CoreLocation.framework CLRegion.h }


{$ifdef TYPES}
type
  CLRegionPtr = ^CLRegion;
{$endif}

{$ifdef TYPES}
type
  CLRegionState = NSInteger;
  CLRegionStatePtr = ^CLRegionState;

const
  CLRegionStateUnknown = 0;
  CLRegionStateInside = 1;
  CLRegionStateOutside = 2;

type
  CLProximity = NSInteger;
  CLProximityPtr = ^CLProximity;

const
  CLProximityUnknown = 0;
  CLProximityImmediate = 1;
  CLProximityNear = 2;
  CLProximityFar = 3;
{$endif}

{$ifdef CLASSES}

type
  CLRegion = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _internal: CLRegionInternal;
  public
    function initCircularRegionWithCenter_radius_identifier (center: CLLocationCoordinate2D; radius: CLLocationDistance; identifier: NSString): instancetype; message 'initCircularRegionWithCenter:radius:identifier:'; deprecated 'in __MAC_10_7,__MAC_10_10,__IPHONE_4_0,__IPHONE_7_0';
    function center: CLLocationCoordinate2D; message 'center';
    function radius: CLLocationDistance; message 'radius';
    function identifier: NSString; message 'identifier';
    procedure setNotifyOnEntry(newValue: ObjCBOOL); message 'setNotifyOnEntry:';
    function notifyOnEntry: ObjCBOOL; message 'notifyOnEntry';
    procedure setNotifyOnExit(newValue: ObjCBOOL); message 'setNotifyOnExit:';
    function notifyOnExit: ObjCBOOL; message 'notifyOnExit';
    function containsCoordinate (coordinate: CLLocationCoordinate2D): ObjCBOOL; message 'containsCoordinate:'; deprecated 'in __MAC_10_7,__MAC_10_10,__IPHONE_4_0,__IPHONE_7_0';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

