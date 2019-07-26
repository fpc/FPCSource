{ Parsed from CoreLocation.framework CLBeaconRegion.h }


{$ifdef TYPES}
type
  CLBeaconRegionPtr = ^CLBeaconRegion;
  CLBeaconPtr = ^CLBeacon;
{$endif}

{$ifdef TYPES}
type
  CLBeaconMajorValue = cuint16;
  CLBeaconMajorValuePtr = ^CLBeaconMajorValue;
  CLBeaconMinorValue = cuint16;
  CLBeaconMinorValuePtr = ^CLBeaconMinorValue;
{$endif}

{$ifdef CLASSES}

type
  CLBeaconRegion = objcclass external (CLRegion)
  public
    function initWithProximityUUID_identifier (proximityUUID: NSUUID; identifier_: NSString): instancetype; message 'initWithProximityUUID:identifier:';
    function initWithProximityUUID_major_identifier (proximityUUID: NSUUID; major: CLBeaconMajorValue; identifier_: NSString): instancetype; message 'initWithProximityUUID:major:identifier:';
    function initWithProximityUUID_major_minor_identifier (proximityUUID: NSUUID; major: CLBeaconMajorValue; minor: CLBeaconMinorValue; identifier_: NSString): instancetype; message 'initWithProximityUUID:major:minor:identifier:';
    function peripheralDataWithMeasuredPower (measuredPower: NSNumber): NSMutableDictionary; message 'peripheralDataWithMeasuredPower:';
    function proximityUUID: NSUUID; message 'proximityUUID';
    function major: NSNumber; message 'major';
    function minor: NSNumber; message 'minor';
    procedure setNotifyEntryStateOnDisplay(newValue: ObjCBOOL); message 'setNotifyEntryStateOnDisplay:';
    function notifyEntryStateOnDisplay: ObjCBOOL; message 'notifyEntryStateOnDisplay';
  end;

type
  CLBeacon = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _internal: CLBeaconInternal;
  public
    function proximityUUID: NSUUID; message 'proximityUUID';
    function major: NSNumber; message 'major';
    function minor: NSNumber; message 'minor';
    function proximity: CLProximity; message 'proximity';
    function accuracy: CLLocationAccuracy; message 'accuracy';
    function rssi: NSInteger; message 'rssi';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

