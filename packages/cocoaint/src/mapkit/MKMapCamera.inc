{ Parsed from MapKit.framework MKMapCamera.h }


{$ifdef TYPES}
type
  MKMapCameraPtr = ^MKMapCamera;
{$endif}

{$ifdef CLASSES}

type
  MKMapCamera = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    procedure setCenterCoordinate(newValue: CLLocationCoordinate2D); message 'setCenterCoordinate:';
    function centerCoordinate: CLLocationCoordinate2D; message 'centerCoordinate';
    procedure setHeading(newValue: CLLocationDirection); message 'setHeading:';
    function heading: CLLocationDirection; message 'heading';
    procedure setPitch(newValue: CGFloat); message 'setPitch:';
    function pitch: CGFloat; message 'pitch';
    procedure setAltitude(newValue: CLLocationDistance); message 'setAltitude:';
    function altitude: CLLocationDistance; message 'altitude';
    class function camera: instancetype; message 'camera';
    class function cameraLookingAtCenterCoordinate_fromEyeCoordinate_eyeAltitude (centerCoordinate_: CLLocationCoordinate2D; eyeCoordinate: CLLocationCoordinate2D; eyeAltitude: CLLocationDistance): instancetype; message 'cameraLookingAtCenterCoordinate:fromEyeCoordinate:eyeAltitude:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

