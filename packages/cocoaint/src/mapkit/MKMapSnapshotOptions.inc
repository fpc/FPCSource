{ Parsed from MapKit.framework MKMapSnapshotOptions.h }


{ Types from MKMapSnapshotOptions }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  MKMapSnapshotOptionsPtr = ^MKMapSnapshotOptions;
{$endif}

{$ifdef CLASSES}

type
  MKMapSnapshotOptions = objcclass external (NSObject, NSCopyingProtocol)
  public
    procedure setCamera(newValue: MKMapCamera); message 'setCamera:';
    function camera: MKMapCamera; message 'camera';
    procedure setMapRect(newValue: MKMapRect); message 'setMapRect:';
    function mapRect: MKMapRect; message 'mapRect';
    procedure setRegion(newValue: MKCoordinateRegion); message 'setRegion:';
    function region: MKCoordinateRegion; message 'region';
    procedure setMapType(newValue: MKMapType); message 'setMapType:';
    function mapType: MKMapType; message 'mapType';
    procedure setShowsPointsOfInterest(newValue: ObjCBOOL); message 'setShowsPointsOfInterest:';
    function showsPointsOfInterest: ObjCBOOL; message 'showsPointsOfInterest';
    procedure setShowsBuildings(newValue: ObjCBOOL); message 'setShowsBuildings:';
    function showsBuildings: ObjCBOOL; message 'showsBuildings';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setSize(newValue: CGSize); message 'setSize:';
    function size: CGSize; message 'size';
    {$else}
    procedure setSize_(newValue: NSSize); message 'setSize:';
    function size_: NSSize; message 'size';
    {$endif}
    {$if defined(TARGET_OS_IPHONE)}
    procedure setScale(newValue: CGFloat); message 'setScale:';
    function scale: CGFloat; message 'scale';
    {$endif}

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

