{ Parsed from MapKit.framework MKGeometry.h }

{$ifdef TYPES}
type
  MKCoordinateSpan = record
    latitudeDelta: CLLocationDegrees;
    longitudeDelta: CLLocationDegrees;
  end;
type
  MKCoordinateSpanPtr = ^MKCoordinateSpan;
type
  MKCoordinateRegion = record
    center: CLLocationCoordinate2D;
    span: MKCoordinateSpan;
  end;
type
  MKCoordinateRegionPtr = ^MKCoordinateRegion;
{$endif}

{$ifdef FUNCTIONS}
function MKCoordinateRegionMakeWithDistance(centerCoordinate: CLLocationCoordinate2D; latitudinalMeters: CLLocationDistance; longitudinalMeters: CLLocationDistance): MKCoordinateRegion; cdecl; external;
{$endif}

{$if ((__IPHONE_4_0 <= __IPHONE_OS_VERSION_MAX_ALLOWED) or (__MAC_10_9 <= __MAC_OS_X_VERSION_MAX_ALLOWED))}
{$ifdef TYPES}
type
  MKMapPoint = record
    x: double;
    y: double;
  end;
type
  MKMapPointPtr = ^MKMapPoint;
type
  MKMapSize = record
    width: double;
    height: double;
  end;
type
  MKMapSizePtr = ^MKMapSize;
type
  MKMapRect = record
    origin: MKMapPoint;
    size: MKMapSize;
  end;
type
  MKMapRectPtr = ^MKMapRect;

type
  MKZoomScale = CGFloat;
  MKZoomScalePtr = ^MKZoomScale;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  MKMapSizeWorld: MKMapSize { available in 10_9, 4_0 }; cvar; external;
  MKMapRectWorld: MKMapRect { available in 10_9, 4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function MKMapPointForCoordinate(coordinate: CLLocationCoordinate2D): MKMapPoint; cdecl; external; { available in 10_9, 4_0 }
function MKCoordinateForMapPoint(mapPoint: MKMapPoint): CLLocationCoordinate2D; cdecl; external; { available in 10_9, 4_0 }
function MKMetersPerMapPointAtLatitude(latitude: CLLocationDegrees): CLLocationDistance; cdecl; external; { available in 10_9, 4_0 }
function MKMapPointsPerMeterAtLatitude(latitude: CLLocationDegrees): double; cdecl; external; { available in 10_9, 4_0 }
function MKMetersBetweenMapPoints(a: MKMapPoint; b: MKMapPoint): CLLocationDistance; cdecl; external; { available in 10_9, 4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  MKMapRectNull: MKMapRect { available in 10_9, 4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function MKMapRectUnion(rect1: MKMapRect; rect2: MKMapRect): MKMapRect; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectIntersection(rect1: MKMapRect; rect2: MKMapRect): MKMapRect; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectInset(rect: MKMapRect; dx: double; dy: double): MKMapRect; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectOffset(rect: MKMapRect; dx: double; dy: double): MKMapRect; cdecl; external; { available in 10_9, 4_0 }
procedure MKMapRectDivide(rect: MKMapRect; slice: MKMapRectPtr; remainder: MKMapRectPtr; amount: double; edge: CGRectEdge); cdecl; external; { available in 10_9, 4_0 }
function MKMapRectContainsPoint(rect: MKMapRect; point: MKMapPoint): ObjCBOOL; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectContainsRect(rect1: MKMapRect; rect2: MKMapRect): ObjCBOOL; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectIntersectsRect(rect1: MKMapRect; rect2: MKMapRect): ObjCBOOL; cdecl; external; { available in 10_9, 4_0 }
function MKCoordinateRegionForMapRect(rect: MKMapRect): MKCoordinateRegion; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectSpans180thMeridian(rect: MKMapRect): ObjCBOOL; cdecl; external; { available in 10_9, 4_0 }
function MKMapRectRemainder(rect: MKMapRect): MKMapRect; cdecl; external; { available in 10_9, 4_0 }
{$endif}

{$ifdef CLASSES}

type
  NSValueMapKitGeometryExtensions = objccategory external (NSValue)
    class function valueWithMKCoordinate (coordinate: CLLocationCoordinate2D): NSValue; message 'valueWithMKCoordinate:';
    class function valueWithMKCoordinateSpan (span: MKCoordinateSpan): NSValue; message 'valueWithMKCoordinateSpan:';
    function MKCoordinateValue: CLLocationCoordinate2D; message 'MKCoordinateValue';
    function MKCoordinateSpanValue: MKCoordinateSpan; message 'MKCoordinateSpanValue';
  end;
{$endif}

