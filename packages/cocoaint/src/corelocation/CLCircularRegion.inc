{ Parsed from CoreLocation.framework CLCircularRegion.h }


{$ifdef TYPES}
type
  CLCircularRegionPtr = ^CLCircularRegion;
{$endif}

{$ifdef CLASSES}

type
  CLCircularRegion = objcclass external (CLRegion)
  public
    function initWithCenter_radius_identifier (center_: CLLocationCoordinate2D; radius_: CLLocationDistance; identifier_: NSString): instancetype; message 'initWithCenter:radius:identifier:';
    function center: CLLocationCoordinate2D; message 'center';
    function radius: CLLocationDistance; message 'radius';
    function containsCoordinate (coordinate: CLLocationCoordinate2D): ObjCBOOL; message 'containsCoordinate:';
  end;
{$endif}

