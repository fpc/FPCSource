{ Parsed from MapKit.framework MKPolygon.h }


{$ifdef TYPES}
type
  MKPolygonPtr = ^MKPolygon;
{$endif}

{$ifdef CLASSES}

type
  MKPolygon = objcclass external (MKMultiPoint, MKOverlayProtocol)
  public
    class function polygonWithPoints_count (points_: MKMapPointPtr; count: NSUInteger): instancetype; message 'polygonWithPoints:count:';
    class function polygonWithPoints_count_interiorPolygons (points_: MKMapPointPtr; count: NSUInteger; interiorPolygons: NSArray): instancetype; message 'polygonWithPoints:count:interiorPolygons:';
    class function polygonWithCoordinates_count (coords: CLLocationCoordinate2DPtr; count: NSUInteger): instancetype; message 'polygonWithCoordinates:count:';
    class function polygonWithCoordinates_count_interiorPolygons (coords: CLLocationCoordinate2DPtr; count: NSUInteger; interiorPolygons: NSArray): instancetype; message 'polygonWithCoordinates:count:interiorPolygons:';
    function interiorPolygons: NSArray; message 'interiorPolygons';

    { Adopted protocols }
    function boundingMapRect: MKMapRect; message 'boundingMapRect';
    function canReplaceMapContent: ObjCBOOL; message 'canReplaceMapContent'; { available in 10_9, 7_0 }
    function intersectsMapRect (mapRect: MKMapRect): ObjCBOOL; message 'intersectsMapRect:';
  end;
{$endif}

