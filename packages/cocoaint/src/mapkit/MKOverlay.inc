{ Parsed from MapKit.framework MKOverlay.h }


{$ifdef TYPES}
type
  MKOverlayProtocolPtr = ^MKOverlayProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  MKOverlayProtocol = objcprotocol external name 'MKOverlay' (MKAnnotationProtocol)
  required
    function coordinate: CLLocationCoordinate2D; message 'coordinate';
    function boundingMapRect: MKMapRect; message 'boundingMapRect';
  optional
    function intersectsMapRect (mapRect: MKMapRect): ObjCBOOL; message 'intersectsMapRect:';
    function canReplaceMapContent: ObjCBOOL; message 'canReplaceMapContent'; { available in 10_9, 7_0 }
  end;
{$endif}

