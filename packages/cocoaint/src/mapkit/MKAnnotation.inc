{ Parsed from MapKit.framework MKAnnotation.h }


{$ifdef TYPES}
type
  MKAnnotationProtocolPtr = ^MKAnnotationProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  MKAnnotationProtocol = objcprotocol external name 'MKAnnotation' (NSObjectProtocol)
    function coordinate: CLLocationCoordinate2D; message 'coordinate';
  optional
    function title: NSString; message 'title';
    function subtitle: NSString; message 'subtitle';
    procedure setCoordinate (newCoordinate: CLLocationCoordinate2D); message 'setCoordinate:'; { available in 10_9, 4_0 }
  end;
{$endif}

