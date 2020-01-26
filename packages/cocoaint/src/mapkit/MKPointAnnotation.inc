{ Parsed from MapKit.framework MKPointAnnotation.h }


{$ifdef TYPES}
type
  MKPointAnnotationPtr = ^MKPointAnnotation;
{$endif}

{$ifdef CLASSES}

type
  MKPointAnnotation = objcclass external (MKShape)
  public
    procedure setCoordinate(newValue: CLLocationCoordinate2D); message 'setCoordinate:';
    function coordinate: CLLocationCoordinate2D; message 'coordinate';
  end;
{$endif}

