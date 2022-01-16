{ Parsed from MapKit.framework MKPinAnnotationView.h }


{$ifdef TYPES}
type
  MKPinAnnotationViewPtr = ^MKPinAnnotationView;
{$endif}

{$ifdef TYPES}
type
  MKPinAnnotationColor = NSUInteger;
  MKPinAnnotationColorPtr = ^MKPinAnnotationColor;

const
  MKPinAnnotationColorRed = 0;
  MKPinAnnotationColorGreen = 1;
  MKPinAnnotationColorPurple = 2;
{$endif}

{$ifdef CLASSES}

type
  MKPinAnnotationView = objcclass external (MKAnnotationView)
  public
    procedure setPinColor(newValue: MKPinAnnotationColor); message 'setPinColor:';
    function pinColor: MKPinAnnotationColor; message 'pinColor';
    procedure setAnimatesDrop(newValue: ObjCBOOL); message 'setAnimatesDrop:';
    function animatesDrop: ObjCBOOL; message 'animatesDrop';
  end;
{$endif}

