{ Parsed from MapKit.framework MKPolylineRenderer.h }


{$ifdef TYPES}
type
  MKPolylineRendererPtr = ^MKPolylineRenderer;
{$endif}

{$ifdef CLASSES}

type
  MKPolylineRenderer = objcclass external (MKOverlayPathRenderer)
  public
    function initWithPolyline (polyline: MKPolyline): instancetype; message 'initWithPolyline:';
    function polyline: MKPolyline; message 'polyline';
  end;
{$endif}

