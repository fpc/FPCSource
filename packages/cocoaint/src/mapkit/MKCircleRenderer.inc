{ Parsed from MapKit.framework MKCircleRenderer.h }


{$ifdef TYPES}
type
  MKCircleRendererPtr = ^MKCircleRenderer;
{$endif}

{$ifdef CLASSES}

type
  MKCircleRenderer = objcclass external (MKOverlayPathRenderer)
  public
    function initWithCircle (circle: MKCircle): instancetype; message 'initWithCircle:';
    function circle: MKCircle; message 'circle';
  end;
{$endif}

