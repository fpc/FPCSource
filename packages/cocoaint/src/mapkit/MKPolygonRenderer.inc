{ Parsed from MapKit.framework MKPolygonRenderer.h }


{$ifdef TYPES}
type
  MKPolygonRendererPtr = ^MKPolygonRenderer;
{$endif}

{$ifdef CLASSES}

type
  MKPolygonRenderer = objcclass external (MKOverlayPathRenderer)
  public
    function initWithPolygon (polygon: MKPolygon): instancetype; message 'initWithPolygon:';
    function polygon: MKPolygon; message 'polygon';
  end;
{$endif}

