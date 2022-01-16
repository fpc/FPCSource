{ Parsed from MapKit.framework MKTileOverlayRenderer.h }


{$ifdef TYPES}
type
  MKTileOverlayRendererPtr = ^MKTileOverlayRenderer;
{$endif}

{$ifdef CLASSES}

type
  MKTileOverlayRenderer = objcclass external (MKOverlayRenderer)
  public
    function initWithTileOverlay (overlay_: MKTileOverlay): instancetype; message 'initWithTileOverlay:';
    procedure reloadData; message 'reloadData';
  end;
{$endif}

