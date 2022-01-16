{ Parsed from MapKit.framework MKOverlayRenderer.h }


{$ifdef TYPES}
type
  MKOverlayRendererPtr = ^MKOverlayRenderer;
{$endif}

{$ifdef CLASSES}

type
  MKOverlayRenderer = objcclass external (NSObject)
  public
    function initWithOverlay (overlay: MKOverlayProtocol): instancetype; message 'initWithOverlay:'; { NS_DESIGNATED_INITIALIZER }
    function overlay: MKOverlayProtocol; message 'overlay';
    function pointForMapPoint (mapPoint: MKMapPoint): CGPoint; message 'pointForMapPoint:';
    function mapPointForPoint (point: CGPoint): MKMapPoint; message 'mapPointForPoint:';
    function rectForMapRect (mapRect: MKMapRect): CGRect; message 'rectForMapRect:';
    function mapRectForRect (rect: CGRect): MKMapRect; message 'mapRectForRect:';
    function canDrawMapRect_zoomScale (mapRect: MKMapRect; zoomScale: MKZoomScale): ObjCBOOL; message 'canDrawMapRect:zoomScale:';
    procedure drawMapRect_zoomScale_inContext (mapRect: MKMapRect; zoomScale: MKZoomScale; context: CGContextRef); message 'drawMapRect:zoomScale:inContext:';
    procedure setNeedsDisplay; message 'setNeedsDisplay';
    procedure setNeedsDisplayInMapRect (mapRect: MKMapRect); message 'setNeedsDisplayInMapRect:';
    procedure setNeedsDisplayInMapRect_zoomScale (mapRect: MKMapRect; zoomScale: MKZoomScale); message 'setNeedsDisplayInMapRect:zoomScale:';
    procedure setAlpha(newValue: CGFloat); message 'setAlpha:';
    function alpha: CGFloat; message 'alpha';
    function contentScaleFactor: CGFloat; message 'contentScaleFactor';
  end;
{$endif}

{$ifdef FUNCTIONS}
function MKRoadWidthAtZoomScale(zoomScale: MKZoomScale): CGFloat; cdecl; external; { available in 10_9, 4_0 }
{$endif}

