{ Parsed from MapKit.framework MKMapView.h }


{ Types from MKMapView }
{$ifdef TYPES}





{$endif}


{ Types from OverlaysAPI }
{$ifdef TYPES}

{$endif}


{ Types from MKMapViewDelegateProtocol }
{$ifdef TYPES}



{$endif}


{$ifdef TYPES}
type
  MKMapViewPtr = ^MKMapView;
  MKMapViewDelegateProtocolPtr = ^MKMapViewDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  MKUserTrackingMode = NSInteger;
  MKUserTrackingModePtr = ^MKUserTrackingMode;

const
  MKUserTrackingModeNone = 0;
  MKUserTrackingModeFollow = 1;
  MKUserTrackingModeFollowWithHeading = 2;
{$endif}

{$ifdef CLASSES}

type
  MKMapView = objcclass external (NSView, NSCodingProtocol)
  public
    procedure setDelegate(newValue: MKMapViewDelegateProtocol); message 'setDelegate:';
    function delegate: MKMapViewDelegateProtocol; message 'delegate';
    procedure setMapType(newValue: MKMapType); message 'setMapType:';
    function mapType: MKMapType; message 'mapType';
    procedure setRegion(newValue: MKCoordinateRegion); message 'setRegion:';
    function region: MKCoordinateRegion; message 'region';
    procedure setRegion_animated (region_: MKCoordinateRegion; animated: ObjCBOOL); message 'setRegion:animated:';
    procedure setCenterCoordinate(newValue: CLLocationCoordinate2D); message 'setCenterCoordinate:';
    function centerCoordinate: CLLocationCoordinate2D; message 'centerCoordinate';
    procedure setCenterCoordinate_animated (coordinate: CLLocationCoordinate2D; animated: ObjCBOOL); message 'setCenterCoordinate:animated:';
    function regionThatFits (region_: MKCoordinateRegion): MKCoordinateRegion; message 'regionThatFits:';
    procedure setVisibleMapRect(newValue: MKMapRect); message 'setVisibleMapRect:';
    function visibleMapRect: MKMapRect; message 'visibleMapRect';
    procedure setVisibleMapRect_animated (mapRect: MKMapRect; animate: ObjCBOOL); message 'setVisibleMapRect:animated:';
    function mapRectThatFits (mapRect: MKMapRect): MKMapRect; message 'mapRectThatFits:';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setVisibleMapRect_edgePadding_animated (mapRect: MKMapRect; insets: UIEdgeInsets; animate: ObjCBOOL); message 'setVisibleMapRect:edgePadding:animated:';
    function mapRectThatFits_edgePadding (mapRect: MKMapRect; insets: UIEdgeInsets): MKMapRect; message 'mapRectThatFits:edgePadding:';
    {$else}
    procedure setVisibleMapRect_edgePadding_animated (mapRect: MKMapRect; insets: NSEdgeInsets; animate: ObjCBOOL); message 'setVisibleMapRect:edgePadding:animated:';
    function mapRectThatFits_edgePadding (mapRect: MKMapRect; insets: NSEdgeInsets): MKMapRect; message 'mapRectThatFits:edgePadding:';
    {$endif}
    procedure setCamera(newValue: MKMapCamera); message 'setCamera:';
    function camera: MKMapCamera; message 'camera';
    procedure setCamera_animated (camera_: MKMapCamera; animated: ObjCBOOL); message 'setCamera:animated:'; { available in 10_9, 7_0 }
    {$if defined(TARGET_OS_IPHONE)}
    function convertCoordinate_toPointToView (coordinate: CLLocationCoordinate2D; view: UIViewPtr): CGPoint; message 'convertCoordinate:toPointToView:';
    function convertPoint_toCoordinateFromView (point: CGPoint; view: UIViewPtr): CLLocationCoordinate2D; message 'convertPoint:toCoordinateFromView:';
    function convertRegion_toRectToView (region_: MKCoordinateRegion; view: UIViewPtr): CGRect; message 'convertRegion:toRectToView:';
    function convertRect_toRegionFromView (rect: CGRect; view: UIViewPtr): MKCoordinateRegion; message 'convertRect:toRegionFromView:';
    {$else}
    function convertCoordinate_toPointToView (coordinate: CLLocationCoordinate2D; view: NSView): CGPoint; message 'convertCoordinate:toPointToView:';
    function convertPoint_toCoordinateFromView (point: CGPoint; view: NSView): CLLocationCoordinate2D; message 'convertPoint:toCoordinateFromView:';
    function convertRegion_toRectToView (region_: MKCoordinateRegion; view: NSView): CGRect; message 'convertRegion:toRectToView:';
    function convertRect_toRegionFromView (rect: CGRect; view: NSView): MKCoordinateRegion; message 'convertRect:toRegionFromView:';
    {$endif}
    procedure setZoomEnabled(newValue: ObjCBOOL); message 'setZoomEnabled:';
    function isZoomEnabled: ObjCBOOL; message 'isZoomEnabled';
    procedure setScrollEnabled(newValue: ObjCBOOL); message 'setScrollEnabled:';
    function isScrollEnabled: ObjCBOOL; message 'isScrollEnabled';
    procedure setRotateEnabled(newValue: ObjCBOOL); message 'setRotateEnabled:';
    function isRotateEnabled: ObjCBOOL; message 'isRotateEnabled';
    procedure setPitchEnabled(newValue: ObjCBOOL); message 'setPitchEnabled:';
    function isPitchEnabled: ObjCBOOL; message 'isPitchEnabled';
    {$if  not defined(TARGET_OS_IPHONE)}
    procedure setShowsCompass(newValue: ObjCBOOL); message 'setShowsCompass:';
    function showsCompass: ObjCBOOL; message 'showsCompass';
    procedure setShowsZoomControls(newValue: ObjCBOOL); message 'setShowsZoomControls:';
    function showsZoomControls: ObjCBOOL; message 'showsZoomControls';
    procedure setShowsScale(newValue: ObjCBOOL); message 'setShowsScale:';
    function showsScale: ObjCBOOL; message 'showsScale';
    {$endif}
    procedure setShowsPointsOfInterest(newValue: ObjCBOOL); message 'setShowsPointsOfInterest:';
    function showsPointsOfInterest: ObjCBOOL; message 'showsPointsOfInterest';
    procedure setShowsBuildings(newValue: ObjCBOOL); message 'setShowsBuildings:';
    function showsBuildings: ObjCBOOL; message 'showsBuildings';
    procedure setShowsUserLocation(newValue: ObjCBOOL); message 'setShowsUserLocation:';
    function showsUserLocation: ObjCBOOL; message 'showsUserLocation';
    function userLocation: MKUserLocation; message 'userLocation';
    {$if defined(TARGET_OS_IPHONE)}
    procedure setUserTrackingMode(newValue: MKUserTrackingMode); message 'setUserTrackingMode:';
    function userTrackingMode: MKUserTrackingMode; message 'userTrackingMode';
    procedure setUserTrackingMode_animated (mode: MKUserTrackingMode; animated: ObjCBOOL); message 'setUserTrackingMode:animated:'; { available in NA, 5_0 }
    {$endif}
    function isUserLocationVisible: ObjCBOOL; message 'isUserLocationVisible';
    procedure addAnnotation (annotation: MKAnnotationProtocol); message 'addAnnotation:';
    procedure addAnnotations (annotations: NSArray); message 'addAnnotations:';
    procedure removeAnnotation (annotation: MKAnnotationProtocol); message 'removeAnnotation:';
    procedure removeAnnotations (annotations: NSArray); message 'removeAnnotations:';
    function annotations: NSArray; message 'annotations';
    function annotationsInMapRect (mapRect: MKMapRect): NSSet; message 'annotationsInMapRect:'; { available in 10_9, 4_2 }
    function viewForAnnotation (annotation: MKAnnotationProtocol): MKAnnotationView; message 'viewForAnnotation:';
    function dequeueReusableAnnotationViewWithIdentifier (identifier: NSString): MKAnnotationView; message 'dequeueReusableAnnotationViewWithIdentifier:';
    procedure selectAnnotation_animated (annotation: MKAnnotationProtocol; animated: ObjCBOOL); message 'selectAnnotation:animated:';
    procedure deselectAnnotation_animated (annotation: MKAnnotationProtocol; animated: ObjCBOOL); message 'deselectAnnotation:animated:';
    procedure setSelectedAnnotations(newValue: NSArray); message 'setSelectedAnnotations:';
    function selectedAnnotations: NSArray; message 'selectedAnnotations';
    function annotationVisibleRect: CGRect; message 'annotationVisibleRect';
    procedure showAnnotations_animated (annotations_: NSArray; animated: ObjCBOOL); message 'showAnnotations:animated:'; { available in 10_9, 7_0 }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef TYPES}
type
  MKOverlayLevel = NSInteger;
  MKOverlayLevelPtr = ^MKOverlayLevel;

const
  MKOverlayLevelAboveRoads = 0;
  MKOverlayLevelAboveLabels = 1;
{$endif}

{$ifdef CLASSES}

type
  OverlaysAPI = objccategory external (MKMapView)
    procedure addOverlay_level (overlay: MKOverlayProtocol; level: MKOverlayLevel); message 'addOverlay:level:'; { available in 10_9, 7_0 }
    procedure addOverlays_level (overlays: NSArray; level: MKOverlayLevel); message 'addOverlays:level:'; { available in 10_9, 7_0 }
    procedure removeOverlay (overlay: MKOverlayProtocol); message 'removeOverlay:'; { available in 10_9, 4_0 }
    procedure removeOverlays (overlays: NSArray); message 'removeOverlays:'; { available in 10_9, 4_0 }
    procedure insertOverlay_atIndex_level (overlay: MKOverlayProtocol; index: NSUInteger; level: MKOverlayLevel); message 'insertOverlay:atIndex:level:'; { available in 10_9, 7_0 }
    procedure insertOverlay_aboveOverlay (overlay: MKOverlayProtocol; sibling: MKOverlayProtocol); message 'insertOverlay:aboveOverlay:'; { available in 10_9, 4_0 }
    procedure insertOverlay_belowOverlay (overlay: MKOverlayProtocol; sibling: MKOverlayProtocol); message 'insertOverlay:belowOverlay:'; { available in 10_9, 4_0 }
    procedure exchangeOverlay_withOverlay (overlay1: MKOverlayProtocol; overlay2: MKOverlayProtocol); message 'exchangeOverlay:withOverlay:'; { available in 10_9, 7_0 }
    function overlays: NSArray; message 'overlays';
    function overlaysInLevel (level: MKOverlayLevel): NSArray; message 'overlaysInLevel:'; { available in 10_9, 7_0 }
    function rendererForOverlay (overlay: MKOverlayProtocol): MKOverlayRenderer; message 'rendererForOverlay:'; { available in 10_9, 7_0 }
    {$if defined(TARGET_OS_IPHONE)}
    function viewForOverlay (overlay: MKOverlayProtocol): MKOverlayViewPtr; message 'viewForOverlay:'; deprecated 'in 4_0, 7_0';
    {$endif}
    procedure addOverlay (overlay: MKOverlayProtocol); message 'addOverlay:'; { available in 10_9, 4_0 }
    procedure addOverlays (overlays_: NSArray); message 'addOverlays:'; { available in 10_9, 4_0 }
    procedure insertOverlay_atIndex (overlay: MKOverlayProtocol; index: NSUInteger); message 'insertOverlay:atIndex:'; { available in 10_9, 4_0 }
    procedure exchangeOverlayAtIndex_withOverlayAtIndex (index1: NSUInteger; index2: NSUInteger); message 'exchangeOverlayAtIndex:withOverlayAtIndex:'; { available in 10_9, 4_0 }
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  MKMapViewDelegateProtocol = objcprotocol external name 'MKMapViewDelegate' (NSObjectProtocol)
  optional
    procedure mapView_regionWillChangeAnimated (mapView: MKMapView; animated: ObjCBOOL); message 'mapView:regionWillChangeAnimated:';
    procedure mapView_regionDidChangeAnimated (mapView: MKMapView; animated: ObjCBOOL); message 'mapView:regionDidChangeAnimated:';
    procedure mapViewWillStartLoadingMap (mapView: MKMapView); message 'mapViewWillStartLoadingMap:';
    procedure mapViewDidFinishLoadingMap (mapView: MKMapView); message 'mapViewDidFinishLoadingMap:';
    procedure mapViewDidFailLoadingMap_withError (mapView: MKMapView; error: NSError); message 'mapViewDidFailLoadingMap:withError:';
    procedure mapViewWillStartRenderingMap (mapView: MKMapView); message 'mapViewWillStartRenderingMap:'; { available in 10_9, 7_0 }
    procedure mapViewDidFinishRenderingMap_fullyRendered (mapView: MKMapView; fullyRendered: ObjCBOOL); message 'mapViewDidFinishRenderingMap:fullyRendered:'; { available in 10_9, 7_0 }
    function mapView_viewForAnnotation (mapView: MKMapView; annotation: MKAnnotationProtocol): MKAnnotationView; message 'mapView:viewForAnnotation:';
    procedure mapView_didAddAnnotationViews (mapView: MKMapView; views: NSArray); message 'mapView:didAddAnnotationViews:';
    {$if defined(TARGET_OS_IPHONE)}
    procedure mapView_annotationView_calloutAccessoryControlTapped (mapView: MKMapView; view: MKAnnotationView; control: UIControlPtr); message 'mapView:annotationView:calloutAccessoryControlTapped:';
    {$endif}
    procedure mapView_didSelectAnnotationView (mapView: MKMapView; view: MKAnnotationView); message 'mapView:didSelectAnnotationView:'; { available in 10_9, 4_0 }
    procedure mapView_didDeselectAnnotationView (mapView: MKMapView; view: MKAnnotationView); message 'mapView:didDeselectAnnotationView:'; { available in 10_9, 4_0 }
    procedure mapViewWillStartLocatingUser (mapView: MKMapView); message 'mapViewWillStartLocatingUser:'; { available in 10_9, 4_0 }
    procedure mapViewDidStopLocatingUser (mapView: MKMapView); message 'mapViewDidStopLocatingUser:'; { available in 10_9, 4_0 }
    procedure mapView_didUpdateUserLocation (mapView: MKMapView; userLocation: MKUserLocation); message 'mapView:didUpdateUserLocation:'; { available in 10_9, 4_0 }
    procedure mapView_didFailToLocateUserWithError (mapView: MKMapView; error: NSError); message 'mapView:didFailToLocateUserWithError:'; { available in 10_9, 4_0 }
    procedure mapView_annotationView_didChangeDragState_fromOldState (mapView: MKMapView; view: MKAnnotationView; newState: MKAnnotationViewDragState; oldState: MKAnnotationViewDragState); message 'mapView:annotationView:didChangeDragState:fromOldState:'; { available in 10_9, 4_0 }
    {$if defined(TARGET_OS_IPHONE)}
    procedure mapView_didChangeUserTrackingMode_animated (mapView: MKMapView; mode: MKUserTrackingMode; animated: ObjCBOOL); message 'mapView:didChangeUserTrackingMode:animated:'; { available in NA, 5_0 }
    {$endif}
    function mapView_rendererForOverlay (mapView: MKMapView; overlay: MKOverlayProtocol): MKOverlayRenderer; message 'mapView:rendererForOverlay:'; { available in 10_9, 7_0 }
    procedure mapView_didAddOverlayRenderers (mapView: MKMapView; renderers: NSArray); message 'mapView:didAddOverlayRenderers:'; { available in 10_9, 7_0 }
    {$if defined(TARGET_OS_IPHONE)}
    function mapView_viewForOverlay (mapView: MKMapView; overlay: MKOverlayProtocol): MKOverlayViewPtr; message 'mapView:viewForOverlay:'; deprecated 'in 4_0, 7_0';
    procedure mapView_didAddOverlayViews (mapView: MKMapView; overlayViews: NSArray); message 'mapView:didAddOverlayViews:'; deprecated 'in 4_0, 7_0';
    {$endif}
  end;
{$endif}

