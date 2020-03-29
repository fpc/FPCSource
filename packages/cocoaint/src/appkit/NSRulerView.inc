{ Parsed from AppKit.framework NSRulerView.h }


{$ifdef TYPES}
type
  NSRulerViewPtr = ^NSRulerView;
{$endif}

{$ifdef TYPES}
type
  NSRulerOrientation = NSUInteger;
  NSRulerOrientationPtr = ^NSRulerOrientation;

const
  NSHorizontalRuler = 0;
  NSVerticalRuler = 1;
{$endif}

{$ifdef CLASSES}

type
  NSRulerView = objcclass external (NSView)
  private
    _scrollView: NSScrollView;
    _orientation: NSRulerOrientation;
    _units: NSString;
    _originOffset: CGFloat;
    _ruleThickness: CGFloat;
    _thicknessForMarkers: CGFloat;
    _thicknessForAccessoryView: CGFloat;
    _clientView: NSView;
    _markers: NSMutableArray;
    _accessoryView: NSView;
    _cachedHashDict: NSDictionary;
    _cachedDocViewToRulerConversion: CGFloat;
    _cachedContentBoundsOrigin: NSPoint;
    _draggingMarker: NSRulerMarker;
    _reservedRulerView1: id;
  public
    class procedure registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle (unitName: NSString; abbreviation: NSString; conversionFactor: CGFloat; stepUpCycle: NSArray; stepDownCycle: NSArray); message 'registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:';
    function initWithScrollView_orientation (scrollView: NSScrollView; orientation: NSRulerOrientation): instancetype; message 'initWithScrollView:orientation:';
    procedure setScrollView(newValue: NSScrollView); message 'setScrollView:';
    function scrollView: NSScrollView; message 'scrollView';
    procedure setOrientation(newValue: NSRulerOrientation); message 'setOrientation:';
    function orientation: NSRulerOrientation; message 'orientation';
    function baselineLocation: CGFloat; message 'baselineLocation';
    function requiredThickness: CGFloat; message 'requiredThickness';
    procedure setRuleThickness(newValue: CGFloat); message 'setRuleThickness:';
    function ruleThickness: CGFloat; message 'ruleThickness';
    procedure setReservedThicknessForMarkers(newValue: CGFloat); message 'setReservedThicknessForMarkers:';
    function reservedThicknessForMarkers: CGFloat; message 'reservedThicknessForMarkers';
    procedure setReservedThicknessForAccessoryView(newValue: CGFloat); message 'setReservedThicknessForAccessoryView:';
    function reservedThicknessForAccessoryView: CGFloat; message 'reservedThicknessForAccessoryView';
    procedure setMeasurementUnits(newValue: NSString); message 'setMeasurementUnits:';
    function measurementUnits: NSString; message 'measurementUnits';
    procedure setOriginOffset(newValue: CGFloat); message 'setOriginOffset:';
    function originOffset: CGFloat; message 'originOffset';
    procedure setClientView(newValue: NSView); message 'setClientView:';
    function clientView: NSView; message 'clientView';
    procedure addMarker (marker: NSRulerMarker); message 'addMarker:';
    procedure removeMarker (marker: NSRulerMarker); message 'removeMarker:';
    procedure setMarkers(newValue: NSArray); message 'setMarkers:';
    function markers: NSArray; message 'markers';
    function trackMarker_withMouseEvent (marker: NSRulerMarker; event: NSEvent): ObjCBOOL; message 'trackMarker:withMouseEvent:';
    procedure setAccessoryView(newValue: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure moveRulerlineFromLocation_toLocation (oldLocation: CGFloat; newLocation: CGFloat); message 'moveRulerlineFromLocation:toLocation:';
    procedure invalidateHashMarks; message 'invalidateHashMarks';
    procedure drawHashMarksAndLabelsInRect (rect: NSRect); message 'drawHashMarksAndLabelsInRect:';
    procedure drawMarkersInRect (rect: NSRect); message 'drawMarkersInRect:';
    function isFlipped: ObjCBOOL; message 'isFlipped';
  end;


type
  NSRulerMarkerClientViewDelegation = objccategory external (NSView)
    function rulerView_shouldMoveMarker (ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldMoveMarker:';
    function rulerView_willMoveMarker_toLocation (ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willMoveMarker:toLocation:';
    procedure rulerView_didMoveMarker (ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didMoveMarker:';
    function rulerView_shouldRemoveMarker (ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldRemoveMarker:';
    procedure rulerView_didRemoveMarker (ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didRemoveMarker:';
    function rulerView_shouldAddMarker (ruler: NSRulerView; marker: NSRulerMarker): ObjCBOOL; message 'rulerView:shouldAddMarker:';
    function rulerView_willAddMarker_atLocation (ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willAddMarker:atLocation:';
    procedure rulerView_didAddMarker (ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didAddMarker:';
    procedure rulerView_handleMouseDown (ruler: NSRulerView; event: NSEvent); message 'rulerView:handleMouseDown:';
    procedure rulerView_willSetClientView (ruler: NSRulerView; newClient: NSView); message 'rulerView:willSetClientView:';
    function rulerView_locationForPoint (ruler: NSRulerView; aPoint: NSPoint): CGFloat; message 'rulerView:locationForPoint:'; { available in 10_7 }
    function rulerView_pointForLocation (ruler: NSRulerView; aPoint: CGFloat): NSPoint; message 'rulerView:pointForLocation:'; { available in 10_7 }
  end;
{$endif}

