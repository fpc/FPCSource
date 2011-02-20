{ Parsed from Appkit.framework NSRulerView.h }

{$ifdef TYPES}
{$ifndef NSRULERVIEW_PAS_T}
{$define NSRULERVIEW_PAS_T}

{ Constants }

const
  NSHorizontalRuler = 0;
  NSVerticalRuler = 1;

{ Types }
type
  NSRulerOrientation = NSUInteger;
  NSRulerOrientationPtr = ^NSRulerOrientation;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRULERVIEW_PAS_R}
{$define NSRULERVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRULERVIEW_PAS_F}
{$define NSRULERVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRULERVIEW_PAS_S}
{$define NSRULERVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRulerView = objcclass;
  NSRulerViewPointer = ^NSRulerView;
  NSRulerViewPtr = NSRulerViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRULERVIEW_PAS_C}
{$define NSRULERVIEW_PAS_C}

{ NSRulerView }
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
    class procedure registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle(unitName: NSString; abbreviation: NSString; conversionFactor: CGFloat; stepUpCycle: NSArray; stepDownCycle: NSArray); message 'registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:';
    function initWithScrollView_orientation(scrollView_: NSScrollView; orientation_: NSRulerOrientation): id; message 'initWithScrollView:orientation:';
    procedure setScrollView(scrollView_: NSScrollView); message 'setScrollView:';
    function scrollView: NSScrollView; message 'scrollView';
    procedure setOrientation(orientation_: NSRulerOrientation); message 'setOrientation:';
    function orientation: NSRulerOrientation; message 'orientation';
    function baselineLocation: CGFloat; message 'baselineLocation';
    function requiredThickness: CGFloat; message 'requiredThickness';
    procedure setRuleThickness(thickness: CGFloat); message 'setRuleThickness:';
    function ruleThickness: CGFloat; message 'ruleThickness';
    procedure setReservedThicknessForMarkers(thickness: CGFloat); message 'setReservedThicknessForMarkers:';
    function reservedThicknessForMarkers: CGFloat; message 'reservedThicknessForMarkers';
    procedure setReservedThicknessForAccessoryView(thickness: CGFloat); message 'setReservedThicknessForAccessoryView:';
    function reservedThicknessForAccessoryView: CGFloat; message 'reservedThicknessForAccessoryView';
    procedure setMeasurementUnits(unitName: NSString); message 'setMeasurementUnits:';
    function measurementUnits: NSString; message 'measurementUnits';
    procedure setOriginOffset(offset: CGFloat); message 'setOriginOffset:';
    function originOffset: CGFloat; message 'originOffset';
    procedure setClientView(client: NSView); message 'setClientView:';
    function clientView: NSView; message 'clientView';
    procedure setMarkers(markers_: NSArray); message 'setMarkers:';
    procedure addMarker(marker: NSRulerMarker); message 'addMarker:';
    procedure removeMarker(marker: NSRulerMarker); message 'removeMarker:';
    function markers: NSArray; message 'markers';
    function trackMarker_withMouseEvent(marker: NSRulerMarker; event: NSEvent): Boolean; message 'trackMarker:withMouseEvent:';
    procedure setAccessoryView(accessory: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure moveRulerlineFromLocation_toLocation(oldLocation: CGFloat; newLocation: CGFloat); message 'moveRulerlineFromLocation:toLocation:';
    procedure invalidateHashMarks; message 'invalidateHashMarks';
    procedure drawHashMarksAndLabelsInRect(rect: NSRect); message 'drawHashMarksAndLabelsInRect:';
    procedure drawMarkersInRect(rect: NSRect); message 'drawMarkersInRect:';
    function isFlipped: Boolean; message 'isFlipped';
  end;

{ NSRulerMarkerClientViewDelegationCategory }
  NSRulerMarkerClientViewDelegationCategory = objccategory external (NSView)
    function rulerView_shouldMoveMarker(ruler: NSRulerView; marker: NSRulerMarker): Boolean; message 'rulerView:shouldMoveMarker:';
    function rulerView_willMoveMarker_toLocation(ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willMoveMarker:toLocation:';
    procedure rulerView_didMoveMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didMoveMarker:';
    function rulerView_shouldRemoveMarker(ruler: NSRulerView; marker: NSRulerMarker): Boolean; message 'rulerView:shouldRemoveMarker:';
    procedure rulerView_didRemoveMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didRemoveMarker:';
    function rulerView_shouldAddMarker(ruler: NSRulerView; marker: NSRulerMarker): Boolean; message 'rulerView:shouldAddMarker:';
    function rulerView_willAddMarker_atLocation(ruler: NSRulerView; marker: NSRulerMarker; location: CGFloat): CGFloat; message 'rulerView:willAddMarker:atLocation:';
    procedure rulerView_didAddMarker(ruler: NSRulerView; marker: NSRulerMarker); message 'rulerView:didAddMarker:';
    procedure rulerView_handleMouseDown(ruler: NSRulerView; event: NSEvent); message 'rulerView:handleMouseDown:';
    procedure rulerView_willSetClientView(ruler: NSRulerView; newClient: NSView); message 'rulerView:willSetClientView:';
  end;

{$endif}
{$endif}
