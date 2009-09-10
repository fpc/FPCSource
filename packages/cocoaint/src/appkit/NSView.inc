{ Parsed from Appkit.framework NSView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSVIEW_PAS_H}
{$define NSVIEW_PAS_H}
type
  NSViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSVIEW_PAS_T}
{$define NSVIEW_PAS_T}

{ Callbacks }
type
  NSViewCompare = function (param1: id; param2: id; param3: Pointer): NSComparisonResult; cdecl;

{ Constants }

const
  NSViewNotSizable = 0;
  NSViewMinXMargin = 1;
  NSViewWidthSizable = 2;
  NSViewMaxXMargin = 4;
  NSViewMinYMargin = 8;
  NSViewHeightSizable = 16;
  NSViewMaxYMargin = 32;

const
  NSNoBorder = 0;
  NSLineBorder = 1;
  NSBezelBorder = 2;
  NSGrooveBorder = 3;

{ Types }
type
  NSBorderType = culong;
  NSTrackingRectTag = clong;
  NSToolTipTag = clong;

{ CFString constants }
var
  NSFullScreenModeAllScreens: CFStringRef; external name '_NSFullScreenModeAllScreens';
  NSFullScreenModeSetting: CFStringRef; external name '_NSFullScreenModeSetting';
  NSFullScreenModeWindowLevel: CFStringRef; external name '_NSFullScreenModeWindowLevel';
  NSViewFrameDidChangeNotification: CFStringRef; external name '_NSViewFrameDidChangeNotification';
  NSViewFocusDidChangeNotification: CFStringRef; external name '_NSViewFocusDidChangeNotification';
  NSViewBoundsDidChangeNotification: CFStringRef; external name '_NSViewBoundsDidChangeNotification';
  NSViewGlobalFrameDidChangeNotification: CFStringRef; external name '_NSViewGlobalFrameDidChangeNotification';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSVIEW_PAS_R}
{$define NSVIEW_PAS_R}

{ Records }
type
  __VFlags = record
{$ifdef fpc_big_endian}
    rotatedFromBase: cuint;
    rotatedOrScaledFromBase: cuint;
    autosizing: cuint;
    autoresizeSubviews: cuint;
    wantsGState: cuint;
    needsDisplay: cuint;
    validGState: cuint;
    newGState: cuint;
    noVerticalAutosizing: cuint;
    frameChangeNotesSuspended: cuint;
    needsFrameChangeNote: cuint;
    focusChangeNotesSuspended: cuint;
    boundsChangeNotesSuspended: cuint;
    needsBoundsChangeNote: cuint;
    removingWithoutInvalidation: cuint;
    interfaceStyle0: cuint;
    needsDisplayForBounds: cuint;
    specialArchiving: cuint;
    interfaceStyle1: cuint;
    retainCount: cuint;
    retainCountOverMax: cuint;
    aboutToResize: cuint;
{$else}
    aboutToResize: cuint;
    retainCountOverMax: cuint;
    retainCount: cuint;
    interfaceStyle1: cuint;
    specialArchiving: cuint;
    needsDisplayForBounds: cuint;
    interfaceStyle0: cuint;
    removingWithoutInvalidation: cuint;
    needsBoundsChangeNote: cuint;
    boundsChangeNotesSuspended: cuint;
    focusChangeNotesSuspended: cuint;
    needsFrameChangeNote: cuint;
    frameChangeNotesSuspended: cuint;
    noVerticalAutosizing: cuint;
    newGState: cuint;
    validGState: cuint;
    needsDisplay: cuint;
    wantsGState: cuint;
    autoresizeSubviews: cuint;
    autosizing: cuint;
    rotatedOrScaledFromBase: cuint;
    rotatedFromBase: cuint;
{$endif}
  end;
_VFlags = __VFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSVIEW_PAS_F}
{$define NSVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSVIEW_PAS_T}
{$define NSVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSVIEW_PAS_C}
{$define NSVIEW_PAS_C}

{ NSView }
  NSView = objcclass(NSResponder)
  private
    __frame: NSRect;
    __bounds: NSRect;
    __superview: id;
    __subviews: id;
    __window: NSWindow;
    __gState: id;
    __frameMatrix: id;
    __drawMatrix: id;
    __dragTypes: id;
    __viewAuxiliary: _NSViewAuxiliary;
    __vFlags: _VFlags;
    __vFlags2: bitpacked record
        nextKeyViewRefCount: 0..((1 shl 14)-1);
        previousKeyViewRefCount: 0..((1 shl 14)-1);
        isVisibleRect: 0..1;
        hasToolTip: 0..1;
        needsRealLockFocus: 0..1;
        menuWasSet: 0..1;
      end;
    
  public
    class function alloc: NSView; message 'alloc';

    function initWithFrame(frameRect: NSRect): id; message 'initWithFrame:';
    function window: NSWindow; message 'window';
    function superview: NSView; message 'superview';
    function subviews: NSArray; message 'subviews';
    function isDescendantOf(aView: NSView): Boolean; message 'isDescendantOf:';
    function ancestorSharedWithView(aView: NSView): NSView; message 'ancestorSharedWithView:';
    function opaqueAncestor: NSView; message 'opaqueAncestor';
    procedure setHidden(flag: Boolean); message 'setHidden:';
    function isHidden: Boolean; message 'isHidden';
    function isHiddenOrHasHiddenAncestor: Boolean; message 'isHiddenOrHasHiddenAncestor';
    procedure getRectsBeingDrawn_count(var rects: NSRect; var count: clong); message 'getRectsBeingDrawn:count:';
    function needsToDrawRect(aRect: NSRect): Boolean; message 'needsToDrawRect:';
    function wantsDefaultClipping: Boolean; message 'wantsDefaultClipping';
    procedure viewDidHide; message 'viewDidHide';
    procedure viewDidUnhide; message 'viewDidUnhide';
    procedure setSubviews(newSubviews: NSArray); message 'setSubviews:';
    procedure addSubview(aView: NSView); message 'addSubview:';
    procedure addSubview_positioned_relativeTo(aView: NSView; place: NSWindowOrderingMode; otherView: NSView); message 'addSubview:positioned:relativeTo:';
    procedure sortSubviewsUsingFunction_context(compare: NSViewCompare; context: Pointer); message 'sortSubviewsUsingFunction:context:';
    procedure viewWillMoveToWindow(newWindow: NSWindow); message 'viewWillMoveToWindow:';
    procedure viewDidMoveToWindow; message 'viewDidMoveToWindow';
    procedure viewWillMoveToSuperview(newSuperview: NSView); message 'viewWillMoveToSuperview:';
    procedure viewDidMoveToSuperview; message 'viewDidMoveToSuperview';
    procedure didAddSubview(subview: NSView); message 'didAddSubview:';
    procedure willRemoveSubview(subview: NSView); message 'willRemoveSubview:';
    procedure removeFromSuperview; message 'removeFromSuperview';
    procedure replaceSubview_with(oldView: NSView; newView: NSView); message 'replaceSubview:with:';
    procedure removeFromSuperviewWithoutNeedingDisplay; message 'removeFromSuperviewWithoutNeedingDisplay';
    procedure setPostsFrameChangedNotifications(flag: Boolean); message 'setPostsFrameChangedNotifications:';
    function postsFrameChangedNotifications: Boolean; message 'postsFrameChangedNotifications';
    procedure resizeSubviewsWithOldSize(oldSize: NSSize); message 'resizeSubviewsWithOldSize:';
    procedure resizeWithOldSuperviewSize(oldSize: NSSize); message 'resizeWithOldSuperviewSize:';
    procedure setAutoresizesSubviews(flag: Boolean); message 'setAutoresizesSubviews:';
    function autoresizesSubviews: Boolean; message 'autoresizesSubviews';
    procedure setAutoresizingMask(mask: culong); message 'setAutoresizingMask:';
    function autoresizingMask: culong; message 'autoresizingMask';
    procedure setFrameOrigin(newOrigin: NSPoint); message 'setFrameOrigin:';
    procedure setFrameSize(newSize: NSSize); message 'setFrameSize:';
    procedure setFrame(frameRect: NSRect); message 'setFrame:';
    function frame: NSRect; message 'frame';
    procedure setFrameRotation(angle: CGFloat); message 'setFrameRotation:';
    function frameRotation: CGFloat; message 'frameRotation';
    procedure setFrameCenterRotation(angle: CGFloat); message 'setFrameCenterRotation:';
    function frameCenterRotation: CGFloat; message 'frameCenterRotation';
    procedure setBoundsOrigin(newOrigin: NSPoint); message 'setBoundsOrigin:';
    procedure setBoundsSize(newSize: NSSize); message 'setBoundsSize:';
    procedure setBoundsRotation(angle: CGFloat); message 'setBoundsRotation:';
    function boundsRotation: CGFloat; message 'boundsRotation';
    procedure translateOriginToPoint(translation: NSPoint); message 'translateOriginToPoint:';
    procedure scaleUnitSquareToSize(newUnitSize: NSSize); message 'scaleUnitSquareToSize:';
    procedure rotateByAngle(angle: CGFloat); message 'rotateByAngle:';
    procedure setBounds(aRect: NSRect); message 'setBounds:';
    function bounds: NSRect; message 'bounds';
    function isFlipped: Boolean; message 'isFlipped';
    function isRotatedFromBase: Boolean; message 'isRotatedFromBase';
    function isRotatedOrScaledFromBase: Boolean; message 'isRotatedOrScaledFromBase';
    function isOpaque: Boolean; message 'isOpaque';
    function convertPoint_fromView(aPoint: NSPoint; aView: NSView): NSPoint; message 'convertPoint:fromView:';
    function convertPoint_toView(aPoint: NSPoint; aView: NSView): NSPoint; message 'convertPoint:toView:';
    function convertSize_fromView(aSize: NSSize; aView: NSView): NSSize; message 'convertSize:fromView:';
    function convertSize_toView(aSize: NSSize; aView: NSView): NSSize; message 'convertSize:toView:';
    function convertRect_fromView(aRect: NSRect; aView: NSView): NSRect; message 'convertRect:fromView:';
    function convertRect_toView(aRect: NSRect; aView: NSView): NSRect; message 'convertRect:toView:';
    function centerScanRect(aRect: NSRect): NSRect; message 'centerScanRect:';
    function convertPointToBase(aPoint: NSPoint): NSPoint; message 'convertPointToBase:';
    function convertPointFromBase(aPoint: NSPoint): NSPoint; message 'convertPointFromBase:';
    function convertSizeToBase(aSize: NSSize): NSSize; message 'convertSizeToBase:';
    function convertSizeFromBase(aSize: NSSize): NSSize; message 'convertSizeFromBase:';
    function convertRectToBase(aRect: NSRect): NSRect; message 'convertRectToBase:';
    function convertRectFromBase(aRect: NSRect): NSRect; message 'convertRectFromBase:';
    function canDraw: Boolean; message 'canDraw';
    procedure setNeedsDisplay_(flag: Boolean); message 'setNeedsDisplay:';
    procedure setNeedsDisplayInRect(invalidRect: NSRect); message 'setNeedsDisplayInRect:';
    function needsDisplay: Boolean; message 'needsDisplay';
    procedure lockFocus; message 'lockFocus';
    procedure unlockFocus; message 'unlockFocus';
    function lockFocusIfCanDraw: Boolean; message 'lockFocusIfCanDraw';
    function lockFocusIfCanDrawInContext(context: NSGraphicsContext): Boolean; message 'lockFocusIfCanDrawInContext:';
    class function focusView: NSView; message 'focusView';
    function visibleRect: NSRect; message 'visibleRect';
    procedure display; message 'display';
    procedure displayIfNeeded; message 'displayIfNeeded';
    procedure displayIfNeededIgnoringOpacity; message 'displayIfNeededIgnoringOpacity';
    procedure displayRect(rect: NSRect); message 'displayRect:';
    procedure displayIfNeededInRect(rect: NSRect); message 'displayIfNeededInRect:';
    procedure displayRectIgnoringOpacity(rect: NSRect); message 'displayRectIgnoringOpacity:';
    procedure displayIfNeededInRectIgnoringOpacity(rect: NSRect); message 'displayIfNeededInRectIgnoringOpacity:';
    procedure drawRect(rect: NSRect); message 'drawRect:';
    procedure displayRectIgnoringOpacity_inContext(aRect: NSRect; context: NSGraphicsContext); message 'displayRectIgnoringOpacity:inContext:';
    function bitmapImageRepForCachingDisplayInRect(rect: NSRect): NSBitmapImageRep; message 'bitmapImageRepForCachingDisplayInRect:';
    procedure cacheDisplayInRect_toBitmapImageRep(rect: NSRect; bitmapImageRep: NSBitmapImageRep); message 'cacheDisplayInRect:toBitmapImageRep:';
    procedure viewWillDraw; message 'viewWillDraw';
    function gState: clong; message 'gState';
    procedure allocateGState; message 'allocateGState';
    procedure releaseGState; message 'releaseGState';
    procedure setUpGState; message 'setUpGState';
    procedure renewGState; message 'renewGState';
    procedure scrollPoint(aPoint: NSPoint); message 'scrollPoint:';
    function scrollRectToVisible(aRect: NSRect): Boolean; message 'scrollRectToVisible:';
    function autoscroll(theEvent: NSEvent): Boolean; message 'autoscroll:';
    function adjustScroll(newVisible: NSRect): NSRect; message 'adjustScroll:';
    procedure scrollRect_by(aRect: NSRect; delta: NSSize); message 'scrollRect:by:';
    procedure translateRectsNeedingDisplayInRect_by(clipRect: NSRect; delta: NSSize); message 'translateRectsNeedingDisplayInRect:by:';
    function hitTest(aPoint: NSPoint): NSView; message 'hitTest:';
    function mouse_inRect(aPoint: NSPoint; aRect: NSRect): Boolean; message 'mouse:inRect:';
    function viewWithTag(aTag: clong): id; message 'viewWithTag:';
    function tag: clong; message 'tag';
    function performKeyEquivalent(theEvent: NSEvent): Boolean; message 'performKeyEquivalent:';
    function acceptsFirstMouse(theEvent: NSEvent): Boolean; message 'acceptsFirstMouse:';
    function shouldDelayWindowOrderingForEvent(theEvent: NSEvent): Boolean; message 'shouldDelayWindowOrderingForEvent:';
    function needsPanelToBecomeKey: Boolean; message 'needsPanelToBecomeKey';
    function mouseDownCanMoveWindow: Boolean; message 'mouseDownCanMoveWindow';
    procedure addCursorRect_cursor(aRect: NSRect; anObj: NSCursor); message 'addCursorRect:cursor:';
    procedure removeCursorRect_cursor(aRect: NSRect; anObj: NSCursor); message 'removeCursorRect:cursor:';
    procedure discardCursorRects; message 'discardCursorRects';
    procedure resetCursorRects; message 'resetCursorRects';
    function addTrackingRect_owner_userData_assumeInside(aRect: NSRect; anObject: id; data: Pointer; flag: Boolean): NSTrackingRectTag; message 'addTrackingRect:owner:userData:assumeInside:';
    procedure removeTrackingRect(tag_: NSTrackingRectTag); message 'removeTrackingRect:';
    procedure setWantsLayer(flag: Boolean); message 'setWantsLayer:';
    function wantsLayer: Boolean; message 'wantsLayer';
    procedure setLayer(var newLayer: CALayer); message 'setLayer:';
    function layer: CALayer; message 'layer';
    procedure setAlphaValue(viewAlpha: CGFloat); message 'setAlphaValue:';
    function alphaValue: CGFloat; message 'alphaValue';
    procedure setBackgroundFilters(filters: NSArray); message 'setBackgroundFilters:';
    function backgroundFilters: NSArray; message 'backgroundFilters';
    procedure setCompositingFilter(var filter: CIFilter); message 'setCompositingFilter:';
    function compositingFilter: CIFilter; message 'compositingFilter';
    procedure setContentFilters(filters: NSArray); message 'setContentFilters:';
    function contentFilters: NSArray; message 'contentFilters';
    procedure setShadow(shadow_: NSShadow); message 'setShadow:';
    function shadow: NSShadow; message 'shadow';
    procedure addTrackingArea(trackingArea: NSTrackingArea); message 'addTrackingArea:';
    procedure removeTrackingArea(trackingArea: NSTrackingArea); message 'removeTrackingArea:';
    function trackingAreas: NSArray; message 'trackingAreas';
    procedure updateTrackingAreas; message 'updateTrackingAreas';
    function shouldDrawColor: Boolean; message 'shouldDrawColor';
    procedure setPostsBoundsChangedNotifications(flag: Boolean); message 'setPostsBoundsChangedNotifications:';
    function postsBoundsChangedNotifications: Boolean; message 'postsBoundsChangedNotifications';
    function enclosingScrollView: NSScrollView; message 'enclosingScrollView';
    function menuForEvent(event: NSEvent): NSMenu; message 'menuForEvent:';
    class function defaultMenu: NSMenu; message 'defaultMenu';
    procedure setToolTip(string_: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';
    function addToolTipRect_owner_userData(aRect: NSRect; anObject: id; data: Pointer): NSToolTipTag; message 'addToolTipRect:owner:userData:';
    procedure removeToolTip(tag_: NSToolTipTag); message 'removeToolTip:';
    procedure removeAllToolTips; message 'removeAllToolTips';
    procedure viewWillStartLiveResize; message 'viewWillStartLiveResize';
    procedure viewDidEndLiveResize; message 'viewDidEndLiveResize';
    function inLiveResize: Boolean; message 'inLiveResize';
    function preservesContentDuringLiveResize: Boolean; message 'preservesContentDuringLiveResize';
    function rectPreservedDuringLiveResize: NSRect; message 'rectPreservedDuringLiveResize';
    procedure getRectsExposedDuringLiveResize_count(exposedRects: NSRect; var count: clong); message 'getRectsExposedDuringLiveResize:count:';

    { Category: NSKeyboardUI }
    function performMnemonic(theString: NSString): Boolean; message 'performMnemonic:';
    procedure setNextKeyView(next: NSView); message 'setNextKeyView:';
    function nextKeyView: NSView; message 'nextKeyView';
    function previousKeyView: NSView; message 'previousKeyView';
    function nextValidKeyView: NSView; message 'nextValidKeyView';
    function previousValidKeyView: NSView; message 'previousValidKeyView';
    function canBecomeKeyView: Boolean; message 'canBecomeKeyView';
    procedure setKeyboardFocusRingNeedsDisplayInRect(rect: NSRect); message 'setKeyboardFocusRingNeedsDisplayInRect:';
    procedure setFocusRingType(focusRingType_: NSFocusRingType); message 'setFocusRingType:';
    function focusRingType: NSFocusRingType; message 'focusRingType';
    class function defaultFocusRingType: NSFocusRingType; message 'defaultFocusRingType';

    { Category: NSPrinting }
    procedure writeEPSInsideRect_toPasteboard(rect: NSRect; pasteboard: NSPasteboard); message 'writeEPSInsideRect:toPasteboard:';
    function dataWithEPSInsideRect(rect: NSRect): NSData; message 'dataWithEPSInsideRect:';
    procedure writePDFInsideRect_toPasteboard(rect: NSRect; pasteboard: NSPasteboard); message 'writePDFInsideRect:toPasteboard:';
    function dataWithPDFInsideRect(rect: NSRect): NSData; message 'dataWithPDFInsideRect:';
    procedure print(sender: id); message 'print:';
    function knowsPageRange(range: NSRangePointer): Boolean; message 'knowsPageRange:';
    function heightAdjustLimit: CGFloat; message 'heightAdjustLimit';
    function widthAdjustLimit: CGFloat; message 'widthAdjustLimit';
    procedure adjustPageWidthNew_left_right_limit(var newRight: CGFloat; oldLeft: CGFloat; oldRight: CGFloat; rightLimit: CGFloat); message 'adjustPageWidthNew:left:right:limit:';
    procedure adjustPageHeightNew_top_bottom_limit(var newBottom: CGFloat; oldTop: CGFloat; oldBottom: CGFloat; bottomLimit: CGFloat); message 'adjustPageHeightNew:top:bottom:limit:';
    function rectForPage(page: clong): NSRect; message 'rectForPage:';
    function locationOfPrintRect(aRect: NSRect): NSPoint; message 'locationOfPrintRect:';
    procedure drawPageBorderWithSize(borderSize: NSSize); message 'drawPageBorderWithSize:';
    function pageHeader: NSAttributedString; message 'pageHeader';
    function pageFooter: NSAttributedString; message 'pageFooter';
    procedure drawSheetBorderWithSize(borderSize: NSSize); message 'drawSheetBorderWithSize:';
    function printJobTitle: NSString; message 'printJobTitle';
    procedure beginDocument; message 'beginDocument';
    procedure endDocument; message 'endDocument';
    procedure beginPageInRect_atPlacement(aRect: NSRect; location: NSPoint); message 'beginPageInRect:atPlacement:';
    procedure endPage; message 'endPage';

    { Category: NSDrag }
    procedure dragImage_at_offset_event_pasteboard_source_slideBack(anImage: NSImage; viewLocation: NSPoint; initialOffset: NSSize; event: NSEvent; pboard: NSPasteboard; sourceObj: id; slideFlag: Boolean); message 'dragImage:at:offset:event:pasteboard:source:slideBack:';
    function registeredDraggedTypes: NSArray; message 'registeredDraggedTypes';
    procedure registerForDraggedTypes(newTypes: NSArray); message 'registerForDraggedTypes:';
    procedure unregisterDraggedTypes; message 'unregisterDraggedTypes';
    function dragFile_fromRect_slideBack_event(filename: NSString; rect: NSRect; aFlag: Boolean; event: NSEvent): Boolean; message 'dragFile:fromRect:slideBack:event:';
    function dragPromisedFilesOfTypes_fromRect_source_slideBack_event(typeArray: NSArray; rect: NSRect; sourceObject: id; aFlag: Boolean; event: NSEvent): Boolean; message 'dragPromisedFilesOfTypes:fromRect:source:slideBack:event:';

    { Category: NSFullScreenMode }
    function enterFullScreenMode_withOptions(screen: NSScreen; options: NSDictionary): Boolean; message 'enterFullScreenMode:withOptions:';
    procedure exitFullScreenModeWithOptions(options: NSDictionary); message 'exitFullScreenModeWithOptions:';
    function isInFullScreenMode: Boolean; message 'isInFullScreenMode';

    { Category: NSClipViewSuperview }
    procedure reflectScrolledClipView(aClipView: NSClipView); message 'reflectScrolledClipView:';
    procedure scrollClipView_toPoint(aClipView: NSClipView; aPoint: NSPoint); message 'scrollClipView:toPoint:';

    { Category: NSRulerMarkerClientViewDelegation }
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
  end; external;

{$endif}
{$endif}
