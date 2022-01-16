{ Parsed from AppKit.framework NSView.h }


{ Callbacks }
{$ifdef TYPES}
type
  NSViewSortSubviewsUsingFunctionCompareCallback = function (param1: id; param2: id; param3: pointer): NSComparisonResult; cdecl;
{$endif}


{$ifdef TYPES}
type
  NSViewPtr = ^NSView;
{$endif}

{$ifdef TYPES}
type
  NSAutoresizingMaskOptions = NSUInteger;
  NSAutoresizingMaskOptionsPtr = ^NSAutoresizingMaskOptions;

const
  NSViewNotSizable = 0;
  NSViewMinXMargin = 1;
  NSViewWidthSizable = 2;
  NSViewMaxXMargin = 4;
  NSViewMinYMargin = 8;
  NSViewHeightSizable = 16;
  NSViewMaxYMargin = 32;

type
  NSBorderType = NSUInteger;
  NSBorderTypePtr = ^NSBorderType;

const
  NSNoBorder = 0;
  NSLineBorder = 1;
  NSBezelBorder = 2;
  NSGrooveBorder = 3;

type
  NSViewLayerContentsRedrawPolicy = NSInteger;
  NSViewLayerContentsRedrawPolicyPtr = ^NSViewLayerContentsRedrawPolicy;

const
  NSViewLayerContentsRedrawNever = 0;
  NSViewLayerContentsRedrawOnSetNeedsDisplay = 1;
  NSViewLayerContentsRedrawDuringViewResize = 2;
  NSViewLayerContentsRedrawBeforeViewResize = 3;
  NSViewLayerContentsRedrawCrossfade = 4 { available in 10_9 };

type
  NSViewLayerContentsPlacement = NSInteger;
  NSViewLayerContentsPlacementPtr = ^NSViewLayerContentsPlacement;

const
  NSViewLayerContentsPlacementScaleAxesIndependently = 0;
  NSViewLayerContentsPlacementScaleProportionallyToFit = 1;
  NSViewLayerContentsPlacementScaleProportionallyToFill = 2;
  NSViewLayerContentsPlacementCenter = 3;
  NSViewLayerContentsPlacementTop = 4;
  NSViewLayerContentsPlacementTopRight = 5;
  NSViewLayerContentsPlacementRight = 6;
  NSViewLayerContentsPlacementBottomRight = 7;
  NSViewLayerContentsPlacementBottom = 8;
  NSViewLayerContentsPlacementBottomLeft = 9;
  NSViewLayerContentsPlacementLeft = 10;
  NSViewLayerContentsPlacementTopLeft = 11;

type
  _VFlags = bitpacked record
    case byte of
      0: (_anonBitField__VFlags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
        rotatedFromBase: 0..1;
        rotatedOrScaledFromBase: 0..1;
        autosizing: 0..((1 shl 6)-1);
        autoresizeSubviews: 0..1;
        wantsGState: 0..1;
        needsDisplay: 0..1;
        allowsVibrancy: 0..((1 shl 2)-1);
        canDrawSubviewsIntoLayer: 0..1;
        frameChangeNotesSuspended: 0..1;
        needsFrameChangeNote: 0..1;
        unused2: 0..1;
        boundsChangeNotesSuspended: 0..1;
        needsBoundsChangeNote: 0..1;
        removingWithoutInvalidation: 0..1;
        isFlipped: 0..1;
        needsDisplayForBounds: 0..1;
        specialArchiving: 0..1;
        ignoreHitTest: 0..1;
        hasNotMessedWithIsFlipped: 0..1;
        unused3: 0..((1 shl 5)-1);
        isOpaque: 0..1;
        aboutToResize: 0..1;
        {$else}
        aboutToResize_: 0..1;
        isOpaque_: 0..1;
        unused3_: 0..((1 shl 5)-1);
        hasNotMessedWithIsFlipped_: 0..1;
        ignoreHitTest_: 0..1;
        specialArchiving_: 0..1;
        needsDisplayForBounds_: 0..1;
        isFlipped_: 0..1;
        removingWithoutInvalidation_: 0..1;
        needsBoundsChangeNote_: 0..1;
        boundsChangeNotesSuspended_: 0..1;
        unused2_: 0..1;
        needsFrameChangeNote_: 0..1;
        frameChangeNotesSuspended_: 0..1;
        canDrawSubviewsIntoLayer_: 0..1;
        allowsVibrancy_: 0..((1 shl 2)-1);
        needsDisplay_: 0..1;
        wantsGState_: 0..1;
        autoresizeSubviews_: 0..1;
        autosizing_: 0..((1 shl 6)-1);
        rotatedOrScaledFromBase_: 0..1;
        rotatedFromBase_: 0..1;
        {$endif}
      );
    end;
type
  __VFlags = _VFlags;
type
  _VFlagsPtr = ^__VFlags;
type
  __VFlagsPtr = ^__VFlags;

type
  NSTrackingRectTag = NSInteger;
  NSTrackingRectTagPtr = ^NSTrackingRectTag;
  NSToolTipTag = NSInteger;
  NSToolTipTagPtr = ^NSToolTipTag;
{$endif}

{$ifdef CLASSES}

type
  NSView = objcclass external (NSResponder, NSAnimatablePropertyContainerProtocol, NSUserInterfaceItemIdentificationProtocol, NSDraggingDestinationProtocol, NSAppearanceCustomizationProtocol, NSAccessibilityElementProtocol, NSAccessibilityProtocol)
  private
    _frame: NSRect;
    _bounds: NSRect;
    _superview: id;
    _subviews: id;
    _window: NSWindow;
    _unused_was_gState: id;
    _frameMatrix: id;
    _layer: CALayer;
    _dragTypes: id;
    _viewAuxiliary: _NSViewAuxiliary;
    _vFlags: _VFlags;
    __VFlags2: bitpacked record
      case byte of
        0: (_anonBitField___VFlags20: cuint);
        1: (
          nextKeyViewRefCount: 0..((1 shl 14)-1);
          previousKeyViewRefCount: 0..((1 shl 14)-1);
          isVisibleRect: 0..1;
          hasToolTip: 0..1;
          cachedIsFlipped: 0..1;
          menuWasSet: 0..1;
        );
      end;
  public
    function initWithFrame (frameRect: NSRect): instancetype; message 'initWithFrame:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function window: NSWindow; message 'window';
    function superview: NSView; message 'superview';
    function subviews: NSArray; message 'subviews';
    function isDescendantOf (aView: NSView): ObjCBOOL; message 'isDescendantOf:';
    function ancestorSharedWithView (aView: NSView): NSView; message 'ancestorSharedWithView:';
    function opaqueAncestor: NSView; message 'opaqueAncestor';
    procedure setHidden(newValue: ObjCBOOL); message 'setHidden:';
    function isHidden: ObjCBOOL; message 'isHidden';
    function isHiddenOrHasHiddenAncestor: ObjCBOOL; message 'isHiddenOrHasHiddenAncestor';
    procedure getRectsBeingDrawn_count (rects: NSRectPtr; count: NSIntegerPtr); message 'getRectsBeingDrawn:count:';
    function needsToDrawRect (aRect: NSRect): ObjCBOOL; message 'needsToDrawRect:';
    function wantsDefaultClipping: ObjCBOOL; message 'wantsDefaultClipping';
    procedure viewDidHide; message 'viewDidHide'; { available in 10_5 }
    procedure viewDidUnhide; message 'viewDidUnhide'; { available in 10_5 }
    procedure setSubviews (newSubviews: NSArray); message 'setSubviews:'; { available in 10_5 }
    procedure addSubview (aView: NSView); message 'addSubview:';
    procedure addSubview_positioned_relativeTo (aView: NSView; place: NSWindowOrderingMode; otherView: NSView); message 'addSubview:positioned:relativeTo:';
    procedure sortSubviewsUsingFunction_context (compare: NSViewSortSubviewsUsingFunctionCompareCallback; context: pointer); message 'sortSubviewsUsingFunction:context:';
    procedure viewWillMoveToWindow (newWindow: NSWindow); message 'viewWillMoveToWindow:';
    procedure viewDidMoveToWindow; message 'viewDidMoveToWindow';
    procedure viewWillMoveToSuperview (newSuperview: NSView); message 'viewWillMoveToSuperview:';
    procedure viewDidMoveToSuperview; message 'viewDidMoveToSuperview';
    procedure didAddSubview (subview: NSView); message 'didAddSubview:';
    procedure willRemoveSubview (subview: NSView); message 'willRemoveSubview:';
    procedure removeFromSuperview; message 'removeFromSuperview';
    procedure replaceSubview_with (oldView: NSView; newView: NSView); message 'replaceSubview:with:';
    procedure removeFromSuperviewWithoutNeedingDisplay; message 'removeFromSuperviewWithoutNeedingDisplay';
    procedure viewDidChangeBackingProperties; message 'viewDidChangeBackingProperties'; { available in 10_7 }
    procedure setPostsFrameChangedNotifications(newValue: ObjCBOOL); message 'setPostsFrameChangedNotifications:';
    function postsFrameChangedNotifications: ObjCBOOL; message 'postsFrameChangedNotifications';
    procedure resizeSubviewsWithOldSize (oldSize: NSSize); message 'resizeSubviewsWithOldSize:';
    procedure resizeWithOldSuperviewSize (oldSize: NSSize); message 'resizeWithOldSuperviewSize:';
    procedure setAutoresizesSubviews(newValue: ObjCBOOL); message 'setAutoresizesSubviews:';
    function autoresizesSubviews: ObjCBOOL; message 'autoresizesSubviews';
    procedure setAutoresizingMask(newValue: NSAutoresizingMaskOptions); message 'setAutoresizingMask:';
    function autoresizingMask: NSAutoresizingMaskOptions; message 'autoresizingMask';
    procedure setFrameOrigin (newOrigin: NSPoint); message 'setFrameOrigin:';
    procedure setFrameSize (newSize: NSSize); message 'setFrameSize:';
    procedure setFrame(newValue: NSRect); message 'setFrame:';
    function frame: NSRect; message 'frame';
    procedure setFrameRotation(newValue: CGFloat); message 'setFrameRotation:';
    function frameRotation: CGFloat; message 'frameRotation';
    procedure setFrameCenterRotation(newValue: CGFloat); message 'setFrameCenterRotation:';
    function frameCenterRotation: CGFloat; message 'frameCenterRotation';
    procedure setBoundsOrigin (newOrigin: NSPoint); message 'setBoundsOrigin:';
    procedure setBoundsSize (newSize: NSSize); message 'setBoundsSize:';
    procedure setBoundsRotation(newValue: CGFloat); message 'setBoundsRotation:';
    function boundsRotation: CGFloat; message 'boundsRotation';
    procedure translateOriginToPoint (translation: NSPoint); message 'translateOriginToPoint:';
    procedure scaleUnitSquareToSize (newUnitSize: NSSize); message 'scaleUnitSquareToSize:';
    procedure rotateByAngle (angle: CGFloat); message 'rotateByAngle:';
    procedure setBounds(newValue: NSRect); message 'setBounds:';
    function bounds: NSRect; message 'bounds';
    function isFlipped: ObjCBOOL; message 'isFlipped';
    function isRotatedFromBase: ObjCBOOL; message 'isRotatedFromBase';
    function isRotatedOrScaledFromBase: ObjCBOOL; message 'isRotatedOrScaledFromBase';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    function convertPoint_fromView (aPoint: NSPoint; aView: NSView): NSPoint; message 'convertPoint:fromView:';
    function convertPoint_toView (aPoint: NSPoint; aView: NSView): NSPoint; message 'convertPoint:toView:';
    function convertSize_fromView (aSize: NSSize; aView: NSView): NSSize; message 'convertSize:fromView:';
    function convertSize_toView (aSize: NSSize; aView: NSView): NSSize; message 'convertSize:toView:';
    function convertRect_fromView (aRect: NSRect; aView: NSView): NSRect; message 'convertRect:fromView:';
    function convertRect_toView (aRect: NSRect; aView: NSView): NSRect; message 'convertRect:toView:';
    function backingAlignedRect_options (aRect: NSRect; options: NSAlignmentOptions): NSRect; message 'backingAlignedRect:options:'; { available in 10_7 }
    function centerScanRect (aRect: NSRect): NSRect; message 'centerScanRect:';
    function convertPointToBacking (aPoint: NSPoint): NSPoint; message 'convertPointToBacking:'; { available in 10_7 }
    function convertPointFromBacking (aPoint: NSPoint): NSPoint; message 'convertPointFromBacking:'; { available in 10_7 }
    function convertSizeToBacking (aSize: NSSize): NSSize; message 'convertSizeToBacking:'; { available in 10_7 }
    function convertSizeFromBacking (aSize: NSSize): NSSize; message 'convertSizeFromBacking:'; { available in 10_7 }
    function convertRectToBacking (aRect: NSRect): NSRect; message 'convertRectToBacking:'; { available in 10_7 }
    function convertRectFromBacking (aRect: NSRect): NSRect; message 'convertRectFromBacking:'; { available in 10_7 }
    function convertPointToLayer (aPoint: NSPoint): NSPoint; message 'convertPointToLayer:'; { available in 10_7 }
    function convertPointFromLayer (aPoint: NSPoint): NSPoint; message 'convertPointFromLayer:'; { available in 10_7 }
    function convertSizeToLayer (aSize: NSSize): NSSize; message 'convertSizeToLayer:'; { available in 10_7 }
    function convertSizeFromLayer (aSize: NSSize): NSSize; message 'convertSizeFromLayer:'; { available in 10_7 }
    function convertRectToLayer (aRect: NSRect): NSRect; message 'convertRectToLayer:'; { available in 10_7 }
    function convertRectFromLayer (aRect: NSRect): NSRect; message 'convertRectFromLayer:'; { available in 10_7 }
    function convertPointToBase (aPoint: NSPoint): NSPoint; message 'convertPointToBase:'; deprecated 'in 10_5, 10_7';
    function convertPointFromBase (aPoint: NSPoint): NSPoint; message 'convertPointFromBase:'; deprecated 'in 10_5, 10_7';
    function convertSizeToBase (aSize: NSSize): NSSize; message 'convertSizeToBase:'; deprecated 'in 10_5, 10_7';
    function convertSizeFromBase (aSize: NSSize): NSSize; message 'convertSizeFromBase:'; deprecated 'in 10_5, 10_7';
    function convertRectToBase (aRect: NSRect): NSRect; message 'convertRectToBase:'; deprecated 'in 10_5, 10_7';
    function convertRectFromBase (aRect: NSRect): NSRect; message 'convertRectFromBase:'; deprecated 'in 10_5, 10_7';
    procedure setCanDrawConcurrently(newValue: ObjCBOOL); message 'setCanDrawConcurrently:';
    function canDrawConcurrently: ObjCBOOL; message 'canDrawConcurrently';
    function canDraw: ObjCBOOL; message 'canDraw';
    procedure setNeedsDisplayInRect (invalidRect: NSRect); message 'setNeedsDisplayInRect:';
		{$ifdef LEGACY_SETNEEDSDISPLAY}
		procedure setNeedsDisplay_(newValue: ObjCBOOL); message 'setNeedsDisplay:';
 		{$else}
 		procedure setNeedsDisplay(newValue: ObjCBOOL); message 'setNeedsDisplay:';
		{$endif}
    function needsDisplay: ObjCBOOL; message 'needsDisplay';
    procedure lockFocus; message 'lockFocus';
    procedure unlockFocus; message 'unlockFocus';
    function lockFocusIfCanDraw: ObjCBOOL; message 'lockFocusIfCanDraw';
    function lockFocusIfCanDrawInContext (context: NSGraphicsContext): ObjCBOOL; message 'lockFocusIfCanDrawInContext:';
    class function focusView: NSView; message 'focusView';
    function visibleRect: NSRect; message 'visibleRect';
    procedure display; message 'display';
    procedure displayIfNeeded; message 'displayIfNeeded';
    procedure displayIfNeededIgnoringOpacity; message 'displayIfNeededIgnoringOpacity';
    procedure displayRect (rect: NSRect); message 'displayRect:';
    procedure displayIfNeededInRect (rect: NSRect); message 'displayIfNeededInRect:';
    procedure displayRectIgnoringOpacity (rect: NSRect); message 'displayRectIgnoringOpacity:';
    procedure displayIfNeededInRectIgnoringOpacity (rect: NSRect); message 'displayIfNeededInRectIgnoringOpacity:';
    procedure drawRect (dirtyRect: NSRect); message 'drawRect:';
    procedure displayRectIgnoringOpacity_inContext (aRect: NSRect; context: NSGraphicsContext); message 'displayRectIgnoringOpacity:inContext:';
    function bitmapImageRepForCachingDisplayInRect (rect: NSRect): NSBitmapImageRep; message 'bitmapImageRepForCachingDisplayInRect:';
    procedure cacheDisplayInRect_toBitmapImageRep (rect: NSRect; bitmapImageRep: NSBitmapImageRep); message 'cacheDisplayInRect:toBitmapImageRep:';
    procedure viewWillDraw; message 'viewWillDraw'; { available in 10_5 }
    procedure scrollPoint (aPoint: NSPoint); message 'scrollPoint:';
    function scrollRectToVisible (aRect: NSRect): ObjCBOOL; message 'scrollRectToVisible:';
    function autoscroll (theEvent: NSEvent): ObjCBOOL; message 'autoscroll:';
    function adjustScroll (newVisible: NSRect): NSRect; message 'adjustScroll:';
    procedure scrollRect_by (aRect: NSRect; delta: NSSize); message 'scrollRect:by:';
    procedure translateRectsNeedingDisplayInRect_by (clipRect: NSRect; delta: NSSize); message 'translateRectsNeedingDisplayInRect:by:'; { available in 10_5 }
    function hitTest (aPoint: NSPoint): NSView; message 'hitTest:';
    function mouse_inRect (aPoint: NSPoint; aRect: NSRect): ObjCBOOL; message 'mouse:inRect:';
    function viewWithTag (aTag: NSInteger): id; message 'viewWithTag:';
    function tag: NSInteger; message 'tag';
    function performKeyEquivalent (theEvent: NSEvent): ObjCBOOL; message 'performKeyEquivalent:';
    function acceptsFirstMouse (theEvent: NSEvent): ObjCBOOL; message 'acceptsFirstMouse:';
    function shouldDelayWindowOrderingForEvent (theEvent: NSEvent): ObjCBOOL; message 'shouldDelayWindowOrderingForEvent:';
    function needsPanelToBecomeKey: ObjCBOOL; message 'needsPanelToBecomeKey';
    function mouseDownCanMoveWindow: ObjCBOOL; message 'mouseDownCanMoveWindow';
    procedure setAcceptsTouchEvents(newValue: ObjCBOOL); message 'setAcceptsTouchEvents:';
    function acceptsTouchEvents: ObjCBOOL; message 'acceptsTouchEvents';
    procedure setWantsRestingTouches(newValue: ObjCBOOL); message 'setWantsRestingTouches:';
    function wantsRestingTouches: ObjCBOOL; message 'wantsRestingTouches';
    procedure addCursorRect_cursor (aRect: NSRect; anObj: NSCursor); message 'addCursorRect:cursor:';
    procedure removeCursorRect_cursor (aRect: NSRect; anObj: NSCursor); message 'removeCursorRect:cursor:';
    procedure discardCursorRects; message 'discardCursorRects';
    procedure resetCursorRects; message 'resetCursorRects';
    function addTrackingRect_owner_userData_assumeInside (aRect: NSRect; anObject: id; data: pointer; flag: ObjCBOOL): NSTrackingRectTag; message 'addTrackingRect:owner:userData:assumeInside:';
    procedure removeTrackingRect (tag_: NSTrackingRectTag); message 'removeTrackingRect:';
    function makeBackingLayer: CALayer; message 'makeBackingLayer'; { available in 10_6 }
    procedure setLayerContentsRedrawPolicy(newValue: NSViewLayerContentsRedrawPolicy); message 'setLayerContentsRedrawPolicy:';
    function layerContentsRedrawPolicy: NSViewLayerContentsRedrawPolicy; message 'layerContentsRedrawPolicy';
    procedure setLayerContentsPlacement(newValue: NSViewLayerContentsPlacement); message 'setLayerContentsPlacement:';
    function layerContentsPlacement: NSViewLayerContentsPlacement; message 'layerContentsPlacement';
    procedure setWantsLayer(newValue: ObjCBOOL); message 'setWantsLayer:';
    function wantsLayer: ObjCBOOL; message 'wantsLayer';
    procedure setLayer(newValue: CALayer); message 'setLayer:';
    function layer: CALayer; message 'layer';
    function wantsUpdateLayer: ObjCBOOL; message 'wantsUpdateLayer';
    procedure updateLayer; message 'updateLayer'; { available in 10_8 }
    procedure setCanDrawSubviewsIntoLayer(newValue: ObjCBOOL); message 'setCanDrawSubviewsIntoLayer:';
    function canDrawSubviewsIntoLayer: ObjCBOOL; message 'canDrawSubviewsIntoLayer';
    procedure setAlphaValue(newValue: CGFloat); message 'setAlphaValue:';
    function alphaValue: CGFloat; message 'alphaValue';
    procedure setLayerUsesCoreImageFilters(newValue: ObjCBOOL); message 'setLayerUsesCoreImageFilters:';
    function layerUsesCoreImageFilters: ObjCBOOL; message 'layerUsesCoreImageFilters';
    procedure setBackgroundFilters(newValue: NSArray); message 'setBackgroundFilters:';
    function backgroundFilters: NSArray; message 'backgroundFilters';
    procedure setCompositingFilter(newValue: CIFilter); message 'setCompositingFilter:';
    function compositingFilter: CIFilter; message 'compositingFilter';
    procedure setContentFilters(newValue: NSArray); message 'setContentFilters:';
    function contentFilters: NSArray; message 'contentFilters';
    procedure setShadow(newValue: NSShadow); message 'setShadow:';
    function shadow: NSShadow; message 'shadow';
    procedure addTrackingArea (trackingArea: NSTrackingArea); message 'addTrackingArea:'; { available in 10_5 }
    procedure removeTrackingArea (trackingArea: NSTrackingArea); message 'removeTrackingArea:'; { available in 10_5 }
    function trackingAreas: NSArray; message 'trackingAreas';
    procedure updateTrackingAreas; message 'updateTrackingAreas'; { available in 10_5 }
    procedure setPostsBoundsChangedNotifications(newValue: ObjCBOOL); message 'setPostsBoundsChangedNotifications:';
    function postsBoundsChangedNotifications: ObjCBOOL; message 'postsBoundsChangedNotifications';
    function enclosingScrollView: NSScrollView; message 'enclosingScrollView';
    function menuForEvent (event: NSEvent): NSMenu; message 'menuForEvent:';
    class function defaultMenu: NSMenu; message 'defaultMenu';
    procedure setToolTip(newValue: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';
    function addToolTipRect_owner_userData (aRect: NSRect; anObject: id; data: pointer): NSToolTipTag; message 'addToolTipRect:owner:userData:';
    procedure removeToolTip (tag_: NSToolTipTag); message 'removeToolTip:';
    procedure removeAllToolTips; message 'removeAllToolTips';
    procedure viewWillStartLiveResize; message 'viewWillStartLiveResize';
    procedure viewDidEndLiveResize; message 'viewDidEndLiveResize';
    function inLiveResize: ObjCBOOL; message 'inLiveResize';
    function preservesContentDuringLiveResize: ObjCBOOL; message 'preservesContentDuringLiveResize';
    function rectPreservedDuringLiveResize: NSRect; message 'rectPreservedDuringLiveResize';
    procedure getRectsExposedDuringLiveResize_count (exposedRects: NSRectPtr { 4 element array of NSRect }; count: NSIntegerPtr); message 'getRectsExposedDuringLiveResize:count:';
    function inputContext: NSTextInputContext; message 'inputContext';
    function rectForSmartMagnificationAtPoint_inRect (location: NSPoint; visibleRect_: NSRect): NSRect; message 'rectForSmartMagnificationAtPoint:inRect:'; { available in 10_8 }
    procedure setUserInterfaceLayoutDirection(newValue: NSUserInterfaceLayoutDirection); message 'setUserInterfaceLayoutDirection:';
    function userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection; message 'userInterfaceLayoutDirection';
    procedure prepareForReuse; message 'prepareForReuse'; { available in 10_7 }
    class function isCompatibleWithResponsiveScrolling: ObjCBOOL; message 'isCompatibleWithResponsiveScrolling'; { available in 10_9 }
    procedure prepareContentInRect (rect: NSRect); message 'prepareContentInRect:'; { available in 10_9 }
    procedure setPreparedContentRect(newValue: NSRect); message 'setPreparedContentRect:';
    function preparedContentRect: NSRect; message 'preparedContentRect';
    function allowsVibrancy: ObjCBOOL; message 'allowsVibrancy';

    { Adopted protocols }
		procedure setAccessibilityFrame(newValue: NSRect); message 'setAccessibilityFrame:';
    procedure setAccessibilityParent(newValue: id); message 'setAccessibilityParent:';
    procedure setAccessibilityIdentifier(newValue: NSString); message 'setAccessibilityIdentifier:';
		
    procedure setAccessibilityActivationPoint(newValue: NSPoint); message 'setAccessibilityActivationPoint:';
    function accessibilityActivationPoint: NSPoint; message 'accessibilityActivationPoint';
    procedure setAccessibilityAllowedValues(newValue: NSArray); message 'setAccessibilityAllowedValues:';
    function accessibilityAllowedValues: NSArray; message 'accessibilityAllowedValues';
    procedure setAccessibilityAlternateUIVisible(newValue: ObjCBOOL); message 'setAccessibilityAlternateUIVisible:';
    function isAccessibilityAlternateUIVisible: ObjCBOOL; message 'isAccessibilityAlternateUIVisible';
    procedure setAccessibilityApplicationFocusedUIElement(newValue: id); message 'setAccessibilityApplicationFocusedUIElement:';
    function accessibilityApplicationFocusedUIElement: id; message 'accessibilityApplicationFocusedUIElement';
    function accessibilityAttributedStringForRange (range: NSRange): NSAttributedString; message 'accessibilityAttributedStringForRange:'; { available in 10_10 }
    procedure setAccessibilityCancelButton(newValue: id); message 'setAccessibilityCancelButton:';
    function accessibilityCancelButton: id; message 'accessibilityCancelButton';
    function accessibilityCellForColumn_row (column: NSInteger; row: NSInteger): id; message 'accessibilityCellForColumn:row:'; { available in 10_10 }
    procedure setAccessibilityChildren(newValue: NSArray); message 'setAccessibilityChildren:';
    function accessibilityChildren: NSArray; message 'accessibilityChildren';
    procedure setAccessibilityClearButton(newValue: id); message 'setAccessibilityClearButton:';
    function accessibilityClearButton: id; message 'accessibilityClearButton';
    procedure setAccessibilityCloseButton(newValue: id); message 'setAccessibilityCloseButton:';
    function accessibilityCloseButton: id; message 'accessibilityCloseButton';
    procedure setAccessibilityColumnCount(newValue: NSInteger); message 'setAccessibilityColumnCount:';
    function accessibilityColumnCount: NSInteger; message 'accessibilityColumnCount';
    procedure setAccessibilityColumnHeaderUIElements(newValue: NSArray); message 'setAccessibilityColumnHeaderUIElements:';
    function accessibilityColumnHeaderUIElements: NSArray; message 'accessibilityColumnHeaderUIElements';
    procedure setAccessibilityColumnIndexRange(newValue: NSRange); message 'setAccessibilityColumnIndexRange:';
    function accessibilityColumnIndexRange: NSRange; message 'accessibilityColumnIndexRange';
    procedure setAccessibilityColumns(newValue: NSArray); message 'setAccessibilityColumns:';
    function accessibilityColumns: NSArray; message 'accessibilityColumns';
    procedure setAccessibilityColumnTitles(newValue: NSArray); message 'setAccessibilityColumnTitles:';
    function accessibilityColumnTitles: NSArray; message 'accessibilityColumnTitles';
    procedure setAccessibilityContents(newValue: NSArray); message 'setAccessibilityContents:';
    function accessibilityContents: NSArray; message 'accessibilityContents';
    procedure setAccessibilityCriticalValue(newValue: id); message 'setAccessibilityCriticalValue:';
    function accessibilityCriticalValue: id; message 'accessibilityCriticalValue';
    procedure setAccessibilityDecrementButton(newValue: id); message 'setAccessibilityDecrementButton:';
    function accessibilityDecrementButton: id; message 'accessibilityDecrementButton';
    procedure setAccessibilityDefaultButton(newValue: id); message 'setAccessibilityDefaultButton:';
    function accessibilityDefaultButton: id; message 'accessibilityDefaultButton';
    procedure setAccessibilityDisclosed(newValue: ObjCBOOL); message 'setAccessibilityDisclosed:';
    function isAccessibilityDisclosed: ObjCBOOL; message 'isAccessibilityDisclosed';
    procedure setAccessibilityDisclosedByRow(newValue: id); message 'setAccessibilityDisclosedByRow:';
    function accessibilityDisclosedByRow: id; message 'accessibilityDisclosedByRow';
    procedure setAccessibilityDisclosedRows(newValue: id); message 'setAccessibilityDisclosedRows:';
    function accessibilityDisclosedRows: id; message 'accessibilityDisclosedRows';
    procedure setAccessibilityDisclosureLevel(newValue: NSInteger); message 'setAccessibilityDisclosureLevel:';
    function accessibilityDisclosureLevel: NSInteger; message 'accessibilityDisclosureLevel';
    procedure setAccessibilityDocument(newValue: NSString); message 'setAccessibilityDocument:';
    function accessibilityDocument: NSString; message 'accessibilityDocument';
    procedure setAccessibilityEdited(newValue: ObjCBOOL); message 'setAccessibilityEdited:';
    function isAccessibilityEdited: ObjCBOOL; message 'isAccessibilityEdited';
    procedure setAccessibilityElement(newValue: ObjCBOOL); message 'setAccessibilityElement:';
    function isAccessibilityElement: ObjCBOOL; message 'isAccessibilityElement';
    procedure setAccessibilityEnabled(newValue: ObjCBOOL); message 'setAccessibilityEnabled:';
    function isAccessibilityEnabled: ObjCBOOL; message 'isAccessibilityEnabled';
    procedure setAccessibilityExpanded(newValue: ObjCBOOL); message 'setAccessibilityExpanded:';
    function isAccessibilityExpanded: ObjCBOOL; message 'isAccessibilityExpanded';
    procedure setAccessibilityExtrasMenuBar(newValue: id); message 'setAccessibilityExtrasMenuBar:';
    function accessibilityExtrasMenuBar: id; message 'accessibilityExtrasMenuBar';
    procedure setAccessibilityFilename(newValue: NSString); message 'setAccessibilityFilename:';
    function accessibilityFilename: NSString; message 'accessibilityFilename';
    procedure setAccessibilityFocused(newValue: ObjCBOOL); message 'setAccessibilityFocused:';
    function isAccessibilityFocused: ObjCBOOL; message 'isAccessibilityFocused';
    procedure setAccessibilityFocusedWindow(newValue: id); message 'setAccessibilityFocusedWindow:';
    function accessibilityFocusedWindow: id; message 'accessibilityFocusedWindow';
    function accessibilityFrame: NSRect; message 'accessibilityFrame';
    function accessibilityFrameForRange (range: NSRange): NSRect; message 'accessibilityFrameForRange:'; { available in 10_10 }
    procedure setAccessibilityFrontmost(newValue: ObjCBOOL); message 'setAccessibilityFrontmost:';
    function isAccessibilityFrontmost: ObjCBOOL; message 'isAccessibilityFrontmost';
    procedure setAccessibilityFullScreenButton(newValue: id); message 'setAccessibilityFullScreenButton:';
    function accessibilityFullScreenButton: id; message 'accessibilityFullScreenButton';
    procedure setAccessibilityGrowArea(newValue: id); message 'setAccessibilityGrowArea:';
    function accessibilityGrowArea: id; message 'accessibilityGrowArea';
    procedure setAccessibilityHandles(newValue: NSArray); message 'setAccessibilityHandles:';
    function accessibilityHandles: NSArray; message 'accessibilityHandles';
    procedure setAccessibilityHeader(newValue: id); message 'setAccessibilityHeader:';
    function accessibilityHeader: id; message 'accessibilityHeader';
    procedure setAccessibilityHelp(newValue: NSString); message 'setAccessibilityHelp:';
    function accessibilityHelp: NSString; message 'accessibilityHelp';
    procedure setAccessibilityHidden(newValue: ObjCBOOL); message 'setAccessibilityHidden:';
    function isAccessibilityHidden: ObjCBOOL; message 'isAccessibilityHidden';
    procedure setAccessibilityHorizontalScrollBar(newValue: id); message 'setAccessibilityHorizontalScrollBar:';
    function accessibilityHorizontalScrollBar: id; message 'accessibilityHorizontalScrollBar';
    procedure setAccessibilityHorizontalUnitDescription(newValue: NSString); message 'setAccessibilityHorizontalUnitDescription:';
    function accessibilityHorizontalUnitDescription: NSString; message 'accessibilityHorizontalUnitDescription';
    procedure setAccessibilityHorizontalUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityHorizontalUnits:';
    function accessibilityHorizontalUnits: NSAccessibilityUnits; message 'accessibilityHorizontalUnits';
    function accessibilityIdentifier: NSString; message 'accessibilityIdentifier';
    procedure setAccessibilityIncrementButton(newValue: id); message 'setAccessibilityIncrementButton:';
    function accessibilityIncrementButton: id; message 'accessibilityIncrementButton';
    procedure setAccessibilityIndex(newValue: NSInteger); message 'setAccessibilityIndex:';
    function accessibilityIndex: NSInteger; message 'accessibilityIndex';
    procedure setAccessibilityInsertionPointLineNumber(newValue: NSInteger); message 'setAccessibilityInsertionPointLineNumber:';
    function accessibilityInsertionPointLineNumber: NSInteger; message 'accessibilityInsertionPointLineNumber';
    procedure setAccessibilityLabel(newValue: NSString); message 'setAccessibilityLabel:';
    function accessibilityLabel: NSString; message 'accessibilityLabel';
    procedure setAccessibilityLabelUIElements(newValue: NSArray); message 'setAccessibilityLabelUIElements:';
    function accessibilityLabelUIElements: NSArray; message 'accessibilityLabelUIElements';
    procedure setAccessibilityLabelValue(newValue: single); message 'setAccessibilityLabelValue:';
    function accessibilityLabelValue: single; message 'accessibilityLabelValue';
    function accessibilityLayoutPointForScreenPoint (point: NSPoint): NSPoint; message 'accessibilityLayoutPointForScreenPoint:'; { available in 10_10 }
    function accessibilityLayoutSizeForScreenSize (size: NSSize): NSSize; message 'accessibilityLayoutSizeForScreenSize:'; { available in 10_10 }
    function accessibilityLineForIndex (index: NSInteger): NSInteger; message 'accessibilityLineForIndex:'; { available in 10_10 }
    procedure setAccessibilityLinkedUIElements(newValue: NSArray); message 'setAccessibilityLinkedUIElements:';
    function accessibilityLinkedUIElements: NSArray; message 'accessibilityLinkedUIElements';
    procedure setAccessibilityMain(newValue: ObjCBOOL); message 'setAccessibilityMain:';
    function isAccessibilityMain: ObjCBOOL; message 'isAccessibilityMain';
    procedure setAccessibilityMainWindow(newValue: id); message 'setAccessibilityMainWindow:';
    function accessibilityMainWindow: id; message 'accessibilityMainWindow';
    procedure setAccessibilityMarkerGroupUIElement(newValue: id); message 'setAccessibilityMarkerGroupUIElement:';
    function accessibilityMarkerGroupUIElement: id; message 'accessibilityMarkerGroupUIElement';
    procedure setAccessibilityMarkerTypeDescription(newValue: NSString); message 'setAccessibilityMarkerTypeDescription:';
    function accessibilityMarkerTypeDescription: NSString; message 'accessibilityMarkerTypeDescription';
    procedure setAccessibilityMarkerUIElements(newValue: NSArray); message 'setAccessibilityMarkerUIElements:';
    function accessibilityMarkerUIElements: NSArray; message 'accessibilityMarkerUIElements';
    procedure setAccessibilityMarkerValues(newValue: id); message 'setAccessibilityMarkerValues:';
    function accessibilityMarkerValues: id; message 'accessibilityMarkerValues';
    procedure setAccessibilityMaxValue(newValue: id); message 'setAccessibilityMaxValue:';
    function accessibilityMaxValue: id; message 'accessibilityMaxValue';
    procedure setAccessibilityMenuBar(newValue: id); message 'setAccessibilityMenuBar:';
    function accessibilityMenuBar: id; message 'accessibilityMenuBar';
    procedure setAccessibilityMinimizeButton(newValue: id); message 'setAccessibilityMinimizeButton:';
    function accessibilityMinimizeButton: id; message 'accessibilityMinimizeButton';
    procedure setAccessibilityMinimized(newValue: ObjCBOOL); message 'setAccessibilityMinimized:';
    function isAccessibilityMinimized: ObjCBOOL; message 'isAccessibilityMinimized';
    procedure setAccessibilityMinValue(newValue: id); message 'setAccessibilityMinValue:';
    function accessibilityMinValue: id; message 'accessibilityMinValue';
    procedure setAccessibilityModal(newValue: ObjCBOOL); message 'setAccessibilityModal:';
    function isAccessibilityModal: ObjCBOOL; message 'isAccessibilityModal';
    procedure setAccessibilityNextContents(newValue: NSArray); message 'setAccessibilityNextContents:';
    function accessibilityNextContents: NSArray; message 'accessibilityNextContents';
    procedure setAccessibilityNumberOfCharacters(newValue: NSInteger); message 'setAccessibilityNumberOfCharacters:';
    function accessibilityNumberOfCharacters: NSInteger; message 'accessibilityNumberOfCharacters';
    procedure setAccessibilityOrderedByRow(newValue: ObjCBOOL); message 'setAccessibilityOrderedByRow:';
    function isAccessibilityOrderedByRow: ObjCBOOL; message 'isAccessibilityOrderedByRow';
    procedure setAccessibilityOrientation(newValue: NSAccessibilityOrientation); message 'setAccessibilityOrientation:';
    function accessibilityOrientation: NSAccessibilityOrientation; message 'accessibilityOrientation';
    procedure setAccessibilityOverflowButton(newValue: id); message 'setAccessibilityOverflowButton:';
    function accessibilityOverflowButton: id; message 'accessibilityOverflowButton';
    function accessibilityParent: id; message 'accessibilityParent';
    function accessibilityPerformCancel: ObjCBOOL; message 'accessibilityPerformCancel'; { available in 10_10 }
    function accessibilityPerformConfirm: ObjCBOOL; message 'accessibilityPerformConfirm'; { available in 10_10 }
    function accessibilityPerformDecrement: ObjCBOOL; message 'accessibilityPerformDecrement'; { available in 10_10 }
    function accessibilityPerformDelete: ObjCBOOL; message 'accessibilityPerformDelete'; { available in 10_10 }
    function accessibilityPerformIncrement: ObjCBOOL; message 'accessibilityPerformIncrement'; { available in 10_10 }
    function accessibilityPerformPick: ObjCBOOL; message 'accessibilityPerformPick'; { available in 10_10 }
    function accessibilityPerformPress: ObjCBOOL; message 'accessibilityPerformPress'; { available in 10_10 }
    function accessibilityPerformRaise: ObjCBOOL; message 'accessibilityPerformRaise'; { available in 10_10 }
    function accessibilityPerformShowAlternateUI: ObjCBOOL; message 'accessibilityPerformShowAlternateUI'; { available in 10_10 }
    function accessibilityPerformShowDefaultUI: ObjCBOOL; message 'accessibilityPerformShowDefaultUI'; { available in 10_10 }
    function accessibilityPerformShowMenu: ObjCBOOL; message 'accessibilityPerformShowMenu'; { available in 10_10 }
    procedure setAccessibilityPlaceholderValue(newValue: NSString); message 'setAccessibilityPlaceholderValue:';
    function accessibilityPlaceholderValue: NSString; message 'accessibilityPlaceholderValue';
    procedure setAccessibilityPreviousContents(newValue: NSArray); message 'setAccessibilityPreviousContents:';
    function accessibilityPreviousContents: NSArray; message 'accessibilityPreviousContents';
    procedure setAccessibilityProtectedContent(newValue: ObjCBOOL); message 'setAccessibilityProtectedContent:';
    function isAccessibilityProtectedContent: ObjCBOOL; message 'isAccessibilityProtectedContent';
    procedure setAccessibilityProxy(newValue: id); message 'setAccessibilityProxy:';
    function accessibilityProxy: id; message 'accessibilityProxy';
    function accessibilityRangeForIndex (index: NSInteger): NSRange; message 'accessibilityRangeForIndex:'; { available in 10_10 }
    function accessibilityRangeForLine (line: NSInteger): NSRange; message 'accessibilityRangeForLine:'; { available in 10_10 }
    function accessibilityRangeForPosition (point: NSPoint): NSRange; message 'accessibilityRangeForPosition:'; { available in 10_10 }
    procedure setAccessibilityRole(newValue: NSString); message 'setAccessibilityRole:';
    function accessibilityRole: NSString; message 'accessibilityRole';
    procedure setAccessibilityRoleDescription(newValue: NSString); message 'setAccessibilityRoleDescription:';
    function accessibilityRoleDescription: NSString; message 'accessibilityRoleDescription';
    procedure setAccessibilityRowCount(newValue: NSInteger); message 'setAccessibilityRowCount:';
    function accessibilityRowCount: NSInteger; message 'accessibilityRowCount';
    procedure setAccessibilityRowHeaderUIElements(newValue: NSArray); message 'setAccessibilityRowHeaderUIElements:';
    function accessibilityRowHeaderUIElements: NSArray; message 'accessibilityRowHeaderUIElements';
    procedure setAccessibilityRowIndexRange(newValue: NSRange); message 'setAccessibilityRowIndexRange:';
    function accessibilityRowIndexRange: NSRange; message 'accessibilityRowIndexRange';
    procedure setAccessibilityRows(newValue: NSArray); message 'setAccessibilityRows:';
    function accessibilityRows: NSArray; message 'accessibilityRows';
    function accessibilityRTFForRange (range: NSRange): NSData; message 'accessibilityRTFForRange:'; { available in 10_10 }
    procedure setAccessibilityRulerMarkerType(newValue: NSAccessibilityRulerMarkerType); message 'setAccessibilityRulerMarkerType:';
    function accessibilityRulerMarkerType: NSAccessibilityRulerMarkerType; message 'accessibilityRulerMarkerType';
    function accessibilityScreenPointForLayoutPoint (point: NSPoint): NSPoint; message 'accessibilityScreenPointForLayoutPoint:'; { available in 10_10 }
    function accessibilityScreenSizeForLayoutSize (size: NSSize): NSSize; message 'accessibilityScreenSizeForLayoutSize:'; { available in 10_10 }
    procedure setAccessibilitySearchButton(newValue: id); message 'setAccessibilitySearchButton:';
    function accessibilitySearchButton: id; message 'accessibilitySearchButton';
    procedure setAccessibilitySearchMenu(newValue: id); message 'setAccessibilitySearchMenu:';
    function accessibilitySearchMenu: id; message 'accessibilitySearchMenu';
    procedure setAccessibilitySelected(newValue: ObjCBOOL); message 'setAccessibilitySelected:';
    function isAccessibilitySelected: ObjCBOOL; message 'isAccessibilitySelected';
    procedure setAccessibilitySelectedCells(newValue: NSArray); message 'setAccessibilitySelectedCells:';
    function accessibilitySelectedCells: NSArray; message 'accessibilitySelectedCells';
    procedure setAccessibilitySelectedChildren(newValue: NSArray); message 'setAccessibilitySelectedChildren:';
    function accessibilitySelectedChildren: NSArray; message 'accessibilitySelectedChildren';
    procedure setAccessibilitySelectedColumns(newValue: NSArray); message 'setAccessibilitySelectedColumns:';
    function accessibilitySelectedColumns: NSArray; message 'accessibilitySelectedColumns';
    procedure setAccessibilitySelectedRows(newValue: NSArray); message 'setAccessibilitySelectedRows:';
    function accessibilitySelectedRows: NSArray; message 'accessibilitySelectedRows';
    procedure setAccessibilitySelectedText(newValue: NSString); message 'setAccessibilitySelectedText:';
    function accessibilitySelectedText: NSString; message 'accessibilitySelectedText';
    procedure setAccessibilitySelectedTextRange(newValue: NSRange); message 'setAccessibilitySelectedTextRange:';
    function accessibilitySelectedTextRange: NSRange; message 'accessibilitySelectedTextRange';
    procedure setAccessibilitySelectedTextRanges(newValue: NSArray); message 'setAccessibilitySelectedTextRanges:';
    function accessibilitySelectedTextRanges: NSArray; message 'accessibilitySelectedTextRanges';
    procedure setAccessibilityServesAsTitleForUIElements(newValue: NSArray); message 'setAccessibilityServesAsTitleForUIElements:';
    function accessibilityServesAsTitleForUIElements: NSArray; message 'accessibilityServesAsTitleForUIElements';
    procedure setAccessibilitySharedCharacterRange(newValue: NSRange); message 'setAccessibilitySharedCharacterRange:';
    function accessibilitySharedCharacterRange: NSRange; message 'accessibilitySharedCharacterRange';
    procedure setAccessibilitySharedFocusElements(newValue: NSArray); message 'setAccessibilitySharedFocusElements:';
    function accessibilitySharedFocusElements: NSArray; message 'accessibilitySharedFocusElements';
    procedure setAccessibilitySharedTextUIElements(newValue: NSArray); message 'setAccessibilitySharedTextUIElements:';
    function accessibilitySharedTextUIElements: NSArray; message 'accessibilitySharedTextUIElements';
    procedure setAccessibilityShownMenu(newValue: id); message 'setAccessibilityShownMenu:';
    function accessibilityShownMenu: id; message 'accessibilityShownMenu';
    procedure setAccessibilitySortDirection(newValue: NSAccessibilitySortDirection); message 'setAccessibilitySortDirection:';
    function accessibilitySortDirection: NSAccessibilitySortDirection; message 'accessibilitySortDirection';
    procedure setAccessibilitySplitters(newValue: NSArray); message 'setAccessibilitySplitters:';
    function accessibilitySplitters: NSArray; message 'accessibilitySplitters';
    function accessibilityStringForRange (range: NSRange): NSString; message 'accessibilityStringForRange:'; { available in 10_10 }
    function accessibilityStyleRangeForIndex (index: NSInteger): NSRange; message 'accessibilityStyleRangeForIndex:'; { available in 10_10 }
    procedure setAccessibilitySubrole(newValue: NSString); message 'setAccessibilitySubrole:';
    function accessibilitySubrole: NSString; message 'accessibilitySubrole';
    procedure setAccessibilityTabs(newValue: NSArray); message 'setAccessibilityTabs:';
    function accessibilityTabs: NSArray; message 'accessibilityTabs';
    procedure setAccessibilityTitle(newValue: NSString); message 'setAccessibilityTitle:';
    function accessibilityTitle: NSString; message 'accessibilityTitle';
    procedure setAccessibilityTitleUIElement(newValue: id); message 'setAccessibilityTitleUIElement:';
    function accessibilityTitleUIElement: id; message 'accessibilityTitleUIElement';
    procedure setAccessibilityToolbarButton(newValue: id); message 'setAccessibilityToolbarButton:';
    function accessibilityToolbarButton: id; message 'accessibilityToolbarButton';
    procedure setAccessibilityTopLevelUIElement(newValue: id); message 'setAccessibilityTopLevelUIElement:';
    function accessibilityTopLevelUIElement: id; message 'accessibilityTopLevelUIElement';
    procedure setAccessibilityUnitDescription(newValue: NSString); message 'setAccessibilityUnitDescription:';
    function accessibilityUnitDescription: NSString; message 'accessibilityUnitDescription';
    procedure setAccessibilityUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityUnits:';
    function accessibilityUnits: NSAccessibilityUnits; message 'accessibilityUnits';
    procedure setAccessibilityURL(newValue: NSURL); message 'setAccessibilityURL:';
    function accessibilityURL: NSURL; message 'accessibilityURL';
    procedure setAccessibilityValue(newValue: id); message 'setAccessibilityValue:';
    function accessibilityValue: id; message 'accessibilityValue';
    procedure setAccessibilityValueDescription(newValue: NSString); message 'setAccessibilityValueDescription:';
    function accessibilityValueDescription: NSString; message 'accessibilityValueDescription';
    procedure setAccessibilityVerticalScrollBar(newValue: id); message 'setAccessibilityVerticalScrollBar:';
    function accessibilityVerticalScrollBar: id; message 'accessibilityVerticalScrollBar';
    procedure setAccessibilityVerticalUnitDescription(newValue: NSString); message 'setAccessibilityVerticalUnitDescription:';
    function accessibilityVerticalUnitDescription: NSString; message 'accessibilityVerticalUnitDescription';
    procedure setAccessibilityVerticalUnits(newValue: NSAccessibilityUnits); message 'setAccessibilityVerticalUnits:';
    function accessibilityVerticalUnits: NSAccessibilityUnits; message 'accessibilityVerticalUnits';
    procedure setAccessibilityVisibleCells(newValue: NSArray); message 'setAccessibilityVisibleCells:';
    function accessibilityVisibleCells: NSArray; message 'accessibilityVisibleCells';
    procedure setAccessibilityVisibleCharacterRange(newValue: NSRange); message 'setAccessibilityVisibleCharacterRange:';
    function accessibilityVisibleCharacterRange: NSRange; message 'accessibilityVisibleCharacterRange';
    procedure setAccessibilityVisibleChildren(newValue: NSArray); message 'setAccessibilityVisibleChildren:';
    function accessibilityVisibleChildren: NSArray; message 'accessibilityVisibleChildren';
    procedure setAccessibilityVisibleColumns(newValue: NSArray); message 'setAccessibilityVisibleColumns:';
    function accessibilityVisibleColumns: NSArray; message 'accessibilityVisibleColumns';
    procedure setAccessibilityVisibleRows(newValue: NSArray); message 'setAccessibilityVisibleRows:';
    function accessibilityVisibleRows: NSArray; message 'accessibilityVisibleRows';
    procedure setAccessibilityWarningValue(newValue: id); message 'setAccessibilityWarningValue:';
    function accessibilityWarningValue: id; message 'accessibilityWarningValue';
    procedure setAccessibilityWindow(newValue: id); message 'setAccessibilityWindow:';
    function accessibilityWindow: id; message 'accessibilityWindow';
    procedure setAccessibilityWindows(newValue: NSArray); message 'setAccessibilityWindows:';
    function accessibilityWindows: NSArray; message 'accessibilityWindows';
    procedure setAccessibilityZoomButton(newValue: id); message 'setAccessibilityZoomButton:';
    function accessibilityZoomButton: id; message 'accessibilityZoomButton';
    function animationForKey (key: NSString): id; message 'animationForKey:'; { available in 10_5 }
    function animations: NSDictionary; message 'animations'; { available in 10_5 }
    function animator: instancetype; message 'animator'; { available in 10_5 }
    procedure setAppearance(newValue: NSAppearance); message 'setAppearance:';
    function appearance: NSAppearance; message 'appearance';
    procedure concludeDragOperation (sender: NSDraggingInfoProtocol); message 'concludeDragOperation:';
    class function defaultAnimationForKey (key: NSString): id; message 'defaultAnimationForKey:'; { available in 10_5 }
    procedure draggingEnded (sender: NSDraggingInfoProtocol); message 'draggingEnded:';
    function draggingEntered (sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingEntered:';
    procedure draggingExited (sender: NSDraggingInfoProtocol); message 'draggingExited:';
    function draggingUpdated (sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingUpdated:';
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance';
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    function isAccessibilitySelectorAllowed (selector: SEL): ObjCBOOL; message 'isAccessibilitySelectorAllowed:'; { available in 10_10 }
    function performDragOperation (sender: NSDraggingInfoProtocol): ObjCBOOL; message 'performDragOperation:';
    function prepareForDragOperation (sender: NSDraggingInfoProtocol): ObjCBOOL; message 'prepareForDragOperation:';
    procedure setAnimations (animations_: NSDictionary); message 'setAnimations:'; { available in 10_5 }
    procedure updateDraggingItemsForDrag (sender: NSDraggingInfoProtocol); message 'updateDraggingItemsForDrag:'; { available in 10_7 }
    function wantsPeriodicDraggingUpdates: ObjCBOOL; message 'wantsPeriodicDraggingUpdates';
  end;


type
  NSLayerDelegateContentsScaleUpdating = objccategory external (NSObject)
    function layer_shouldInheritContentsScale_fromWindow (layer: CALayer; newScale: CGFloat; window: NSWindow): ObjCBOOL; message 'layer:shouldInheritContentsScale:fromWindow:'; { available in 10_7 }
  end;

type
  NSToolTipOwner = objccategory external (NSObject)
    function view_stringForToolTip_point_userData (view: NSView; tag: NSToolTipTag; point: NSPoint; data: pointer): NSString; message 'view:stringForToolTip:point:userData:';
  end;

type
  NSView_NSKeyboardUI = objccategory external name 'NSKeyboardUI' (NSView)
    procedure setNextKeyView(newValue: NSView); message 'setNextKeyView:';
    function nextKeyView: NSView; message 'nextKeyView';
    function previousKeyView: NSView; message 'previousKeyView';
    function nextValidKeyView: NSView; message 'nextValidKeyView';
    function previousValidKeyView: NSView; message 'previousValidKeyView';
    function canBecomeKeyView: ObjCBOOL; message 'canBecomeKeyView';
    procedure setKeyboardFocusRingNeedsDisplayInRect (rect: NSRect); message 'setKeyboardFocusRingNeedsDisplayInRect:';
    procedure setFocusRingType(newValue: NSFocusRingType); message 'setFocusRingType:';
    function focusRingType: NSFocusRingType; message 'focusRingType';
    class function defaultFocusRingType: NSFocusRingType; message 'defaultFocusRingType';
    procedure drawFocusRingMask; message 'drawFocusRingMask'; { available in 10_7 }
    function focusRingMaskBounds: NSRect; message 'focusRingMaskBounds';
    procedure noteFocusRingMaskChanged; message 'noteFocusRingMaskChanged'; { available in 10_7 }
  end;

type
  NSPrinting = objccategory external (NSView)
    procedure writeEPSInsideRect_toPasteboard (rect: NSRect; pasteboard: NSPasteboard); message 'writeEPSInsideRect:toPasteboard:';
    function dataWithEPSInsideRect (rect: NSRect): NSData; message 'dataWithEPSInsideRect:';
    procedure writePDFInsideRect_toPasteboard (rect: NSRect; pasteboard: NSPasteboard); message 'writePDFInsideRect:toPasteboard:';
    function dataWithPDFInsideRect (rect: NSRect): NSData; message 'dataWithPDFInsideRect:';
    procedure print (sender: id); message 'print:';
    function knowsPageRange (range: NSRangePointer): ObjCBOOL; message 'knowsPageRange:';
    function heightAdjustLimit: CGFloat; message 'heightAdjustLimit';
    function widthAdjustLimit: CGFloat; message 'widthAdjustLimit';
    procedure adjustPageWidthNew_left_right_limit (newRight: CGFloatPtr; oldLeft: CGFloat; oldRight: CGFloat; rightLimit: CGFloat); message 'adjustPageWidthNew:left:right:limit:';
    procedure adjustPageHeightNew_top_bottom_limit (newBottom: CGFloatPtr; oldTop: CGFloat; oldBottom: CGFloat; bottomLimit: CGFloat); message 'adjustPageHeightNew:top:bottom:limit:';
    function rectForPage (page: NSInteger): NSRect; message 'rectForPage:';
    function locationOfPrintRect (aRect: NSRect): NSPoint; message 'locationOfPrintRect:';
    procedure drawPageBorderWithSize (borderSize: NSSize); message 'drawPageBorderWithSize:';
    function pageHeader: NSAttributedString; message 'pageHeader';
    function pageFooter: NSAttributedString; message 'pageFooter';
    procedure drawSheetBorderWithSize (borderSize: NSSize); message 'drawSheetBorderWithSize:';
    function printJobTitle: NSString; message 'printJobTitle';
    procedure beginDocument; message 'beginDocument';
    procedure endDocument; message 'endDocument';
    procedure beginPageInRect_atPlacement (aRect: NSRect; location: NSPoint); message 'beginPageInRect:atPlacement:';
    procedure endPage; message 'endPage';
  end;

type
  NSView_NSDrag = objccategory external name 'NSDrag' (NSView)
    function beginDraggingSessionWithItems_event_source (items: NSArray; event: NSEvent; source: NSDraggingSourceProtocol): NSDraggingSession; message 'beginDraggingSessionWithItems:event:source:'; { available in 10_7 }
    procedure dragImage_at_offset_event_pasteboard_source_slideBack (anImage: NSImage; viewLocation: NSPoint; initialOffset: NSSize; event: NSEvent; pboard: NSPasteboard; sourceObj: id; slideFlag: ObjCBOOL); message 'dragImage:at:offset:event:pasteboard:source:slideBack:'; deprecated 'in 10_0, 10_7, "Use -beginDraggingSessionWithItems:event:source: instead"';
    function registeredDraggedTypes: NSArray; message 'registeredDraggedTypes';
    procedure registerForDraggedTypes (newTypes: NSArray); message 'registerForDraggedTypes:';
    procedure unregisterDraggedTypes; message 'unregisterDraggedTypes';
    function dragFile_fromRect_slideBack_event (filename: NSString; rect: NSRect; aFlag: ObjCBOOL; event: NSEvent): ObjCBOOL; message 'dragFile:fromRect:slideBack:event:';
    function dragPromisedFilesOfTypes_fromRect_source_slideBack_event (typeArray: NSArray; rect: NSRect; sourceObject: id; aFlag: ObjCBOOL; event: NSEvent): ObjCBOOL; message 'dragPromisedFilesOfTypes:fromRect:source:slideBack:event:';
  end;

type
  NSFullScreenMode = objccategory external (NSView)
    function enterFullScreenMode_withOptions (screen: NSScreen; options: NSDictionary): ObjCBOOL; message 'enterFullScreenMode:withOptions:'; { available in 10_5 }
    procedure exitFullScreenModeWithOptions (options: NSDictionary); message 'exitFullScreenModeWithOptions:'; { available in 10_5 }
    function isInFullScreenMode: ObjCBOOL; message 'isInFullScreenMode';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFullScreenModeAllScreens: NSString { available in 10_5 }; cvar; external;
  NSFullScreenModeSetting: NSString { available in 10_5 }; cvar; external;
  NSFullScreenModeWindowLevel: NSString { available in 10_5 }; cvar; external;
  NSFullScreenModeApplicationPresentationOptions: NSString { available in 10_5 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSDefinition = objccategory external (NSView)
    procedure showDefinitionForAttributedString_atPoint (attrString: NSAttributedString; textBaselineOrigin: NSPoint); message 'showDefinitionForAttributedString:atPoint:'; { available in 10_6 }
    procedure showDefinitionForAttributedString_range_options_baselineOriginProvider (attrString: NSAttributedString; targetRange: NSRange; options: NSDictionary; originProvider: OpaqueCBlock); message 'showDefinitionForAttributedString:range:options:baselineOriginProvider:'; { available in 10_6 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSDefinitionPresentationTypeKey: NSString { available in 10_6 }; cvar; external;
  NSDefinitionPresentationTypeOverlay: NSString { available in 10_6 }; cvar; external;
  NSDefinitionPresentationTypeDictionaryApplication: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSFindIndicator = objccategory external (NSView)
    function isDrawingFindIndicator: ObjCBOOL; message 'isDrawingFindIndicator';
  end;

type
  NSView_NSGestureRecognizer = objccategory external name 'NSGestureRecognizer' (NSView)
    procedure setGestureRecognizers(newValue: NSArray); message 'setGestureRecognizers:';
    function gestureRecognizers: NSArray; message 'gestureRecognizers';
    procedure addGestureRecognizer (gestureRecognizer: NSGestureRecognizer); message 'addGestureRecognizer:'; { available in 10_10 }
    procedure removeGestureRecognizer (gestureRecognizer: NSGestureRecognizer); message 'removeGestureRecognizer:'; { available in 10_10 }
  end;

type
  NSView_NSDeprecated = objccategory external name 'NSDeprecated' (NSView)
    function performMnemonic (theString: NSString): ObjCBOOL; message 'performMnemonic:'; deprecated 'in 10_0, 10_8';
    function shouldDrawColor: ObjCBOOL; message 'shouldDrawColor'; deprecated 'in 10_0, 10_10';
    function gState: NSInteger; message 'gState'; deprecated 'in 10_0, 10_10';
    procedure allocateGState; message 'allocateGState'; deprecated 'in 10_0, 10_10';
    procedure releaseGState; message 'releaseGState'; deprecated 'in 10_0, 10_10';
    procedure setUpGState; message 'setUpGState'; deprecated 'in 10_0, 10_10';
    procedure renewGState; message 'renewGState'; deprecated 'in 10_0, 10_10';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSViewFrameDidChangeNotification: NSString; cvar; external;
  NSViewFocusDidChangeNotification: NSString; cvar; external;
  NSViewBoundsDidChangeNotification: NSString; cvar; external;
  NSViewGlobalFrameDidChangeNotification: NSString; cvar; external;
  NSViewDidUpdateTrackingAreasNotification: NSString { available in 10_5 }; cvar; external;
{$endif}

