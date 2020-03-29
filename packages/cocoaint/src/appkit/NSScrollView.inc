{ Parsed from AppKit.framework NSScrollView.h }


{$ifdef TYPES}
type
  NSScrollViewPtr = ^NSScrollView;
{$endif}

{$ifdef TYPES}
type
  NSScrollElasticity = NSInteger;
  NSScrollElasticityPtr = ^NSScrollElasticity;

const
  NSScrollElasticityAutomatic = 0;
  NSScrollElasticityNone = 1;
  NSScrollElasticityAllowed = 2;

type
  __SFlags = bitpacked record
    case byte of
      0: (_anonBitField___SFlags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
        vScrollerRequired: 0..1;
        hScrollerRequired: 0..1;
        vScrollerStatus: 0..1;
        hScrollerStatus: 0..1;
        noDynamicScrolling: 0..1;
        borderType: 0..((1 shl 2)-1);
        oldRulerInstalled: 0..1;
        showRulers: 0..1;
        hasHorizontalRuler: 0..1;
        hasVerticalRuler: 0..1;
        needsTile: 0..1;
        doesNotDrawBackground: 0..1;
        skipRemoveSuperviewCheck: 0..1;
        focusRingNeedsRedisplay: 0..1;
        hasCustomLineBorderColor: 0..1;
        autohidesScrollers: 0..1;
        autoforwardsScrollWheelEvents: 0..1;
        findBarVisible: 0..1;
        unused: 0..1;
        vContentElasticity: 0..((1 shl 2)-1);
        hContentElasticity: 0..((1 shl 2)-1);
        predominantAxisScrolling: 0..1;
        findBarPosition: 0..((1 shl 2)-1);
        registeredForWindowWillClose: 0..1;
        unarchiving: 0..1;
        RESERVED: 0..((1 shl 3)-1);
        {$else}
        RESERVED_: 0..((1 shl 3)-1);
        unarchiving_: 0..1;
        registeredForWindowWillClose_: 0..1;
        findBarPosition_: 0..((1 shl 2)-1);
        predominantAxisScrolling_: 0..1;
        hContentElasticity_: 0..((1 shl 2)-1);
        vContentElasticity_: 0..((1 shl 2)-1);
        unused_: 0..1;
        findBarVisible_: 0..1;
        autoforwardsScrollWheelEvents_: 0..1;
        autohidesScrollers_: 0..1;
        hasCustomLineBorderColor_: 0..1;
        focusRingNeedsRedisplay_: 0..1;
        skipRemoveSuperviewCheck_: 0..1;
        doesNotDrawBackground_: 0..1;
        needsTile_: 0..1;
        hasVerticalRuler_: 0..1;
        hasHorizontalRuler_: 0..1;
        showRulers_: 0..1;
        oldRulerInstalled_: 0..1;
        borderType_: 0..((1 shl 2)-1);
        noDynamicScrolling_: 0..1;
        hScrollerStatus_: 0..1;
        vScrollerStatus_: 0..1;
        hScrollerRequired_: 0..1;
        vScrollerRequired_: 0..1;
        {$endif}
      );
    end;
type
  _SFlags = __SFlags;
type
  _SFlagsPtr = ^__SFlags;
type
  __SFlagsPtr = ^__SFlags;
{$endif}

{$ifdef CLASSES}

type
  NSScrollView = objcclass external (NSView, NSTextFinderBarContainerProtocol)
  private
    _vScroller: NSScroller;
    _hScroller: NSScroller;
    _contentView: NSClipView;
    _headerClipView: NSClipView;
    _cornerView: NSView;
    _ruler: id;
    _sFlags: _SFlags;
    _extraIvars: pointer;
    _horizontalRuler: NSRulerView;
    _verticalRuler: NSRulerView;
  public
    function initWithFrame (frameRect: NSRect): instancetype; message 'initWithFrame:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    class function frameSizeForContentSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle (cSize: NSSize; horizontalScrollerClass: pobjc_class; verticalScrollerClass: pobjc_class; aType: NSBorderType; controlSize: NSControlSize; scrollerStyle: NSScrollerStyle): NSSize; message 'frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:'; { available in 10_7 }
    class function contentSizeForFrameSize_horizontalScrollerClass_verticalScrollerClass_borderType_controlSize_scrollerStyle (fSize: NSSize; horizontalScrollerClass: pobjc_class; verticalScrollerClass: pobjc_class; aType: NSBorderType; controlSize: NSControlSize; scrollerStyle: NSScrollerStyle): NSSize; message 'contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle:'; { available in 10_7 }
    class function frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType (cSize: NSSize; hFlag: ObjCBOOL; vFlag: ObjCBOOL; aType: NSBorderType): NSSize; message 'frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:'; deprecated 'in 10_0, 10_7, "Use +frameSizeForContentSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle: instead"';
    class function contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType (fSize: NSSize; hFlag: ObjCBOOL; vFlag: ObjCBOOL; aType: NSBorderType): NSSize; message 'contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:'; deprecated 'in 10_0, 10_7, "+contentSizeForFrameSize:horizontalScrollerClass:verticalScrollerClass:borderType:controlSize:scrollerStyle: instead"';
    function documentVisibleRect: NSRect; message 'documentVisibleRect';
    function contentSize: NSSize; message 'contentSize';
    procedure setDocumentView(newValue: NSView); message 'setDocumentView:';
    function documentView: NSView; message 'documentView';
    procedure setContentView(newValue: NSClipView); message 'setContentView:';
    function contentView: NSClipView; message 'contentView';
    procedure setDocumentCursor(newValue: NSCursor); message 'setDocumentCursor:';
    function documentCursor: NSCursor; message 'documentCursor';
    procedure setBorderType(newValue: NSBorderType); message 'setBorderType:';
    function borderType: NSBorderType; message 'borderType';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setHasVerticalScroller(newValue: ObjCBOOL); message 'setHasVerticalScroller:';
    function hasVerticalScroller: ObjCBOOL; message 'hasVerticalScroller';
    procedure setHasHorizontalScroller(newValue: ObjCBOOL); message 'setHasHorizontalScroller:';
    function hasHorizontalScroller: ObjCBOOL; message 'hasHorizontalScroller';
    procedure setVerticalScroller(newValue: NSScroller); message 'setVerticalScroller:';
    function verticalScroller: NSScroller; message 'verticalScroller';
    procedure setHorizontalScroller(newValue: NSScroller); message 'setHorizontalScroller:';
    function horizontalScroller: NSScroller; message 'horizontalScroller';
    procedure setAutohidesScrollers(newValue: ObjCBOOL); message 'setAutohidesScrollers:';
    function autohidesScrollers: ObjCBOOL; message 'autohidesScrollers';
    procedure setHorizontalLineScroll(newValue: CGFloat); message 'setHorizontalLineScroll:';
    function horizontalLineScroll: CGFloat; message 'horizontalLineScroll';
    procedure setVerticalLineScroll(newValue: CGFloat); message 'setVerticalLineScroll:';
    function verticalLineScroll: CGFloat; message 'verticalLineScroll';
    procedure setLineScroll(newValue: CGFloat); message 'setLineScroll:';
    function lineScroll: CGFloat; message 'lineScroll';
    procedure setHorizontalPageScroll(newValue: CGFloat); message 'setHorizontalPageScroll:';
    function horizontalPageScroll: CGFloat; message 'horizontalPageScroll';
    procedure setVerticalPageScroll(newValue: CGFloat); message 'setVerticalPageScroll:';
    function verticalPageScroll: CGFloat; message 'verticalPageScroll';
    procedure setPageScroll(newValue: CGFloat); message 'setPageScroll:';
    function pageScroll: CGFloat; message 'pageScroll';
    procedure setScrollsDynamically(newValue: ObjCBOOL); message 'setScrollsDynamically:';
    function scrollsDynamically: ObjCBOOL; message 'scrollsDynamically';
    procedure tile; message 'tile';
    procedure reflectScrolledClipView (cView: NSClipView); message 'reflectScrolledClipView:';
    procedure scrollWheel (theEvent: NSEvent); message 'scrollWheel:';
    procedure setScrollerStyle(newValue: NSScrollerStyle); message 'setScrollerStyle:';
    function scrollerStyle: NSScrollerStyle; message 'scrollerStyle';
    procedure setScrollerKnobStyle(newValue: NSScrollerKnobStyle); message 'setScrollerKnobStyle:';
    function scrollerKnobStyle: NSScrollerKnobStyle; message 'scrollerKnobStyle';
    procedure flashScrollers; message 'flashScrollers'; { available in 10_7 }
    procedure setHorizontalScrollElasticity(newValue: NSScrollElasticity); message 'setHorizontalScrollElasticity:';
    function horizontalScrollElasticity: NSScrollElasticity; message 'horizontalScrollElasticity';
    procedure setVerticalScrollElasticity(newValue: NSScrollElasticity); message 'setVerticalScrollElasticity:';
    function verticalScrollElasticity: NSScrollElasticity; message 'verticalScrollElasticity';
    procedure setUsesPredominantAxisScrolling(newValue: ObjCBOOL); message 'setUsesPredominantAxisScrolling:';
    function usesPredominantAxisScrolling: ObjCBOOL; message 'usesPredominantAxisScrolling';
    procedure setAllowsMagnification(newValue: ObjCBOOL); message 'setAllowsMagnification:';
    function allowsMagnification: ObjCBOOL; message 'allowsMagnification';
    procedure setMagnification(newValue: CGFloat); message 'setMagnification:';
    function magnification: CGFloat; message 'magnification';
    procedure setMaxMagnification(newValue: CGFloat); message 'setMaxMagnification:';
    function maxMagnification: CGFloat; message 'maxMagnification';
    procedure setMinMagnification(newValue: CGFloat); message 'setMinMagnification:';
    function minMagnification: CGFloat; message 'minMagnification';
    procedure magnifyToFitRect (rect: NSRect); message 'magnifyToFitRect:'; { available in 10_8 }
    procedure setMagnification_centeredAtPoint (magnification_: CGFloat; point: NSPoint); message 'setMagnification:centeredAtPoint:'; { available in 10_8 }
    procedure addFloatingSubview_forAxis (view: NSView; axis: NSEventGestureAxis); message 'addFloatingSubview:forAxis:'; { available in 10_9 }
    procedure setAutomaticallyAdjustsContentInsets(newValue: ObjCBOOL); message 'setAutomaticallyAdjustsContentInsets:';
    function automaticallyAdjustsContentInsets: ObjCBOOL; message 'automaticallyAdjustsContentInsets';
    procedure setContentInsets(newValue: NSEdgeInsets); message 'setContentInsets:';
    function contentInsets: NSEdgeInsets; message 'contentInsets';
    procedure setScrollerInsets(newValue: NSEdgeInsets); message 'setScrollerInsets:';
    function scrollerInsets: NSEdgeInsets; message 'scrollerInsets';

    { Adopted protocols }
    procedure setFindBarView(newValue: NSView); message 'setFindBarView:';
    function findBarView: NSView; message 'findBarView';
    procedure findBarViewDidChangeHeight; message 'findBarViewDidChangeHeight';
    procedure setFindBarVisible(newValue: ObjCBOOL); message 'setFindBarVisible:';
    function isFindBarVisible: ObjCBOOL; message 'isFindBarVisible';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSScrollViewWillStartLiveMagnifyNotification: NSString { available in 10_8 }; cvar; external;
  NSScrollViewDidEndLiveMagnifyNotification: NSString { available in 10_8 }; cvar; external;
  NSScrollViewWillStartLiveScrollNotification: NSString { available in 10_9 }; cvar; external;
  NSScrollViewDidLiveScrollNotification: NSString { available in 10_9 }; cvar; external;
  NSScrollViewDidEndLiveScrollNotification: NSString { available in 10_9 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSRulerSupport = objccategory external (NSScrollView)
    class procedure setRulerViewClass (rulerViewClass: pobjc_class); message 'setRulerViewClass:';
    class function rulerViewClass: pobjc_class; message 'rulerViewClass';
    procedure setRulersVisible(newValue: ObjCBOOL); message 'setRulersVisible:';
    function rulersVisible: ObjCBOOL; message 'rulersVisible';
    procedure setHasHorizontalRuler(newValue: ObjCBOOL); message 'setHasHorizontalRuler:';
    function hasHorizontalRuler: ObjCBOOL; message 'hasHorizontalRuler';
    procedure setHasVerticalRuler(newValue: ObjCBOOL); message 'setHasVerticalRuler:';
    function hasVerticalRuler: ObjCBOOL; message 'hasVerticalRuler';
    procedure setHorizontalRulerView(newValue: NSRulerView); message 'setHorizontalRulerView:';
    function horizontalRulerView: NSRulerView; message 'horizontalRulerView';
    procedure setVerticalRulerView(newValue: NSRulerView); message 'setVerticalRulerView:';
    function verticalRulerView: NSRulerView; message 'verticalRulerView';
  end;
{$endif}

{$ifdef TYPES}
type
  NSScrollViewFindBarPosition = NSInteger;
  NSScrollViewFindBarPositionPtr = ^NSScrollViewFindBarPosition;

const
  NSScrollViewFindBarPositionAboveHorizontalRuler = 0;
  NSScrollViewFindBarPositionAboveContent = 1;
  NSScrollViewFindBarPositionBelowContent = 2;
{$endif}

{$ifdef CLASSES}

type
  NSFindBarSupport = objccategory external (NSScrollView)
    procedure setFindBarPosition(newValue: NSScrollViewFindBarPosition); message 'setFindBarPosition:';
    function findBarPosition: NSScrollViewFindBarPosition; message 'findBarPosition';
  end;
{$endif}

