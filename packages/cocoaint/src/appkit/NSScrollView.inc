{ Parsed from Appkit.framework NSScrollView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCROLLVIEW_PAS_H}
{$define NSSCROLLVIEW_PAS_H}
type
  NSScrollViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCROLLVIEW_PAS_T}
{$define NSSCROLLVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCROLLVIEW_PAS_R}
{$define NSSCROLLVIEW_PAS_R}

{ Records }
type
  __SFlags = record
{$ifdef fpc_big_endian}
    vScrollerRequired: cuint;
    hScrollerRequired: cuint;
    vScrollerStatus: cuint;
    hScrollerStatus: cuint;
    noDynamicScrolling: cuint;
    borderType: NSBorderType;
    oldRulerInstalled: cuint;
    showRulers: cuint;
    hasHorizontalRuler: cuint;
    hasVerticalRuler: cuint;
    needsTile: cuint;
    doesNotDrawBackground: cuint;
    skipRemoveSuperviewCheck: cuint;
    focusRingNeedsRedisplay: cuint;
    hasCustomLineBorderColor: cuint;
    autohidesScrollers: cuint;
    autoforwardsScrollWheelEvents: cuint;
    RESERVED: cuint;
{$else}
    RESERVED: cuint;
    autoforwardsScrollWheelEvents: cuint;
    autohidesScrollers: cuint;
    hasCustomLineBorderColor: cuint;
    focusRingNeedsRedisplay: cuint;
    skipRemoveSuperviewCheck: cuint;
    doesNotDrawBackground: cuint;
    needsTile: cuint;
    hasVerticalRuler: cuint;
    hasHorizontalRuler: cuint;
    showRulers: cuint;
    oldRulerInstalled: cuint;
    borderType: NSBorderType;
    noDynamicScrolling: cuint;
    hScrollerStatus: cuint;
    vScrollerStatus: cuint;
    hScrollerRequired: cuint;
    vScrollerRequired: cuint;
{$endif}
  end;
_SFlags = __SFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCROLLVIEW_PAS_F}
{$define NSSCROLLVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCROLLVIEW_PAS_T}
{$define NSSCROLLVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScrollView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCROLLVIEW_PAS_C}
{$define NSSCROLLVIEW_PAS_C}

{ NSScrollView }
  NSScrollView = objcclass(NSView)
  private
    __vScroller: NSScroller;
    __hScroller: NSScroller;
    __contentView: NSClipView;
    __headerClipView: NSClipView;
    __cornerView: NSView;
    __ruler: id;
    __sFlags: _SFlags;
    __extraIvars: Pointer; {garbage collector: __strong }
    __horizontalRuler: NSRulerView;
    __verticalRuler: NSRulerView;
    
  public
    class function alloc: NSScrollView; message 'alloc';

    class function frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType(cSize: NSSize; hFlag: Boolean; vFlag: Boolean; aType: NSBorderType): NSSize; message 'frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:';
    class function contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType(fSize: NSSize; hFlag: Boolean; vFlag: Boolean; aType: NSBorderType): NSSize; message 'contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:';
    function documentVisibleRect: NSRect; message 'documentVisibleRect';
    function contentSize: NSSize; message 'contentSize';
    procedure setDocumentView(aView: NSView); message 'setDocumentView:';
    function documentView: id; message 'documentView';
    procedure setContentView(contentView_: NSClipView); message 'setContentView:';
    function contentView: NSClipView; message 'contentView';
    procedure setDocumentCursor(anObj: NSCursor); message 'setDocumentCursor:';
    function documentCursor: NSCursor; message 'documentCursor';
    procedure setBorderType(aType: NSBorderType); message 'setBorderType:';
    function borderType: NSBorderType; message 'borderType';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setHasVerticalScroller(flag: Boolean); message 'setHasVerticalScroller:';
    function hasVerticalScroller: Boolean; message 'hasVerticalScroller';
    procedure setHasHorizontalScroller(flag: Boolean); message 'setHasHorizontalScroller:';
    function hasHorizontalScroller: Boolean; message 'hasHorizontalScroller';
    procedure setVerticalScroller(anObject: NSScroller); message 'setVerticalScroller:';
    function verticalScroller: NSScroller; message 'verticalScroller';
    procedure setHorizontalScroller(anObject: NSScroller); message 'setHorizontalScroller:';
    function horizontalScroller: NSScroller; message 'horizontalScroller';
    function autohidesScrollers: Boolean; message 'autohidesScrollers';
    procedure setAutohidesScrollers(flag: Boolean); message 'setAutohidesScrollers:';
    procedure setHorizontalLineScroll(value: CGFloat); message 'setHorizontalLineScroll:';
    procedure setVerticalLineScroll(value: CGFloat); message 'setVerticalLineScroll:';
    procedure setLineScroll(value: CGFloat); message 'setLineScroll:';
    function horizontalLineScroll: CGFloat; message 'horizontalLineScroll';
    function verticalLineScroll: CGFloat; message 'verticalLineScroll';
    function lineScroll: CGFloat; message 'lineScroll';
    procedure setHorizontalPageScroll(value: CGFloat); message 'setHorizontalPageScroll:';
    procedure setVerticalPageScroll(value: CGFloat); message 'setVerticalPageScroll:';
    procedure setPageScroll(value: CGFloat); message 'setPageScroll:';
    function horizontalPageScroll: CGFloat; message 'horizontalPageScroll';
    function verticalPageScroll: CGFloat; message 'verticalPageScroll';
    function pageScroll: CGFloat; message 'pageScroll';
    procedure setScrollsDynamically(flag: Boolean); message 'setScrollsDynamically:';
    function scrollsDynamically: Boolean; message 'scrollsDynamically';
    procedure tile; message 'tile';
    procedure reflectScrolledClipView(cView: NSClipView); message 'reflectScrolledClipView:';
    procedure scrollWheel(theEvent: NSEvent); message 'scrollWheel:';

    { Category: NSRulerSupport }
    class procedure setRulerViewClass(rulerViewClass_: Pobjc_class); message 'setRulerViewClass:';
    class function rulerViewClass: Pobjc_class; message 'rulerViewClass';
    procedure setRulersVisible(flag: Boolean); message 'setRulersVisible:';
    function rulersVisible: Boolean; message 'rulersVisible';
    procedure setHasHorizontalRuler(flag: Boolean); message 'setHasHorizontalRuler:';
    function hasHorizontalRuler: Boolean; message 'hasHorizontalRuler';
    procedure setHasVerticalRuler(flag: Boolean); message 'setHasVerticalRuler:';
    function hasVerticalRuler: Boolean; message 'hasVerticalRuler';
    procedure setHorizontalRulerView(ruler: NSRulerView); message 'setHorizontalRulerView:';
    function horizontalRulerView: NSRulerView; message 'horizontalRulerView';
    procedure setVerticalRulerView(ruler: NSRulerView); message 'setVerticalRulerView:';
    function verticalRulerView: NSRulerView; message 'verticalRulerView';
  end; external;

{$endif}
{$endif}
