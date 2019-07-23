{ Parsed from Appkit.framework NSScrollView.h }

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
    case byte of
    0: (_anonbitfield_NSScrollView0: cuint);
    1: (data: bitpacked record
{$ifdef fpc_big_endian}
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
          RESERVED: 0..((1 shl 14)-1);
{$else}
          RESERVED: 0..((1 shl 14)-1);
          autoforwardsScrollWheelEvents: 0..1;
          autohidesScrollers: 0..1;
          hasCustomLineBorderColor: 0..1;
          focusRingNeedsRedisplay: 0..1;
          skipRemoveSuperviewCheck: 0..1;
          doesNotDrawBackground: 0..1;
          needsTile: 0..1;
          hasVerticalRuler: 0..1;
          hasHorizontalRuler: 0..1;
          showRulers: 0..1;
          oldRulerInstalled: 0..1;
          borderType: 0..((1 shl 2)-1);
          noDynamicScrolling: 0..1;
          hScrollerStatus: 0..1;
          vScrollerStatus: 0..1;
          hScrollerRequired: 0..1;
          vScrollerRequired: 0..1;
{$endif}
         end;
       );
  end;
_SFlags = __SFlags;
__SFlagsPtr = ^__SFlags;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCROLLVIEW_PAS_F}
{$define NSSCROLLVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCROLLVIEW_PAS_S}
{$define NSSCROLLVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScrollView = objcclass;
  NSScrollViewPointer = ^NSScrollView;
  NSScrollViewPtr = NSScrollViewPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCROLLVIEW_PAS_C}
{$define NSSCROLLVIEW_PAS_C}

{ NSScrollView }
  NSScrollView = objcclass external (NSView)
  private
    _vScroller: NSScroller;
    _hScroller: NSScroller;
    _contentView: NSClipView;
    _headerClipView: NSClipView;
    _cornerView: NSView;
    _ruler: id;
    _sFlags: _SFlags;
    _extraIvars: Pointer; {garbage collector: __strong }
    _horizontalRuler: NSRulerView;
    _verticalRuler: NSRulerView;
    
  public
    class function frameSizeForContentSize_hasHorizontalScroller_hasVerticalScroller_borderType(cSize: NSSize; hFlag: ObjCBOOL; vFlag: ObjCBOOL; aType: NSBorderType): NSSize; message 'frameSizeForContentSize:hasHorizontalScroller:hasVerticalScroller:borderType:';
    class function contentSizeForFrameSize_hasHorizontalScroller_hasVerticalScroller_borderType(fSize: NSSize; hFlag: ObjCBOOL; vFlag: ObjCBOOL; aType: NSBorderType): NSSize; message 'contentSizeForFrameSize:hasHorizontalScroller:hasVerticalScroller:borderType:';
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
    procedure setDrawsBackground(flag: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setHasVerticalScroller(flag: ObjCBOOL); message 'setHasVerticalScroller:';
    function hasVerticalScroller: ObjCBOOL; message 'hasVerticalScroller';
    procedure setHasHorizontalScroller(flag: ObjCBOOL); message 'setHasHorizontalScroller:';
    function hasHorizontalScroller: ObjCBOOL; message 'hasHorizontalScroller';
    procedure setVerticalScroller(anObject: NSScroller); message 'setVerticalScroller:';
    function verticalScroller: NSScroller; message 'verticalScroller';
    procedure setHorizontalScroller(anObject: NSScroller); message 'setHorizontalScroller:';
    function horizontalScroller: NSScroller; message 'horizontalScroller';
    function autohidesScrollers: ObjCBOOL; message 'autohidesScrollers';
    procedure setAutohidesScrollers(flag: ObjCBOOL); message 'setAutohidesScrollers:';
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
    procedure setScrollsDynamically(flag: ObjCBOOL); message 'setScrollsDynamically:';
    function scrollsDynamically: ObjCBOOL; message 'scrollsDynamically';
    procedure tile; message 'tile';
    procedure reflectScrolledClipView(cView: NSClipView); message 'reflectScrolledClipView:';
    procedure scrollWheel(theEvent: NSEvent); message 'scrollWheel:';
  end;

{ NSRulerSupportCategory }
  NSRulerSupportCategory = objccategory external (NSScrollView)
    class procedure setRulerViewClass(rulerViewClass_: Pobjc_class); message 'setRulerViewClass:';
    class function rulerViewClass: Pobjc_class; message 'rulerViewClass';
    procedure setRulersVisible(flag: ObjCBOOL); message 'setRulersVisible:';
    function rulersVisible: ObjCBOOL; message 'rulersVisible';
    procedure setHasHorizontalRuler(flag: ObjCBOOL); message 'setHasHorizontalRuler:';
    function hasHorizontalRuler: ObjCBOOL; message 'hasHorizontalRuler';
    procedure setHasVerticalRuler(flag: ObjCBOOL); message 'setHasVerticalRuler:';
    function hasVerticalRuler: ObjCBOOL; message 'hasVerticalRuler';
    procedure setHorizontalRulerView(ruler: NSRulerView); message 'setHorizontalRulerView:';
    function horizontalRulerView: NSRulerView; message 'horizontalRulerView';
    procedure setVerticalRulerView(ruler: NSRulerView); message 'setVerticalRulerView:';
    function verticalRulerView: NSRulerView; message 'verticalRulerView';
  end;

{$endif}
{$endif}
