{ Parsed from AppKit.framework NSClipView.h }


{$ifdef TYPES}
type
  NSClipViewPtr = ^NSClipView;
{$endif}

{$ifdef CLASSES}

type
  NSClipView = objcclass external (NSView)
  private
    _backgroundColor: NSColor;
    _docView: NSView;
    _docRect: NSRect;
    _oldDocFrame: NSRect;
    _cursor: NSCursor;
    _scrollAnimationHelper: id;
    _cvFlags: bitpacked record
      case byte of
        0: (_anonBitField__cvFlags0: cuint);
        1: (
          int: 0..1;
          onlyUncovered: 0..1;
          reflectScroll: 0..1;
          usedByCell: 0..1;
          scrollClipTo: 0..1;
          noCopyOnScroll: 0..1;
          drawsBackground: 0..1;
          scrollInProgress: 0..1;
          skipRemoveSuperviewCheck: 0..1;
          animateCurrentScroll: 0..1;
          canAnimateScrolls: 0..1;
          nextScrollRelativeToCurrentPosition: 0..1;
          viewBoundsChangedOverridden: 0..1;
          viewFrameChangedOverridden: 0..1;
          documentViewAlignment: 0..((1 shl 4)-1);
          redrawnWhileScrolling: 0..1;
          dontConstrainScroll: 0..1;
          lastAtEdgesState: 0..((1 shl 4)-1);
          showOverlayScrollersForScrollStep: 0..1;
          scrollerKnobFlashSpecifier: 0..((1 shl 2)-1);
          drawsContentShadow: 0..1;
          dontConstrainBoundsChange: 0..1;
          isScrollDueToUserAction: 0..1;
          hasOverlappingViews: 0..1;
          automaticallyCalculateContentSize: 0..1;
        );
      end;
  public
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setDocumentView(newValue: NSView); message 'setDocumentView:';
    function documentView: NSView; message 'documentView';
    function documentRect: NSRect; message 'documentRect';
    procedure setDocumentCursor(newValue: NSCursor); message 'setDocumentCursor:';
    function documentCursor: NSCursor; message 'documentCursor';
    function documentVisibleRect: NSRect; message 'documentVisibleRect';
    procedure viewFrameChanged (notification: NSNotification); message 'viewFrameChanged:';
    procedure viewBoundsChanged (notification: NSNotification); message 'viewBoundsChanged:';
    procedure setCopiesOnScroll(newValue: ObjCBOOL); message 'setCopiesOnScroll:';
    function copiesOnScroll: ObjCBOOL; message 'copiesOnScroll';
    function autoscroll (theEvent: NSEvent): ObjCBOOL; message 'autoscroll:';
    procedure scrollToPoint (newOrigin: NSPoint); message 'scrollToPoint:';
    function constrainBoundsRect (proposedBounds: NSRect): NSRect; message 'constrainBoundsRect:'; { available in 10_9 }
    procedure setContentInsets(newValue: NSEdgeInsets); message 'setContentInsets:';
    function contentInsets: NSEdgeInsets; message 'contentInsets';
    procedure setAutomaticallyAdjustsContentInsets(newValue: ObjCBOOL); message 'setAutomaticallyAdjustsContentInsets:';
    function automaticallyAdjustsContentInsets: ObjCBOOL; message 'automaticallyAdjustsContentInsets';
  end;


type
  NSClipView_NSDeprecated = objccategory external name 'NSDeprecated' (NSClipView)
    function constrainScrollPoint (newOrigin: NSPoint): NSPoint; message 'constrainScrollPoint:'; deprecated 'in 10_0, 10_10, "Use -constrainBoundsRect: instead"';
  end;

type
  NSClipViewSuperview = objccategory external (NSView)
    procedure reflectScrolledClipView (aClipView: NSClipView); message 'reflectScrolledClipView:';
    procedure scrollClipView_toPoint (aClipView: NSClipView; aPoint: NSPoint); message 'scrollClipView:toPoint:';
  end;
{$endif}

