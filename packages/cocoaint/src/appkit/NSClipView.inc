{ Parsed from Appkit.framework NSClipView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCLIPVIEW_PAS_H}
{$define NSCLIPVIEW_PAS_H}
type
  NSClipViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCLIPVIEW_PAS_T}
{$define NSCLIPVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCLIPVIEW_PAS_R}
{$define NSCLIPVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCLIPVIEW_PAS_F}
{$define NSCLIPVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCLIPVIEW_PAS_S}
{$define NSCLIPVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSClipView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCLIPVIEW_PAS_C}
{$define NSCLIPVIEW_PAS_C}

{ NSClipView }
  NSClipView = objcclass(NSView)
  private
    __backgroundColor: NSColor;
    __docView: NSView;
    __docRect: NSRect;
    __oldDocFrame: NSRect;
    __cursor: NSCursor;
    __scrollAnimationHelper: id;
    __cvFlags: bitpacked record
        isFlipped: 0..1;
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
        RESERVED: 0..((1 shl 14)-1);
      end;
    
  public
    class function alloc: NSClipView; message 'alloc';

    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setDocumentView(aView: NSView); message 'setDocumentView:';
    function documentView: id; message 'documentView';
    function documentRect: NSRect; message 'documentRect';
    procedure setDocumentCursor(anObj: NSCursor); message 'setDocumentCursor:';
    function documentCursor: NSCursor; message 'documentCursor';
    function documentVisibleRect: NSRect; message 'documentVisibleRect';
    procedure viewFrameChanged(notification: NSNotification); message 'viewFrameChanged:';
    procedure viewBoundsChanged(notification: NSNotification); message 'viewBoundsChanged:';
    procedure setCopiesOnScroll(flag: Boolean); message 'setCopiesOnScroll:';
    function copiesOnScroll: Boolean; message 'copiesOnScroll';
    function autoscroll(theEvent: NSEvent): Boolean; message 'autoscroll:';
    function constrainScrollPoint(newOrigin: NSPoint): NSPoint; message 'constrainScrollPoint:';
    procedure scrollToPoint(newOrigin: NSPoint); message 'scrollToPoint:';
  end; external;

{$endif}
{$endif}
