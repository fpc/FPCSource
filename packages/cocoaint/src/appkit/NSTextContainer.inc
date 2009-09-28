{ Parsed from Appkit.framework NSTextContainer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTEXTCONTAINER_PAS_H}
{$define NSTEXTCONTAINER_PAS_H}
type
  NSTextContainerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTEXTCONTAINER_PAS_T}
{$define NSTEXTCONTAINER_PAS_T}

{ Constants }

const
  NSLineSweepLeft = 0;
  NSLineSweepRight = 1;
  NSLineSweepDown = 2;
  NSLineSweepUp = 3;

const
  NSLineDoesntMove = 0;
  NSLineMovesLeft = 1;
  NSLineMovesRight = 2;
  NSLineMovesDown = 3;
  NSLineMovesUp = 4;

{ Types }
type
  NSLineSweepDirection = NSUInteger;
  NSLineMovementDirection = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTCONTAINER_PAS_R}
{$define NSTEXTCONTAINER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTCONTAINER_PAS_F}
{$define NSTEXTCONTAINER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTCONTAINER_PAS_S}
{$define NSTEXTCONTAINER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextContainer = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTCONTAINER_PAS_C}
{$define NSTEXTCONTAINER_PAS_C}

{ NSTextContainer }
  NSTextContainer = objcclass(NSObject, NSCodingProtocol)
  private
    __layoutManager: NSLayoutManager;
    __textView: NSTextView;
    __size: NSSize;
    __lineFragmentPadding: CGFloat;
    __tcFlags: bitpacked record
        widthTracksTextView: 0..1;
        heightTracksTextView: 0..1;
        observingFrameChanges: 0..1;
        _reserved: 0..((1 shl 13)-1);
      end;
    
  public
    class function alloc: NSTextContainer; message 'alloc';

    function initWithContainerSize(size: NSSize): id; message 'initWithContainerSize:';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    procedure setLayoutManager(layoutManager_: NSLayoutManager); message 'setLayoutManager:';
    procedure replaceLayoutManager(newLayoutManager: NSLayoutManager); message 'replaceLayoutManager:';
    function textView: NSTextView; message 'textView';
    procedure setTextView(textView_: NSTextView); message 'setTextView:';
    procedure setWidthTracksTextView(flag: Boolean); message 'setWidthTracksTextView:';
    function widthTracksTextView: Boolean; message 'widthTracksTextView';
    procedure setHeightTracksTextView(flag: Boolean); message 'setHeightTracksTextView:';
    function heightTracksTextView: Boolean; message 'heightTracksTextView';
    procedure setContainerSize(size: NSSize); message 'setContainerSize:';
    function containerSize: NSSize; message 'containerSize';
    procedure setLineFragmentPadding(pad: CGFloat); message 'setLineFragmentPadding:';
    function lineFragmentPadding: CGFloat; message 'lineFragmentPadding';
    function lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect(proposedRect: NSRect; sweepDirection: NSLineSweepDirection; movementDirection: NSLineMovementDirection; remainingRect: NSRectPointer): NSRect; message 'lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:';
    function isSimpleRectangularTextContainer: Boolean; message 'isSimpleRectangularTextContainer';
    function containsPoint(point: NSPoint): Boolean; message 'containsPoint:';
  end; external;

{$endif}
{$endif}
