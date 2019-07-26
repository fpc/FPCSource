{ Parsed from AppKit.framework NSTextContainer.h }


{$ifdef TYPES}
type
  NSTextContainerPtr = ^NSTextContainer;
{$endif}

{$ifdef TYPES}
type
  NSLineSweepDirection = NSUInteger;
  NSLineSweepDirectionPtr = ^NSLineSweepDirection;

const
  NSLineSweepLeft = 0;
  NSLineSweepRight = 1;
  NSLineSweepDown = 2;
  NSLineSweepUp = 3;

type
  NSLineMovementDirection = NSUInteger;
  NSLineMovementDirectionPtr = ^NSLineMovementDirection;

const
  NSLineDoesntMove = 0;
  NSLineMovesLeft = 1;
  NSLineMovesRight = 2;
  NSLineMovesDown = 3;
  NSLineMovesUp = 4;
{$endif}

{$ifdef CLASSES}

type
  NSTextContainer = objcclass external (NSObject, NSCodingProtocol, NSTextLayoutOrientationProviderProtocol)
  private
    _layoutManager: NSLayoutManager;
    _textView: NSTextView;
    _size: NSSize;
    _lineFragmentPadding: CGFloat;
    __tcFlags: bitpacked record
      case byte of
        0: (_anonBitField___tcFlags0: cuint);
        1: (
          widthTracksTextView: 0..1;
          heightTracksTextView: 0..1;
          observingFrameChanges: 0..1;
          lineBreakMode: 0..((1 shl 4)-1);
          _reserved: 0..((1 shl 9)-1);
        );
      end;
  public
    function initWithContainerSize (size: NSSize): instancetype; message 'initWithContainerSize:';
    procedure setLayoutManager(newValue: NSLayoutManager); message 'setLayoutManager:';
    function layoutManager: NSLayoutManager; message 'layoutManager';
    procedure replaceLayoutManager (newLayoutManager: NSLayoutManager); message 'replaceLayoutManager:';
    procedure setTextView(newValue: NSTextView); message 'setTextView:';
    function textView: NSTextView; message 'textView';
    procedure setWidthTracksTextView(newValue: ObjCBOOL); message 'setWidthTracksTextView:';
    function widthTracksTextView: ObjCBOOL; message 'widthTracksTextView';
    procedure setHeightTracksTextView(newValue: ObjCBOOL); message 'setHeightTracksTextView:';
    function heightTracksTextView: ObjCBOOL; message 'heightTracksTextView';
    procedure setContainerSize(newValue: NSSize); message 'setContainerSize:';
    function containerSize: NSSize; message 'containerSize';
    procedure setLineFragmentPadding(newValue: CGFloat); message 'setLineFragmentPadding:';
    function lineFragmentPadding: CGFloat; message 'lineFragmentPadding';
    function lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect (proposedRect: NSRect; sweepDirection: NSLineSweepDirection; movementDirection: NSLineMovementDirection; remainingRect: NSRectPointer): NSRect; message 'lineFragmentRectForProposedRect:sweepDirection:movementDirection:remainingRect:';
    function isSimpleRectangularTextContainer: ObjCBOOL; message 'isSimpleRectangularTextContainer';
    function containsPoint (point: NSPoint): ObjCBOOL; message 'containsPoint:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function layoutOrientation: NSTextLayoutOrientation; message 'layoutOrientation'; { available in 10_7 }
  end;
{$endif}

