{ Parsed from AppKit.framework NSCursor.h }


{$ifdef TYPES}
type
  NSCursorPtr = ^NSCursor;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumberWithCursorSizeSupport = 682.0;
{$endif}

{$ifdef CLASSES}

type
  NSCursor = objcclass external (NSObject, NSCodingProtocol)
  private
    _hotSpot: NSPoint;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          onMouseExited: 0..1;
          onMouseEntered: 0..1;
          cursorType: 0..((1 shl 8)-1);
          int: 0..((1 shl 22)-1);
        );
      end;
    _image: id;
  public
    class function currentCursor: NSCursor; message 'currentCursor';
    class function currentSystemCursor: NSCursor; message 'currentSystemCursor'; { available in 10_6 }
    class function arrowCursor: NSCursor; message 'arrowCursor';
    class function IBeamCursor: NSCursor; message 'IBeamCursor';
    class function pointingHandCursor: NSCursor; message 'pointingHandCursor';
    class function closedHandCursor: NSCursor; message 'closedHandCursor';
    class function openHandCursor: NSCursor; message 'openHandCursor';
    class function resizeLeftCursor: NSCursor; message 'resizeLeftCursor';
    class function resizeRightCursor: NSCursor; message 'resizeRightCursor';
    class function resizeLeftRightCursor: NSCursor; message 'resizeLeftRightCursor';
    class function resizeUpCursor: NSCursor; message 'resizeUpCursor';
    class function resizeDownCursor: NSCursor; message 'resizeDownCursor';
    class function resizeUpDownCursor: NSCursor; message 'resizeUpDownCursor';
    class function crosshairCursor: NSCursor; message 'crosshairCursor';
    class function disappearingItemCursor: NSCursor; message 'disappearingItemCursor';
    class function operationNotAllowedCursor: NSCursor; message 'operationNotAllowedCursor'; { available in 10_5 }
    class function dragLinkCursor: NSCursor; message 'dragLinkCursor'; { available in 10_6 }
    class function dragCopyCursor: NSCursor; message 'dragCopyCursor'; { available in 10_6 }
    class function contextualMenuCursor: NSCursor; message 'contextualMenuCursor'; { available in 10_6 }
    class function IBeamCursorForVerticalLayout: NSCursor; message 'IBeamCursorForVerticalLayout'; { available in 10_7 }
    function initWithImage_hotSpot (newImage: NSImage; aPoint: NSPoint): instancetype; message 'initWithImage:hotSpot:';
    function initWithImage_foregroundColorHint_backgroundColorHint_hotSpot (newImage: NSImage; fg: NSColor; bg: NSColor; hotSpot: NSPoint): instancetype; message 'initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:';
    class procedure hide; message 'hide';
    class procedure unhide; message 'unhide';
    class procedure setHiddenUntilMouseMoves (flag: ObjCBOOL); message 'setHiddenUntilMouseMoves:';
    class procedure classPop; message 'pop';
    function image: NSImage; message 'image';
    function hotSpot: NSPoint; message 'hotSpot';
    procedure push; message 'push';
    procedure pop; message 'pop';
    procedure set_; message 'set';
    procedure setOnMouseExited (flag: ObjCBOOL); message 'setOnMouseExited:';
    procedure setOnMouseEntered (flag: ObjCBOOL); message 'setOnMouseEntered:';
    function isSetOnMouseExited: ObjCBOOL; message 'isSetOnMouseExited';
    function isSetOnMouseEntered: ObjCBOOL; message 'isSetOnMouseEntered';
    procedure mouseEntered (theEvent: NSEvent); message 'mouseEntered:';
    procedure mouseExited (theEvent: NSEvent); message 'mouseExited:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

