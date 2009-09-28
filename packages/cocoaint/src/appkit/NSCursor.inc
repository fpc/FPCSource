{ Parsed from Appkit.framework NSCursor.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCURSOR_PAS_H}
{$define NSCURSOR_PAS_H}
type
  NSCursorPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCURSOR_PAS_T}
{$define NSCURSOR_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithCursorSizeSupport = 682.0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCURSOR_PAS_R}
{$define NSCURSOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCURSOR_PAS_F}
{$define NSCURSOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCURSOR_PAS_S}
{$define NSCURSOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCursor = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCURSOR_PAS_C}
{$define NSCURSOR_PAS_C}

{ NSCursor }
  NSCursor = objcclass(NSObject, NSCodingProtocol)
  private
    __hotSpot: NSPoint;
    __flags: bitpacked record
        onMouseExited: 0..1;
        onMouseEntered: 0..1;
        cursorType: 0..((1 shl 8)-1);
        int: 0..((1 shl 22)-1);
      end;
    __image: id;
    
  public
    class function alloc: NSCursor; message 'alloc';

    class function currentCursor: NSCursor; message 'currentCursor';
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
    function initWithImage_hotSpot(newImage: NSImage; aPoint: NSPoint): id; message 'initWithImage:hotSpot:';
    function initWithImage_foregroundColorHint_backgroundColorHint_hotSpot(newImage: NSImage; fg: NSColor; bg: NSColor; hotSpot_: NSPoint): id; message 'initWithImage:foregroundColorHint:backgroundColorHint:hotSpot:';
    class procedure hide; message 'hide';
    class procedure unhide; message 'unhide';
    class procedure setHiddenUntilMouseMoves(flag: Boolean); message 'setHiddenUntilMouseMoves:';
    class procedure pop; message 'pop';
    function image: NSImage; message 'image';
    function hotSpot: NSPoint; message 'hotSpot';
    procedure push; message 'push';
    procedure set_; message 'set';
    procedure setOnMouseExited(flag: Boolean); message 'setOnMouseExited:';
    procedure setOnMouseEntered(flag: Boolean); message 'setOnMouseEntered:';
    function isSetOnMouseExited: Boolean; message 'isSetOnMouseExited';
    function isSetOnMouseEntered: Boolean; message 'isSetOnMouseEntered';
    procedure mouseEntered(theEvent: NSEvent); message 'mouseEntered:';
    procedure mouseExited(theEvent: NSEvent); message 'mouseExited:';
  end; external;

{$endif}
{$endif}
