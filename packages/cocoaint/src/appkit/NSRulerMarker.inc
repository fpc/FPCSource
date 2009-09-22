{ Parsed from Appkit.framework NSRulerMarker.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSRULERMARKER_PAS_H}
{$define NSRULERMARKER_PAS_H}
type
  NSRulerMarkerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSRULERMARKER_PAS_T}
{$define NSRULERMARKER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRULERMARKER_PAS_R}
{$define NSRULERMARKER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRULERMARKER_PAS_F}
{$define NSRULERMARKER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRULERMARKER_PAS_T}
{$define NSRULERMARKER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRulerMarker = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRULERMARKER_PAS_C}
{$define NSRULERMARKER_PAS_C}

{ NSRulerMarker }
  NSRulerMarker = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __ruler: NSRulerView;
    __location: CGFloat;
    __image: NSImage;
    __imageOrigin: NSPoint;
    __rFlags: bitpacked record
        movable: 0..1;
        removable: 0..1;
        dragging: 0..1;
        pinned: 0..1;
        _reserved: 0..((1 shl 28)-1);
      end;
    __representedObject: id;
    
  public
    class function alloc: NSRulerMarker; message 'alloc';

    function initWithRulerView_markerLocation_image_imageOrigin(ruler_: NSRulerView; location: CGFloat; image_: NSImage; imageOrigin_: NSPoint): id; message 'initWithRulerView:markerLocation:image:imageOrigin:';
    function ruler: NSRulerView; message 'ruler';
    procedure setMarkerLocation(location: CGFloat); message 'setMarkerLocation:';
    function markerLocation: CGFloat; message 'markerLocation';
    procedure setImage(image_: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setImageOrigin(imageOrigin_: NSPoint); message 'setImageOrigin:';
    function imageOrigin: NSPoint; message 'imageOrigin';
    procedure setMovable(flag: Boolean); message 'setMovable:';
    procedure setRemovable(flag: Boolean); message 'setRemovable:';
    function isMovable: Boolean; message 'isMovable';
    function isRemovable: Boolean; message 'isRemovable';
    function isDragging: Boolean; message 'isDragging';
    procedure setRepresentedObject(representedObject_: id); message 'setRepresentedObject:';
    function representedObject: id; message 'representedObject';
    function imageRectInRuler: NSRect; message 'imageRectInRuler';
    function thicknessRequiredInRuler: CGFloat; message 'thicknessRequiredInRuler';
    procedure drawRect(rect: NSRect); message 'drawRect:';
    function trackMouse_adding(mouseDownEvent: NSEvent; isAdding: Boolean): Boolean; message 'trackMouse:adding:';
  end; external;

{$endif}
{$endif}
