{ Parsed from Appkit.framework NSRulerMarker.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
{$ifndef NSRULERMARKER_PAS_S}
{$define NSRULERMARKER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRulerMarker = objcclass;
  NSRulerMarkerPointer = ^NSRulerMarker;
  NSRulerMarkerPtr = NSRulerMarkerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRULERMARKER_PAS_C}
{$define NSRULERMARKER_PAS_C}

{ NSRulerMarker }
  NSRulerMarker = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _ruler: NSRulerView;
    _location: CGFloat;
    _image: NSImage;
    _imageOrigin: NSPoint;
    __rFlags: bitpacked record
      case byte of
       0: (_anonbitfield_NSRulerMarker0: cuint);
       1: (
        movable: 0..1;
        removable: 0..1;
        dragging: 0..1;
        pinned: 0..1;
        _reserved: 0..((1 shl 28)-1);
      );
      end;
    _representedObject: id;
    
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

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{$endif}
{$endif}
