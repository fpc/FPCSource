{ Parsed from AppKit.framework NSRulerMarker.h }


{$ifdef TYPES}
type
  NSRulerMarkerPtr = ^NSRulerMarker;
{$endif}

{$ifdef CLASSES}

type
  NSRulerMarker = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _ruler: NSRulerView;
    _location: CGFloat;
    _image: NSImage;
    _imageOrigin: NSPoint;
    _rFlags: bitpacked record
      case byte of
        0: (_anonBitField__rFlags0: cuint);
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
    function initWithRulerView_markerLocation_image_imageOrigin (ruler: NSRulerView; location: CGFloat; image: NSImage; imageOrigin: NSPoint): instancetype; message 'initWithRulerView:markerLocation:image:imageOrigin:';
    function ruler: NSRulerView; message 'ruler';
    procedure setMarkerLocation(newValue: CGFloat); message 'setMarkerLocation:';
    function markerLocation: CGFloat; message 'markerLocation';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setImageOrigin(newValue: NSPoint); message 'setImageOrigin:';
    function imageOrigin: NSPoint; message 'imageOrigin';
    procedure setMovable(newValue: ObjCBOOL); message 'setMovable:';
    function isMovable: ObjCBOOL; message 'isMovable';
    procedure setRemovable(newValue: ObjCBOOL); message 'setRemovable:';
    function isRemovable: ObjCBOOL; message 'isRemovable';
    function isDragging: ObjCBOOL; message 'isDragging';
    procedure setRepresentedObject(newValue: NSCopyingProtocol); message 'setRepresentedObject:';
    function representedObject: NSCopyingProtocol; message 'representedObject';
    function imageRectInRuler: NSRect; message 'imageRectInRuler';
    function thicknessRequiredInRuler: CGFloat; message 'thicknessRequiredInRuler';
    procedure drawRect (rect: NSRect); message 'drawRect:';
    function trackMouse_adding (mouseDownEvent: NSEvent; isAdding: ObjCBOOL): ObjCBOOL; message 'trackMouse:adding:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

