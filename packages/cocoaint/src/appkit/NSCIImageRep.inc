{ Parsed from AppKit.framework NSCIImageRep.h }


{$ifdef TYPES}
type
  NSCIImageRepPtr = ^NSCIImageRep;
{$endif}

{$ifdef CLASSES}

type
  NSCIImageRep = objcclass external (NSImageRep)
  private
    _ciImage: CIImage;
  public
    class function imageRepWithCIImage (image: CIImage): instancetype; message 'imageRepWithCIImage:';
    function initWithCIImage (image: CIImage): instancetype; message 'initWithCIImage:';
    function CIImage_: CIImage; message 'CIImage';
  end;


type
  NSAppKitAdditions = objccategory external (CIImage)
    function initWithBitmapImageRep (bitmapImageRep: NSBitmapImageRep): instancetype; message 'initWithBitmapImageRep:';
    procedure drawInRect_fromRect_operation_fraction (rect: NSRect; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawInRect:fromRect:operation:fraction:';
    procedure drawAtPoint_fromRect_operation_fraction (point: NSPoint; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawAtPoint:fromRect:operation:fraction:';
  end;
{$endif}

