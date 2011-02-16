{ Parsed from Appkit.framework NSCIImageRep.h }

{$ifdef TYPES}
{$ifndef NSCIIMAGEREP_PAS_T}
{$define NSCIIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCIIMAGEREP_PAS_R}
{$define NSCIIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCIIMAGEREP_PAS_F}
{$define NSCIIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCIIMAGEREP_PAS_S}
{$define NSCIIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCIImageRep = objcclass;
  NSCIImageRepPointer = ^NSCIImageRep;
  NSCIImageRepPtr = NSCIImageRepPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCIIMAGEREP_PAS_C}
{$define NSCIIMAGEREP_PAS_C}

{ NSCIImageRep }
  NSCIImageRep = objcclass external (NSImageRep)
  private
    _ciImage: CIImage;
    
  public
    class function imageRepWithCIImage(image: CIImage): id; message 'imageRepWithCIImage:';
    function initWithCIImage(image: CIImage): id; message 'initWithCIImage:';
    function CIImage: CIImage; message 'CIImage';
  end;

{ NSAppKitAdditions_CIImageCategory }
  NSAppKitAdditions_CIImageCategory = objccategory external name 'NSAppKitAdditions' (CIImage)
    function initWithBitmapImageRep(bitmapImageRep: NSBitmapImageRep): id; message 'initWithBitmapImageRep:';
    procedure drawInRect_fromRect_operation_fraction(rect: NSRect; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawInRect:fromRect:operation:fraction:';
    procedure drawAtPoint_fromRect_operation_fraction(point: NSPoint; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawAtPoint:fromRect:operation:fraction:';
  end;

{$endif}
{$endif}
