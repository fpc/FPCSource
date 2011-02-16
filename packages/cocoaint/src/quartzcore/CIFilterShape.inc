{ Parsed from Quartzcore.framework CIFilterShape.h }

{$ifdef TYPES}
{$ifndef CIFILTERSHAPE_PAS_T}
{$define CIFILTERSHAPE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIFILTERSHAPE_PAS_R}
{$define CIFILTERSHAPE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIFILTERSHAPE_PAS_F}
{$define CIFILTERSHAPE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIFILTERSHAPE_PAS_S}
{$define CIFILTERSHAPE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CIFilterShape = objcclass;
  CIFilterShapePointer = ^CIFilterShape;
  CIFilterShapePtr = CIFilterShapePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CIFILTERSHAPE_PAS_C}
{$define CIFILTERSHAPE_PAS_C}

{ CIFilterShape }
  CIFilterShape = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pad: cuint32;
    _priv: Pointer;
    
  public
    class function shapeWithRect(r: CGRect): id; message 'shapeWithRect:';
    function initWithRect(r: CGRect): id; message 'initWithRect:';
    function transformBy_interior(m: CGAffineTransform; flag: Boolean): CIFilterShape; message 'transformBy:interior:';
    function insetByX_Y(dx: cint; dy: cint): CIFilterShape; message 'insetByX:Y:';
    function unionWith(s: CIFilterShape): CIFilterShape; message 'unionWith:';
    function unionWithRect(r: CGRect): CIFilterShape; message 'unionWithRect:';
    function intersectWith(s: CIFilterShape): CIFilterShape; message 'intersectWith:';
    function intersectWithRect(r: CGRect): CIFilterShape; message 'intersectWithRect:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
