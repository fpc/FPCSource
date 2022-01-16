{ Parsed from CoreImage.framework CIFilterShape.h }


{$ifdef TYPES}
type
  CIFilterShapePtr = ^CIFilterShape;
{$endif}

{$ifdef CLASSES}

type
  CIFilterShape = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pad: cuint32;
    _priv: pointer;
  public
    class function shapeWithRect (r: CGRect): id; message 'shapeWithRect:';
    function initWithRect (r: CGRect): id; message 'initWithRect:';
    function transformBy_interior (m: CGAffineTransform; flag: ObjCBOOL): CIFilterShape; message 'transformBy:interior:';
    function insetByX_Y (dx: cint; dy: cint): CIFilterShape; message 'insetByX:Y:';
    function unionWith (s2: CIFilterShape): CIFilterShape; message 'unionWith:';
    function unionWithRect (r: CGRect): CIFilterShape; message 'unionWithRect:';
    function intersectWith (s2: CIFilterShape): CIFilterShape; message 'intersectWith:';
    function intersectWithRect (r: CGRect): CIFilterShape; message 'intersectWithRect:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

