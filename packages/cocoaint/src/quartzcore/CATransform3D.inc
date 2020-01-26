{ Parsed from QuartzCore.framework CATransform3D.h }

{$ifndef CATRANSFORM_H}

{$ifdef TYPES}
type
  CATransform3D = record
    m11, m12, m13, m14: CGFloat;
    m21, m22, m23, m24: CGFloat;
    m31, m32, m33, m34: CGFloat;
    m41, m42, m43, m44: CGFloat;
  end;
type
  CATransform3DPtr = ^CATransform3D;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CATransform3DIdentity: CATransform3D { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CATransform3DIsIdentity(t: CATransform3D): cbool; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DEqualToTransform(a: CATransform3D; b: CATransform3D): cbool; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DMakeTranslation(tx: CGFloat; ty: CGFloat; tz: CGFloat): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DMakeScale(sx: CGFloat; sy: CGFloat; sz: CGFloat): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DMakeRotation(angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DTranslate(t: CATransform3D; tx: CGFloat; ty: CGFloat; tz: CGFloat): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DScale(t: CATransform3D; sx: CGFloat; sy: CGFloat; sz: CGFloat): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DRotate(t: CATransform3D; angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DConcat(a: CATransform3D; b: CATransform3D): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DInvert(t: CATransform3D): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DMakeAffineTransform(m: CGAffineTransform): CATransform3D; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DIsAffine(t: CATransform3D): cbool; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
function CATransform3DGetAffineTransform(t: CATransform3D): CGAffineTransform; cdecl; external; { available starting in __MAC_10_5, __IPHONE_2_0 }
{$endif}

{$ifdef CLASSES}

type
  CATransform3DAdditions = objccategory external (NSValue)
    class function valueWithCATransform3D (t: CATransform3D): NSValue; message 'valueWithCATransform3D:';
    function CATransform3DValue: CATransform3D; message 'CATransform3DValue';
  end;
{$endif}

{$endif}
