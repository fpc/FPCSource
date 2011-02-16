{ Parsed from Quartzcore.framework CATransform3D.h }

{$ifdef TYPES}
{$ifndef CATRANSFORM3D_PAS_T}
{$define CATRANSFORM3D_PAS_T}

{ Types }
type
  CATransform3D = record end;
  CATransform3DPtr = ^CATransform3D;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CATRANSFORM3D_PAS_R}
{$define CATRANSFORM3D_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CATRANSFORM3D_PAS_F}
{$define CATRANSFORM3D_PAS_F}

{ Functions }
function CATransform3DIsIdentity(t: CATransform3D): bool; cdecl; external;
function CATransform3DConcat(a: CATransform3D; b: CATransform3D): CATransform3D; cdecl; external;
function CATransform3DInvert(t: CATransform3D): CATransform3D; cdecl; external;
function CATransform3DMakeAffineTransform(m: CGAffineTransform): CATransform3D; cdecl; external;
function CATransform3DIsAffine(t: CATransform3D): bool; cdecl; external;
function CATransform3DGetAffineTransform(t: CATransform3D): CGAffineTransform; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CATRANSFORM3D_PAS_S}
{$define CATRANSFORM3D_PAS_S}

{ External symbols }
var
  CATransform3DIdentity: CATransform3D; cvar; external;
  CATransform3DEqualToTransform: bool; cvar; external;
  CATransform3DMakeTranslation: CATransform3D; cvar; external;
  CATransform3DMakeScale: CATransform3D; cvar; external;
  CATransform3DMakeRotation: CATransform3D; cvar; external;
  CATransform3DTranslate: CATransform3D; cvar; external;
  CATransform3DScale: CATransform3D; cvar; external;
  CATransform3DRotate: CATransform3D; cvar; external;

{$endif}
{$endif}
