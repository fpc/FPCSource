{ Parsed from SpriteKit.framework SpriteKitBase.h }

{$ifdef TYPES}
type
  SKColor = NSColor;
	SKColorPtr = ^SKColor;
{$endif}

{$ifdef TYPES}
type
  vector_float3 = single;
  vector_float3Ptr = ^vector_float3;
{$endif}

{$ifndef CGVECTOR_DEFINED}
{$ifdef TYPES}
const
  CGVECTOR_DEFINED = 1;

type
  CGVector = record
    dx: CGFloat;
    dy: CGFloat;
  end;
type
  CGVectorPtr = ^CGVector;
{$endif}

{$endif}
