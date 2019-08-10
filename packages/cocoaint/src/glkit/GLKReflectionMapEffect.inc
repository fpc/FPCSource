{ Parsed from GLKit.framework GLKReflectionMapEffect.h }


{$ifdef TYPES}
type
  GLKReflectionMapEffectPtr = ^GLKReflectionMapEffect;
{$endif}

{$ifdef CLASSES}

type
  GLKReflectionMapEffect = objcclass external (GLKBaseEffect, GLKNamedEffectProtocol)
  private
    _textureCubeMap: GLKEffectPropertyTexture;
    _matrix: GLKMatrix3;
  public
    procedure prepareToDraw; message 'prepareToDraw';
    function textureCubeMap: GLKEffectPropertyTexture; message 'textureCubeMap';
    procedure setMatrix(newValue: GLKMatrix3); message 'setMatrix:';
    function matrix: GLKMatrix3; message 'matrix';
  end;
{$endif}

