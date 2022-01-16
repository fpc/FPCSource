{ Parsed from GLKit.framework GLKBaseEffect.h }


{$ifdef TYPES}
type
  GLKBaseEffectPtr = ^GLKBaseEffect;
{$endif}

{$ifdef CLASSES}

type
  GLKBaseEffect = objcclass external (NSObject, GLKNamedEffectProtocol)
  private
    _colorMaterialEnabled: GLboolean;
    _fogEnabled: GLboolean;
    _transform: GLKEffectPropertyTransform;
    _lightingType: GLKLightingType;
    _light0, _light1, _light2: GLKEffectPropertyLight;
    _material: GLKEffectPropertyMaterial;
    _texture2d0, _texture2d1: GLKEffectPropertyTexture;
    _textureOrder: NSArray;
    _constantColor: GLKVector4;
    _fog: GLKEffectPropertyFog;
    _label: NSString;
  public
    procedure prepareToDraw; message 'prepareToDraw';
    procedure setColorMaterialEnabled(newValue: GLboolean); message 'setColorMaterialEnabled:';
    function colorMaterialEnabled: GLboolean; message 'colorMaterialEnabled';
    procedure setLightModelTwoSided(newValue: GLboolean); message 'setLightModelTwoSided:';
    function lightModelTwoSided: GLboolean; message 'lightModelTwoSided';
    procedure setUseConstantColor(newValue: GLboolean); message 'setUseConstantColor:';
    function useConstantColor: GLboolean; message 'useConstantColor';
    function transform: GLKEffectPropertyTransform; message 'transform';
    function light0: GLKEffectPropertyLight; message 'light0';
    function light1: GLKEffectPropertyLight; message 'light1';

    function light2: GLKEffectPropertyLight; message 'light2';
    procedure setLightingType(newValue: GLKLightingType); message 'setLightingType:';
    function lightingType: GLKLightingType; message 'lightingType';
    procedure setLightModelAmbientColor(newValue: GLKVector4); message 'setLightModelAmbientColor:';
    function lightModelAmbientColor: GLKVector4; message 'lightModelAmbientColor';
    function material: GLKEffectPropertyMaterial; message 'material';
    function texture2d0: GLKEffectPropertyTexture; message 'texture2d0';
    function texture2d1: GLKEffectPropertyTexture; message 'texture2d1';
    procedure setTextureOrder(newValue: NSArray); message 'setTextureOrder:';
    function textureOrder: NSArray; message 'textureOrder';
    procedure setConstantColor(newValue: GLKVector4); message 'setConstantColor:';
    function constantColor: GLKVector4; message 'constantColor';
    function fog: GLKEffectPropertyFog; message 'fog';
    procedure setLabel(newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label';
  end;
{$endif}

