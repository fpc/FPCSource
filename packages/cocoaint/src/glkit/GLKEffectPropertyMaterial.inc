{ Parsed from GLKit.framework GLKEffectPropertyMaterial.h }


{$ifdef TYPES}
type
  GLKEffectPropertyMaterialPtr = ^GLKEffectPropertyMaterial;
{$endif}

{$ifdef CLASSES}

type
  GLKEffectPropertyMaterial = objcclass external (GLKEffectProperty)
  private
    _ambientColor, _diffuseColor, _specularColor, _emissiveColor: GLKVector4;
    _shininess: GLfloat;
  public
    procedure setAmbientColor(newValue: GLKVector4); message 'setAmbientColor:';
    function ambientColor: GLKVector4; message 'ambientColor';
    procedure setDiffuseColor(newValue: GLKVector4); message 'setDiffuseColor:';
    function diffuseColor: GLKVector4; message 'diffuseColor';
    procedure setSpecularColor(newValue: GLKVector4); message 'setSpecularColor:';
    function specularColor: GLKVector4; message 'specularColor';
    procedure setEmissiveColor(newValue: GLKVector4); message 'setEmissiveColor:';
    function emissiveColor: GLKVector4; message 'emissiveColor';
    procedure setShininess(newValue: GLfloat); message 'setShininess:';
    function shininess: GLfloat; message 'shininess';
  end;
{$endif}

