{ Parsed from GLKit.framework GLKEffectPropertyLight.h }


{$ifdef TYPES}
type
  GLKEffectPropertyLightPtr = ^GLKEffectPropertyLight;
{$endif}

{$ifdef TYPES}
type
  GLKLightingType = GLint;
  GLKLightingTypePtr = ^GLKLightingType;

const
  GLKLightingTypePerVertex = 0;
  GLKLightingTypePerPixel = 1;
{$endif}

{$ifdef CLASSES}

type
  GLKEffectPropertyLight = objcclass external (GLKEffectProperty)
  private
    _enabled: GLboolean;
    _position: GLKVector4;
    _ambientColor, _diffuseColor, _specularColor: GLKVector4;
    _spotDirection: GLKVector3;
    _spotExponent, _spotCutoff: GLfloat;
    _constantAttenuation, _linearAttenuation, _quadraticAttenuation: GLfloat;
    _transform: GLKEffectPropertyTransform;
  public
    procedure setEnabled(newValue: GLboolean); message 'setEnabled:';
    function enabled: GLboolean; message 'enabled';
    procedure setPosition(newValue: GLKVector4); message 'setPosition:';
    function position: GLKVector4; message 'position';
    procedure setAmbientColor(newValue: GLKVector4); message 'setAmbientColor:';
    function ambientColor: GLKVector4; message 'ambientColor';
    procedure setDiffuseColor(newValue: GLKVector4); message 'setDiffuseColor:';
    function diffuseColor: GLKVector4; message 'diffuseColor';
    procedure setSpecularColor(newValue: GLKVector4); message 'setSpecularColor:';
    function specularColor: GLKVector4; message 'specularColor';
    procedure setSpotDirection(newValue: GLKVector3); message 'setSpotDirection:';
    function spotDirection: GLKVector3; message 'spotDirection';
    procedure setSpotExponent(newValue: GLfloat); message 'setSpotExponent:';
    function spotExponent: GLfloat; message 'spotExponent';
    procedure setSpotCutoff(newValue: GLfloat); message 'setSpotCutoff:';
    function spotCutoff: GLfloat; message 'spotCutoff';
    procedure setConstantAttenuation(newValue: GLfloat); message 'setConstantAttenuation:';
    function constantAttenuation: GLfloat; message 'constantAttenuation';
    procedure setLinearAttenuation(newValue: GLfloat); message 'setLinearAttenuation:';
    function linearAttenuation: GLfloat; message 'linearAttenuation';

    procedure setQuadraticAttenuation(newValue: GLfloat); message 'setQuadraticAttenuation:';
    function quadraticAttenuation: GLfloat; message 'quadraticAttenuation';
    procedure setTransform(newValue: GLKEffectPropertyTransform); message 'setTransform:';
    function transform: GLKEffectPropertyTransform; message 'transform';
  end;
{$endif}

