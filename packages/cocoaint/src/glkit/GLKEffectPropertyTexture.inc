{ Parsed from GLKit.framework GLKEffectPropertyTexture.h }


{$ifdef TYPES}
type
  GLKEffectPropertyTexturePtr = ^GLKEffectPropertyTexture;
{$endif}

{$ifdef TYPES}
type
  GLKTextureTarget = GLenum;
  GLKTextureTargetPtr = ^GLKTextureTarget;

const
  GLKTextureTarget2D = GL_TEXTURE_2D;
  GLKTextureTargetCubeMap = GL_TEXTURE_CUBE_MAP;
  GLKTextureTargetCt = 2;

type
  GLKTextureEnvMode = GLint;
  GLKTextureEnvModePtr = ^GLKTextureEnvMode;

const
  GLKTextureEnvModeReplace = 0;
  GLKTextureEnvModeModulate = 1;
  GLKTextureEnvModeDecal = 2;
{$endif}

{$ifdef CLASSES}

type
  GLKEffectPropertyTexture = objcclass external (GLKEffectProperty)
  private
    _enabled: GLboolean;
    _name: GLuint;
    _target: GLKTextureTarget;
    _envMode: GLKTextureEnvMode;
  public
    procedure setEnabled(newValue: GLboolean); message 'setEnabled:';
    function enabled: GLboolean; message 'enabled';
    procedure setName(newValue: GLuint); message 'setName:';
    function name: GLuint; message 'name';
    procedure setTarget(newValue: GLKTextureTarget); message 'setTarget:';
    function target: GLKTextureTarget; message 'target';
    procedure setEnvMode(newValue: GLKTextureEnvMode); message 'setEnvMode:';
    function envMode: GLKTextureEnvMode; message 'envMode';
  end;
{$endif}

