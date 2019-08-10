{ Parsed from GLKit.framework GLKEffectPropertyFog.h }


{$ifdef TYPES}
type
  GLKEffectPropertyFogPtr = ^GLKEffectPropertyFog;
{$endif}

{$ifdef TYPES}
type
  GLKFogMode = GLint;
  GLKFogModePtr = ^GLKFogMode;

const
  GLKFogModeExp = 0;
  GLKFogModeExp2 = 1;
  GLKFogModeLinear = 2;
{$endif}

{$ifdef CLASSES}

type
  GLKEffectPropertyFog = objcclass external (GLKEffectProperty)
  private
    _enabled: GLboolean;
    _mode: GLint;
    _color: GLKVector4;
    _density: GLfloat;
    _start: GLfloat;
    _end: GLfloat;
  public
    procedure setEnabled(newValue: GLboolean); message 'setEnabled:';
    function enabled: GLboolean; message 'enabled';
    procedure setMode(newValue: GLint); message 'setMode:';
    function mode: GLint; message 'mode';
    procedure setColor(newValue: GLKVector4); message 'setColor:';
    function color: GLKVector4; message 'color';
    procedure setDensity(newValue: GLfloat); message 'setDensity:';
    function density: GLfloat; message 'density';
    procedure setStart(newValue: GLfloat); message 'setStart:';
    function start: GLfloat; message 'start';
    procedure setEnd(newValue: GLfloat); message 'setEnd:';
    function end_: GLfloat; message 'end';
  end;
{$endif}

