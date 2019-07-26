{ Parsed from SpriteKit.framework SKShader.h }


{$ifdef TYPES}
type
  SKShaderPtr = ^SKShader;
{$endif}

{$ifdef CLASSES}

type
  SKShader = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    function initWithSource (source: NSString): instancetype; message 'initWithSource:';
    function initWithSource_uniforms (source: NSString; uniforms: NSArray): instancetype; message 'initWithSource:uniforms:';
    class function shader: instancetype; message 'shader';
    class function shaderWithSource (source: NSString): instancetype; message 'shaderWithSource:';
    class function shaderWithSource_uniforms (source: NSString; uniforms: NSArray): instancetype; message 'shaderWithSource:uniforms:';
    class function shaderWithFileNamed (name: NSString): instancetype; message 'shaderWithFileNamed:';
    procedure setSource(newValue: NSString); message 'setSource:';
    function source: NSString; message 'source';
    procedure setUniforms(newValue: NSArray); message 'setUniforms:';
    function uniforms: NSArray; message 'uniforms';
    procedure addUniform (uniform: SKUniform); message 'addUniform:';
    function uniformNamed (name: NSString): SKUniform; message 'uniformNamed:';
    procedure removeUniformNamed (name: NSString); message 'removeUniformNamed:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

