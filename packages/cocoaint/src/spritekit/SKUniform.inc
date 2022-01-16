{ Parsed from SpriteKit.framework SKUniform.h }


{$ifdef TYPES}
type
  SKUniformPtr = ^SKUniform;
{$endif}

{$ifdef TYPES}
type
  SKUniformType = NSInteger;
  SKUniformTypePtr = ^SKUniformType;

const
  SKUniformTypeNone = 0;
  SKUniformTypeFloat = 1;
  SKUniformTypeFloatVector2 = 2;
  SKUniformTypeFloatVector3 = 3;
  SKUniformTypeFloatVector4 = 4;
  SKUniformTypeFloatMatrix2 = 5;
  SKUniformTypeFloatMatrix3 = 6;
  SKUniformTypeFloatMatrix4 = 7;
  SKUniformTypeTexture = 8;
{$endif}

{$ifdef CLASSES}

type
  SKUniform = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  public
    class function uniformWithName (name: NSString): instancetype; message 'uniformWithName:';
    class function uniformWithName_texture (name: NSString; texture: SKTexture): instancetype; message 'uniformWithName:texture:';
    class function uniformWithName_float (name: NSString; value: single): instancetype; message 'uniformWithName:float:';
    class function uniformWithName_floatVector2 (name: NSString; value: GLKVector2): instancetype; message 'uniformWithName:floatVector2:';
    class function uniformWithName_floatVector3 (name: NSString; value: GLKVector3): instancetype; message 'uniformWithName:floatVector3:';
    class function uniformWithName_floatVector4 (name: NSString; value: GLKVector4): instancetype; message 'uniformWithName:floatVector4:';
    class function uniformWithName_floatMatrix2 (name: NSString; value: GLKMatrix2): instancetype; message 'uniformWithName:floatMatrix2:';
    class function uniformWithName_floatMatrix3 (name: NSString; value: GLKMatrix3): instancetype; message 'uniformWithName:floatMatrix3:';
    class function uniformWithName_floatMatrix4 (name: NSString; value: GLKMatrix4): instancetype; message 'uniformWithName:floatMatrix4:';
    function name: NSString; message 'name';
    function uniformType: SKUniformType; message 'uniformType';
    procedure setTextureValue(newValue: SKTexture); message 'setTextureValue:';
    function textureValue: SKTexture; message 'textureValue';
    procedure setFloatValue(newValue: single); message 'setFloatValue:';
    function floatValue: single; message 'floatValue';
    procedure setFloatVector2Value(newValue: GLKVector2); message 'setFloatVector2Value:';
    function floatVector2Value: GLKVector2; message 'floatVector2Value';
    procedure setFloatVector3Value(newValue: GLKVector3); message 'setFloatVector3Value:';
    function floatVector3Value: GLKVector3; message 'floatVector3Value';
    procedure setFloatVector4Value(newValue: GLKVector4); message 'setFloatVector4Value:';
    function floatVector4Value: GLKVector4; message 'floatVector4Value';
    procedure setFloatMatrix2Value(newValue: GLKMatrix2); message 'setFloatMatrix2Value:';
    function floatMatrix2Value: GLKMatrix2; message 'floatMatrix2Value';
    procedure setFloatMatrix3Value(newValue: GLKMatrix3); message 'setFloatMatrix3Value:';
    function floatMatrix3Value: GLKMatrix3; message 'floatMatrix3Value';
    procedure setFloatMatrix4Value(newValue: GLKMatrix4); message 'setFloatMatrix4Value:';
    function floatMatrix4Value: GLKMatrix4; message 'floatMatrix4Value';
    function initWithName (name_: NSString): instancetype; message 'initWithName:';
    function initWithName_texture (name_: NSString; texture: SKTexture): instancetype; message 'initWithName:texture:';
    function initWithName_float (name_: NSString; value: single): instancetype; message 'initWithName:float:';
    function initWithName_floatVector2 (name_: NSString; value: GLKVector2): instancetype; message 'initWithName:floatVector2:';
    function initWithName_floatVector3 (name_: NSString; value: GLKVector3): instancetype; message 'initWithName:floatVector3:';
    function initWithName_floatVector4 (name_: NSString; value: GLKVector4): instancetype; message 'initWithName:floatVector4:';
    function initWithName_floatMatrix2 (name_: NSString; value: GLKMatrix2): instancetype; message 'initWithName:floatMatrix2:';
    function initWithName_floatMatrix3 (name_: NSString; value: GLKMatrix3): instancetype; message 'initWithName:floatMatrix3:';
    function initWithName_floatMatrix4 (name_: NSString; value: GLKMatrix4): instancetype; message 'initWithName:floatMatrix4:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

