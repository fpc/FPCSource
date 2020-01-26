{ Parsed from SceneKit.framework SCNSkinner.h }


{$ifdef TYPES}
type
  SCNSkinnerPtr = ^SCNSkinner;
{$endif}

{$ifdef CLASSES}

type
  SCNSkinner = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    procedure setSkeleton(newValue: SCNNode); message 'setSkeleton:';
    function skeleton: SCNNode; message 'skeleton';
    class function skinnerWithBaseGeometry_bones_boneInverseBindTransforms_boneWeights_boneIndices (baseGeometry: SCNGeometry; bones: NSArray; boneInverseBindTransforms: NSArray; boneWeights: SCNGeometrySource; boneIndices: SCNGeometrySource): instancetype; message 'skinnerWithBaseGeometry:bones:boneInverseBindTransforms:boneWeights:boneIndices:'; { available in 10_10, 8_0 }
    procedure setBaseGeometry(newValue: SCNGeometry); message 'setBaseGeometry:';
    function baseGeometry: SCNGeometry; message 'baseGeometry';
    procedure setBaseGeometryBindTransform(newValue: SCNMatrix4); message 'setBaseGeometryBindTransform:';
    function baseGeometryBindTransform: SCNMatrix4; message 'baseGeometryBindTransform';
    function boneInverseBindTransforms: NSArray; message 'boneInverseBindTransforms';
    function bones: NSArray; message 'bones';
    function boneWeights: SCNGeometrySource; message 'boneWeights';
    function boneIndices: SCNGeometrySource; message 'boneIndices';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

