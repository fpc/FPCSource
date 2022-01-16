{ Parsed from SceneKit.framework SCNGeometry.h }


{$ifdef TYPES}
type
  SCNGeometryPtr = ^SCNGeometry;
  SCNGeometrySourcePtr = ^SCNGeometrySource;
  SCNGeometryElementPtr = ^SCNGeometryElement;
{$endif}

{$ifdef TYPES}
type
  SCNGeometryPrimitiveType = NSInteger;
  SCNGeometryPrimitiveTypePtr = ^SCNGeometryPrimitiveType;

const
  SCNGeometryPrimitiveTypeTriangles = 0;
  SCNGeometryPrimitiveTypeTriangleStrip = 1;
  SCNGeometryPrimitiveTypeLine = 2;
  SCNGeometryPrimitiveTypePoint = 3;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNGeometrySourceSemanticVertex: NSString; cvar; external;
  SCNGeometrySourceSemanticNormal: NSString; cvar; external;
  SCNGeometrySourceSemanticColor: NSString; cvar; external;
  SCNGeometrySourceSemanticTexcoord: NSString; cvar; external;
  SCNGeometrySourceSemanticVertexCrease: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNGeometrySourceSemanticEdgeCrease: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNGeometrySourceSemanticBoneWeights: NSString { available in 10_10, 8_0 }; cvar; external;
  SCNGeometrySourceSemanticBoneIndices: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  SCNGeometry = objcclass external (NSObject, SCNAnimatableProtocol, SCNBoundingVolumeProtocol, SCNShadableProtocol, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _geometryReserved: id;
  public
    class function geometry: instancetype; message 'geometry'; { available in 10_9, 8_0 }
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setMaterials(newValue: NSArray); message 'setMaterials:';
    function materials: NSArray; message 'materials';
    procedure setFirstMaterial(newValue: SCNMaterial); message 'setFirstMaterial:';
    function firstMaterial: SCNMaterial; message 'firstMaterial';
    procedure insertMaterial_atIndex (material: SCNMaterial; index: NSUInteger); message 'insertMaterial:atIndex:';
    procedure removeMaterialAtIndex (index: NSUInteger); message 'removeMaterialAtIndex:';
    procedure replaceMaterialAtIndex_withMaterial (index: NSUInteger; material: SCNMaterial); message 'replaceMaterialAtIndex:withMaterial:';
    function materialWithName (name_: NSString): SCNMaterial; message 'materialWithName:';
    class function geometryWithSources_elements (sources: NSArray; elements: NSArray): instancetype; message 'geometryWithSources:elements:';
    function geometrySourcesForSemantic (semantic: NSString): NSArray; message 'geometrySourcesForSemantic:';
    function geometryElementCount: NSInteger; message 'geometryElementCount';
    function geometryElementAtIndex (elementIndex: NSInteger): SCNGeometryElement; message 'geometryElementAtIndex:';
    procedure setLevelsOfDetail(newValue: NSArray); message 'setLevelsOfDetail:';
    function levelsOfDetail: NSArray; message 'levelsOfDetail';
    procedure setSubdivisionLevel(newValue: NSUInteger); message 'setSubdivisionLevel:';
    function subdivisionLevel: NSUInteger; message 'subdivisionLevel';
    procedure setEdgeCreasesElement(newValue: SCNGeometryElement); message 'setEdgeCreasesElement:';
    function edgeCreasesElement: SCNGeometryElement; message 'edgeCreasesElement';
    procedure setEdgeCreasesSource(newValue: SCNGeometrySource); message 'setEdgeCreasesSource:';
    function edgeCreasesSource: SCNGeometrySource; message 'edgeCreasesSource';

    { Adopted protocols }
    procedure addAnimation_forKey (animation: CAAnimation; key: NSString); message 'addAnimation:forKey:';
    function animationForKey (key: NSString): CAAnimation; message 'animationForKey:';
    function animationKeys: NSArray; message 'animationKeys';
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function getBoundingBoxMin_max (min: SCNVector3Ptr; max: SCNVector3Ptr): ObjCBOOL; message 'getBoundingBoxMin:max:';
    function getBoundingSphereCenter_radius (center: SCNVector3Ptr; radius: CGFloatPtr): ObjCBOOL; message 'getBoundingSphereCenter:radius:';
    procedure handleBindingOfSymbol_usingBlock (symbol: NSString; block: SCNBindingBlock); message 'handleBindingOfSymbol:usingBlock:'; { available in 10_9, 8_0 }
    procedure handleUnbindingOfSymbol_usingBlock (symbol: NSString; block: SCNBindingBlock); message 'handleUnbindingOfSymbol:usingBlock:'; { available in 10_9, 8_0 }
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function isAnimationForKeyPaused (key: NSString): ObjCBOOL; message 'isAnimationForKeyPaused:'; { available in 10_9, 8_0 }
    procedure pauseAnimationForKey (key: NSString); message 'pauseAnimationForKey:'; { available in 10_9, 8_0 }
    procedure setProgram(newValue: SCNProgram); message 'setProgram:';
    function program_: SCNProgram; message 'program';
    procedure removeAllAnimations; message 'removeAllAnimations';
    procedure removeAnimationForKey (key: NSString); message 'removeAnimationForKey:';
    procedure removeAnimationForKey_fadeOutDuration (key: NSString; duration: CGFloat); message 'removeAnimationForKey:fadeOutDuration:'; { available in 10_10, 8_0 }
    procedure resumeAnimationForKey (key: NSString); message 'resumeAnimationForKey:'; { available in 10_9, 8_0 }
    procedure setBoundingBoxMin_max (min: SCNVector3Ptr; max: SCNVector3Ptr); message 'setBoundingBoxMin:max:'; { available in 10_9, 8_0 }
    procedure setShaderModifiers(newValue: NSDictionary); message 'setShaderModifiers:';
    function shaderModifiers: NSDictionary; message 'shaderModifiers';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  SCNGeometrySource = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function geometrySourceWithData_semantic_vectorCount_floatComponents_componentsPerVector_bytesPerComponent_dataOffset_dataStride (data: NSData; semantic: NSString; vectorCount: NSInteger; floatComponents: ObjCBOOL; componentsPerVector: NSInteger; bytesPerComponent: NSInteger; offset: NSInteger; stride: NSInteger): instancetype; message 'geometrySourceWithData:semantic:vectorCount:floatComponents:componentsPerVector:bytesPerComponent:dataOffset:dataStride:';
    class function geometrySourceWithVertices_count (vertices: SCNVector3Ptr; count: NSInteger): instancetype; message 'geometrySourceWithVertices:count:';
    class function geometrySourceWithNormals_count (normals: SCNVector3Ptr; count: NSInteger): instancetype; message 'geometrySourceWithNormals:count:';
    class function geometrySourceWithTextureCoordinates_count (texcoord: CGPointPtr; count: NSInteger): instancetype; message 'geometrySourceWithTextureCoordinates:count:';
    function data: NSData; message 'data';
    function semantic: NSString; message 'semantic';
    function vectorCount: NSInteger; message 'vectorCount';
    function floatComponents: ObjCBOOL; message 'floatComponents';
    function componentsPerVector: NSInteger; message 'componentsPerVector';
    function bytesPerComponent: NSInteger; message 'bytesPerComponent';
    function dataOffset: NSInteger; message 'dataOffset';
    function dataStride: NSInteger; message 'dataStride';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  SCNGeometryElement = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function geometryElementWithData_primitiveType_primitiveCount_bytesPerIndex (data: NSData; primitiveType: SCNGeometryPrimitiveType; primitiveCount: NSInteger; bytesPerIndex: NSInteger): instancetype; message 'geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:';
    function data: NSData; message 'data';
    function primitiveType: SCNGeometryPrimitiveType; message 'primitiveType';
    function primitiveCount: NSInteger; message 'primitiveCount';
    function bytesPerIndex: NSInteger; message 'bytesPerIndex';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

