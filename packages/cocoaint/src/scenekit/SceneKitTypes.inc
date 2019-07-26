{ Parsed from SceneKit.framework SceneKitTypes.h }

{$ifdef TYPES}
type
  SCNMatrix4 = CATransform3D;
  SCNMatrix4Ptr = ^SCNMatrix4;

type
  SCNVector3 = record
    x, y, z: CGFloat;
  end;
type
  SCNVector3Ptr = ^SCNVector3;
type
  SCNVector4 = record
    x, y, z, w: CGFloat;
  end;
type
  SCNVector4Ptr = ^SCNVector4;

type
  SCNQuaternion = SCNVector4;
  SCNQuaternionPtr = ^SCNQuaternion;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNMatrix4Identity: SCNMatrix4 { available in 10_10, 8_0 }; cvar; external;
  SCNVector3Zero: SCNVector3 { available in 10_10, 8_0 }; cvar; external;
  SCNVector4Zero: SCNVector4 { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function SCNVector3EqualToVector3(a: SCNVector3; b: SCNVector3): cbool; cdecl; external;
function SCNVector4EqualToVector4(a: SCNVector4; b: SCNVector4): cbool; cdecl; external;
function SCNMatrix4MakeRotation(angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): SCNMatrix4; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4Scale(mat: SCNMatrix4; x: CGFloat; y: CGFloat; z: CGFloat): SCNMatrix4; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4Rotate(mat: SCNMatrix4; angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): SCNMatrix4; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4Invert(mat: SCNMatrix4): SCNMatrix4; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4Mult(matA: SCNMatrix4; matB: SCNMatrix4): SCNMatrix4; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4IsIdentity(mat: SCNMatrix4): cbool; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4EqualToMatrix4(matA: SCNMatrix4; matB: SCNMatrix4): cbool; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4ToGLKMatrix4(mat: SCNMatrix4): GLKMatrix4; cdecl; external; { available in 10_10, 8_0 }
function SCNMatrix4FromGLKMatrix4(mat: GLKMatrix4): SCNMatrix4; cdecl; external; { available in 10_10, 8_0 }
{$endif}

{$ifdef CLASSES}

type
  NSValue_SceneKitAdditions = objccategory external name 'SceneKitAdditions' (NSValue)
    class function valueWithSCNVector3 (v: SCNVector3): NSValue; message 'valueWithSCNVector3:';
    class function valueWithSCNVector4 (v: SCNVector4): NSValue; message 'valueWithSCNVector4:';
    class function valueWithSCNMatrix4 (v: SCNMatrix4): NSValue; message 'valueWithSCNMatrix4:'; { available in 10_10, 8_0 }
    function SCNVector3Value: SCNVector3; message 'SCNVector3Value';
    function SCNVector4Value: SCNVector4; message 'SCNVector4Value';
    function SCNMatrix4Value: SCNMatrix4; message 'SCNMatrix4Value';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SCNErrorDomain: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  SCNProgramCompilationError = 1;
{$endif}
