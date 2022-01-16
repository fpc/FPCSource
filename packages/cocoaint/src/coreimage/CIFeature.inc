{ Parsed from CoreImage.framework CIFeature.h }


{$ifdef TYPES}
type
  CIFeaturePtr = ^CIFeature;
  CIFaceFeaturePtr = ^CIFaceFeature;
  CIRectangleFeaturePtr = ^CIRectangleFeature;
  CIQRCodeFeaturePtr = ^CIQRCodeFeature;
{$endif}

{$ifdef CLASSES}

type
  CIFeature = objcclass external (NSObject)
  private
  public
    function type_: NSString; message 'type';
    function bounds: CGRect; message 'bounds';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CIFeatureTypeFace: NSString; cvar; external;
  CIFeatureTypeRectangle: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CIFaceFeature = objcclass external (CIFeature)
  private
    bounds_: CGRect;
    hasLeftEyePosition: ObjCBOOL;
    leftEyePosition: CGPoint;
    hasRightEyePosition: ObjCBOOL;
    rightEyePosition: CGPoint;
    hasMouthPosition: ObjCBOOL;
    mouthPosition: CGPoint;
    hasTrackingID: ObjCBOOL;
    trackingID: cint;
    hasTrackingFrameCount: ObjCBOOL;
    trackingFrameCount: cint;
    hasFaceAngle: ObjCBOOL;
    faceAngle: single;
    hasSmile: ObjCBOOL;
    leftEyeClosed: ObjCBOOL;
    rightEyeClosed: ObjCBOOL;
  public
    function bounds: CGRect; message 'bounds';
    function hasLeftEyePosition_: ObjCBOOL; message 'hasLeftEyePosition';
    function leftEyePosition_: CGPoint; message 'leftEyePosition';
    function hasRightEyePosition_: ObjCBOOL; message 'hasRightEyePosition';
    function rightEyePosition_: CGPoint; message 'rightEyePosition';
    function hasMouthPosition_: ObjCBOOL; message 'hasMouthPosition';
    function mouthPosition_: CGPoint; message 'mouthPosition';
    function hasTrackingID_: ObjCBOOL; message 'hasTrackingID';
    function trackingID_: cint; message 'trackingID';
    function hasTrackingFrameCount_: ObjCBOOL; message 'hasTrackingFrameCount';
    function trackingFrameCount_: cint; message 'trackingFrameCount';
    function hasFaceAngle_: ObjCBOOL; message 'hasFaceAngle';
    function faceAngle_: single; message 'faceAngle';
    function hasSmile_: ObjCBOOL; message 'hasSmile';
    function leftEyeClosed_: ObjCBOOL; message 'leftEyeClosed';
    function rightEyeClosed_: ObjCBOOL; message 'rightEyeClosed';
  end;

type
  CIRectangleFeature = objcclass external (CIFeature)
  private
    bounds_: CGRect;
    topLeft: CGPoint;
    topRight: CGPoint;
    bottomLeft: CGPoint;
    bottomRight: CGPoint;
  public
    function bounds: CGRect; message 'bounds';
    function topLeft_: CGPoint; message 'topLeft';
    function topRight_: CGPoint; message 'topRight';
    function bottomLeft_: CGPoint; message 'bottomLeft';
    function bottomRight_: CGPoint; message 'bottomRight';
  end;

type
  CIQRCodeFeature = objcclass external (CIFeature)
  private
    bounds_: CGRect;
    topLeft: CGPoint;
    topRight: CGPoint;
    bottomLeft: CGPoint;
    bottomRight: CGPoint;
    messageString: NSString;
  public
    function bounds: CGRect; message 'bounds';
    function topLeft_: CGPoint; message 'topLeft';
    function topRight_: CGPoint; message 'topRight';
    function bottomLeft_: CGPoint; message 'bottomLeft';
    function bottomRight_: CGPoint; message 'bottomRight';
    function messageString_: NSString; message 'messageString';
  end;
{$endif}

