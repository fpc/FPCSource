{ Parsed from SceneKit.framework SCNLevelOfDetail.h }


{$ifdef TYPES}
type
  SCNLevelOfDetailPtr = ^SCNLevelOfDetail;
{$endif}

{$ifdef CLASSES}

type
  SCNLevelOfDetail = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  private
    _reserved: id;
  public
    class function levelOfDetailWithGeometry_screenSpaceRadius (geometry: SCNGeometry; radius: CGFloat): instancetype; message 'levelOfDetailWithGeometry:screenSpaceRadius:';
    class function levelOfDetailWithGeometry_worldSpaceDistance (geometry: SCNGeometry; distance: CGFloat): instancetype; message 'levelOfDetailWithGeometry:worldSpaceDistance:';
    function geometry: SCNGeometry; message 'geometry';
    function screenSpaceRadius: CGFloat; message 'screenSpaceRadius';
    function worldSpaceDistance: CGFloat; message 'worldSpaceDistance';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

