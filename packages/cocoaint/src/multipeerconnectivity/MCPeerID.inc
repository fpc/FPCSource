{ Parsed from MultipeerConnectivity.framework MCPeerID.h }


{$ifdef TYPES}
type
  MCPeerIDPtr = ^MCPeerID;
{$endif}

{$ifdef CLASSES}

type
  MCPeerID = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    function initWithDisplayName (myDisplayName: NSString): instancetype; message 'initWithDisplayName:'; { NS_DESIGNATED_INITIALIZER }
    function displayName: NSString; message 'displayName';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

