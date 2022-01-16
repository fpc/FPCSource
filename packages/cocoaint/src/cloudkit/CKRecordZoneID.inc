{ Parsed from CloudKit.framework CKRecordZoneID.h }


{$ifdef TYPES}
type
  CKRecordZoneIDPtr = ^CKRecordZoneID;
{$endif}

{$ifdef CLASSES}

type
  CKRecordZoneID = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  public
    function init: instancetype; message 'init';
    function initWithZoneName_ownerName (zoneName: NSString; ownerName: NSString): instancetype; message 'initWithZoneName:ownerName:'; { NS_DESIGNATED_INITIALIZER }
    function zoneName: NSString; message 'zoneName';
    function ownerName: NSString; message 'ownerName';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

