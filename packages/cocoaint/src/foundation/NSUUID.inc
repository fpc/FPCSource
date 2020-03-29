{ Parsed from Foundation.framework NSUUID.h }


{$ifdef TYPES}
type
  NSUUIDPtr = ^NSUUID;
{$endif}

{$ifdef CLASSES}

type
  NSUUID = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    class function UUID: instancetype; message 'UUID';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithUUIDString (string_: NSString): instancetype; message 'initWithUUIDString:';
    function initWithUUIDBytes (bytes: uuid_t): instancetype; message 'initWithUUIDBytes:';
    procedure getUUIDBytes (uuid_: uuid_t); message 'getUUIDBytes:';
    function UUIDString: NSString; message 'UUIDString';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

