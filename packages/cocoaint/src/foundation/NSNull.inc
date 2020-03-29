{ Parsed from Foundation.framework NSNull.h }


{$ifdef TYPES}
type
  NSNullPtr = ^NSNull;
{$endif}

{$ifdef CLASSES}

type
  NSNull = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    class function null: NSNull; message 'null';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

