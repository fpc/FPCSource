{ Parsed from CoreBluetooth.framework CBCentral.h }


{$ifdef TYPES}
type
  CBCentralPtr = ^CBCentral;
{$endif}

{$ifdef CLASSES}

type
  CBCentral = objcclass external (NSObject, NSCopyingProtocol)
  private
    _UUID: CFUUIDRef;
    _identifier: NSUUID;
    _maximumUpdateValueLength: NSUInteger;
  public
    function UUID: CFUUIDRef; message 'UUID';
    function identifier: NSUUID; message 'identifier';
    function maximumUpdateValueLength: NSUInteger; message 'maximumUpdateValueLength';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

