{ Parsed from AudioVideoBridging.framework AVBMACAddress.h }


{$ifdef TYPES}
type
  AVBMACAddressPtr = ^AVBMACAddress;
{$endif}

{$ifdef CLASSES}

type
  AVBMACAddress = objcclass external (NSObject, NSCopyingProtocol)
  private
    _bytes:array[0..(AVBMACAddressSize)-1] of cuint8;
  public
    function initWithBytes (bytes: pbyte): instancetype; message 'initWithBytes:';
    function bytes: pbyte; message 'bytes';
    procedure setDataRepresentation(newValue: NSData); message 'setDataRepresentation:';
    function dataRepresentation: NSData; message 'dataRepresentation';
    procedure setStringRepresentation(newValue: NSString); message 'setStringRepresentation:';
    function stringRepresentation: NSString; message 'stringRepresentation';
    procedure setMulticast(newValue: ObjCBOOL); message 'setMulticast:';
    function isMulticast: ObjCBOOL; message 'isMulticast';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

