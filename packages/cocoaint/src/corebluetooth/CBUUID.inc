{ Parsed from CoreBluetooth.framework CBUUID.h }


{$ifdef TYPES}
type
  CBUUIDPtr = ^CBUUID;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  CBUUIDCharacteristicExtendedPropertiesString: NSString; cvar; external;
  CBUUIDCharacteristicUserDescriptionString: NSString; cvar; external;
  CBUUIDClientCharacteristicConfigurationString: NSString; cvar; external;
  CBUUIDServerCharacteristicConfigurationString: NSString; cvar; external;
  CBUUIDCharacteristicFormatString: NSString; cvar; external;
  CBUUIDCharacteristicAggregateFormatString: NSString; cvar; external;
  CBUUIDGenericAccessProfileString: NSString; cvar; external;
  CBUUIDGenericAttributeProfileString: NSString; cvar; external;
  CBUUIDDeviceNameString: NSString; cvar; external;
  CBUUIDAppearanceString: NSString; cvar; external;
  CBUUIDPeripheralPrivacyFlagString: NSString; cvar; external;
  CBUUIDReconnectionAddressString: NSString; cvar; external;
  CBUUIDPeripheralPreferredConnectionParametersString: NSString; cvar; external;
  CBUUIDServiceChangedString: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  CBUUID = objcclass external (NSObject, NSCopyingProtocol)
  private
    _data: NSData;
    _range: NSRange;
  public
    function data: NSData; message 'data';
    function UUIDString: NSString; message 'UUIDString';
    class function UUIDWithString (theString: NSString): CBUUID; message 'UUIDWithString:';
    class function UUIDWithData (theData: NSData): CBUUID; message 'UUIDWithData:';
    class function UUIDWithCFUUID (theUUID: CFUUIDRef): CBUUID; message 'UUIDWithCFUUID:';
    class function UUIDWithNSUUID (theUUID: NSUUID): CBUUID; message 'UUIDWithNSUUID:'; { available in 10_9, 7_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

