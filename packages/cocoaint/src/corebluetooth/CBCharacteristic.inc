{ Parsed from CoreBluetooth.framework CBCharacteristic.h }


{$ifdef TYPES}
type
  CBCharacteristicPtr = ^CBCharacteristic;
  CBMutableCharacteristicPtr = ^CBMutableCharacteristic;
{$endif}

{$ifdef TYPES}
type
  CBCharacteristicProperties = NSInteger;
  CBCharacteristicPropertiesPtr = ^CBCharacteristicProperties;

const
  CBCharacteristicPropertyBroadcast = $01;
  CBCharacteristicPropertyRead = $02;
  CBCharacteristicPropertyWriteWithoutResponse = $04;
  CBCharacteristicPropertyWrite = $08;
  CBCharacteristicPropertyNotify = $10;
  CBCharacteristicPropertyIndicate = $20;
  CBCharacteristicPropertyAuthenticatedSignedWrites = $40;
  CBCharacteristicPropertyExtendedProperties = $80;
  CBCharacteristicPropertyNotifyEncryptionRequired = $100 { available in 10_9, 6_0 };
  CBCharacteristicPropertyIndicateEncryptionRequired = $200 { available in 10_9, 6_0 };
{$endif}

{$ifdef CLASSES}

type
  CBCharacteristic = objcclass external (NSObject)
  private
    _service: CBService;
    _UUID: CBUUID;
    _properties: CBCharacteristicProperties;
    _value: NSData;
    _descriptors: NSArray;
    _isBroadcasted: ObjCBOOL;
    _isNotifying: ObjCBOOL;
    _peripheral: CBPeripheral;
    _handle: NSNumber;
    _valueHandle: NSNumber;
  public
    function service: CBService; message 'service';
    function UUID: CBUUID; message 'UUID';
    function properties: CBCharacteristicProperties; message 'properties';
    function value: NSData; message 'value';
    function descriptors: NSArray; message 'descriptors';
    function isBroadcasted: ObjCBOOL; message 'isBroadcasted';
    function isNotifying: ObjCBOOL; message 'isNotifying';
  end;
{$endif}

{$ifdef TYPES}
type
  CBAttributePermissions = NSInteger;
  CBAttributePermissionsPtr = ^CBAttributePermissions;

const
  CBAttributePermissionsReadable = $01;
  CBAttributePermissionsWriteable = $02;
  CBAttributePermissionsReadEncryptionRequired = $04;
  CBAttributePermissionsWriteEncryptionRequired = $08;
{$endif}

{$ifdef CLASSES}

type
  CBMutableCharacteristic = objcclass external (CBCharacteristic)
  private
    _ID: NSNumber;
    _permissions: CBAttributePermissions;
    _subscribedCentrals: NSMutableArray;
  public
    procedure setPermissions(newValue: CBAttributePermissions); message 'setPermissions:';
    function permissions: CBAttributePermissions; message 'permissions';
    function subscribedCentrals: NSArray; message 'subscribedCentrals';
    procedure setUUID(newValue: CBUUID); message 'setUUID:';
    function UUID: CBUUID; message 'UUID';
    procedure setProperties(newValue: CBCharacteristicProperties); message 'setProperties:';
    function properties: CBCharacteristicProperties; message 'properties';
    procedure setValue(newValue: NSData); message 'setValue:';
    function value: NSData; message 'value';
    procedure setDescriptors(newValue: NSArray); message 'setDescriptors:';
    function descriptors: NSArray; message 'descriptors';
    function initWithType_properties_value_permissions (UUID_: CBUUID; properties_: CBCharacteristicProperties; value_: NSData; permissions_: CBAttributePermissions): id; message 'initWithType:properties:value:permissions:';
  end;
{$endif}

