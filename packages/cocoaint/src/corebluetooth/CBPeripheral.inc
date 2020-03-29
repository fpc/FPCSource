{ Parsed from CoreBluetooth.framework CBPeripheral.h }


{$ifdef TYPES}
type
  CBPeripheralPtr = ^CBPeripheral;
  CBPeripheralDelegateProtocolPtr = ^CBPeripheralDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  CBPeripheralState = NSInteger;
  CBPeripheralStatePtr = ^CBPeripheralState;

const
  CBPeripheralStateDisconnected = 0;
  CBPeripheralStateConnecting = 1;
  CBPeripheralStateConnected = 2;

type
  CBCharacteristicWriteType = NSInteger;
  CBCharacteristicWriteTypePtr = ^CBCharacteristicWriteType;

const
  CBCharacteristicWriteWithResponse = 0;
  CBCharacteristicWriteWithoutResponse = 1;
{$endif}

{$ifdef CLASSES}

type
  CBPeripheral = objcclass external (NSObject, NSCopyingProtocol)
  private
    _delegate: CBPeripheralDelegateProtocol;
    _UUID: CFUUIDRef;
    _identifier: NSUUID;
    _name: NSString;
    _RSSI: NSNumber;
    _state: CBPeripheralState;
    _services: NSMutableArray;
    _centralManager: CBCentralManager;
    mtuLength: NSUInteger;
    _attributes: NSMutableDictionary;
    _isPaired: ObjCBOOL;
    _isConnectedToSystem: ObjCBOOL;
  public
    procedure setDelegate(newValue: CBPeripheralDelegateProtocol); message 'setDelegate:';
    function delegate: CBPeripheralDelegateProtocol; message 'delegate';
    function UUID: CFUUIDRef; message 'UUID';
    function identifier: NSUUID; message 'identifier';
    function name: NSString; message 'name';
    function RSSI: NSNumber; message 'RSSI';
    function isConnected: ObjCBOOL; message 'isConnected';
    function state: CBPeripheralState; message 'state';
    function services: NSArray; message 'services';
    procedure readRSSI; message 'readRSSI';
    procedure discoverServices (serviceUUIDs: NSArray); message 'discoverServices:';
    procedure discoverIncludedServices_forService (includedServiceUUIDs: NSArray; service: CBService); message 'discoverIncludedServices:forService:';
    procedure discoverCharacteristics_forService (characteristicUUIDs: NSArray; service: CBService); message 'discoverCharacteristics:forService:';
    procedure readValueForCharacteristic (characteristic: CBCharacteristic); message 'readValueForCharacteristic:';
    procedure writeValue_forCharacteristic_type (data: NSData; characteristic: CBCharacteristic; type_: CBCharacteristicWriteType); message 'writeValue:forCharacteristic:type:';
    procedure setNotifyValue_forCharacteristic (enabled: ObjCBOOL; characteristic: CBCharacteristic); message 'setNotifyValue:forCharacteristic:';
    procedure discoverDescriptorsForCharacteristic (characteristic: CBCharacteristic); message 'discoverDescriptorsForCharacteristic:';
    procedure readValueForDescriptor (descriptor: CBDescriptor); message 'readValueForDescriptor:';
    procedure writeValue_forDescriptor (data: NSData; descriptor: CBDescriptor); message 'writeValue:forDescriptor:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  CBPeripheralDelegateProtocol = objcprotocol external name 'CBPeripheralDelegate' (NSObjectProtocol)
  optional
    procedure peripheralDidUpdateName (peripheral: CBPeripheral); message 'peripheralDidUpdateName:'; { available in 10_9, 6_0 }
    procedure peripheralDidInvalidateServices (peripheral: CBPeripheral); message 'peripheralDidInvalidateServices:'; deprecated 'in NA, NA, 6_0, 7_0';
    procedure peripheral_didModifyServices (peripheral: CBPeripheral; invalidatedServices: NSArray); message 'peripheral:didModifyServices:'; { available in 10_9, 7_0 }
    procedure peripheralDidUpdateRSSI_error (peripheral: CBPeripheral; error: NSError); message 'peripheralDidUpdateRSSI:error:';
    procedure peripheral_didDiscoverServices (peripheral: CBPeripheral; error: NSError); message 'peripheral:didDiscoverServices:';
    procedure peripheral_didDiscoverIncludedServicesForService_error (peripheral: CBPeripheral; service: CBService; error: NSError); message 'peripheral:didDiscoverIncludedServicesForService:error:';
    procedure peripheral_didDiscoverCharacteristicsForService_error (peripheral: CBPeripheral; service: CBService; error: NSError); message 'peripheral:didDiscoverCharacteristicsForService:error:';
    procedure peripheral_didUpdateValueForCharacteristic_error (peripheral: CBPeripheral; characteristic: CBCharacteristic; error: NSError); message 'peripheral:didUpdateValueForCharacteristic:error:';
    procedure peripheral_didWriteValueForCharacteristic_error (peripheral: CBPeripheral; characteristic: CBCharacteristic; error: NSError); message 'peripheral:didWriteValueForCharacteristic:error:';
    procedure peripheral_didUpdateNotificationStateForCharacteristic_error (peripheral: CBPeripheral; characteristic: CBCharacteristic; error: NSError); message 'peripheral:didUpdateNotificationStateForCharacteristic:error:';
    procedure peripheral_didDiscoverDescriptorsForCharacteristic_error (peripheral: CBPeripheral; characteristic: CBCharacteristic; error: NSError); message 'peripheral:didDiscoverDescriptorsForCharacteristic:error:';
    procedure peripheral_didUpdateValueForDescriptor_error (peripheral: CBPeripheral; descriptor: CBDescriptor; error: NSError); message 'peripheral:didUpdateValueForDescriptor:error:';
    procedure peripheral_didWriteValueForDescriptor_error (peripheral: CBPeripheral; descriptor: CBDescriptor; error: NSError); message 'peripheral:didWriteValueForDescriptor:error:';
  end;
{$endif}

