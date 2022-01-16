{ Parsed from CoreBluetooth.framework CBDescriptor.h }


{$ifdef TYPES}
type
  CBDescriptorPtr = ^CBDescriptor;
  CBMutableDescriptorPtr = ^CBMutableDescriptor;
{$endif}

{$ifdef CLASSES}

type
  CBDescriptor = objcclass external (NSObject)
  private
    _characteristic: CBCharacteristic;
    _UUID: CBUUID;
    _value: id;
    _peripheral: CBPeripheral;
    _handle: NSNumber;
  public
    function characteristic: CBCharacteristic; message 'characteristic';
    function UUID: CBUUID; message 'UUID';
    function value: id; message 'value';
  end;

type
  CBMutableDescriptor = objcclass external (CBDescriptor)
  private
    _ID: NSNumber;
  public
    function initWithType_value (UUID_: CBUUID; value_: id): id; message 'initWithType:value:';
  end;
{$endif}

