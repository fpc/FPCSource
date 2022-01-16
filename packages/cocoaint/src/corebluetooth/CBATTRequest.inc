{ Parsed from CoreBluetooth.framework CBATTRequest.h }


{$ifdef TYPES}
type
  CBATTRequestPtr = ^CBATTRequest;
{$endif}

{$ifdef CLASSES}

type
  CBATTRequest = objcclass external (NSObject)
  private
    _central: CBCentral;
    _characteristic: CBCharacteristic;
    _offset: NSUInteger;
    _value: NSData;
    _transactionID: NSNumber;
    _ignoreResponse: ObjCBOOL;
  public
    function central: CBCentral; message 'central';
    function characteristic: CBCharacteristic; message 'characteristic';
    function offset: NSUInteger; message 'offset';
    procedure setValue(newValue: NSData); message 'setValue:';
    function value: NSData; message 'value';
  end;
{$endif}

