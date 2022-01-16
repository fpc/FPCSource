{ Parsed from CoreBluetooth.framework CBService.h }


{$ifdef TYPES}
type
  CBServicePtr = ^CBService;
  CBMutableServicePtr = ^CBMutableService;
{$endif}

{$ifdef CLASSES}

type
  CBService = objcclass external (NSObject)
  private
    _peripheral: CBPeripheral;
    _UUID: CBUUID;
    _isPrimary: ObjCBOOL;
    _includedServices: NSArray;
    _characteristics: NSArray;
    _startHandle: NSNumber;
    _endHandle: NSNumber;
  public
    function peripheral: CBPeripheral; message 'peripheral';
    function UUID: CBUUID; message 'UUID';
    function isPrimary: ObjCBOOL; message 'isPrimary';
    function includedServices: NSArray; message 'includedServices';
    function characteristics: NSArray; message 'characteristics';
  end;

type
  CBMutableService = objcclass external (CBService)
  private
    _ID: NSNumber;
  public
    procedure setUUID(newValue: CBUUID); message 'setUUID:';
    function UUID: CBUUID; message 'UUID';
    procedure setIsPrimary(newValue: ObjCBOOL); message 'setIsPrimary:';
    function isPrimary: ObjCBOOL; message 'isPrimary';
    procedure setIncludedServices(newValue: NSArray); message 'setIncludedServices:';
    function includedServices: NSArray; message 'includedServices';
    procedure setCharacteristics(newValue: NSArray); message 'setCharacteristics:';
    function characteristics: NSArray; message 'characteristics';
    function initWithType_primary (UUID_: CBUUID; isPrimary_: ObjCBOOL): id; message 'initWithType:primary:';
  end;
{$endif}

