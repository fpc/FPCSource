{ Parsed from CoreBluetooth.framework CBPeripheralManager.h }


{$ifdef TYPES}
type
  CBPeripheralManagerPtr = ^CBPeripheralManager;
  CBPeripheralManagerDelegateProtocolPtr = ^CBPeripheralManagerDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  CBPeripheralAuthorizationStatus = NSInteger;
  CBPeripheralAuthorizationStatusPtr = ^CBPeripheralAuthorizationStatus;

const
  CBPeripheralAuthorizationStatusNotDetermined = 0;
  CBPeripheralAuthorizationStatusRestricted = 1;
  CBPeripheralAuthorizationStatusDenied = 2;
  CBPeripheralAuthorizationStatusAuthorized = 3;

type
  CBPeripheralManagerState = NSInteger;
  CBPeripheralManagerStatePtr = ^CBPeripheralManagerState;

const
  CBPeripheralManagerStateUnknown = 0;
  CBPeripheralManagerStateResetting = 1;
  CBPeripheralManagerStateUnsupported = 2;
  CBPeripheralManagerStateUnauthorized = 3;
  CBPeripheralManagerStatePoweredOff = 4;
  CBPeripheralManagerStatePoweredOn = 5;

type
  CBPeripheralManagerConnectionLatency = NSInteger;
  CBPeripheralManagerConnectionLatencyPtr = ^CBPeripheralManagerConnectionLatency;

const
  CBPeripheralManagerConnectionLatencyLow = 0;
  CBPeripheralManagerConnectionLatencyMedium = 1;
  CBPeripheralManagerConnectionLatencyHigh = 2;
{$endif}

{$ifdef CLASSES}

type
  CBPeripheralManager = objcclass external (NSObject)
  private
    _delegate: CBPeripheralManagerDelegateProtocol;
    _connection: id;
    _state: CBPeripheralManagerState;
    _advertising: ObjCBOOL;
    _centrals: NSMutableDictionary;
    _services: NSMutableArray;
    _characteristicIDs: NSMutableDictionary;
    _updateLock: NSLock;
    _readyForUpdates: ObjCBOOL;
    _waitingForReady: ObjCBOOL;
  public
    procedure setDelegate(newValue: CBPeripheralManagerDelegateProtocol); message 'setDelegate:';
    function delegate: CBPeripheralManagerDelegateProtocol; message 'delegate';
    function state: CBPeripheralManagerState; message 'state';
    function isAdvertising: ObjCBOOL; message 'isAdvertising';
    class function authorizationStatus: CBPeripheralAuthorizationStatus; message 'authorizationStatus'; { available in NA, 7_0 }
    function initWithDelegate_queue (delegate_: CBPeripheralManagerDelegateProtocol; queue: dispatch_queue_t): id; message 'initWithDelegate:queue:';
    function initWithDelegate_queue_options (delegate_: CBPeripheralManagerDelegateProtocol; queue: dispatch_queue_t; options: NSDictionary): id; message 'initWithDelegate:queue:options:'; { available in 10_9, 7_0 }
    procedure startAdvertising (advertisementData: NSDictionary); message 'startAdvertising:';
    procedure stopAdvertising; message 'stopAdvertising';
    procedure setDesiredConnectionLatency_forCentral (latency: CBPeripheralManagerConnectionLatency; central: CBCentral); message 'setDesiredConnectionLatency:forCentral:';
    procedure addService (service: CBMutableService); message 'addService:';
    procedure removeService (service: CBMutableService); message 'removeService:';
    procedure removeAllServices; message 'removeAllServices';
    procedure respondToRequest_withResult (request: CBATTRequest; result_: CBATTError); message 'respondToRequest:withResult:';
    function updateValue_forCharacteristic_onSubscribedCentrals (value: NSData; characteristic: CBMutableCharacteristic; centrals: NSArray): ObjCBOOL; message 'updateValue:forCharacteristic:onSubscribedCentrals:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  CBPeripheralManagerDelegateProtocol = objcprotocol external name 'CBPeripheralManagerDelegate' (NSObjectProtocol)
  required
    procedure peripheralManagerDidUpdateState (peripheral: CBPeripheralManager); message 'peripheralManagerDidUpdateState:';
  optional
    procedure peripheralManager_willRestoreState (peripheral: CBPeripheralManager; dict: NSDictionary); message 'peripheralManager:willRestoreState:';
    procedure peripheralManagerDidStartAdvertising_error (peripheral: CBPeripheralManager; error: NSError); message 'peripheralManagerDidStartAdvertising:error:';
    procedure peripheralManager_didAddService_error (peripheral: CBPeripheralManager; service: CBService; error: NSError); message 'peripheralManager:didAddService:error:';
    procedure peripheralManager_central_didSubscribeToCharacteristic (peripheral: CBPeripheralManager; central: CBCentral; characteristic: CBCharacteristic); message 'peripheralManager:central:didSubscribeToCharacteristic:';
    procedure peripheralManager_central_didUnsubscribeFromCharacteristic (peripheral: CBPeripheralManager; central: CBCentral; characteristic: CBCharacteristic); message 'peripheralManager:central:didUnsubscribeFromCharacteristic:';
    procedure peripheralManager_didReceiveReadRequest (peripheral: CBPeripheralManager; request: CBATTRequest); message 'peripheralManager:didReceiveReadRequest:';
    procedure peripheralManager_didReceiveWriteRequests (peripheral: CBPeripheralManager; requests: NSArray); message 'peripheralManager:didReceiveWriteRequests:';
    procedure peripheralManagerIsReadyToUpdateSubscribers (peripheral: CBPeripheralManager); message 'peripheralManagerIsReadyToUpdateSubscribers:';
  end;
{$endif}

